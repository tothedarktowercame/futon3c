;;; futon3c-chat.el --- Chat with Claude via futon3c API -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs chat buffer backed by futon3c's /api/alpha/invoke endpoint.
;; Asynchronous: type, RET, Claude responds without blocking Emacs UI.
;; Routes through the agent registry — same invoke-fn as IRC.
;; Logs every turn to the evidence landscape (same pattern as codex-repl.el).
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/futon3c-ui.el")
;;   (load "/home/joe/code/futon3c/emacs/futon3c-chat.el")
;;   M-x futon3c-chat

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'futon3c-ui)

;;; Configuration

(defgroup futon3c-chat nil
  "Chat with Claude via futon3c API."
  :group 'futon3c-ui)

(defcustom futon3c-chat-api-url "http://localhost:7070"
  "Base URL for the futon3c API server."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-agent-id "claude-1"
  "Agent ID to invoke via the API."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-session-file "/tmp/futon-session-id"
  "File storing Claude session ID (shared with IRC relay)."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-evidence-url
  (or (getenv "FUTON3C_EVIDENCE_URL")
      (format "%s/api/alpha/evidence"
              (string-remove-suffix "/" futon3c-ui-agency-base-url)))
  "Evidence API endpoint for logging chat turns."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-evidence-log-turns t
  "When non-nil, log user/assistant turns into the evidence API."
  :type 'boolean
  :group 'futon3c-chat)

(defcustom futon3c-chat-evidence-timeout 1
  "Timeout in seconds for evidence API requests."
  :type 'number
  :group 'futon3c-chat)

;;; Face (Claude-specific; shared faces are in futon3c-ui)

(defface futon3c-chat-claude-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Claude."
  :group 'futon3c-chat)

;;; Internal state

(defvar futon3c-chat--buffer-name "*futon3c-chat*")
(defvar futon3c-chat--last-evidence-id nil
  "Last evidence entry ID, used for in-reply-to chaining.")
(defvar futon3c-chat--evidence-session-id nil
  "Session ID associated with `futon3c-chat--last-evidence-id'.")
(defvar futon3c-chat--last-emitted-session-id nil
  "Last session ID for which a session-start evidence entry was emitted.")

;;; Surface contract

(defun futon3c-chat--surface-contract ()
  "Return runtime surface contract for Claude invoke prompts."
  (string-join
   (list
    "Runtime surface contract:"
    "- Current surface: emacs-futon3c-chat."
    "- Your response is shown in the user's Emacs chat buffer."
    (format "- To post to IRC: curl -sS -H 'Content-Type: application/json' -d '{\"channel\":\"#futon\",\"from\":\"%s\",\"text\":\"YOUR MESSAGE\"}' %s/api/alpha/irc/send"
            futon3c-chat-agent-id futon3c-chat-api-url)
    "- Do not claim to post to IRC or send network messages unless a tool call in this turn actually did it.")
   "\n"))

;;; Evidence logging

(defun futon3c-chat--evidence-enabled-p ()
  "Return non-nil when evidence logging is configured."
  (and (stringp futon3c-chat-evidence-url)
       (not (string-empty-p futon3c-chat-evidence-url))))

(defun futon3c-chat--evidence-base-url ()
  "Return evidence API base URL without trailing /api/alpha/evidence."
  (let ((value (string-remove-suffix "/" (or futon3c-chat-evidence-url ""))))
    (replace-regexp-in-string "/api/alpha/evidence\\'" "" value)))

(defun futon3c-chat--evidence-request-json (method url &optional payload)
  "Send evidence METHOD request to URL with optional JSON PAYLOAD.
Returns plist with keys :status and :json.  Returns nil on transport failure."
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data (when payload
                             (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t futon3c-chat-evidence-timeout)
                   (error nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status) 0)))
          (re-search-forward "\n\n" nil 'move)
          (let* ((body (buffer-substring-no-properties (point) (point-max)))
                 (parsed (condition-case nil
                             (json-parse-string body :object-type 'plist)
                           (error nil))))
            (kill-buffer buffer)
            (list :status status :json parsed)))))))

(defun futon3c-chat--evidence-post-entry-id (payload)
  "POST PAYLOAD to evidence API and return created evidence id, or nil."
  (when (futon3c-chat--evidence-enabled-p)
    (let* ((url (format "%s/api/alpha/evidence" (futon3c-chat--evidence-base-url)))
           (response (futon3c-chat--evidence-request-json "POST" url payload))
           (status (plist-get response :status))
           (parsed (plist-get response :json)))
      (when (and (integerp status) (<= 200 status) (< status 300))
        (or (plist-get parsed :evidence/id)
            (plist-get (plist-get parsed :entry) :evidence/id))))))

(defun futon3c-chat--evidence-fetch-latest-id (sid)
  "Fetch most recent evidence id for session SID."
  (when (and (futon3c-chat--evidence-enabled-p)
             (stringp sid)
             (not (string-empty-p sid)))
    (let* ((query (format "session-id=%s&limit=1"
                          (url-hexify-string sid)))
           (url (format "%s/api/alpha/evidence?%s"
                        (futon3c-chat--evidence-base-url)
                        query))
           (response (futon3c-chat--evidence-request-json "GET" url nil))
           (status (plist-get response :status))
           (entries (and (integerp status)
                         (<= 200 status)
                         (< status 300)
                         (plist-get (plist-get response :json) :entries)))
           (entry (and (listp entries) (car entries))))
      (and (listp entry)
           (plist-get entry :evidence/id)))))

(defun futon3c-chat--sync-evidence-anchor! (&optional sid force)
  "Refresh last evidence anchor from API for SID.
When FORCE is non-nil, refresh even when session is unchanged."
  (let ((target-sid (or sid futon3c-ui--session-id)))
    (when (and (stringp target-sid) (not (string-empty-p target-sid))
               (or force
                   (not (equal target-sid futon3c-chat--evidence-session-id))
                   (null futon3c-chat--last-evidence-id)))
      (setq futon3c-chat--evidence-session-id target-sid
            futon3c-chat--last-evidence-id (futon3c-chat--evidence-fetch-latest-id target-sid)))))

(defun futon3c-chat--emit-session-start-evidence! (sid)
  "Emit a lightweight session-start evidence entry for SID."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (unless (equal sid futon3c-chat--evidence-session-id)
      (setq futon3c-chat--evidence-session-id sid
            futon3c-chat--last-evidence-id nil))
    (futon3c-chat--sync-evidence-anchor! sid t)
    (when (and (not (equal sid futon3c-chat--last-emitted-session-id))
               (not (and (stringp futon3c-chat--last-evidence-id)
                         (not (string-empty-p futon3c-chat--last-evidence-id))))
               (futon3c-chat--evidence-enabled-p))
      (let ((payload `((subject . ((ref/type . "session")
                                   (ref/id . ,sid)))
                       (type . "coordination")
                       (claim-type . "goal")
                       (author . ,(or (getenv "USER") user-login-name "joe"))
                       (session-id . ,sid)
                       (body . ((event . "session-start")
                                (source . "futon3c-chat")
                                (mode . "emacs")))
                       (tags . ["claude" "session-start" "chat"]))))
        (when-let ((new-id (futon3c-chat--evidence-post-entry-id payload)))
          (setq futon3c-chat--last-evidence-id new-id))
        (setq futon3c-chat--last-emitted-session-id sid)))
    (unless (and (stringp futon3c-chat--last-evidence-id)
                 (not (string-empty-p futon3c-chat--last-evidence-id)))
      (futon3c-chat--sync-evidence-anchor! sid t))))

(defun futon3c-chat--emit-turn-evidence! (role text)
  "Emit a turn evidence event for ROLE (\"user\" or \"assistant\") and TEXT."
  (let ((sid futon3c-ui--session-id))
    (when (and futon3c-chat-evidence-log-turns
               (stringp text)
               (not (string-empty-p (string-trim text)))
               (stringp sid)
               (not (string-empty-p sid))
               (futon3c-chat--evidence-enabled-p))
      (futon3c-chat--sync-evidence-anchor! sid)
      (let* ((trimmed (string-trim text))
             (is-user (string= role "user"))
             (is-error (string-prefix-p "[Error" trimmed))
             (claim-type (cond
                          (is-user "question")
                          (is-error "correction")
                          (t "observation")))
             (author (if is-user
                         (or (getenv "USER") user-login-name "joe")
                       futon3c-chat-agent-id))
             (role-tag (if is-user "user" "assistant"))
             (payload `((subject . ((ref/type . "session")
                                    (ref/id . ,sid)))
                        (type . "coordination")
                        (claim-type . ,claim-type)
                        (author . ,author)
                        (session-id . ,sid)
                        (body . ((event . "chat-turn")
                                 (transport . "emacs-futon3c-chat")
                                 (role . ,role)
                                 (text . ,trimmed)))
                        (tags . ["claude" "chat" "turn" ,role-tag]))))
        (when (and (stringp futon3c-chat--last-evidence-id)
                   (not (string-empty-p futon3c-chat--last-evidence-id)))
          (setq payload (append payload
                                `((in-reply-to . ,futon3c-chat--last-evidence-id)))))
        (when-let ((new-id (futon3c-chat--evidence-post-entry-id payload)))
          (setq futon3c-chat--evidence-session-id sid
                futon3c-chat--last-evidence-id new-id))))))

(defun futon3c-chat--emit-user-turn-evidence! (text)
  "Emit evidence for user TEXT."
  (futon3c-chat--emit-turn-evidence! "user" text))

(defun futon3c-chat--emit-assistant-turn-evidence! (text)
  "Emit evidence for assistant TEXT."
  (futon3c-chat--emit-turn-evidence! "assistant" text))

;;; Claude API call

(defun futon3c-chat--call-claude-async (text callback)
  "Send TEXT to Claude via POST /api/alpha/invoke.
Uses curl to POST to the futon3c API server. The server's invoke-fn
calls `claude -p' with the correct session-id (managed by the registry).
CALLBACK receives the response string."
  (let* ((chat-buffer (current-buffer))
         (url (concat futon3c-chat-api-url "/api/alpha/invoke"))
         (full-prompt (format "%s\n\nUser message:\n%s"
                              (futon3c-chat--surface-contract) text))
         (json-body (json-serialize
                     `(:agent-id ,futon3c-chat-agent-id
                       :prompt ,full-prompt
                       :caller ,(or (getenv "USER") user-login-name "joe"))))
         (outbuf (generate-new-buffer " *futon3c-invoke*"))
         (proc nil))
    (setq proc
          (make-process
           :name "futon3c-invoke"
           :buffer outbuf
           :command (list "curl" "-sS" "--max-time" "1800"
                          "-H" "Content-Type: application/json"
                          "-d" json-body url)
           :noquery t
           :connection-type 'pipe
           :filter (lambda (proc string)
                     (when (buffer-live-p (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (goto-char (point-max))
                         (insert string))))
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (condition-case err
                   (let* ((exit-code (process-exit-status p))
                          (raw (if (buffer-live-p (process-buffer p))
                                   (with-current-buffer (process-buffer p)
                                     (buffer-string))
                                 ""))
                          (response
                           (if (/= exit-code 0)
                               (format "[curl error (exit %d): %s]"
                                       exit-code (string-trim raw))
                             (condition-case parse-err
                                 (let* ((json-obj (json-parse-string
                                                   raw
                                                   :object-type 'alist
                                                   :null-object nil
                                                   :false-object nil))
                                        (ok (alist-get 'ok json-obj))
                                        (result (alist-get 'result json-obj))
                                        (sid (alist-get 'session-id json-obj))
                                        (err-msg (or (alist-get 'message json-obj)
                                                     (alist-get 'error json-obj))))
                                   (when (and ok sid (buffer-live-p chat-buffer))
                                     (with-current-buffer chat-buffer
                                       (futon3c-ui-update-session-id sid))
                                     (when futon3c-chat-session-file
                                       (write-region sid nil futon3c-chat-session-file nil 'silent))
                                     ;; Emit session-start evidence on first successful response
                                     (futon3c-chat--emit-session-start-evidence! sid))
                                   (if ok
                                       (let ((r (and (stringp result)
                                                     (not (string-empty-p (string-trim result)))
                                                     result)))
                                         (or r "[empty response]"))
                                     (format "[Error: %s]" (or err-msg raw))))
                               (error
                                (format "[JSON parse error: %s\nRaw: %s]"
                                        (error-message-string parse-err)
                                        (truncate-string-to-width raw 200)))))))
                     (message "futon3c-chat: exit=%d raw-len=%d" exit-code (length raw))
                     (when (buffer-live-p (process-buffer p))
                       (kill-buffer (process-buffer p)))
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (when (eq futon3c-ui--pending-process p)
                           (setq futon3c-ui--pending-process nil))
                         (funcall callback response))))
                 (error
                  (message "futon3c-chat sentinel error: %s" (error-message-string err))
                  (when (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (setq futon3c-ui--pending-process nil)
                      (futon3c-ui-remove-thinking)
                      (futon3c-ui-insert-message
                       "claude"
                       (format "[Sentinel error: %s]" (error-message-string err)))))))))))
    proc))

;;; Modeline

(defun futon3c-chat--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list "emacs-chat (active, via /api/alpha/invoke)"))
        (irc-up (futon3c-ui-irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (claude code)" transports)
    (format "Available transports: [%s]. Current: emacs-chat."
            (string-join (reverse transports) ", "))))

;;; Mode

(defvar futon3c-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'futon3c-chat-send-input)
    (define-key map (kbd "C-c C-k") #'futon3c-chat-clear)
    map))

(define-derived-mode futon3c-chat-mode nil "Chat"
  "Chat with Claude via futon3c API.
Type after the prompt, RET to send.
\\{futon3c-chat-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0))

(defun futon3c-chat-send-input ()
  "Send input to Claude and display response."
  (interactive)
  (futon3c-ui-send-input
   #'futon3c-chat--call-claude-async
   "claude"
   (list :before-send #'futon3c-chat--emit-user-turn-evidence!
         :on-response #'futon3c-chat--emit-assistant-turn-evidence!)))

(defun futon3c-chat-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (futon3c-ui-clear #'futon3c-chat--init))

(defun futon3c-chat--init ()
  "Initialize.
Load existing session-id from file if available."
  (let ((existing-sid
         (when (and futon3c-chat-session-file
                    (file-exists-p futon3c-chat-session-file))
           (let ((s (string-trim
                     (with-temp-buffer
                       (insert-file-contents futon3c-chat-session-file)
                       (buffer-string)))))
             (unless (string-empty-p s) s)))))
    (futon3c-ui-init-buffer
     (list :title "futon3c chat"
           :session-id (or existing-sid
                           (format "%s (awaiting session)" futon3c-chat-agent-id))
           :modeline-fn #'futon3c-chat--build-modeline
           :face-alist `(("claude" . futon3c-chat-claude-face))
           :agent-name "claude"
           :thinking-text "claude is thinking..."
           :thinking-prop 'futon3c-thinking))
    (when existing-sid
      (futon3c-chat--emit-session-start-evidence! existing-sid))))

;;;###autoload
(defun futon3c-chat ()
  "Start or switch to chat."
  (interactive)
  (let ((buf (get-buffer-create futon3c-chat--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'futon3c-chat-mode)
        (futon3c-chat-mode)
        (futon3c-chat--init)))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'futon3c-chat)
;;; futon3c-chat.el ends here
