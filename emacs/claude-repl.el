;;; claude-repl.el --- Chat with Claude via futon3c API -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs chat buffer backed by futon3c's /api/alpha/invoke endpoint.
;; Asynchronous: type, RET, Claude responds without blocking Emacs UI.
;; Routes through the agent registry — same invoke-fn as IRC.
;; Logs every turn to the evidence landscape (same pattern as codex-repl.el).
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/agent-chat.el")
;;   (load "/home/joe/code/futon3c/emacs/claude-repl.el")
;;   M-x claude-repl

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'agent-chat)

;;; Configuration

(defgroup claude-repl nil
  "Chat with Claude via futon3c API."
  :group 'agent-chat)

(defcustom claude-repl-api-url "http://localhost:7070"
  "Base URL for the futon3c API server."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-agent-id "claude-1"
  "Agent ID to invoke via the API."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-session-file "/tmp/futon-session-id"
  "File storing Claude session ID (shared with IRC relay)."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-evidence-url
  (or (getenv "FUTON3C_EVIDENCE_URL")
      (format "%s/api/alpha/evidence"
              (string-remove-suffix "/" agent-chat-agency-base-url)))
  "Evidence API endpoint for logging chat turns."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-evidence-log-turns t
  "When non-nil, log user/assistant turns into the evidence API."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-evidence-timeout 1
  "Timeout in seconds for evidence API requests."
  :type 'number
  :group 'claude-repl)

;;; Face (Claude-specific; shared faces are in agent-chat)

(defface claude-repl-claude-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Claude."
  :group 'claude-repl)

;;; Internal state

(defvar claude-repl--buffer-name "*claude-repl*")
(defvar claude-repl--last-evidence-id nil
  "Last evidence entry ID, used for in-reply-to chaining.")
(defvar claude-repl--evidence-session-id nil
  "Session ID associated with `claude-repl--last-evidence-id'.")
(defvar claude-repl--last-emitted-session-id nil
  "Last session ID for which a session-start evidence entry was emitted.")

;;; Auto-registration

(defun claude-repl--auto-register ()
  "Ask the futon3c server to auto-register the next available Claude agent.
Returns the assigned agent-id string, or nil on failure."
  (let* ((url (concat claude-repl-api-url "/api/alpha/agents/auto"))
         (json-body (json-serialize '(:type "claude")))
         (result (with-temp-buffer
                   (let ((exit (call-process "curl" nil t nil
                                             "-sS" "--max-time" "5"
                                             "-H" "Content-Type: application/json"
                                             "-d" json-body url)))
                     (when (= exit 0)
                       (goto-char (point-min))
                       (condition-case nil
                           (json-parse-buffer :object-type 'alist)
                         (error nil)))))))
    (when (and result (alist-get 'ok result))
      (let ((agent-id (alist-get 'agent-id result)))
        (when (and (stringp agent-id) (not (string-empty-p agent-id)))
          (setq claude-repl-agent-id agent-id)
          (setq claude-repl-session-file
                (format "/tmp/futon-session-id-%s" agent-id))
          (message "claude-repl: registered as %s" agent-id)
          agent-id)))))

;;; Surface contract

(defun claude-repl--surface-contract ()
  "Return surface metadata — where the output goes."
  (format "Surface: emacs-claude-repl | Agent: %s" claude-repl-agent-id))

;;; Evidence logging

(defun claude-repl--evidence-enabled-p ()
  "Return non-nil when evidence logging is configured."
  (and (stringp claude-repl-evidence-url)
       (not (string-empty-p claude-repl-evidence-url))))

(defun claude-repl--evidence-base-url ()
  "Return evidence API base URL without trailing /api/alpha/evidence."
  (let ((value (string-remove-suffix "/" (or claude-repl-evidence-url ""))))
    (replace-regexp-in-string "/api/alpha/evidence\\'" "" value)))

(defun claude-repl--evidence-request-json (method url &optional payload)
  "Send evidence METHOD request to URL with optional JSON PAYLOAD.
Returns plist with keys :status and :json.  Returns nil on transport failure."
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data (when payload
                             (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t claude-repl-evidence-timeout)
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

(defun claude-repl--evidence-post-entry-id (payload)
  "POST PAYLOAD to evidence API and return created evidence id, or nil."
  (when (claude-repl--evidence-enabled-p)
    (let* ((url (format "%s/api/alpha/evidence" (claude-repl--evidence-base-url)))
           (response (claude-repl--evidence-request-json "POST" url payload))
           (status (plist-get response :status))
           (parsed (plist-get response :json)))
      (when (and (integerp status) (<= 200 status) (< status 300))
        (or (plist-get parsed :evidence/id)
            (plist-get (plist-get parsed :entry) :evidence/id))))))

(defun claude-repl--evidence-fetch-latest-id (sid)
  "Fetch most recent evidence id for session SID."
  (when (and (claude-repl--evidence-enabled-p)
             (stringp sid)
             (not (string-empty-p sid)))
    (let* ((query (format "session-id=%s&limit=1"
                          (url-hexify-string sid)))
           (url (format "%s/api/alpha/evidence?%s"
                        (claude-repl--evidence-base-url)
                        query))
           (response (claude-repl--evidence-request-json "GET" url nil))
           (status (plist-get response :status))
           (entries (and (integerp status)
                         (<= 200 status)
                         (< status 300)
                         (plist-get (plist-get response :json) :entries)))
           (entry (and (listp entries) (car entries))))
      (and (listp entry)
           (plist-get entry :evidence/id)))))

(defun claude-repl--sync-evidence-anchor! (&optional sid force)
  "Refresh last evidence anchor from API for SID.
When FORCE is non-nil, refresh even when session is unchanged."
  (let ((target-sid (or sid agent-chat--session-id)))
    (when (and (stringp target-sid) (not (string-empty-p target-sid))
               (or force
                   (not (equal target-sid claude-repl--evidence-session-id))
                   (null claude-repl--last-evidence-id)))
      (setq claude-repl--evidence-session-id target-sid
            claude-repl--last-evidence-id (claude-repl--evidence-fetch-latest-id target-sid)))))

(defun claude-repl--emit-session-start-evidence! (sid)
  "Emit a lightweight session-start evidence entry for SID."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (unless (equal sid claude-repl--evidence-session-id)
      (setq claude-repl--evidence-session-id sid
            claude-repl--last-evidence-id nil))
    (claude-repl--sync-evidence-anchor! sid t)
    (when (and (not (equal sid claude-repl--last-emitted-session-id))
               (not (and (stringp claude-repl--last-evidence-id)
                         (not (string-empty-p claude-repl--last-evidence-id))))
               (claude-repl--evidence-enabled-p))
      (let ((payload `((subject . ((ref/type . "session")
                                   (ref/id . ,sid)))
                       (type . "coordination")
                       (claim-type . "goal")
                       (author . ,(or (getenv "USER") user-login-name "joe"))
                       (session-id . ,sid)
                       (body . ((event . "session-start")
                                (source . "claude-repl")
                                (mode . "emacs")))
                       (tags . ["claude" "session-start" "chat"]))))
        (when-let ((new-id (claude-repl--evidence-post-entry-id payload)))
          (setq claude-repl--last-evidence-id new-id))
        (setq claude-repl--last-emitted-session-id sid)))
    (unless (and (stringp claude-repl--last-evidence-id)
                 (not (string-empty-p claude-repl--last-evidence-id)))
      (claude-repl--sync-evidence-anchor! sid t))))

(defun claude-repl--emit-turn-evidence! (role text)
  "Emit a turn evidence event for ROLE (\"user\" or \"assistant\") and TEXT."
  (let ((sid agent-chat--session-id))
    (when (and claude-repl-evidence-log-turns
               (stringp text)
               (not (string-empty-p (string-trim text)))
               (stringp sid)
               (not (string-empty-p sid))
               (claude-repl--evidence-enabled-p))
      (claude-repl--sync-evidence-anchor! sid)
      (let* ((trimmed (string-trim text))
             (is-user (string= role "user"))
             (is-error (string-prefix-p "[Error" trimmed))
             (claim-type (cond
                          (is-user "question")
                          (is-error "correction")
                          (t "observation")))
             (author (if is-user
                         (or (getenv "USER") user-login-name "joe")
                       claude-repl-agent-id))
             (role-tag (if is-user "user" "assistant"))
             (payload `((subject . ((ref/type . "session")
                                    (ref/id . ,sid)))
                        (type . "coordination")
                        (claim-type . ,claim-type)
                        (author . ,author)
                        (session-id . ,sid)
                        (body . ((event . "chat-turn")
                                 (transport . "emacs-claude-repl")
                                 (role . ,role)
                                 (text . ,trimmed)))
                        (tags . ["claude" "chat" "turn" ,role-tag]))))
        (when (and (stringp claude-repl--last-evidence-id)
                   (not (string-empty-p claude-repl--last-evidence-id)))
          (setq payload (append payload
                                `((in-reply-to . ,claude-repl--last-evidence-id)))))
        (when-let ((new-id (claude-repl--evidence-post-entry-id payload)))
          (setq claude-repl--evidence-session-id sid
                claude-repl--last-evidence-id new-id))))))

(defun claude-repl--emit-user-turn-evidence! (text)
  "Emit evidence for user TEXT."
  (claude-repl--emit-turn-evidence! "user" text))

(defun claude-repl--emit-assistant-turn-evidence! (text)
  "Emit evidence for assistant TEXT."
  (claude-repl--emit-turn-evidence! "assistant" text))

;;; Claude API call

(defun claude-repl--call-claude-async (text callback)
  "Send TEXT to Claude via POST /api/alpha/invoke.
Uses curl to POST to the futon3c API server. The server's invoke-fn
calls `claude -p' with the correct session-id (managed by the registry).
CALLBACK receives the response string."
  (let* ((chat-buffer (current-buffer))
         (url (concat claude-repl-api-url "/api/alpha/invoke"))
         (full-prompt (format "%s\n\nUser message:\n%s"
                              (claude-repl--surface-contract) text))
         (json-body (json-serialize
                     `(:agent-id ,claude-repl-agent-id
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
               ;; If interrupted (pending-process already cleared), just clean up
               (if (and (eq (process-status p) 'signal)
                        (not (eq agent-chat--pending-process p)))
                   (when (buffer-live-p (process-buffer p))
                     (kill-buffer (process-buffer p)))
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
                                       (agent-chat-update-session-id sid))
                                     (when claude-repl-session-file
                                       (write-region sid nil claude-repl-session-file nil 'silent))
                                     ;; Emit session-start evidence on first successful response
                                     (claude-repl--emit-session-start-evidence! sid))
                                   (if ok
                                       (let ((r (and (stringp result)
                                                     (not (string-empty-p (string-trim result)))
                                                     result)))
                                         (or r "[empty response]"))
                                     (message "claude-repl invoke error: %s"
                                              (truncate-string-to-width
                                               (or err-msg raw) 500))
                                     (let ((msg (or err-msg raw)))
                                       (if (and (stringp msg)
                                                (string-match-p
                                                 "\\`\\(Exit [0-9]+:\\|invoke-error\\)"
                                                 msg)
                                                (< (length (string-trim msg)) 20))
                                           (format "[Error: %s — try C-c C-n for fresh session]"
                                                   msg)
                                         (format "[Error: %s]" msg)))))
                               (error
                                (format "[JSON parse error: %s\nRaw: %s]"
                                        (error-message-string parse-err)
                                        (truncate-string-to-width raw 200)))))))
                     (message "claude-repl: exit=%d raw-len=%d" exit-code (length raw))
                     (when (buffer-live-p (process-buffer p))
                       (kill-buffer (process-buffer p)))
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (when (eq agent-chat--pending-process p)
                           (setq agent-chat--pending-process nil))
                         (funcall callback response))))
                 (error
                  (message "claude-repl sentinel error: %s" (error-message-string err))
                  (when (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (setq agent-chat--pending-process nil)
                      (agent-chat-remove-thinking)
                      (agent-chat-insert-message
                       "claude"
                       (format "[Sentinel error: %s]" (error-message-string err))))))))))))
    proc))

;;; Modeline

(defun claude-repl--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list "emacs-chat (active, via /api/alpha/invoke)"))
        (irc-up (agent-chat-irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (claude code)" transports)
    (format "Available transports: [%s]. Current: emacs-chat."
            (string-join (reverse transports) ", "))))

;;; Mode

(defvar claude-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-repl-send-input)
    (define-key map (kbd "C-c C-c") #'agent-chat-interrupt)
    (define-key map (kbd "C-c C-k") #'claude-repl-clear)
    (define-key map (kbd "C-c C-n") #'claude-repl-new-session)
    map))

(define-derived-mode claude-repl-mode nil "Claude-REPL"
  "Chat with Claude via futon3c API.
Type after the prompt, RET to send, C-c C-n for fresh session.
\\{claude-repl-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0))

(defun claude-repl-send-input ()
  "Send input to Claude and display response."
  (interactive)
  (agent-chat-send-input
   #'claude-repl--call-claude-async
   "claude"
   (list :before-send #'claude-repl--emit-user-turn-evidence!
         :on-response #'claude-repl--emit-assistant-turn-evidence!)))

(defun claude-repl-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (agent-chat-clear #'claude-repl--init))

(defcustom claude-repl-drawbridge-url "http://localhost:6768"
  "Drawbridge REPL URL for direct registry access."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawbridge-token nil
  "Admin token for Drawbridge REPL.
If nil, reads from .admintoken in the project root at first use."
  :type '(choice (const nil) string)
  :group 'claude-repl)

(defun claude-repl--drawbridge-token ()
  "Return the Drawbridge admin token, reading .admintoken if needed."
  (or claude-repl-drawbridge-token
      (let ((f (expand-file-name ".admintoken"
                                 (locate-dominating-file default-directory ".admintoken"))))
        (when (file-exists-p f)
          (setq claude-repl-drawbridge-token
                (string-trim (with-temp-buffer
                               (insert-file-contents f)
                               (buffer-string))))))))

(defun claude-repl--reset-via-api ()
  "Try to reset session via the reset-session HTTP endpoint.
Returns (ok . old-session-id) on success, nil on failure."
  (let* ((url (format "%s/api/alpha/agents/%s/reset-session"
                       claude-repl-api-url claude-repl-agent-id))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data "{}")
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t 10)
                   (error nil)))
         (result nil))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (condition-case nil
              (let* ((json-obj (json-parse-string
                                (buffer-substring (point) (point-max))
                                :object-type 'alist :null-object nil)))
                (when (alist-get 'ok json-obj)
                  (setq result (cons t (alist-get 'old-session-id json-obj)))))
            (error nil))))
      (kill-buffer buffer))
    result))

(defun claude-repl--reset-via-drawbridge ()
  "Try to reset session via Drawbridge REPL eval.
Returns (ok . old-session-id) on success, nil on failure."
  (let* ((clj-code (format "(let [r (swap-vals! futon3c.agency.registry/!registry update \"%s\" assoc :agent/session-id nil)] (get-in (first r) [\"%s\" :agent/session-id]))"
                           claude-repl-agent-id claude-repl-agent-id))
         (url (format "%s/eval" claude-repl-drawbridge-url))
         (url-request-method "POST")
         (token (claude-repl--drawbridge-token))
         (url-request-extra-headers
          `(("x-admin-token" . ,token)
            ("Content-Type" . "text/plain")))
         (url-request-data (encode-coding-string clj-code 'utf-8))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t 10)
                   (error nil)))
         (result nil))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (condition-case nil
              (let* ((json-obj (json-parse-string
                                (buffer-substring (point) (point-max))
                                :object-type 'alist :null-object nil)))
                (when (alist-get 'ok json-obj)
                  (setq result (cons t (alist-get 'value json-obj)))))
            (error nil))))
      (kill-buffer buffer))
    result))

(defun claude-repl-new-session ()
  "Reset the agent session so the next message starts a fresh conversation.
Useful when a session becomes poisoned (e.g. API rejects the conversation
history). Tries the reset-session endpoint first, falls back to Drawbridge."
  (interactive)
  (let* ((api-result (claude-repl--reset-via-api))
         (result (or api-result (claude-repl--reset-via-drawbridge)))
         (ok (car result))
         (old-sid (cdr result)))
    ;; Clear local session state regardless of server response
    (setq agent-chat--session-id nil)
    (when claude-repl-session-file
      (when (file-exists-p claude-repl-session-file)
        (delete-file claude-repl-session-file)))
    ;; Update the buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "(session: [^)]*)" (line-end-position 2) t)
          (replace-match (propertize "(session: new)"
                                     'face 'font-lock-comment-face)
                         t t))))
    (agent-chat-insert-message
     "system"
     (cond
      (ok
       (format "[Session reset — was %s. Next message starts fresh.]"
               (or old-sid "unknown")))
      (t
       "[Session reset locally only — could not reach server. Next message may still fail.]")))
    (goto-char (point-max))
    (message "claude-repl: session reset (server=%s, was %s)"
             (if ok "yes" "no") (or old-sid "nil"))))

(defun claude-repl--init ()
  "Initialize.
Auto-register with the server to get a unique agent-id, then
load existing session-id from file if available."
  ;; Auto-register: get next available claude-N from the server
  (claude-repl--auto-register)
  (let ((existing-sid
         (when (and claude-repl-session-file
                    (file-exists-p claude-repl-session-file))
           (let ((s (string-trim
                     (with-temp-buffer
                       (insert-file-contents claude-repl-session-file)
                       (buffer-string)))))
             (unless (string-empty-p s) s)))))
    (agent-chat-init-buffer
     (list :title "claude repl"
           :session-id (or existing-sid
                           (format "%s (awaiting session)" claude-repl-agent-id))
           :modeline-fn #'claude-repl--build-modeline
           :face-alist `(("claude" . claude-repl-claude-face))
           :agent-name "claude"
           :thinking-text "claude is thinking..."
           :thinking-prop 'claude-repl-thinking))
    (when existing-sid
      (claude-repl--emit-session-start-evidence! existing-sid))))

;;;###autoload
(defun claude-repl ()
  "Start or switch to chat."
  (interactive)
  (let ((buf (get-buffer-create claude-repl--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-repl-mode)
        (claude-repl-mode)
        (claude-repl--init)))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'claude-repl)
;;; claude-repl.el ends here
