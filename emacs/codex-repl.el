;;; codex-repl.el --- Chat with Codex via codex exec -*- lexical-binding: t; -*-

;; Author: Codex + Joe
;; Description: Emacs chat buffer backed by `codex exec --json`.
;; Asynchronous: type, RET, Codex responds without blocking Emacs UI.
;; Session continuity via Codex thread id + `codex exec resume <id>`.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/futon3c-ui.el")
;;   (load "/home/joe/code/futon3c/emacs/codex-repl.el")
;;   M-x codex-repl

(require 'cl-lib)
(require 'futon3c-ui)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

;;; Configuration

(defgroup codex-repl nil
  "Chat with Codex via CLI."
  :group 'futon3c-ui)

(defcustom codex-repl-codex-command
  (or (executable-find "codex") "codex")
  "Path to codex CLI."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-session-file "/tmp/futon-codex-session-id"
  "File storing Codex thread/session ID (shared with IRC codex relay)."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-session-id nil
  "Current Codex thread ID for resume.
If nil, no resume is used until Codex returns thread.started."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-sandbox "workspace-write"
  "Sandbox mode passed to `codex exec --sandbox`."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-approval-policy "never"
  "Approval policy passed as `-c approval_policy=\"...\"`."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-model "gpt-5-codex"
  "Model passed to `codex exec --model`.
Set to nil to use the Codex CLI/user config default."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-evidence-url
  (or (getenv "FUTON3C_EVIDENCE_URL")
      "http://localhost:7070/api/alpha/evidence")
  "Evidence API endpoint used to log codex-repl session starts."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-evidence-log-turns t
  "When non-nil, log user/assistant turns into the evidence API."
  :type 'boolean
  :group 'codex-repl)

(defcustom codex-repl-evidence-timeout 1
  "Timeout in seconds for evidence API requests from codex-repl."
  :type 'number
  :group 'codex-repl)

;;; Face (Codex-specific; shared faces are in futon3c-ui)

(defface codex-repl-codex-face
  '((t :foreground "#ff79c6" :weight bold))
  "Face for codex."
  :group 'codex-repl)

;;; Internal state

(defvar codex-repl--buffer-name "*codex-repl*")
(defvar codex-repl--last-emitted-session-id nil
  "Most recent session id emitted to evidence API.")
(defvar codex-repl--last-evidence-id nil
  "Most recent evidence id in the active session chain.")
(defvar codex-repl--evidence-session-id nil
  "Session id associated with `codex-repl--last-evidence-id`.")
(defvar codex-repl--thinking-start-time nil
  "Epoch seconds when the current Codex turn started.")
(defvar codex-repl--last-progress-status nil
  "Most recent short progress status from Codex stream events.")
(defvar codex-repl--thinking-timer nil
  "Timer used to refresh Codex liveness/progress while waiting.")

(defun codex-repl--progress-line (status &optional elapsed-seconds)
  "Render STATUS as a codex thinking progress line.
When ELAPSED-SECONDS is non-nil, include it in the display."
  (if (and (stringp status) (not (string-empty-p status)))
      (if (numberp elapsed-seconds)
          (format "codex is thinking... (%s, %ds)" status elapsed-seconds)
        (format "codex is thinking... (%s)" status))
    "codex is thinking..."))

(defun codex-repl--thinking-elapsed-seconds ()
  "Return elapsed seconds for current Codex turn."
  (if (numberp codex-repl--thinking-start-time)
      (max 0 (floor (- (float-time) codex-repl--thinking-start-time)))
    0))

(defun codex-repl--set-progress-status (status)
  "Update STATUS and return a formatted progress line."
  (setq codex-repl--last-progress-status status)
  (codex-repl--progress-line status (codex-repl--thinking-elapsed-seconds)))

(defun codex-repl--stop-thinking-heartbeat ()
  "Stop Codex liveness heartbeat timer."
  (when (timerp codex-repl--thinking-timer)
    (cancel-timer codex-repl--thinking-timer))
  (setq codex-repl--thinking-timer nil))

(defun codex-repl--start-thinking-heartbeat (chat-buffer)
  "Start periodic liveness updates in CHAT-BUFFER while waiting on Codex."
  (codex-repl--stop-thinking-heartbeat)
  (setq codex-repl--thinking-timer
        (run-at-time
         1 1
         (lambda ()
           (if (not (buffer-live-p chat-buffer))
               (codex-repl--stop-thinking-heartbeat)
             (with-current-buffer chat-buffer
               (if (not (process-live-p futon3c-ui--pending-process))
                   (codex-repl--stop-thinking-heartbeat)
                 (futon3c-ui-update-progress
                  (codex-repl--progress-line
                   (or codex-repl--last-progress-status "working")
                   (codex-repl--thinking-elapsed-seconds))))))))))

(defun codex-repl--ui-state-valid-p ()
  "Return non-nil when futon3c-ui markers/state are usable in this buffer."
  (and (markerp futon3c-ui--prompt-marker)
       (markerp futon3c-ui--separator-start)
       (markerp futon3c-ui--input-start)
       futon3c-ui--agent-name))

(defun codex-repl--apply-ui-state-defaults ()
  "Ensure required futon3c-ui buffer-local state is initialized."
  (setq-local futon3c-ui--face-alist
              (append `(("codex" . codex-repl-codex-face))
                      (list (cons "joe" 'futon3c-ui-joe-face))))
  (setq-local futon3c-ui--agent-name "codex")
  (setq-local futon3c-ui--thinking-text "codex is thinking...")
  (setq-local futon3c-ui--thinking-property 'codex-repl-thinking))

(defun codex-repl--restore-ui-state ()
  "Best-effort repair for stale `*codex-repl*` buffers.
Returns non-nil when prompt markers were restored."
  (codex-repl--apply-ui-state-defaults)
  (let ((prompt-pos nil)
        (separator-pos nil))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^> " nil t)
        (setq prompt-pos (match-end 0)
              separator-pos (line-beginning-position))
        (save-excursion
          (forward-line -1)
          (when (looking-at "^[-─]+$")
            (setq separator-pos (line-beginning-position))))))
    (when prompt-pos
      (setq-local futon3c-ui--prompt-marker (copy-marker separator-pos t))
      (setq-local futon3c-ui--separator-start (copy-marker separator-pos))
      ;; Input-start must stay fixed at prompt boundary while user types.
      (setq-local futon3c-ui--input-start (copy-marker prompt-pos))
      (set-marker-insertion-type futon3c-ui--prompt-marker t)
      (set-marker-insertion-type futon3c-ui--input-start nil)
      t)))

(defun codex-repl--ensure-input-marker-stable! ()
  "Ensure `futon3c-ui--input-start` does not advance while typing."
  (when (and (markerp futon3c-ui--input-start)
             (marker-insertion-type futon3c-ui--input-start))
    (set-marker-insertion-type futon3c-ui--input-start nil)))

;;; Streaming event parser (for progress display)

(defun codex-repl--parse-stream-event (json-line)
  "Parse a Codex JSONL event and return a progress string or nil."
  (condition-case nil
      (let* ((evt (json-parse-string json-line
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
             (type (alist-get 'type evt)))
         (cond
         ((string= type "thread.started")
          (codex-repl--set-progress-status "thread started"))
         ((string= type "item.completed")
          (let* ((item (alist-get 'item evt))
                 (item-type (and (listp item) (alist-get 'type item))))
            (cond
             ((and (stringp item-type) (string= item-type "tool_call"))
              (let ((name (alist-get 'name item)))
                (codex-repl--set-progress-status
                 (if (stringp name)
                     (format "ran: %s" name)
                   "tool call completed"))))
             ((and (stringp item-type) (string= item-type "agent_message"))
              (codex-repl--set-progress-status "composing response..."))
             (t nil))))
         ((string= type "turn.failed")
          (let* ((err (alist-get 'error evt))
                 (msg (and (listp err) (alist-get 'message err))))
            (when (stringp msg)
              (codex-repl--set-progress-status
               (format "error: %s" (truncate-string-to-width msg 60))))))
         ((string= type "error")
          (let ((msg (alist-get 'message evt)))
            (when (stringp msg)
              (codex-repl--set-progress-status
               (format "error: %s" (truncate-string-to-width msg 60))))))
         (t nil)))
    (error nil)))

(defun codex-repl--parse-json-string (text)
  "Parse TEXT as JSON and return a plist/list structure, or nil."
  (when (and (stringp text) (not (string-empty-p text)))
    (condition-case nil
        (if (fboundp 'json-parse-string)
            (json-parse-string text
                               :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)
          (let ((json-object-type 'plist)
                (json-array-type 'list)
                (json-null nil)
                (json-false nil))
            (json-read-from-string text)))
      (error nil))))

(defun codex-repl--evidence-enabled-p ()
  "Return non-nil when evidence logging is configured."
  (and (stringp codex-repl-evidence-url)
       (not (string-empty-p codex-repl-evidence-url))))

(defun codex-repl--evidence-base-url ()
  "Return evidence API base URL without trailing /api/alpha/evidence."
  (let ((value (string-remove-suffix "/" (or codex-repl-evidence-url ""))))
    (replace-regexp-in-string "/api/alpha/evidence\\'" "" value)))

(defun codex-repl--evidence-query-string (pairs)
  "Encode PAIRS as URL query string."
  (mapconcat (lambda (pair)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car pair)))
                       (url-hexify-string (format "%s" (cdr pair)))))
             pairs "&"))

(defun codex-repl--evidence-request-json (method url &optional payload)
  "Send evidence METHOD request to URL with optional JSON PAYLOAD.
Returns plist with keys :status and :json. Returns nil on transport failure."
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data (when payload
                             (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t codex-repl-evidence-timeout)
                   (error nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status) 0)))
          (re-search-forward "\n\n" nil 'move)
          (let* ((body (buffer-substring-no-properties (point) (point-max)))
                 (parsed (codex-repl--parse-json-string body)))
            (kill-buffer buffer)
            (list :status status :json parsed)))))))

(defun codex-repl--evidence-post-entry-id (payload)
  "POST PAYLOAD to evidence API and return created evidence id, or nil."
  (when (codex-repl--evidence-enabled-p)
    (let* ((url (format "%s/api/alpha/evidence" (codex-repl--evidence-base-url)))
           (response (codex-repl--evidence-request-json "POST" url payload))
           (status (plist-get response :status))
           (parsed (plist-get response :json)))
      (when (and (integerp status) (<= 200 status) (< status 300))
        (or (plist-get parsed :evidence/id)
            (plist-get (plist-get parsed :entry) :evidence/id))))))

(defun codex-repl--evidence-fetch-latest-id (sid)
  "Fetch most recent evidence id for session SID."
  (when (and (codex-repl--evidence-enabled-p)
             (stringp sid)
             (not (string-empty-p sid)))
    (let* ((query (codex-repl--evidence-query-string
                   `(("session-id" . ,sid)
                     ("limit" . "1"))))
           (url (format "%s/api/alpha/evidence?%s"
                        (codex-repl--evidence-base-url)
                        query))
           (response (codex-repl--evidence-request-json "GET" url nil))
           (status (plist-get response :status))
           (entries (and (integerp status)
                         (<= 200 status)
                         (< status 300)
                         (plist-get (plist-get response :json) :entries)))
           (entry (and (listp entries) (car entries))))
      (and (listp entry)
           (plist-get entry :evidence/id)))))

(defun codex-repl--sync-evidence-anchor! (&optional sid force)
  "Refresh last evidence anchor from API for SID.
When FORCE is non-nil, refresh even when session is unchanged."
  (let ((target-sid (or sid codex-repl-session-id)))
    (when (and (stringp target-sid) (not (string-empty-p target-sid))
               (or force
                   (not (equal target-sid codex-repl--evidence-session-id))
                   (null codex-repl--last-evidence-id)))
      (setq codex-repl--evidence-session-id target-sid
            codex-repl--last-evidence-id (codex-repl--evidence-fetch-latest-id target-sid)))))

(defun codex-repl--emit-turn-evidence! (role text)
  "Emit a turn evidence event for ROLE (\"user\" or \"assistant\") and TEXT."
  (when (and codex-repl-evidence-log-turns
             (stringp text)
             (not (string-empty-p (string-trim text)))
             (stringp codex-repl-session-id)
             (not (string-empty-p codex-repl-session-id))
             (codex-repl--evidence-enabled-p))
    (codex-repl--sync-evidence-anchor! codex-repl-session-id)
    (let* ((trimmed (string-trim text))
           (is-user (string= role "user"))
           (is-error (string-prefix-p "[Error" trimmed))
           (claim-type (cond
                        (is-user "question")
                        (is-error "correction")
                        (t "observation")))
           (author (if is-user
                       (or (getenv "USER") user-login-name "joe")
                     "codex"))
           (role-tag (if is-user "user" "assistant"))
           (payload `((subject . ((ref/type . "session")
                                  (ref/id . ,codex-repl-session-id)))
                      (type . "coordination")
                      (claim-type . ,claim-type)
                      (author . ,author)
                      (session-id . ,codex-repl-session-id)
                      (body . ((event . "chat-turn")
                               (transport . "emacs-codex-repl")
                               (role . ,role)
                               (text . ,trimmed)))
                      (tags . ["codex" "repl" "turn" ,role-tag]))))
      (when (and (stringp codex-repl--last-evidence-id)
                 (not (string-empty-p codex-repl--last-evidence-id)))
        (setq payload (append payload
                              `((in-reply-to . ,codex-repl--last-evidence-id)))))
      (when-let ((new-id (codex-repl--evidence-post-entry-id payload)))
        (setq codex-repl--evidence-session-id codex-repl-session-id
              codex-repl--last-evidence-id new-id)))))

(defun codex-repl--emit-user-turn-evidence! (text)
  "Emit evidence for user TEXT."
  (codex-repl--emit-turn-evidence! "user" text))

(defun codex-repl--emit-assistant-turn-evidence! (text)
  "Emit evidence for assistant TEXT."
  (codex-repl--emit-turn-evidence! "assistant" text))

;;; Session

(defun codex-repl--refresh-session-header (&optional buffer)
  "Refresh session text in BUFFER header (defaults to current buffer)."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'codex-repl-mode)
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (when (re-search-forward "(session: [^)]+)" (line-end-position 2) t)
                (replace-match (format "(session: %s)"
                                       (or codex-repl-session-id "pending"))
                               t t))
              (goto-char (point-min))
              (when (re-search-forward "emacs-codex-repl (active, session [^)]+)" nil t)
                (replace-match (format "emacs-codex-repl (active, session %s)"
                                       (or codex-repl-session-id "pending"))
                               t t))))))))
        (codex-repl-refresh-header-line nil buf))))

(defun codex-repl--persist-session-id! (sid)
  "Persist SID to `codex-repl-session-file` and local state."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (setq codex-repl-session-id sid)
    (when (not (equal sid codex-repl--evidence-session-id))
      (setq codex-repl--evidence-session-id sid
            codex-repl--last-evidence-id nil))
    (codex-repl--refresh-session-header (get-buffer codex-repl--buffer-name))
    (when codex-repl-session-file
      (write-region sid nil codex-repl-session-file nil 'silent))
    (codex-repl-refresh-header-line t (get-buffer codex-repl--buffer-name))
    (codex-repl--emit-session-start-evidence! sid)))

(defun codex-repl--emit-session-start-evidence! (sid)
  "Emit a lightweight session-start evidence entry for SID."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (unless (equal sid codex-repl--evidence-session-id)
      (setq codex-repl--evidence-session-id sid
            codex-repl--last-evidence-id nil))
    (codex-repl--sync-evidence-anchor! sid t)
    (when (and (not (equal sid codex-repl--last-emitted-session-id))
               (not (and (stringp codex-repl--last-evidence-id)
                         (not (string-empty-p codex-repl--last-evidence-id))))
               (codex-repl--evidence-enabled-p))
      (let ((payload `((subject . ((ref/type . "session")
                                   (ref/id . ,sid)))
                       (type . "coordination")
                       (claim-type . "goal")
                       (author . ,(or (getenv "USER") user-login-name "codex"))
                       (session-id . ,sid)
                       (body . ((event . "session-start")
                                (source . "codex-repl")
                                (mode . "emacs")))
                       (tags . ["codex" "session-start" "repl"]))))
        (when-let ((new-id (codex-repl--evidence-post-entry-id payload)))
          (setq codex-repl--last-evidence-id new-id))
        (setq codex-repl--last-emitted-session-id sid)))
    (unless (and (stringp codex-repl--last-evidence-id)
                 (not (string-empty-p codex-repl--last-evidence-id)))
      (codex-repl--sync-evidence-anchor! sid t))))

(defun codex-repl--ensure-session-id ()
  "Load existing Codex session id from file, if present."
  (futon3c-ui-ensure-session-id
   codex-repl-session-file
   codex-repl-session-id
   (lambda (sid)
     (setq codex-repl-session-id sid)
     (codex-repl--emit-session-start-evidence! sid))))

;;; Codex output parsing

(defun codex-repl--extract-agent-text (item)
  "Extract assistant text from a Codex item.completed payload ITEM."
  (let ((content (or (alist-get 'text item)
                     (alist-get 'content item))))
    (cond
      ((stringp content)
       content)
      ((listp content)
       (string-join
        (delq nil
              (mapcar (lambda (part)
                        (when (and (listp part)
                                   (string= (alist-get 'type part) "text"))
                          (alist-get 'text part)))
                      content))
        ""))
      (t nil))))

(defun codex-repl--parse-codex-json-output (output)
  "Parse OUTPUT JSONL from `codex exec --json`.
Returns plist: (:session-id sid :text response :error err)."
  (let ((sid codex-repl-session-id)
        (last-message nil)
        (last-error nil))
    (dolist (line (split-string output "\n" t))
      (condition-case _
          (let* ((evt (json-parse-string line
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil))
                 (type (alist-get 'type evt)))
            (cond
              ((string= type "thread.started")
               (let ((thread-id (or (alist-get 'thread_id evt)
                                    (alist-get 'session_id evt))))
                 (when (and (stringp thread-id) (not (string-empty-p thread-id)))
                   (setq sid thread-id))))
              ((string= type "error")
               (let ((msg (alist-get 'message evt)))
                 (when (and (stringp msg) (not (string-empty-p msg)))
                   (setq last-error msg))))
              ((string= type "turn.failed")
               (let* ((err (alist-get 'error evt))
                      (msg (and (listp err) (alist-get 'message err))))
                 (when (and (stringp msg) (not (string-empty-p msg)))
                   (setq last-error msg))))
              ((string= type "item.completed")
               (let* ((item (alist-get 'item evt))
                      (item-type (and (listp item) (alist-get 'type item))))
                 (when (and (stringp item-type) (string= item-type "agent_message"))
                   (let ((msg (codex-repl--extract-agent-text item)))
                     (when (and (stringp msg) (not (string-empty-p msg)))
                       (setq last-message msg))))))))
        (error nil)))
    (list :session-id sid
          :text (or last-message
                    (string-trim output)
                    "[No assistant message returned]")
          :error last-error)))

;;; Codex CLI call

(defun codex-repl--build-codex-args (session-id)
  "Build argv for `codex exec --json` using SESSION-ID when present."
  (let* ((exec-args (list "exec"
                          "--json"
                          "--sandbox" codex-repl-sandbox
                          "-c" (format "approval_policy=\"%s\"" codex-repl-approval-policy)))
         (exec-args (if (and codex-repl-model (not (string-empty-p codex-repl-model)))
                        (append exec-args (list "--model" codex-repl-model))
                      exec-args)))
    (if (and session-id (not (string-empty-p session-id)))
        (append exec-args (list "resume" session-id "-"))
      (append exec-args (list "-")))))

(defun codex-repl--call-codex-async (text callback)
  "Call `codex exec --json` with TEXT asynchronously.
Invoke CALLBACK with the final response text."
  (let* ((args (codex-repl--build-codex-args codex-repl-session-id))
         (repl-buffer (current-buffer))
         (outbuf (generate-new-buffer " *codex-repl-codex*"))
         (process-environment process-environment)
         (proc nil)
         (payload (if (string-suffix-p "\n" text) text (concat text "\n"))))
    (setq codex-repl--thinking-start-time (float-time)
          codex-repl--last-progress-status "starting")
    (setq proc
          (make-process
           :name "codex-repl-codex"
           :buffer outbuf
           :command (cons codex-repl-codex-command args)
           :noquery t
           :connection-type 'pipe
           :filter (futon3c-ui-make-streaming-filter
                    #'codex-repl--parse-stream-event
                    repl-buffer)
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let* ((exit-code (process-exit-status p))
                      (raw (with-current-buffer (process-buffer p)
                             (buffer-string)))
                      (parsed (codex-repl--parse-codex-json-output raw))
                      (sid (plist-get parsed :session-id))
                      (response (plist-get parsed :text))
                      (err (plist-get parsed :error))
                      (final-text (if (= exit-code 0)
                                      (string-trim response)
                                    (format "[Error (exit %d): %s]"
                                            exit-code
                                            (string-trim (or err response))))))
                 (when (and (stringp sid) (not (string-empty-p sid)))
                   (codex-repl--persist-session-id! sid))
                 (codex-repl--stop-thinking-heartbeat)
                 (setq codex-repl--thinking-start-time nil
                       codex-repl--last-progress-status nil)
                 (when (buffer-live-p (process-buffer p))
                   (kill-buffer (process-buffer p)))
                 (when (buffer-live-p repl-buffer)
                   (with-current-buffer repl-buffer
                     (when (eq futon3c-ui--pending-process p)
                       (setq futon3c-ui--pending-process nil))
                     (funcall callback final-text))))))))
    (codex-repl--start-thinking-heartbeat repl-buffer)
    (process-send-string proc payload)
    (process-send-eof proc)
    proc))

;;; Modeline

(defvar codex-repl--last-modeline-state nil
  "Cache of the most recently computed modeline state.")

(defun codex-repl--compute-modeline-state ()
  "Return plist describing current transport/modeline state."
  (let* ((session (or codex-repl-session-id "pending"))
         (irc-up (futon3c-ui-irc-available-p))
         (transports
          (list (list :key 'codex-repl
                      :label (format "emacs-codex-repl (active, session %s)" session)
                      :status 'active
                      :session session)
                (list :key 'cli
                      :label "cli (codex exec --json)"
                      :status 'available)
                (list :key 'irc
                      :label (if irc-up
                                 "irc (#futon :6667, available)"
                               "irc (#futon :6667, offline)")
                      :status (if irc-up 'available 'offline)))))
    (list :current 'codex-repl
          :current-label "emacs-codex-repl"
          :session-id session
          :irc-available? irc-up
          :timestamp (current-time)
          :transports transports)))

(defun codex-repl-modeline-state (&optional refresh)
  "Return current modeline state plist.
With REFRESH non-nil, recompute the state even if cached."
  (when (or refresh (null codex-repl--last-modeline-state))
    (setq codex-repl--last-modeline-state (codex-repl--compute-modeline-state)))
  codex-repl--last-modeline-state)

(defun codex-repl--render-modeline (state)
  "Render STATE plist into a human-readable modeline string."
  (let* ((entries (plist-get state :transports))
         (labels (mapcar (lambda (entry) (plist-get entry :label)) entries))
         (current (plist-get state :current-label)))
    (format "Available transports: [%s]. Current: %s."
            (string-join labels ", ")
            current)))

(defun codex-repl--world-view-string (state)
  "Return multi-line description of STATE plist."
  (let* ((session (plist-get state :session-id))
         (irc? (if (plist-get state :irc-available?) "up" "down"))
         (timestamp (plist-get state :timestamp))
         (time-str (format-time-string "%Y-%m-%d %H:%M:%S %Z" timestamp))
         (current (plist-get state :current-label))
         (lines (list (format "Codex REPL world @ %s" time-str)
                      (format "  Session: %s" session)
                      (format "  Current transport: %s" current)
                      (format "  IRC relay: %s" irc?)
                      "  Transports:")))
    (dolist (entry (plist-get state :transports))
      (setq lines
            (append lines
                    (list (format "    - %-8s status=%s"
                                  (plist-get entry :key)
                                  (plist-get entry :status))))))
    (string-join lines "\n")))

(defun codex-repl--header-line ()
  "Return header line string summarizing Codex transport state."
  (let* ((state (codex-repl-modeline-state))
         (session (plist-get state :session-id))
         (irc (if (plist-get state :irc-available?) "irc:up" "irc:down"))
         (current (plist-get state :current-label))
         (transports (mapconcat (lambda (entry)
                                  (format "%s:%s"
                                          (plist-get entry :key)
                                          (plist-get entry :status)))
                                (plist-get state :transports)
                                ", ")))
    (format "Codex session %s | current=%s | %s | transports[%s]"
            session current irc transports)))

(defun codex-repl-refresh-header-line (&optional refresh buffer)
  "Refresh Codex modeline header.
With REFRESH non-nil, recompute transport state. Optionally target BUFFER.
Default buffer is current buffer."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'codex-repl-mode)
          (codex-repl-modeline-state refresh)
          (force-mode-line-update t))))))

(defun codex-repl--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (codex-repl--render-modeline (codex-repl-modeline-state t)))

(defun codex-repl-describe-modeline (&optional refresh)
  "Display current modeline string in the echo area.
With REFRESH non-nil, recompute transport state before rendering."
  (interactive "P")
  (message "%s" (codex-repl--render-modeline (codex-repl-modeline-state refresh))))

(defun codex-repl-copy-modeline (&optional refresh)
  "Copy current modeline string to the kill ring.
With REFRESH non-nil, recompute transport state before copying."
  (interactive "P")
  (let ((text (codex-repl--render-modeline (codex-repl-modeline-state refresh))))
    (kill-new text)
    (message "Copied Codex modeline to kill ring.")))

(defun codex-repl-describe-world (&optional refresh)
  "Describe current Codex transport world in the echo area.
With REFRESH non-nil, recompute state first."
  (interactive "P")
  (message "%s"
           (codex-repl--world-view-string
            (codex-repl-modeline-state refresh))))

(defun codex-repl-insert-world-view (&optional refresh)
  "Insert current Codex transport world description at point.
With REFRESH non-nil, recompute state first."
  (interactive "P")
  (insert (codex-repl--world-view-string
           (codex-repl-modeline-state refresh))
          "\n"))

;;; Mode

(defvar codex-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-repl-send-input)
    (define-key map (kbd "C-c C-k") #'codex-repl-clear)
    map))

(define-derived-mode codex-repl-mode nil "Codex-REPL"
  "Chat with Codex via CLI.
Type after the prompt, RET to send.
\\{codex-repl-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local header-line-format '(:eval (codex-repl--header-line)))
  (codex-repl-refresh-header-line t))

(defun codex-repl-send-input ()
  "Send input to Codex and display response."
  (interactive)
  (codex-repl--ensure-input-marker-stable!)
  (futon3c-ui-send-input
   #'codex-repl--call-codex-async
   "codex"
   (list :before-send #'codex-repl--emit-user-turn-evidence!
         :on-response #'codex-repl--emit-assistant-turn-evidence!)))

(defun codex-repl-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (codex-repl--stop-thinking-heartbeat)
  (setq codex-repl--thinking-start-time nil
        codex-repl--last-progress-status nil)
  (futon3c-ui-clear #'codex-repl--init))

(defun codex-repl--init ()
  "Initialize buffer UI."
  (codex-repl--ensure-session-id)
  (futon3c-ui-init-buffer
   (list :title "codex repl"
         :session-id (or codex-repl-session-id "pending")
         :modeline-fn #'codex-repl--build-modeline
         :face-alist `(("codex" . codex-repl-codex-face))
         :agent-name "codex"
         :thinking-text "codex is thinking..."
         :thinking-prop 'codex-repl-thinking)))

;;;###autoload
(defun codex-repl ()
  "Start or switch to Codex REPL."
  (interactive)
  (let ((buf (get-buffer-create codex-repl--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'codex-repl-mode)
        (codex-repl-mode)
        (codex-repl--init))
      (unless (codex-repl--ui-state-valid-p)
        (unless (codex-repl--restore-ui-state)
          (let ((inhibit-read-only t))
            (erase-buffer))
          (codex-repl--init))
        (codex-repl--refresh-session-header (current-buffer))))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'codex-repl)
;;; codex-repl.el ends here
