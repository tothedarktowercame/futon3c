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

(defcustom codex-repl-sandbox "danger-full-access"
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

(defcustom codex-repl-reasoning-effort "high"
  "Reasoning effort passed as `-c model_reasoning_effort=\"...\"`.
Set to nil to use the Codex CLI/user config default."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-chat-preamble nil
  "Optional instruction prepended to each Codex REPL turn.
Set to nil to disable and send raw user text."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-evidence-url
  (or (getenv "FUTON3C_EVIDENCE_URL")
      (format "%s/api/alpha/evidence"
              (string-remove-suffix "/" futon3c-ui-agency-base-url)))
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

(defcustom codex-repl-irc-send-base-url
  (or (getenv "FUTON3C_IRC_SEND_BASE")
      (getenv "FUTON3C_LINODE_URL"))
  "Optional fallback Agency base URL for IRC send requests.
When local Agency returns irc-unavailable (503), codex-repl retries against this base."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-invoke-buffer-name "*invoke: codex-repl*"
  "Buffer used for live Codex stream/invoke trace output."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-show-invoke-buffer t
  "When non-nil, keep a side-window open with live invoke trace."
  :type 'boolean
  :group 'codex-repl)

(defcustom codex-repl-invoke-window-side 'right
  "Side used when displaying the invoke trace buffer."
  :type '(choice (const left) (const right) (const bottom) (const top))
  :group 'codex-repl)

(defcustom codex-repl-invoke-window-size 0.33
  "Window size for invoke trace side window.
Interpreted as width on left/right and height on top/bottom."
  :type 'number
  :group 'codex-repl)

(defcustom codex-repl-invoke-log-raw-events nil
  "When non-nil, include raw NDJSON lines in invoke trace output."
  :type 'boolean
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
(defvar codex-repl--cached-irc-send-base nil
  "Cached remote Agency base URL hint for IRC send fallback.")
(defvar codex-repl--invoke-turn-id 0
  "Monotonic local counter for codex-repl invoke turns.")

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

(defun codex-repl--invoke-buffer ()
  "Return (and initialize) the Codex invoke trace buffer."
  (let ((buf (get-buffer-create codex-repl-invoke-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (setq-local truncate-lines t))
    buf))

(defun codex-repl--display-invoke-buffer ()
  "Display invoke trace side-window if enabled."
  (when codex-repl-show-invoke-buffer
    (let* ((buf (codex-repl--invoke-buffer))
           (side codex-repl-invoke-window-side)
           (size-pair (if (memq side '(left right))
                          (cons 'window-width codex-repl-invoke-window-size)
                        (cons 'window-height codex-repl-invoke-window-size)))
           (params (list (cons 'side side)
                         size-pair
                         (cons 'slot 1))))
      (display-buffer-in-side-window buf params))))

(defun codex-repl--truncate-single-line (text max-len)
  "Return TEXT collapsed to one line and truncated to MAX-LEN chars."
  (let* ((collapsed (replace-regexp-in-string "[\n\r\t]+" " " (or text "")))
         (trimmed (string-trim collapsed)))
    (if (> (length trimmed) max-len)
        (concat (substring trimmed 0 max-len) "...")
      trimmed)))

(defun codex-repl--append-invoke-trace (line &optional face)
  "Append LINE to invoke trace buffer, optionally with FACE."
  (when (and (stringp line) (not (string-empty-p line)))
    (let ((buf (codex-repl--invoke-buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize
                   (format "[%s] %s\n" (format-time-string "%H:%M:%S") line)
                   'face (or face 'default))))))))

(defun codex-repl--titleize-token (token)
  "Return TOKEN converted from snake_case/kebab-case to Title Case."
  (if (and (stringp token) (not (string-empty-p token)))
      (mapconcat #'capitalize
                 (split-string (replace-regexp-in-string "[-_]+" " " token) " " t)
                 " ")
    "Unknown"))

(defun codex-repl--humanize-tool-name (name)
  "Return a human-friendly label for Codex tool NAME."
  (let ((tool (and (stringp name) (downcase name))))
    (cond
     ((member tool '("command_execution" "command-execution" "bash" "shell"))
      "Using Bash")
     ((member tool '("read_file" "read-files"))
      "Reading Files")
     ((member tool '("write_file" "edit_file" "apply_patch"))
      "Editing Files")
     ((member tool '("search" "grep" "ripgrep"))
      "Searching Code")
     ((member tool '("list_files" "list_directory"))
      "Inspecting Files")
     ((stringp tool)
      (format "Using %s" (codex-repl--titleize-token tool)))
     (t "Using Tool"))))

(defun codex-repl--humanize-item-type (item-type)
  "Return a human-friendly label for stream ITEM-TYPE."
  (let ((kind (and (stringp item-type) (downcase item-type))))
    (cond
     ((member kind '("reasoning" "agent_message")) "Preparing Response")
     ((stringp kind) (codex-repl--titleize-token kind))
     (t "Working"))))

(defun codex-repl--stream-event-summary (evt)
  "Render one-line summary for Codex stream event EVT."
  (let ((type (alist-get 'type evt)))
    (cond
     ((string= type "thread.started")
      (let ((sid (or (alist-get 'thread_id evt)
                     (alist-get 'session_id evt)
                     "?")))
        (format "thread.started session=%s" sid)))
     ((or (string= type "item.started")
          (string= type "item.completed"))
      (let* ((item (alist-get 'item evt))
             (item-type (and (listp item) (alist-get 'type item)))
             (name (and (listp item) (alist-get 'name item))))
        (cond
         ((and (stringp item-type) (string= item-type "tool_call"))
          (format "%s (%s)"
                  (codex-repl--humanize-tool-name name)
                  (if (string= type "item.started") "started" "done")))
         ((stringp item-type)
          (format "%s (%s)"
                  (codex-repl--humanize-item-type item-type)
                  (if (string= type "item.started") "started" "done")))
         (t type))))
     ((string= type "reasoning")
      "Preparing Response")
     ((string= type "command_execution")
      "Using Bash")
     ((string= type "turn.failed")
      (let* ((err (alist-get 'error evt))
             (msg (and (listp err) (alist-get 'message err))))
        (if (stringp msg)
            (format "turn.failed %s" (truncate-string-to-width msg 100))
          "turn.failed")))
     ((string= type "error")
      (let ((msg (alist-get 'message evt)))
        (if (stringp msg)
            (format "error %s" (truncate-string-to-width msg 100))
          "error")))
     ((stringp type)
      (format "event %s" type))
     (t nil))))

(defun codex-repl--log-stream-event (evt json-line)
  "Emit stream event EVT (and optional JSON-LINE) to invoke trace buffer."
  (when-let ((summary (codex-repl--stream-event-summary evt)))
    (codex-repl--append-invoke-trace summary 'font-lock-comment-face))
  (when (and codex-repl-invoke-log-raw-events
             (stringp json-line))
    (codex-repl--append-invoke-trace
     (format "raw %s" (codex-repl--truncate-single-line json-line 500))
     'shadow)))

;;; Streaming event parser (for progress display)

(defun codex-repl--parse-stream-event (json-line)
  "Parse a Codex JSONL event and return a progress string or nil."
  (condition-case err
      (let* ((evt (json-parse-string json-line
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
             (type (alist-get 'type evt)))
         (codex-repl--log-stream-event evt json-line)
         (cond
         ((string= type "thread.started")
          (let ((thread-id (or (alist-get 'thread_id evt)
                               (alist-get 'session_id evt))))
            (when (and (stringp thread-id)
                       (not (string-empty-p thread-id))
                       (not (equal thread-id codex-repl-session-id)))
              (condition-case persist-err
                  (codex-repl--persist-session-id! thread-id)
                (error
                 (codex-repl--append-invoke-trace
                  (format "session persist warning: %s"
                          (error-message-string persist-err))
                  'font-lock-warning-face))))
            (codex-repl--set-progress-status
             (if (and (stringp thread-id) (not (string-empty-p thread-id)))
                 (format "thread started (%s)"
                         (substring thread-id 0 (min 8 (length thread-id))))
               "thread started"))))
         ((string= type "item.started")
          (let* ((item (alist-get 'item evt))
                 (item-type (and (listp item) (alist-get 'type item))))
            (cond
             ((and (stringp item-type) (string= item-type "tool_call"))
              (let ((name (alist-get 'name item)))
                (codex-repl--set-progress-status
                 (codex-repl--humanize-tool-name name))))
             ((stringp item-type)
              (codex-repl--set-progress-status
               (codex-repl--humanize-item-type item-type)))
             (t nil))))
         ((string= type "item.completed")
          (let* ((item (alist-get 'item evt))
                 (item-type (and (listp item) (alist-get 'type item))))
            (cond
             ((and (stringp item-type) (string= item-type "tool_call"))
              (let ((name (alist-get 'name item)))
                (codex-repl--set-progress-status
                 (format "%s (done)"
                         (codex-repl--humanize-tool-name name)))))
             ((and (stringp item-type) (string= item-type "agent_message"))
              (codex-repl--set-progress-status "Preparing Response"))
             ((and (stringp item-type) (string= item-type "reasoning"))
              (codex-repl--set-progress-status "Preparing Response"))
             (t nil))))
         ((string= type "reasoning")
          (codex-repl--set-progress-status "Preparing Response"))
         ((string= type "command_execution")
          (codex-repl--set-progress-status "Using Bash"))
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
    (error
     (codex-repl--append-invoke-trace
      (format "event parse error: %s"
              (error-message-string err))
      'font-lock-warning-face)
     nil)))

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

(defconst codex-repl--irc-send-regex
  "^IRC_SEND[[:space:]]+\\([^[:space:]]+\\)[[:space:]]*::[[:space:]]*\\(.+\\)$"
  "Regex for explicit IRC send directives emitted by Codex.")

(defun codex-repl--extract-irc-send-directive (text)
  "Extract IRC send directive from TEXT.
Returns plist (:channel :text), or nil."
  (when (stringp text)
    (catch 'directive
      (dolist (line (split-string text "\n"))
        (when (string-match codex-repl--irc-send-regex line)
          (throw 'directive
                 (list :channel (match-string 1 line)
                       :text (match-string 2 line)))))
      nil)))

(defun codex-repl--strip-irc-send-directives (text)
  "Remove IRC_SEND directive lines from TEXT."
  (string-join
   (cl-remove-if
    (lambda (line)
      (string-match-p codex-repl--irc-send-regex line))
   (split-string (or text "") "\n"))
   "\n"))

(defun codex-repl--normalize-base-url (base)
  "Return BASE without trailing slash, or nil when blank."
  (when (and (stringp base) (not (string-empty-p (string-trim base))))
    (string-remove-suffix "/" (string-trim base))))

(defun codex-repl--health-irc-send-base ()
  "Fetch IRC send base hint from local Agency /health."
  (let ((local (codex-repl--normalize-base-url futon3c-ui-agency-base-url)))
    (when local
      (let* ((url (format "%s/health" local))
             (response (codex-repl--evidence-request-json "GET" url nil))
             (status (plist-get response :status))
             (parsed (plist-get response :json))
             (hint (plist-get parsed :irc-send-base)))
        (when (and (integerp status) (<= 200 status) (< status 300))
          (codex-repl--normalize-base-url hint))))))

(defun codex-repl--irc-send-candidate-bases ()
  "Return ordered base URLs to try for IRC send."
  (let* ((local (codex-repl--normalize-base-url futon3c-ui-agency-base-url))
         (fallback (or (codex-repl--normalize-base-url codex-repl-irc-send-base-url)
                       codex-repl--cached-irc-send-base
                       (setq codex-repl--cached-irc-send-base
                             (codex-repl--health-irc-send-base)))))
    (cond
      ((and local fallback (not (string= local fallback)))
       (list local fallback))
      (local (list local))
      (fallback (list fallback))
      (t nil))))

(defun codex-repl--send-irc-via-base (base channel text)
  "Send TEXT to IRC CHANNEL through Agency BASE."
  (let* ((url (format "%s/api/alpha/irc/send" base))
         (response (codex-repl--evidence-request-json
                    "POST" url
                    `((channel . ,channel)
                      (text . ,text)
                      (from . "codex"))))
         (status (plist-get response :status))
         (parsed (plist-get response :json)))
    (if (and (integerp status) (<= 200 status) (< status 300))
        (list :ok t :status status :base base)
      (list :ok nil
            :status (or status 0)
            :base base
            :message (or (plist-get parsed :message)
                         "IRC send failed")))))

(defun codex-repl--send-irc-via-agency (channel text)
  "Send TEXT to IRC CHANNEL through Agency HTTP API with fallback."
  (let ((bases (codex-repl--irc-send-candidate-bases))
        (last-failure (list :ok nil :status 0 :message "IRC send failed")))
    (if (null bases)
        (list :ok nil :status 0 :message "No Agency base configured for IRC send")
      (catch 'done
        (dolist (base bases)
          (let ((result (codex-repl--send-irc-via-base base channel text)))
            (if (plist-get result :ok)
                (throw 'done result)
              (setq last-failure result)
              ;; Retry on irc-unavailable only; other failures are terminal.
              (unless (= 503 (plist-get result :status))
                (throw 'done result)))))
        last-failure))))

(defun codex-repl--apply-irc-send-directive (final-text)
  "Execute any IRC_SEND directive in FINAL-TEXT and append delivery status."
  (let ((directive (codex-repl--extract-irc-send-directive final-text)))
    (if (null directive)
        final-text
      (let* ((channel (plist-get directive :channel))
             (text (plist-get directive :text))
             (result (codex-repl--send-irc-via-agency channel text))
             (cleaned (string-trim (codex-repl--strip-irc-send-directives final-text)))
             (status-line
              (if (plist-get result :ok)
                  (format "[IRC sent] %s: %s" channel text)
                (format "[IRC send failed status=%s] %s"
                        (plist-get result :status)
                        (plist-get result :message)))))
        (if (string-empty-p cleaned)
            status-line
          (format "%s\n\n%s" cleaned status-line))))))

(defconst codex-repl--irc-send-request-regexes
  '("\\bpost\\b.*\\birc\\b"
    "\\bsend\\b.*\\birc\\b"
    "\\btell\\s+@?\\(claude\\|codex\\|joe\\)\\b"
    "\\bping\\s+@?\\(claude\\|codex\\|joe\\)\\b"
    "\\bmessage\\s+@?\\(claude\\|codex\\|joe\\)\\b"
    "\\bnotify\\s+@?\\(claude\\|codex\\|joe\\)\\b")
  "Regexes indicating likely user intent to send a message to IRC.")

(defun codex-repl--likely-irc-send-request-p (text)
  "Return non-nil when TEXT likely asks to post to IRC."
  (let ((s (downcase (or text ""))))
    (cl-some (lambda (re) (string-match-p re s))
             codex-repl--irc-send-request-regexes)))

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
                               t t))))
          (codex-repl-refresh-header-line nil buf))))))

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
         (exec-args (if (and codex-repl-reasoning-effort
                             (not (string-empty-p codex-repl-reasoning-effort)))
                        (append exec-args
                                (list "-c" (format "model_reasoning_effort=\"%s\""
                                                   codex-repl-reasoning-effort)))
                      exec-args))
         (exec-args (if (and codex-repl-model (not (string-empty-p codex-repl-model)))
                        (append exec-args (list "--model" codex-repl-model))
                      exec-args)))
    (if (and session-id (not (string-empty-p session-id)))
        (append exec-args (list "resume" session-id "-"))
      (append exec-args (list "-")))))

(defun codex-repl--surface-contract ()
  "Return a strict runtime contract for prompt routing semantics."
  (let* ((state (codex-repl-modeline-state t))
         (agency (if (plist-get state :agency-available?) "up" "down"))
         (irc (if (plist-get state :irc-available?) "up" "down")))
    (string-join
     (list
      "Runtime surface contract:"
      "- Current surface: emacs-codex-repl."
      "- Your response is shown only in this Emacs buffer."
      "- Do not claim to post to IRC, write /tmp relay files, or send network messages unless a tool call in this turn actually did it."
      "- Do not claim that work is actively starting/running unless this turn executed tool calls/commands."
      "- Any progress claim must include concrete evidence (artifact path, commit SHA, or PR/issue URL)."
      "- If the user asks you to tell/ping/message someone, treat it as an IRC-send request."
      "- For IRC-send requests, output only the single-line message text to send (no wrappers)."
      "- Never use curl/tool calls for IRC on this surface."
      (format "- Telemetry snapshot: agency=%s irc=%s." agency irc))
     "\n")))

(defun codex-repl--call-codex-async (text callback)
  "Call `codex exec --json` with TEXT asynchronously.
Invoke CALLBACK with the final response text."
  (let* ((args (codex-repl--build-codex-args codex-repl-session-id))
         (repl-buffer (current-buffer))
         (outbuf (generate-new-buffer " *codex-repl-codex*"))
         (process-environment process-environment)
         (proc nil)
         (runtime-preamble (codex-repl--surface-contract))
         (extra-preamble (and codex-repl-chat-preamble
                              (not (string-empty-p codex-repl-chat-preamble))
                              codex-repl-chat-preamble))
         (turn-directive (when (codex-repl--likely-irc-send-request-p text)
                           (string-join
                            '("Turn-specific directive:"
                              "- Interpret this as an IRC-send request."
                              "- Output only the single-line message text (no wrappers)."
                              "- Do not run tools/curl for this turn.")
                            "\n")))
         (prompt-text (format "%s\n\nUser message:\n%s"
                              (cond
                               ((and extra-preamble turn-directive)
                                (format "%s\n\n%s\n\n%s" runtime-preamble extra-preamble turn-directive))
                               (extra-preamble
                                (format "%s\n\n%s" runtime-preamble extra-preamble))
                               (turn-directive
                                (format "%s\n\n%s" runtime-preamble turn-directive))
                               (t runtime-preamble))
                              text))
         (payload (if (string-suffix-p "\n" prompt-text)
                      prompt-text
                    (concat prompt-text "\n"))))
    (setq codex-repl--invoke-turn-id (1+ codex-repl--invoke-turn-id))
    (codex-repl--display-invoke-buffer)
    (codex-repl--append-invoke-trace
     (make-string 72 ?-)
     'shadow)
    (codex-repl--append-invoke-trace
     (format "invoke start turn=%d session=%s model=%s effort=%s"
             codex-repl--invoke-turn-id
             (or codex-repl-session-id "new")
             (or codex-repl-model "default")
             (or codex-repl-reasoning-effort "default"))
     'font-lock-keyword-face)
    (codex-repl--append-invoke-trace
     (format "user prompt %s"
             (codex-repl--truncate-single-line text 240))
     'shadow)
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
                     (elapsed (codex-repl--thinking-elapsed-seconds))
                     (raw (if (buffer-live-p (process-buffer p))
                               (with-current-buffer (process-buffer p)
                                 (buffer-string))
                             ""))
                      (parsed (codex-repl--parse-codex-json-output raw))
                      (sid (plist-get parsed :session-id))
                      (response (plist-get parsed :text))
                      (err (plist-get parsed :error))
                      (final-text (if (= exit-code 0)
                                      (codex-repl--apply-irc-send-directive
                                       (string-trim response))
                                    (format "[Error (exit %d): %s]"
                                            exit-code
                                            (string-trim (or err response))))))
                 (codex-repl--append-invoke-trace
                  (format "invoke done exit=%d elapsed=%ds session=%s"
                          exit-code elapsed (or sid codex-repl-session-id "unknown"))
                  (if (= exit-code 0) 'font-lock-string-face 'font-lock-warning-face))
                 (when (and err (not (string-empty-p (string-trim err))))
                   (codex-repl--append-invoke-trace
                    (format "invoke error %s"
                            (codex-repl--truncate-single-line err 240))
                    'font-lock-warning-face))
                 (unwind-protect
                     (progn
                       (when (and (stringp sid) (not (string-empty-p sid)))
                         (condition-case persist-err
                             (codex-repl--persist-session-id! sid)
                           (error
                            (message "codex-repl persist warning: %s"
                                     (error-message-string persist-err)))))
                       (when (buffer-live-p repl-buffer)
                         (with-current-buffer repl-buffer
                           (when (eq futon3c-ui--pending-process p)
                             (setq futon3c-ui--pending-process nil))
                           (condition-case callback-err
                               (funcall callback final-text)
                             (error
                              (message "codex-repl callback warning: %s"
                                       (error-message-string callback-err)))))))
                   (codex-repl--stop-thinking-heartbeat)
                   (setq codex-repl--thinking-start-time nil
                         codex-repl--last-progress-status nil)
                   (when (buffer-live-p (process-buffer p))
                     (kill-buffer (process-buffer p)))))))))
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
         (agency-up (futon3c-ui-agency-available-p))
         (irc-up (futon3c-ui-irc-available-p))
         (transports
          (append
           (list (list :key 'codex-repl
                       :label (format "emacs-codex-repl (active, session %s)" session)
                       :status 'active
                       :session session)
                 (list :key 'agency
                       :label "agency (/api/alpha/invoke)"
                       :status (if agency-up 'available 'configured))
                 (list :key 'cli
                       :label "cli (codex exec --json)"
                       :status 'available))
           (when irc-up
             (list (list :key 'irc
                         :label "irc (#futon :6667, available)"
                         :status 'available))))))
    (list :current 'codex-repl
          :current-label "emacs-codex-repl"
          :session-id session
          :agency-available? agency-up
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
         (labels (mapcar (lambda (entry)
                           (let ((label (plist-get entry :label))
                                 (status (plist-get entry :status)))
                             (if (memq status '(available active))
                                 label
                               (format "%s (%s)" label status))))
                         entries))
         (current (plist-get state :current-label)))
    (format "Transports: [%s]. Current: %s."
            (string-join labels ", ")
            current)))

(defun codex-repl--world-view-string (state)
  "Return multi-line description of STATE plist."
  (let* ((session (plist-get state :session-id))
         (agency? (if (plist-get state :agency-available?) "up" "down"))
         (irc? (if (plist-get state :irc-available?) "up" "down"))
         (timestamp (plist-get state :timestamp))
         (time-str (format-time-string "%Y-%m-%d %H:%M:%S %Z" timestamp))
         (current (plist-get state :current-label))
         (lines (list (format "Codex REPL world @ %s" time-str)
                      (format "  Session: %s" session)
                      (format "  Current transport: %s" current)
                      (format "  Agency API: %s" agency?)
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
         (agency (if (plist-get state :agency-available?) "agency:up" "agency:down"))
         (irc (if (plist-get state :irc-available?) "irc:up" "irc:down"))
         (current (plist-get state :current-label))
         (transports (mapconcat (lambda (entry)
                                  (format "%s:%s"
                                          (plist-get entry :key)
                                          (plist-get entry :status)))
                                (plist-get state :transports)
                                ", ")))
    (format "Codex session %s | current=%s | %s | %s | transports[%s]"
            session current agency irc transports)))

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

(defun codex-repl--ensure-header-line! ()
  "Ensure Codex header-line is configured in current buffer."
  (when (eq major-mode 'codex-repl-mode)
    (setq-local header-line-format '(:eval (codex-repl--header-line)))
    (codex-repl-refresh-header-line t (current-buffer))))

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

(defun codex-repl-show-invoke-trace ()
  "Display invoke trace buffer."
  (interactive)
  (pop-to-buffer (codex-repl--invoke-buffer))
  (goto-char (point-max)))

(defun codex-repl-clear-invoke-trace ()
  "Clear invoke trace buffer."
  (interactive)
  (let ((buf (codex-repl--invoke-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (codex-repl--append-invoke-trace "invoke trace cleared" 'shadow)))

;;; Mode

(defvar codex-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-repl-send-input)
    (define-key map (kbd "C-c C-k") #'codex-repl-clear)
    (define-key map (kbd "C-c C-v") #'codex-repl-show-invoke-trace)
    (define-key map (kbd "C-c C-l") #'codex-repl-clear-invoke-trace)
    map))

(define-derived-mode codex-repl-mode nil "Codex-REPL"
  "Chat with Codex via CLI.
Type after the prompt, RET to send.
\\{codex-repl-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (codex-repl--ensure-header-line!))

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
  (setq codex-repl--cached-irc-send-base nil)
  (codex-repl--ensure-session-id)
  (futon3c-ui-init-buffer
   (list :title "codex repl"
         :session-id (or codex-repl-session-id "pending")
         :modeline-fn #'codex-repl--build-modeline
         :face-alist `(("codex" . codex-repl-codex-face))
         :agent-name "codex"
         :thinking-text "codex is thinking..."
         :thinking-prop 'codex-repl-thinking))
  (codex-repl--ensure-header-line!))

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
        (codex-repl--refresh-session-header (current-buffer)))
      (codex-repl--refresh-session-header (current-buffer))
      (codex-repl--ensure-header-line!))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'codex-repl)
;;; codex-repl.el ends here
