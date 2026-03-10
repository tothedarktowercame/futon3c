;;; codex-repl.el --- Chat with Codex via futon3c API -*- lexical-binding: t; -*-

;; Author: Codex + Joe
;; Description: Emacs chat buffer backed by futon3c's Codex invoke API.
;; Asynchronous: type, RET, Codex responds without blocking Emacs UI.
;; Session continuity is owned by the server-side Codex agent registry state.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/agent-chat.el")
;;   (load "/home/joe/code/futon3c/emacs/codex-repl.el")
;;   M-x codex-repl

(require 'cl-lib)
(require 'agent-chat)
(require 'agent-chat-invariants)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

;;; Configuration

(defgroup codex-repl nil
  "Chat with Codex via futon3c API."
  :group 'agent-chat)

(defcustom codex-repl-api-url agent-chat-agency-base-url
  "Base URL for the futon3c API server."
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

(defcustom codex-repl-chat-preamble nil
  "Optional instruction prepended to each Codex REPL turn.
Set to nil to disable and send raw user text."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-evidence-url
  (or (getenv "FUTON3C_EVIDENCE_URL")
      (format "%s/api/alpha/evidence"
              (string-remove-suffix "/" agent-chat-agency-base-url)))
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

(defcustom codex-repl-drawbridge-url "http://localhost:6768"
  "Drawbridge REPL URL for direct registry access."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-drawbridge-token nil
  "Admin token for Drawbridge REPL.
If nil, reads from .admintoken in the project root at first use."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-irc-send-base-url
  (or (getenv "FUTON3C_IRC_SEND_BASE")
      (getenv "FUTON3C_LINODE_URL"))
  "Optional fallback Agency base URL for IRC send requests.
When local Agency returns irc-unavailable (503), codex-repl retries against this base."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-agency-agent-id
  (or (getenv "AGENCY_AGENT_ID") "codex-1")
  "Agent ID used when querying Agency invoke-readiness diagnostics."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-routing-diagnostic-ttl 10
  "Seconds to cache invoke-routing diagnostics in `codex-repl`."
  :type 'number
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

(defcustom codex-repl-invoke-show-tool-details t
  "When non-nil, include tool-call details in invoke trace summaries."
  :type 'boolean
  :group 'codex-repl)

;;; Face (Codex-specific; shared faces are in agent-chat)

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
(defvar codex-repl--last-registry-heartbeat-time 0
  "Epoch seconds of the last external invoke heartbeat sent to the JVM registry.")
(defvar codex-repl--cached-irc-send-base nil
  "Cached remote Agency base URL hint for IRC send fallback.")
(defvar codex-repl--invoke-turn-id 0
  "Monotonic local counter for codex-repl invoke turns.")
(defvar codex-repl--routing-diagnostic-cache nil
  "Cached invoke-routing diagnostics keyed by Agency base.")
(defvar codex-repl--routing-diagnostic-cached-at 0
  "Epoch seconds when routing diagnostics were last refreshed.")

(defvar codex-repl--resolved-api-base-cache nil
  "Cached reachable futon3c API base URL for Codex REPL.")

;;; Invoke dashboard state

(defvar codex-repl--invoke-trace-entries nil
  "List of (TIMESTAMP LINE FACE) trace entries for invoke dashboard.")

(defvar codex-repl--invoke-prompt-preview nil
  "Truncated prompt text for current invoke turn.")

(defvar codex-repl--invoke-done-info nil
  "Plist with :exit-code :elapsed :error on completion, nil while running.")

(defvar-local codex-repl--last-stream-summary nil
  "Last inline narration summary emitted for the current turn.")

(defvar-local codex-repl--final-message-text nil
  "Final assistant message text observed in stream events for current turn.")

(defvar-local codex-repl--final-text-rendered nil
  "Non-nil when the current turn's assistant text has already been rendered inline.")

(defvar-local codex-repl--streamed-text-seen nil
  "Non-nil when text chunks have already been rendered for the current turn.")

(defvar-local codex-repl--rendered-assistant-text ""
  "Assistant text rendered for the current turn, excluding tool narration.")

(defconst codex-repl--registry-source "emacs-codex-repl"
  "Stable source key used when publishing Codex REPL invoke state to the JVM registry.")

(defconst codex-repl--registry-heartbeat-interval 5
  "Seconds between external invoke heartbeats from Emacs into the JVM registry.")

(defconst codex-repl--invoke-spinner ["/" "-" "\\" "|"]
  "Spinner animation characters for invoke dashboard.")

(defvar codex-repl--invoke-trace-max 20
  "Maximum trace entries shown in invoke dashboard.")

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

(defun codex-repl--drawbridge-token ()
  "Return the Drawbridge admin token, reading .admintoken if needed."
  (or codex-repl-drawbridge-token
      (let* ((roots (delete-dups
                     (delq nil
                           (list (locate-dominating-file default-directory ".admintoken")
                                 (and load-file-name
                                      (locate-dominating-file load-file-name ".admintoken"))
                                 (locate-dominating-file
                                  "/home/joe/code/futon3c/emacs/codex-repl.el"
                                  ".admintoken")))))
             (token-file (cl-find-if
                          #'file-exists-p
                          (mapcar (lambda (root)
                                    (expand-file-name ".admintoken" root))
                                  roots))))
        (when token-file
          (setq codex-repl-drawbridge-token
                (string-trim (with-temp-buffer
                               (insert-file-contents token-file)
                               (buffer-string))))))))

(defun codex-repl--drawbridge-eval (clj-code &optional timeout-seconds)
  "Evaluate CLJ-CODE via Drawbridge and return parsed JSON response, or nil."
  (let* ((url (format "%s/eval" codex-repl-drawbridge-url))
         (token (codex-repl--drawbridge-token))
         (payload (string-as-unibyte (encode-coding-string clj-code 'utf-8)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("x-admin-token" . ,token)
            ("Content-Type" . "text/plain; charset=utf-8")))
         (url-request-data payload)
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t (or timeout-seconds 2))
                   (error nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (parsed
                (or (agent-chat--parse-json-string body)
                    (let* ((ok (and (string-match-p ":ok[[:space:]]+true" body) t))
                           (value
                            (cond
                             ((string-match ":value[[:space:]]+\"\\([^\"]*\\)\"" body)
                              (match-string 1 body))
                             ((string-match ":value[[:space:]]+\\([^,}\n]+\\)" body)
                              (let ((raw (string-trim (match-string 1 body))))
                                (cond
                                 ((string= raw "nil") nil)
                                 ((string= raw "true") t)
                                 ((string= raw "false") nil)
                                 ((string-match-p "\\`[0-9]+\\'" raw) (string-to-number raw))
                                 (t raw))))
                             (t nil))))
                      (list :ok ok :value value)))))
          (kill-buffer buffer)
          parsed)))))

(defun codex-repl--drawbridge-api-base ()
  "Return local futon3c API base discovered from Drawbridge, or nil."
  (let* ((result (codex-repl--drawbridge-eval
                  "(when-let [s @futon3c.dev/!f3c-sys] (str \"http://127.0.0.1:\" (:port s)))"
                  2))
         (value (plist-get result :value)))
    (when (and (plist-get result :ok)
               (stringp value)
               (not (string-empty-p value)))
      (string-remove-suffix "/" value))))

(defun codex-repl--base-reachable-p (base)
  "Return non-nil when BASE responds successfully to /health."
  (when (and (stringp base) (not (string-empty-p base)))
    (let* ((response (codex-repl--request-json
                      "GET" (format "%s/health" (string-remove-suffix "/" base)) nil))
           (status (plist-get response :status)))
      (and (integerp status) (<= 200 status) (< status 300)))))

(defun codex-repl--resolved-api-base (&optional force)
  "Return the best reachable futon3c API base URL.
When FORCE is non-nil, refresh cached discovery."
  (when (or force
            (null codex-repl--resolved-api-base-cache)
            (not (codex-repl--base-reachable-p codex-repl--resolved-api-base-cache)))
    (let* ((candidates (delete-dups
                        (delq nil
                              (list (codex-repl--normalize-base-url codex-repl-api-url)
                                    (codex-repl--normalize-base-url agent-chat-agency-base-url)
                                    (codex-repl--drawbridge-api-base)))))
           (reachable (cl-find-if #'codex-repl--base-reachable-p candidates)))
      (setq codex-repl--resolved-api-base-cache (or reachable (car candidates)))))
  codex-repl--resolved-api-base-cache)

(defun codex-repl--report-registry-invoke-state! (status &optional activity)
  "Publish current Codex REPL invoke STATUS to the JVM registry.
STATUS is typically :invoking or :idle. ACTIVITY, when present, is the
short human-readable progress string to surface in *agents*."
  (when (and (stringp codex-repl-agency-agent-id)
             (not (string-empty-p codex-repl-agency-agent-id)))
    (let* ((status-form (if (keywordp status)
                            (format ":%s" (symbol-name status))
                          (format "%S" (format "%s" status))))
           (clj-code
           (format
             (concat "(do "
                     "(require 'futon3c.agency.registry) "
                     "(futon3c.agency.registry/report-external-invoke! "
                     "%S %S {:status %s :session-id %S :prompt-preview %S :activity %S}))")
             codex-repl-agency-agent-id
             codex-repl--registry-source
             status-form
             (and (stringp codex-repl-session-id)
                  (not (string-empty-p codex-repl-session-id))
                  codex-repl-session-id)
             (when (eq status :invoking) "[external invoke]")
             (and (stringp activity)
                  (not (string-empty-p activity))
                  activity))))
      (condition-case nil
          (progn
            (codex-repl--drawbridge-eval clj-code 2)
            (setq codex-repl--last-registry-heartbeat-time (float-time)))
        (error nil)))))

(defun codex-repl--clear-registry-invoke-state! ()
  "Clear the Codex REPL external invoke state from the JVM registry."
  (codex-repl--report-registry-invoke-state! :idle nil)
  (setq codex-repl--last-registry-heartbeat-time 0))

(defun codex-repl--maybe-heartbeat-registry-invoke! ()
  "Refresh the JVM registry heartbeat for the current external invoke."
  (when (>= (- (float-time) codex-repl--last-registry-heartbeat-time)
            codex-repl--registry-heartbeat-interval)
    (codex-repl--report-registry-invoke-state!
     :invoking
     (or codex-repl--last-progress-status "working"))))

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
               (if (not (process-live-p agent-chat--pending-process))
                   (codex-repl--stop-thinking-heartbeat)
                 (agent-chat-update-progress
                  (codex-repl--progress-line
                   (or codex-repl--last-progress-status "working")
                   (codex-repl--thinking-elapsed-seconds)))
                 (codex-repl--refresh-invoke-dashboard))))))))

(defun codex-repl--ui-state-valid-p ()
  "Return non-nil when agent-chat markers/state are usable in this buffer."
  (and (markerp agent-chat--prompt-marker)
       (markerp agent-chat--separator-start)
       (markerp agent-chat--input-start)
       agent-chat--agent-name))

(defun codex-repl--apply-ui-state-defaults ()
  "Ensure required agent-chat buffer-local state is initialized."
  (setq-local agent-chat--face-alist
              (append `(("codex" . codex-repl-codex-face))
                      (list (cons "joe" 'agent-chat-joe-face))))
  (setq-local agent-chat--agent-name "codex")
  (setq-local agent-chat--thinking-text "codex is thinking...")
  (setq-local agent-chat--thinking-property 'codex-repl-thinking))

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
      (setq-local agent-chat--prompt-marker (copy-marker separator-pos t))
      (setq-local agent-chat--separator-start (copy-marker separator-pos))
      ;; Input-start must stay fixed at prompt boundary while user types.
      (setq-local agent-chat--input-start (copy-marker prompt-pos))
      (set-marker-insertion-type agent-chat--prompt-marker t)
      (set-marker-insertion-type agent-chat--input-start nil)
      t)))

(defun codex-repl--ensure-input-marker-stable! ()
  "Ensure `agent-chat--input-start` does not advance while typing."
  (when (and (markerp agent-chat--input-start)
             (marker-insertion-type agent-chat--input-start))
    (set-marker-insertion-type agent-chat--input-start nil)))

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
  "Record LINE with FACE in trace entries and refresh dashboard."
  (when (and (stringp line) (not (string-empty-p line)))
    (let ((entry (list (format-time-string "%H:%M:%S") line (or face 'default))))
      (setq codex-repl--invoke-trace-entries
            (last (append codex-repl--invoke-trace-entries (list entry))
                  codex-repl--invoke-trace-max)))
    (codex-repl--refresh-invoke-dashboard)))

(defun codex-repl--refresh-invoke-dashboard ()
  "Replace invoke buffer content with current dashboard state."
  (let ((buf (get-buffer codex-repl-invoke-buffer-name)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (running (numberp codex-repl--thinking-start-time))
               (elapsed (if running (codex-repl--thinking-elapsed-seconds) 0))
               (done codex-repl--invoke-done-info)
               (activity (or codex-repl--last-progress-status
                             (and running "working")))
               (spin (when running
                       (aref codex-repl--invoke-spinner
                             (mod (floor elapsed) 4)))))
          (erase-buffer)
          ;; Title line
          (insert (propertize
                   (cond
                    (done (format "Invoke: codex-repl — DONE (exit=%s)\n"
                                  (or (plist-get done :exit-code) "?")))
                    (running (format "Invoke: codex-repl %s %ds\n" spin elapsed))
                    (t "Invoke: codex-repl\n"))
                   'face 'bold))
          ;; Session
          (insert (format "Session: %s\n" (or codex-repl-session-id "pending")))
          ;; Prompt preview
          (when codex-repl--invoke-prompt-preview
            (insert (propertize
                     (format "Prompt: %s\n" codex-repl--invoke-prompt-preview)
                     'face 'shadow)))
          ;; Activity
          (when (and running activity)
            (insert (propertize (format "\nActivity: %s\n" activity)
                                'face 'font-lock-keyword-face)))
          ;; Trace entries
          (when codex-repl--invoke-trace-entries
            (insert (propertize "\n--- trace ---\n" 'face 'shadow))
            (dolist (entry codex-repl--invoke-trace-entries)
              (let ((ts (nth 0 entry))
                    (text (nth 1 entry))
                    (face (nth 2 entry)))
                (insert (propertize (format "[%s] %s\n" ts text)
                                    'face face)))))
          ;; Status line
          (insert (propertize
                   (cond
                    (done (format "\nCompleted in %ds."
                                  (or (plist-get done :elapsed) 0)))
                    (running (format "\nWorking... (%s)" (or activity "working")))
                    (t ""))
                   'face 'agent-chat-thinking-face)))))))

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

(defun codex-repl--json-object->alist (value)
  "Best-effort coerce VALUE into an alist if it looks JSON-like."
  (cond
   ((listp value) value)
   ((and (stringp value)
         (not (string-empty-p value))
         (or (string-prefix-p "{" value)
             (string-prefix-p "[" value)))
    (condition-case nil
        (json-parse-string value
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil)
      (error nil)))
   (t nil)))

(defun codex-repl--first-string-value (alist keys)
  "Return first non-empty string for any key in KEYS from ALIST."
  (let ((found nil))
    (while (and keys (not found))
      (let* ((key (car keys))
             (val (and (listp alist) (alist-get key alist))))
        (when (and (stringp val) (not (string-empty-p val)))
          (setq found val))
        (setq keys (cdr keys))))
    found))

(defun codex-repl--tool-call-detail (item)
  "Extract compact human-readable detail from tool-call ITEM."
  (let* ((name (and (listp item) (alist-get 'name item)))
         (args-raw (or (and (listp item) (alist-get 'arguments item))
                       (and (listp item) (alist-get 'input item))
                       (and (listp item) (alist-get 'params item))
                       (and (listp item) (alist-get 'payload item))))
         (args (codex-repl--json-object->alist args-raw))
         (tool (and (stringp name) (downcase name)))
         (cmd (or (codex-repl--first-string-value args '(cmd command chars patch))
                  (and (stringp args-raw) args-raw)))
         (q (codex-repl--first-string-value args '(q pattern search_query location)))
         (path (codex-repl--first-string-value args '(path ref_id file filename location)))
         (recipient (codex-repl--first-string-value args '(recipient_name tool_name fn)))
         (detail
          (cond
           ((and (stringp tool)
                 (member tool '("command_execution" "command-execution" "bash" "shell"))
                 (stringp cmd))
            (format "cmd: %s" (codex-repl--truncate-single-line cmd 140)))
           ((stringp q)
            (format "query: %s" (codex-repl--truncate-single-line q 120)))
           ((stringp path)
            (format "target: %s" (codex-repl--truncate-single-line path 120)))
           ((stringp recipient)
            (format "tool: %s" (codex-repl--truncate-single-line recipient 120)))
           ((and (stringp cmd) (not (string-empty-p cmd)))
            (format "input: %s" (codex-repl--truncate-single-line cmd 120)))
           (t nil))))
    detail))

(defun codex-repl--extract-agent-text (item)
  "Extract assistant text from Codex ITEM payload."
  (let ((content (or (alist-get 'text item)
                     (alist-get 'content item))))
    (cond
     ((stringp content)
      (let ((trimmed (string-trim content)))
        (unless (string-empty-p trimmed)
          trimmed)))
     ((listp content)
      (let ((joined
             (string-join
              (delq nil
                    (mapcar (lambda (part)
                              (when (and (listp part)
                                         (string= (alist-get 'type part) "text"))
                                (alist-get 'text part)))
                            content))
              "")))
        (let ((trimmed (string-trim joined)))
          (unless (string-empty-p trimmed)
            trimmed))))
     (t nil))))

(defun codex-repl--record-rendered-assistant-text! (text)
  "Append assistant TEXT rendered for the current turn."
  (when (stringp text)
    (setq codex-repl--rendered-assistant-text
          (concat (or codex-repl--rendered-assistant-text "") text))))

(defun codex-repl--stream-event-summary (evt)
  "Render one-line summary for Codex stream event EVT."
  (let ((type (alist-get 'type evt)))
    (cond
     ((string= type "started")
      "invoke stream started")
     ((string= type "tool_use")
      (let* ((tools (alist-get 'tools evt))
             (tool-list (cond
                         ((vectorp tools) (append tools nil))
                         ((listp tools) tools)
                         (t nil)))
             (tool-text (and tool-list
                             (mapconcat #'identity tool-list ", "))))
        (if (and (stringp tool-text) (not (string-empty-p tool-text)))
            (format "using %s" tool-text)
          "using tool")))
     ((string= type "text")
      "text")
     ((string= type "done")
      (if (alist-get 'ok evt)
          "invoke done"
        (let ((msg (or (alist-get 'message evt)
                       (alist-get 'error evt))))
          (if (stringp msg)
              (format "invoke failed %s" (truncate-string-to-width msg 100))
            "invoke failed"))))
     ((string= type "thread.started")
      (let ((sid (or (alist-get 'thread_id evt)
                     (alist-get 'session_id evt)
                     "?")))
        (format "thread.started session=%s" sid)))
     ((or (string= type "item.started")
          (string= type "item.completed"))
      (let* ((item (alist-get 'item evt))
             (item-type (and (listp item) (alist-get 'type item)))
             (name (and (listp item) (alist-get 'name item)))
             (detail (and codex-repl-invoke-show-tool-details
                          (stringp item-type)
                          (member item-type '("tool_call" "command_execution"))
                          (codex-repl--tool-call-detail item))))
        (cond
         ((and (stringp item-type) (string= item-type "command_execution"))
          (if (stringp detail)
              (format "Using Bash (%s): %s"
                      (if (string= type "item.started") "started" "done")
                      detail)
            (format "Using Bash (%s)"
                    (if (string= type "item.started") "started" "done"))))
         ((and (stringp item-type) (string= item-type "tool_call"))
          (if (stringp detail)
              (format "%s (%s): %s"
                      (codex-repl--humanize-tool-name name)
                      (if (string= type "item.started") "started" "done")
                      detail)
            (format "%s (%s)"
                    (codex-repl--humanize-tool-name name)
                    (if (string= type "item.started") "started" "done"))))
         ((stringp item-type)
          (format "%s (%s)"
                  (codex-repl--humanize-item-type item-type)
                  (if (string= type "item.started") "started" "done")))
         (t type))))
     ((string= type "reasoning")
      "Preparing Response")
     ((string= type "command_execution")
      (let* ((detail (and codex-repl-invoke-show-tool-details
                          (codex-repl--tool-call-detail evt))))
        (if (stringp detail)
            (format "Using Bash: %s" detail)
          "Using Bash")))
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

(defun codex-repl--inline-stream-summary (evt)
  "Return a compact narration line for main-buffer rendering of EVT."
  (let ((type (alist-get 'type evt)))
    (cond
     ((string= type "reasoning")
      "Preparing Response")
     ((string= type "tool_use")
      nil)
     ((string= type "item.started")
      (let* ((item (alist-get 'item evt))
             (item-type (and (listp item) (alist-get 'type item)))
             (name (and (listp item) (alist-get 'name item)))
             (detail (and codex-repl-invoke-show-tool-details
                          (stringp item-type)
                          (member item-type '("tool_call" "command_execution"))
                          (codex-repl--tool-call-detail item))))
        (cond
         ((and (stringp item-type) (string= item-type "command_execution"))
          (if (stringp detail)
              (format "Using Bash: %s" detail)
            "Using Bash"))
         ((and (stringp item-type) (string= item-type "tool_call"))
          (if (stringp detail)
              (format "%s: %s"
                      (codex-repl--humanize-tool-name name)
                      detail)
            (codex-repl--humanize-tool-name name)))
         ((and (stringp item-type)
               (member item-type '("reasoning" "agent_message")))
          "Preparing Response")
         (t nil))))
     ((string= type "command_execution")
      (let ((detail (and codex-repl-invoke-show-tool-details
                         (codex-repl--tool-call-detail evt))))
        (if (stringp detail)
            (format "Using Bash: %s" detail)
          "Using Bash")))
     (t nil))))

(defun codex-repl--stream-inline-event (evt)
  "Render selected Codex stream events inline in the main chat buffer."
  (let* ((type (alist-get 'type evt))
         (summary (codex-repl--inline-stream-summary evt)))
    (cond
     ((and (string= type "text")
           (stringp (alist-get 'text evt))
           (not (string-empty-p (string-trim (alist-get 'text evt)))))
      (unless agent-chat--streaming-started
        (agent-chat-begin-streaming-message "codex")
        (setq codex-repl--last-stream-summary nil))
      (setq codex-repl--streamed-text-seen t
            codex-repl--final-text-rendered t)
      (codex-repl--record-rendered-assistant-text! (alist-get 'text evt))
      (agent-chat-stream-text (alist-get 'text evt)))
     ((string= type "item.completed")
      (let* ((item (alist-get 'item evt))
             (item-type (and (listp item) (alist-get 'type item)))
             (message-text (and (stringp item-type)
                                (string= item-type "agent_message")
                                (codex-repl--extract-agent-text item))))
        (when (stringp message-text)
          (setq codex-repl--final-message-text message-text)
          (unless codex-repl--streamed-text-seen
            (unless agent-chat--streaming-started
              (agent-chat-begin-streaming-message "codex")
              (setq codex-repl--last-stream-summary nil))
            (setq codex-repl--final-text-rendered t)
            (codex-repl--record-rendered-assistant-text! message-text)
            (agent-chat-stream-text message-text)))))
     ((and (member type '("tool_use" "item.started" "command_execution" "reasoning"))
           (stringp summary)
           (not (string-empty-p summary))
           (not (equal summary codex-repl--last-stream-summary)))
      (unless agent-chat--streaming-started
        (agent-chat-begin-streaming-message "codex")
        (setq codex-repl--last-stream-summary nil))
      (setq codex-repl--last-stream-summary summary)
      (agent-chat-stream-text (concat summary "\n"))))))

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
         ((string= type "started")
          (codex-repl--set-progress-status "starting"))
         ((string= type "tool_use")
          (let* ((tools (alist-get 'tools evt))
                 (tool-list (cond
                             ((vectorp tools) (append tools nil))
                             ((listp tools) tools)
                             (t nil)))
                 (tool-text (and tool-list
                                 (mapconcat #'identity tool-list ", "))))
            (codex-repl--set-progress-status
             (if (and (stringp tool-text) (not (string-empty-p tool-text)))
                 (format "using %s" tool-text)
               "using tool"))))
         ((string= type "text")
          (codex-repl--set-progress-status "Preparing Response"))
         ((string= type "done")
          (let ((sid (alist-get 'session-id evt))
                (ok (alist-get 'ok evt))
                (msg (or (alist-get 'message evt)
                         (alist-get 'error evt))))
            (when (and (stringp sid)
                       (not (string-empty-p sid))
                       (not (equal sid codex-repl-session-id)))
              (condition-case persist-err
                  (codex-repl--persist-session-id! sid)
                (error
                 (codex-repl--append-invoke-trace
                  (format "session persist warning: %s"
                          (error-message-string persist-err))
                  'font-lock-warning-face))))
            (codex-repl--set-progress-status
             (cond
              (ok "done")
              ((and (stringp msg)
                    (codex-repl--stream-error-progress-status msg))
               (codex-repl--stream-error-progress-status msg))
              (t "failed")))))
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
             ((and (stringp item-type) (string= item-type "command_execution"))
              (codex-repl--set-progress-status "Using Bash"))
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
             ((and (stringp item-type) (string= item-type "command_execution"))
              (codex-repl--set-progress-status "Using Bash (done)"))
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
            (when-let ((status (codex-repl--stream-error-progress-status msg)))
              (codex-repl--set-progress-status status))))
         ((string= type "error")
          (let ((msg (alist-get 'message evt)))
            (when-let ((status (codex-repl--stream-error-progress-status msg)))
              (codex-repl--set-progress-status status))))
         (t nil))
         (codex-repl--stream-inline-event evt))
    (error
     (codex-repl--append-invoke-trace
      (format "event parse error: %s"
              (error-message-string err))
      'font-lock-warning-face)
     nil)))

(defun codex-repl--request-json (method url &optional payload)
  "Send METHOD to URL with JSON PAYLOAD using the shared HTTP helper."
  (agent-chat-evidence-request-json
   method url codex-repl-evidence-timeout payload))

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
  (let ((local (or (codex-repl--resolved-api-base)
                   (codex-repl--normalize-base-url agent-chat-agency-base-url))))
    (when local
      (let* ((url (format "%s/health" local))
             (response (codex-repl--request-json "GET" url nil))
             (status (plist-get response :status))
             (parsed (plist-get response :json))
             (hint (plist-get parsed :irc-send-base)))
        (when (and (integerp status) (<= 200 status) (< status 300))
          (codex-repl--normalize-base-url hint))))))

(defun codex-repl--irc-send-candidate-bases ()
  "Return ordered base URLs to try for IRC send."
  (let* ((local (codex-repl--normalize-base-url agent-chat-agency-base-url))
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
         (response (codex-repl--request-json
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
              ;; Always try all candidates before failing; stale local bases can 404.
              (setq last-failure result))))
        last-failure))))

(defun codex-repl--routing-bases ()
  "Return distinct Agency base URLs to probe for routing diagnostics."
  (let* ((local (or (codex-repl--resolved-api-base)
                    (codex-repl--normalize-base-url agent-chat-agency-base-url)))
         (fallback (or (codex-repl--normalize-base-url codex-repl-irc-send-base-url)
                       codex-repl--cached-irc-send-base
                       (setq codex-repl--cached-irc-send-base
                             (codex-repl--health-irc-send-base)))))
    (delete-dups (delq nil (list local fallback)))))

(defun codex-repl--probe-agent-routing (base)
  "Fetch invoke-routing diagnostics for `codex-repl-agency-agent-id` from BASE."
  (let* ((agent-id (or codex-repl-agency-agent-id "codex-1"))
         (url (format "%s/api/alpha/agents/%s"
                      base (url-hexify-string agent-id)))
         (response (codex-repl--request-json "GET" url nil))
         (status (or (plist-get response :status) 0))
         (parsed (plist-get response :json))
         (agent (and (plist-get parsed :ok) (plist-get parsed :agent))))
    (if (and (= status 200) (listp agent))
        (list :base base
              :reachable t
              :status status
              :agent-id agent-id
              :invoke-route (or (plist-get agent :invoke-route) "unknown")
              :invoke-ready? (if (plist-member agent :invoke-ready?)
                                 (plist-get agent :invoke-ready?)
                               nil)
              :invoke-local? (if (plist-member agent :invoke-local?)
                                 (plist-get agent :invoke-local?)
                               nil)
              :invoke-ws-available? (if (plist-member agent :invoke-ws-available?)
                                        (plist-get agent :invoke-ws-available?)
                                      nil)
              :invoke-diagnostic (or (plist-get agent :invoke-diagnostic) "")
              :metadata-note (let ((meta (plist-get agent :metadata)))
                               (or (plist-get meta :note) "")))
      (list :base base
            :reachable nil
            :status status
            :agent-id agent-id
            :invoke-route "unknown"
            :invoke-ready? nil
            :invoke-diagnostic (or (plist-get parsed :message)
                                   (plist-get parsed :error)
                                   "unreachable or invalid response")))))

(defun codex-repl--routing-diagnostics (&optional force)
  "Return invoke-routing diagnostics for Agency bases.
When FORCE is non-nil, refresh immediately."
  (let* ((now (float-time))
         (ttl (max 1 (truncate codex-repl-routing-diagnostic-ttl)))
         (stale? (> (- now codex-repl--routing-diagnostic-cached-at) ttl)))
    (when (or force stale? (null codex-repl--routing-diagnostic-cache))
      (setq codex-repl--routing-diagnostic-cache
            (mapcar #'codex-repl--probe-agent-routing
                    (codex-repl--routing-bases))
            codex-repl--routing-diagnostic-cached-at now))
    codex-repl--routing-diagnostic-cache))

(defun codex-repl--routing-summary-text (&optional force)
  "Return one-line invoke-routing summary text."
  (let* ((items (codex-repl--routing-diagnostics force))
         (parts
          (mapcar
           (lambda (item)
             (if (plist-get item :reachable)
                 (format "%s route=%s ready=%s ws=%s local=%s (%s)"
                         (plist-get item :base)
                         (plist-get item :invoke-route)
                         (if (plist-get item :invoke-ready?) "yes" "no")
                         (if (plist-get item :invoke-ws-available?) "yes" "no")
                         (if (plist-get item :invoke-local?) "yes" "no")
                         (or (plist-get item :invoke-diagnostic) ""))
               (format "%s unreachable status=%s (%s)"
                       (plist-get item :base)
                       (plist-get item :status)
                       (or (plist-get item :invoke-diagnostic) ""))))
           items)))
    (if parts
        (string-join parts " | ")
      "no agency bases configured")))

(defun codex-repl--routing-diagnostics-report (&optional force)
  "Return multi-line invoke-routing diagnostics report."
  (let* ((items (codex-repl--routing-diagnostics force))
         (lines (list (format "Codex routing diagnostics for agent %s"
                              (or codex-repl-agency-agent-id "codex-1")))))
    (if (null items)
        (setq lines (append lines (list "  - no agency base URLs configured")))
      (dolist (item items)
        (setq lines
              (append lines
                      (if (plist-get item :reachable)
                          (list (format "  - %s status=%s route=%s ready=%s ws=%s local=%s"
                                        (plist-get item :base)
                                        (plist-get item :status)
                                        (plist-get item :invoke-route)
                                        (if (plist-get item :invoke-ready?) "yes" "no")
                                        (if (plist-get item :invoke-ws-available?) "yes" "no")
                                        (if (plist-get item :invoke-local?) "yes" "no"))
                                (format "    diagnostic: %s"
                                        (or (plist-get item :invoke-diagnostic) ""))
                                (let ((note (plist-get item :metadata-note)))
                                  (if (and (stringp note) (not (string-empty-p (string-trim note))))
                                      (format "    metadata.note: %s" note)
                                    "    metadata.note: (none)")))
                        (list (format "  - %s status=%s unreachable"
                                      (plist-get item :base)
                                      (plist-get item :status))
                              (format "    diagnostic: %s"
                                      (or (plist-get item :invoke-diagnostic) ""))))))))
    (string-join lines "\n")))

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

(defun codex-repl--emit-turn-evidence! (role text)
  "Emit a turn evidence event for ROLE (\"user\" or \"assistant\") and TEXT."
  (agent-chat-emit-turn-evidence!
   codex-repl-evidence-url
   codex-repl-evidence-timeout
   codex-repl-evidence-log-turns
   codex-repl-session-id
   role
   text
   "codex"
   "emacs-codex-repl"
   '("codex" "repl" "turn")
   'codex-repl--evidence-session-id
   'codex-repl--last-evidence-id))

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
    (codex-repl--emit-session-start-evidence! sid)
    (when (process-live-p agent-chat--pending-process)
      (codex-repl--report-registry-invoke-state!
       :invoking
       (or codex-repl--last-progress-status "working")))))

(defun codex-repl--emit-session-start-evidence! (sid)
  "Emit a lightweight session-start evidence entry for SID."
  (agent-chat-emit-session-start-evidence!
   codex-repl-evidence-url
   codex-repl-evidence-timeout
   sid
   'codex-repl--evidence-session-id
   'codex-repl--last-evidence-id
   'codex-repl--last-emitted-session-id
   "codex-repl"
   '("codex" "session-start" "repl")))

(defun codex-repl--ensure-session-id ()
  "Load existing Codex session id from file, if present."
  (agent-chat-ensure-session-id
   codex-repl-session-file
   codex-repl-session-id
   (lambda (sid)
     (setq codex-repl-session-id sid)
     (codex-repl--emit-session-start-evidence! sid))))

(defun codex-repl--stale-session-error-p (text)
  "Return non-nil when TEXT indicates a stale/resume-corrupted Codex session."
  (let ((msg (downcase (or text ""))))
    (or
     ;; Known stale-resume signature from Codex/OpenAI protocol drift.
     (and (string-match-p "action\\.type" msg)
          (or (string-match-p "invalid value: 'other'" msg)
              (string-match-p "supported values are: 'search', 'open_page', and 'find_in_page'" msg)))
     ;; Current user-facing failure mode.
     (string-match-p "stream error: unexpected status 400 bad request" msg)
     ;; Additional stale-thread variants.
     (string-match-p "unknown thread" msg)
     (string-match-p "thread.*not found" msg)
     (string-match-p "session.*not found" msg))))

(defun codex-repl--stream-error-progress-status (msg)
  "Return a user-facing progress status for stream error message MSG."
  (cond
   ((not (stringp msg)) nil)
   ((codex-repl--stale-session-error-p msg)
    "recovering session")
   (t
    (format "error: %s" (truncate-string-to-width msg 60)))))

(defun codex-repl--clear-session-state! ()
  "Clear locally persisted Codex session continuity."
  (setq codex-repl-session-id nil
        codex-repl--evidence-session-id nil
        codex-repl--last-evidence-id nil
        codex-repl--last-emitted-session-id nil)
  (when (and codex-repl-session-file
             (file-exists-p codex-repl-session-file))
    (delete-file codex-repl-session-file))
  (codex-repl--refresh-session-header (get-buffer codex-repl--buffer-name))
  (codex-repl-refresh-header-line t (get-buffer codex-repl--buffer-name)))

(defun codex-repl--surface-contract ()
  "Return a strict runtime contract for prompt routing semantics."
  (let* ((state (codex-repl-modeline-state t))
         (agency (if (plist-get state :agency-available?) "up" "down"))
         (irc (if (plist-get state :irc-available?) "up" "down"))
         (routing (codex-repl--routing-summary-text)))
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
      "- For IRC-send transport, do not assume http://127.0.0.1:7070; prefer routing hint / health irc-send-base."
      "- For transport debugging requests, you SHOULD run verification commands (curl/ss/ps) and quote actual outputs."
      (format "- Telemetry snapshot: agency=%s irc=%s." agency irc)
      (format "- Invoke routing snapshot: %s" routing))
     "\n")))

(defun codex-repl--find-done-event (raw)
  "Return the final NDJSON done event parsed from RAW, or nil."
  (let ((done-event nil))
    (dolist (line (split-string (or raw "") "\n"))
      (when (and (not (string-empty-p (string-trim line)))
                 (string-match-p "\"type\"[[:space:]]*:[[:space:]]*\"done\"" line))
        (condition-case nil
            (setq done-event
                  (json-parse-string line
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
          (error nil))))
    done-event))

(defun codex-repl--finish-invoke (proc done-event raw callback)
  "Finalize invoke state for PROC using DONE-EVENT and RAW, then run CALLBACK."
  (let* ((exit-code (process-exit-status proc))
         (elapsed (codex-repl--thinking-elapsed-seconds))
         (ok (and done-event (alist-get 'ok done-event)))
         (sid (and done-event (alist-get 'session-id done-event)))
         (result-text (and done-event (alist-get 'result done-event)))
         (err-msg (and done-event
                       (or (alist-get 'message done-event)
                           (alist-get 'error done-event))))
         (final-text (cond
                      (ok
                       (codex-repl--apply-irc-send-directive
                        (string-trim (or result-text
                                         codex-repl--final-message-text
                                         "[empty response]"))))
                      (done-event
                       (format "[Error: %s]" (or err-msg "invoke failed")))
                      (t
                       (format "[curl error (exit %d): %s]"
                               exit-code
                               (string-trim (truncate-string-to-width raw 200))))))
         (trace-session (or sid codex-repl-session-id "unknown"))
         (rendered-text (string-trim (or codex-repl--rendered-assistant-text "")))
         (needs-terminal-insert?
          (and ok
               (stringp final-text)
               (not (string-empty-p final-text))
               (not (string= rendered-text (string-trim final-text))))))
    (setq codex-repl--invoke-done-info
          (list :exit-code exit-code
                :elapsed elapsed
                :error (unless ok err-msg)))
    (codex-repl--append-invoke-trace
     (format "invoke done exit=%d elapsed=%ds session=%s"
             exit-code elapsed trace-session)
     (if ok 'font-lock-string-face 'font-lock-warning-face))
    (when (and err-msg (not (string-empty-p (string-trim err-msg))))
      (codex-repl--append-invoke-trace
       (format "invoke error %s"
               (codex-repl--truncate-single-line err-msg 240))
       'font-lock-warning-face))
    (unwind-protect
        (progn
          (when (and (stringp sid) (not (string-empty-p sid)))
            (condition-case persist-err
                (codex-repl--persist-session-id! sid)
              (error
               (message "codex-repl persist warning: %s"
                        (error-message-string persist-err)))))
          (when (eq agent-chat--pending-process proc)
            (setq agent-chat--pending-process nil))
          (condition-case callback-err
              (if (and ok agent-chat--streaming-started)
                  (progn
                    (when needs-terminal-insert?
                      (when (and (stringp codex-repl--last-stream-summary)
                                 (not (string-empty-p codex-repl--last-stream-summary)))
                        (agent-chat-stream-text "\n"))
                      (agent-chat-stream-text final-text)
                      (codex-repl--record-rendered-assistant-text! final-text)
                      (setq codex-repl--final-text-rendered t))
                    (agent-chat-end-streaming-message)
                    (setq codex-repl--last-stream-summary nil)
                    (codex-repl--emit-assistant-turn-evidence! final-text)
                    (agent-chat-invariants-turn-ended)
                    (goto-char (point-max))
                    (agent-chat-scroll-to-bottom))
                (progn
                  (when agent-chat--streaming-started
                    (agent-chat-end-streaming-message)
                    (setq codex-repl--last-stream-summary nil))
                  (funcall callback final-text)))
            (error
             (message "codex-repl callback warning: %s"
                      (error-message-string callback-err)))))
      (codex-repl--stop-thinking-heartbeat)
      (setq codex-repl--thinking-start-time nil
            codex-repl--last-progress-status nil
            codex-repl--final-message-text nil
            codex-repl--final-text-rendered nil
            codex-repl--streamed-text-seen nil
            codex-repl--rendered-assistant-text ""))))

(defun codex-repl--call-codex-async (text callback &optional _retry-attempt)
  "Invoke server-managed Codex asynchronously for TEXT.
CALLBACK receives the final response text."
  (let* ((repl-buffer (current-buffer))
         (api-base (or (codex-repl--resolved-api-base)
                       (string-remove-suffix "/" codex-repl-api-url)))
         (url (concat api-base
                      "/api/alpha/invoke-stream"))
         (json-body (json-serialize
                     `(:agent-id ,codex-repl-agency-agent-id
                       :prompt ,text
                       :surface "emacs-repl"
                       :caller ,(or (getenv "USER") user-login-name "joe"))))
         (outbuf (generate-new-buffer " *codex-repl-stream*"))
         (line-buffer ""))
    (setq codex-repl--invoke-turn-id (1+ codex-repl--invoke-turn-id))
    (setq codex-repl--invoke-prompt-preview
          (codex-repl--truncate-single-line text 300))
    (setq codex-repl--invoke-done-info nil)
    (setq codex-repl--invoke-trace-entries nil)
    (setq codex-repl--last-stream-summary nil
          codex-repl--final-message-text nil
          codex-repl--final-text-rendered nil
          codex-repl--streamed-text-seen nil
          codex-repl--rendered-assistant-text "")
    (codex-repl--display-invoke-buffer)
    (codex-repl--append-invoke-trace (make-string 72 ?-) 'shadow)
    (codex-repl--append-invoke-trace
     (format "invoke start turn=%d session=%s transport=%s"
             codex-repl--invoke-turn-id
             (or codex-repl-session-id "new")
             "server-managed")
     'font-lock-keyword-face)
    (codex-repl--append-invoke-trace
     (format "user prompt %s"
             (codex-repl--truncate-single-line text 240))
     'shadow)
    (setq codex-repl--thinking-start-time (float-time)
          codex-repl--last-progress-status "starting")
    (let ((proc
           (make-process
            :name "codex-repl-stream"
            :buffer outbuf
            :command (list "curl" "-N" "-sS" "--max-time" "1800"
                           "-H" "Content-Type: application/json"
                           "-d" json-body url)
            :noquery t
            :connection-type 'pipe
            :filter
            (lambda (p output)
              (when (buffer-live-p (process-buffer p))
                (with-current-buffer (process-buffer p)
                  (goto-char (point-max))
                  (insert output)))
              (setq line-buffer (concat line-buffer output))
              (let ((lines (split-string line-buffer "\n")))
                (setq line-buffer (car (last lines)))
                (dolist (line (butlast lines))
                  (when (and (not (string-empty-p (string-trim line)))
                             (buffer-live-p repl-buffer))
                    (with-current-buffer repl-buffer
                      (codex-repl--parse-stream-event line))))))
            :sentinel
            (lambda (p _event)
              (when (and (memq (process-status p) '(exit signal))
                         (buffer-live-p repl-buffer))
                (let ((raw (if (buffer-live-p (process-buffer p))
                               (with-current-buffer (process-buffer p)
                                 (buffer-string))
                             "")))
                  (with-current-buffer repl-buffer
                    (if (not (eq agent-chat--pending-process p))
                        (progn
                          (when agent-chat--streaming-started
                            (agent-chat-end-streaming-message)
                            (setq codex-repl--last-stream-summary nil))
                          (codex-repl--append-invoke-trace
                           (format "invoke interrupted elapsed=%ds"
                                   (codex-repl--thinking-elapsed-seconds))
                           'font-lock-warning-face)
                          (codex-repl--stop-thinking-heartbeat)
                          (setq codex-repl--thinking-start-time nil
                                codex-repl--last-progress-status nil))
                      (codex-repl--finish-invoke
                       p (codex-repl--find-done-event raw) raw callback)))
                  (when (buffer-live-p (process-buffer p))
                    (kill-buffer (process-buffer p)))))))))
      (codex-repl--start-thinking-heartbeat repl-buffer)
      proc)))

;;; Modeline

(defvar codex-repl--last-modeline-state nil
  "Cache of the most recently computed modeline state.")

(defun codex-repl--compute-modeline-state ()
  "Return plist describing current transport/modeline state."
  (let* ((session (or codex-repl-session-id "pending"))
         (api-base (codex-repl--resolved-api-base))
         (agency-up (codex-repl--base-reachable-p api-base))
         (irc-up (agent-chat-irc-available-p))
         (transports
          (append
           (list (list :key 'agency
                       :label (format "agency (%s/api/alpha/invoke-stream, session %s)"
                                      (or api-base "unresolved")
                                      session)
                       :status (if agency-up 'active 'configured)
                       :session session)
                 (list :key 'codex-repl
                       :label "emacs-codex-repl (UI)"
                       :status 'available))
           (when irc-up
             (list (list :key 'irc
                         :label "irc (#futon :6667, available)"
                         :status 'available))))))
    (list :current 'agency
          :current-label "agency (/api/alpha/invoke-stream)"
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

(defun codex-repl-diagnose-routing (&optional refresh)
  "Display invoke-routing diagnostics for known Agency bases.
With REFRESH non-nil, force an immediate refresh."
  (interactive "P")
  (let* ((report (codex-repl--routing-diagnostics-report refresh))
         (buf (get-buffer-create "*codex-repl-routing*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report "\n")
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)
    (kill-new report)
    (message "Routing diagnostics copied to kill ring.")))

(defun codex-repl-show-invoke-trace ()
  "Display invoke trace buffer."
  (interactive)
  (pop-to-buffer (codex-repl--invoke-buffer))
  (goto-char (point-max)))

(defun codex-repl-clear-invoke-trace ()
  "Clear invoke dashboard state and buffer."
  (interactive)
  (setq codex-repl--invoke-trace-entries nil
        codex-repl--invoke-prompt-preview nil
        codex-repl--invoke-done-info nil)
  (codex-repl--append-invoke-trace "invoke trace cleared" 'shadow))

(defun codex-repl-interrupt ()
  "Interrupt current Codex turn with explicit invoke-trace logging."
  (interactive)
  (if (process-live-p agent-chat--pending-process)
      (let ((proc agent-chat--pending-process))
        (codex-repl--append-invoke-trace
         (format "interrupt requested pid=%s"
                 (or (process-id proc) "?"))
         'font-lock-warning-face)
        (codex-repl--stop-thinking-heartbeat)
        (setq codex-repl--thinking-start-time nil
              codex-repl--last-progress-status "interrupted")
        (agent-chat-interrupt))
    (codex-repl--append-invoke-trace
     "interrupt requested but no live invoke process"
     'shadow)
    (message "Nothing to interrupt")))

(defun codex-repl--reset-via-api ()
  "Try to reset Codex session via the agent reset-session HTTP endpoint.
Returns (ok . old-session-id) on success, nil on failure."
  (let* ((api-base (or (codex-repl--resolved-api-base)
                       (string-remove-suffix "/" codex-repl-api-url)))
         (url (format "%s/api/alpha/agents/%s/reset-session"
                      api-base
                      codex-repl-agency-agent-id))
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
                                :object-type 'alist
                                :null-object nil)))
                (when (alist-get 'ok json-obj)
                  (setq result (cons t (alist-get 'old-session-id json-obj)))))
            (error nil))))
      (kill-buffer buffer))
    result))

(defun codex-repl--reset-via-drawbridge ()
  "Try to reset Codex session via Drawbridge as a fallback.
Returns (ok . old-session-id) on success, nil on failure."
  (let* ((clj-code (format "(futon3c.agency.registry/reset-session! %S)"
                           codex-repl-agency-agent-id))
         (result (codex-repl--drawbridge-eval clj-code 10))
         (ok (plist-get result :ok))
         (value (plist-get result :value)))
    (when ok
      (cons t value))))

(defun codex-repl-new-session ()
  "Reset the server-managed Codex session so the next turn starts fresh."
  (interactive)
  (when (process-live-p agent-chat--pending-process)
    (user-error "Codex is still responding; interrupt first (C-c C-c)"))
  (let* ((api-result (codex-repl--reset-via-api))
         (result (or api-result (codex-repl--reset-via-drawbridge)))
         (ok (car result))
         (old-sid (or (cdr result) codex-repl-session-id)))
    (setq codex-repl-session-id nil
          agent-chat--session-id nil
          codex-repl--evidence-session-id nil
          codex-repl--last-evidence-id nil
          codex-repl--last-emitted-session-id nil)
    (when (and codex-repl-session-file
               (file-exists-p codex-repl-session-file))
      (delete-file codex-repl-session-file))
    (codex-repl--refresh-session-header (current-buffer))
    (codex-repl-refresh-header-line t (current-buffer))
    (agent-chat-insert-message
     "system"
     (cond
      (ok
       (format "[Session reset on server%s. Next message starts fresh.]"
               (if (and (stringp old-sid) (not (string-empty-p old-sid)))
                   (format " (was %s)" old-sid)
                 "")))
      (t
       (format "[Local session cleared; server reset unconfirmed%s.]"
               (if (and (stringp old-sid) (not (string-empty-p old-sid)))
                   (format " (was %s)" old-sid)
                 "")))))
    (goto-char (point-max))
    (message "codex-repl: session reset %s (was %s)"
             (if ok "via server" "locally only")
             (or old-sid "nil"))))

;;; Mode

(defvar codex-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-repl-send-input)
    (define-key map (kbd "C-c C-c") #'codex-repl-interrupt)
    (define-key map (kbd "C-c C-k") #'codex-repl-clear)
    (define-key map (kbd "C-c C-n") #'codex-repl-new-session)
    (define-key map (kbd "C-c C-d") #'codex-repl-diagnose-routing)
    (define-key map (kbd "C-c C-v") #'codex-repl-show-invoke-trace)
    (define-key map (kbd "C-c C-l") #'codex-repl-clear-invoke-trace)
    map))

(define-derived-mode codex-repl-mode nil "Codex-REPL"
  "Chat with Codex via CLI.
Type after the prompt, RET to send, C-c C-c to interrupt, C-c C-n for fresh session.
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
  (agent-chat-send-input
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
  (agent-chat-clear #'codex-repl--init))

(defun codex-repl--init ()
  "Initialize buffer UI."
  (setq codex-repl--cached-irc-send-base nil)
  (codex-repl--ensure-session-id)
  (agent-chat-init-buffer
   (list :title "codex repl"
         :session-id (or codex-repl-session-id "pending")
         :modeline-fn #'codex-repl--build-modeline
         :face-alist `(("codex" . codex-repl-codex-face))
         :agent-name "codex"
         :agent-id "codex-1"
         :thinking-text "codex is thinking..."
         :thinking-prop 'codex-repl-thinking))
  (agent-chat-invariants-setup)
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
