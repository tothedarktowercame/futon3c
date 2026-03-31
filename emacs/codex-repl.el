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
(require 'button)
(require 'json)
(require 'outline)
(require 'pp)
(require 'sqlite)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(load (expand-file-name "futon3c-blackboard.el"
                        (file-name-directory (or load-file-name (buffer-file-name))))
      nil t)

;;; Configuration

(defgroup codex-repl nil
  "Chat with Codex via futon3c API."
  :group 'agent-chat)

(defcustom codex-repl-api-url agent-chat-agency-base-url
  "Base URL for the futon3c API server."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-buffer-name "*codex-repl*"
  "Buffer name used for this Codex REPL instance."
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

(defcustom codex-repl-reference-commit "ada2533"
  "Reference futon5 commit used for Codex REPL comparison trials."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-reference-api-url "http://127.0.0.1:47170"
  "Default API base for the dedicated reference-test dev stack."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-sessions-root
  (expand-file-name "~/.codex/sessions")
  "Root directory containing persisted Codex rollout JSONL files."
  :type 'directory
  :group 'codex-repl)

(defcustom codex-repl-mirror-poll-interval 2
  "Seconds between rollout file polls in mirror mode."
  :type 'number
  :group 'codex-repl)

(defcustom codex-repl-frame-store-file
  (expand-file-name
   "futon3c/codex-repl.sqlite3"
   (or (getenv "XDG_STATE_HOME")
       (expand-file-name "~/.local/state")))
  "SQLite store used for Codex REPL session/frame metadata."
  :type 'file
  :group 'codex-repl)

;;; Face (Codex-specific; shared faces are in agent-chat)

(defface codex-repl-codex-face
  '((t :foreground "#ff79c6" :weight bold))
  "Face for codex."
  :group 'codex-repl)

;;; Internal state

(defconst codex-repl--source-file
  (or load-file-name (buffer-file-name))
  "Path used to recover the workspace root for Codex REPL helpers.")

(defvar-local codex-repl--last-emitted-session-id nil
  "Most recent session id emitted to evidence API.")
(defvar-local codex-repl--last-evidence-id nil
  "Most recent evidence id in the active session chain.")
(defvar-local codex-repl--evidence-session-id nil
  "Session id associated with `codex-repl--last-evidence-id`.")
(defvar-local codex-repl--thinking-start-time nil
  "Epoch seconds when the current Codex turn started.")
(defvar-local codex-repl--last-progress-status nil
  "Most recent short progress status from Codex stream events.")
(defvar-local codex-repl--thinking-timer nil
  "Timer used to refresh Codex liveness/progress while waiting.")
(defvar-local codex-repl--last-registry-heartbeat-time 0
  "Epoch seconds of the last external invoke heartbeat sent to the JVM registry.")
(defvar-local codex-repl--cached-irc-send-base nil
  "Cached remote Agency base URL hint for IRC send fallback.")
(defvar-local codex-repl--invoke-turn-id 0
  "Monotonic local counter for codex-repl invoke turns.")
(defvar-local codex-repl--routing-diagnostic-cache nil
  "Cached invoke-routing diagnostics keyed by Agency base.")
(defvar-local codex-repl--routing-diagnostic-cached-at 0
  "Epoch seconds when routing diagnostics were last refreshed.")

(defvar-local codex-repl--resolved-api-base-cache nil
  "Cached reachable futon3c API base URL for Codex REPL.")

;;; Frame state

(defconst codex-repl--frame-fence-re
  "```\\([a-zA-Z0-9_+-]*\\)\n\\(\\(?:.*\n\\)*?\\)```"
  "Regex matching markdown code fences in assistant output.")

(defvar-local codex-repl--frame-table nil
  "Hash table of frame-id -> frame plist for the current REPL buffer.")

(defvar-local codex-repl--frame-order nil
  "Chronological list of frame ids for the current REPL buffer.")

(defvar-local codex-repl--frame-counter 0
  "Monotonic counter used to allocate local Codex frame ids.")

(defvar-local codex-repl--current-frame-id nil
  "Frame id currently receiving live stream events, if any.")

(defvar-local codex-repl--last-frame-id nil
  "Most recent completed or updated frame id.")

(defvar-local codex-repl--mirror-open-frame-id nil
  "Frame id currently being assembled from rollout mirror replay.")

(defvar-local codex-repl--store-session-key nil
  "SQLite session key for the current Codex REPL buffer.")

(defvar-local codex-repl--session-frame-id nil
  "Frame id used for the current session anchor.")

(defvar-local codex-repl--session-open-mode 'repl
  "Opening mode for the current Codex REPL surface.")

(put 'codex-repl--store-session-key 'permanent-local t)
(put 'codex-repl--session-frame-id 'permanent-local t)
(put 'codex-repl--session-open-mode 'permanent-local t)

;;; Invoke dashboard state

(defvar-local codex-repl--invoke-trace-entries nil
  "List of (TIMESTAMP LINE FACE) trace entries for invoke dashboard.")

(defvar-local codex-repl--invoke-prompt-preview nil
  "Truncated prompt text for current invoke turn.")

(defvar-local codex-repl--invoke-done-info nil
  "Plist with :exit-code :elapsed :error on completion, nil while running.")

(defvar-local codex-repl--runtime-state nil
  "Latest verified runtime.process event for the current invoke turn.")

(defvar-local codex-repl--last-runtime-state nil
  "Most recent verified runtime.process event, preserved after completion.")

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

(defvar-local codex-repl--mirror-mode-p nil
  "Non-nil when current buffer is a read-only rollout mirror.")

(defvar-local codex-repl--mirror-rollout-file nil
  "Rollout JSONL file mirrored by the current buffer.")

(defvar-local codex-repl--mirror-poll-timer nil
  "Timer used to refresh the mirrored rollout file.")

(defvar-local codex-repl--mirror-lines-processed 0
  "Number of rollout lines already mirrored into the buffer.")

(defvar-local codex-repl--mirror-last-mtime nil
  "Most recent modification time seen for the mirrored rollout file.")

(put 'codex-repl--mirror-rollout-file 'permanent-local t)
(put 'codex-repl-session-id 'permanent-local t)

(defvar-local codex-repl--last-modeline-state nil
  "Cache of the most recently computed modeline state.")

(defconst codex-repl--registry-source "emacs-codex-repl"
  "Stable source key used when publishing Codex REPL invoke state to the JVM registry.")

(defconst codex-repl--registry-heartbeat-interval 5
  "Seconds between external invoke heartbeats from Emacs into the JVM registry.")

(defconst codex-repl--invoke-spinner ["/" "-" "\\" "|"]
  "Spinner animation characters for invoke dashboard.")

(defvar codex-repl--invoke-trace-max 20
  "Maximum trace entries shown in invoke dashboard.")

(defvar-local codex-repl-frame--source-buffer nil
  "Source Codex REPL buffer backing the current frame inspector.")

(defvar-local codex-repl-frame--frame-id nil
  "Frame id shown in the current frame inspector buffer.")

(defvar codex-repl--frame-db nil
  "Global SQLite handle for Codex frame/session storage.")

(defun codex-repl--store-db ()
  "Return initialized SQLite handle for Codex frame/session storage."
  (unless (sqlite-available-p)
    (error "Emacs sqlite support is unavailable"))
  (unless codex-repl--frame-db
    (let ((db-dir (file-name-directory codex-repl-frame-store-file)))
      (when db-dir
        (make-directory db-dir t))
      (setq codex-repl--frame-db (sqlite-open codex-repl-frame-store-file))
      (condition-case nil
          (sqlite-execute codex-repl--frame-db "PRAGMA journal_mode=WAL")
        (error nil))
      (sqlite-execute
       codex-repl--frame-db
       "CREATE TABLE IF NOT EXISTS sessions (
          session_key TEXT PRIMARY KEY,
          created_at TEXT NOT NULL,
          mode TEXT,
          agent_id TEXT,
          transport TEXT,
          lane_kind TEXT,
          session_id TEXT,
          cwd0 TEXT,
          rollout_file TEXT,
          agency_base TEXT,
          evidence_base TEXT,
          metadata_json TEXT
        )")
      (sqlite-execute
       codex-repl--frame-db
       "CREATE TABLE IF NOT EXISTS frames (
          session_key TEXT NOT NULL,
          frame_id TEXT NOT NULL,
          ordinal INTEGER NOT NULL,
          kind TEXT NOT NULL,
          origin TEXT,
          status TEXT,
          started_at TEXT,
          finished_at TEXT,
          updated_at TEXT,
          session_id TEXT,
          cwd TEXT,
          prompt TEXT,
          prompt_preview TEXT,
          assistant_text TEXT,
          tool_event_count INTEGER DEFAULT 0,
          command_event_count INTEGER DEFAULT 0,
          event_count INTEGER DEFAULT 0,
          artifact_count INTEGER DEFAULT 0,
          metadata_json TEXT,
          PRIMARY KEY (session_key, frame_id)
        )")
      (sqlite-execute
       codex-repl--frame-db
       "CREATE TABLE IF NOT EXISTS frame_events (
          session_key TEXT NOT NULL,
          frame_id TEXT NOT NULL,
          ordinal INTEGER NOT NULL,
          at TEXT,
          type TEXT,
          summary TEXT,
          payload_json TEXT
        )")
      (sqlite-execute
       codex-repl--frame-db
       "CREATE TABLE IF NOT EXISTS frame_artifacts (
          session_key TEXT NOT NULL,
          frame_id TEXT NOT NULL,
          ordinal INTEGER NOT NULL,
          kind TEXT,
          label TEXT,
          lang TEXT,
          source TEXT,
          content TEXT
        )")
      (sqlite-execute
       codex-repl--frame-db
       "CREATE INDEX IF NOT EXISTS idx_frames_session_ordinal
          ON frames(session_key, ordinal)")
      (sqlite-execute
       codex-repl--frame-db
       "CREATE INDEX IF NOT EXISTS idx_frame_events_frame
          ON frame_events(session_key, frame_id, ordinal)")
      (sqlite-execute
       codex-repl--frame-db
       "CREATE INDEX IF NOT EXISTS idx_frame_artifacts_frame
          ON frame_artifacts(session_key, frame_id, ordinal)")))
  codex-repl--frame-db)

(defun codex-repl--store-time (time-value)
  "Encode TIME-VALUE as an ISO-like string for SQLite."
  (when time-value
    (format-time-string "%Y-%m-%dT%H:%M:%S%z" time-value)))

(defun codex-repl--read-stored-time (value)
  "Decode SQLite time VALUE into Emacs time."
  (when (and (stringp value) (not (string-empty-p value)))
    (condition-case nil
        (date-to-time value)
      (error nil))))

(defun codex-repl--json-encodable (value)
  "Return VALUE converted to a form acceptable to `json-serialize'."
  (cond
   ((hash-table-p value) value)
   ((and (listp value) (consp value) (consp (car value)))
    (let ((obj (make-hash-table :test 'equal)))
      (dolist (entry value obj)
        (puthash (format "%s" (car entry))
                 (codex-repl--json-encodable (cdr entry))
                 obj))))
   ((listp value)
    (mapcar #'codex-repl--json-encodable value))
   ((vectorp value)
    (apply #'vector (mapcar #'codex-repl--json-encodable value)))
   (t value)))

(defun codex-repl--json-encode-value (value)
  "Encode VALUE as JSON string, or nil when VALUE is empty."
  (when value
    (json-serialize (codex-repl--json-encodable value))))

(defun codex-repl--json-decode-value (value)
  "Decode JSON string VALUE into Elisp, or nil on failure."
  (when (and (stringp value) (not (string-empty-p value)))
    (condition-case nil
        (json-parse-string value
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil)
      (error nil))))

(defun codex-repl--session-key ()
  "Return or allocate the SQLite session key for the current buffer."
  (or codex-repl--store-session-key
      (setq codex-repl--store-session-key
            (format "codex-session-%s-%06d"
                    (format-time-string "%Y%m%dT%H%M%S")
                    (random 1000000)))))

(defun codex-repl--session-anchor-frame-id ()
  "Return the frame id used for the immutable session anchor."
  (or codex-repl--session-frame-id
      (setq codex-repl--session-frame-id "frame-0")))

(defun codex-repl--alist-clean (alist)
  "Drop nil or empty-string values from ALIST."
  (delq nil
        (mapcar (lambda (entry)
                  (let ((value (cdr entry)))
                    (unless (or (null value)
                                (and (stringp value) (string-empty-p value)))
                      entry)))
                alist)))

(defun codex-repl--frame-metadata (frame)
  "Return supplemental metadata alist for FRAME."
  (let* ((known '(:frame/id :frame/index :frame/kind :frame/origin :status
                  :started-at :finished-at :updated-at :session-id :cwd
                  :prompt :prompt-preview :assistant-text :events :artifacts
                  :tool-event-count :command-event-count :runtime-events
                  :metadata))
         (cursor frame)
         (extras nil)
         (seed (plist-get frame :metadata)))
    (while cursor
      (let ((key (car cursor))
            (value (cadr cursor)))
        (unless (memq key known)
          (push (cons (if (symbolp key)
                          (substring (symbol-name key) 1)
                        (format "%s" key))
                      value)
                extras))
        (setq cursor (cddr cursor))))
    (append (if (listp seed) (copy-tree seed) nil)
            (nreverse extras))))

(defun codex-repl--session-anchor-metadata ()
  "Return session-anchor metadata alist for the current buffer."
  (codex-repl--alist-clean
   `(("open_mode" . ,(symbol-name (or codex-repl--session-open-mode 'repl)))
     ("buffer_name" . ,(buffer-name))
     ("agent_id" . ,codex-repl-agency-agent-id)
     ("working_directory" . ,default-directory)
     ("session_file" . ,codex-repl-session-file)
     ("rollout_file" . ,codex-repl--mirror-rollout-file)
     ("agency_base" . ,(unless codex-repl--mirror-mode-p
                         (or (codex-repl--resolved-api-base)
                             (string-remove-suffix "/" codex-repl-api-url))))
     ("evidence_url" . ,codex-repl-evidence-url)
     ("transport" . ,(if codex-repl--mirror-mode-p "mirror" "agency"))
     ("mirror_mode" . ,(and codex-repl--mirror-mode-p t)))))

(defun codex-repl--store-upsert-session ()
  "Upsert the current buffer's session row into SQLite."
  (let ((db (codex-repl--store-db))
        (session-key (codex-repl--session-key)))
    (sqlite-execute
     db
     "INSERT OR REPLACE INTO sessions
      (session_key, created_at, mode, agent_id, transport, lane_kind,
       session_id, cwd0, rollout_file, agency_base, evidence_base, metadata_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list session-key
           (codex-repl--store-time (current-time))
           (symbol-name (or codex-repl--session-open-mode 'repl))
           codex-repl-agency-agent-id
           (if codex-repl--mirror-mode-p "mirror" "agency")
           (if codex-repl--mirror-mode-p "mirror" "headless-codex-lane")
           codex-repl-session-id
           default-directory
           codex-repl--mirror-rollout-file
           (unless codex-repl--mirror-mode-p
             (or (ignore-errors (codex-repl--resolved-api-base))
                 (and (stringp codex-repl-api-url)
                      (string-remove-suffix "/" codex-repl-api-url))))
           codex-repl-evidence-url
           (codex-repl--json-encode-value
            (codex-repl--session-anchor-metadata))))))

(defun codex-repl--store-upsert-frame (frame)
  "Persist FRAME row into SQLite."
  (let ((db (codex-repl--store-db)))
    (sqlite-execute
     db
     "INSERT OR REPLACE INTO frames
      (session_key, frame_id, ordinal, kind, origin, status,
       started_at, finished_at, updated_at, session_id, cwd,
       prompt, prompt_preview, assistant_text,
       tool_event_count, command_event_count, event_count, artifact_count,
       metadata_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list (codex-repl--session-key)
           (plist-get frame :frame/id)
           (or (plist-get frame :frame/index) 0)
           (symbol-name (or (plist-get frame :frame/kind) 'codex-turn))
           (format "%s" (or (plist-get frame :frame/origin) "unknown"))
           (format "%s" (or (plist-get frame :status) "unknown"))
           (codex-repl--store-time (plist-get frame :started-at))
           (codex-repl--store-time (plist-get frame :finished-at))
           (codex-repl--store-time (or (plist-get frame :updated-at)
                                       (plist-get frame :started-at)))
           (plist-get frame :session-id)
           (plist-get frame :cwd)
           (plist-get frame :prompt)
           (plist-get frame :prompt-preview)
           (plist-get frame :assistant-text)
           (or (plist-get frame :tool-event-count) 0)
           (or (plist-get frame :command-event-count) 0)
           (length (plist-get frame :events))
           (length (plist-get frame :artifacts))
           (codex-repl--json-encode-value
            (codex-repl--frame-metadata frame))))))

(defun codex-repl--store-insert-event (frame-id ordinal event)
  "Persist EVENT for FRAME-ID with ORDINAL."
  (sqlite-execute
   (codex-repl--store-db)
   "INSERT INTO frame_events
    (session_key, frame_id, ordinal, at, type, summary, payload_json)
    VALUES (?, ?, ?, ?, ?, ?, ?)"
   (list (codex-repl--session-key)
         frame-id
         ordinal
         (codex-repl--store-time (plist-get event :at))
         (plist-get event :type)
         (plist-get event :summary)
         (codex-repl--json-encode-value (plist-get event :payload)))))

(defun codex-repl--store-insert-artifact (frame-id ordinal artifact)
  "Persist ARTIFACT for FRAME-ID with ORDINAL."
  (sqlite-execute
   (codex-repl--store-db)
   "INSERT INTO frame_artifacts
    (session_key, frame_id, ordinal, kind, label, lang, source, content)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
   (list (codex-repl--session-key)
         frame-id
         ordinal
         (format "%s" (or (plist-get artifact :kind) "artifact"))
         (plist-get artifact :label)
         (plist-get artifact :lang)
         (format "%s" (or (plist-get artifact :source) "unknown"))
         (plist-get artifact :content))))

(defun codex-repl--store-clear-session-frames ()
  "Delete all persisted frames/events/artifacts for the current session."
  (let ((db (codex-repl--store-db))
        (session-key (codex-repl--session-key)))
    (sqlite-execute db "DELETE FROM frame_events WHERE session_key = ?" (list session-key))
    (sqlite-execute db "DELETE FROM frame_artifacts WHERE session_key = ?" (list session-key))
    (sqlite-execute db "DELETE FROM frames WHERE session_key = ?" (list session-key))))

(defun codex-repl--store-select-rows (sql params)
  "Run SQLite SELECT SQL with PARAMS and return rows."
  (sqlite-select (codex-repl--store-db) sql params))

(defun codex-repl--store-load-frame (frame-id)
  "Load FRAME-ID for the current session from SQLite."
  (when (and (stringp frame-id) codex-repl--store-session-key)
    (when-let* ((row (car (codex-repl--store-select-rows
                           "SELECT frame_id, ordinal, kind, origin, status,
                                   started_at, finished_at, updated_at, session_id, cwd,
                                   prompt, prompt_preview, assistant_text,
                                   tool_event_count, command_event_count, metadata_json
                              FROM frames
                             WHERE session_key = ? AND frame_id = ?"
                           (list (codex-repl--session-key) frame-id)))))
      (pcase-let ((`(,loaded-frame-id ,ordinal ,kind ,origin ,status
                                      ,started-at ,finished-at ,updated-at ,session-id ,cwd
                                      ,prompt ,prompt-preview ,assistant-text
                                      ,tool-count ,command-count ,metadata-json) row))
        (let* ((events
                (mapcar
                 (lambda (event-row)
                   (pcase-let ((`(,_ordinal ,event-at ,event-type ,summary ,payload-json) event-row))
                     (list :at (codex-repl--read-stored-time event-at)
                           :type event-type
                           :summary summary
                           :payload (codex-repl--json-decode-value payload-json))))
                 (codex-repl--store-select-rows
                  "SELECT ordinal, at, type, summary, payload_json
                     FROM frame_events
                    WHERE session_key = ? AND frame_id = ?
                 ORDER BY ordinal"
                  (list (codex-repl--session-key) frame-id))))
               (artifacts
                (mapcar
                 (lambda (artifact-row)
                   (pcase-let ((`(,_ordinal ,artifact-kind ,label ,lang ,source ,content) artifact-row))
                     (list :kind (and artifact-kind (intern artifact-kind))
                           :label label
                           :lang lang
                           :source (and source (intern source))
                           :content content)))
                 (codex-repl--store-select-rows
                  "SELECT ordinal, kind, label, lang, source, content
                     FROM frame_artifacts
                    WHERE session_key = ? AND frame_id = ?
                 ORDER BY ordinal"
                  (list (codex-repl--session-key) frame-id))))
               (metadata (codex-repl--json-decode-value metadata-json))
               (runtime-events
                (cl-remove-if-not
                 (lambda (event)
                   (equal (plist-get event :type) "runtime.process"))
                 (mapcar (lambda (event)
                           (list :at (plist-get event :at)
                                 :payload (plist-get event :payload)))
                         events))))
          (list :frame/id loaded-frame-id
                :frame/index ordinal
                :frame/kind (and kind (intern kind))
                :frame/origin (and origin (intern origin))
                :status (and status (intern status))
                :started-at (codex-repl--read-stored-time started-at)
                :finished-at (codex-repl--read-stored-time finished-at)
                :updated-at (codex-repl--read-stored-time updated-at)
                :session-id session-id
                :cwd cwd
                :prompt prompt
                :prompt-preview prompt-preview
                :assistant-text assistant-text
                :events events
                :artifacts artifacts
                :runtime-events runtime-events
                :tool-event-count tool-count
                :command-event-count command-count
                :metadata metadata))))))

(defun codex-repl--ensure-store-session ()
  "Ensure the current buffer has a persisted session row and frame-0 anchor."
  (codex-repl--store-upsert-session)
  (let ((frame-id (codex-repl--session-anchor-frame-id)))
    (unless (codex-repl--frame-get frame-id)
      (codex-repl--frame-put
       (list :frame/id frame-id
             :frame/index 0
             :frame/kind 'session
             :frame/origin 'session
             :status 'anchored
             :session-id codex-repl-session-id
             :cwd default-directory
             :started-at (current-time)
             :updated-at (current-time)
             :metadata (codex-repl--session-anchor-metadata))))))

(defun codex-repl--ensure-frame-table ()
  "Ensure the current buffer has a frame table."
  (unless (hash-table-p codex-repl--frame-table)
    (setq codex-repl--frame-table (make-hash-table :test 'equal))))

(defun codex-repl--reset-frame-registry ()
  "Reset the current buffer's frame registry."
  (setq codex-repl--frame-table (make-hash-table :test 'equal)
        codex-repl--frame-order nil
        codex-repl--frame-counter 0
        codex-repl--current-frame-id nil
        codex-repl--last-frame-id nil
        codex-repl--mirror-open-frame-id nil))

(defun codex-repl--frame-id-at (index)
  "Return a stable frame id for local turn INDEX."
  (format "codex-turn-%d" index))

(defun codex-repl--frame-get (frame-id)
  "Return frame plist for FRAME-ID in the current buffer."
  (when (and (stringp frame-id) (hash-table-p codex-repl--frame-table))
    (gethash frame-id codex-repl--frame-table)))

(defun codex-repl--frame-put (frame)
  "Store FRAME plist in the current buffer and return it."
  (when-let* ((frame-id (plist-get frame :frame/id)))
    (codex-repl--ensure-frame-table)
    (puthash frame-id frame codex-repl--frame-table)
    (unless (member frame-id codex-repl--frame-order)
      (setq codex-repl--frame-order
            (append codex-repl--frame-order (list frame-id))))
    (setq codex-repl--last-frame-id frame-id)
    (when codex-repl--store-session-key
      (codex-repl--store-upsert-frame frame))
    frame))

(defun codex-repl--frame-update (frame-id updater)
  "Apply UPDATER to FRAME-ID and return the updated frame."
  (when-let* ((frame (codex-repl--frame-get frame-id)))
    (let ((updated (funcall updater frame)))
      (codex-repl--frame-put updated)
      updated)))

(defun codex-repl--tool-event-p (evt)
  "Return non-nil when EVT represents a tool/command execution boundary."
  (let ((evt-type (alist-get 'type evt))
        (item (alist-get 'item evt)))
    (or (string= evt-type "command_execution")
        (and (member evt-type '("item.started" "item.completed"))
             (string= (alist-get 'type item) "command_execution"))
        (and (member evt-type '("item.started" "item.completed"))
             (string= (alist-get 'type item) "tool_call")))))

(defun codex-repl--command-event-p (evt)
  "Return non-nil when EVT represents shell/bash execution."
  (let* ((evt-type (alist-get 'type evt))
         (item (alist-get 'item evt))
         (name-raw (alist-get 'name item))
         (name (and (stringp name-raw) (downcase name-raw))))
    (or (string= evt-type "command_execution")
        (and (member evt-type '("item.started" "item.completed"))
             (string= (alist-get 'type item) "command_execution"))
        (and (member evt-type '("item.started" "item.completed"))
             (string= (alist-get 'type item) "tool_call")
             (member name '("command_execution" "command-execution" "bash" "shell"))))))

(defun codex-repl--frame-open-buffer-name (frame-id)
  "Return inspector buffer name for FRAME-ID."
  (format "*codex-frame:%s*" frame-id))

(defun codex-repl--frame-refresh-viewers (&optional frame-id)
  "Refresh live frame inspectors for FRAME-ID in the current REPL buffer."
  (let ((source-buffer (current-buffer)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'codex-repl-frame-mode)
                     (eq codex-repl-frame--source-buffer source-buffer)
                     (or (null frame-id)
                         (equal frame-id codex-repl-frame--frame-id)))
            (codex-repl-frame-refresh)))))))

(defun codex-repl--frame-start (origin prompt &optional metadata)
  "Create a new frame for ORIGIN and PROMPT plus optional METADATA."
  (let* ((index (cl-incf codex-repl--frame-counter))
         (frame-id (codex-repl--frame-id-at index))
         (frame
          (append
           (list :frame/id frame-id
                 :frame/index index
                 :frame/kind 'codex-turn
                 :frame/origin origin
                 :status 'running
                 :session-id codex-repl-session-id
                 :cwd default-directory
                 :prompt prompt
                 :prompt-preview (codex-repl--truncate-single-line prompt 240)
                 :started-at (current-time)
                 :updated-at (current-time)
                 :assistant-text nil
                 :events nil
                 :artifacts nil
                 :tool-event-count 0
                 :command-event-count 0
                 :runtime-events nil)
           metadata)))
    (setq codex-repl--current-frame-id frame-id)
    (codex-repl--frame-put frame)
    (codex-repl--frame-refresh-viewers frame-id)
    frame-id))

(defun codex-repl--frame-add-artifacts (frame-id artifacts)
  "Append ARTIFACTS to FRAME-ID."
  (when artifacts
    (when-let* ((frame (codex-repl--frame-get frame-id)))
      (let ((ordinal (length (plist-get frame :artifacts))))
        (dolist (artifact artifacts)
          (codex-repl--store-insert-artifact frame-id ordinal artifact)
          (setq ordinal (1+ ordinal)))))
    (codex-repl--frame-update
     frame-id
     (lambda (frame)
       (plist-put frame :artifacts
                  (append (plist-get frame :artifacts) artifacts))))
    (codex-repl--frame-refresh-viewers frame-id)))

(defun codex-repl--frame-register-runtime (frame-id evt)
  "Append runtime EVT snapshot to FRAME-ID."
  (codex-repl--frame-update
   frame-id
   (lambda (frame)
     (plist-put frame :runtime-events
                (append (plist-get frame :runtime-events)
                        (list (list :at (current-time)
                                    :payload evt)))))))

(defun codex-repl--frame-record-event (frame-id evt &optional summary)
  "Record EVT and optional SUMMARY inside FRAME-ID."
  (when-let* ((evt-type (alist-get 'type evt)))
    (when-let* ((frame (codex-repl--frame-get frame-id)))
      (codex-repl--store-insert-event
       frame-id
       (length (plist-get frame :events))
       (list :at (current-time)
             :type evt-type
             :summary summary
             :payload evt)))
    (codex-repl--frame-update
     frame-id
     (lambda (frame)
       (let ((events (plist-get frame :events))
             (tool-count (or (plist-get frame :tool-event-count) 0))
             (command-count (or (plist-get frame :command-event-count) 0)))
         (when (string= evt-type "runtime.process")
           (setq frame (plist-put frame :runtime-events
                                  (append (plist-get frame :runtime-events)
                                          (list (list :at (current-time)
                                                      :payload evt))))))
         (setq frame
               (plist-put frame :events
                          (append events
                                  (list (list :at (current-time)
                                              :type evt-type
                                              :summary summary
                                              :payload evt)))))
         (setq frame (plist-put frame :updated-at (current-time)))
         (setq frame (plist-put frame :session-id
                                (or (alist-get 'thread_id evt)
                                    (alist-get 'session_id evt)
                                    (plist-get frame :session-id)
                                    codex-repl-session-id)))
         (setq frame (plist-put frame :cwd
                                (or (alist-get 'cwd evt)
                                    (plist-get frame :cwd)
                                    default-directory)))
         (setq frame (plist-put frame :tool-event-count
                                (if (codex-repl--tool-event-p evt)
                                    (1+ tool-count)
                                  tool-count)))
         (setq frame (plist-put frame :command-event-count
                                (if (codex-repl--command-event-p evt)
                                    (1+ command-count)
                                  command-count)))
         frame)))
    (codex-repl--frame-refresh-viewers frame-id)))

(defun codex-repl--frame-finish (frame-id status &optional final-text metadata)
  "Mark FRAME-ID finished with STATUS, FINAL-TEXT, and optional METADATA."
  (codex-repl--frame-update
   frame-id
   (lambda (frame)
     (let ((frame (plist-put frame :status status)))
       (setq frame (plist-put frame :finished-at (current-time)))
       (setq frame (plist-put frame :updated-at (current-time)))
       (when (stringp final-text)
         (setq frame (plist-put frame :assistant-text final-text)))
       (while metadata
         (setq frame (plist-put frame (car metadata) (cadr metadata)))
         (setq metadata (cddr metadata)))
       frame)))
  (when (equal codex-repl--current-frame-id frame-id)
    (setq codex-repl--current-frame-id nil))
  (codex-repl--frame-refresh-viewers frame-id)
  (codex-repl--frame-get frame-id))

(defun codex-repl--frame-mark-mirror-orphaned ()
  "Close any open mirror frame that never received an assistant response."
  (when-let* ((frame-id codex-repl--mirror-open-frame-id))
    (codex-repl--frame-finish frame-id 'mirror-orphaned nil nil)
    (setq codex-repl--mirror-open-frame-id nil)))

(defun codex-repl--frame-event-patch-artifact (evt)
  "Extract patch artifact plist from EVT, or nil."
  (let* ((item (or (alist-get 'item evt) evt))
         (item-type (alist-get 'type item))
         (name-raw (alist-get 'name item))
         (name (and (stringp name-raw) (downcase name-raw)))
         (args-raw (or (alist-get 'arguments item)
                       (alist-get 'input item)
                       (alist-get 'params item)
                       (alist-get 'payload item)))
         (args (codex-repl--json-object->alist args-raw))
         (patch (or (and (listp args) (alist-get 'patch args))
                    (and (stringp args-raw)
                         (string-prefix-p "*** Begin Patch" args-raw)
                         args-raw))))
    (when (and (stringp patch)
               (not (string-empty-p (string-trim patch)))
               (or (string= item-type "command_execution")
                   (and (string= item-type "tool_call")
                        (member name '("apply_patch" "write_file" "edit_file")))))
      (list :kind 'patch
            :label (or name item-type "patch")
            :lang "diff"
            :content patch
            :source 'event))))

(defun codex-repl--text-has-unified-diff-p (text)
  "Return non-nil when TEXT looks like a plain unified diff."
  (and (stringp text)
       (or (string-match-p "^diff --git " text)
           (and (string-match-p "^--- " text)
                (string-match-p "^\\+\\+\\+ " text)))))

(defun codex-repl--text-artifacts (text)
  "Extract code/diff artifacts from assistant TEXT."
  (let ((artifacts nil))
    (when (stringp text)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward codex-repl--frame-fence-re nil t)
          (let* ((lang (downcase (or (match-string 1) "")))
                 (content (string-trim-right (or (match-string 2) "")))
                 (kind (if (member lang '("diff" "patch"))
                           'diff
                         'code)))
            (push (list :kind kind
                        :label (if (string-empty-p lang) "text" lang)
                        :lang (if (string-empty-p lang) nil lang)
                        :content content
                        :source 'assistant-text)
                  artifacts)))))
    (when (and (null artifacts)
               (codex-repl--text-has-unified-diff-p (or text "")))
      (push (list :kind 'diff
                  :label "diff"
                  :lang "diff"
                  :content text
                  :source 'assistant-text)
            artifacts))
    (nreverse artifacts)))

(defun codex-repl--frame-artifact-buffer-name (frame-id index artifact)
  "Return a display buffer name for ARTIFACT at INDEX from FRAME-ID."
  (format "*codex-artifact:%s:%d:%s*"
          frame-id
          index
          (or (plist-get artifact :label)
              (symbol-name (or (plist-get artifact :kind) 'artifact)))))

(defun codex-repl--frame-artifact-mode (artifact)
  "Return a major mode appropriate for ARTIFACT."
  (let ((lang (downcase (or (plist-get artifact :lang) "")))
        (kind (plist-get artifact :kind)))
    (let ((mode
           (cond
     ((or (eq kind 'diff)
          (eq kind 'patch)
          (member lang '("diff" "patch")))
            #'diff-mode)
     ((member lang '("clojure" "clj"))
      #'clojure-mode)
     ((member lang '("elisp" "emacs-lisp"))
      #'emacs-lisp-mode)
     ((member lang '("bash" "sh" "shell"))
      #'sh-mode)
     ((member lang '("json"))
      #'js-mode)
     ((member lang '("markdown" "md"))
      #'markdown-mode)
     (t #'text-mode))))
      (if (fboundp mode) mode #'text-mode))))

(defun codex-repl--display-frame-artifact (source-buffer frame-id index)
  "Display artifact INDEX from FRAME-ID in SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (when-let* ((frame (codex-repl--frame-get frame-id))
                  (artifact (nth index (plist-get frame :artifacts))))
        (let ((buf (get-buffer-create
                    (codex-repl--frame-artifact-buffer-name frame-id index artifact))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize
                       (format "%s / %s\n\n"
                               frame-id
                               (or (plist-get artifact :label)
                                   (symbol-name (or (plist-get artifact :kind)
                                                    'artifact))))
                       'face 'bold))
              (insert (or (plist-get artifact :content) ""))
              (goto-char (point-min))
              (forward-line 2)
              (funcall (codex-repl--frame-artifact-mode artifact))
              (view-mode 1)))
          (display-buffer buf))))))

(defun codex-repl--insert-frame-artifact-button (source-buffer frame-id index artifact)
  "Insert a clickable button for ARTIFACT at INDEX from FRAME-ID."
  (insert-text-button
   (format "[artifact %d: %s]" (1+ index)
           (or (plist-get artifact :label)
               (symbol-name (or (plist-get artifact :kind) 'artifact))))
   'follow-link t
   'action (lambda (_button)
             (codex-repl--display-frame-artifact source-buffer frame-id index))))

(defun codex-repl--frame-event-line (event)
  "Return a human-readable line for frame EVENT."
  (let* ((at (plist-get event :at))
         (evt-type (plist-get event :type))
         (summary (plist-get event :summary)))
    (format "  - %s %-18s %s"
            (format-time-string "%H:%M:%S" at)
            (or evt-type "?")
            (or summary ""))))

(defun codex-repl--frame-runtime-line (runtime)
  "Return a short line for RUNTIME snapshot."
  (let* ((at (plist-get runtime :at))
         (payload (plist-get runtime :payload))
         (state (alist-get 'state payload))
         (command (alist-get 'command payload))
         (root-pid (alist-get 'root-pid payload)))
    (string-trim-right
     (concat
      (format "  - %s state=%s" (format-time-string "%H:%M:%S" at) (or state "?"))
      (if root-pid (format " root=%s" root-pid) "")
      (if (and (stringp command) (not (string-empty-p command)))
          (format " cmd=%s" (codex-repl--truncate-single-line command 120))
        "")))))

(defun codex-repl--frame-sessions-label (frame)
  "Return session label for FRAME."
  (or (plist-get frame :session-id) "pending"))

(defun codex-repl--frame-metadata-lines (metadata)
  "Return pretty lines for frame METADATA alist."
  (mapcar
   (lambda (entry)
     (format "  - %s: %s"
             (car entry)
             (let ((value (cdr entry)))
               (cond
                ((or (stringp value) (numberp value) (symbolp value)) value)
                ((null value) "nil")
                (t (string-trim (pp-to-string value)))))))
   metadata))

(defun codex-repl--render-frame-buffer (source-buffer frame-id)
  "Render FRAME-ID from SOURCE-BUFFER into the current inspector buffer."
  (let ((frame (with-current-buffer source-buffer
                 (or (codex-repl--store-load-frame frame-id)
                     (codex-repl--frame-get frame-id)))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (null frame)
          (insert (format "Frame %s no longer exists.\n" frame-id))
        (let* ((events (plist-get frame :events))
               (artifacts (plist-get frame :artifacts))
               (runtime-events (plist-get frame :runtime-events))
               (metadata (plist-get frame :metadata))
               (status (plist-get frame :status))
               (kind (plist-get frame :frame/kind))
               (started-at (plist-get frame :started-at))
               (finished-at (plist-get frame :finished-at))
               (elapsed (when (and started-at finished-at)
                          (float-time (time-subtract finished-at started-at)))))
          (insert (propertize
                   (format "Codex Frame %s\n" frame-id)
                   'face 'bold))
          (insert (format "  Status: %s\n" status))
          (insert (format "  Origin: %s\n" (plist-get frame :frame/origin)))
          (insert (format "  Session: %s\n" (codex-repl--frame-sessions-label frame)))
          (insert (format "  Started: %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S %Z" started-at)))
          (when finished-at
            (insert (format "  Finished: %s\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S %Z" finished-at))))
          (when elapsed
            (insert (format "  Elapsed: %.1fs\n" elapsed)))
          (insert (format "  Events: %d | tool=%d | command=%d | artifacts=%d\n\n"
                          (length events)
                          (or (plist-get frame :tool-event-count) 0)
                          (or (plist-get frame :command-event-count) 0)
                          (length artifacts)))
          (when metadata
            (insert "* Metadata\n")
            (dolist (line (codex-repl--frame-metadata-lines metadata))
              (insert line "\n")))
          (when (and (not (eq kind 'session))
                     (plist-get frame :prompt))
            (insert (if metadata "\n* Prompt\n" "* Prompt\n"))
            (insert (plist-get frame :prompt)))
          (when (and (not (eq kind 'session))
                     (plist-get frame :assistant-text))
            (insert (if (or metadata (plist-get frame :prompt))
                        "\n\n* Assistant\n"
                      "* Assistant\n"))
            (insert (plist-get frame :assistant-text)))
          (when events
            (insert (if (or metadata (plist-get frame :prompt) (plist-get frame :assistant-text))
                        "\n\n* Timeline\n"
                      "* Timeline\n"))
            (dolist (event events)
              (insert (codex-repl--frame-event-line event) "\n")))
          (when runtime-events
            (insert "\n\n* Runtime\n")
            (dolist (runtime runtime-events)
              (insert (codex-repl--frame-runtime-line runtime) "\n")))
          (when artifacts
            (insert "\n\n* Artifacts\n")
            (cl-loop for artifact in artifacts
                     for index from 0
                     do (progn
                          (insert "  ")
                          (codex-repl--insert-frame-artifact-button
                           source-buffer frame-id index artifact)
                          (insert (format "  kind=%s source=%s\n"
                                          (plist-get artifact :kind)
                                          (plist-get artifact :source))))))
          (when events
            (insert "\n\n* Raw Events\n")
            (dolist (event events)
              (insert (propertize
                       (format "\n;; %s %s\n"
                               (format-time-string "%H:%M:%S" (plist-get event :at))
                               (or (plist-get event :summary)
                                   (plist-get event :type)))
                       'face 'shadow))
              (insert (pp-to-string (plist-get event :payload)))))))
      (goto-char (point-min))
      (outline-minor-mode 1)
      (setq-local outline-regexp "\\*+ ")
      (view-mode 1))))

(defun codex-repl--workspace-root ()
  "Return the enclosing workspace root containing AGENTS.md, or nil."
  (or (locate-dominating-file default-directory "AGENTS.md")
      (and codex-repl--source-file
           (locate-dominating-file codex-repl--source-file "AGENTS.md"))))

(defun codex-repl--profile-slug (name)
  "Normalize NAME into a filesystem-friendly profile slug."
  (let* ((raw (downcase (string-trim (or name ""))))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" raw)))
    (string-trim slug "-+" "-+")))

(defun codex-repl--profile-buffer-name (slug)
  "Return the main Codex REPL buffer name for SLUG."
  (format "*codex-repl:%s*" slug))

(defun codex-repl--profile-invoke-buffer-name (slug)
  "Return the invoke trace buffer name for SLUG."
  (format "*invoke: codex-repl:%s*" slug))

(defun codex-repl--profile-session-file (slug)
  "Return a default session-file path for profile SLUG."
  (if-let ((workspace (codex-repl--workspace-root)))
      (expand-file-name (format "futon3c/.state/codex-tests/%s/session-id" slug)
                        workspace)
    (expand-file-name (format "futon-codex-%s-session-id" slug)
                      temporary-file-directory)))

(defun codex-repl--reference-profile-defaults ()
  "Return plist of defaults for the futon5 reference trial profile."
  (let* ((slug (format "futon5-%s" codex-repl-reference-commit))
         (workspace (codex-repl--workspace-root))
         (working-directory (and workspace
                                 (expand-file-name (format ".worktrees/%s" slug)
                                                   workspace))))
    (list :name slug
          :buffer-name (codex-repl--profile-buffer-name slug)
          :invoke-buffer-name (codex-repl--profile-invoke-buffer-name slug)
          :api-url codex-repl-reference-api-url
          :agent-id (format "codex-%s" slug)
          :session-file (codex-repl--profile-session-file slug)
          :working-directory working-directory)))

(defun codex-repl--mode-active-p ()
  "Return non-nil when current buffer is a Codex REPL buffer."
  (memq major-mode '(codex-repl-mode codex-repl-mirror-mode)))

(defun codex-repl--mirror-rollout-candidates ()
  "Return rollout JSONL files under `codex-repl-sessions-root', newest first."
  (when (file-directory-p codex-repl-sessions-root)
    (sort (directory-files-recursively codex-repl-sessions-root
                                       "\\`rollout-.*\\.jsonl\\'")
          (lambda (a b)
            (time-less-p (file-attribute-modification-time (file-attributes b))
                         (file-attribute-modification-time (file-attributes a)))))))

(defun codex-repl--latest-rollout-file ()
  "Return the newest rollout JSONL file, or nil."
  (car (codex-repl--mirror-rollout-candidates)))

(defun codex-repl--recover-mirror-rollout-file ()
  "Best-effort recovery of the rollout file for the current mirror buffer."
  (let* ((buffer-base
          (when (string-match "\\*codex-repl-mirror:\\([^*]+\\)\\*" (buffer-name))
            (match-string 1 (buffer-name))))
         (session-id (and (stringp codex-repl-session-id)
                          (not (string-empty-p codex-repl-session-id))
                          codex-repl-session-id)))
    (or
     (and (stringp codex-repl--mirror-rollout-file)
          (file-readable-p codex-repl--mirror-rollout-file)
          codex-repl--mirror-rollout-file)
     (cl-find-if
      (lambda (path)
        (string= (file-name-base path) buffer-base))
      (codex-repl--mirror-rollout-candidates))
     (and session-id
          (cl-find-if
           (lambda (path)
             (string-match-p (regexp-quote session-id) path))
           (codex-repl--mirror-rollout-candidates))))))

(defun codex-repl--mirror-prompt-rollout-file ()
  "Prompt for a rollout JSONL file."
  (let ((default (or (codex-repl--latest-rollout-file)
                     codex-repl-sessions-root)))
    (read-file-name "Mirror rollout: "
                    (if (file-directory-p codex-repl-sessions-root)
                        codex-repl-sessions-root
                      default)
                    default
                    t
                    nil
                    (lambda (path)
                      (or (file-directory-p path)
                          (string-match-p "\\.jsonl\\'" path))))))

(defun codex-repl--set-session-id-local! (sid)
  "Update local session state to SID without writing files or evidence."
  (when (and (stringp sid)
             (not (string-empty-p sid))
             (not (equal sid codex-repl-session-id)))
    (setq codex-repl-session-id sid
          agent-chat--session-id sid)
    (when codex-repl--store-session-key
      (codex-repl--store-upsert-session))
    (codex-repl--refresh-session-header (current-buffer))
    (codex-repl-refresh-header-line t (current-buffer))))

(defun codex-repl--json-object-entries (value)
  "Return VALUE as an alist of (KEY . VAL) pairs when it looks JSON-like."
  (cond
   ((and (listp value) (consp (car value)))
    value)
   ((and (listp value) (keywordp (car value)))
    (let ((items value)
          entries)
      (while items
        (let ((key (pop items))
              (val (pop items)))
          (push (cons (substring (symbol-name key) 1) val) entries)))
      (nreverse entries)))
   (t nil)))

(defun codex-repl--lane-buffer-name (agent-id)
  "Return the dedicated Codex REPL buffer name for AGENT-ID."
  (format "*codex-repl:%s*" agent-id))

(defun codex-repl--lane-invoke-buffer-name (agent-id)
  "Return the dedicated invoke trace buffer name for AGENT-ID."
  (format "*invoke: codex-repl:%s*" agent-id))

(defun codex-repl--default-session-file-for-agent (agent-id)
  "Best-effort fallback session-file path for AGENT-ID."
  (cond
   ((string= agent-id (or codex-repl-agency-agent-id "codex-1"))
    codex-repl-session-file)
   ((string= agent-id "codex-vscode")
    "/tmp/futon-vscode-codex-session-id")
   (t codex-repl-session-file)))

(defun codex-repl--fetch-codex-agent-ids ()
  "Return sorted registered Codex agent IDs from the live Agency API."
  (let* ((base (or (codex-repl--resolved-api-base)
                   (string-remove-suffix "/" codex-repl-api-url)))
         (url (format "%s/api/alpha/agents" base))
         (response (codex-repl--request-json "GET" url nil))
         (status (plist-get response :status))
         (parsed (plist-get response :json))
         (agents (codex-repl--json-object-entries (plist-get parsed :agents))))
    (when (and (integerp status) (<= 200 status) (< status 300))
      (sort
       (cl-loop for (agent-id . agent) in agents
                when (string= (format "%s" (plist-get agent :type)) "codex")
                collect agent-id)
       #'string<))))

(defun codex-repl--fetch-lane-process-state (agent-id)
  "Return live CYDER lane state plist for AGENT-ID, or nil."
  (let* ((base (or (codex-repl--resolved-api-base)
                   (string-remove-suffix "/" codex-repl-api-url)))
         (url (format "%s/api/alpha/processes/%s"
                      base
                      (url-hexify-string agent-id)))
         (response (codex-repl--request-json "GET" url nil))
         (status (plist-get response :status))
         (parsed (plist-get response :json))
         (process (plist-get parsed :process)))
    (when (and (= status 200)
               (plist-get parsed :ok)
               (listp process)
               (string= (format "%s" (plist-get process :process/type))
                        "agent-lane"))
      (plist-get process :process/state))))

(defun codex-repl--read-attach-agent-id ()
  "Prompt for a registered Codex lane."
  (let* ((agent-ids (codex-repl--fetch-codex-agent-ids))
         (default (or (car agent-ids) (or codex-repl-agency-agent-id "codex-1"))))
    (if agent-ids
        (completing-read (format "Attach Codex lane (default %s): " default)
                         agent-ids nil t nil nil default)
      (read-string "Attach Codex lane: " default))))

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

(defun codex-repl--runtime-age-seconds (iso8601)
  "Return whole seconds since ISO8601 timestamp, or nil."
  (when (and (stringp iso8601) (not (string-empty-p iso8601)))
    (condition-case nil
        (max 0 (floor (float-time (time-subtract (current-time)
                                                 (date-to-time iso8601)))))
      (error nil))))

(defun codex-repl--runtime-progress-status (runtime)
  "Return a compact progress string for verified RUNTIME state."
  (let* ((state (alist-get 'state runtime))
         (pid (alist-get 'root-pid runtime))
         (command (alist-get 'command runtime)))
    (cond
     ((member state '("starting" "running" "background-running"))
      (string-join
       (delq nil
             (list (if (string= state "starting") "starting runtime" "running")
                   (when pid (format "pid=%s" pid))
                   (when (and (stringp command) (not (string-empty-p command)))
                     (codex-repl--truncate-single-line command 80))))
       " "))
     ((member state '("exited" "failed-launch" "launch-error"))
      (if pid
          (format "%s pid=%s" state pid)
        state))
     (t nil))))

(defun codex-repl--runtime-block (runtime)
  "Return dashboard text block for verified RUNTIME state, or nil."
  (when (listp runtime)
    (let* ((state (or (alist-get 'state runtime) "unknown"))
           (pid (alist-get 'root-pid runtime))
           (live-pids (alist-get 'live-pids runtime))
           (child-pids (alist-get 'child-pids runtime))
           (command (alist-get 'command runtime))
           (processes-raw (alist-get 'processes runtime))
           (processes (cond
                       ((vectorp processes-raw) (append processes-raw nil))
                       ((listp processes-raw) processes-raw)
                       (t nil)))
           (last-output-at (alist-get 'last-output-at runtime))
           (last-output-stream (alist-get 'last-output-stream runtime))
           (last-output-bytes (alist-get 'last-output-bytes runtime))
           (total-output-bytes (alist-get 'total-output-bytes runtime))
           (background-command (alist-get 'background-command runtime))
           (age-s (codex-repl--runtime-age-seconds last-output-at)))
      (concat
       (format "\nRuntime: %s%s\n"
               state
               (if pid
                   (format " (root pid %s%s%s)"
                           pid
                           (if live-pids
                               (format ", live %d" (length live-pids))
                             "")
                           (if child-pids
                               (format ", children %d" (length child-pids))
                             ""))
                 ""))
       (if (and (stringp command) (not (string-empty-p command)))
           (format "Command: %s\n"
                   (codex-repl--truncate-single-line command 140))
         "")
       (if last-output-at
           (format "Last output: %s%s%s%s\n"
                   (or last-output-stream "output")
                   (if age-s (format " %ss ago" age-s) "")
                   (if last-output-bytes (format " (+%s bytes" last-output-bytes) "")
                   (if (or last-output-bytes total-output-bytes)
                       (format "%s)"
                               (if total-output-bytes
                                   (format ", total %s" total-output-bytes)
                                 ""))
                     ""))
         "")
       (if (and (stringp background-command) (not (string-empty-p background-command)))
           (format "Detached launch observed: %s (not verified after invoke exit)\n"
                   (codex-repl--truncate-single-line background-command 120))
         "")
       (if processes
           (concat "Live processes:\n"
                   (mapconcat
                    (lambda (proc)
                      (format "  %s %s"
                              (or (alist-get 'pid proc) "?")
                              (codex-repl--truncate-single-line
                               (or (alist-get 'command proc) "[command unavailable]")
                               120)))
                    (cl-subseq processes 0 (min 6 (length processes)))
                    "\n")
                   "\n")
         "")))))

(defun codex-repl--append-invoke-trace (line &optional face)
  "Record LINE with FACE in trace entries and refresh dashboard."
  (when (and (stringp line) (not (string-empty-p line)))
    (let ((entry (list (format-time-string "%H:%M:%S") line (or face 'default))))
      (setq codex-repl--invoke-trace-entries
            (last (append codex-repl--invoke-trace-entries (list entry))
                  codex-repl--invoke-trace-max)))
    (codex-repl--refresh-invoke-dashboard (current-buffer))))

(defun codex-repl--refresh-invoke-dashboard (&optional source-buffer)
  "Replace invoke buffer content with dashboard state from SOURCE-BUFFER."
  (let* ((source (or source-buffer (current-buffer)))
         (invoke-buffer-name (buffer-local-value 'codex-repl-invoke-buffer-name source))
         (buf (get-buffer invoke-buffer-name)))
    (when (and buf (buffer-live-p buf))
      (let* ((running (with-current-buffer source
                        (numberp codex-repl--thinking-start-time)))
             (elapsed (with-current-buffer source
                        (if running (codex-repl--thinking-elapsed-seconds) 0)))
             (done (buffer-local-value 'codex-repl--invoke-done-info source))
             (runtime (or (buffer-local-value 'codex-repl--runtime-state source)
                          (buffer-local-value 'codex-repl--last-runtime-state source)))
             (activity (or (buffer-local-value 'codex-repl--last-progress-status source)
                           (and running "working")))
             (spin (when running
                     (aref codex-repl--invoke-spinner
                           (mod (floor elapsed) 4))))
             (session (buffer-local-value 'codex-repl-session-id source))
             (prompt-preview (buffer-local-value 'codex-repl--invoke-prompt-preview source))
             (trace-entries (buffer-local-value 'codex-repl--invoke-trace-entries source))
             (title (buffer-name source)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Title line
          (insert (propertize
                   (cond
                    (done (format "Invoke: %s - DONE (exit=%s)\n"
                                  title
                                  (or (plist-get done :exit-code) "?")))
                    (running (format "Invoke: %s %s %ds\n" title spin elapsed))
                    (t (format "Invoke: %s\n" title)))
                   'face 'bold))
          ;; Session
          (insert (format "Session: %s\n" (or session "pending")))
          ;; Prompt preview
          (when prompt-preview
            (insert (propertize
                     (format "Prompt: %s\n" prompt-preview)
                     'face 'shadow)))
          ;; Activity
          (when (and running activity)
            (insert (propertize (format "\nActivity: %s\n" activity)
                                'face 'font-lock-keyword-face)))
          (when runtime
            (insert (propertize (codex-repl--runtime-block runtime)
                                'face 'default)))
          ;; Trace entries
          (when trace-entries
            (insert (propertize "\n--- trace ---\n" 'face 'shadow))
            (dolist (entry trace-entries)
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
                   'face 'agent-chat-thinking-face))))))))

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

(defun codex-repl--mirror-message-text (content)
  "Extract visible text from rollout CONTENT."
  (let ((parts
         (delq nil
               (mapcar
                (lambda (item)
                  (let ((kind (plist-get item :type)))
                    (cond
                     ((member kind '("input_text" "output_text"))
                      (plist-get item :text))
                     ((string= kind "text")
                      (or (plist-get item :text)
                          (plist-get (plist-get item :payload) :text)))
                     (t nil))))
                (cond
                 ((vectorp content) (append content nil))
                 ((listp content) content)
                 (t nil))))))
    (when parts
      (string-trim (string-join parts "")))))

(defun codex-repl--mirror-apply-entry (entry)
  "Apply one rollout ENTRY to the current mirror buffer."
  (let ((etype (plist-get entry :type))
        (payload (plist-get entry :payload)))
    (cond
     ((string= etype "session_meta")
      (let ((sid (plist-get payload :id))
            (cwd (plist-get payload :cwd)))
        (when sid
          (codex-repl--set-session-id-local! sid))
        (when (and (stringp cwd) (file-directory-p cwd))
          (setq-local default-directory (file-name-as-directory cwd)))
        (codex-repl-refresh-header-line t (current-buffer))))
     ((and (string= etype "response_item")
           (string= (plist-get payload :type) "message"))
      (let* ((role (plist-get payload :role))
             (text (codex-repl--mirror-message-text (plist-get payload :content))))
        (when (and (stringp text) (not (string-empty-p text)))
          (pcase role
            ("user"
             (codex-repl--frame-mark-mirror-orphaned)
             (setq codex-repl--mirror-open-frame-id
                   (codex-repl--frame-start 'mirror text
                                            (list :status 'mirrored)))
             (agent-chat-insert-message "joe" text))
            ("assistant"
             (agent-chat-insert-message "codex" text)
             (if codex-repl--mirror-open-frame-id
                 (progn
                   (codex-repl--frame-add-artifacts
                    codex-repl--mirror-open-frame-id
                    (codex-repl--text-artifacts text))
                   (codex-repl--frame-finish
                    codex-repl--mirror-open-frame-id
                    'mirrored
                    text
                    nil)
                   (setq codex-repl--mirror-open-frame-id nil))
               (let ((frame-id (codex-repl--frame-start 'mirror "[assistant-only replay]"
                                                        (list :status 'mirrored))))
                 (codex-repl--frame-add-artifacts frame-id
                                                  (codex-repl--text-artifacts text))
                 (codex-repl--frame-finish frame-id 'mirrored text nil)))
             (codex-repl--record-rendered-assistant-text! text))
            ("system" (agent-chat-insert-message "system" text))
            ("developer" nil))))))))

(defun codex-repl--mirror-read-json-line (line)
  "Parse rollout JSONL LINE into a plist, or nil on parse failure."
  (condition-case nil
      (json-parse-string line
                         :object-type 'plist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error nil)))

(defun codex-repl--stop-mirror-polling ()
  "Stop the current mirror poll timer, if any."
  (when (timerp codex-repl--mirror-poll-timer)
    (cancel-timer codex-repl--mirror-poll-timer))
  (setq codex-repl--mirror-poll-timer nil))

(defun codex-repl--cleanup-buffer ()
  "Clean up Codex REPL timers when the current buffer is killed."
  (codex-repl--stop-thinking-heartbeat)
  (codex-repl--stop-mirror-polling))

(defun codex-repl--mirror-help-line ()
  "Return the mirror-mode help line."
  "Mirror mode: rollout tail only | g refresh | q quit | RET disabled")

(defun codex-repl--mirror-init ()
  "Initialize buffer UI for rollout mirror mode."
  (setq codex-repl--cached-irc-send-base nil
        codex-repl--mirror-lines-processed 0
        codex-repl--mirror-last-mtime nil
        codex-repl--last-modeline-state nil
        codex-repl--rendered-assistant-text "")
  (codex-repl--reset-frame-registry)
  (when codex-repl--store-session-key
    (codex-repl--store-clear-session-frames))
  (setq-local codex-repl--mirror-rollout-file
              (or codex-repl--mirror-rollout-file
                  (codex-repl--recover-mirror-rollout-file)))
  (setq-local codex-repl--session-open-mode 'mirror)
  (codex-repl--ensure-store-session)
  (codex-repl--apply-ui-state-defaults)
  (setq-local agent-chat-hide-markup agent-chat-hide-markup-default)
  (setq-local agent-chat--agent-id nil)
  (setq-local agent-chat--session-id (or codex-repl-session-id "pending"))
  (let ((inhibit-read-only t)
        (title (replace-regexp-in-string
                "\\`\\*\\|\\*\\'" ""
                (or codex-repl-buffer-name "*codex-repl-mirror*"))))
    (erase-buffer)
    (insert (propertize (concat title " ") 'face 'bold)
            (propertize (format "(session: %s)\n" agent-chat--session-id)
                        'face 'font-lock-comment-face))
    (insert (propertize (format "  %s\n" (codex-repl--build-modeline))
                        'face 'font-lock-comment-face))
    (insert (propertize (format "%s\n" (codex-repl--mirror-help-line))
                        'face 'font-lock-comment-face))
    (when codex-repl--mirror-rollout-file
      (insert (propertize
               (format "  Rollout: %s\n"
                       (abbreviate-file-name codex-repl--mirror-rollout-file))
               'face 'font-lock-comment-face)))
    (insert "\n")
    (setq agent-chat--prompt-marker (point-marker))
    (setq agent-chat--separator-start (point-marker))
    (insert (propertize (make-string 72 ?─) 'face 'font-lock-comment-face) "\n")
    (insert (propertize "[mirror: read-only]\n" 'face 'font-lock-comment-face))
    (setq agent-chat--input-start (point-marker))
    (set-marker-insertion-type agent-chat--prompt-marker t)
    (set-marker-insertion-type agent-chat--input-start nil)
    (agent-chat-enable-markdown-font-lock)
    (goto-char (point-max))
    (agent-chat-scroll-to-bottom))
  (codex-repl--ensure-header-line!))

(defun codex-repl--mirror-process-lines (lines)
  "Apply rollout JSONL LINES to the current mirror buffer."
  (dolist (line lines)
    (when (and (stringp line)
               (not (string-empty-p (string-trim line))))
      (when-let ((entry (codex-repl--mirror-read-json-line line)))
        (codex-repl--mirror-apply-entry entry)))))

(defun codex-repl--mirror-file-lines ()
  "Return rollout JSONL lines for the current mirror file."
  (let ((rollout-file codex-repl--mirror-rollout-file))
    (when (and (stringp rollout-file)
               (file-readable-p rollout-file))
      (with-temp-buffer
        (insert-file-contents rollout-file)
        (split-string (buffer-string) "\n" t)))))

(defun codex-repl--mirror-refresh-full ()
  "Rebuild the current mirror buffer from the rollout file."
  (interactive)
  (unless (and (stringp codex-repl--mirror-rollout-file)
               (file-readable-p codex-repl--mirror-rollout-file))
    (user-error "Mirror rollout file is not readable"))
  (let ((lines (codex-repl--mirror-file-lines))
        (attrs (file-attributes codex-repl--mirror-rollout-file)))
    (codex-repl--mirror-init)
    (codex-repl--mirror-process-lines lines)
    (setq codex-repl--mirror-lines-processed (length lines)
          codex-repl--mirror-last-mtime (file-attribute-modification-time attrs)
          codex-repl--last-modeline-state nil)
    (codex-repl--refresh-session-header (current-buffer))
    (goto-char (point-max))
    (agent-chat-scroll-to-bottom)))

(defun codex-repl--mirror-refresh-incremental ()
  "Apply new rollout lines to the current mirror buffer."
  (when (and (stringp codex-repl--mirror-rollout-file)
             (file-readable-p codex-repl--mirror-rollout-file))
    (let* ((lines (codex-repl--mirror-file-lines))
           (count (length lines)))
      (cond
       ((< count codex-repl--mirror-lines-processed)
        (codex-repl--mirror-refresh-full))
       ((> count codex-repl--mirror-lines-processed)
        (codex-repl--mirror-process-lines
         (nthcdr codex-repl--mirror-lines-processed lines))
        (setq codex-repl--mirror-lines-processed count)
        (goto-char (point-max))
        (agent-chat-scroll-to-bottom))))))

(defun codex-repl--mirror-poll ()
  "Poll the mirror rollout file and ingest any new lines."
  (if (not (buffer-live-p (current-buffer)))
      (codex-repl--stop-mirror-polling)
    (if (not (and (stringp codex-repl--mirror-rollout-file)
                  (file-readable-p codex-repl--mirror-rollout-file)))
        (codex-repl--stop-mirror-polling)
      (let ((mtime (file-attribute-modification-time
                    (file-attributes codex-repl--mirror-rollout-file))))
        (unless (equal mtime codex-repl--mirror-last-mtime)
          (setq codex-repl--mirror-last-mtime mtime)
          (codex-repl--mirror-refresh-incremental)
          (codex-repl-refresh-header-line t (current-buffer)))))))

(defun codex-repl--start-mirror-polling (buffer)
  "Start rollout polling for mirror BUFFER."
  (with-current-buffer buffer
    (codex-repl--stop-mirror-polling)
    (let ((timer nil))
      (setq timer
            (run-at-time
             codex-repl-mirror-poll-interval
             codex-repl-mirror-poll-interval
             (lambda ()
               (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (codex-repl--mirror-poll))
                 (cancel-timer timer)))))
      (setq codex-repl--mirror-poll-timer timer))))

(defun codex-repl-mirror-refresh ()
  "Refresh the current Codex rollout mirror buffer."
  (interactive)
  (unless codex-repl--mirror-mode-p
    (user-error "Not in Codex mirror mode"))
  (unless (and (stringp codex-repl--mirror-rollout-file)
               (file-readable-p codex-repl--mirror-rollout-file))
    (setq-local codex-repl--mirror-rollout-file
                (codex-repl--recover-mirror-rollout-file)))
  (codex-repl--mirror-refresh-full)
  (codex-repl-refresh-header-line t (current-buffer))
  (message "codex-repl mirror refreshed"))

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
     ((string= type "runtime.process")
      (or (codex-repl--runtime-progress-status evt)
          "runtime update"))
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
  (when codex-repl--current-frame-id
    (codex-repl--frame-record-event
     codex-repl--current-frame-id
     evt
     (codex-repl--stream-event-summary evt))
    (when-let* ((artifact (codex-repl--frame-event-patch-artifact evt)))
      (codex-repl--frame-add-artifacts codex-repl--current-frame-id
                                       (list artifact))))
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
         ((string= type "runtime.process")
          (setq codex-repl--runtime-state evt
                codex-repl--last-runtime-state evt)
          (when-let ((status (codex-repl--runtime-progress-status evt)))
            (codex-repl--set-progress-status status)))
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

(defun codex-repl--replace-header-region (beg end replacement)
  "Replace header region from BEG to END with literal REPLACEMENT."
  (when (and beg end)
    (delete-region beg end)
    (goto-char beg)
    (insert (propertize replacement 'face 'font-lock-comment-face))))

(defun codex-repl--refresh-session-header (&optional buffer)
  "Refresh session text in BUFFER header (defaults to current buffer)."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (codex-repl--mode-active-p)
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (when (re-search-forward "(session: [^)]+)" (line-end-position 2) t)
                (let ((beg (match-beginning 0))
                      (end (match-end 0))
                      (replacement (format "(session: %s)"
                                           (or codex-repl-session-id "pending"))))
                  (codex-repl--replace-header-region beg end replacement)))
              (goto-char (point-min))
              (when (re-search-forward "^  Transports: .*$" nil t)
                (let ((beg (match-beginning 0))
                      (end (match-end 0))
                      (replacement (format "  %s" (codex-repl--build-modeline))))
                  (codex-repl--replace-header-region beg end replacement)))
              (goto-char (point-min))
              (when (and codex-repl--mirror-mode-p
                         codex-repl--mirror-rollout-file
                         (re-search-forward "^  Rollout: .*$" nil t))
                (let ((beg (match-beginning 0))
                      (end (match-end 0))
                      (replacement
                       (format "  Rollout: %s"
                               (abbreviate-file-name codex-repl--mirror-rollout-file))))
                  (codex-repl--replace-header-region beg end replacement))))
          (codex-repl-refresh-header-line nil buf)))))))

(defun codex-repl--persist-session-id! (sid)
  "Persist SID to `codex-repl-session-file` and local state."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (setq codex-repl-session-id sid)
    (when (not (equal sid codex-repl--evidence-session-id))
      (setq codex-repl--evidence-session-id sid
            codex-repl--last-evidence-id nil))
    (codex-repl--refresh-session-header (current-buffer))
    (when codex-repl-session-file
      (when-let ((session-dir (file-name-directory codex-repl-session-file)))
        (make-directory session-dir t))
      (write-region sid nil codex-repl-session-file nil 'silent))
    (codex-repl-refresh-header-line t (current-buffer))
    (when codex-repl--store-session-key
      (codex-repl--store-upsert-session))
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
  (codex-repl--refresh-session-header (current-buffer))
  (codex-repl-refresh-header-line t (current-buffer)))

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
         (frame-id codex-repl--current-frame-id)
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
    (setq codex-repl--last-runtime-state
          (or codex-repl--runtime-state codex-repl--last-runtime-state))
    (codex-repl--append-invoke-trace
     (format "invoke done exit=%d elapsed=%ds session=%s"
             exit-code elapsed trace-session)
     (if ok 'font-lock-string-face 'font-lock-warning-face))
    (when frame-id
      (codex-repl--frame-add-artifacts frame-id (codex-repl--text-artifacts final-text))
      (codex-repl--frame-finish
       frame-id
       (if ok 'done 'failed)
       final-text
       (list :exit-code exit-code
             :elapsed elapsed
             :error err-msg
             :raw-output raw)))
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
            codex-repl--runtime-state nil
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
    (codex-repl--frame-start
     'invoke
     text
     (list :turn/id codex-repl--invoke-turn-id
           :status 'running
           :transport 'agency))
    (setq codex-repl--invoke-done-info nil)
    (setq codex-repl--runtime-state nil
          codex-repl--last-runtime-state nil)
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
                          (when codex-repl--current-frame-id
                            (codex-repl--frame-finish
                             codex-repl--current-frame-id
                             'interrupted
                             codex-repl--final-message-text
                             (list :elapsed (codex-repl--thinking-elapsed-seconds)
                                   :raw-output raw)))
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

(defun codex-repl--compute-modeline-state ()
  "Return plist describing current transport/modeline state."
  (let* ((session (or codex-repl-session-id "pending"))
         (mirror? codex-repl--mirror-mode-p)
         (api-base (unless mirror? (codex-repl--resolved-api-base)))
         (agency-up (and api-base (codex-repl--base-reachable-p api-base)))
         (irc-up (unless mirror? (agent-chat-irc-available-p)))
         (transports (if mirror?
                         (list
                          (list :key 'mirror
                                :label (format "mirror (%s)"
                                               (or (and codex-repl--mirror-rollout-file
                                                        (file-name-nondirectory
                                                         codex-repl--mirror-rollout-file))
                                                   "rollout"))
                                :status 'active)
                          (list :key 'codex-repl
                                :label "emacs-codex-repl (UI only)"
                                :status 'available))
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
                                      :status 'available)))))))
    (list :current (if mirror? 'mirror 'agency)
          :current-label (if mirror?
                             "mirror (rollout tail)"
                           "agency (/api/alpha/invoke-stream)")
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
         (agency? (if codex-repl--mirror-mode-p
                      "n/a"
                    (if (plist-get state :agency-available?) "up" "down")))
         (irc? (if codex-repl--mirror-mode-p
                   "n/a"
                 (if (plist-get state :irc-available?) "up" "down")))
         (timestamp (plist-get state :timestamp))
         (time-str (format-time-string "%Y-%m-%d %H:%M:%S %Z" timestamp))
         (current (plist-get state :current-label))
         (lines (list (format "Codex REPL world @ %s" time-str)
                      (format "  Session: %s" session)
                      (format "  Current transport: %s" current)
                      (format "  Agency API: %s" agency?)
                      (format "  IRC relay: %s" irc?)
                      (if codex-repl--mirror-rollout-file
                          (format "  Rollout: %s"
                                  (abbreviate-file-name codex-repl--mirror-rollout-file))
                        "  Rollout: (none)")
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
         (agency (if codex-repl--mirror-mode-p
                     "agency:n/a"
                   (if (plist-get state :agency-available?) "agency:up" "agency:down")))
         (irc (if codex-repl--mirror-mode-p
                  "irc:n/a"
                (if (plist-get state :irc-available?) "irc:up" "irc:down")))
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
        (when (codex-repl--mode-active-p)
          (codex-repl-modeline-state refresh)
          (force-mode-line-update t))))))

(defun codex-repl--ensure-header-line! ()
  "Ensure Codex header-line is configured in current buffer."
  (when (codex-repl--mode-active-p)
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

(defun codex-repl--frame-candidates ()
  "Return completion candidates for known frames."
  (mapcar (lambda (frame-id)
            (let ((frame (codex-repl--frame-get frame-id)))
              (cons (format "%s [%s/%s] %s"
                            frame-id
                            (or (plist-get frame :frame/kind) 'frame)
                            (or (plist-get frame :status) 'unknown)
                            (or (plist-get frame :prompt-preview)
                                (alist-get "working_directory"
                                           (plist-get frame :metadata)
                                           nil nil #'string=)
                                ""))
                    frame-id)))
          codex-repl--frame-order))

(defun codex-repl--open-frame-buffer (frame-id)
  "Display inspector for FRAME-ID."
  (let ((source-buffer (current-buffer))
        (buf (get-buffer-create (codex-repl--frame-open-buffer-name frame-id))))
    (with-current-buffer buf
      (codex-repl-frame-mode)
      (setq-local codex-repl-frame--source-buffer source-buffer)
      (setq-local codex-repl-frame--frame-id frame-id)
      (codex-repl--render-frame-buffer source-buffer frame-id))
    (display-buffer buf)))

(defun codex-repl-show-last-frame ()
  "Open the most recent Codex frame inspector for this buffer."
  (interactive)
  (unless codex-repl--last-frame-id
    (user-error "No Codex frames captured yet"))
  (codex-repl--open-frame-buffer codex-repl--last-frame-id))

(defun codex-repl-show-frame (frame-id)
  "Prompt for a Codex FRAME-ID and open its inspector."
  (interactive
   (list
    (let ((candidates (codex-repl--frame-candidates)))
      (unless candidates
        (user-error "No Codex frames captured yet"))
      (cdr (assoc (completing-read "Frame: " candidates nil t nil nil
                                   (caar (last candidates)))
                  candidates)))))
  (codex-repl--open-frame-buffer frame-id))

(defun codex-repl-frame-refresh ()
  "Refresh the current frame inspector."
  (interactive)
  (unless (and (buffer-live-p codex-repl-frame--source-buffer)
               (stringp codex-repl-frame--frame-id))
    (user-error "Frame inspector is no longer attached to a live Codex buffer"))
  (codex-repl--render-frame-buffer
   codex-repl-frame--source-buffer
   codex-repl-frame--frame-id))

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
  (when codex-repl--mirror-mode-p
    (codex-repl--mirror-read-only-error "interrupt"))
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
  (when codex-repl--mirror-mode-p
    (codex-repl--mirror-read-only-error "start a new session"))
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
  (make-sparse-keymap))

(defvar codex-repl-mirror-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map))

(defvar codex-repl-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map))

(define-key codex-repl-mode-map (kbd "RET") #'codex-repl-send-input)
(define-key codex-repl-mode-map (kbd "C-l") #'recenter-top-bottom)
(define-key codex-repl-mode-map (kbd "C-c C-c") #'codex-repl-interrupt)
(define-key codex-repl-mode-map (kbd "C-c C-k") #'codex-repl-clear)
(define-key codex-repl-mode-map (kbd "C-c C-n") #'codex-repl-new-session)
(define-key codex-repl-mode-map (kbd "C-c C-a") #'futon3c-blackboard-toggle-agents-hud)
(define-key codex-repl-mode-map (kbd "C-c M-a") #'futon3c-blackboard-toggle-agents-window-display)
(define-key codex-repl-mode-map (kbd "C-c C-d") #'codex-repl-diagnose-routing)
(define-key codex-repl-mode-map (kbd "C-c C-v") #'codex-repl-show-invoke-trace)
(define-key codex-repl-mode-map (kbd "C-c C-l") #'codex-repl-clear-invoke-trace)
(define-key codex-repl-mode-map (kbd "C-c C-f") #'codex-repl-show-last-frame)

(define-key codex-repl-mirror-mode-map (kbd "g") #'codex-repl-mirror-refresh)
(define-key codex-repl-mirror-mode-map (kbd "RET") #'codex-repl-send-input)
(define-key codex-repl-mirror-mode-map (kbd "C-l") #'recenter-top-bottom)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-c") #'codex-repl-interrupt)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-k") #'codex-repl-clear)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-n") #'codex-repl-new-session)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-a") #'futon3c-blackboard-toggle-agents-hud)
(define-key codex-repl-mirror-mode-map (kbd "C-c M-a") #'futon3c-blackboard-toggle-agents-window-display)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-d") #'codex-repl-diagnose-routing)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-v") #'codex-repl-show-invoke-trace)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-l") #'codex-repl-clear-invoke-trace)
(define-key codex-repl-mirror-mode-map (kbd "C-c C-f") #'codex-repl-show-last-frame)

(define-key codex-repl-frame-mode-map (kbd "g") #'codex-repl-frame-refresh)
(define-key codex-repl-frame-mode-map (kbd "TAB")
            (if (fboundp 'outline-cycle) #'outline-cycle #'ignore))

(define-derived-mode codex-repl-mode nil "Codex-REPL"
  "Chat with Codex via CLI.
Type after the prompt, RET to send, C-c C-c to interrupt, C-c C-n for fresh session, C-c C-a for the `*agents*' HUD, C-c M-a to toggle persistent popup behavior.
\\{codex-repl-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local codex-repl--mirror-mode-p nil)
  (codex-repl--ensure-header-line!))

(define-derived-mode codex-repl-mirror-mode special-mode "Codex-Mirror"
  "Read-only Codex rollout mirror.
This mode tails a Codex rollout JSONL and replays turns without sending."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local codex-repl--mirror-mode-p t)
  (setq-local buffer-read-only t)
  (setq-local codex-repl--mirror-rollout-file
              (or codex-repl--mirror-rollout-file
                  (codex-repl--recover-mirror-rollout-file)))
  (codex-repl--ensure-header-line!))

(define-derived-mode codex-repl-frame-mode special-mode "Codex-Frame"
  "Read-only inspector for one Codex turn frame."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local outline-regexp "\\*+ ")
  (outline-minor-mode 1))

(defun codex-repl--mirror-read-only-error (action)
  "Signal a read-only mirror error for ACTION."
  (user-error "Codex mirror mode is read-only; cannot %s" action))

(defun codex-repl-send-input ()
  "Send input to Codex and display response."
  (interactive)
  (when codex-repl--mirror-mode-p
    (codex-repl--mirror-read-only-error "send input"))
  (codex-repl--ensure-input-marker-stable!)
  (agent-chat-send-input
   #'codex-repl--call-codex-async
   "codex"
   (list :before-send #'codex-repl--emit-user-turn-evidence!
         :on-response #'codex-repl--emit-assistant-turn-evidence!)))

(defun codex-repl-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (if codex-repl--mirror-mode-p
      (codex-repl-mirror-refresh)
    (codex-repl--stop-thinking-heartbeat)
    (setq codex-repl--thinking-start-time nil
          codex-repl--last-progress-status nil)
    (agent-chat-clear #'codex-repl--init)))

(defun codex-repl--init ()
  "Initialize buffer UI."
  (setq codex-repl--cached-irc-send-base nil)
  (codex-repl--ensure-session-id)
  (codex-repl--ensure-store-session)
  (agent-chat-init-buffer
   (list :title (replace-regexp-in-string
                 "\\`\\*\\|\\*\\'" ""
                 (or codex-repl-buffer-name "*codex-repl*"))
         :session-id (or codex-repl-session-id "pending")
         :modeline-fn #'codex-repl--build-modeline
         :face-alist `(("codex" . codex-repl-codex-face))
         :agent-name "codex"
         :agent-id (or codex-repl-agency-agent-id "codex-1")
         :thinking-text "codex is thinking..."
         :thinking-prop 'codex-repl-thinking))
  (agent-chat-invariants-setup)
  (add-hook 'kill-buffer-hook #'codex-repl--cleanup-buffer nil t)
  (codex-repl--ensure-header-line!))

(defun codex-repl--open-instance (buffer-name invoke-buffer-name
                                              &optional api-url agent-id session-file
                                              working-directory open-mode)
  "Open or switch to a Codex REPL instance with explicit local settings."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'codex-repl-mode)
        (codex-repl-mode))
      (setq-local codex-repl-buffer-name buffer-name)
      (setq-local codex-repl-invoke-buffer-name invoke-buffer-name)
      (when api-url
        (setq-local codex-repl-api-url (string-remove-suffix "/" api-url)))
      (when agent-id
        (setq-local codex-repl-agency-agent-id agent-id))
      (when session-file
        (setq-local codex-repl-session-file session-file))
      (when (and working-directory
                 (file-directory-p working-directory))
        (setq-local default-directory (file-name-as-directory working-directory)))
      (setq-local codex-repl--session-open-mode (or open-mode 'repl))
      (unless (codex-repl--ui-state-valid-p)
        (unless (codex-repl--restore-ui-state)
          (let ((inhibit-read-only t))
            (erase-buffer))
          (codex-repl--init))
        (codex-repl--refresh-session-header (current-buffer)))
      (codex-repl--refresh-session-header (current-buffer))
      (codex-repl--ensure-header-line!))
    (pop-to-buffer buf)
    (goto-char (point-max))
    buf))

(defun codex-repl--mirror-buffer-name (rollout-file)
  "Return a stable buffer name for ROLLOUT-FILE."
  (format "*codex-repl-mirror:%s*"
          (file-name-base rollout-file)))

(defun codex-repl--open-mirror (rollout-file)
  "Open or switch to a read-only mirror for ROLLOUT-FILE."
  (unless (file-readable-p rollout-file)
    (user-error "Rollout file is not readable: %s" rollout-file))
  (let ((buf (get-buffer-create (codex-repl--mirror-buffer-name rollout-file))))
    (with-current-buffer buf
      (unless (eq major-mode 'codex-repl-mirror-mode)
        (codex-repl-mirror-mode))
      (setq-local codex-repl-buffer-name (buffer-name buf))
      (setq-local codex-repl-invoke-buffer-name
                  (format "*invoke: %s*" (buffer-name buf)))
      (setq-local codex-repl-session-file nil)
      (setq-local codex-repl--mirror-mode-p t)
      (setq-local codex-repl--mirror-rollout-file rollout-file)
      (setq-local codex-repl-session-id nil)
      (setq-local agent-chat--session-id nil)
      (setq-local codex-repl--evidence-session-id nil)
      (setq-local codex-repl--last-evidence-id nil)
      (setq-local codex-repl--last-emitted-session-id nil)
      (codex-repl--mirror-refresh-full)
      (codex-repl--start-mirror-polling buf)
      (add-hook 'kill-buffer-hook #'codex-repl--cleanup-buffer nil t))
    (pop-to-buffer buf)
    (goto-char (point-max))
    buf))

(defun codex-repl-open-profile (name &optional api-url agent-id session-file
                                     working-directory)
  "Open a named Codex REPL profile with isolated buffer and session state."
  (interactive
   (let* ((raw-name (read-string "Codex REPL profile: "))
          (name (or (and (not (string-empty-p (codex-repl--profile-slug raw-name)))
                         (codex-repl--profile-slug raw-name))
                    "test")))
     (list name nil nil nil nil)))
  (let* ((slug (or (and (not (string-empty-p (codex-repl--profile-slug name)))
                        (codex-repl--profile-slug name))
                   "test"))
         (buffer-name (codex-repl--profile-buffer-name slug))
         (invoke-buffer-name (codex-repl--profile-invoke-buffer-name slug))
         (resolved-api-url (or api-url codex-repl-api-url))
         (resolved-agent-id (or agent-id (format "codex-%s" slug)))
         (resolved-session-file (or session-file (codex-repl--profile-session-file slug))))
    (codex-repl--open-instance buffer-name invoke-buffer-name
                               resolved-api-url
                               resolved-agent-id
                               resolved-session-file
                               working-directory
                               'profile)))

(defun codex-repl-open-reference-profile ()
  "Open the dedicated futon5 reference-test Codex REPL profile."
  (interactive)
  (let ((defaults (codex-repl--reference-profile-defaults)))
    (codex-repl--open-instance
     (plist-get defaults :buffer-name)
     (plist-get defaults :invoke-buffer-name)
     (plist-get defaults :api-url)
     (plist-get defaults :agent-id)
     (plist-get defaults :session-file)
     (plist-get defaults :working-directory)
     'reference)))

;;;###autoload
(defun codex-repl-attach-agent (agent-id)
  "Attach a Codex REPL buffer to the live headless lane AGENT-ID."
  (interactive (list (codex-repl--read-attach-agent-id)))
  (let* ((state (codex-repl--fetch-lane-process-state agent-id))
         (session-file (or (plist-get state :session-file)
                           (codex-repl--default-session-file-for-agent agent-id)))
         (working-directory (or (plist-get state :working-directory)
                                default-directory))
         (buffer (codex-repl--open-instance (codex-repl--lane-buffer-name agent-id)
                                            (codex-repl--lane-invoke-buffer-name agent-id)
                                            codex-repl-api-url
                                            agent-id
                                            session-file
                                            working-directory
                                            'attached)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (codex-repl--refresh-session-header (current-buffer))))
    (message "codex-repl: attached %s (%s)"
             agent-id
             (or (plist-get state :backing) "headless lane"))
    buffer))

;;;###autoload
(defun codex-repl-attach-codex-1 ()
  "Attach to the default live Codex lane."
  (interactive)
  (codex-repl-attach-agent "codex-1"))

;;;###autoload
(defun codex-repl ()
  "Start or switch to Codex REPL."
  (interactive)
  (codex-repl--open-instance codex-repl-buffer-name
                             codex-repl-invoke-buffer-name
                             codex-repl-api-url
                             codex-repl-agency-agent-id
                             codex-repl-session-file
                             default-directory
                             'repl))

;;;###autoload
(defun codex-repl-mirror-rollout (rollout-file)
  "Open a read-only Codex mirror for ROLLOUT-FILE."
  (interactive (list (codex-repl--mirror-prompt-rollout-file)))
  (codex-repl--open-mirror rollout-file))

;;;###autoload
(defun codex-repl-mirror-latest ()
  "Open a read-only mirror for the newest Codex rollout."
  (interactive)
  (let ((rollout (codex-repl--latest-rollout-file)))
    (unless rollout
      (user-error "No rollout JSONL files found under %s" codex-repl-sessions-root))
    (codex-repl--open-mirror rollout)))

(provide 'codex-repl)
;;; codex-repl.el ends here
