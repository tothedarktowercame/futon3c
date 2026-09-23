;;; kimi-repl.el --- Chat with Kimi through the Futon Agency -*- lexical-binding: t; -*-

;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/agent-chat.el")
;;   (load "/home/joe/code/futon3c/emacs/kimi-repl.el")
;;   M-x kimi-repl              — open a fresh Agency-backed Kimi lane
;;   M-x kimi-repl-attach-agent — attach to an already registered kimi-N
;;
;; Modelled on zai-repl.el, which it deliberately does NOT copy whole: that
;; file still carries a direct API path (`zai-repl--call-direct') from before
;; ZAI turns always went through Agency.  A Kimi lane has never had a direct
;; path, so there is none here — every turn is an Agency turn, which is what
;; gives the lane tools, evidence, bells and interrupts.

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'agent-chat)
(require 'agent-chat-invariants)
(declare-function claude-repl--registry-model-for-agent "claude-repl" (agent-id))

;;; Configuration

(defgroup kimi-repl nil
  "Chat with Kimi models through the Futon Agency."
  :group 'agent-chat)

(defcustom kimi-repl-buffer-name "*kimi-repl*"
  "Buffer name used for the Kimi REPL instance."
  :type 'string
  :group 'kimi-repl)

(defcustom kimi-repl-agent-id "kimi-api"
  "Agency agent id associated with this Kimi REPL buffer."
  :type 'string
  :group 'kimi-repl)

(defcustom kimi-repl-agency-url agent-chat-agency-base-url
  "Base URL for the futon3c Agency API."
  :type 'string
  :group 'kimi-repl)

(defcustom kimi-repl-session-file nil
  "Optional file storing this Kimi REPL's local session id."
  :type '(choice (const nil) file)
  :group 'kimi-repl)

(defcustom kimi-repl-model
  (or (getenv "KIMI_MODEL") "k3")
  "Kimi model shown in the modeline.
The model a turn actually runs on is the seat's registry model; this is the
display default for a lane that has not reported one.  Live ids as of
2026-09-23: k3, k3-256k, kimi-for-coding, kimi-for-coding-highspeed."
  :type 'string
  :group 'kimi-repl)

(defcustom kimi-repl-turn-timeout-ms 3600000
  "Wall-clock envelope, in ms, requested for one Agency turn.

Sent as `timeout-ms' on /api/alpha/invoke-stream.  Without it the Agency
passes nil and the harness falls back to its five-minute constructor default,
which is meagre for an interactive turn.  Kept below the 3660s curl
--max-time on the same request."
  :type 'integer
  :group 'kimi-repl)

;;; Faces and state

(defface kimi-repl-kimi-face
  '((t :foreground "#b5a1ff" :weight bold))
  "Face for Kimi responses."
  :group 'kimi-repl)

(defface kimi-repl-string-face
  '((t :foreground "yellow"))
  "Face for Kimi's plain response text (the string body), so it is
distinguishable from the shared agent-chat text face the other harness
REPLs use (Joe, 2026-09-23)."
  :group 'kimi-repl)

(defface kimi-repl-tool-line-face
  '((t :inverse-video t))
  "Face for tool-use lines in Kimi REPLs.
Inverse video, matching the other harness-backed REPLs: the shared orange is
claude's."
  :group 'kimi-repl)

(defvar-local kimi-repl--session-id nil
  "Local display-only session id for this conversation.")

(defvar-local kimi-repl--pending-tool-uses nil
  "Alist of tool-call id to display detail for the active Agency turn.")

(defvar-local kimi-repl--registry-model nil
  "Model read from this agent's registry metadata at attach time.")

(defvar-local kimi-repl--turns 0
  "Completed turns in this buffer, for the modeline.")

(defun kimi-repl--refresh-displayed-model-title ()
  "Add the cached model to an already drawn first header line."
  (when (and (stringp kimi-repl--registry-model)
             (not (string-empty-p kimi-repl--registry-model)))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward " (session:" (line-end-position) t)
          (let* ((end (match-beginning 0))
                 (current (buffer-substring-no-properties (point-min) end)))
            (unless (string-suffix-p
                     (concat " · " kimi-repl--registry-model) current)
              (goto-char end)
              (insert (concat " · " kimi-repl--registry-model)))))))))

;;; Session id helpers

(defun kimi-repl--read-session-file ()
  "Return a nonempty session id from `kimi-repl-session-file', or nil.
Any read error also yields nil: the id is display-only, so a failed read
must never break buffer setup."
  (condition-case nil
      (when (and (stringp kimi-repl-session-file)
                 (file-exists-p kimi-repl-session-file))
        (let ((sid (string-trim
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents kimi-repl-session-file))
                      (buffer-string)))))
          (and (not (string-empty-p sid)) sid)))
    (error nil)))

(defun kimi-repl--new-session-id ()
  "Return a fresh local Kimi session id."
  (format "kimi-local-%s-%06d"
          (format-time-string "%Y%m%dT%H%M%S")
          (random 1000000)))

(defun kimi-repl--session-id ()
  "Return or allocate a display-only local session id."
  (or kimi-repl--session-id
      (setq kimi-repl--session-id
            (or (kimi-repl--read-session-file)
                (kimi-repl--new-session-id)))))

(defun kimi-repl--persist-session-id ()
  "Persist the current local Kimi session id when configured."
  (when (and (stringp kimi-repl-session-file)
             (stringp kimi-repl--session-id)
             (not (string-empty-p kimi-repl-session-file))
             (not (string-empty-p kimi-repl--session-id)))
    (when-let ((dir (file-name-directory kimi-repl-session-file)))
      (make-directory dir t))
    (write-region kimi-repl--session-id nil kimi-repl-session-file nil 'silent)))

(defun kimi-repl--dispatch-clock-id ()
  "Return the most specific buffer clock id for Agency invoke payloads."
  (or (agent-chat-normalize-excursion-id agent-chat--excursion-id)
      (agent-chat-normalize-mission-id agent-chat--mission-id)
      (agent-chat-normalize-campaign-id agent-chat--campaign-id)))

;;; Agency streaming

(defun kimi-repl--tool-preview (detail)
  "Return a compact display string for a tool-use DETAIL alist."
  (let* ((name (alist-get 'name detail))
         (input (alist-get 'input detail))
         (path (and (listp input)
                    (or (alist-get 'path input)
                        (alist-get 'base_dir input))))
         (cmd (and (listp input) (alist-get 'command input)))
         (pattern (and (listp input) (alist-get 'pattern input)))
         (text (or path cmd pattern "")))
    (if (string-empty-p text)
        (format "[%s]" name)
      (format "[%s] %s" name (truncate-string-to-width text 100)))))

(defun kimi-repl--stream-tool-use (json-obj)
  "Render a tool-use event from Agency JSON-OBJ."
  (let* ((tools (alist-get 'tools json-obj))
         (tool-list (cond
                     ((vectorp tools) (append tools nil))
                     ((listp tools) tools)
                     (tools (list (format "%s" tools)))
                     (t nil)))
         (details-raw (alist-get 'tool_details json-obj))
         (details (cond
                   ((vectorp details-raw) (append details-raw nil))
                   ((listp details-raw) details-raw)
                   (t nil)))
         (tool-names (mapconcat (lambda (x) (format "%s" x)) tool-list ", "))
         (tool-text (if details
                        (concat "\n"
                                (mapconcat #'kimi-repl--tool-preview details "\n")
                                "\n")
                      (format "\n[%s]\n" tool-names))))
    (dolist (detail details)
      (when-let ((tid (alist-get 'id detail)))
        (push (cons tid detail) kimi-repl--pending-tool-uses)))
    (unless agent-chat--streaming-started
      (agent-chat-begin-streaming-message "kimi"))
    (agent-chat-stream-text tool-text 'kimi-repl-tool-line-face)
    (agent-chat-update-progress
     (format "using %s" (if (string-empty-p tool-names) "tools" tool-names))
     'agent-chat-prompt-face)))

(defun kimi-repl--handle-agency-event (json-obj final-text-cell streamed-text-cell)
  "Render one Agency stream JSON-OBJ and update FINAL-TEXT-CELL."
  (let ((type (alist-get 'type json-obj)))
    (cond
     ((equal type "text")
      (let ((txt (or (alist-get 'text json-obj) "")))
        (setcar final-text-cell (concat (car final-text-cell) txt))
        (unless (string-empty-p txt)
          (setcar streamed-text-cell t))
        (unless agent-chat--streaming-started
          (agent-chat-begin-streaming-message "kimi"))
        (agent-chat-stream-text txt 'kimi-repl-string-face)))
     ((equal type "tool_use")
      (kimi-repl--stream-tool-use json-obj))
     ((equal type "tool_result")
      (when agent-chat--streaming-started
        (agent-chat-stream-text "")))
     ((equal type "invoke.activity")
      ;; A queued turn streams only "queued #N" until the running turn ends;
      ;; without rendering it the REPL sits on "thinking..." with no
      ;; explanation.
      (let ((activity (or (alist-get 'activity json-obj) "")))
        (when (and (string-match-p "queued" activity)
                   (not agent-chat--streaming-started))
          (agent-chat-insert-message
           "system"
           (format "[%s — waiting for the running turn to finish]"
                   (string-trim activity))))))
     ((equal type "done")
      (when-let ((sid (alist-get 'session-id json-obj)))
        (setq kimi-repl--session-id sid)
        (kimi-repl--persist-session-id))
      (setq kimi-repl--turns (1+ kimi-repl--turns))
      (unless (alist-get 'ok json-obj)
        (setcar final-text-cell
                (format "[kimi invoke failed: %s]"
                        (or (alist-get 'message json-obj)
                            (alist-get 'error json-obj)
                            "unknown error"))))
      (when (and (string-empty-p (car final-text-cell))
                 (alist-get 'result json-obj))
        (setcar final-text-cell (alist-get 'result json-obj)))))))

(defun kimi-repl--call-agency-streaming (text callback)
  "Send TEXT through Agency /api/alpha/invoke-stream and stream the response."
  (let* ((chat-buffer (current-buffer))
         (url (concat (string-remove-suffix "/" kimi-repl-agency-url)
                      "/api/alpha/invoke-stream"))
         (json-body (json-serialize
                     (append
                      `(:agent-id ,kimi-repl-agent-id
                        :prompt ,text
                        :surface "emacs-repl"
                        :timeout-ms ,kimi-repl-turn-timeout-ms
                        :caller ,(or (getenv "USER") user-login-name "joe"))
                      (when-let ((clock-id (kimi-repl--dispatch-clock-id)))
                        `(:mission-id ,clock-id)))))
         (outbuf (generate-new-buffer " *kimi-repl-stream*"))
         (line-buffer "")
         (final-text-cell (list ""))
         (streamed-text-cell (list nil)))
    (setq kimi-repl--pending-tool-uses nil)
    (make-process
     :name "kimi-repl-stream"
     :buffer outbuf
     :command (list "curl" "-N" "-sS" "--max-time" "3660"
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
                      (buffer-live-p chat-buffer))
             (condition-case nil
                 (let ((json-obj (json-parse-string
                                  line
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil)))
                   (with-current-buffer chat-buffer
                     (kimi-repl--handle-agency-event
                      json-obj final-text-cell streamed-text-cell)))
               (error nil))))))
     :sentinel
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (when (buffer-live-p chat-buffer)
           (with-current-buffer chat-buffer
             (when agent-chat--streaming-started
               (agent-chat-end-streaming-message))
             (if (car streamed-text-cell)
                 (progn
                   (agent-chat-remove-thinking)
                   (agent-chat-finish-turn!)
                   (agent-chat-scroll-to-bottom))
               (funcall callback (car final-text-cell)))))
         (when (buffer-live-p (process-buffer p))
           (kill-buffer (process-buffer p))))))))

(defun kimi-repl--call (text callback)
  "Send TEXT to the Kimi lane and call CALLBACK with assistant text."
  (kimi-repl--call-agency-streaming text callback))

;;; Mode

(defvar kimi-repl-mode-map
  (make-sparse-keymap))

(define-key kimi-repl-mode-map (kbd "RET") #'kimi-repl-send-input)
(define-key kimi-repl-mode-map (kbd "C-l") #'recenter-top-bottom)
(define-key kimi-repl-mode-map (kbd "C-c C-c") #'agent-chat-interrupt)
(define-key kimi-repl-mode-map (kbd "C-c C-k") #'kimi-repl-clear)
(define-key kimi-repl-mode-map (kbd "C-c C-n") #'kimi-repl-new-session)
(define-key kimi-repl-mode-map (kbd "C-c C-f") #'agent-chat-reface-buffer)
(define-key kimi-repl-mode-map (kbd "C-c C-m") #'agent-chat-clock-in)
(define-key kimi-repl-mode-map (kbd "C-c C-e") #'agent-chat-excurse)
(define-key kimi-repl-mode-map (kbd "C-c C-o") #'agent-chat-clock-menu)
(define-key kimi-repl-mode-map (kbd "C-c .") #'agent-chat-mark-menu)
(define-key kimi-repl-mode-map (kbd "C-c ,") #'agent-chat-mark-menu-2)

(defvar kimi-repl--font-lock-keywords
  ;; Highlighting must come FROM font-lock: applying face text-properties at
  ;; insert time loses to refontification, which strips properties it thinks
  ;; it owns (diagnosed in the zai lane, 2026-07-04).
  '(("^\\[[a-z_].*$" 0 'kimi-repl-tool-line-face t)
    ;; Tool preview on the same line as a follow-mode name prefix
    ;; ("kimi-1⇐claude-4: [run_shell …") — color the bracketed part.
    ("⇐[^:\n]*: \\(\\[[a-z_].*\\)$" 1 'kimi-repl-tool-line-face t)))

(define-derived-mode kimi-repl-mode nil "KIMI-REPL"
  "Chat with a Kimi Agency lane."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local line-move-visual nil)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  ;; KEYWORDS-ONLY is the t: with no font-lock-defaults, font-lock also runs
  ;; syntactic fontification under the standard syntax table, which paints
  ;; every double-quoted span of Kimi's prose font-lock-string-face (green in
  ;; Joe's theme) over the yellow body face. Chat prose is not code.
  (setq-local font-lock-defaults '(kimi-repl--font-lock-keywords t)))

(defun kimi-repl--build-modeline ()
  "Build Kimi REPL modeline text."
  (format "%s Agency: %s/api/alpha/invoke-stream | model: %s | turns: %d"
          (agent-chat-mission-segment)
          (string-remove-suffix "/" kimi-repl-agency-url)
          (or kimi-repl--registry-model kimi-repl-model)
          kimi-repl--turns))

(defun kimi-repl--init-display ()
  "Draw the Kimi REPL header and prompt."
  ;; Kimi runs on a flat-rate coding subscription, so per-turn marginal cost is
  ;; not meaningful: leave the cost vendor unset rather than price it wrongly.
  (setq-local agent-chat--cost-vendor nil)
  (agent-chat-init-buffer
   (list :title (if (and (stringp kimi-repl--registry-model)
                         (not (string-empty-p kimi-repl--registry-model)))
                    (format "kimi repl · %s" kimi-repl--registry-model)
                  "kimi repl")
         :session-id (kimi-repl--session-id)
         :modeline-fn #'kimi-repl--build-modeline
         :face-alist `(("kimi" . kimi-repl-kimi-face))
         :text-face 'kimi-repl-string-face
         :agent-name "kimi"
         :agent-id kimi-repl-agent-id
         :campaign-id agent-chat--campaign-id
         :mission-id agent-chat--mission-id
         :excursion-id agent-chat--excursion-id
         :thinking-text "kimi is thinking..."
         :thinking-prop 'kimi-repl-thinking))
  (agent-chat-invariants-setup))

(defun kimi-repl-send-input ()
  "Send input to the Kimi lane and display the response."
  (interactive)
  (agent-chat-send-input #'kimi-repl--call "kimi"))

(defun kimi-repl-clear ()
  "Clear display and redraw the current Kimi session."
  (interactive)
  (agent-chat-clear #'kimi-repl--init-display))

(defun kimi-repl-new-session (&optional target)
  "Start a fresh Kimi conversation.
With prefix argument, prompt for a clock TARGET."
  (interactive (list (when current-prefix-arg
                       (agent-chat-read-clock-target))))
  (setq kimi-repl--session-id nil)
  (setq kimi-repl--turns 0)
  (when (and kimi-repl-session-file
             (file-exists-p kimi-repl-session-file))
    (delete-file kimi-repl-session-file))
  (agent-chat-set-clock! target nil t)
  (agent-chat-clear #'kimi-repl--init-display)
  (kimi-repl--persist-session-id)
  (agent-chat-insert-message "system" "[new Kimi session]")
  (goto-char (point-max)))

;;; Registration and attachment

(defun kimi-repl--auto-register ()
  "Register a fresh Agency Kimi lane.
Return a plist containing its agent and session binding."
  (let* ((session-id (kimi-repl--new-session-id))
         (url (concat (string-remove-suffix "/" kimi-repl-agency-url)
                      "/api/alpha/agents/auto"))
         (payload (json-serialize
                   (append `(:type "kimi"
                             :session-id ,session-id
                             :cwd ,(expand-file-name default-directory))
                           (when agent-chat--campaign-id
                             `(:campaign-id ,agent-chat--campaign-id))
                           (when agent-chat--mission-id
                             `(:mission-id ,agent-chat--mission-id))
                           (when agent-chat--excursion-id
                             `(:excursion-id ,agent-chat--excursion-id)))))
         (buffer (generate-new-buffer " *kimi-repl-register*")))
    (unwind-protect
        (let ((status (call-process "curl" nil buffer nil
                                    "-sS" "--max-time" "10"
                                    "-H" "Content-Type: application/json"
                                    "-X" "POST" "-d" payload url)))
          (unless (zerop status)
            (user-error "Agency Kimi registration failed (curl exit %s)" status))
          (with-current-buffer buffer
            (goto-char (point-min))
            (let* ((response (json-parse-buffer :object-type 'alist
                                                :array-type 'list
                                                :null-object nil
                                                :false-object nil))
                   (ok (alist-get 'ok response))
                   (agent-id (alist-get 'agent-id response))
                   (session-file (alist-get 'session-file response)))
              (unless (and ok (stringp agent-id) (stringp session-file))
                (user-error "Agency Kimi registration rejected: %s"
                            (or (alist-get 'message response) response)))
              (list :agent-id agent-id
                    :session-id session-id
                    :session-file session-file))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;###autoload
(defun kimi-repl--open-instance (buffer-name agent-id session-file &optional target session-id)
  "Open a named Kimi REPL BUFFER-NAME bound to Agency AGENT-ID.
SESSION-FILE stores the local session id.  TARGET optionally clocks the buffer
into a campaign, mission, or excursion."
  (let ((buf (get-buffer-create buffer-name)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'kimi-repl-mode)
      (kimi-repl-mode))
    (setq-local kimi-repl-buffer-name buffer-name)
    (setq-local kimi-repl-agent-id agent-id)
    (setq-local kimi-repl-session-file session-file)
    (when (and (stringp session-id)
               (not (string-empty-p session-id)))
      (setq-local kimi-repl--session-id session-id))
    (when target
      (agent-chat-set-clock! target nil t))
    (unless (and (markerp agent-chat--prompt-marker)
                 (marker-position agent-chat--prompt-marker))
      (kimi-repl--init-display)
      (kimi-repl--persist-session-id))
    buf))

(defun kimi-repl--open-agency (buffer-name &optional target)
  "Open an Agency-backed Kimi BUFFER-NAME."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'kimi-repl-mode)
        (kimi-repl-mode))
      (setq-local kimi-repl-buffer-name buffer-name)
      (when target
        (agent-chat-set-clock! target nil t))
      (unless (local-variable-p 'kimi-repl-agent-id)
        (let ((binding (kimi-repl--auto-register)))
          (setq-local kimi-repl-agent-id (plist-get binding :agent-id))
          (setq-local kimi-repl-session-file (plist-get binding :session-file))
          (setq-local kimi-repl--session-id (plist-get binding :session-id))))
      (unless (and (markerp agent-chat--prompt-marker)
                   (marker-position agent-chat--prompt-marker))
        (kimi-repl--init-display)
        (kimi-repl--persist-session-id)))
    (pop-to-buffer buffer)
    (goto-char (point-max))
    buffer))

;;;###autoload
(defun kimi-repl (&optional target)
  "Open an Agency-backed Kimi REPL buffer.
With prefix argument, prompt for a clock TARGET."
  (interactive (list (when current-prefix-arg
                       (agent-chat-read-clock-target))))
  (kimi-repl--open-agency kimi-repl-buffer-name target))

(defun kimi-repl--roster-kimi-ids ()
  "Registered Kimi agent ids from the Agency roster, or nil on failure."
  (condition-case nil
      (let* ((url (concat (string-remove-suffix "/" kimi-repl-agency-url)
                          "/api/alpha/agents"))
             (data (with-temp-buffer
                     (call-process "curl" nil t nil "-sS" "--max-time" "5" url)
                     (goto-char (point-min))
                     (json-parse-buffer :object-type 'alist)))
             ids)
        (dolist (pair (alist-get 'agents data) (nreverse ids))
          (let ((id (symbol-name (car pair))))
            (when (string-prefix-p "kimi" id)
              (push id ids)))))
    (error nil)))

(defun kimi-repl--read-attach-agent-id ()
  "Prompt for a registered Kimi agent with completion."
  (let* ((agent-ids (kimi-repl--roster-kimi-ids))
         (default (or (car agent-ids) "kimi-1")))
    (if agent-ids
        (completing-read (format "Attach Kimi agent (default %s): " default)
                         agent-ids nil t nil nil default)
      (read-string "Attach Kimi agent: " default))))

(defun kimi-repl--attach-registered-agent (agent-id)
  "Attach to registered Kimi AGENT-ID."
  (let ((agent-id (string-trim agent-id)))
    (when (string-empty-p agent-id)
      (user-error "Kimi agent id cannot be empty"))
    (let ((all-kimi-ids (kimi-repl--roster-kimi-ids)))
      (unless all-kimi-ids
        (user-error "Cannot verify Kimi agent: Agency roster unavailable"))
      (unless (member agent-id all-kimi-ids)
        (user-error "%s is not a registered Kimi agent" agent-id)))
    (let* ((buffer-name (format "*kimi-repl:%s*" agent-id))
           (_model-cache
            (with-current-buffer (get-buffer-create buffer-name)
              (unless (eq major-mode 'kimi-repl-mode)
                (kimi-repl-mode))
              (setq-local kimi-repl--registry-model
                          (and (fboundp 'claude-repl--registry-model-for-agent)
                               (claude-repl--registry-model-for-agent agent-id)))))
           (buffer
            (kimi-repl--open-instance
             buffer-name
             agent-id
             (format "/tmp/futon-kimi-session-id-%s" agent-id))))
      (message "kimi-repl: attached to %s" agent-id)
      (with-current-buffer buffer
        (kimi-repl--refresh-displayed-model-title))
      buffer)))

;;;###autoload
(defun kimi-repl-attach-agent (agent-id)
  "Attach a Kimi REPL buffer to Agency AGENT-ID (e.g. \"kimi-1\").
Fetches live Kimi agents from the Agency roster for completion.
The buffer is named *kimi-repl:AGENT-ID* and shares the agent's server-side
session file, so it attaches to the same identity the Agency invokes."
  (interactive (list (kimi-repl--read-attach-agent-id)))
  (kimi-repl--attach-registered-agent agent-id))

;;;###autoload
(defalias 'kimi-repl-for-agent #'kimi-repl-attach-agent
  "Compatibility alias for `kimi-repl-attach-agent'.")

(provide 'kimi-repl)
;;; kimi-repl.el ends here
