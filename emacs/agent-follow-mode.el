;;; agent-follow-mode.el --- Follow Agency invoke jobs in agent REPL buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; A buffer-local minor mode for agent-chat REPL buffers (zai-repl,
;; codex-repl, claude-repl instances) that replays turns the agent performs
;; OUTSIDE this REPL — inter-agent bells, direct HTTP invokes — into the
;; REPL buffer, by polling the Agency invoke-jobs ledger
;; (GET /api/alpha/invoke/jobs).
;;
;; Motivation (M-custom-harness, 2026-07-04): a bell-seeded zai turn ran
;; entirely server-side; the operator watching *zai-repl:zai-7* saw nothing.
;; The server side records text/tool_use events into the job ledger (see
;; record-job-stream-event! in transport/http.clj); this mode is the Emacs
;; half that renders them.  While a followed job is running, the REPL's
;; progress line shows who is invoking and the last tool used.
;;
;; Usage: M-x agent-follow-mode in a *zai-repl:...* buffer.  Turns seeded
;; by the local user are skipped by default (the REPL already streams
;; those live); set `agent-follow-include-own' to follow everything.

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup agent-follow nil
  "Replay Agency invoke-job events into agent REPL buffers."
  :group 'tools)

(defface agent-follow-marker-face
  '((t :inherit shadow))
  "Face for agent-follow job marker lines (⟲ …)."
  :group 'agent-follow)

(defcustom agent-follow-poll-interval 10
  "Seconds between FALLBACK polls of the Agency invoke-jobs ledger.
With the WS doorbell connected (`futon-agency-ws'), pushes trigger an
immediate poll, so this only paces gap-repair; 10s is plenty. Without
the doorbell it is the render latency — lower it if you disable WS."
  :type 'integer
  :group 'agent-follow)

(defvar-local agent-follow--doorbell-pending nil
  "Non-nil while a doorbell-triggered poll is already scheduled (debounce).")

(defun agent-follow--doorbell (frame)
  "WS push handler: an invoke event landed for some agent.
If it is ours, poll now (debounced 0.2s) instead of waiting for the
fallback interval. Runs in every follow-enabled buffer via the shared
connector's dispatch — filter by this buffer's agent id."
  (let ((aid (alist-get 'agent-id frame)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p agent-follow-mode)
                   agent-follow--agent-id
                   (equal aid agent-follow--agent-id)
                   (not agent-follow--doorbell-pending))
          (setq agent-follow--doorbell-pending t)
          (let ((buffer (current-buffer)))
            (run-at-time 0.2 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq agent-follow--doorbell-pending nil))
                             (condition-case nil
                                 (agent-follow--poll buffer)
                               (error nil)))))))))))

(defun agent-follow--doorbell-ensure ()
  "Connect the shared Agency WS and subscribe the doorbell, if available.
Safe no-op when `futon-agency-ws' or websocket.el is missing — the
fallback poll carries the mode alone, just slower."
  (when (require 'futon-agency-ws nil t)
    (condition-case nil
        (progn
          (futon-agency-ws-subscribe "invoke_event" #'agent-follow--doorbell)
          (unless (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws))
            (futon-agency-ws-connect)))
      (error nil))))

(defcustom agent-follow-jobs-limit 30
  "How many recent jobs to fetch per poll (filtered client-side by agent)."
  :type 'integer
  :group 'agent-follow)

(defcustom agent-follow-include-own nil
  "When non-nil, also replay jobs whose caller is the local user.
By default those are skipped: the REPL already streams its own turns."
  :type 'boolean
  :group 'agent-follow)

(declare-function futon-agency-ws-subscribe "futon-agency-ws" (type handler))
(declare-function futon-agency-ws-connect "futon-agency-ws" ())
(declare-function websocket-openp "websocket" (websocket))
(defvar futon-agency-ws--ws)

(defvar-local agent-follow--agent-id nil
  "Agent id this buffer follows.")

(defvar-local agent-follow--timer nil
  "Repeating poll timer for this buffer.")

(defvar-local agent-follow--seen nil
  "Hash table: job-id -> highest event seq already rendered.")

(defvar-local agent-follow--progress-jobs nil
  "Job-ids currently running that drive the progress line.")

(defun agent-follow--agency-url ()
  "Base URL of the Agency server."
  (string-remove-suffix
   "/"
   (or (and (boundp 'zai-repl-agency-url) zai-repl-agency-url)
       (and (boundp 'agent-chat-agency-base-url) agent-chat-agency-base-url)
       "http://localhost:7070")))

(defun agent-follow--detect-agent-id ()
  "Agent id for the current buffer: the repl's buffer-local id, else
parse a *foo-repl:AGENT* style buffer name."
  (or (and (boundp 'zai-repl-agent-id)
           (local-variable-p 'zai-repl-agent-id)
           zai-repl-agent-id)
      (and (string-match ":\\([^:*]+\\)\\*\\'" (buffer-name))
           (match-string 1 (buffer-name)))))

(defun agent-follow--event-lines (event)
  "Render one ledger EVENT alist as a list of display lines (maybe empty)."
  (let ((type (alist-get 'type event)))
    (cond
     ((equal type "text")
      (let ((txt (string-trim (or (alist-get 'text event) ""))))
        (unless (string-empty-p txt) (list txt))))
     ((equal type "tool_use")
      (let ((previews (alist-get 'previews event))
            (tools (alist-get 'tools event)))
        (cond
         ;; Collapse embedded newlines: previews must stay single display
         ;; lines or line-based fontification breaks on the tail.
         (previews (mapcar (lambda (p)
                             (format "[%s]"
                                     (replace-regexp-in-string "[\n\r]+" " ⏎ " p)))
                           (append previews nil)))
         (tools (list (format "[%s]" (mapconcat (lambda (x) (format "%s" x))
                                                (append tools nil) ", ")))))))
     ((member type '("done" "failed"))
      (list (format "⟲ %s%s" type
                    (let ((msg (alist-get 'message event)))
                      (if (and msg (not (string-empty-p (format "%s" msg))))
                          (format " (%s)" msg)
                        "")))))
     ((equal type "running")
      (list "⟲ turn started"))
     (t nil))))

(defun agent-follow--insert-message (name lines)
  "Insert LINES above the prompt as a message from NAME.
Like `agent-chat-insert-message', but tool-preview lines ([tool …]) get
`agent-chat-tool-line-face' and ⟲ marker lines get
`agent-follow-marker-face'."
  (let ((inhibit-read-only t)
        (at-end (>= (point) (marker-position agent-chat--input-start))))
    (save-excursion
      (goto-char (marker-position agent-chat--prompt-marker))
      (let ((name-start (point)))
        (insert (format "%s: " name))
        (put-text-property name-start (point) 'face 'agent-chat-prompt-face))
      (dolist (line lines)
        (let ((start (point)))
          (insert line "\n")
          (put-text-property
           start (point) 'face
           (cond ((string-prefix-p "[" line) 'agent-chat-tool-line-face)
                 ((string-prefix-p "⟲" line) 'agent-follow-marker-face)
                 (t 'agent-chat-text-face)))))
      (insert "\n"))
    (when at-end (agent-chat-scroll-to-bottom))))

(defun agent-follow--last-tool-preview (events)
  "Return the last tool preview string in EVENTS, or nil."
  (let (result)
    (dolist (e events result)
      (when (equal (alist-get 'type e) "tool_use")
        (let ((previews (append (alist-get 'previews e) nil)))
          (when previews
            (setq result (car (reverse previews)))))))))

(defun agent-follow--update-progress (job-id caller running? last-tool)
  "Show/clear the REPL progress line for a followed JOB-ID.
Never touches the progress line while the REPL streams its own turn."
  (unless (and (boundp 'agent-chat--streaming-started)
               agent-chat--streaming-started)
    (if running?
        (progn
          (unless (member job-id agent-follow--progress-jobs)
            (push job-id agent-follow--progress-jobs))
          (agent-chat-update-progress
           (format "⟲ %s invoking (bell from %s)%s"
                   agent-follow--agent-id caller
                   (if last-tool (format ": [%s]" last-tool) " …"))
           'agent-chat-prompt-face))
      (when (member job-id agent-follow--progress-jobs)
        (setq agent-follow--progress-jobs
              (delete job-id agent-follow--progress-jobs))
        (unless agent-follow--progress-jobs
          (agent-chat-remove-thinking))))))

(defun agent-follow--render-jobs (jobs)
  "Render new events from JOBS (a list of job alists) into this buffer."
  (dolist (job (reverse jobs)) ; oldest first
    (let* ((job-id (alist-get 'job-id job))
           (agent (alist-get 'agent-id job))
           (caller (or (alist-get 'caller job) "?"))
           (own (equal caller (user-login-name)))
           (state (alist-get 'state job))
           (events (append (alist-get 'events job) nil)))
      (when (and job-id
                 (equal agent agent-follow--agent-id)
                 (or agent-follow-include-own (not own)))
        (let* ((last-seen (gethash job-id agent-follow--seen 0))
               (new-events (seq-filter
                            (lambda (e) (> (or (alist-get 'seq e) 0) last-seen))
                            events))
               (lines (apply #'append
                             (mapcar #'agent-follow--event-lines new-events)))
               (max-seq (apply #'max last-seen
                               (mapcar (lambda (e) (or (alist-get 'seq e) 0))
                                       events))))
          (when (and lines (zerop last-seen))
            (push (format "⟲ following job %s (from %s)" job-id caller) lines))
          (when lines
            (agent-follow--insert-message
             (format "%s⇐%s" agent-follow--agent-id caller)
             lines))
          (puthash job-id max-seq agent-follow--seen)
          (agent-follow--update-progress
           job-id caller (equal state "running")
           (agent-follow--last-tool-preview events)))))))

(defun agent-follow--handle-response (buffer json-string)
  "Parse JSON-STRING and render into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (condition-case nil
          (let* ((data (json-parse-string json-string
                                          :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil))
                 (jobs (alist-get 'jobs data)))
            (when jobs (agent-follow--render-jobs jobs)))
        (error nil)))))

(defun agent-follow--poll (buffer)
  "Fetch recent invoke jobs and render new events into BUFFER."
  (when (buffer-live-p buffer)
    (let ((url (format "%s/api/alpha/invoke/jobs?limit=%d"
                       (with-current-buffer buffer (agent-follow--agency-url))
                       agent-follow-jobs-limit))
          (outbuf (generate-new-buffer " *agent-follow*")))
      (make-process
       :name "agent-follow"
       :buffer outbuf
       :command (list "curl" "-sS" "--max-time" "10" url)
       :noquery t
       :sentinel
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (let ((out (when (buffer-live-p (process-buffer p))
                        (with-current-buffer (process-buffer p)
                          (buffer-string)))))
             (when (buffer-live-p (process-buffer p))
               (kill-buffer (process-buffer p)))
             (when out
               (agent-follow--handle-response buffer out)))))))))

(defun agent-follow--mark-history-seen ()
  "Mark all finished jobs as fully seen so enabling the mode replays only
jobs still in flight plus anything new."
  (let ((url (format "%s/api/alpha/invoke/jobs?limit=%d"
                     (agent-follow--agency-url) agent-follow-jobs-limit))
        (buffer (current-buffer)))
    (make-process
     :name "agent-follow-init"
     :buffer (generate-new-buffer " *agent-follow-init*")
     :command (list "curl" "-sS" "--max-time" "10" url)
     :noquery t
     :sentinel
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (let ((out (when (buffer-live-p (process-buffer p))
                      (with-current-buffer (process-buffer p) (buffer-string)))))
           (when (buffer-live-p (process-buffer p))
             (kill-buffer (process-buffer p)))
           (when (and out (buffer-live-p buffer))
             (with-current-buffer buffer
               (condition-case nil
                   (let* ((data (json-parse-string out :object-type 'alist
                                                   :array-type 'list
                                                   :null-object nil
                                                   :false-object nil)))
                     (dolist (job (alist-get 'jobs data))
                       (let ((job-id (alist-get 'job-id job))
                             (state (alist-get 'state job)))
                         (when (and job-id
                                    (member state '("done" "failed")))
                           (puthash job-id
                                    (apply #'max 0
                                           (mapcar (lambda (e)
                                                     (or (alist-get 'seq e) 0))
                                                   (append (alist-get 'events job) nil)))
                                    agent-follow--seen)))))
                 (error nil))
               (agent-follow--start-timer)))))))))

(defun agent-follow--start-timer ()
  "Start (or restart) the poll timer for the current buffer.
Always cancels any existing timer first — a stale timer object must
never block creation (found live 2026-07-05: a buffer-recreate race
left a buffer with mode on but no ticking timer). The tick self-cancels
when its buffer dies."
  (agent-follow--stop-timer)
  (let ((buffer (current-buffer)) timer)
    (setq timer
          (run-at-time 0 agent-follow-poll-interval
                       (lambda ()
                         (if (not (buffer-live-p buffer))
                             ;; buffer is gone: self-cancel by identity
                             ;; (the buffer-local var died with the buffer)
                             (when timer (cancel-timer timer))
                           (condition-case nil
                               (agent-follow--poll buffer)
                             (error nil))))))
    (setq agent-follow--timer timer)
    (add-hook 'kill-buffer-hook #'agent-follow--stop-timer nil t)))

(defun agent-follow--stop-timer ()
  "Cancel this buffer's poll timer."
  (when agent-follow--timer
    (cancel-timer agent-follow--timer)
    (setq agent-follow--timer nil)))

;;;###autoload
(define-minor-mode agent-follow-mode
  "Replay this agent's out-of-REPL turns (bells, HTTP invokes) into the buffer.
Polls the Agency invoke-jobs ledger and renders text/tool events; while a
followed job runs, the REPL progress line shows the caller and last tool.
History is not replayed: on enable, finished jobs are marked seen; only
in-flight and future turns stream in."
  :lighter " ⟲follow"
  (if agent-follow-mode
      (let ((aid (agent-follow--detect-agent-id)))
        (if (not aid)
            (progn
              (setq agent-follow-mode nil)
              (message "agent-follow-mode: could not determine agent id for %s"
                       (buffer-name)))
          (setq agent-follow--agent-id aid)
          (setq agent-follow--seen (make-hash-table :test 'equal))
          (setq agent-follow--progress-jobs nil)
          ;; Marker-line highlighting via font-lock (insert-time face
          ;; properties get stripped by refontification).
          (font-lock-add-keywords
           nil '(("^⟲.*$" 0 'agent-follow-marker-face t)))
          (when font-lock-mode (font-lock-flush))
          (add-hook 'kill-buffer-hook #'agent-follow--stop-timer nil t)
          ;; Mark finished history seen (async), but ALSO start the timer
          ;; synchronously — the sentinel's later start-timer just restarts
          ;; it (idempotent-fresh). Relying on the async sentinel alone left
          ;; a race where enable completed with no ticking timer (2026-07-05).
          (agent-follow--mark-history-seen)
          (agent-follow--start-timer)
          ;; WS doorbell: push-triggered immediate polls; the timer above
          ;; becomes the gap-repair fallback (see agent-follow-poll-interval).
          (agent-follow--doorbell-ensure)
          (message "agent-follow-mode: following %s" aid)))
    (agent-follow--stop-timer)))

(provide 'agent-follow-mode)
;;; agent-follow-mode.el ends here
