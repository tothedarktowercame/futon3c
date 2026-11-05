;;; wm-follow-mode.el --- Watch a War Machine click live -*- lexical-binding: t; -*-

;;; Commentary:
;; `M-x wm-follow' opens a read-only transcript of the current WM click.  It
;; combines the click endpoint, appended phase-log bytes, and wm-full-loop
;; Agency jobs.  All transports are GETs or reads performed by async child
;; processes; this file never mutates WM state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'agent-follow-mode)

(declare-function futon-agency-ws-subscribe "futon-agency-ws" (type handler))
(declare-function futon-agency-ws-connect "futon-agency-ws" ())
(declare-function websocket-openp "websocket" (websocket))
(defvar futon-agency-ws--ws)

(defgroup wm-follow nil "Follow a War Machine click." :group 'tools)

(defcustom wm-follow-running-interval 3
  "Seconds between polls while a click is running."
  :type 'integer :group 'wm-follow)
(defcustom wm-follow-idle-interval 15
  "Seconds between polls while no click is running."
  :type 'integer :group 'wm-follow)
(defcustom wm-follow-min-poll-gap 2.0
  "Hard floor between polls; doorbell bursts coalesce at this boundary."
  :type 'number :group 'wm-follow)
(defcustom wm-follow-max-sections 8
  "Maximum click sections retained in `*wm-follow*'."
  :type 'integer :group 'wm-follow)
(defcustom wm-follow-phase-log
  "/home/joe/code/futon2/data/wm-full-loop-phases.edn.log"
  "Append-only WM phase log."
  :type 'file :group 'wm-follow)
(defcustom wm-follow-ledger
  "/home/joe/code/futon2/data/wm-click-loop/ledger.jsonl"
  "Click supervisor ledger containing terminal cost receipts."
  :type 'file :group 'wm-follow)
(defcustom wm-follow-initial-phase-bytes (* 1024 1024)
  "Maximum phase-log tail read when first observing a click."
  :type 'integer :group 'wm-follow)

(defvar wm-follow-buffer-name "*wm-follow*")
(defvar-local wm-follow--timer nil)
(defvar-local wm-follow--inflight nil)
(defvar-local wm-follow--queued nil)
(defvar-local wm-follow--last-poll-at 0.0)
(defvar-local wm-follow--doorbell-pending nil)
(defvar-local wm-follow--click-id nil)
(defvar-local wm-follow--click-started nil)
(defvar-local wm-follow--click-running nil)
(defvar-local wm-follow--attempt-id nil)
(defvar-local wm-follow--opportunity-id nil)
(defvar-local wm-follow--external-attempt-id nil)
(defvar-local wm-follow--phase-offset 0)
(defvar-local wm-follow--phase-remainder "")
(defvar-local wm-follow--job-cursors nil)
(defvar-local wm-follow--job-marked nil)
(defvar-local wm-follow--tallies nil)
(defvar-local wm-follow--tally-marker nil)
(defvar-local wm-follow--section-markers nil)
(defvar-local wm-follow--ended-clicks nil)

(defun wm-follow--base-url () "Agency base URL." "http://localhost:7070")
(defun wm-follow--click-url () (concat (wm-follow--base-url) "/api/alpha/wm/click"))
(defun wm-follow--jobs-url () (concat (wm-follow--base-url) "/api/alpha/invoke/jobs?limit=100"))

(defun wm-follow--insert (text)
  "Append TEXT before the running tally."
  (let ((inhibit-read-only t))
    (goto-char (or (and (markerp wm-follow--tally-marker)
                        (marker-position wm-follow--tally-marker))
                   (point-max)))
    (insert text)))

(defun wm-follow--trim-sections ()
  "Drop oldest complete click sections beyond `wm-follow-max-sections'."
  (setq wm-follow--section-markers
        (cl-remove-if-not #'marker-position wm-follow--section-markers))
  (while (> (length wm-follow--section-markers) wm-follow-max-sections)
    (let ((old (pop wm-follow--section-markers))
          (next (car wm-follow--section-markers))
          (inhibit-read-only t))
      (when (and (marker-position old) (marker-position next))
        (delete-region (marker-position old) (marker-position next)))
      (set-marker old nil))))

(defun wm-follow--new-section (click)
  "Start a section for CLICK and reset per-click cursors."
  (let* ((id (alist-get 'click-id click))
         (started (alist-get 'started-at click))
         (attrs (ignore-errors (file-attributes wm-follow-phase-log)))
         (size (or (and attrs (file-attribute-size attrs)) 0))
         (marker (copy-marker (point-max))))
    (setq wm-follow--click-id id
          wm-follow--click-started started
          wm-follow--click-running (alist-get 'running? click)
          wm-follow--attempt-id (alist-get 'attempt-id click)
          wm-follow--opportunity-id nil
          wm-follow--external-attempt-id nil
          wm-follow--phase-offset (max 0 (- size wm-follow-initial-phase-bytes))
          wm-follow--phase-remainder ""
          wm-follow--job-cursors (make-hash-table :test 'equal)
          wm-follow--job-marked (make-hash-table :test 'equal)
          wm-follow--tallies (make-hash-table :test 'equal)
          wm-follow--tally-marker nil)
    (setq wm-follow--section-markers
          (append wm-follow--section-markers (list marker)))
    (wm-follow--insert
     (format "\n=== WM click %s ===\nstarted: %s  running: %s  phase: %s %s\n"
             id started (if wm-follow--click-running "yes" "no")
             (or (alist-get 'phase click) "-")
             (or (alist-get 'phase-transition click) "-")))
    (wm-follow--trim-sections)))

(defun wm-follow--handle-click (click)
  "Render CLICK status, starting a section when its identity changes."
  (let ((id (alist-get 'click-id click)))
    (when (and id (not (equal id wm-follow--click-id)))
      (wm-follow--new-section click))
    (when id
      (setq wm-follow--click-running (alist-get 'running? click)
            wm-follow--attempt-id (or (alist-get 'attempt-id click)
                                      wm-follow--attempt-id))
      (unless wm-follow--click-running
        (unless (member id wm-follow--ended-clicks)
          (push id wm-follow--ended-clicks)
          (let* ((last (alist-get 'last-result click))
                 (outcome (or (alist-get 'outcome last) "unknown"))
                 (path (or (alist-get 'run-record last)
                           (alist-get 'run-record-path last)
                           (alist-get 'run-binding last)
                           "not retained")))
            (wm-follow--insert
             (format "click ended: %s\nrun record: %s\n" outcome path))
            (wm-follow--fetch-cost id)))))))

(defun wm-follow--phase-field (name line &optional quoted)
  "Extract EDN field NAME from LINE; QUOTED expects a string."
  (let ((rx (if quoted
                (format ":%s[[:space:]]+\"\\([^\"]+\\)\"" name)
              (format ":%s[[:space:]]+\\([^,}[:space:]]+\\)" name))))
    (when (string-match rx line) (match-string 1 line))))

(defun wm-follow--phase-line-parse (line)
  "Parse the small scalar projection needed from one phase EDN LINE."
  (when (string-match-p "^[[:space:]]*{" line)
    (let ((duration (wm-follow--phase-field "duration-ms" line)))
      `((phase . ,(wm-follow--phase-field "phase" line))
        (transition . ,(wm-follow--phase-field "transition" line))
        (outcome . ,(wm-follow--phase-field "outcome" line))
        (duration-ms . ,(and duration (string-to-number duration)))
        (at . ,(wm-follow--phase-field "at" line t))
        (attempt-id . ,(wm-follow--phase-field "attempt-id" line t))
        (opportunity-id . ,(wm-follow--phase-field "opportunity-id" line t))
        (external-attempt-id . ,(wm-follow--phase-field "external-attempt-id" line t))))))

(defun wm-follow--phase-time (at)
  "Return HH:MM:SS from ISO timestamp AT."
  (if (and at (string-match "T\\([0-9:]+\\)" at))
      (substring (match-string 1 at) 0 8)
    "??:??:??"))

(defun wm-follow--phase-matches-p (phase)
  "Whether PHASE belongs to the current click; discover its identities."
  (let ((at (alist-get 'at phase))
        (attempt (alist-get 'attempt-id phase))
        (opp (alist-get 'opportunity-id phase))
        (external (alist-get 'external-attempt-id phase)))
    (when (and (or (null wm-follow--click-started)
                   (null at) (not (string< at wm-follow--click-started)))
               (or (and wm-follow--opportunity-id
                        (equal opp wm-follow--opportunity-id))
                   (and wm-follow--external-attempt-id
                        (equal external wm-follow--external-attempt-id))
                   (and wm-follow--attempt-id
                        (equal attempt wm-follow--attempt-id))))
      (setq wm-follow--opportunity-id (or wm-follow--opportunity-id opp)
            wm-follow--external-attempt-id
            (or wm-follow--external-attempt-id external))
      t)))

(defun wm-follow--phase-display-line (phase)
  "Format PHASE for the transcript."
  (let ((duration (alist-get 'duration-ms phase)))
    (format "%s  %s  %s%s%s\n"
            (wm-follow--phase-time (alist-get 'at phase))
            (string-remove-prefix ":" (or (alist-get 'phase phase) "?"))
            (string-remove-prefix ":" (or (alist-get 'transition phase) "?"))
            (if-let* ((outcome (alist-get 'outcome phase)))
                (format " %s" (string-remove-prefix ":" outcome)) "")
            (if duration (format " %.1fs" (/ duration 1000.0)) ""))))

(defun wm-follow--consume-phase-text (text)
  "Consume appended phase-log TEXT, retaining an incomplete final line."
  (let* ((all (concat wm-follow--phase-remainder text))
         (complete (string-suffix-p "\n" all))
         (parts (split-string all "\n"))
         (lines (if complete parts (butlast parts)))
         (remainder (if complete "" (car (last parts)))))
    (setq wm-follow--phase-remainder remainder)
    (dolist (line lines)
      (let ((phase (wm-follow--phase-line-parse line)))
        (when (and phase (wm-follow--phase-matches-p phase))
          (wm-follow--insert (wm-follow--phase-display-line phase)))))))

(defun wm-follow--tail-file (path offset)
  "Synchronously return (TEXT . NEW-OFFSET) for tests and small fixtures."
  (let* ((attrs (file-attributes path))
         (size (if attrs (file-attribute-size attrs) 0)))
    (if (<= size offset) (cons "" size)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path nil offset size)
        (cons (decode-coding-string (buffer-string) 'utf-8) size)))))

(defun wm-follow--fetch-phases ()
  "Asynchronously read only phase bytes appended after the saved offset."
  (when (and wm-follow--click-id (file-readable-p wm-follow-phase-log))
    (let* ((buffer (current-buffer)) (offset wm-follow--phase-offset)
           (out (generate-new-buffer " *wm-follow-phases*")))
      (make-process
       :name "wm-follow-phases" :buffer out :noquery t
       :command (list "dd" (concat "if=" wm-follow-phase-log) "bs=1"
                      (format "skip=%d" offset) "status=none")
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (let ((text (when (buffer-live-p out)
                         (with-current-buffer out (buffer-string)))))
             (when (buffer-live-p out) (kill-buffer out))
             (when (and text (buffer-live-p buffer))
               (with-current-buffer buffer
                 (setq wm-follow--phase-offset
                       (+ offset (string-bytes text)))
                 (wm-follow--consume-phase-text text))))))))))

(defun wm-follow--prompt-lines (text)
  "Render the first three lines and character count of prompt TEXT."
  (let* ((text (or text "")) (lines (split-string text "\n"))
         (head (string-join (seq-take lines 3) "\n")))
    (list (format "prompt (%d chars):\n%s%s" (length text) head
                  (if (> (length lines) 3) "\n…" "")))))

(defun wm-follow--tally-add (seat type text tools)
  "Add one event of TYPE for SEAT to the chars-not-tokens tally."
  (let ((row (copy-sequence (or (gethash seat wm-follow--tallies)
                                '(:turns 0 :prompt 0 :text 0 :tools 0)))))
    (pcase type
      ("prompt" (cl-incf (plist-get row :turns))
       (cl-incf (plist-get row :prompt) (length (or text ""))))
      ("text" (cl-incf (plist-get row :text) (length (or text ""))))
      ("tool_use" (cl-incf (plist-get row :tools) (length tools))))
    (puthash seat row wm-follow--tallies)))

(defun wm-follow--tally-snapshot ()
  "Return sorted tally rows for tests and rendering."
  (let (rows)
    (maphash (lambda (seat row) (push (cons seat row) rows)) wm-follow--tallies)
    (sort rows (lambda (a b) (string< (car a) (car b))))))

(defun wm-follow--render-tally ()
  "Replace the terminal running tally."
  (let ((inhibit-read-only t))
    (when (and (markerp wm-follow--tally-marker)
               (marker-position wm-follow--tally-marker))
      (delete-region (marker-position wm-follow--tally-marker) (point-max)))
    (setq wm-follow--tally-marker (copy-marker (point-max)))
    (goto-char (point-max))
    (insert "\nchars, not tokens:\n")
    (dolist (cell (wm-follow--tally-snapshot))
      (let ((row (cdr cell)))
        (insert (format "  %s  turns %d  prompt %d  text %d  tools %d\n"
                        (car cell) (plist-get row :turns) (plist-get row :prompt)
                        (plist-get row :text) (plist-get row :tools)))))
    ;; New events are inserted at the tally's start.  Make the marker advance
    ;; across them so repeated polls retain chronological arrival order.
    (set-marker-insertion-type wm-follow--tally-marker t)))

(defun wm-follow--job-after-start-p (job)
  "Whether JOB belongs to the current click's time window and caller."
  (let ((started (alist-get 'started-at job)))
    (and (equal (alist-get 'caller job) "wm-full-loop")
         started wm-follow--click-started
         (not (string< started wm-follow--click-started)))))

(defun wm-follow--render-jobs (jobs)
  "Render unseen events from WM JOBS, retaining one cursor per job."
  (dolist (job (reverse jobs))
    (when (wm-follow--job-after-start-p job)
      (let* ((id (alist-get 'job-id job))
             (seat (or (alist-get 'agent-id job) "wm-seat"))
             (cursor (gethash id wm-follow--job-cursors 0))
             (events (append (alist-get 'events job) nil))
             (new (seq-filter (lambda (e) (> (or (alist-get 'seq e) 0) cursor)) events)))
        (when (and new (not (gethash id wm-follow--job-marked)))
          (puthash id t wm-follow--job-marked)
          (wm-follow--insert (format "\n⟲ %s %s\n" seat id)))
        (dolist (event new)
          (let* ((type (alist-get 'type event))
                 (text (alist-get 'text event))
                 (tools (append (alist-get 'tools event) nil))
                 (lines (if (equal type "prompt")
                            (wm-follow--prompt-lines text)
                          (agent-follow--event-lines event))))
            (wm-follow--tally-add seat type text tools)
            (dolist (line lines) (wm-follow--insert (concat line "\n")))
            (puthash id (max (gethash id wm-follow--job-cursors 0)
                             (or (alist-get 'seq event) 0))
                     wm-follow--job-cursors))))))
  (wm-follow--render-tally))

(defun wm-follow--fetch-jobs ()
  "Fetch Agency jobs asynchronously and render WM seats."
  (let ((buffer (current-buffer)) (out (generate-new-buffer " *wm-follow-jobs*")))
    (make-process
     :name "wm-follow-jobs" :buffer out :noquery t
     :command (list "curl" "-sS" "--max-time" "10" (wm-follow--jobs-url))
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((text (when (buffer-live-p out) (with-current-buffer out (buffer-string)))))
           (when (buffer-live-p out) (kill-buffer out))
           (when (and text (buffer-live-p buffer))
             (with-current-buffer buffer
               (condition-case nil
                   (let* ((json (json-parse-string text :object-type 'alist
                                                   :array-type 'list
                                                   :null-object nil
                                                   :false-object nil)))
                     (wm-follow--render-jobs (alist-get 'jobs json)))
                 (error nil))))))))))

(defun wm-follow--render-cost (receipt)
  "Render per-seat token totals from cost RECEIPT."
  (let ((calls (alist-get 'calls (alist-get 'model-usage receipt))) (totals nil))
    (dolist (call calls)
      (let* ((role (alist-get 'role call))
             (seat (if (listp role) (or (alist-get 'agent role) "unknown")
                     (format "%s" role)))
             (cell (or (assoc seat totals) (car (push (cons seat '(0 0 0)) totals)))))
        (setcdr cell (list (+ (nth 0 (cdr cell)) (or (alist-get 'input-tokens call) 0))
                            (+ (nth 1 (cdr cell)) (or (alist-get 'output-tokens call) 0))
                            (+ (nth 2 (cdr cell)) (or (alist-get 'total-tokens call) 0))))))
    (when totals
      (wm-follow--insert "provider tokens (cost receipt):\n")
      (dolist (cell (sort totals (lambda (a b) (string< (car a) (car b)))))
        (wm-follow--insert
         (format "  %s  input %d  output %d  total %d\n"
                 (car cell) (nth 0 (cdr cell)) (nth 1 (cdr cell))
                 (nth 2 (cdr cell))))))))

(defun wm-follow--fetch-cost (click-id)
  "Read CLICK-ID's cost receipt asynchronously from the supervisor ledger."
  (when (file-readable-p wm-follow-ledger)
    (let ((buffer (current-buffer)) (out (generate-new-buffer " *wm-follow-cost*")))
      (make-process
       :name "wm-follow-cost" :buffer out :noquery t
       :command (list "grep" "-F" click-id wm-follow-ledger)
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (let ((text (when (buffer-live-p out) (with-current-buffer out (buffer-string)))))
             (when (buffer-live-p out) (kill-buffer out))
             (when (and text (buffer-live-p buffer))
               (with-current-buffer buffer
                 (dolist (line (split-string text "\n" t))
                   (condition-case nil
                       (let ((row (json-parse-string line :object-type 'alist
                                                     :array-type 'list
                                                     :null-object nil
                                                     :false-object nil)))
                         (when (and (equal (alist-get 'event row) "cost-receipt")
                                    (equal (alist-get 'click-id row) click-id))
                           (wm-follow--render-cost row)))
                     (error nil))))))))))))

(defun wm-follow--schedule ()
  "Schedule one next poll at the running or idle cadence."
  (when wm-follow--timer (cancel-timer wm-follow--timer))
  (let ((buffer (current-buffer)))
    (setq wm-follow--timer
          (run-at-time (if wm-follow--click-running wm-follow-running-interval
                         wm-follow-idle-interval)
                       nil #'wm-follow--poll buffer))))

(defun wm-follow--finish-poll (buffer process)
  "Handle click endpoint PROCESS for BUFFER and release single-flight state."
  (let* ((out (process-buffer process))
         (text (when (buffer-live-p out) (with-current-buffer out (buffer-string)))))
    (when (buffer-live-p out) (kill-buffer out))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unwind-protect
            (when text
              (condition-case nil
                  (let ((click (json-parse-string text :object-type 'alist
                                                  :array-type 'list
                                                  :null-object nil
                                                  :false-object nil)))
                    (wm-follow--handle-click click)
                    (wm-follow--fetch-phases)
                    (wm-follow--fetch-jobs))
                (error nil)))
          (when (eq wm-follow--inflight process) (setq wm-follow--inflight nil))
          (wm-follow--schedule)
          (when wm-follow--queued
            (setq wm-follow--queued nil)
            (run-at-time .2 nil #'wm-follow--poll buffer)))))))

(defun wm-follow--poll (buffer)
  "Asynchronously refresh BUFFER, with single-flight and a hard poll floor."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((since (- (float-time) wm-follow--last-poll-at)))
        (cond
         ((and (processp wm-follow--inflight) (process-live-p wm-follow--inflight))
          (setq wm-follow--queued t))
         ((< since wm-follow-min-poll-gap)
          (unless wm-follow--doorbell-pending
            (setq wm-follow--doorbell-pending t)
            (run-at-time (- wm-follow-min-poll-gap since) nil
                         (lambda (b)
                           (when (buffer-live-p b)
                             (with-current-buffer b (setq wm-follow--doorbell-pending nil))
                             (wm-follow--poll b))) buffer)))
         (t
          (setq wm-follow--last-poll-at (float-time))
          (let ((out (generate-new-buffer " *wm-follow-click*")))
            (setq wm-follow--inflight
                  (make-process
                   :name "wm-follow-click" :buffer out :noquery t
                   :command (list "curl" "-sS" "--max-time" "10" (wm-follow--click-url))
                   :sentinel (lambda (process _event)
                               (when (memq (process-status process) '(exit signal))
                                 (wm-follow--finish-poll buffer process))))))))))))

(defun wm-follow--doorbell (_frame)
  "Coalesced Agency event doorbell for the WM transcript."
  (when-let* ((buffer (get-buffer wm-follow-buffer-name)))
    (wm-follow--poll buffer)))

(defun wm-follow--doorbell-ensure ()
  "Subscribe to Agency invoke events when the shared websocket is available."
  (when (require 'futon-agency-ws nil t)
    (condition-case nil
        (progn
          (futon-agency-ws-subscribe "invoke_event" #'wm-follow--doorbell)
          (unless (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws))
            (futon-agency-ws-connect)))
      (error nil))))

(defun wm-follow--stop ()
  "Stop timers and transport processes owned by the current follow buffer."
  (when wm-follow--timer (cancel-timer wm-follow--timer) (setq wm-follow--timer nil))
  (when (process-live-p wm-follow--inflight) (delete-process wm-follow--inflight)))

(define-derived-mode wm-follow-mode special-mode "WM-Follow"
  "Read-only live War Machine click transcript."
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'wm-follow--stop nil t))

;;;###autoload
(defun wm-follow ()
  "Open or refresh the read-only `*wm-follow*' click transcript."
  (interactive)
  (let ((buffer (get-buffer-create wm-follow-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'wm-follow-mode) (wm-follow-mode))
      (unless wm-follow--job-cursors
        (setq wm-follow--job-cursors (make-hash-table :test 'equal)
              wm-follow--job-marked (make-hash-table :test 'equal)
              wm-follow--tallies (make-hash-table :test 'equal)))
      (wm-follow--poll buffer)
      (wm-follow--doorbell-ensure))
    (pop-to-buffer buffer)))

(provide 'wm-follow-mode)
;;; wm-follow-mode.el ends here
