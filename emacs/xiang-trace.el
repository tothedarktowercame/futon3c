;;; xiang-trace.el --- 象 turn events and reazon rules over them -*- lexical-binding: t; -*-

;; M-象-2000.  Records, in order, what happens to each operator turn on its
;; way through 象 -- sent, reply ended, dispatched, reading landed or failed,
;; stepper reloaded -- and states the order these must come in as reazon
;; relations.  `xiang-trace-check' runs the rules over the live trace.
;;
;; Attached to session-turn-analysis.el and turn-stepper.el by advice only,
;; so loading or not loading this file changes nothing else.  The trace is
;; also appended to `xiang-trace-file' so it survives an Emacs restart.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'reazon nil t)

(defvar agent-chat--session-id)
(defvar session-mode--reply-pending-path)
(defvar turn-stepper-buffer-name)
(defvar turn-stepper--session-id)

(defcustom xiang-trace-file (expand-file-name "~/.emacs-graph/xiang-trace.jsonl")
  "Where the trace is appended, one JSON event per line."
  :type 'file :group 'session-mode)

(defcustom xiang-trace-open-after 900
  "Seconds after which an unfinished step counts as a violation, not open."
  :type 'integer :group 'session-mode)

(defcustom xiang-trace-live-check-window 200
  "How many of the newest events the live check re-runs the rules over.
The rules are evaluated after EVERY recorded event (I13); only this
many of the most recent events are considered, so a long session does
not make recording expensive."
  :type 'integer :group 'session-mode)

(defvar xiang-trace--events nil
  "Events, newest first.
Each is a plist (:at SECONDS :kind SYMBOL :path STR :session STR ...).")

(defconst xiang-trace--max 2000 "Events kept in memory.")

;;; ---------------------------------------------------------------- recording

(defun xiang-trace-record (kind path &rest props)
  "Record an event of KIND for the turn record at PATH, with PROPS."
  (let ((ev (append (list :at (float-time) :kind kind
                          :path (and path (file-name-nondirectory path)))
                    props)))
    (push ev xiang-trace--events)
    (when (> (length xiang-trace--events) xiang-trace--max)
      (setcdr (nthcdr (1- xiang-trace--max) xiang-trace--events) nil))
    (ignore-errors
      (let ((line (concat (json-encode
                           (cl-loop for (k v) on ev by #'cddr
                                    collect (cons (substring (symbol-name k) 1)
                                                  (if (symbolp v) (symbol-name v) v))))
                          "\n")))
        (write-region line nil xiang-trace-file t 'silent)))
    (xiang-trace--live-check)
    ev))

(defun xiang-trace--session-of (path)
  (ignore-errors (alist-get 'session_id (json-read-file path))))

(defun xiang-trace--stepper-session ()
  "Session of the stepper if it is on screen, else nil."
  (let ((b (and (boundp 'turn-stepper-buffer-name) (get-buffer turn-stepper-buffer-name))))
    (and b (get-buffer-window b t)
         (buffer-local-value 'turn-stepper--session-id b))))

(defun xiang-trace--requested-p (path)
  "Whether the record at PATH asks for a reading.
Only a record that says `not-requested' or `declared' asks for none; an
unreadable one is assumed to, so the trace never hides a missing dispatch."
  (not (member (condition-case nil
                   (let ((json-object-type 'alist))
                     (alist-get 'analysis_status (json-read-file path)))
                 (error nil))
               '("not-requested" "declared"))))

(defun xiang-trace--on-record-turn (orig &rest args)
  (let ((path (apply orig args)))
    (when path
      (xiang-trace-record 'sent path :session (bound-and-true-p agent-chat--session-id))
      ;; Under the jvm recorder the JVM dispatches at send ("soon");
      ;; Emacs never calls `session-mode--dispatch-analysis', so the
      ;; dispatch is recorded here or R1 fires on every turn.
      (when (and (eq (bound-and-true-p session-mode-turn-recorder) 'jvm)
                 (xiang-trace--requested-p path))
        (xiang-trace-record 'dispatched path)))
    path))

(defun xiang-trace--on-reply-end (&rest _)
  ;; A turn that asks for no reading (象-off, the Codex autorunner) is
  ;; never dispatched, so its reply end owes nothing.
  (when (and (bound-and-true-p session-mode--reply-pending-path)
             (xiang-trace--requested-p session-mode--reply-pending-path))
    (xiang-trace-record 'reply-ended session-mode--reply-pending-path)))

(defun xiang-trace--on-dispatch (path &rest _)
  (xiang-trace-record 'dispatched path))

(defun xiang-trace--on-landed (path)
  (xiang-trace-record 'landed path
                      :session (xiang-trace--session-of path)
                      :stepper (xiang-trace--stepper-session)))

(defun xiang-trace--on-health (state &optional detail &rest _)
  (when (and (eq state 'failing) (stringp detail)
             (string-match "\\(turn-[A-Za-z0-9]+\\)" detail))
    (xiang-trace-record 'failed (concat (match-string 1 detail) ".json") :detail detail)))

(defvar turn-stepper--reload-failed)

(defun xiang-trace--on-stepper-open (session-id &rest _)
  ;; Reopening the old frames after a failed reload is not a reload.
  (xiang-trace-record (if (bound-and-true-p turn-stepper--reload-failed)
                          'stepper-reload-failed 'stepper-reloaded)
                      nil :session session-id))

(defun xiang-trace-enable ()
  "Attach the recorders."
  (interactive)
  (advice-add 'session-mode--record-turn :around #'xiang-trace--on-record-turn)
  (advice-add 'session-mode--dispatch-pending-turn :before #'xiang-trace--on-reply-end)
  (advice-add 'session-mode--dispatch-analysis :after #'xiang-trace--on-dispatch)
  (advice-add 'session-mode--set-analysis-health :after #'xiang-trace--on-health)
  (advice-add 'turn-stepper--open :after #'xiang-trace--on-stepper-open)
  (add-hook 'session-mode-analysis-landed-functions #'xiang-trace--on-landed))

(with-eval-after-load 'session-turn-analysis (xiang-trace-enable))
(with-eval-after-load 'turn-stepper
  (advice-add 'turn-stepper--open :after #'xiang-trace--on-stepper-open))

;;; ---------------------------------------------------------------- rules

;; The trace as reazon sees it: a list, oldest first, of (KIND PATH SESSION).
;; Order is list order, so no arithmetic on times is needed to say "after".

(defun xiang-trace--facts (events)
  "EVENTS (newest first, as stored) as reazon facts, oldest first."
  (mapcar (lambda (e) (list (plist-get e :kind) (plist-get e :path)
                            (plist-get e :session)))
          (reverse events)))

(reazon-defrel xiang-trace-followso (log a b)
  "Event B comes after event A in LOG."
  (reazon-precedeso a b log))

(reazon-defrel xiang-trace-outcomeo (log path)
  "PATH's dispatch was followed by a landed reading or a failure."
  (reazon-fresh (s1 s2 k)
    (xiang-trace-followso log `(dispatched ,path ,s1) `(,k ,path ,s2))
    (reazon-conde ((reazon-== k 'landed)) ((reazon-== k 'failed)))))

(defun xiang-trace--exists (goal-fn)
  "Non-nil when the reazon goal built by GOAL-FN has a solution."
  (reazon-run 1 q (funcall goal-fn q)))

(defun xiang-trace--age (events path kind)
  "Seconds since the latest KIND event for PATH in EVENTS."
  (let ((e (cl-find-if (lambda (e) (and (eq (plist-get e :kind) kind)
                                        (equal (plist-get e :path) path)))
                       events)))
    (and e (- (float-time) (plist-get e :at)))))

(defun xiang-trace-violations (events)
  "Check EVENTS (newest first) against the 象 turn rules.
Returns a list of (RULE PATH-OR-SESSION STATUS) where STATUS is
`violation', or `open' for a step younger than `xiang-trace-open-after'."
  (let* ((log (xiang-trace--facts events))
         (out nil)
         (note (lambda (rule who kind)
                 (let ((age (and who (xiang-trace--age events who kind))))
                   (push (list rule who (if (and age (< age xiang-trace-open-after))
                                            'open 'violation))
                         out)))))
    (dolist (f log)
      (pcase f
        ;; R1: a reply end is followed by a dispatch of the same turn.
        (`(reply-ended ,p ,_)
         (unless (xiang-trace--exists
                  (lambda (_q) (reazon-fresh (s1 s2)
                                 (xiang-trace-followso log `(reply-ended ,p ,s1)
                                                       `(dispatched ,p ,s2)))))
           (funcall note "reply end → dispatch" p 'reply-ended)))
        ;; R3: a dispatch ends in a reading or a failure, never silence.
        (`(dispatched ,p ,_)
         (unless (xiang-trace--exists (lambda (_q) (xiang-trace-outcomeo log p)))
           (funcall note "dispatch → reading or failure" p 'dispatched)))
        ;; R4: a reading for the session on screen reloads the stepper.
        (`(landed ,p ,s)
         (let ((shown (plist-get (cl-find-if (lambda (e) (and (eq (plist-get e :kind) 'landed)
                                                               (equal (plist-get e :path) p)))
                                              events)
                                  :stepper)))
           (when (and s (equal s shown)
                      (not (xiang-trace--exists
                            (lambda (_q) (reazon-fresh (x y)
                                           (xiang-trace-followso log `(landed ,p ,x)
                                                                 `(stepper-reloaded ,y ,s)))))))
             (funcall note "reading → stepper reload" p 'landed))))))
    ;; R2: a turn is dispatched at most once.
    (dolist (p (delete-dups
                (reazon-run* p
                  (reazon-fresh (s1 s2)
                    (xiang-trace-followso log `(dispatched ,p ,s1) `(dispatched ,p ,s2))))))
      (push (list "dispatched once" p 'violation) out))
    (nreverse out)))

;;; ---------------------------------------------------------------- live check (I13)

;; The elephantKanren loop: the standing relations re-run on each new
;; event, and a violation shows in the 象 modeline segment beside the
;; analysis-health lighter.  Evaluation NEVER signals out of the
;; recorder: an error is caught, shown as one message, and recording
;; continues.

(defvar xiang-trace--violations nil
  "Live rule state: the current (RULE WHO violation) triples, or nil.
Recomputed after every recorded event by `xiang-trace--live-check';
read by `xiang-trace--modeline-segment'.")

(defvar xiang-trace--live-check-error nil
  "The last evaluation error message, when the live check failed.
Kept (and shown in the segment's help-echo) until a check succeeds.")

(defun xiang-trace--live-check ()
  "Re-run the rules over the newest `xiang-trace-live-check-window' events.
Sets `xiang-trace--violations' and redraws the modeline.  Never
signals: an evaluation error is caught and shown as one message."
  (condition-case err
      (let* ((n (min xiang-trace-live-check-window
                     (length xiang-trace--events)))
             (events (cl-subseq xiang-trace--events 0 n))
             (vs (cl-remove-if-not (lambda (v) (eq (nth 2 v) 'violation))
                                   (xiang-trace-violations events))))
        (setq xiang-trace--violations vs
              xiang-trace--live-check-error nil))
    (error
     (setq xiang-trace--live-check-error (error-message-string err))
     (message "象 trace live check failed (recording continues): %s"
              xiang-trace--live-check-error)))
  (force-mode-line-update t))

(defvar xiang-trace--segment-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'xiang-trace-check)
    map)
  "Keymap on the violation segment: mouse-1 opens `*象 trace check*'.")

(defun xiang-trace--modeline-segment ()
  "The violation segment beside the 象 lighter, or nil when all is well.
Names the first violated rule and the count, e.g. `!reply end → dispatch ×2'."
  (cond
   (xiang-trace--violations
    (let ((rules (delete-dups (mapcar #'car xiang-trace--violations))))
      (propertize
       (format "!%s ×%d" (car rules) (length xiang-trace--violations))
       'face '(:foreground "hot pink" :weight bold)
       'mouse-face 'mode-line-highlight
       'local-map xiang-trace--segment-map
       'help-echo
       (concat (format "象 trace violation: %s\nmouse-1: run xiang-trace-check"
                       (mapconcat #'identity rules ", "))
               (when xiang-trace--live-check-error
                 (concat "\n(live check error: " xiang-trace--live-check-error ")"))))))
   (xiang-trace--live-check-error
    (propertize "!?"
                'face '(:foreground "orange")
                'help-echo (concat "象 trace live check error: "
                                   xiang-trace--live-check-error)))))

(defun xiang-trace--lighter-with-violations (orig)
  "Append the violation segment to the 象 lighter drawn by ORIG.
Sits beside the analysis-health states; it does not change what they mean."
  (concat (funcall orig) (or (xiang-trace--modeline-segment) "")))

(with-eval-after-load 'session-mode
  (advice-add 'session-mode--analysis-lighter :around
              #'xiang-trace--lighter-with-violations))

(defun xiang-trace-load-file (&optional file)
  "Read the trace from FILE (default `xiang-trace-file'); newest first."
  (let ((file (or file xiang-trace-file)) events)
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let ((a (ignore-errors (json-read-from-string line))))
            (when a
              (push (cl-loop for (k . v) in a
                             append (list (intern (concat ":" (symbol-name k)))
                                          (if (member (symbol-name k) '("kind")) (intern v) v)))
                    events))))))
    events))

(defun xiang-trace-check (&optional since-hours)
  "Run the 象 turn rules over the trace file and show the result.
With prefix arg SINCE-HOURS, only the last that many hours."
  (interactive "P")
  (let* ((all (xiang-trace-load-file))
         (cut (and since-hours (- (float-time) (* 3600 (prefix-numeric-value since-hours)))))
         (events (if cut (cl-remove-if (lambda (e) (< (plist-get e :at) cut)) all) all))
         (vs (xiang-trace-violations events)))
    (with-current-buffer (get-buffer-create "*象 trace check*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d events; %d violations, %d open\n\n" (length events)
                        (cl-count 'violation vs :key #'caddr)
                        (cl-count 'open vs :key #'caddr)))
        (dolist (v vs)
          (insert (format "%-9s %-32s %s\n" (nth 2 v) (nth 0 v) (nth 1 v)))))
      (special-mode)
      (display-buffer (current-buffer)))
    vs))

(provide 'xiang-trace)
;;; xiang-trace.el ends here
