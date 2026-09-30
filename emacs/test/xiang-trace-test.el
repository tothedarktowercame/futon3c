;;; xiang-trace-test.el --- planted bad cases for the 象 turn rules -*- lexical-binding: t; -*-
(require 'ert)
(require 'xiang-trace)

(defvar session-mode-turn-tags-mode nil)
(defvar session-mode-analysis-agent nil)
(defvar agent-chat--agent-id nil)
(defvar agent-chat--session-id nil)
(defvar agent-chat-user-speaker nil)
(defvar agent-chat--turn-git-heads nil)
(defvar turn-stepper--reload-failed nil)

(defun xiang-trace-test--ev (kind path &optional session age &rest props)
  "An event AGE seconds old (default an hour, so steps are overdue)."
  (append (list :at (- (float-time) (or age 3600)) :kind kind :path path :session session)
          props))

(defun xiang-trace-test--trace (&rest evs)
  "EVS oldest first -> stored order (newest first)."
  (reverse evs))

(defun xiang-trace-test--good ()
  (xiang-trace-test--trace
   (xiang-trace-test--ev 'sent "t1.json" "s")
   (xiang-trace-test--ev 'reply-ended "t1.json")
   (xiang-trace-test--ev 'dispatched "t1.json")
   (xiang-trace-test--ev 'landed "t1.json" "s" nil :stepper "s")
   (xiang-trace-test--ev 'stepper-reloaded nil "s")))

(ert-deftest xiang-trace-good-turn-passes ()
  (should-not (xiang-trace-violations (xiang-trace-test--good))))

(ert-deftest xiang-trace-reply-without-dispatch ()
  "The ff56da73 bug: reply ended, nothing sent to 象."
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'sent "t1.json" "s")
             (xiang-trace-test--ev 'reply-ended "t1.json"))))
    (should (member '("reply end → dispatch" "t1.json" violation)
                    (xiang-trace-violations tr)))))

(ert-deftest xiang-trace-dispatch-before-reply-is-not-after ()
  "Order matters: a dispatch that came BEFORE the reply ended does not count."
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'dispatched "t1.json")
             (xiang-trace-test--ev 'reply-ended "t1.json")
             (xiang-trace-test--ev 'failed "t1.json"))))
    (should (assoc "reply end → dispatch" (xiang-trace-violations tr)))))

(ert-deftest xiang-trace-silent-dispatch ()
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'reply-ended "t1.json")
             (xiang-trace-test--ev 'dispatched "t1.json"))))
    (should (member '("dispatch → reading or failure" "t1.json" violation)
                    (xiang-trace-violations tr)))))

(ert-deftest xiang-trace-recent-dispatch-is-open ()
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'reply-ended "t1.json" nil 30)
             (xiang-trace-test--ev 'dispatched "t1.json" nil 30))))
    (should (equal '(("dispatch → reading or failure" "t1.json" open))
                   (xiang-trace-violations tr)))))

(ert-deftest xiang-trace-failure-is-an-outcome ()
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'reply-ended "t1.json")
             (xiang-trace-test--ev 'dispatched "t1.json")
             (xiang-trace-test--ev 'failed "t1.json"))))
    (should-not (xiang-trace-violations tr))))

(ert-deftest xiang-trace-reading-without-reload ()
  "Stepper on screen for the session, reading landed, no reload."
  (let ((tr (butlast (reverse (xiang-trace-test--good)))))
    (should (member '("reading → stepper reload" "t1.json" violation)
                    (xiang-trace-violations (reverse tr))))))

(ert-deftest xiang-trace-no-reload-needed-when-stepper-closed ()
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'reply-ended "t1.json")
             (xiang-trace-test--ev 'dispatched "t1.json")
             (xiang-trace-test--ev 'landed "t1.json" "s" nil :stepper nil))))
    (should-not (xiang-trace-violations tr))))

(ert-deftest xiang-trace-double-dispatch ()
  (let ((tr (xiang-trace-test--trace
             (xiang-trace-test--ev 'reply-ended "t1.json")
             (xiang-trace-test--ev 'dispatched "t1.json")
             (xiang-trace-test--ev 'dispatched "t1.json")
             (xiang-trace-test--ev 'failed "t1.json"))))
    (should (member '("dispatched once" "t1.json" violation)
                    (xiang-trace-violations tr)))))

(ert-deftest xiang-trace-file-round-trip ()
  (let ((xiang-trace-file (make-temp-file "xiang-trace" nil ".jsonl"))
        (xiang-trace--events nil))
    (unwind-protect
        (progn
          (xiang-trace-record 'reply-ended "/a/t1.json")
          (xiang-trace-record 'dispatched "/a/t1.json")
          (let ((evs (xiang-trace-load-file)))
            (should (equal '(dispatched reply-ended) (mapcar (lambda (e) (plist-get e :kind)) evs)))
            (should (equal "t1.json" (plist-get (car evs) :path)))))
      (delete-file xiang-trace-file))))

(ert-deftest xiang-trace-records-a-real-streamed-turn ()
  "The recorders fire on the real code path, not only on hand-built traces.
The turn wrapper and the turn-end step are the real ones; recording the
turn and dispatching it are stubbed beneath the recorders."
  (require 'session-turn-analysis)
  (xiang-trace-enable)
  (let ((xiang-trace-file (make-temp-file "xiang-trace" nil ".jsonl"))
        (xiang-trace--events nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (cl-letf ((session-mode-turn-tags-mode t)
                      (session-mode-analysis-agent "象")
                      (agent-chat--agent-id "claude-17")
                      (agent-chat--session-id "s")
                      (agent-chat-user-speaker "joe")
                      (agent-chat--turn-git-heads nil)
                      ((symbol-function 'session-mode--split-failure-marker)
                       (lambda (text) (cons text nil)))
                      ((symbol-function 'agent-chat--walkie-command-p) (lambda (&rest _) nil))
                      ((symbol-function 'session-mode--record-add-field) (lambda (&rest _) nil))
                      ((symbol-function 'session-mode--display-analysis) (lambda (&rest _) nil))
                      ((symbol-function 'session-mode--record-turn)
                       (lambda (&rest args)
                         (apply #'xiang-trace--on-record-turn
                                (lambda (&rest _) "/tmp/t1.json") args)))
                      ((symbol-function 'session-mode--dispatch-analysis)
                       (lambda (path &rest _) (xiang-trace--on-dispatch path))))
              (session-mode--analyze-start-turn
               (lambda (call &rest _) (funcall call "PROMPT" #'ignore))
               (lambda (_sent _callback) nil)   ; streamed: callback never called
               "claude" nil "do the thing" "joe" 'operator)
              (session-mode--on-turn-finished nil nil)))
          (should (equal '(sent reply-ended dispatched)
                         (mapcar (lambda (e) (plist-get e :kind))
                                 (reverse xiang-trace--events))))
          (should (equal '(("dispatch → reading or failure" "t1.json" open))
                         (xiang-trace-violations xiang-trace--events))))
      (delete-file xiang-trace-file))))

(ert-deftest xiang-trace-failed-reload-is-not-a-reload ()
  (let ((xiang-trace-file (make-temp-file "xiang-trace" nil ".jsonl"))
        (xiang-trace--events nil))
    (unwind-protect
        (progn
          (let ((turn-stepper--reload-failed t)) (xiang-trace--on-stepper-open "s"))
          (should (eq 'stepper-reload-failed (plist-get (car xiang-trace--events) :kind))))
      (delete-file xiang-trace-file))))

(provide 'xiang-trace-test)
