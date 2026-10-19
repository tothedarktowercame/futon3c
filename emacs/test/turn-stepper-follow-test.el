;;; turn-stepper-follow-test.el --- stepper follows the active REPL -*- lexical-binding: t; -*-
;;; Commentary:
;; With `turn-stepper-follow-active-repl' non-nil (the default), a turn
;; recorded from a REPL buffer other than the stepper's source switches
;; the visible stepper to that buffer's session (an asynchronous refetch,
;; `turn-stepper--start-fetch').  With it nil the stepper stays put.
;;
;; Run:
;;   emacs -Q --batch -L emacs -l emacs/test/turn-stepper-follow-test.el \
;;     -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(load (expand-file-name
       "turn-stepper.el"
       (file-name-directory
        (directory-file-name
         (file-name-directory (or load-file-name buffer-file-name))))))

(defun turn-stepper-follow-test--record (dir session)
  "Write a turn-record JSON naming SESSION under DIR; return its path."
  (let ((path (expand-file-name (concat session "-record.json") dir)))
    (with-temp-file path
      (insert (json-encode `((session_id . ,session)))))
    path))

(defun turn-stepper-follow-test--with-stepper (source session fn)
  "Show a stepper on SESSION from SOURCE, then call FN."
  (let ((buf (turn-stepper--open session source 0 t)))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (funcall fn))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest turn-stepper-follow-active-repl-switches-to-new-repl ()
  "A visible stepper on session A from buffer X switches to session B
when a turn is recorded from buffer Y (the async fetch is what M-x
turn-stepper in Y would do)."
  (let* ((dir (make-temp-file "turn-stepper-follow" t))
         (record-b (turn-stepper-follow-test--record dir "sess-B"))
         (fetches nil)
         (X (get-buffer-create " *ts-follow-X*"))
         (Y (get-buffer-create " *ts-follow-Y*")))
    (unwind-protect
        (turn-stepper-follow-test--with-stepper X "sess-A"
          (lambda ()
            (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                       (lambda (session source &optional _quiet _tries)
                         (push (list session source) fetches))))
              (with-current-buffer Y
                (turn-stepper--turn-recorded record-b))
              (should (equal (list (list "sess-B" Y)) fetches)))))
      (dolist (b (list X Y)) (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest turn-stepper-follow-active-repl-nil-stays-on-session ()
  "With `turn-stepper-follow-active-repl' nil, a turn recorded from
another REPL leaves the stepper on its session and source buffer."
  (let* ((dir (make-temp-file "turn-stepper-follow" t))
         (record-b (turn-stepper-follow-test--record dir "sess-B"))
         (fetches nil)
         (X (get-buffer-create " *ts-follow-X*"))
         (Y (get-buffer-create " *ts-follow-Y*")))
    (unwind-protect
        (let ((turn-stepper-follow-active-repl nil))
          (turn-stepper-follow-test--with-stepper X "sess-A"
            (lambda ()
              (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                         (lambda (session source &optional _quiet _tries)
                           (push (list session source) fetches))))
                (with-current-buffer Y
                  (turn-stepper--turn-recorded record-b))
                (should (null fetches))
                (with-current-buffer (get-buffer turn-stepper-buffer-name)
                  (should (equal "sess-A" turn-stepper--session-id))
                  (should (eq X turn-stepper--source-buffer)))))))
      (dolist (b (list X Y)) (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest turn-stepper-follow-active-repl-closed-stepper-stays-closed ()
  "A closed stepper is never reopened by the follow switch."
  (let* ((dir (make-temp-file "turn-stepper-follow" t))
         (record-b (turn-stepper-follow-test--record dir "sess-B"))
         (fetches nil)
         (X (get-buffer-create " *ts-follow-X*"))
         (Y (get-buffer-create " *ts-follow-Y*")))
    (unwind-protect
        (progn
          (let ((buf (turn-stepper--open "sess-A" X 0 t)))
            (kill-buffer buf))
          (should (null (get-buffer turn-stepper-buffer-name)))
          (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                     (lambda (session source &optional _quiet _tries)
                       (push (list session source) fetches))))
            (with-current-buffer Y
              (turn-stepper--turn-recorded record-b))
            (should (null fetches))
            (should (null (get-buffer turn-stepper-buffer-name)))))
      (dolist (b (list X Y)) (when (buffer-live-p b) (kill-buffer b))))))

;;; turn-stepper-follow-test.el ends here
