;;; turn-stepper-reconcile-test.el --- a visible stepper catches up -*- lexical-binding: t; -*-
;;; Commentary:
;; Joe, 2026-10-19: the Emacs stepper sat one turn behind the web page.  A
;; turn recorded while a frames fetch ran was skipped and never caught up.
;; Now a skipped update marks the stepper dirty, and `turn-stepper--reconcile'
;; refetches a visible, idle stepper that is dirty or whose record store
;; changed since the last fetch, and does nothing otherwise.
;;
;; Run:
;;   emacs -Q --batch -L emacs -l emacs/test/turn-stepper-reconcile-test.el \
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

(defmacro turn-stepper-reconcile-test--visible (&rest body)
  "Run BODY with a visible stepper on session \"s-a\"."
  `(let ((buf (turn-stepper--open "s-a" (current-buffer) 0 t)))
     (unwind-protect
         (progn (switch-to-buffer buf) ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest turn-stepper-reconcile-refetches-when-dirty ()
  (turn-stepper-reconcile-test--visible
   (let ((fetches 0))
     (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                (lambda (&rest _) (cl-incf fetches)))
               ((symbol-function 'turn-stepper--store-changed-since)
                (lambda (_) nil)))
       (setq turn-stepper--dirty t)
       (turn-stepper--reconcile)
       (should (= 1 fetches))
       (should-not turn-stepper--dirty)))))

(ert-deftest turn-stepper-reconcile-refetches-when-the-store-changed ()
  (turn-stepper-reconcile-test--visible
   (let ((fetches 0))
     (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                (lambda (&rest _) (cl-incf fetches)))
               ((symbol-function 'turn-stepper--store-changed-since)
                (lambda (_) t)))
       (setq turn-stepper--dirty nil)
       (turn-stepper--reconcile)
       (should (= 1 fetches))))))

(ert-deftest turn-stepper-reconcile-is-quiet-when-nothing-changed ()
  (turn-stepper-reconcile-test--visible
   (let ((fetches 0))
     (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                (lambda (&rest _) (cl-incf fetches)))
               ((symbol-function 'turn-stepper--store-changed-since)
                (lambda (_) nil)))
       (setq turn-stepper--dirty nil)
       (turn-stepper--reconcile)
       (should (= 0 fetches))))))

(ert-deftest turn-stepper-turn-skipped-during-a-fetch-marks-dirty ()
  (turn-stepper-reconcile-test--visible
   (let* ((dir (make-temp-file "ts-rec" t))
          (path (expand-file-name "r.json" dir)))
     (with-temp-file path (insert (json-encode '((session_id . "s-a")))))
     (setq turn-stepper--dirty nil)
     (cl-letf (((symbol-function 'get-process)
                (lambda (name) (and (equal name "turn-stepper-frames") 'running))))
       (turn-stepper--turn-recorded path))
     (should turn-stepper--dirty))))

(provide 'turn-stepper-reconcile-test)
;;; turn-stepper-reconcile-test.el ends here
