;;; session-mode-regions-test.el --- turn text regions keyed after the send -*- lexical-binding: t; -*-
;;; Commentary:
;; Run:
;;   emacs -Q --batch -L emacs -l emacs/test/session-mode-regions-test.el -f ert-run-tests-batch-and-exit
;;; Code:
(require 'ert)
(require 'cl-lib)

(defvar agent-chat--current-turn-id nil)
(defvar session-mode--turn-text-regions nil)

(defun session-mode--retain-recent-turn-regions (regions)
  (let ((turns (seq-take (delete-dups (mapcar #'car regions)) 3)))
    (cl-remove-if-not (lambda (entry) (member (car entry) turns)) regions)))

(defun session-mode-regions-test--load-real-definition ()
  "Evaluate the production `session-mode--key-pending-region' from session-mode.el,
so the test exercises the definition that runs, not a copy."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "session-mode.el"
                                            (file-name-directory
                                             (directory-file-name
                                              (file-name-directory (or load-file-name buffer-file-name))))))
    (goto-char (point-min))
    (search-forward "(defun session-mode--key-pending-region")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(session-mode-regions-test--load-real-definition)

(ert-deftest session-mode-pending-operator-region-gets-the-new-turn-id ()
  "The operator's text is inserted before the turn counter advances, so the
region is recorded as :pending and keyed once the send assigns the id."
  (let ((session-mode--turn-text-regions
         (list (cons :pending (cons "operator" (cons 10 . (20))))
               (cons "seat-turn-7" (cons "agent" (cons 1 . (9))))))
        (agent-chat--current-turn-id "seat-turn-8"))
    (session-mode--key-pending-region)
    (should (equal '("seat-turn-8" "seat-turn-7")
                   (mapcar #'car session-mode--turn-text-regions)))
    (should (equal "operator" (cadr (assoc "seat-turn-8" session-mode--turn-text-regions))))
    ;; a second send re-keys only the new pending entry
    (push (cons :pending (cons "operator" (cons 30 . (40)))) session-mode--turn-text-regions)
    (setq agent-chat--current-turn-id "seat-turn-9")
    (session-mode--key-pending-region)
    (should (equal '("seat-turn-9" "seat-turn-8" "seat-turn-7")
                   (mapcar #'car session-mode--turn-text-regions)))))

(provide 'session-mode-regions-test)
;;; session-mode-regions-test.el ends here
