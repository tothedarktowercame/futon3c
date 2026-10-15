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
    (dolist (name '("session-mode--key-pending-region" "session-mode--record-streamed-segment"))
      (goto-char (point-min))
      (search-forward (concat "(defun " name))
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(defvar agent-chat--streaming-origin nil)
(defvar agent-chat--streaming-marker nil)
(defvar session-mode--last-agent-region nil)

(ert-deftest session-mode-streamed-segment-becomes-an-agent-region ()
  "A streamed reply never passes through agent-chat-insert-message; each
segment is recorded when streaming ends, keyed to the current turn."
  (with-temp-buffer
    (insert "joe: hello\n\nfirst segment of the reply\n\n[tool summary]\n\nsecond segment\n\n")
    (let ((session-mode--turn-text-regions nil)
          (agent-chat--current-turn-id "seat-turn-3")
          (agent-chat--streaming-origin nil)
          (agent-chat--streaming-marker nil)
          (first-beg (progn (goto-char (point-min)) (search-forward "first segment") (match-beginning 0)))
          (second-beg (progn (goto-char (point-min)) (search-forward "second segment") (match-beginning 0))))
      (setq agent-chat--streaming-origin (copy-marker first-beg)
            agent-chat--streaming-marker (copy-marker (+ first-beg (length "first segment of the reply"))))
      (session-mode--record-streamed-segment)
      (setq agent-chat--streaming-origin (copy-marker second-beg)
            agent-chat--streaming-marker (copy-marker (+ second-beg (length "second segment"))))
      (session-mode--record-streamed-segment)
      (should (= 2 (length session-mode--turn-text-regions)))
      (should (cl-every (lambda (r) (and (equal "seat-turn-3" (car r)) (equal "agent" (cadr r))))
                        session-mode--turn-text-regions))
      (let ((texts (mapcar (lambda (r) (buffer-substring-no-properties
                                        (marker-position (car (cddr r)))
                                        (marker-position (cdr (cddr r)))))
                           (reverse session-mode--turn-text-regions))))
        (should (equal '("first segment of the reply" "second segment") texts)))
      ;; nothing recorded without a live origin
      (setq agent-chat--streaming-origin nil)
      (session-mode--record-streamed-segment)
      (should (= 2 (length session-mode--turn-text-regions))))))

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
