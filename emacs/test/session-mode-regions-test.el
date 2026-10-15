;;; session-mode-regions-test.el --- turn text regions keyed after the send -*- lexical-binding: t; -*-
;;; Commentary:
;; Run:
;;   emacs -Q --batch -L emacs -l emacs/test/session-mode-regions-test.el -f ert-run-tests-batch-and-exit
;;; Code:
(require 'ert)
(require 'cl-lib)

(defvar agent-chat--current-turn-id nil)
(defvar session-mode--turn-text-regions nil)

(defun session-mode-regions-test--load-real-definition ()
  "Evaluate the production `session-mode--key-pending-region' from session-mode.el,
so the test exercises the definition that runs, not a copy."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "session-mode.el"
                                            (file-name-directory
                                             (directory-file-name
                                              (file-name-directory (or load-file-name buffer-file-name))))))
    (goto-char (point-min))
    (dolist (name '("session-mode--key-pending-region" "session-mode--record-streamed-segment"
                    "session-mode--retain-recent-turn-regions"))
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

(ert-deftest session-mode-retention-keeps-three-operator-turns-past-a-bell-turn ()
  "A bell or park-wake turn has agent text only.  It must not count as one
of the three retained turns, or the previous reply's regions are dropped
while the route still returns its paragraphs."
  (let ((regions (list (cons :pending (cons "operator" (cons 90 . (99))))
                       (cons "seat-turn-156" (cons "agent" (cons 70 . (89))))
                       (cons "seat-turn-155" (cons "agent" (cons 50 . (69))))
                       (cons "seat-turn-155" (cons "operator" (cons 40 . (49))))
                       (cons "seat-turn-153" (cons "agent" (cons 20 . (39))))
                       (cons "seat-turn-153" (cons "operator" (cons 10 . (19))))
                       (cons "seat-turn-151" (cons "agent" (cons 5 . (9))))
                       (cons "seat-turn-151" (cons "operator" (cons 1 . (4)))))))
    (should (equal '(:pending "seat-turn-156" "seat-turn-155" "seat-turn-155"
                     "seat-turn-153" "seat-turn-153")
                   (mapcar #'car (session-mode--retain-recent-turn-regions regions))))
    ;; with operator text in every turn the window is three turns, as before
    (should (equal '("t3" "t3" "t2" "t1")
                   (mapcar #'car (session-mode--retain-recent-turn-regions
                                  (list (cons "t3" (cons "operator" (cons 7 . (8))))
                                        (cons "t3" (cons "agent" (cons 5 . (6))))
                                        (cons "t2" (cons "operator" (cons 3 . (4))))
                                        (cons "t1" (cons "operator" (cons 1 . (2))))
                                        (cons "t0" (cons "operator" (cons 0 . (1)))))))))))

(provide 'session-mode-regions-test)
;;; session-mode-regions-test.el ends here
