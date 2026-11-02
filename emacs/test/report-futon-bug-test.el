;;; report-futon-bug-test.el --- Tests for bug feed attachment -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../report-futon-bug.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)

(ert-deftest futon-bug-feed-attach-persists-and-detach-removes ()
  (let* ((dir (make-temp-file "bug-feed-test-" t))
         (futon-bug-feed-attachment-file (expand-file-name "feed/agent" dir)))
    (unwind-protect
        (progn
          (should (equal "codex-7" (futon-bug-feed-attach " codex-7 ")))
          (should (equal "codex-7" (futon-bug-feed--attached-agent)))
          (should (= #o600 (logand #o777 (file-modes futon-bug-feed-attachment-file))))
          (should (equal "codex-7" (futon-bug-feed-status)))
          (should (equal "codex-7" (futon-bug-feed-detach)))
          (should-not (file-exists-p futon-bug-feed-attachment-file)))
      (delete-directory dir t))))

(ert-deftest report-futon-bug-command-delegates-to-background-entry-point ()
  (let (seen)
    (cl-letf (((symbol-function 'report-futon-bug-async)
               (lambda (summary agent)
                 (setq seen (list summary agent))
                 '(:accepted t :process 42))))
      (should (equal '(:accepted t :process 42)
                     (report-futon-bug "slow UI" "codex-7")))
      (should (equal '("slow UI" "codex-7") seen)))))

(ert-deftest report-futon-bug-messages-buffer-is-not-copied-twice ()
  (with-current-buffer (messages-buffer)
    (let ((context (report-futon-bug--emacs-context)))
      (should (string-match-p "Last 80 lines of \\*Messages\\*" context))
      (should-not (string-match-p "### \\*Messages\\* (last" context)))))

(provide 'report-futon-bug-test)
;;; report-futon-bug-test.el ends here
