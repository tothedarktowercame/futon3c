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

(provide 'report-futon-bug-test)
;;; report-futon-bug-test.el ends here
