;;; inbox-zero-reazon-reminder-test.el --- Tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat-invariants)
(require 'inbox-zero-reazon-reminder)

(ert-deftest inbox-zero-reazon-reminder-is-quiet-for-a-fresh-clean-receipt ()
  (cl-letf (((symbol-function 'inbox-zero-reazon-reminder--status)
             (lambda () 'healthy)))
    (should-not (inbox-zero-reazon-reminder-check))))

(ert-deftest inbox-zero-reazon-reminder-tells-joe-to-ask-codex-on-failure ()
  (cl-letf (((symbol-function 'inbox-zero-reazon-reminder--status)
             (lambda () 'failures-present)))
    (let ((violations (inbox-zero-reazon-reminder-check)))
      (should (= 1 (length violations)))
      (should (string-match-p "ask Codex" (cadar violations))))))

(ert-deftest inbox-zero-reazon-reminder-reads-live-sweeper-health ()
  (let* ((receipt (make-temp-file "inbox-zero-state-"))
         (inbox-zero-reazon-reminder-receipt-files (list receipt)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function
                      'inbox-zero-reazon-reminder--systemd-properties)
                     (lambda (_unit _properties)
                       '(("ActiveState" . "active")
                         ("SubState" . "running")))))
            (should (eq 'healthy (inbox-zero-reazon-reminder--status)))))
      (delete-file receipt))))

(ert-deftest inbox-zero-reazon-reminder-rejects-stale-sweeper-receipts ()
  (cl-letf (((symbol-function 'inbox-zero-reazon-reminder--systemd-properties)
             (lambda (_unit _properties)
               '(("ActiveState" . "active") ("SubState" . "running"))))
            ((symbol-function 'inbox-zero-reazon-reminder--newest-receipt-age)
             (lambda () (1+ inbox-zero-reazon-reminder-max-age-seconds))))
    (should (eq 'sweeper-stale (inbox-zero-reazon-reminder--status)))))

(provide 'inbox-zero-reazon-reminder-test)
;;; inbox-zero-reazon-reminder-test.el ends here
