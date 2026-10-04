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

(ert-deftest inbox-zero-reazon-reminder-reads-live-systemd-health ()
  (let ((inbox-zero-reazon-reminder-state-file
         (make-temp-file "inbox-zero-state-")))
    (unwind-protect
        (progn
          (with-temp-file inbox-zero-reazon-reminder-state-file
            (insert "{\"signature\":{\"futon3c\":[]}}"))
          (cl-letf (((symbol-function
                      'inbox-zero-reazon-reminder--systemd-properties)
                     (lambda (unit _properties)
                       (if (string-suffix-p ".timer" unit)
                           '(("ActiveState" . "active"))
                         `(("Result" . "success")
                           ("ExecMainStatus" . "0")
                           ("ExecMainExitTimestamp" .
                            ,(format-time-string "%a %Y-%m-%d %H:%M:%S UTC"
                                                 nil t)))))))
            (should (eq 'healthy (inbox-zero-reazon-reminder--status)))))
      (delete-file inbox-zero-reazon-reminder-state-file))))

(provide 'inbox-zero-reazon-reminder-test)
;;; inbox-zero-reazon-reminder-test.el ends here
