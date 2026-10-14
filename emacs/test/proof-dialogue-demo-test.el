;;; proof-dialogue-demo-test.el --- Tests for local dialogue demo -*- lexical-binding: t; -*-

(require 'ert)
(require 'proof-dialogue-demo)

(ert-deftest proof-dialogue-demo-renders-frozen-local-trace ()
  (with-temp-buffer
    (proof-dialogue-demo-mode)
    (proof-dialogue-demo--render proof-dialogue-demo-trace)
    (should (string-match-p "11 moves · 11 receipts" (buffer-string)))
    (should (string-match-p "✓  m0011 +critic.*discharge" (buffer-string)))
    (should (string-match-p "dialogue is closed" (buffer-string)))
    (should-not (string-match-p "m0013" (buffer-string)))))

(ert-deftest proof-dialogue-demo-marks-a-missing-delivery ()
  (let ((file (make-temp-file "proof-dialogue-demo" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"record_type\":\"turn\",\"record_sha256\":\"abc\",\"move_id\":\"m0001\",\"parent_move_id\":null,\"role\":\"student\",\"move_kind\":\"claim\",\"body\":\"hello\"}\n"))
          (with-temp-buffer
            (proof-dialogue-demo-mode)
            (proof-dialogue-demo--render file)
            (should (string-match-p "1 moves · 0 receipts" (buffer-string)))
            (should (string-match-p "! +m0001" (buffer-string)))))
      (delete-file file))))

;;; proof-dialogue-demo-test.el ends here
