;;; xiang-brief-test.el --- Tests for xiang-brief -*- lexical-binding: t; -*-
(require 'ert)
(load (expand-file-name "../emacs/xiang-brief.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest xiang-brief-seat-from-buffer-name ()
  (should (equal "claude-16"
                 (xiang-brief--seat-from-buffer-name "*claude-repl:claude-16*")))
  (should (equal "codex-71"
                 (xiang-brief--seat-from-buffer-name "*codex-repl:codex-71*")))
  (should-not (xiang-brief--seat-from-buffer-name "*scratch*")))

;;; xiang-brief-test.el ends here
