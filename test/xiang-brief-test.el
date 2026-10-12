;;; xiang-brief-test.el --- Tests for xiang-brief -*- lexical-binding: t; -*-
(require 'ert)
(require (quote cl-lib))
(load (expand-file-name "../emacs/xiang-brief.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest xiang-brief-seat-from-buffer-name ()
  (should (equal "claude-16"
                 (xiang-brief--seat-from-buffer-name "*claude-repl:claude-16*")))
  (should (equal "codex-71"
                 (xiang-brief--seat-from-buffer-name "*codex-repl:codex-71*")))
  (should-not (xiang-brief--seat-from-buffer-name "*scratch*")))

;;; xiang-brief-test.el ends here

(ert-deftest xiang-brief-roster-null-mission-is-nil ()
  "A roster entry whose mission-id is JSON null yields no default, not :null."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _)
               (insert "{\"agents\":{\"claude-19\":{\"mission-id\":null},\"codex-70\":{\"mission-id\":\"M-x\"}}}")
               0)))
    (should (null (xiang-brief--roster-mission "claude-19")))
    (should (equal "M-x" (xiang-brief--roster-mission "codex-70")))))
