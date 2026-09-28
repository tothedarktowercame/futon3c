;;; agent-repl-prompt-line-test.el --- P7a-1e done-event prompt consumers -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-chat)
(require 'kimi-repl)
(require 'zai-repl)
(ert-deftest p7a1e-kimi-and-zai-done-events-carry-prompt ()
  (dolist (handler '(kimi-repl--handle-agency-event zai-repl--handle-agency-event))
    (with-temp-buffer
      (funcall handler '((type . "done") (ok . t) (result . "r")
                         (prompt-line . "$~x/y> "))
               (list "") (list ""))
      (should (equal "$~x/y> " agent-chat--done-prompt-line)))))
(ert-deftest p7a1e-repl-done-safe-with-old-agent-chat ()
  (let ((saved (symbol-function 'agent-chat-note-done-prompt-line)))
    (unwind-protect
        (progn
          (fmakunbound 'agent-chat-note-done-prompt-line)
          (with-temp-buffer
            (kimi-repl--handle-agency-event
             '((type . "done") (ok . t) (result . "r") (prompt-line . "$~x/y> "))
             (list "") (list ""))
            (should (null agent-chat--done-prompt-line))))
      (fset 'agent-chat-note-done-prompt-line saved))))
