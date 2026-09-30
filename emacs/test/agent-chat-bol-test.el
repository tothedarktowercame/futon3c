;;; agent-chat-bol-test.el --- C-a stops after the prompt -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-chat)

(defmacro agent-chat-bol-test--with-repl (&rest body)
  "A buffer holding a transcript line, a prompt, and typed input \"hello\"."
  `(with-temp-buffer
     (insert "transcript line\n")
     (agent-chat--insert-prompt nil "$~inbox-zero> ")
     (setq agent-chat--input-start (copy-marker (point)))
     (insert "hello")
     ,@body))

(ert-deftest agent-chat-bol-goes-to-just-after-the-prompt ()
  (agent-chat-bol-test--with-repl
   (agent-chat-beginning-of-line)
   (should (= (point) (marker-position agent-chat--input-start)))
   (should (looking-at-p "hello"))))

(ert-deftest agent-chat-bol-stays-put-when-already-after-the-prompt ()
  (agent-chat-bol-test--with-repl
   (goto-char agent-chat--input-start)
   (agent-chat-beginning-of-line)
   (should (= (point) (marker-position agent-chat--input-start)))))

(ert-deftest agent-chat-bol-from-inside-the-prompt-goes-to-line-start ()
  (agent-chat-bol-test--with-repl
   (goto-char (- (marker-position agent-chat--input-start) 3))
   (agent-chat-beginning-of-line)
   (should (bolp))
   (should (looking-at-p "\\$~inbox-zero> "))))

(ert-deftest agent-chat-bol-on-a-later-input-line-is-ordinary ()
  (agent-chat-bol-test--with-repl
   (insert "\nsecond line")
   (agent-chat-beginning-of-line)
   (should (bolp))
   (should (looking-at-p "second line"))))

(ert-deftest agent-chat-bol-in-the-transcript-is-ordinary ()
  (agent-chat-bol-test--with-repl
   (goto-char (+ (point-min) 5))
   (agent-chat-beginning-of-line)
   (should (= (point) (point-min)))))

(provide 'agent-chat-bol-test)
