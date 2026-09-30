;;; agent-chat-git-log-test.el --- turn-commits parsing -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-chat)

(ert-deftest agent-chat-git-log-records-have-clean-shas ()
  "Found by the McCarthy R3 check (claude-17): every commit after the first
in a turn was recorded as \"\\n<sha>\", which names no commit."
  (let ((r (agent-chat--parse-git-log-records
            "/x/futon3c" "aaa\037t\037A\037one\036\nbbb\037t\037A\037two\036\n")))
    (should (equal '("aaa" "bbb") (mapcar (lambda (c) (alist-get 'sha c)) r)))
    (should (equal "two" (alist-get 'subject (nth 1 r))))))

(provide 'agent-chat-git-log-test)
