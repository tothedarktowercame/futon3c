;;; agent-chat-turnline-test.el --- Live turnline tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat)

(defmacro agent-chat-turnline-test--with-buffer (&rest body)
  `(with-temp-buffer
     (let ((agent-chat-turnline-enabled t)
           (agent-chat-turnline-update-seconds 60)
           (agent-chat-turnline-animate-hidden t)
           (agent-chat-turnline-large-buffer-animation-threshold nil))
       (insert "> ")
       (setq-local agent-chat--prompt-marker (copy-marker (point-min) t))
       (setq-local agent-chat--thinking-property 'test-thinking)
       (unwind-protect
           (progn ,@body)
         (agent-chat-turnline-finish)))))

(ert-deftest agent-chat-turnline-keeps-exactly-one-live-line ()
  (agent-chat-turnline-test--with-buffer
   (agent-chat-turnline-start 'queued)
   (agent-chat-turnline-update 'tool "Bash: check corpus")
   (should (= 1 (how-many "Working" (point-min) (point-max))))
   (should-not (string-match-p "Queued" (buffer-string)))
   (should (string-match-p "Bash: check corpus" (buffer-string)))
   (should (text-property-any (point-min) (point-max)
                              'agent-chat-turnline t))))

(ert-deftest agent-chat-turnline-adopts-a-legacy-live-turn ()
  (agent-chat-turnline-test--with-buffer
   (save-excursion
     (goto-char (point-min))
     (insert (propertize "codex is thinking...\n" 'test-thinking t)))
   (agent-chat-turnline-update 'inferring "working")
   (should-not (string-match-p "codex is thinking" (buffer-string)))
   (should (string-match-p "Thinking.*working" (buffer-string)))
   (should (timerp agent-chat--turnline-timer))))

(ert-deftest agent-chat-turnline-compacting-animates-only-the-live-line ()
  (agent-chat-turnline-test--with-buffer
   (agent-chat-turnline-start 'compacting "context")
   (let ((before (buffer-string)))
     (agent-chat--turnline-tick (current-buffer))
     (should-not (equal before (buffer-string)))
     (should (= 1 (how-many "Compacting" (point-min) (point-max))))
     (should (= 1 (how-many "▰" (point-min) (point-max)))))))

(ert-deftest agent-chat-turnline-freezes-as-plain-cooked-history ()
  (agent-chat-turnline-test--with-buffer
   (agent-chat-turnline-start 'streaming)
   (agent-chat-turnline-finish)
   (goto-char (point-min))
   (insert "Cooked for 2m 56s\n")
   (should-not agent-chat--turnline-timer)
   (should-not agent-chat--turnline-marker)
   (should-not (text-property-any (point-min) (point-max)
                                  'agent-chat-turnline t))
   (should (equal 1 (how-many "Cooked for" (point-min) (point-max))))))

(ert-deftest agent-chat-turnline-large-buffer-compacting-updates-elapsed-only ()
  (agent-chat-turnline-test--with-buffer
   (setq-local agent-chat-turnline-large-buffer-animation-threshold 1)
   (agent-chat-turnline-start 'compacting "context")
   (should (string-match-p "Compacting · [0-9]" (buffer-string)))
   (should-not (string-match-p "▰" (buffer-string)))
   (setq agent-chat--turnline-start-time (- (float-time) 65))
   (agent-chat--turnline-tick (current-buffer))
   (should (string-match-p "Compacting · 1m 05s · context"
                           (buffer-string)))
   (should-not (string-match-p "▰" (buffer-string)))
   (should (= 0 agent-chat--turnline-frame))))

(ert-deftest agent-chat-turnline-custom-label-is-used ()
  (agent-chat-turnline-test--with-buffer
   (let ((agent-chat-turnline-labels '((inferring . "Cogitating"))))
     (agent-chat-turnline-start 'inferring)
     (should (string-match-p "Cogitating" (buffer-string))))))

(ert-deftest agent-chat-turnline-complete-label-is-configurable ()
  (agent-chat-turnline-test--with-buffer
   (let ((agent-chat-turnline-complete-label "Done"))
     (agent-chat--insert-turn-end-flair 3)
     (should (string-match-p "Done for 3s" (buffer-string)))
     (should-not (string-match-p "Cooked for" (buffer-string))))))

(provide 'agent-chat-turnline-test)
;;; agent-chat-turnline-test.el ends here
