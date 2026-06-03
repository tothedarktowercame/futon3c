;;; agent-chat-test.el --- Tests for shared chat buffer behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat)

(defun agent-chat-test--init-buffer ()
  (cl-letf (((symbol-function 'agent-chat--refresh-session-turn-count)
             (lambda (&rest _) nil)))
    (agent-chat-init-buffer
     (list :title "agent chat test"
           :session-id "sid-test"
           :modeline-fn (lambda () "test modeline")
           :agent-name "agent"
           :agent-id "agent-1"
           :face-alist nil
           :thinking-text "agent is thinking..."
           :thinking-prop 'agent-chat-test-thinking))))

(ert-deftest agent-chat-ensure-prompt-markers-preserves-live-input ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((original-input-start (marker-position agent-chat--input-start)))
      (insert "first line\n> quoted line in pending input\nfinal line")
      (agent-chat--ensure-prompt-markers!)
      (should (= original-input-start
                 (marker-position agent-chat--input-start)))
      (should (equal "first line\n> quoted line in pending input\nfinal line"
                     (buffer-substring-no-properties
                      (marker-position agent-chat--input-start)
                      (point-max)))))))

(ert-deftest agent-chat-send-input-keeps-multiline-prompt-input ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let (sent-text callback)
      (insert "first line\n> quoted line in prompt input\nfinal line")
      (cl-letf (((symbol-function 'agent-chat-start-turn-commit-window!)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-finish-turn-commits)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-scroll-to-bottom)
                 (lambda (&rest _) nil))
                ((symbol-function 'redisplay)
                 (lambda (&rest _) nil)))
        (agent-chat-send-input
         (lambda (text cb)
           (setq sent-text text)
           (setq callback cb)
           nil)
         "agent")
        (should (equal sent-text
                       "first line\n> quoted line in prompt input\nfinal line"))
        (should callback)))))

(ert-deftest agent-chat-ensure-prompt-markers-repairs-drifting-input-start ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((expected-input-start (marker-position agent-chat--input-start)))
      (insert "pending prompt text")
      (setq agent-chat--input-start (copy-marker (point-max) t))
      (agent-chat--ensure-prompt-markers!)
      (should (= expected-input-start
                 (marker-position agent-chat--input-start)))
      (should-not (marker-insertion-type agent-chat--input-start))
      (should (equal "pending prompt text"
                     (buffer-substring-no-properties
                      (marker-position agent-chat--input-start)
                      (point-max)))))))

;;; agent-chat-test.el ends here
