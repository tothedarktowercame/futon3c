;;; claude-repl-session-routing-test.el --- Session routing tests for claude-repl -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-chat)
(require 'claude-repl)

(ert-deftest claude-repl-finds-buffer-by-session-id ()
  (let ((buf-a (generate-new-buffer " *claude-session-a*"))
        (buf-b (generate-new-buffer " *claude-session-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (claude-repl-mode)
            (setq-local agent-chat--session-id "sid-a")
            (setq-local claude-repl-agent-id "claude-a"))
          (with-current-buffer buf-b
            (claude-repl-mode)
            (setq-local agent-chat--session-id "sid-b")
            (setq-local claude-repl-agent-id "claude-b"))
          (should (eq (claude-repl-find-buffer-by-session-id "sid-a") buf-a))
          (should (eq (claude-repl-find-buffer-by-session-id "sid-b") buf-b))
          (should-not (claude-repl-find-buffer-by-session-id "sid-missing")))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(ert-deftest claude-repl-finds-buffer-by-agent-id ()
  (let ((buf-a (generate-new-buffer " *claude-agent-a*"))
        (buf-b (generate-new-buffer " *claude-agent-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (claude-repl-mode)
            (setq-local agent-chat--session-id "sid-a")
            (setq-local claude-repl-agent-id "claude-a"))
          (with-current-buffer buf-b
            (claude-repl-mode)
            (setq-local agent-chat--session-id "sid-b")
            (setq-local claude-repl-agent-id "claude-b"))
          (should (eq (claude-repl-find-buffer-by-agent-id "claude-a") buf-a))
          (should (eq (claude-repl-find-buffer-by-agent-id "claude-b") buf-b))
          (should-not (claude-repl-find-buffer-by-agent-id "claude-missing")))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(ert-deftest claude-repl-stale-duplicate-buffer-is-rejected-before-send ()
  (let ((owner (generate-new-buffer " *claude-owner*"))
        (stale (generate-new-buffer " *claude-stale*")))
    (unwind-protect
        (progn
          (with-current-buffer owner
            (claude-repl-mode)
            (setq-local agent-chat--session-id "sid-shared")
            (setq-local claude-repl-agent-id "claude-2"))
          (with-current-buffer stale
            (claude-repl-mode)
            (setq-local agent-chat--session-id "sid-shared")
            (setq-local claude-repl-agent-id "claude-6")
            (cl-letf (((symbol-function 'claude-repl--live-agents-response)
                       (lambda ()
                         '((agents . (("claude-2" . ((session-id . "sid-shared")))
                                      ("claude-6" . ((session-id . "sid-shared")))))))))
              (should-error (claude-repl--assert-session-owned-by-current-agent)
                            :type 'user-error))))
      (when (buffer-live-p owner) (kill-buffer owner))
      (when (buffer-live-p stale) (kill-buffer stale)))))

(ert-deftest claude-repl-backfills-first-user-turn-on-session-start ()
  (with-temp-buffer
    (claude-repl-mode)
    (setq-local claude-repl-evidence-url "http://example.test/api/alpha/evidence")
    (setq-local claude-repl-evidence-log-turns t)
    (let (payloads)
      (cl-letf (((symbol-function 'agent-chat-evidence-enabled-p)
                 (lambda (_url) t))
                ((symbol-function 'agent-chat-evidence-fetch-latest-id)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-evidence-post-entry-id)
                 (lambda (_url _timeout payload)
                   (push payload payloads)
                   (format "e-%d" (length payloads)))))
        (claude-repl--emit-user-turn-evidence! "hello from first turn")
        (should (equal agent-chat--pending-user-turn-text
                       "hello from first turn"))
        (setq-local agent-chat--session-id "sid-1")
        (claude-repl--emit-session-start-evidence! "sid-1")
        (should-not agent-chat--pending-user-turn-text)
        (should (= 2 (length payloads)))
        (let ((session-start (seq-find (lambda (payload)
                                         (equal "session-start"
                                                (alist-get 'event
                                                           (alist-get 'body payload))))
                                       payloads))
              (user-turn (seq-find (lambda (payload)
                                     (equal "chat-turn"
                                            (alist-get 'event
                                                       (alist-get 'body payload))))
                                   payloads)))
          (should session-start)
          (should user-turn)
          (should (equal "sid-1" (alist-get 'session-id user-turn)))
          (should (equal "user" (alist-get 'role (alist-get 'body user-turn))))
          (should (equal "hello from first turn"
                         (alist-get 'text (alist-get 'body user-turn)))))))))
