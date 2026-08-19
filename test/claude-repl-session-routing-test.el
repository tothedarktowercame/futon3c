;;; claude-repl-session-routing-test.el --- Session routing tests for claude-repl -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-chat)
(require 'claude-repl)

(ert-deftest claude-repl-cost-state-distinguishes-warm-and-cold ()
  (let ((claude-repl-cost-large-context-tokens 200000))
    (let ((warm (claude-repl--cost-state 400000 3599))
          (cold (claude-repl--cost-state 400000 3600)))
      (should (eq (plist-get warm :state) 'warm))
      (should (= (plist-get warm :next-input-cost) 0.2))
      (should (string-match-p "cache is warm" (plist-get warm :advice)))
      (should (eq (plist-get cold :state) 'cold))
      (should (= (plist-get cold :next-input-cost) 4.0))
      (should (string-match-p "durable handoff" (plist-get cold :advice))))))

(ert-deftest claude-repl-usage-context-tokens-sums-billing-fields ()
  (should (= 600
             (claude-repl--usage-context-tokens
              '((input_tokens . 100)
                (cache_read_input_tokens . 200)
                (cache_creation_input_tokens . 300))))))

(ert-deftest claude-repl-latest-usage-record-ignores-non-usage-lines ()
  (let ((path (make-temp-file "claude-repl-cost-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"timestamp\":\"2026-08-19T10:00:00Z\",\"message\":{\"usage\":{\"input_tokens\":123}}}\n")
            (insert "{\"timestamp\":\"2026-08-19T10:01:00Z\",\"type\":\"progress\"}\n"))
          (let ((record (claude-repl--latest-usage-record path)))
            (should (equal (car record) "2026-08-19T10:00:00Z"))
            (should (= (alist-get 'input_tokens (cdr record)) 123))))
      (delete-file path))))

(ert-deftest claude-repl-turn-identity-is-exact ()
  (should (claude-repl--turn-event-matches-p
           "turn-1" '((type . "done") (turn-id . "turn-1"))))
  (should-not (claude-repl--turn-event-matches-p
               "turn-1" '((type . "done") (turn-id . "turn-0"))))
  (should-not (claude-repl--turn-event-matches-p
               "turn-1" '((type . "done")))))

(ert-deftest claude-repl-turn-mismatch-is-visible ()
  (let ((marker (claude-repl--turn-mismatch-marker
                 "turn-current" '((turn-id . "turn-earlier")))))
    (should (string-match-p "reply for an earlier turn" marker))
    (should (string-match-p "turn-current" marker))
    (should (string-match-p "turn-earlier" marker))))

(ert-deftest claude-repl-turn-identity-negotiates-rolling-deployment ()
  (should (eq 'required
              (claude-repl--turn-identity-mode
               '((type . "started") (turn-id . "turn-new")))))
  (should (eq 'legacy
              (claude-repl--turn-identity-mode '((type . "started")))))
  ;; A strict stream still rejects missing or foreign ids; only a stream whose
  ;; first event advertised no capability takes the compatibility path.
  (should-not (claude-repl--turn-event-matches-p
               "turn-new" '((type . "done")))))

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
