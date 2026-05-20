;;; codex-repl-identity-test.el --- Identity tests for codex-repl -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat)
(require 'codex-repl)

(defmacro codex-repl-test--without-side-effects (&rest body)
  `(cl-letf (((symbol-function 'codex-repl--refresh-session-header) (lambda (&rest _) nil))
             ((symbol-function 'codex-repl-refresh-header-line) (lambda (&rest _) nil))
             ((symbol-function 'codex-repl--store-upsert-session) (lambda (&rest _) nil))
             ((symbol-function 'codex-repl--emit-session-start-evidence!) (lambda (&rest _) nil))
             ((symbol-function 'codex-repl--report-registry-invoke-state!) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest codex-repl-session-id-is-buffer-local-per-lane ()
  (let ((buf-a (generate-new-buffer " *codex-lane-a*"))
        (buf-b (generate-new-buffer " *codex-lane-b*")))
    (unwind-protect
        (codex-repl-test--without-side-effects
         (with-current-buffer buf-a
           (codex-repl-mode)
           (setq-local codex-repl-session-file nil)
           (setq-local agent-chat--pending-process nil)
           (codex-repl--persist-session-id! "sid-a"))
         (with-current-buffer buf-b
           (codex-repl-mode)
           (setq-local codex-repl-session-file nil)
           (setq-local agent-chat--pending-process nil)
           (codex-repl--persist-session-id! "sid-b"))
         (with-current-buffer buf-a
           (should (local-variable-p 'codex-repl-session-id))
           (should (equal codex-repl-session-id "sid-a"))
           (should (equal agent-chat--session-id "sid-a")))
         (with-current-buffer buf-b
           (should (local-variable-p 'codex-repl-session-id))
           (should (equal codex-repl-session-id "sid-b"))
           (should (equal agent-chat--session-id "sid-b"))))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(ert-deftest codex-repl-rejects-session-id-collision-across-live-lanes ()
  (let ((buf-a (generate-new-buffer " *codex-collision-a*"))
        (buf-b (generate-new-buffer " *codex-collision-b*")))
    (unwind-protect
        (codex-repl-test--without-side-effects
         (with-current-buffer buf-a
           (codex-repl-mode)
           (setq-local codex-repl-session-file nil)
           (setq-local agent-chat--pending-process nil)
           (setq-local codex-repl-agency-agent-id "codex-a")
           (codex-repl--persist-session-id! "sid-shared"))
         (with-current-buffer buf-b
           (codex-repl-mode)
           (setq-local codex-repl-session-file nil)
           (setq-local agent-chat--pending-process nil)
           (setq-local codex-repl-agency-agent-id "codex-b")
           (should-error (codex-repl--persist-session-id! "sid-shared")))
         (with-current-buffer buf-b
           (should-not codex-repl-session-id)
           (should-not agent-chat--session-id)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(ert-deftest codex-repl-clear-session-state-clears-agent-chat-session-too ()
  (with-temp-buffer
    (codex-repl-mode)
    (setq-local codex-repl-session-file nil)
    (setq-local codex-repl-session-id "sid-x")
    (setq-local agent-chat--session-id "sid-x")
    (setq-local codex-repl--evidence-session-id "sid-x")
    (setq-local codex-repl--last-evidence-id "e-x")
    (setq-local codex-repl--last-emitted-session-id "sid-x")
    (codex-repl-test--without-side-effects
     (codex-repl--clear-session-state!))
    (should-not codex-repl-session-id)
    (should-not agent-chat--session-id)
    (should-not codex-repl--evidence-session-id)
    (should-not codex-repl--last-evidence-id)
    (should-not codex-repl--last-emitted-session-id)))

(ert-deftest codex-repl-backfills-first-user-turn-when-session-is-minted ()
  (with-temp-buffer
    (codex-repl-mode)
    (setq-local codex-repl-session-file nil)
    (setq-local codex-repl-evidence-url "http://example.test/api/alpha/evidence")
    (setq-local codex-repl-evidence-log-turns t)
    (let (payloads)
      (cl-letf (((symbol-function 'codex-repl--refresh-session-header) (lambda (&rest _) nil))
                ((symbol-function 'codex-repl-refresh-header-line) (lambda (&rest _) nil))
                ((symbol-function 'codex-repl--store-upsert-session) (lambda (&rest _) nil))
                ((symbol-function 'codex-repl--report-registry-invoke-state!) (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-evidence-enabled-p) (lambda (_url) t))
                ((symbol-function 'agent-chat-evidence-fetch-latest-id)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-evidence-post-entry-id)
                 (lambda (_url _timeout payload)
                   (push payload payloads)
                   (format "e-%d" (length payloads)))))
        (codex-repl--emit-user-turn-evidence! "hello from first turn")
        (should (equal agent-chat--pending-user-turn-text
                       "hello from first turn"))
        (codex-repl--persist-session-id! "sid-1")
        (should-not agent-chat--pending-user-turn-text)
        (should (= 2 (length payloads)))
        (let ((user-turn (seq-find (lambda (payload)
                                     (equal "chat-turn"
                                            (alist-get 'event
                                                       (alist-get 'body payload))))
                                   payloads)))
          (should user-turn)
          (should (equal "sid-1" (alist-get 'session-id user-turn)))
          (should (equal "user" (alist-get 'role (alist-get 'body user-turn))))
          (should (equal "hello from first turn"
                         (alist-get 'text (alist-get 'body user-turn)))))))))
