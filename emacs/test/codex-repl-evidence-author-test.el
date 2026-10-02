;;; codex-repl-evidence-author-test.el --- seat id as evidence author -*- lexical-binding: t; -*-

;; The agreement route (futon3c/transport/http.clj reply-offer!) requires
;; (= agent (:evidence/author reply)), so a codex seat's turn evidence must
;; carry the seat id (e.g. "codex-10"), not the literal "codex".

(require 'ert)
(require 'agent-chat)
(require 'codex-repl)

(defmacro codex-repl-evidence-author-test--capture (&rest body)
  "Run BODY with the evidence POST stubbed; return the captured payload."
  `(let (captured)
     (cl-letf (((symbol-function 'agent-chat-sync-evidence-anchor!)
                (lambda (&rest _) nil))
               ((symbol-function 'agent-chat-evidence-post-entry-id)
                (lambda (_url _timeout payload)
                  (setq captured payload)
                  "e-test-id"))
               ((symbol-function 'agent-chat-note-turn-recorded)
                (lambda () nil)))
       (let ((codex-repl-evidence-url "http://localhost:7073")
             (codex-repl-evidence-timeout 1)
             (codex-repl-evidence-log-turns t)
             (codex-repl-session-id "sess-1")
             (agent-chat--current-turn-id "turn-1"))
         ,@body))
     captured))

(ert-deftest codex-repl-evidence-author-uses-seat-id ()
  "Assistant turn evidence posts the seat id as author."
  (let* ((codex-repl-agency-agent-id "codex-99")
         (payload (codex-repl-evidence-author-test--capture
                   (codex-repl--emit-assistant-turn-evidence! "hello from codex-99"))))
    (should payload)
    (should (equal "codex-99" (cdr (assoc 'author payload))))))

(ert-deftest codex-repl-evidence-author-falls-back-to-codex ()
  "With no seat id bound, author falls back to \"codex\"."
  (let* ((codex-repl-agency-agent-id nil)
         (payload (codex-repl-evidence-author-test--capture
                   (codex-repl--emit-assistant-turn-evidence! "hello from fallback"))))
    (should payload)
    (should (equal "codex" (cdr (assoc 'author payload))))))

(ert-deftest codex-repl-evidence-author-empty-seat-id-falls-back ()
  "An empty seat id also falls back to \"codex\"."
  (let* ((codex-repl-agency-agent-id "")
         (payload (codex-repl-evidence-author-test--capture
                   (codex-repl--emit-assistant-turn-evidence! "hello from empty"))))
    (should payload)
    (should (equal "codex" (cdr (assoc 'author payload))))))

(provide 'codex-repl-evidence-author-test)
;;; codex-repl-evidence-author-test.el ends here
