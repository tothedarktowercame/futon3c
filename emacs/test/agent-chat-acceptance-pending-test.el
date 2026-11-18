;;; agent-chat-acceptance-pending-test.el --- yes waits for queued evidence -*- lexical-binding: t; -*-

;; 2026-11-18 (Joe, claude-5 buffer, turn-1bWqsV): "🈸: yes" reported
;; "✗ yes not checked: no evidence id" although its evidence row was only
;; queued and landed seconds later.

(require 'ert)
(require 'agent-chat)

(defmacro agent-chat-acceptance-test--with-buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local agent-chat--agent-id "seat-fixture"
                 agent-chat--session-id "session-fixture"
                 agent-chat--acceptances-awaiting-evidence nil)
     (let ((notes nil) (posts nil))
       (cl-letf (((symbol-function 'agent-chat--insert-note)
                  (lambda (text) (push text notes)))
                 ((symbol-function 'agent-chat--agreement-post-async)
                  (lambda (_url payload _cb) (push payload posts))))
         ,@body))))

(ert-deftest agent-chat-acceptance-waits-for-queued-evidence ()
  (agent-chat-acceptance-test--with-buffer
    (agent-chat--check-acceptance "yes" nil "emacs-pending-1")
    (should (null posts))
    (should (equal '("· yes will be checked when its evidence lands") notes))
    (agent-chat-evidence--finish-replay
     "/nonexistent/outbox-record.json"
     '((payload . ((evidence-id . "emacs-pending-1")))) "/nonexistent/response"
     nil '(:status 201 :json nil))
    (should (equal "emacs-pending-1" (alist-get 'evidence-id (car posts))))
    (should (null agent-chat--acceptances-awaiting-evidence))))

(ert-deftest agent-chat-acceptance-reports-rejected-evidence ()
  (agent-chat-acceptance-test--with-buffer
    (agent-chat--check-acceptance "yes" nil "emacs-pending-2")
    (agent-chat--resume-acceptance "emacs-pending-2" 'failed)
    (should (null posts))
    (should (equal "✗ yes not checked: its evidence was rejected" (car notes)))))

(ert-deftest agent-chat-acceptance-with-no-id-at-all-still-says-so ()
  (agent-chat-acceptance-test--with-buffer
    (agent-chat--check-acceptance "yes" nil nil)
    (should (null posts))
    (should (equal '("✗ yes not checked: no evidence id") notes))))

(ert-deftest agent-chat-acceptance-acked-id-posts-at-once ()
  (agent-chat-acceptance-test--with-buffer
    (agent-chat--check-acceptance "yes" "emacs-acked" "emacs-acked")
    (should (equal "emacs-acked" (alist-get 'evidence-id (car posts))))))
