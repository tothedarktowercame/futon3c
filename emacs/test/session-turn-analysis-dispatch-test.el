;;; session-turn-analysis-dispatch-test.el --- ERT tests for reply-end dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Fixture-only tests for the M-象-2000 change: 象 is dispatched when the
;; agent's REPLY arrives, with a "What the agent did" happened_summary.
;; Run:
;;   emacs -Q --batch -L emacs -l emacs/test/session-turn-analysis-dispatch-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'session-turn-analysis)

(defvar session-mode-turn-tags-mode)
(defvar agent-chat-user-speaker)
(defvar agent-chat--agent-id)

(defun session-turn-analysis-dispatch-test--run-turn (&optional summary-fn)
  "Run one fake operator turn through `session-mode--analyze-start-turn'.
Returns the list of events in order.  SUMMARY-FN replaces
`session-mode--turn-happened-summary' when given."
  (let ((events nil))
    (cl-letf ((session-mode-turn-tags-mode t)
              (session-mode-analysis-agent "象")
              (agent-chat--agent-id "claude-17")
              (agent-chat-user-speaker "joe")
              ((symbol-function 'session-mode--split-failure-marker)
               (lambda (text) (cons text nil)))
              ((symbol-function 'agent-chat--walkie-command-p)
               (lambda (&rest _) nil))
              ((symbol-function 'session-mode--record-turn)
               (lambda (&rest _) "/tmp/fake-turn.json"))
              ((symbol-function 'session-mode--turn-happened-summary)
               (or summary-fn
                   (lambda (response)
                     (push (list 'summary response) events)
                     "SUMMARY")))
              ((symbol-function 'session-mode--record-add-field)
               (lambda (_path key value)
                 (push (list 'record-update key value) events)))
              ((symbol-function 'session-mode--dispatch-analysis)
               (lambda (&rest _) (push 'dispatch events)))
              ((symbol-function 'session-mode--display-analysis)
               (lambda (&rest _) nil)))
      (let ((original
             (lambda (call &rest _args)
               (funcall call "PROMPT"
                        (lambda (response)
                          (push (list 'outer-callback response) events))))))
        (session-mode--analyze-start-turn
         original
         (lambda (_sent callback) (funcall callback "REPLY line1\nREPLY line2"))
         "claude" nil "do the thing" "joe" 'operator)))
    (nreverse events)))

(ert-deftest session-turn-analysis-test-dispatch-after-reply ()
  "The dispatch happens after the reply arrives, with the summary stored first."
  (let ((events (session-turn-analysis-dispatch-test--run-turn)))
    (should (equal events
                   '((summary "REPLY line1\nREPLY line2")
                     (record-update happened_summary "SUMMARY")
                     dispatch
                     (outer-callback "REPLY line1\nREPLY line2"))))))

(ert-deftest session-turn-analysis-test-summary-error-still-dispatches ()
  "A failure building the summary warns and still dispatches the turn."
  (let ((events (session-turn-analysis-dispatch-test--run-turn
                 (lambda (_response) (error "boom")))))
    (should (memq 'dispatch events))
    (should-not (assq 'record-update events))
    (should (assq 'outer-callback events))))

(ert-deftest session-turn-analysis-test-numstat-line ()
  "Commit lines carry repo, short sha, subject and +a -r over N files — no diff."
  (cl-letf (((symbol-function 'session-mode--git-numstat)
             (lambda (_repo _sha)
               "3\t1\tfoo.el\n-\t-\tbin.png\n2\t0\tbar.py\n")))
    (let ((line (session-mode--commit-summary-line
                 '((repo . "futon3c") (repo-path . "/x/futon3c")
                   (sha . "abc123def456") (subject . "fix a thing")))))
      (should (string-match-p "futon3c abc123de fix a thing" line))
      (should (string-match-p "(\\+5 -1 over 3 files)" line))
      (should-not (string-match-p "diff --git\\|@@\\|^+++" line)))))

(ert-deftest session-turn-analysis-test-happened-summary-shape ()
  "The summary has reply lines, a numstat commit line, and no diff text."
  (cl-letf (((symbol-function 'session-mode--turn-commits-snapshot)
             (lambda ()
               (list '((repo . "futon3c") (repo-path . "/x/futon3c")
                       (sha . "abc123def456") (subject . "fix a thing")))))
            ((symbol-function 'session-mode--git-numstat)
             (lambda (_repo _sha) "10\t2\temacs/x.el\n")))
    (let ((summary (session-mode--turn-happened-summary
                    "line one\nline two\nline three\nline four\nline five\nline six")))
      (should (string-match-p "line five" summary))
      (should-not (string-match-p "line six" summary))
      (should (string-match-p "futon3c abc123de fix a thing (\\+10 -2 over 1 files)" summary))
      (should-not (string-match-p "diff --git\\|@@" summary)))))

(ert-deftest session-turn-analysis-test-commit-cap ()
  "More than 20 commits ends with …and K more."
  (cl-letf (((symbol-function 'session-mode--turn-commits-snapshot)
             (lambda ()
               (cl-loop for i from 1 to 23
                        collect `((repo . "r") (repo-path . "/x/r")
                                  (sha . ,(format "%040d" i))
                                  (subject . ,(format "c%d" i))))))
            ((symbol-function 'session-mode--git-numstat)
             (lambda (&rest _) "1\t0\tf\n")))
    (let ((summary (session-mode--turn-happened-summary "reply")))
      (should (string-match-p "…and 3 more" summary))
      (should (string-match-p "c20" summary))
      (should-not (string-match-p "c21" summary)))))

(ert-deftest session-turn-analysis-test-dispatch-error-keeps-the-reply ()
  "Planted in review (claude-17): a failing dispatch must not swallow the reply.
The dispatch now runs inside the reply callback, so an error there would
stop the agent's reply from reaching the REPL."
  (let ((reached nil))
    (cl-letf ((session-mode-turn-tags-mode t)
              (session-mode-analysis-agent "象")
              (agent-chat--agent-id "claude-17")
              (agent-chat-user-speaker "joe")
              ((symbol-function 'session-mode--split-failure-marker) (lambda (text) (cons text nil)))
              ((symbol-function 'agent-chat--walkie-command-p) (lambda (&rest _) nil))
              ((symbol-function 'session-mode--record-turn) (lambda (&rest _) "/tmp/fake-turn.json"))
              ((symbol-function 'session-mode--turn-happened-summary) (lambda (_) nil))
              ((symbol-function 'session-mode--dispatch-analysis) (lambda (&rest _) (error "no python")))
              ((symbol-function 'session-mode--display-analysis) (lambda (&rest _) nil))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (session-mode--analyze-start-turn
       (lambda (call &rest _) (funcall call "PROMPT" (lambda (r) (setq reached r))))
       (lambda (_sent callback) (funcall callback "REPLY"))
       "claude" nil "do the thing" "joe" 'operator))
    (should (equal reached "REPLY"))))

(ert-deftest session-turn-analysis-test-streamed-reply-dispatches-at-turn-end ()
  "Planted in review (claude-17): a streamed reply never calls the callback.
claude-repl finishes a streamed turn itself: segment evidence, then the
turn-commits emit (which clears the heads), then `agent-chat-finish-turn!'.
The turn must still reach 象 exactly once, with the reply and the commits."
  (let ((dispatched nil) (stored nil))
    (with-temp-buffer
      (cl-letf ((session-mode-turn-tags-mode t)
                (session-mode-analysis-agent "象")
                (agent-chat--agent-id "claude-17")
                (agent-chat-user-speaker "joe")
                (agent-chat--turn-git-heads nil)
                ((symbol-function 'session-mode--split-failure-marker) (lambda (text) (cons text nil)))
                ((symbol-function 'agent-chat--walkie-command-p) (lambda (&rest _) nil))
                ((symbol-function 'session-mode--record-turn) (lambda (&rest _) "/tmp/fake-turn.json"))
                ((symbol-function 'session-mode--record-add-field)
                 (lambda (_p _k v) (setq stored v)))
                ((symbol-function 'session-mode--git-numstat) (lambda (&rest _) "1\t0\tf.el\n"))
                ((symbol-function 'session-mode--dispatch-analysis)
                 (lambda (path &rest _) (push path dispatched)))
                ((symbol-function 'session-mode--display-analysis) (lambda (&rest _) nil)))
        ;; The call streams: it never invokes its callback.
        (session-mode--analyze-start-turn
         (lambda (call &rest _) (funcall call "PROMPT" (lambda (_r) (error "not reached"))))
         (lambda (_sent _callback) nil)
         "claude" nil "do the thing" "joe" 'operator)
        (should-not dispatched)
        (session-mode--note-reply-segment "Gist: streamed reply" t)
        (session-mode--note-turn-commits
         (lambda () (list '((repo . "futon3c") (sha . "abcdef1234") (subject . "fix a thing")))))
        (session-mode--on-turn-finished nil nil)
        (session-mode--on-turn-finished nil nil)))
    (should (equal dispatched '("/tmp/fake-turn.json")))
    (should (string-match-p "Gist: streamed reply" stored))
    (should (string-match-p "futon3c abcdef12" stored))))

(provide 'session-turn-analysis-dispatch-test)
;;; session-turn-analysis-dispatch-test.el ends here

(defun session-turn-analysis-test--reap-with (outputs)
  "Run the reaper against OUTPUTS, one per reap, with timers run at once.
Returns (DELAYS . LANDED): the delays asked for and the paths landed."
  (let* ((delays nil) (landed nil) (outs outputs)
        (real-make-process (symbol-function 'make-process))
        (session-mode-analysis-landed-functions
         (list (lambda (p) (push p landed)))))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (d _r fn &rest args) (push d delays) (apply fn args)))
              ((symbol-function 'session-mode--handle-reap-output) #'ignore)
              ((symbol-function 'session-mode--set-analysis-health) #'ignore)
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (let ((p (funcall real-make-process :name "reap-stub"
                                   :buffer (plist-get plist :buffer)
                                   :command (list "printf" "%s" (or (pop outs) "running")))))
                   (while (process-live-p p) (accept-process-output p 0.05))
                   (funcall (plist-get plist :sentinel) p "finished\n")
                   p))))
      (session-mode--reap-dispatch "/tmp/turn-x.json" nil))
    (cons (nreverse delays) landed)))

(ert-deftest session-turn-analysis-reap-keeps-asking-after-nine-minutes ()
  "claude-17, 2026-09-30: with 象 backed up, readings landed after the third
reap and ran no hook.  A queued job is asked about again, three times at the
short interval and then at the long one, and a late landing runs the hook."
  (let* ((session-mode-analysis-reap-after 180)
         (session-mode-analysis-reap-late-after 600)
         (session-mode-analysis-reap-late-tries 6)
         (r (session-turn-analysis-test--reap-with
             '("queued" "running" "running" "running" "analyzed"))))
    (should (equal '(180 180 600 600) (car r)))
    (should (equal '("/tmp/turn-x.json") (cdr r)))))

(ert-deftest session-turn-analysis-reap-stops-after-an-hour ()
  (let* ((session-mode-analysis-reap-after 180)
         (session-mode-analysis-reap-late-after 600)
         (session-mode-analysis-reap-late-tries 6)
         (r (session-turn-analysis-test--reap-with (make-list 20 "running"))))
    (should (equal '(180 180 600 600 600 600 600 600) (car r)))
    (should-not (cdr r))))
