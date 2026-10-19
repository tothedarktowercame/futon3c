;;; agent-follow-mode-test.el --- Tests for agent-follow-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "../emacs/agent-follow-mode.el"
                            (file-name-directory load-file-name)))

(ert-deftest agent-follow-poll-queues-behind-live-process ()
  (with-temp-buffer
    (let ((agent-follow--inflight 'live-process)
          (agent-follow--queued nil))
      (cl-letf (((symbol-function 'processp)
                 (lambda (value) (eq value 'live-process)))
                ((symbol-function 'process-live-p) (lambda (_process) t))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (ert-fail "started a second poll"))))
        (agent-follow--poll (current-buffer))
        (should agent-follow--queued)
        (should (eq agent-follow--inflight 'live-process))))))

(ert-deftest agent-follow-poll-replaces-orphaned-latch ()
  (with-temp-buffer
    (let ((agent-follow--inflight t)
          (agent-follow--queued t)
          (agent-follow--progress-jobs '(stale-job))
          (agent-follow-jobs-limit 10)
          started
          cleared)
      (cl-letf (((symbol-function 'agent-follow--agency-url)
                 (lambda () "http://agency.test"))
                ((symbol-function 'agent-chat-remove-thinking)
                 (lambda () (setq cleared t)))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq started args)
                   'replacement-process)))
        (agent-follow--poll (current-buffer))
        (should started)
        (should cleared)
        (should-not agent-follow--queued)
        (should-not agent-follow--progress-jobs)
        (should (eq agent-follow--inflight 'replacement-process))))))

(ert-deftest agent-follow-poll-clears-slot-when-process-creation-fails ()
  (with-temp-buffer
    (let ((agent-follow--inflight t)
          (agent-follow--queued t)
          (agent-follow-jobs-limit 10)
          outbuf
          (real-generate-new-buffer (symbol-function 'generate-new-buffer)))
      (cl-letf (((symbol-function 'agent-follow--agency-url)
                 (lambda () "http://agency.test"))
                ((symbol-function 'generate-new-buffer)
                 (lambda (_name)
                   (setq outbuf
                         (funcall real-generate-new-buffer
                                  " *agent-follow-test*"))))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (error "process creation failed"))))
        (should-error (agent-follow--poll (current-buffer))
                      :type 'error)
        (should-not agent-follow--inflight)
        (should-not (buffer-live-p outbuf))))))

(ert-deftest agent-follow-late-sentinel-does-not-clear-replacement ()
  (with-temp-buffer
    (let ((target (current-buffer))
          (agent-follow--inflight t)
          (agent-follow--queued nil)
          (agent-follow-jobs-limit 10)
          sentinel
          process-buffer)
      (cl-letf (((symbol-function 'agent-follow--agency-url)
                 (lambda () "http://agency.test"))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq sentinel (plist-get args :sentinel)
                         process-buffer (plist-get args :buffer))
                   'old-process)))
        (agent-follow--poll target))
      (setq agent-follow--inflight 'replacement-process)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_process) nil))
                ((symbol-function 'process-buffer)
                 (lambda (_process) process-buffer)))
        (funcall sentinel 'old-process "finished\n"))
      (should (eq agent-follow--inflight 'replacement-process)))))

(provide 'agent-follow-mode-test)
;;; agent-follow-mode-test.el ends here

(ert-deftest agent-follow-jobs-url-is-per-agent ()
  (with-temp-buffer
    (let ((agent-follow--agent-id "codex-68")
          (agent-follow-jobs-limit 10))
      (cl-letf (((symbol-function 'agent-follow--agency-url)
                 (lambda () "http://agency.test")))
        (should (equal (agent-follow--jobs-url)
                       "http://agency.test/api/alpha/invoke/jobs?limit=10&agent=codex-68"))
        (should (equal (agent-follow--jobs-url "象-sonnet")
                       (concat "http://agency.test/api/alpha/invoke/jobs?limit=10&agent="
                               (url-hexify-string "象-sonnet"))))))))

(ert-deftest agent-follow-pin-survives-silence-while-job-open ()
  "A queued job that emits nothing for longer than the grace keeps its pin;
a failed one that went quiet does not."
  (with-temp-buffer
    (let ((agent-follow--agent-id "codex-68")
          (agent-follow--seen (make-hash-table :test 'equal))
          (agent-follow--live-jobs (make-hash-table :test 'equal))
          (agent-follow--progress-jobs nil)
          (agent-follow-include-own t)
          (stale (- (float-time) 2000)))
      (puthash "job-queued" 1 agent-follow--seen)
      (puthash "job-failed" 1 agent-follow--seen)
      (puthash "job-queued" stale agent-follow--live-jobs)
      (puthash "job-failed" stale agent-follow--live-jobs)
      (cl-letf (((symbol-function 'agent-follow--insert-message)
                 (lambda (&rest _) (ert-fail "nothing new to render")))
                ((symbol-function 'agent-chat-update-progress) (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-remove-thinking) (lambda (&rest _) nil)))
        (agent-follow--render-jobs
         (list '((job-id . "job-queued") (agent-id . "codex-68") (caller . "wm-click-loop")
                 (state . "queued") (events . (((seq . 1) (type . "accepted")))))
               '((job-id . "job-failed") (agent-id . "codex-68") (caller . "wm-click-loop")
                 (state . "failed") (events . (((seq . 1) (type . "accepted"))))))))
      (should (> (gethash "job-queued" agent-follow--live-jobs) stale))
      (should (= (gethash "job-failed" agent-follow--live-jobs) stale)))))

(ert-deftest agent-follow-history-pending-marks-finished-jobs-without-rendering ()
  "Until the enable-time history mark has answered, a finished job is recorded
as seen and not inserted; a running job still renders."
  (with-temp-buffer
    (let ((agent-follow--agent-id "codex-1")
          (agent-follow--seen (make-hash-table :test 'equal))
          (agent-follow--live-jobs (make-hash-table :test 'equal))
          (agent-follow--progress-jobs nil)
          (agent-follow--history-pending t)
          (agent-follow-include-own t)
          inserted)
      (cl-letf (((symbol-function 'agent-follow--insert-message)
                 (lambda (name _lines) (push name inserted)))
                ((symbol-function 'agent-chat-update-progress) (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-remove-thinking) (lambda (&rest _) nil)))
        (agent-follow--render-jobs
         (list '((job-id . "old-bell") (agent-id . "codex-1") (caller . "claude-5")
                 (state . "done")
                 (events . (((seq . 1) (type . "text") (text . "hello"))
                            ((seq . 2) (type . "done")))))
               '((job-id . "live-bell") (agent-id . "codex-1") (caller . "claude-4")
                 (state . "running")
                 (events . (((seq . 1) (type . "text") (text . "working"))))))))
      (should (equal '("codex-1⇐claude-4") inserted))
      (should (= 2 (gethash "old-bell" agent-follow--seen)))
      ;; once history is marked, the same finished job is simply already seen
      (setq agent-follow--history-pending nil)
      (setq inserted nil)
      (cl-letf (((symbol-function 'agent-follow--insert-message)
                 (lambda (name _lines) (push name inserted)))
                ((symbol-function 'agent-chat-update-progress) (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-remove-thinking) (lambda (&rest _) nil)))
        (agent-follow--render-jobs
         (list '((job-id . "old-bell") (agent-id . "codex-1") (caller . "claude-5")
                 (state . "done")
                 (events . (((seq . 1) (type . "text") (text . "hello"))
                            ((seq . 2) (type . "done"))))))))
      (should (null inserted)))))
