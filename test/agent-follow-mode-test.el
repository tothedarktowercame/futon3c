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
