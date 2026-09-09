;;; claude-repl-tool-stream-test.el --- Tool-first transcript regression -*- lexical-binding: t; -*-
(require 'ert)
(require 'claude-repl)

(ert-deftest claude-repl-tool-first-stream-keeps-commands-and-prose ()
  ;; Exercise the actual NDJSON filter, including fragmented network input.
  (dolist (with-details '(t nil))
    (with-temp-buffer
      (setq-local agent-chat--prompt-marker (copy-marker (point) t))
      (insert "> ")
      (set-marker agent-chat--prompt-marker (point-min))
      (let (filter output-buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest args)
                         (setq filter (plist-get args :filter)
                               output-buffer (plist-get args :buffer))
                         'test-process))
                      ((symbol-function 'process-buffer) (lambda (_) output-buffer))
                      ((symbol-function 'claude-repl--new-turn-id) (lambda () "test-turn"))
                      ((symbol-function 'agent-chat-scroll-to-bottom) #'ignore)
                      ((symbol-function 'claude-repl--frame-add-event) #'ignore)
                      ((symbol-function 'claude-repl--frame-append-text) #'ignore))
              (claude-repl--call-claude-streaming "resume" #'ignore)
              (funcall filter 'test-process
                       "{\"type\":\"started\",\"turn-id\":\"test-turn\"}\n")
              (let ((event (concat
                            "{\"type\":\"tool_use\",\"turn-id\":\"test-turn\",\"tools\":[\"Bash\"]"
                            (when with-details
                              ",\"tool_details\":[{\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"pwd\"}}]")
                            "}\n")))
                (funcall filter 'test-process (substring event 0 20))
                (should-not agent-chat--streaming-started)
                (funcall filter 'test-process (substring event 20)))
              (should agent-chat--streaming-started)
              (should (string-match-p (regexp-quote (if with-details "[Bash] pwd" "[Bash]"))
                                      (buffer-string)))
              (when with-details
                (should (equal (alist-get 'name (cdr (assoc "tool-1" claude-repl--pending-tool-uses)))
                               "Bash")))
              (should (seq-some (lambda (ov) (overlay-get ov 'agent-chat-tool-details))
                                (overlays-in (point-min) (point-max))))
              (funcall filter 'test-process
                       "{\"type\":\"text\",\"turn-id\":\"test-turn\",\"text\":\"Found the cause.\"}\n")
              (should (string-match-p "\\[Bash\\][^\n]*\nFound the cause\\."
                                      (buffer-string)))
              (should (= 1 (how-many "claude: " (point-min) (point-max)))))
          (when (buffer-live-p output-buffer) (kill-buffer output-buffer)))))))
