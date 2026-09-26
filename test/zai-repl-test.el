;;; zai-repl-test.el --- Tests for zai-repl -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'zai-repl)

(ert-deftest zai-repl-read-attach-agent-id-completes-over-roster ()
  (let (completion-args)
    (cl-letf (((symbol-function 'zai-repl--roster-zai-ids)
               (lambda () '("zai-3" "zai-8")))
              ((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq completion-args args)
                 "zai-8")))
      (should (equal (zai-repl--read-attach-agent-id) "zai-8"))
      (should (equal (nth 1 completion-args) '("zai-3" "zai-8")))
      (should (eq (nth 3 completion-args) t))
      (should (equal (nth 6 completion-args) "zai-3")))))

(ert-deftest zai-repl-attach-agent-opens-agency-instance ()
  (let (open-args)
    (cl-letf (((symbol-function 'zai-repl--roster-zai-ids)
               (lambda () '("zai-8")))
              ((symbol-function 'zai-repl--open-instance)
               (lambda (&rest args)
                 (setq open-args args)
                 'attached-buffer)))
      (should (eq (zai-repl-attach-agent " zai-8 ") 'attached-buffer))
      (should (equal open-args
                     '("*zai-repl:zai-8*"
                       "zai-8"
                       "/tmp/futon-zai-session-id-zai-8"))))))

(ert-deftest zai-repl-call-always-uses-agency ()
  (let (agency-call)
    (cl-letf (((symbol-function 'zai-repl--call-agency-streaming)
               (lambda (&rest args) (setq agency-call args) 'agency-process))
              ((symbol-function 'zai-repl--call-direct)
               (lambda (&rest _) (ert-fail "direct ZAI path was invoked"))))
      (let ((zai-repl-use-agency nil))
        (should (eq (zai-repl--call "prove" #'ignore) 'agency-process))
        (should (equal (car agency-call) "prove"))))))

(ert-deftest zai-repl-auto-register-sends-session ()
  (let (curl-args)
    (cl-letf (((symbol-function 'zai-repl--new-session-id)
               (lambda () "zai-local-test"))
              ((symbol-function 'call-process)
               (lambda (_program _infile destination _display &rest args)
                 (setq curl-args args)
                 (with-current-buffer destination
                   (insert "{\"ok\":true,\"agent-id\":\"zai-9\",\"session-file\":\"/tmp/zai-9\"}"))
                 0)))
      (let ((binding (zai-repl--auto-register)))
        (should (equal (plist-get binding :agent-id) "zai-9"))
        (let* ((data-index (cl-position "-d" curl-args :test #'equal))
               (payload (json-parse-string (nth (1+ data-index) curl-args)
                                           :object-type 'alist)))
          (should (equal (alist-get 'type payload) "zai"))
          (should (equal (alist-get 'session-id payload) "zai-local-test")))))))

(ert-deftest zai-repl-for-agent-remains-a-command-alias ()
  (should (commandp 'zai-repl-for-agent))
  (should (eq (symbol-function 'zai-repl-for-agent)
              'zai-repl-attach-agent)))

(provide 'zai-repl-test)
;;; zai-repl-test.el ends here
