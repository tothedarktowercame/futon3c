;;; agent-repl-park-batch-test.el --- Batch ready-inbox tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-repl-park)

(defvar url-http-response-status)

(agent-repl-park-disable)

(defun agent-repl-park-batch-test--buffers ()
  (cl-loop for n from 1 to 3
           collect
           (let ((buf (generate-new-buffer (format " *batch-seat-%d*" n))))
             (with-current-buffer buf
               (setq major-mode 'claude-repl-mode
                     agent-chat--agent-id (format "agent-%d" n)
                     agent-chat--session-id (format "session-%d" n)
                     claude-repl-api-url "http://agency.test"))
             buf)))

(defun agent-repl-park-batch-test--response (callback code body)
  (let ((buf (generate-new-buffer " *batch-http-response*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %d Test\r\nContent-Type: application/json\r\n\r\n%s"
                      code body))
      ;; url.el reports a 4xx/5xx reply as an error status, with the body
      ;; still in the buffer (checked against a live 404 on 2026-10-29).
      (let ((url-http-response-status code))
        (funcall callback (when (>= code 400)
                            (list :error (list 'error 'http code))))))))

(defmacro agent-repl-park-batch-test--with-buffers (buffers &rest body)
  (declare (indent 1))
  `(let* ((,buffers (agent-repl-park-batch-test--buffers))
          (agent-repl-park--batch-inflight nil)
          (agent-repl-park--batch-disabled-until (make-hash-table :test #'equal))
          (agent-repl-park--seen-ids (make-ring 32)))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl-park--eligible-modes)
                    (lambda () '(claude-repl-mode)))
                   ((symbol-function 'agent-repl-drivable-p) (lambda (_mode) t))
                   ((symbol-function 'agent-repl-park--reap-dead-url-processes)
                    #'ignore)
                   ((symbol-function 'buffer-list) (lambda () ,buffers)))
           ,@body)
       (dolist (buf ,buffers)
         (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-park-batch-one-request-for-three-seats ()
  (agent-repl-park-batch-test--with-buffers buffers
    (let (calls request-data)
      (cl-letf (((symbol-function 'agent-repl-park--retrieve)
                 (lambda (url callback)
                   (push url calls)
                   (setq request-data url-request-data)
                   (agent-repl-park-batch-test--response
                    callback 200 "{\"ok\":true,\"seats\":[]}"))))
        (agent-repl-park--poll-once))
      (should (= 1 (length calls)))
      (should (equal "http://agency.test/api/alpha/ready/batch" (car calls)))
      (let ((seats (plist-get
                    (json-parse-string request-data :object-type 'plist)
                    :seats)))
        (should (= 3 (length seats)))
        (should (equal '("agent-1" "agent-2" "agent-3")
                       (mapcar (lambda (seat) (plist-get seat :agent))
                               (append seats nil))))))))

(ert-deftest agent-repl-park-batch-delivers-to-matching-buffers ()
  (agent-repl-park-batch-test--with-buffers buffers
    (let (parked followups acks)
      (cl-letf (((symbol-function 'agent-repl-park--retrieve)
                 (lambda (_url callback)
                   (agent-repl-park-batch-test--response
                    callback 200
                    (concat
                     "{\"ok\":true,\"seats\":["
                     "{\"agent\":\"agent-1\",\"session\":\"session-1\","
                     "\"parked\":{\"ok\":true,\"ready\":[]},"
                     "\"followups\":{\"ok\":true,\"ready\":[]}},"
                     "{\"agent\":\"agent-2\",\"session\":\"session-2\","
                     "\"parked\":{\"ok\":true,\"ready\":[{\"park-id\":\"p2\",\"prompt\":\"park two\"}]},"
                     "\"followups\":{\"ok\":true,\"ready\":[]}},"
                     "{\"agent\":\"agent-3\",\"session\":\"session-3\","
                     "\"parked\":{\"ok\":true,\"ready\":[]},"
                     "\"followups\":{\"ok\":true,\"ready\":[{\"followup-id\":\"f3\",\"prompt\":\"follow three\"}]}}]}"))))
                ((symbol-function 'agent-repl-park--resume-in-buffer)
                 (lambda (buf prompt id &optional api-url)
                   (push (list buf prompt id api-url) parked)))
                ((symbol-function 'agent-repl-capability)
                 (lambda (key) (pcase key (:sender 'sender) (:agent-name "agent") (_ nil))))
                ((symbol-function 'agent-chat-send-unsolicited-input)
                 (lambda (&rest args) (push (cons (current-buffer) args) followups)))
                ((symbol-function 'agent-repl-followup--ack)
                 (lambda (id api-url) (push (list id api-url) acks))))
        (agent-repl-park--poll-once))
      (should (equal (list (nth 1 buffers) "park two" "p2" "http://agency.test")
                     (car parked)))
      (should (eq (nth 2 buffers) (caar followups)))
      (should (equal '("f3" "http://agency.test") (car acks)))
      (should (= 1 (length parked)))
      (should (= 1 (length followups))))))

(ert-deftest agent-repl-park-batch-404-falls-back-next-tick ()
  (agent-repl-park-batch-test--with-buffers buffers
    (let ((retrieves 0) legacy)
      (cl-letf (((symbol-function 'agent-repl-park--retrieve)
                 (lambda (_url callback)
                   (setq retrieves (1+ retrieves))
                   (agent-repl-park-batch-test--response callback 404 "{}")))
                ((symbol-function 'agent-repl-park--poll-buffer-async)
                 (lambda (buf) (push (list :park buf) legacy)))
                ((symbol-function 'agent-repl-followup--poll-buffer-async)
                 (lambda (buf) (push (list :followup buf) legacy))))
        (agent-repl-park--poll-once)
        (should (= 1 retrieves))
        (should-not legacy)
        (agent-repl-park--poll-once)
        (should (= 1 retrieves))
        (should (= 6 (length legacy)))))))

(ert-deftest agent-repl-park-batch-inflight-prevents-stacking ()
  (agent-repl-park-batch-test--with-buffers buffers
    (let (callbacks calls)
      (cl-letf (((symbol-function 'agent-repl-park--retrieve)
                 (lambda (_url callback)
                   (setq calls (1+ (or calls 0)))
                   (push callback callbacks))))
        (agent-repl-park--poll-once)
        (agent-repl-park--poll-once)
        (should (= 1 calls))
        (should (= 1 (length callbacks)))))))

(provide 'agent-repl-park-batch-test)
;;; agent-repl-park-batch-test.el ends here
