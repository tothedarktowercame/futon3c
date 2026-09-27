;;; agent-repl-park-test.el --- Connection hygiene for the park pollers -*- lexical-binding: t; -*-

;; On 2026-09-27 the graph daemon hung: requests :7070 never answered stayed
;; open, the park poller's expiring latch replaced them every 30 s without
;; closing them, and the descriptor limit ran out.  These tests use a local
;; server that accepts connections and never replies.

(require 'ert)
(require 'cl-lib)
(require 'url-http)
(add-to-list 'load-path (expand-file-name "../emacs" (file-name-directory load-file-name)))
(require 'agent-repl-park)
;; Loading the module starts the real poller against the live Agency.
(agent-repl-park-disable)

(defun agent-repl-park-test--silent-server ()
  "Start a server that accepts connections and never answers; return it."
  (make-network-process :name "park-test-silent" :server t :host "127.0.0.1"
                        :service t :family 'ipv4 :noquery t
                        :filter #'ignore :sentinel #'ignore))

(defun agent-repl-park-test--open-clients (port)
  "Open client connections from this Emacs to PORT."
  (cl-count-if (lambda (p)
                 (and (eq (process-type p) 'network)
                      (not (process-contact p :server))
                      (eq (process-status p) 'open)
                      (equal (plist-get (process-contact p t) :service) port)))
               (process-list)))

(defmacro agent-repl-park-test--with-silent-server (port-var &rest body)
  (declare (indent 1))
  (let ((server (make-symbol "server")))
    `(let* ((,server (agent-repl-park-test--silent-server))
            (,port-var (process-contact ,server :service))
            (url-show-status nil))
       (unwind-protect (progn ,@body)
         (dolist (p (process-list))
           (when (equal (plist-get (process-contact p t) :service) ,port-var)
             (set-process-sentinel p #'ignore)
             (delete-process p)))
         (delete-process ,server)))))

(ert-deftest agent-repl-park-retrieve-aborts-an-unanswered-request ()
  (agent-repl-park-test--with-silent-server port
    (let ((agent-repl-park-request-timeout 0.3)
          (status :pending))
      (agent-repl-park--retrieve (format "http://127.0.0.1:%d/x" port)
                                 (lambda (s) (setq status s)))
      (with-timeout (5 (ert-fail "callback never ran"))
        (while (eq status :pending) (accept-process-output nil 0.05)))
      (should (eq 'timeout (car (plist-get status :error))))
      (should (= 0 (agent-repl-park-test--open-clients port))))))

(ert-deftest agent-repl-park-stale-polls-do-not-accumulate-connections ()
  (agent-repl-park-test--with-silent-server port
    (let ((agent-repl-park-request-timeout 0.2)
          (agent-repl-park-poll-stale-seconds 0.3)
          (buf (generate-new-buffer " *park-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local claude-repl-api-url (format "http://127.0.0.1:%d" port))
              (setq-local agent-chat--agent-id "park-test")
              (setq-local agent-chat--session-id "park-test"))
            ;; Poll as often as the latch allows for 3 s: without a request
            ;; deadline this leaves about ten connections open.
            (let ((end (+ (float-time) 3)))
              (while (< (float-time) end)
                (agent-repl-park--poll-buffer-async buf)
                (accept-process-output nil 0.05)))
            (should (<= (agent-repl-park-test--open-clients port) 1)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-park-reaper-deletes-dead-url-connections-only ()
  (let ((dead (cl-loop repeat 5 collect
                       (make-network-process
                        :name "park-test-dead" :host "127.0.0.1" :service 1
                        :nowait t :noquery t :buffer (generate-new-buffer " *park-test-dead*")
                        :sentinel 'url-http-idle-sentinel)))
        (other (make-network-process
                :name "park-test-other" :host "127.0.0.1" :service 1
                :nowait t :noquery t :sentinel #'ignore)))
    (unwind-protect
        (progn
          (with-timeout (5 (ert-fail "connections to port 1 did not fail"))
            (while (cl-some (lambda (p) (eq (process-status p) 'connect)) (cons other dead))
              (accept-process-output nil 0.05)))
          (agent-repl-park--reap-dead-url-processes)
          ;; First sight only marks them, so their sentinels have run first.
          (should (cl-every (lambda (p) (memq p (process-list))) dead))
          (agent-repl-park--reap-dead-url-processes)
          (should-not (cl-some (lambda (p) (memq p (process-list))) dead))
          (should (memq other (process-list))))
      (delete-process other))))

;;; agent-repl-park-test.el ends here
