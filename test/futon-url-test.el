;;; futon-url-test.el --- Deadline-bounded synchronous url.el requests -*- lexical-binding: t; -*-

;; A local server answers promptly, late, or never.  In the late and never
;; cases `url-retrieve-synchronously' leaves a buffer or an open socket behind;
;; `futon-url-retrieve-synchronously' must leave neither.

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../emacs" (file-name-directory load-file-name)))
(require 'futon-url)

(defun futon-url-test--server (delay)
  "Start a server answering each request after DELAY seconds, or never if nil."
  (make-network-process
   :name "futon-url-test" :server t :host "127.0.0.1" :service t
   :family 'ipv4 :noquery t :sentinel #'ignore
   :filter (lambda (proc _)
             (when delay
               (run-at-time delay nil
                            (lambda ()
                              (when (process-live-p proc)
                                (process-send-string
                                 proc "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"))))))))

(defun futon-url-test--leftovers (port)
  "Open client connections and response buffers for PORT."
  (list :connections
        (cl-count-if (lambda (p)
                       (and (not (process-contact p :server))
                            (eq (process-status p) 'open)
                            (equal (plist-get (process-contact p t) :service) port)))
                     (process-list))
        :buffers
        (cl-count-if (lambda (b)
                       (string-prefix-p (format " *http 127.0.0.1:%d" port) (buffer-name b)))
                     (buffer-list))))

(defmacro futon-url-test--with-server (delay port-var &rest body)
  (declare (indent 2))
  (let ((server (make-symbol "server")))
    `(let* ((,server (futon-url-test--server ,delay))
            (,port-var (process-contact ,server :service))
            (url-show-status nil))
       (unwind-protect (progn ,@body)
         (dolist (p (process-list))
           (when (equal (plist-get (process-contact p t) :service) ,port-var)
             (set-process-sentinel p #'ignore)
             (delete-process p)))))))

(defun futon-url-test--settle (seconds)
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (accept-process-output nil 0.05))))

(ert-deftest futon-url-returns-the-response-buffer ()
  (futon-url-test--with-server 0 port
    (let ((buf (futon-url-retrieve-synchronously (format "http://127.0.0.1:%d/" port) 2)))
      (should (buffer-live-p buf))
      (should (string-suffix-p "ok" (with-current-buffer buf (buffer-string))))
      (kill-buffer buf))))

(ert-deftest futon-url-cancels-an-unanswered-request ()
  (futon-url-test--with-server nil port
    (should-not (futon-url-retrieve-synchronously (format "http://127.0.0.1:%d/" port) 0.3))
    (should (equal '(:connections 0 :buffers 0) (futon-url-test--leftovers port)))))

(ert-deftest futon-url-leaves-nothing-for-a-late-answer ()
  (futon-url-test--with-server 0.6 port
    (should-not (futon-url-retrieve-synchronously (format "http://127.0.0.1:%d/" port) 0.3))
    (futon-url-test--settle 1.0)
    (should (equal '(:connections 0 :buffers 0) (futon-url-test--leftovers port)))))

(ert-deftest futon-url-returns-promptly-when-nothing-listens ()
  ;; Port 1 refuses.  url.el then runs the callback with an error, so, as with
  ;; `url-retrieve-synchronously', the result may be a buffer with no HTTP
  ;; response in it; the wait must end on the failure, not at the deadline.
  (let* ((start (float-time))
         (buf (futon-url-retrieve-synchronously "http://127.0.0.1:1/" 5)))
    (should (< (- (float-time) start) 2))
    (when buf
      (should-not (with-current-buffer buf
                    (goto-char (point-min))
                    (search-forward "\n\n" nil t)))
      (kill-buffer buf))
    (should (equal '(:connections 0 :buffers 0) (futon-url-test--leftovers 1)))))

;;; futon-url-test.el ends here
