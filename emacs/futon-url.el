;;; futon-url.el --- Synchronous url.el requests that end at their deadline -*- lexical-binding: t; -*-

;; `url-retrieve-synchronously' with a TIMEOUT returns nil when the time is up
;; but leaves the request running.  A late answer then lands in a response
;; buffer nobody kills, and an answer that never comes holds its socket open
;; for the life of Emacs.  In a long-running daemon those sockets add up: on
;; 2026-09-27 the graph daemon reached its 1024-descriptor limit, every later
;; connection failed and left a dead process behind, and with 20,000 of those
;; Emacs spent all its time choosing process names and stopped answering
;; emacsclient.  See futon0/README-emacs.md §9.
;;
;; Use `futon-url-retrieve-synchronously' wherever you would pass a TIMEOUT to
;; `url-retrieve-synchronously'.  It takes the same dynamic bindings
;; (`url-request-method', `url-request-data', ...) and returns the same kind of
;; buffer, which the caller still kills.

;;; Code:

(require 'url)

(defun futon-url-retrieve-synchronously (url timeout)
  "Retrieve URL silently, waiting at most TIMEOUT seconds; return the buffer.

Return the response buffer, which the caller must kill, or nil when there is no
answer in time or the connection fails.  Unlike `url-retrieve-synchronously', a
request that misses the deadline is cancelled: its connection is deleted and
its buffer killed, so nothing outlives the call.  TIMEOUT nil waits
indefinitely."
  (let* ((data-buffer nil)
         (proc-buffer (url-retrieve url (lambda (&rest _) (setq data-buffer (current-buffer)))
                                    nil t t))
         (deadline (and timeout (+ (float-time) timeout))))
    (when proc-buffer
      (catch 'done
        (while (not data-buffer)
          (when (or (not (buffer-live-p proc-buffer))
                    (and deadline (> (float-time) deadline)))
            (throw 'done nil))
          ;; Follow a redirect the way `url-retrieve-synchronously' does.
          (let ((redirect (buffer-local-value 'url-redirect-buffer proc-buffer)))
            (when (and redirect (not (eq redirect proc-buffer)))
              (let (kill-buffer-query-functions) (kill-buffer proc-buffer))
              (setq proc-buffer redirect)))
          (let ((proc (get-buffer-process proc-buffer)))
            (when (and proc (memq (process-status proc) '(closed exit signal failed)))
              (throw 'done nil)))
          (accept-process-output nil 0.05)))
      (unless (eq data-buffer proc-buffer)
        (when (buffer-live-p proc-buffer)
          (let ((proc (get-buffer-process proc-buffer)))
            (when proc
              ;; url.el's end-of-document sentinel re-issues a request whose
              ;; connection closed early, so detach it before deleting.
              (set-process-sentinel proc #'ignore)
              (delete-process proc)))
          (let (kill-buffer-query-functions) (kill-buffer proc-buffer)))))
    data-buffer))

(defun futon-url-retrieve (url timeout callback)
  "`url-retrieve' URL silently, calling CALLBACK once, with a TIMEOUT deadline.

CALLBACK gets url.el's STATUS argument in the response buffer, which it must
kill.  If no answer arrives within TIMEOUT seconds the connection is deleted
and CALLBACK runs in a scratch buffer with status (:error (timeout URL)), so
the caller handles a timeout the way it handles any other failure.  Callers
bind `url-request-method' and friends around this call, as for `url-retrieve'."
  (let* ((done nil)
         (resp-buf nil)
         (once (lambda (status)
                 (unless done
                   (setq done t)
                   (funcall callback status)))))
    (setq resp-buf (url-retrieve url once nil t t))
    (run-at-time
     timeout nil
     (lambda ()
       (unless done
         (when (buffer-live-p resp-buf)
           (let ((proc (get-buffer-process resp-buf)))
             (when proc
               ;; url.el's end-of-document sentinel re-issues a request whose
               ;; connection closed early, so detach it before deleting.
               (set-process-sentinel proc #'ignore)
               (delete-process proc)))
           (kill-buffer resp-buf))
         (with-temp-buffer
           (funcall once (list :error (list 'timeout url)))))))
    resp-buf))

(provide 'futon-url)

;;; futon-url.el ends here
