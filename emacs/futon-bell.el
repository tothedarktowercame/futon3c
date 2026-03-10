;;; futon-bell.el --- Poll-based bell for Agency turn completion -*- lexical-binding: t; -*-

;; When an agent finishes a turn, the server writes /tmp/futon-bell.edn.
;; A 1-second timer polls the file for changes and fires joe/visible-bell.
;; After firing, writes a receipt to /tmp/futon-bell-receipt.edn.
;; Uses polling because file-notify on /tmp (tmpfs) is unreliable.

(defvar futon-bell-file "/tmp/futon-bell.edn"
  "File written by the Agency registry when an agent goes idle.")

(defvar futon-bell-receipt-file "/tmp/futon-bell-receipt.edn"
  "Receipt file written by Emacs after processing a bell.")

(defvar futon-bell--timer nil
  "Active poll timer, or nil.")

(defvar futon-bell--last-nonce nil
  "Last seen nonce, to avoid double-firing.")

(defvar futon-bell--stats (list :delivered 0 :missed 0)
  "Running stats for bell delivery.")

(defun futon-bell--write-receipt (agent-id nonce)
  "Write a signed receipt confirming the bell was delivered."
  (let ((receipt (format "(:agent-id %S :nonce %s :received-at %S :emacs-socket %S)"
                         agent-id nonce
                         (format-time-string "%FT%T%z")
                         (or (daemonp) "default"))))
    (write-region receipt nil futon-bell-receipt-file nil 'silent)))

(defun futon-bell--poll ()
  "Check bell file for new events. Called by timer."
  (when (file-exists-p futon-bell-file)
    (condition-case err
        (let* ((content (with-temp-buffer
                          (insert-file-contents-literally futon-bell-file)
                          (buffer-string)))
               (data (and (not (string-empty-p (string-trim content)))
                          (read content)))
               (agent-id (and data (plist-get data :agent-id)))
               (nonce (and data (plist-get data :nonce))))
          (when (and nonce (not (equal nonce futon-bell--last-nonce)))
            (setq futon-bell--last-nonce nonce)
            ;; Fire the bell
            (if (fboundp 'joe/visible-bell)
                (progn
                  (joe/visible-bell (format "[bell] %s done" (or agent-id "?")))
                  (plist-put futon-bell--stats :delivered
                             (1+ (plist-get futon-bell--stats :delivered))))
              (message "[bell] %s done (joe/visible-bell not defined)" (or agent-id "?"))
              (plist-put futon-bell--stats :missed
                         (1+ (plist-get futon-bell--stats :missed))))
            ;; Write receipt
            (futon-bell--write-receipt agent-id nonce)))
      (error
       (message "futon-bell poll error: %S" err)))))

(defun futon-bell-start ()
  "Start polling the bell file (every 1 second)."
  (interactive)
  (futon-bell-stop)
  (setq futon-bell--timer (run-at-time 1 1 #'futon-bell--poll))
  (message "futon-bell: polling %s every 1s" futon-bell-file))

(defun futon-bell-stop ()
  "Stop polling."
  (interactive)
  (when futon-bell--timer
    (cancel-timer futon-bell--timer)
    (setq futon-bell--timer nil)
    (message "futon-bell: stopped")))

(defun futon-bell-status ()
  "Show bell status and last receipt."
  (interactive)
  (let ((receipt (when (file-exists-p futon-bell-receipt-file)
                   (with-temp-buffer
                     (insert-file-contents-literally futon-bell-receipt-file)
                     (buffer-string)))))
    (message "futon-bell: timer=%s delivered=%d missed=%d last-receipt=%s"
             (if futon-bell--timer "active" "stopped")
             (plist-get futon-bell--stats :delivered)
             (plist-get futon-bell--stats :missed)
             (or receipt "none"))))

(provide 'futon-bell)
;;; futon-bell.el ends here
