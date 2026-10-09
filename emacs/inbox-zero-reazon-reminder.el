;;; inbox-zero-reazon-reminder.el --- Remind Joe when hygiene needs Codex -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)
(require 'agent-chat-invariants)

(agent-chat-invariants--ensure-reazon)

(defcustom inbox-zero-reazon-reminder-receipt-files
  '("/home/joe/code/storage/inbox-zero/operator-backlog.edn"
    "/home/joe/code/storage/inbox-zero/push-log.edn"
    "/home/joe/code/storage/inbox-zero/sync-log.edn"
    "/home/joe/code/storage/inbox-zero/worktree-log.edn")
  "Receipts written by one pass of the in-process inbox-zero sweeper."
  :type '(repeat file)
  :group 'agent-chat-invariants)

(defcustom inbox-zero-reazon-reminder-max-age-seconds 7200
  "Maximum age of the latest completed inbox-zero gate run."
  :type 'integer
  :group 'agent-chat-invariants)

(defun inbox-zero-reazon-reminder--systemd-properties (unit properties)
  "Return an alist of PROPERTIES reported by systemd for UNIT."
  (with-temp-buffer
    (let ((exit (apply #'process-file
                       "systemctl" nil t nil "--user" "show" unit "--no-pager"
                       (mapcar (lambda (property)
                                 (concat "--property=" property))
                               properties))))
      (unless (zerop exit)
        (error "systemctl failed for %s (exit %s)" unit exit))
      (mapcar (lambda (line)
                (pcase-let ((`(,key ,value) (split-string line "=")))
                  (cons key value)))
              (split-string (string-trim (buffer-string)) "\n" t)))))

(defun inbox-zero-reazon-reminder--newest-receipt-age ()
  "Return the age in seconds of the newest inbox-zero sweeper receipt."
  (let ((mtimes (delq nil
                      (mapcar (lambda (file)
                                (when (file-exists-p file)
                                  (float-time
                                   (file-attribute-modification-time
                                    (file-attributes file)))))
                              inbox-zero-reazon-reminder-receipt-files))))
    (when mtimes
      (- (float-time) (apply #'max mtimes)))))

(defun inbox-zero-reazon-reminder--status ()
  "Return health of the pressure-triggered inbox-zero sweeper.

The old `futon-sync-check-clean.timer' was deliberately retired on
2026-09-18.  The serving futon3c JVM now owns the recurring sweeper, and its
current-state files are the durable proof that passes continue to finish."
  (condition-case err
      (let* ((service (inbox-zero-reazon-reminder--systemd-properties
                       "futon3c-zone.service" '("ActiveState" "SubState")))
             (age (inbox-zero-reazon-reminder--newest-receipt-age)))
        (cond
         ((or (not (equal "active" (cdr (assoc "ActiveState" service))))
              (not (equal "running" (cdr (assoc "SubState" service)))))
          'sweeper-service-inactive)
         ((or (not age) (> age inbox-zero-reazon-reminder-max-age-seconds))
          'sweeper-stale)
         (t 'healthy)))
    (error (cons 'monitor-unreadable (error-message-string err)))))

(reazon-defrel inbox-zero-reazon-reminder--unhealthyo (status)
  (reazon-conde
   ((reazon-== status 'sweeper-service-inactive))
   ((reazon-== status 'sweeper-stale))
   ((reazon-== status 'failures-present))
   ((reazon-fresh (message)
      (reazon-conso 'monitor-unreadable message status)))))

(defun inbox-zero-reazon-reminder-check ()
  "Return a reminder violation when the inbox-zero receipt is unhealthy."
  (let ((status (inbox-zero-reazon-reminder--status)))
    (when (reazon-run 1 q
            (inbox-zero-reazon-reminder--unhealthyo status))
      (list (list :inbox-zero
                  (format "Inbox zero is unhealthy (%S); please ask Codex to repair it"
                          status))))))

(agent-chat-invariants-register-reazon-check
 #'inbox-zero-reazon-reminder-check)

(provide 'inbox-zero-reazon-reminder)
;;; inbox-zero-reazon-reminder.el ends here
