;;; inbox-zero-reazon-reminder.el --- Remind Joe when hygiene needs Codex -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)
(require 'agent-chat-invariants)

(agent-chat-invariants--ensure-reazon)

(defcustom inbox-zero-reazon-reminder-state-file
  "/home/joe/code/futon0/data/inbox-zero-delta-state.json"
  "Hourly inbox-zero health receipt consumed by the reminder."
  :type 'file
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

(defun inbox-zero-reazon-reminder--status ()
  "Return the current inbox-zero health status from systemd and its receipt."
  (condition-case err
      (let* ((timer (inbox-zero-reazon-reminder--systemd-properties
                     "futon-sync-check-clean.timer" '("ActiveState")))
             (service (inbox-zero-reazon-reminder--systemd-properties
                       "futon-sync-check-clean.service"
                       '("Result" "ExecMainStatus" "ExecMainExitTimestamp")))
             (timestamp (cdr (assoc "ExecMainExitTimestamp" service)))
             (age (and timestamp (not (string-empty-p timestamp))
                       (- (float-time) (float-time (date-to-time timestamp)))))
             (receipt (with-temp-buffer
                        (insert-file-contents
                         inbox-zero-reazon-reminder-state-file)
                        (json-parse-buffer :object-type 'alist
                                           :array-type 'list)))
             (signature (alist-get 'signature receipt)))
        (cond
         ((not (equal "active" (cdr (assoc "ActiveState" timer))))
          'timer-inactive)
         ((or (not (equal "success" (cdr (assoc "Result" service))))
              (not (equal "0" (cdr (assoc "ExecMainStatus" service)))))
          'gate-failed)
         ((or (not age) (> age inbox-zero-reazon-reminder-max-age-seconds))
          'gate-stale)
         ((and signature (cl-some (lambda (repo) (cdr repo)) signature))
          'failures-present)
         (t 'healthy)))
    (error (cons 'monitor-unreadable (error-message-string err)))))

(reazon-defrel inbox-zero-reazon-reminder--unhealthyo (status)
  (reazon-conde
   ((reazon-== status 'timer-inactive))
   ((reazon-== status 'gate-failed))
   ((reazon-== status 'gate-stale))
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
