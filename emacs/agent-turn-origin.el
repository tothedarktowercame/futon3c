;;; agent-turn-origin.el --- Write-time turn provenance -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(defvar-local agent-turn-origin-current nil
  "Immutable source context for the active turn; independent of displayed author.")
(defvar-local agent-turn-origin-pending-user nil
  "Source retained while the first user record waits for a session ID.")
(defvar-local agent-turn-origin-evidence-user nil
  "Source of the staged user record being flushed, independent of active input.")
(defvar agent-turn-origin-input nil
  "Source context carried into a queued or unsolicited turn.")

(defun agent-turn-harness-stamp (source session-id)
  "Return the execution harness known by the turn producer.
SOURCE is the same write-time provenance plist used for origin, optionally
carrying :delivery, :job-id and :job-harness from a trusted Agency delivery.
SESSION-ID identifies an explicitly plain Emacs turn.  Caller and author names
are deliberately ignored."
  (let ((kind (plist-get source :kind))
        (actor (plist-get source :actor))
        (delivery (plist-get source :delivery))
        (job-id (plist-get source :job-id))
        (job-harness (plist-get source :job-harness)))
    (cond
     ((equal kind "operator")
      `((kind . "none") (basis . "producer-context")
        (source-ref . ,(or session-id "emacs-repl"))))
     ((eq delivery 'bell)
      (cond
       ((and (stringp job-id) (not (string-empty-p job-id))
             (consp job-harness))
        (copy-tree job-harness))
       ((and (stringp job-id) (not (string-empty-p job-id)))
        `((kind . "none") (basis . "producer-context")
          (source-ref . ,job-id)))
       (t
        '((kind . "unknown") (basis . "producer-context")
          (reason . "bell turn has no bound Agency job id")))))
     ((or (member actor '("parked-resume" "followup" "continuation"))
          (member delivery '(parked-resume typed)))
      `((kind . "none") (basis . "producer-context")
        (source-ref . ,(or (plist-get source :source-id)
                           session-id "emacs-repl"))))
     (t
      '((kind . "unknown") (basis . "producer-context")
        (reason . "turn producer cannot determine execution harness"))))))

(defun agent-turn-origin-decide (origin speaker)
  "Decide ORIGIN from the input path and SPEAKER, never from message text."
  (cond
   ((eq origin 'operator)
    (list :kind "operator" :actor (or (getenv "USER") user-login-name "joe")))
   ((member speaker '("continuation" "parked-resume"))
    '(:kind "harness" :actor "parked-resume"))
   ((member speaker '("followup" "system" "inbox-zero"))
    (list :kind "harness" :actor speaker))
   ((eq origin 'agent) (list :kind "agent" :actor speaker))
   (t '(:kind "unknown" :actor "unknown"))))

(defun agent-turn-origin-caller ()
  "Return the source actor sent to Agency; do not substitute the login name."
  (or (plist-get agent-turn-origin-current :actor) "unknown"))

(defun agent-turn-origin-stamp (author writer &optional source)
  "Make AUTHOR/WRITER's wire stamp using SOURCE or the active turn context.
This records origin, never grants authorization."
  (let ((source (or source agent-turn-origin-current
                    '(:kind "unknown" :actor "unknown"))))
    (append `((kind . ,(plist-get source :kind))
              (actor . ,(plist-get source :actor))
              (writer . ,writer) (attributed-author . ,author)
              (authorization . "unknown") (basis . "write-time")
              (recorded-at . ,(format-time-string "%FT%TZ" nil t))
              (surface . "emacs-repl"))
            (when (plist-get source :source-id)
              `((source-id . ,(plist-get source :source-id)))))))

(provide 'agent-turn-origin)
;;; agent-turn-origin.el ends here
