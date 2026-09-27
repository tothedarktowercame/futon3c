;;; agent-turn-origin.el --- Write-time turn provenance -*- lexical-binding: t; -*-

(require 'cl-lib)
(defvar-local agent-turn-origin-current nil
  "Immutable source context for the active turn; independent of displayed author.")
(defvar-local agent-turn-origin-pending-user nil
  "Source retained while the first user record waits for a session ID.")
(defvar-local agent-turn-origin-evidence-user nil
  "Source of the staged user record being flushed, independent of active input.")
(defvar agent-turn-origin-input nil
  "Source context carried into a queued or unsolicited turn.")

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
