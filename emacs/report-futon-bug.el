;;; report-futon-bug.el --- Report a futon bug without thinking about it -*- lexical-binding: t; -*-

;; Like `report-emacs-bug': the command gathers its own context, so Joe only
;; supplies one line.  Joe, 2026-10-10: "so I don't have to think about the
;; bugs, I could just bell them out ... the bug report would have to be
;; relatively self contained".
;;
;; `report-futon-bug' collects the Emacs side (region or REPL tail,
;; *Messages*, *Backtrace*), hands it to scripts/futon_bug_report.py, which
;; adds health, journal excerpts, repo HEADs and zone-health, scrubs secrets
;; and writes one markdown file under ~/notes/futon-bugs/.  The report is
;; opened for review; optionally it is belled, whole, to an Agency agent with
;; --mode work.  The bell carries the full text, so a reader on another host
;; needs nothing else.  Which agent to send to is left open (routing later).
;;
;; Origin: futon0/holes/missions/M-landscape-positioning.md §5.

;;; Code:

(require 'subr-x)

(defgroup report-futon-bug nil "Self-contained futon bug reports." :group 'tools)

(defcustom report-futon-bug-collector
  (expand-file-name "../scripts/futon_bug_report.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to futon_bug_report.py." :type 'string)

(defcustom report-futon-bug-sender
  (expand-file-name "../scripts/agency_send.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to agency_send.py." :type 'string)

(defcustom report-futon-bug-from "futon-bug"
  "Sender id on the bell.  Not on the roster, so no bellback lands anywhere;
the result is read from the job, or the triage agent reports where told."
  :type 'string)

(defcustom report-futon-bug-default-agent nil
  "Agent offered as the default recipient, or nil to offer none."
  :type '(choice (const nil) string))

(defcustom futon-bug-feed-attachment-file
  (expand-file-name "futon/bug-feed-agent" (or (getenv "XDG_CONFIG_HOME")
                                                "~/.config"))
  "Private file naming the agent subscribed to completed bug reports."
  :type 'file)

(defcustom report-futon-bug-buffer-lines 150
  "Lines of the current buffer included when no region is active."
  :type 'integer)

(defcustom report-futon-bug-messages-lines 60
  "Lines from the end of *Messages* included in the report."
  :type 'integer)

(defun report-futon-bug--tail (buffer n)
  "Return the last N lines of BUFFER as a string."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (forward-line (- n))
      (buffer-substring-no-properties (point) (point-max)))))

(defun report-futon-bug--fence (text)
  "Wrap TEXT in a code fence that TEXT cannot close."
  (let ((fence "```"))
    (while (string-match-p (regexp-quote fence) text)
      (setq fence (concat fence "`")))
    (format "%s\n%s\n%s" fence (string-trim-right text) fence)))

(defun report-futon-bug--emacs-context ()
  "Markdown describing the Emacs state Joe was in."
  (let* ((buf (current-buffer))
         (agent (and (boundp 'agent-chat--agent-id)
                     (buffer-local-value 'agent-chat--agent-id buf)))
         (region (and (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))))
         (backtrace (get-buffer "*Backtrace*")))
    (concat
     (format "- buffer: %s (%s)\n" (buffer-name buf) major-mode)
     (when agent (format "- REPL agent: %s\n" agent))
     (when buffer-file-name (format "- file: %s\n" buffer-file-name))
     (format "- emacs: %s; daemon: %s\n\n" emacs-version (or (daemonp) "no"))
     (if region
         (format "### Selected region\n\n%s\n\n" (report-futon-bug--fence region))
       (format "### Last %d lines of %s\n\n%s\n\n" report-futon-bug-buffer-lines
               (buffer-name buf)
               (report-futon-bug--fence
                (report-futon-bug--tail buf report-futon-bug-buffer-lines))))
     (when backtrace
       (format "### *Backtrace*\n\n%s\n\n"
               (report-futon-bug--fence (report-futon-bug--tail backtrace 80))))
     (format "### *Messages* (last %d lines)\n\n%s\n"
             report-futon-bug-messages-lines
             (report-futon-bug--fence
              (report-futon-bug--tail (messages-buffer)
                                      report-futon-bug-messages-lines))))))

(defun report-futon-bug--agents ()
  "Agent ids on the local Agency roster, or nil if it cannot be read."
  (ignore-errors
    (let* ((json (shell-command-to-string
                  "curl -s -m 3 http://127.0.0.1:7070/api/alpha/agents"))
           (data (json-parse-string json :object-type 'alist))
           (agents (alist-get 'agents data)))
      (sort (delq nil (mapcar (lambda (a) (if (consp a) (symbol-name (car a)) nil))
                              agents))
            #'string<))))

(defun futon-bug-feed--attached-agent ()
  "Return the attached agent id, or nil when the feed is detached."
  (when (file-readable-p futon-bug-feed-attachment-file)
    (let ((agent (string-trim
                  (with-temp-buffer
                    (insert-file-contents futon-bug-feed-attachment-file)
                    (buffer-string)))))
      (unless (string-empty-p agent) agent))))

(defun futon-bug-feed--current-agent ()
  "Return the agent id associated with the current buffer, if any."
  (and (boundp 'agent-chat--agent-id)
       (stringp agent-chat--agent-id)
       (not (string-empty-p agent-chat--agent-id))
       agent-chat--agent-id))

;;;###autoload
(defun futon-bug-feed-attach (agent)
  "Push future completed bug reports to AGENT.
In an agent REPL, default to that buffer's agent.  The attachment is durable
across Emacs and Voxterm restarts, but names a seat rather than a role; attach
a replacement when the seat is retired."
  (interactive
   (let ((here (futon-bug-feed--current-agent)))
     (list (completing-read
            (if here (format "Attach bug feed to agent [%s]: " here)
              "Attach bug feed to agent: ")
            (report-futon-bug--agents) nil t nil nil here))))
  (setq agent (string-trim (or agent "")))
  (when (string-empty-p agent)
    (user-error "An agent id is required"))
  (make-directory (file-name-directory futon-bug-feed-attachment-file) t)
  (with-temp-file futon-bug-feed-attachment-file (insert agent "\n"))
  (set-file-modes futon-bug-feed-attachment-file #o600)
  (message "Futon bug feed attached to %s" agent)
  agent)

;;;###autoload
(defun futon-bug-feed-detach ()
  "Stop pushing completed bug reports to an agent.
Reports remain durable under ~/notes/futon-bugs and continue to open in Emacs."
  (interactive)
  (let ((old (futon-bug-feed--attached-agent)))
    (when (file-exists-p futon-bug-feed-attachment-file)
      (delete-file futon-bug-feed-attachment-file))
    (if old
        (message "Futon bug feed detached from %s" old)
      (message "Futon bug feed was already detached"))
    old))

;;;###autoload
(defun futon-bug-feed-status ()
  "Report which agent, if any, receives completed bug reports."
  (interactive)
  (let ((agent (futon-bug-feed--attached-agent)))
    (if agent
        (message "Futon bug feed is attached to %s" agent)
      (message "Futon bug feed is detached"))
    agent))

(defun report-futon-bug--bell (agent path)
  "Bell the report at PATH, whole, to AGENT.  Return the sender's output."
  (with-temp-buffer
    (insert (format "A futon bug report follows. Read it in full; it is \
self-contained. Do what its \"What the reader is asked to do\" section says, \
then reply with what you checked and what you found.\n\n"))
    (insert-file-contents path nil nil nil nil)
    (goto-char (point-max))
    (let ((status (call-process-region
                   (point-min) (point-max) "python3" t t nil
                   report-futon-bug-sender "--to" agent
                   "--from" report-futon-bug-from
                   "--kind" "bell" "--mode" "work")))
      (format "exit %s: %s" status
              (string-trim (buffer-substring (point-min) (min (point-max) 400)))))))

(defun report-futon-bug--finish-async (process event)
  "Finish an asynchronous bug collection PROCESS after EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((buffer (process-buffer process))
           (agent (process-get process 'report-futon-bug-agent))
           (path (and (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (string-trim (buffer-string))))))
      (unwind-protect
          (if (and (zerop (process-exit-status process))
                   path (file-exists-p path))
              (progn
                (find-file-other-window path)
                (if agent
                    (message "report-futon-bug: %s; belled %s (%s)" path agent
                             (report-futon-bug--bell agent path))
                  (message "report-futon-bug: wrote %s (not sent)" path)))
            (display-warning
             'report-futon-bug
             (format "Bug collection failed (%s): %s" (string-trim event)
                     (or path "no collector output"))
             :warning))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when-let* ((ctx (process-get process 'report-futon-bug-context-file)))
          (ignore-errors (delete-file ctx)))))))

;;;###autoload
(defun report-futon-bug-async (summary &optional agent)
  "Capture SUMMARY now and collect its report asynchronously.
AGENT, when non-nil, receives the completed report.  This entry point is for
low-friction surfaces such as Voxterm: returning successfully means the
operator's words and Emacs context are already durable in the collector's
input file; slow health and journal reads happen outside the UI request."
  (when (string-empty-p (string-trim summary))
    (user-error "A one-line summary is needed"))
  (setq agent (or agent (futon-bug-feed--attached-agent)))
  (let ((ctx-file (make-temp-file "futon-bug-emacs-" nil ".md"))
        (buffer (generate-new-buffer " *futon-bug-collector*")))
    (with-temp-file ctx-file (insert (report-futon-bug--emacs-context)))
    (let ((process
           (make-process
            :name "futon-bug-collector" :buffer buffer :noquery t
            :command (list "python3" report-futon-bug-collector
                           "--summary" summary "--emacs-context" ctx-file)
            :sentinel #'report-futon-bug--finish-async)))
      (process-put process 'report-futon-bug-agent agent)
      (process-put process 'report-futon-bug-context-file ctx-file)
      (message "BUG REPORT captured; collecting context in the background")
      (list :accepted t :process (process-id process)))))

;;;###autoload
(defun report-futon-bug (summary &optional agent)
  "Write a self-contained futon bug report for SUMMARY; optionally bell AGENT.
Collects the region (or the tail of the current buffer), *Messages* and
*Backtrace*, then service health, journal excerpts, repo HEADs and
zone-health, with secrets scrubbed.  The report opens for review.  With an
empty agent at the prompt, nothing is sent."
  (interactive
   (list (read-string "Bug, in one line: ")
         (let ((choice (completing-read
                        (format "Bell to agent (empty = just write it)%s: "
                                (if report-futon-bug-default-agent
                                    (format " [%s]" report-futon-bug-default-agent) ""))
                        (report-futon-bug--agents) nil nil nil nil
                        report-futon-bug-default-agent)))
           (unless (string-empty-p (or choice "")) choice))))
  (when (string-empty-p (string-trim summary))
    (user-error "A one-line summary is needed"))
  (let* ((context (report-futon-bug--emacs-context)) ; read before any temp buffer
         (ctx-file (make-temp-file "futon-bug-emacs-" nil ".md"))
         (path nil))
    (unwind-protect
        (progn
          (with-temp-file ctx-file (insert context))
          (message "report-futon-bug: collecting (health, journals, zone-health)...")
          (setq path (string-trim
                      (with-output-to-string
                        (with-current-buffer standard-output
                          (call-process "python3" nil t nil report-futon-bug-collector
                                        "--summary" summary
                                        "--emacs-context" ctx-file))))))
      (delete-file ctx-file))
    (unless (and path (file-exists-p path))
      (error "report-futon-bug: collector failed: %s" path))
    (find-file-other-window path)
    (if agent
        (message "report-futon-bug: %s; belled %s (%s)" path agent
                 (report-futon-bug--bell agent path))
      (message "report-futon-bug: wrote %s (not sent)" path))
    path))

(provide 'report-futon-bug)
;;; report-futon-bug.el ends here
