;;; futon-active-agents.el --- Only the agents that are actually working -*- lexical-binding: t; -*-

;; An *active-agents* buffer, in the spirit of *agents*, filtered to agents that
;; are currently doing something.  The roster is long-lived: on this box it
;; typically holds ~130 registrations of which ~90 are `restored' (dormant) and
;; ~40 `idle', so the one agent actually working is a needle in a haystack.
;;
;; "Working" is decided from TWO sources, not one:
;;   - the agent's own `status' from /api/alpha/agents, and
;;   - whether it owns a live job (running/queued) in /api/alpha/invoke/jobs.
;;
;; Keeping both matters because they can disagree, and the disagreement is the
;; interesting case: an agent stuck at `invoking' with no live job is a stale
;; registration or a wedged seat, which is exactly the "is codex-3 healthy?"
;; question you otherwise cannot answer from here.  Such rows are flagged
;; `no-job' rather than hidden.
;;
;; M-x futon-active-agents  → open/refresh.  `g' refreshes, `RET' visits the
;; agent's REPL buffer, `a' toggles auto-refresh, `q' buries.

;;; Code:

(require 'tabulated-list)
(require 'subr-x)
(require 'seq)

(defgroup futon-active-agents nil
  "Roster view filtered to agents that are currently working."
  :group 'tools
  :prefix "futon-active-agents-")

(defcustom futon-active-agents-api-url "http://localhost:7070"
  "Base URL of the futon3c agency API."
  :type 'string)

(defcustom futon-active-agents-idle-statuses '("idle" "restored")
  "Agent statuses treated as NOT working.
Anything else counts as active.  Listing the quiet states rather than the
busy ones means a new status the server invents shows up instead of being
silently filtered away."
  :type '(repeat string))

(defcustom futon-active-agents-live-job-states '("running" "queued")
  "Job states that count as live work."
  :type '(repeat string))

(defcustom futon-active-agents-frames-root "~/code/apm-frames"
  "Directory of frame worktrees, used to name what an agent is working on.
Agents seated on a frame are named \"<frame-id>-<role>\", and the frame's
worktree is \"<frame-id>-<problem-id>-<role>\", so the problem can be
recovered from the directory listing without asking the agency."
  :type 'directory)

(defcustom futon-active-agents-machines
  '(("countdown-control" . "countdown")
    ("library-lane"      . "library"))
  "Map a job's `caller' to the cycle machine that owns it.
The countdown/learning machine and the Codex-only library lane now share
nothing that runs -- separate drivers, preparation, seats and scheduling -- but
they still share the job ledger, which is what makes it the one place both are
visible at once. Each stamps its own caller, so the ledger already carries the
distinction; this just renders it."
  :type '(alist :key-type string :value-type string))

(defcustom futon-active-agents-queued-warn-seconds 300
  "Seconds a job may sit `queued' before the Job column flags it.
A job accepted into the ledger and never drained is the worst failure mode
available -- the caller believes it dispatched. See
futon3c/holes/excursions/E-drainer-stall-announced-jobs.md."
  :type 'integer)

(defcustom futon-active-agents-refresh-seconds 5
  "Seconds between automatic refreshes, or nil for manual `g' only."
  :type '(choice (const :tag "Manual only" nil) integer))

(defvar futon-active-agents--timer nil)

(defun futon-active-agents--get-json (path)
  "GET PATH from the agency API and return parsed JSON, or nil on failure."
  (let* ((url (concat (string-remove-suffix "/" futon-active-agents-api-url) path))
         (buf (ignore-errors (url-retrieve-synchronously url t t 4))))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (search-forward "\n\n" nil t)
              (ignore-errors
                (json-parse-string (buffer-substring-no-properties (point) (point-max))
                                   :object-type 'alist :array-type 'list
                                   :null-object nil :false-object nil))))
        (kill-buffer buf)))))

(defun futon-active-agents--live-jobs ()
  "Return an alist of AGENT-ID → job for jobs in a live state."
  (let ((jobs (alist-get 'jobs (futon-active-agents--get-json "/api/alpha/invoke/jobs")))
        (out '()))
    (dolist (job jobs out)
      (when (member (alist-get 'state job) futon-active-agents-live-job-states)
        (push (cons (alist-get 'agent-id job) job) out)))))

(defun futon-active-agents--stale-queue-p (job)
  "Non-nil when JOB has been queued longer than the warn threshold."
  (and job
       (equal "queued" (alist-get 'state job))
       (let ((c (alist-get 'created-at job)))
         (and (stringp c)
              (condition-case nil
                  (> (float-time (time-since (date-to-time c)))
                     futon-active-agents-queued-warn-seconds)
                (error nil))))))

(defun futon-active-agents--doing (id)
  "Return what agent ID is working on, or \"-\".
For a frame seat, that is the problem its worktree names."
  (if (not (string-match "\\`\\(f[0-9]+\\)-" id))
      "-"
    (let* ((frame (match-string 1 id))
           (root (expand-file-name futon-active-agents-frames-root))
           (hit (car (file-expand-wildcards
                      (expand-file-name (concat frame "-*-solver") root)))))
      (if (and hit (string-match (concat (regexp-quote frame) "-\\(.+\\)-solver\\'")
                                 (file-name-nondirectory hit)))
          (match-string 1 (file-name-nondirectory hit))
        frame))))

(defun futon-active-agents--age (iso)
  "Return a compact age for ISO timestamp, or \"-\"."
  (if (not (stringp iso))
      "-"
    (condition-case nil
        (let ((secs (max 0 (floor (float-time (time-since (date-to-time iso)))))))
          (cond ((< secs 60) (format "%ds" secs))
                ((< secs 3600) (format "%dm" (/ secs 60)))
                ((< secs 86400) (format "%dh%02dm" (/ secs 3600) (/ (mod secs 3600) 60)))
                (t (format "%dd" (/ secs 86400)))))
      (error "-"))))

(defun futon-active-agents--rows ()
  "Build tabulated-list entries for agents that are working."
  (let* ((agents (alist-get 'agents (futon-active-agents--get-json "/api/alpha/agents")))
         (live (futon-active-agents--live-jobs))
         (rows '()))
    (dolist (entry agents)
      (let* ((id (symbol-name (car entry)))
             (a (cdr entry))
             (status (or (alist-get 'status a) "?"))
             (job (alist-get id live nil nil #'string=))
             (busy-status (not (member status futon-active-agents-idle-statuses))))
        (when (or busy-status job)
          (push (list id
                      (vector id
                              (or (alist-get 'type a) "?")
                              status
                              (cond ((futon-active-agents--stale-queue-p job)
                                     "queued!")
                                    (job (or (alist-get 'state job) "?"))
                                    (busy-status "no-job")
                                    (t "-"))
                              ;; Age the JOB when there is one -- falling back
                              ;; to the agent's last-active made a job queued
                              ;; ten minutes ago read as "3h05m", which is the
                              ;; seat's staleness, not the work's.
                              (futon-active-agents--age
                               (if job
                                   (or (alist-get 'started-at job)
                                       (alist-get 'created-at job))
                                 (alist-get 'last-active a)))
                              (futon-active-agents--doing id)
                              (or (and job
                                       (cdr (assoc (alist-get 'caller job)
                                                   futon-active-agents-machines)))
                                  "-")
                              (or (and job (alist-get 'caller job)) "-")))
                rows))))
    (nreverse rows)))

(defun futon-active-agents--refresh ()
  "Recompute `tabulated-list-entries'."
  (setq tabulated-list-entries (futon-active-agents--rows)))

(defun futon-active-agents-visit ()
  "Open the REPL buffer of the agent on this row, if one exists."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (type (elt (tabulated-list-get-entry) 1))
         (candidates (list (format "*%s-repl:%s*" type id)
                           (format "*invoke:%s*" id)
                           (format "*%s*" id)))
         (buf (seq-some #'get-buffer candidates)))
    (if buf (pop-to-buffer buf)
      (message "No live buffer for %s (looked for %s)" id
               (string-join candidates ", ")))))

(defun futon-active-agents--cancel-timer ()
  (when (timerp futon-active-agents--timer)
    (cancel-timer futon-active-agents--timer))
  (setq futon-active-agents--timer nil))

(defun futon-active-agents--tick ()
  "Refresh the buffer if it is still live; otherwise stop the timer."
  (let ((buf (get-buffer "*active-agents*")))
    (if (not (buffer-live-p buf))
        (futon-active-agents--cancel-timer)
      (with-current-buffer buf
        (let ((line (line-number-at-pos)))
          (futon-active-agents--refresh)
          (tabulated-list-print t)
          (goto-char (point-min))
          (forward-line (1- line)))))))

(defun futon-active-agents-toggle-auto-refresh ()
  "Start or stop automatic refresh."
  (interactive)
  (if futon-active-agents--timer
      (progn (futon-active-agents--cancel-timer) (message "auto-refresh off"))
    (setq futon-active-agents--timer
          (run-at-time t (or futon-active-agents-refresh-seconds 5)
                       #'futon-active-agents--tick))
    (message "auto-refresh every %ss" (or futon-active-agents-refresh-seconds 5))))

(defvar futon-active-agents-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'futon-active-agents-visit)
    (define-key map (kbd "a") #'futon-active-agents-toggle-auto-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `futon-active-agents-mode'.")

(define-derived-mode futon-active-agents-mode tabulated-list-mode "ActiveAgents"
  "Major mode for the *active-agents* roster view.
\\{futon-active-agents-mode-map}"
  (setq tabulated-list-format
        [("Agent" 22 t)
         ("Type" 8 t)
         ("Status" 12 t)
         ("Job" 10 t)
         ("Since" 8 nil)
         ("Doing" 14 t)
         ("Machine" 10 t)
         ("Caller" 18 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Agent" nil))
  (add-hook 'tabulated-list-revert-hook #'futon-active-agents--refresh nil t)
  (add-hook 'kill-buffer-hook #'futon-active-agents--cancel-timer nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun futon-active-agents ()
  "Show only the agents currently working, in *active-agents*.
Refresh with `g'; `a' toggles auto-refresh."
  (interactive)
  (let ((buf (get-buffer-create "*active-agents*")))
    (with-current-buffer buf
      (futon-active-agents-mode)
      (futon-active-agents--refresh)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(provide 'futon-active-agents)
;;; futon-active-agents.el ends here
