;;; claude-repl-jobs.el --- Async parked/job visibility for Claude REPL -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)

(defgroup claude-repl-jobs nil
  "Operator visibility for parked continuations and invoke jobs."
  :group 'agent-chat)

(defcustom claude-repl-jobs-default-agent "claude-6"
  "Default agent shown by `claude-repl-jobs'."
  :type 'string
  :group 'claude-repl-jobs)

(defcustom claude-repl-jobs-api-url "http://localhost:7070"
  "Fallback futon3c API URL used outside a Claude REPL buffer."
  :type 'string
  :group 'claude-repl-jobs)

(defvar claude-repl-jobs--last-agent claude-repl-jobs-default-agent)
(defvar claude-repl-jobs--last-api-url nil)

(defvar claude-repl-jobs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'claude-repl-jobs-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `claude-repl-jobs-mode'.")

(define-derived-mode claude-repl-jobs-mode special-mode "Claude-Jobs"
  "Mode for the *claude-jobs* operator visibility buffer.")

(defun claude-repl-jobs--api-url ()
  "Return the current API URL without assuming a REPL buffer is current."
  (string-remove-suffix
   "/"
   (or (and (boundp 'claude-repl-api-url)
            (stringp claude-repl-api-url)
            claude-repl-api-url)
       claude-repl-jobs--last-api-url
       claude-repl-jobs-api-url)))

(defun claude-repl-jobs--body-json ()
  "Parse the current `url-retrieve' response buffer as plist JSON."
  (goto-char (point-min))
  (when (search-forward "\n\n" nil t)
    (json-parse-string (buffer-substring-no-properties (point) (point-max))
                       :object-type 'plist
                       :array-type 'vector
                       :null-object nil
                       :false-object nil)))

(defun claude-repl-jobs--get-json-async (url callback)
  "Fetch URL asynchronously and call CALLBACK with (ERROR JSON)."
  (let ((url-request-method "GET"))
    (url-retrieve
     url
     (lambda (status)
       (let ((buf (current-buffer)))
         (unwind-protect
             (if-let ((err (plist-get status :error)))
                 (funcall callback err nil)
               (condition-case e
                   (funcall callback nil (claude-repl-jobs--body-json))
                 (error (funcall callback e nil))))
           (when (buffer-live-p buf)
             (kill-buffer buf)))))
     nil t t)))

(defun claude-repl-jobs--short-id (s)
  "Shorten identifier S for compact tabular display."
  (let ((s (or s "")))
    (if (> (length s) 28)
        (concat (substring s 0 24) "...")
      s)))

(defun claude-repl-jobs--join-vector (v)
  "Return a compact string for vector V."
  (cond
   ((not (vectorp v)) "")
   ((= 0 (length v)) "")
   ((= 1 (length v)) (claude-repl-jobs--short-id (aref v 0)))
   (t (format "%s +%d"
              (claude-repl-jobs--short-id (aref v 0))
              (1- (length v))))))

(defun claude-repl-jobs--duration (seconds)
  "Format SECONDS as a compact duration."
  (let* ((negative (< seconds 0))
         (seconds (abs (floor seconds)))
         (hours (/ seconds 3600))
         (minutes (/ (% seconds 3600) 60))
         (secs (% seconds 60))
         (text (cond
                ((> hours 0) (format "%dh%02dm" hours minutes))
                ((> minutes 0) (format "%dm%02ds" minutes secs))
                (t (format "%ds" secs)))))
    (if negative (concat "-" text) text)))

(defun claude-repl-jobs--deadline-countdown (deadline-ms)
  "Return time until DEADLINE-MS, or elapsed marker when expired."
  (if (numberp deadline-ms)
      (let ((delta (- (/ deadline-ms 1000.0) (float-time))))
        (if (< delta 0)
            (concat "expired " (claude-repl-jobs--duration delta))
          (claude-repl-jobs--duration delta)))
    ""))

(defun claude-repl-jobs--parse-time (s)
  "Parse ISO timestamp S into float seconds, or nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (ignore-errors (float-time (date-to-time s)))))

(defun claude-repl-jobs--age (job)
  "Return compact age for JOB."
  (let* ((started (or (plist-get job :started-at)
                      (plist-get job :created-at)))
         (then (claude-repl-jobs--parse-time started)))
    (if then
        (claude-repl-jobs--duration (- (float-time) then))
      "")))

(defun claude-repl-jobs--job-active-p (job)
  "Return non-nil when JOB is not terminal."
  (member (plist-get job :state) '("accepted" "queued" "running")))

(defun claude-repl-jobs--jobs-for-agent (agent jobs-json)
  "Return jobs from JOBS-JSON that were called by AGENT."
  (let ((jobs (plist-get jobs-json :jobs)))
    (if (vectorp jobs)
        (cl-loop for job across jobs
                 when (or (string= (or (plist-get job :caller) "") agent)
                          (and (claude-repl-jobs--job-active-p job)
                               (string= (or (plist-get job :agent-id) "") agent)))
                 collect job)
      nil)))

(defun claude-repl-jobs--insert-section-title (title)
  "Insert TITLE with a small separator."
  (insert title "\n")
  (insert (make-string (length title) ?-) "\n"))

(defun claude-repl-jobs--render (agent parks-json jobs-json)
  "Render AGENT's PARKS-JSON and JOBS-JSON into the jobs buffer."
  (let ((buf (get-buffer-create "*claude-jobs*"))
        (parks (plist-get parks-json :parked))
        (jobs (claude-repl-jobs--jobs-for-agent agent jobs-json)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (claude-repl-jobs-mode)
        (setq-local claude-repl-jobs--last-agent agent)
        (insert (format "Agent: %s    refreshed: %s\n\n"
                        agent
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        (claude-repl-jobs--insert-section-title "Parks")
        (insert (format "%-31s %-31s %s\n" "id" "awaiting" "deadline"))
        (if (and (vectorp parks) (> (length parks) 0))
            (cl-loop for park across parks
                     do (insert
                         (format "%-31s %-31s %s\n"
                                 (claude-repl-jobs--short-id (plist-get park :id))
                                 (claude-repl-jobs--join-vector
                                  (plist-get park :awaiting))
                                 (claude-repl-jobs--deadline-countdown
                                  (plist-get park :deadline-ms)))))
          (insert "(none)\n"))
        (insert "\n")
        (claude-repl-jobs--insert-section-title "Jobs")
        (insert (format "%-31s %-16s %-10s %s\n" "id" "recipient" "state" "age"))
        (if jobs
            (dolist (job jobs)
              (insert
               (format "%-31s %-16s %-10s %s\n"
                       (claude-repl-jobs--short-id (plist-get job :job-id))
                       (or (plist-get job :agent-id) "")
                       (or (plist-get job :state) "")
                       (claude-repl-jobs--age job))))
          (insert "(none)\n"))
        (goto-char (point-min))))
    (display-buffer buf '((display-buffer-reuse-window
                           display-buffer-pop-up-window)
                          (inhibit-same-window . t)))))

(defun claude-repl-jobs--render-error (agent error)
  "Render ERROR for AGENT in the jobs buffer."
  (let ((buf (get-buffer-create "*claude-jobs*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (claude-repl-jobs-mode)
        (setq-local claude-repl-jobs--last-agent agent)
        (insert (format "Agent: %s\n\nFetch failed: %S\n" agent error))))
    (display-buffer buf '((display-buffer-reuse-window
                           display-buffer-pop-up-window)
                          (inhibit-same-window . t)))))

(defun claude-repl-jobs-fetch (agent)
  "Fetch parks and invoke jobs for AGENT asynchronously."
  (let* ((api (claude-repl-jobs--api-url))
         (agent-q (url-hexify-string agent))
         (parks-url (format "%s/api/alpha/parked?agent=%s" api agent-q))
         (jobs-url (format "%s/api/alpha/invoke/jobs?limit=100" api)))
    (setq claude-repl-jobs--last-agent agent
          claude-repl-jobs--last-api-url api)
    (claude-repl-jobs--get-json-async
     parks-url
     (lambda (parks-error parks-json)
       (if parks-error
           (claude-repl-jobs--render-error agent parks-error)
         (claude-repl-jobs--get-json-async
          jobs-url
          (lambda (jobs-error jobs-json)
            (if jobs-error
                (claude-repl-jobs--render-error agent jobs-error)
              (claude-repl-jobs--render agent parks-json jobs-json)))))))))

(defun claude-repl-jobs-refresh ()
  "Refresh the current `*claude-jobs*' buffer."
  (interactive)
  (claude-repl-jobs-fetch
   (or (and (boundp 'claude-repl-jobs--last-agent)
            claude-repl-jobs--last-agent)
       claude-repl-jobs-default-agent)))

;;;###autoload
(defun claude-repl-jobs (&optional agent)
  "Show parked continuations and invoke jobs for AGENT.
Display the jobs buffer without selecting it.
With prefix argument, prompt for AGENT."
  (interactive
   (list (if current-prefix-arg
             (read-string "Agent: " claude-repl-jobs-default-agent)
           claude-repl-jobs-default-agent)))
  (claude-repl-jobs-fetch (or agent claude-repl-jobs-default-agent)))

(provide 'claude-repl-jobs)
;;; claude-repl-jobs.el ends here
