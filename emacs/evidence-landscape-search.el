;;; evidence-landscape-search.el --- Search the Evidence Landscape -*- lexical-binding: t; -*-

;;; Commentary:

;; An operator-facing, asynchronous client for futon1b's authoritative
;; full-text evidence index.  `my/evidence-landscape-search' searches the last
;; 24 hours by default; a prefix argument searches the whole index.

;;; Code:

(require 'button)
(require 'map)
(require 'parseedn)
(require 'subr-x)
(require 'url-util)
(require 'futon-url)

(defgroup evidence-landscape-search nil
  "Interactive search of the Futon Evidence Landscape."
  :group 'tools)

(defcustom evidence-landscape-search-url
  "http://127.0.0.1:7073/api/alpha/evidence/text-search"
  "URL of the authoritative Evidence Landscape full-text search endpoint."
  :type 'string)

(defcustom evidence-landscape-search-limit 50
  "Maximum number of search results to request."
  :type 'integer)

(defcustom evidence-landscape-search-timeout 90
  "Seconds before an Evidence Landscape search is cancelled."
  :type 'integer)

(defvar evidence-landscape-search-history nil)

(defvar-local evidence-landscape-search--query nil)
(defvar-local evidence-landscape-search--all-time nil)

(define-derived-mode evidence-landscape-search-mode special-mode "Evidence Search"
  "Major mode for Evidence Landscape search results."
  (setq-local truncate-lines nil))

(defun evidence-landscape-search--since ()
  "Return an ISO-8601 timestamp exactly 24 hours ago."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (current-time) (days-to-time 1)) t))

(defun evidence-landscape-search--url (query all-time)
  "Build the search URL for QUERY, omitting the time bound when ALL-TIME."
  (concat evidence-landscape-search-url
          "?q=" (url-hexify-string query)
          "&limit=" (number-to-string evidence-landscape-search-limit)
          (unless all-time
            (concat "&since=" (url-hexify-string
                                (evidence-landscape-search--since))))))

(defun evidence-landscape-search--body-text (body)
  "Return a useful display string for evidence BODY."
  (cond
   ((stringp body) body)
   ((hash-table-p body)
    (or (gethash :text body)
        (gethash :message body)
        (gethash :summary body)
        (prin1-to-string body)))
   (t (prin1-to-string body))))

(defun evidence-landscape-search--insert-result (result)
  "Insert one parsed evidence search RESULT in the current buffer."
  (let* ((entry (gethash :entry result))
         (at (gethash :evidence/at entry "?"))
         (author (gethash :evidence/author entry "?"))
         (type (gethash :evidence/type entry))
         (session (gethash :evidence/session-id entry))
         (id (gethash :evidence/id entry "?"))
         (text (evidence-landscape-search--body-text
                (gethash :evidence/body entry))))
    (insert (propertize (format "%s  %s  %s\n" at author (or type ""))
                        'face 'compilation-info))
    (when session
      (insert (format "session: %s\n" session)))
    (insert (format "evidence: %s\n%s\n\n" id text))))

(defun evidence-landscape-search--render (query all-time payload)
  "Render PAYLOAD for QUERY and ALL-TIME into the results buffer."
  (let ((results (gethash :results payload))
        (buffer (get-buffer-create "*Evidence Landscape Search*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (evidence-landscape-search-mode)
        (setq evidence-landscape-search--query query
              evidence-landscape-search--all-time all-time)
        (insert (propertize
                 (format "Evidence Landscape: %S (%s)\n\n"
                         query (if all-time "all time" "last 24 hours"))
                 'face 'bold))
        (if (and (vectorp results) (> (length results) 0))
            (mapc #'evidence-landscape-search--insert-result results)
          (insert "No matching evidence.\n"))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun evidence-landscape-search--response (status query all-time)
  "Handle an asynchronous search response with STATUS for QUERY and ALL-TIME."
  (let ((response-buffer (current-buffer)))
    (unwind-protect
        (if-let* ((error-value (plist-get status :error)))
            (message "Evidence Landscape search failed: %s" error-value)
          (let ((http-status (or (bound-and-true-p url-http-response-status) 0)))
            (goto-char (point-min))
            (if (not (re-search-forward "\r?\n\r?\n" nil t))
                (message "Evidence Landscape returned an invalid HTTP response")
              (condition-case err
                  (let ((payload (parseedn-read-str
                                  (buffer-substring-no-properties
                                   (point) (point-max)))))
                    (if (and (= http-status 200) (gethash :ok payload))
                        (evidence-landscape-search--render query all-time payload)
                      (message "Evidence Landscape search returned HTTP %s: %s"
                               http-status (gethash :error payload "unknown error"))))
                (error (message "Could not read Evidence Landscape response: %s"
                                (error-message-string err)))))))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer)))))

;;;###autoload
(defun my/evidence-landscape-search (query &optional all-time)
  "Search Evidence Landscape for QUERY without blocking Emacs.

By default search evidence from the last 24 hours.  With prefix argument
ALL-TIME (interactively, `C-u'), omit the time bound and search the complete
index."
  (interactive
   (list (read-string "Evidence Landscape search: "
                      (thing-at-point 'symbol)
                      'evidence-landscape-search-history)
         current-prefix-arg))
  (when (string-empty-p (string-trim query))
    (user-error "Search text must not be empty"))
  (message "Searching Evidence Landscape for %S%s..."
           query (if all-time " (all time)" " (last 24 hours)"))
  (futon-url-retrieve
   (evidence-landscape-search--url query all-time)
   evidence-landscape-search-timeout
   (lambda (status)
     (evidence-landscape-search--response status query all-time))))

(provide 'evidence-landscape-search)

;;; evidence-landscape-search.el ends here
