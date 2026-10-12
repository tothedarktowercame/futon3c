;;; xiang-brief.el --- Insert a mission resume brief -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)

(defconst xiang-brief--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the futon3c checkout containing this library.")

(defun xiang-brief--seat-from-buffer-name (&optional name)
  "Return the seat encoded by repl buffer NAME, or nil."
  (let ((value (or name (buffer-name))))
    (when (string-match
           (rx string-start "*" (or "claude" "codex") "-repl:"
               (group (+ (not "*"))) "*" string-end)
           value)
      (match-string 1 value))))

(defun xiang-brief--roster-mission (seat)
  "Read SEAT's mission id from the local Agency roster."
  (with-temp-buffer
    (when (= 0 (call-process "curl" nil t nil "-sS" "--max-time" "5"
                             "http://127.0.0.1:7070/api/alpha/agents"))
      (goto-char (point-min))
      (let* ((payload (json-parse-buffer :object-type 'alist :null-object nil))
             (agents (alist-get 'agents payload))
             (agent (alist-get (intern seat) agents))
             (mission (alist-get 'mission-id agent)))
        ;; A seat with no mission answers JSON null; only a string is a default.
        (and (stringp mission) (not (string-empty-p mission)) mission)))))

(defun xiang-brief--script ()
  (expand-file-name "../holes/labs/M-象-2000/brief/resume_brief.py"
                    (expand-file-name "emacs/" xiang-brief--root)))

;;;###autoload
(defun xiang-resume-brief (mission)
  "Insert the classical resume brief for MISSION at point."
  (interactive
   (let* ((seat (or (xiang-brief--seat-from-buffer-name)
                    (user-error "Not in a Claude or Codex REPL buffer")))
          (default (xiang-brief--roster-mission seat)))
     (list (if default
               (read-string "Mission: " default nil default)
             (read-string "Mission (seat has none on the roster): " "M-象-2000")))))
  (let ((seat (or (xiang-brief--seat-from-buffer-name)
                  (user-error "Not in a Claude or Codex REPL buffer"))))
    (unless (and mission (not (string-empty-p mission)))
      (user-error "Mission is required"))
    (let ((status (call-process "python3" nil t nil "-I" (xiang-brief--script)
                                "--mission" mission "--seat" seat)))
      (unless (= status 0)
        (user-error "resume_brief.py failed (%s)" status)))))

(provide 'xiang-brief)
;;; xiang-brief.el ends here
