;;; codex-repl.el --- Chat with Codex via codex exec -*- lexical-binding: t; -*-

;; Author: Codex + Joe
;; Description: Emacs chat buffer backed by `codex exec --json`.
;; Synchronous: type, RET, Codex responds. No polling.
;; Session continuity via Codex thread id + `codex exec resume <id>`.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/codex-repl.el")
;;   M-x codex-repl

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;; Configuration

(defgroup codex-repl nil
  "Chat with Codex via CLI."
  :group 'communication)

(defcustom codex-repl-codex-command
  (or (executable-find "codex") "codex")
  "Path to codex CLI."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-session-file "/tmp/futon-codex-session-id"
  "File storing Codex thread/session ID (shared with IRC codex relay)."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-session-id nil
  "Current Codex thread ID for resume.
If nil, no resume is used until Codex returns thread.started."
  :type '(choice (const nil) string)
  :group 'codex-repl)

(defcustom codex-repl-sandbox "workspace-write"
  "Sandbox mode passed to `codex exec --sandbox`."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-approval-policy "never"
  "Approval policy passed as `-c approval_policy=\"...\"`."
  :type 'string
  :group 'codex-repl)

(defcustom codex-repl-model "gpt-5-codex"
  "Model passed to `codex exec --model`.
Set to nil to use the Codex CLI/user config default."
  :type '(choice (const nil) string)
  :group 'codex-repl)

;;; Faces

(defface codex-repl-joe-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for joe."
  :group 'codex-repl)

(defface codex-repl-codex-face
  '((t :foreground "#ff79c6" :weight bold))
  "Face for codex."
  :group 'codex-repl)

(defface codex-repl-text-face
  '((t :foreground "#f8f8f2"))
  "Face for message text."
  :group 'codex-repl)

(defface codex-repl-prompt-face
  '((t :foreground "#ffb86c" :weight bold))
  "Face for input prompt."
  :group 'codex-repl)

(defface codex-repl-thinking-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for thinking indicator."
  :group 'codex-repl)

;;; Internal state

(defvar codex-repl--buffer-name "*codex-repl*")
(defvar-local codex-repl--prompt-marker nil)
(defvar-local codex-repl--input-start nil)
(defvar-local codex-repl--separator-start nil
  "Marker at start of the separator line above prompt.")

(defun codex-repl--refresh-session-header (&optional buffer)
  "Refresh session text in BUFFER header (defaults to current buffer)."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'codex-repl-mode)
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (when (re-search-forward "(session: [^)]+)" (line-end-position 2) t)
                (replace-match (format "(session: %s)"
                                       (or codex-repl-session-id "pending"))
                               t t))
              (goto-char (point-min))
              (when (re-search-forward "emacs-codex-repl (active, session [^)]+)" nil t)
                (replace-match (format "emacs-codex-repl (active, session %s)"
                                       (or codex-repl-session-id "pending"))
                               t t)))))))))

(defun codex-repl--persist-session-id! (sid)
  "Persist SID to `codex-repl-session-file` and local state."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (setq codex-repl-session-id sid)
    (codex-repl--refresh-session-header (get-buffer codex-repl--buffer-name))
    (when codex-repl-session-file
      (write-region sid nil codex-repl-session-file nil 'silent))))

(defun codex-repl--ensure-session-id ()
  "Load existing Codex session id from file, if present."
  (unless codex-repl-session-id
    (when (and codex-repl-session-file
               (file-exists-p codex-repl-session-file))
      (let ((sid (string-trim (with-temp-buffer
                                (insert-file-contents codex-repl-session-file)
                                (buffer-string)))))
        (unless (string-empty-p sid)
          (setq codex-repl-session-id sid))))))

(defun codex-repl--extract-agent-text (item)
  "Extract assistant text from a Codex item.completed payload ITEM."
  (let ((content (or (alist-get 'text item)
                     (alist-get 'content item))))
    (cond
      ((stringp content)
       content)
      ((listp content)
       (string-join
        (delq nil
              (mapcar (lambda (part)
                        (when (and (listp part)
                                   (string= (alist-get 'type part) "text"))
                          (alist-get 'text part)))
                      content))
        ""))
      (t nil))))

(defun codex-repl--parse-codex-json-output (output)
  "Parse OUTPUT JSONL from `codex exec --json`.
Returns plist: (:session-id sid :text response :error err)."
  (let ((sid codex-repl-session-id)
        (last-message nil)
        (last-error nil))
    (dolist (line (split-string output "\n" t))
      (condition-case _
          (let* ((evt (json-parse-string line
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil))
                 (type (alist-get 'type evt)))
            (cond
              ((string= type "thread.started")
               (let ((thread-id (or (alist-get 'thread_id evt)
                                    (alist-get 'session_id evt))))
                 (when (and (stringp thread-id) (not (string-empty-p thread-id)))
                   (setq sid thread-id))))
              ((string= type "error")
               (let ((msg (alist-get 'message evt)))
                 (when (and (stringp msg) (not (string-empty-p msg)))
                   (setq last-error msg))))
              ((string= type "turn.failed")
               (let* ((err (alist-get 'error evt))
                      (msg (and (listp err) (alist-get 'message err))))
                 (when (and (stringp msg) (not (string-empty-p msg)))
                   (setq last-error msg))))
              ((string= type "item.completed")
               (let* ((item (alist-get 'item evt))
                      (item-type (and (listp item) (alist-get 'type item))))
                 (when (and (stringp item-type) (string= item-type "agent_message"))
                   (let ((msg (codex-repl--extract-agent-text item)))
                     (when (and (stringp msg) (not (string-empty-p msg)))
                       (setq last-message msg))))))))
        (error nil)))
    (list :session-id sid
          :text (or last-message
                    (string-trim output)
                    "[No assistant message returned]")
          :error last-error)))

(defun codex-repl--build-codex-args (session-id)
  "Build argv for `codex exec --json` using SESSION-ID when present."
  (let* ((exec-args (list "exec"
                          "--json"
                          "--sandbox" codex-repl-sandbox
                          "-c" (format "approval_policy=\"%s\"" codex-repl-approval-policy)))
         (exec-args (if (and codex-repl-model (not (string-empty-p codex-repl-model)))
                        (append exec-args (list "--model" codex-repl-model))
                      exec-args)))
    ;; `resume` is a subcommand of `exec`, so all exec opts must come first.
    (if (and session-id (not (string-empty-p session-id)))
        (append exec-args (list "resume" session-id "-"))
      (append exec-args (list "-")))))

(defun codex-repl--call-codex (text)
  "Call `codex exec --json` with TEXT. Return response string."
  (let ((args (codex-repl--build-codex-args codex-repl-session-id))
        (process-environment process-environment))
    (with-temp-buffer
      (insert text)
      (unless (string-suffix-p "\n" text)
        (insert "\n"))
      (let ((exit-code (apply #'call-process-region
                              (point-min) (point-max)
                              codex-repl-codex-command
                              t t nil
                              args)))
        (let* ((parsed (codex-repl--parse-codex-json-output (buffer-string)))
               (sid (plist-get parsed :session-id))
               (response (plist-get parsed :text))
               (err (plist-get parsed :error)))
          (when (and (stringp sid) (not (string-empty-p sid)))
            (codex-repl--persist-session-id! sid))
          (if (= exit-code 0)
              (string-trim response)
            (format "[Error (exit %d): %s]"
                    exit-code
                    (string-trim (or err response)))))))))

;;; Display

(defun codex-repl--insert-message (name text)
  "Insert a message above the prompt, keeping prompt pinned at window bottom."
  (let ((inhibit-read-only t)
        (face (if (string= name "codex") 'codex-repl-codex-face 'codex-repl-joe-face))
        (at-end (>= (point) (marker-position codex-repl--input-start))))
    (save-excursion
      (goto-char (marker-position codex-repl--prompt-marker))
      (insert (propertize (format "%s: " name) 'face face)
              (propertize (format "%s\n\n" text) 'face 'codex-repl-text-face)))
    (when at-end
      (goto-char (point-max))
      (codex-repl--scroll-to-bottom))))

(defun codex-repl--scroll-to-bottom ()
  "Scroll window so input prompt is pinned near the bottom."
  (when-let ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -2))))

(defun codex-repl--remove-thinking ()
  "Remove any thinking indicators from the buffer."
  (let ((inhibit-read-only t))
    ;; Fast path: exact text match.
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "codex is thinking...\n" nil t)
        (delete-region (match-beginning 0) (match-end 0))))
    ;; Fallback: remove lines marked with the dedicated thinking property.
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((next (or (next-single-property-change (point) 'codex-repl-thinking nil (point-max))
                        (point-max))))
          (if (get-text-property (point) 'codex-repl-thinking)
              (delete-region (line-beginning-position)
                             (min (point-max) (1+ (line-end-position))))
            (goto-char next)))))))

(defun codex-repl-send-input ()
  "Send input to Codex and display response."
  (interactive)
  (let ((text (buffer-substring-no-properties
               (marker-position codex-repl--input-start)
               (point-max))))
    (when (not (string-empty-p (string-trim text)))
      (let ((trimmed (string-trim text)))
        (delete-region (marker-position codex-repl--input-start) (point-max))
        (codex-repl--insert-message "joe" trimmed)
        (unwind-protect
            (progn
              (save-excursion
                (goto-char (marker-position codex-repl--prompt-marker))
                (insert (propertize "codex is thinking...\n"
                                    'face 'codex-repl-thinking-face
                                    'codex-repl-thinking t)))
              (redisplay)
              (let ((response (codex-repl--call-codex trimmed)))
                (codex-repl--remove-thinking)
                (codex-repl--insert-message "codex" response)))
          (codex-repl--remove-thinking))
        (goto-char (point-max))
        (codex-repl--scroll-to-bottom)))))

;;; Mode

(defvar codex-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-repl-send-input)
    (define-key map (kbd "C-c C-k") #'codex-repl-clear)
    map))

(define-derived-mode codex-repl-mode nil "Codex-REPL"
  "Chat with Codex via CLI.
Type after the prompt, RET to send.
\\{codex-repl-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0))

(defun codex-repl-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (erase-buffer)
  (codex-repl--init))

(defun codex-repl--irc-available-p ()
  "Check if IRC relay is running on port 6667."
  (condition-case nil
      (let ((proc (open-network-stream "irc-check" nil "127.0.0.1" 6667)))
        (delete-process proc)
        t)
    (error nil)))

(defun codex-repl--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list (format "emacs-codex-repl (active, session %s)"
                                  (or codex-repl-session-id "pending"))))
        (irc-up (codex-repl--irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (codex exec --json)" transports)
    (format "Available transports: [%s]. Current: emacs-codex-repl."
            (string-join (reverse transports) ", "))))

(defun codex-repl--init ()
  "Initialize buffer UI."
  (codex-repl--ensure-session-id)
  (insert (propertize "codex repl " 'face 'bold)
          (propertize (format "(session: %s)\n" (or codex-repl-session-id "pending"))
                      'face 'font-lock-comment-face)
          (propertize (format "  %s\n" (codex-repl--build-modeline))
                      'face 'font-lock-comment-face)
          (propertize "RET send | C-c C-k clear\n\n"
                      'face 'font-lock-comment-face))
  (setq codex-repl--prompt-marker (point-marker))
  (setq codex-repl--separator-start (point-marker))
  (insert (propertize (make-string 72 ?─) 'face 'font-lock-comment-face) "\n")
  (insert (propertize "> " 'face 'codex-repl-prompt-face))
  (setq codex-repl--input-start (point-marker))
  ;; Set type t AFTER init inserts, so marker stays before separator
  ;; but advances when messages are inserted at runtime.
  (set-marker-insertion-type codex-repl--prompt-marker t)
  (goto-char (point-max))
  (codex-repl--scroll-to-bottom))

;;;###autoload
(defun codex-repl ()
  "Start or switch to Codex REPL."
  (interactive)
  (let ((buf (get-buffer-create codex-repl--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'codex-repl-mode)
        (codex-repl-mode)
        (codex-repl--init)))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'codex-repl)
;;; codex-repl.el ends here
