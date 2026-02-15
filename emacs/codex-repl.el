;;; codex-repl.el --- Chat with Codex via codex exec -*- lexical-binding: t; -*-

;; Author: Codex + Joe
;; Description: Emacs chat buffer backed by `codex exec --json`.
;; Asynchronous: type, RET, Codex responds without blocking Emacs UI.
;; Session continuity via Codex thread id + `codex exec resume <id>`.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/futon3c-ui.el")
;;   (load "/home/joe/code/futon3c/emacs/codex-repl.el")
;;   M-x codex-repl

(require 'cl-lib)
(require 'futon3c-ui)

;;; Configuration

(defgroup codex-repl nil
  "Chat with Codex via CLI."
  :group 'futon3c-ui)

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

;;; Face (Codex-specific; shared faces are in futon3c-ui)

(defface codex-repl-codex-face
  '((t :foreground "#ff79c6" :weight bold))
  "Face for codex."
  :group 'codex-repl)

;;; Internal state

(defvar codex-repl--buffer-name "*codex-repl*")

;;; Streaming event parser (for progress display)

(defun codex-repl--parse-stream-event (json-line)
  "Parse a Codex JSONL event and return a progress string or nil."
  (condition-case nil
      (let* ((evt (json-parse-string json-line
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
             (type (alist-get 'type evt)))
        (cond
         ((string= type "thread.started")
          "thread started")
         ((string= type "item.completed")
          (let* ((item (alist-get 'item evt))
                 (item-type (and (listp item) (alist-get 'type item))))
            (cond
             ((and (stringp item-type) (string= item-type "tool_call"))
              (let ((name (alist-get 'name item)))
                (if (stringp name)
                    (format "ran: %s" name)
                  "tool call completed")))
             ((and (stringp item-type) (string= item-type "agent_message"))
              "composing response...")
             (t nil))))
         ((string= type "turn.failed")
          (let* ((err (alist-get 'error evt))
                 (msg (and (listp err) (alist-get 'message err))))
            (when (stringp msg)
              (format "error: %s" (truncate-string-to-width msg 60)))))
         ((string= type "error")
          (let ((msg (alist-get 'message evt)))
            (when (stringp msg)
              (format "error: %s" (truncate-string-to-width msg 60)))))
         (t nil)))
    (error nil)))

;;; Session

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
  (futon3c-ui-ensure-session-id
   codex-repl-session-file
   codex-repl-session-id
   (lambda (sid) (setq codex-repl-session-id sid))))

;;; Codex output parsing

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

;;; Codex CLI call

(defun codex-repl--build-codex-args (session-id)
  "Build argv for `codex exec --json` using SESSION-ID when present."
  (let* ((exec-args (list "exec"
                          "--json"
                          "--sandbox" codex-repl-sandbox
                          "-c" (format "approval_policy=\"%s\"" codex-repl-approval-policy)))
         (exec-args (if (and codex-repl-model (not (string-empty-p codex-repl-model)))
                        (append exec-args (list "--model" codex-repl-model))
                      exec-args)))
    (if (and session-id (not (string-empty-p session-id)))
        (append exec-args (list "resume" session-id "-"))
      (append exec-args (list "-")))))

(defun codex-repl--call-codex-async (text callback)
  "Call `codex exec --json` with TEXT asynchronously.
Invoke CALLBACK with the final response text."
  (let* ((args (codex-repl--build-codex-args codex-repl-session-id))
         (repl-buffer (current-buffer))
         (outbuf (generate-new-buffer " *codex-repl-codex*"))
         (process-environment process-environment)
         (proc nil)
         (payload (if (string-suffix-p "\n" text) text (concat text "\n"))))
    (setq proc
          (make-process
           :name "codex-repl-codex"
           :buffer outbuf
           :command (cons codex-repl-codex-command args)
           :noquery t
           :connection-type 'pipe
           :filter (futon3c-ui-make-streaming-filter
                    #'codex-repl--parse-stream-event
                    repl-buffer)
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let* ((exit-code (process-exit-status p))
                      (raw (with-current-buffer (process-buffer p)
                             (buffer-string)))
                      (parsed (codex-repl--parse-codex-json-output raw))
                      (sid (plist-get parsed :session-id))
                      (response (plist-get parsed :text))
                      (err (plist-get parsed :error))
                      (final-text (if (= exit-code 0)
                                      (string-trim response)
                                    (format "[Error (exit %d): %s]"
                                            exit-code
                                            (string-trim (or err response))))))
                 (when (and (stringp sid) (not (string-empty-p sid)))
                   (codex-repl--persist-session-id! sid))
                 (when (buffer-live-p (process-buffer p))
                   (kill-buffer (process-buffer p)))
                 (when (buffer-live-p repl-buffer)
                   (with-current-buffer repl-buffer
                     (when (eq futon3c-ui--pending-process p)
                       (setq futon3c-ui--pending-process nil))
                     (funcall callback final-text))))))))
    (process-send-string proc payload)
    (process-send-eof proc)
    proc))

;;; Modeline

(defun codex-repl--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list (format "emacs-codex-repl (active, session %s)"
                                  (or codex-repl-session-id "pending"))))
        (irc-up (futon3c-ui-irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (codex exec --json)" transports)
    (format "Available transports: [%s]. Current: emacs-codex-repl."
            (string-join (reverse transports) ", "))))

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

(defun codex-repl-send-input ()
  "Send input to Codex and display response."
  (interactive)
  (futon3c-ui-send-input #'codex-repl--call-codex-async "codex"))

(defun codex-repl-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (futon3c-ui-clear #'codex-repl--init))

(defun codex-repl--init ()
  "Initialize buffer UI."
  (codex-repl--ensure-session-id)
  (futon3c-ui-init-buffer
   (list :title "codex repl"
         :session-id (or codex-repl-session-id "pending")
         :modeline-fn #'codex-repl--build-modeline
         :face-alist `(("codex" . codex-repl-codex-face))
         :agent-name "codex"
         :thinking-text "codex is thinking..."
         :thinking-prop 'codex-repl-thinking)))

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
