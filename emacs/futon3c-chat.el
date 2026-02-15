;;; futon3c-chat.el --- Chat with Claude via claude CLI -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs chat buffer backed by `claude -p`.
;; Asynchronous: type, RET, Claude responds without blocking Emacs UI.
;; Session continuity via --session-id and --resume.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/futon3c-ui.el")
;;   (load "/home/joe/code/futon3c/emacs/futon3c-chat.el")
;;   M-x futon3c-chat

(require 'cl-lib)
(require 'futon3c-ui)

;;; Configuration

(defgroup futon3c-chat nil
  "Chat with Claude via CLI."
  :group 'futon3c-ui)

(defcustom futon3c-chat-claude-command
  (or (executable-find "claude") "/home/joe/.local/bin/claude")
  "Path to claude CLI."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-session-file "/tmp/futon-session-id"
  "File storing the shared session ID (read by IRC relay too)."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-session-id nil
  "Session ID for conversation continuity.
If nil, one is generated and written to `futon3c-chat-session-file'."
  :type '(choice (const nil) string)
  :group 'futon3c-chat)

(defcustom futon3c-chat-model nil
  "Model override (nil uses claude default)."
  :type '(choice (const nil) string)
  :group 'futon3c-chat)

;;; Face (Claude-specific; shared faces are in futon3c-ui)

(defface futon3c-chat-claude-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Claude."
  :group 'futon3c-chat)

;;; Internal state

(defvar futon3c-chat--buffer-name "*futon3c-chat*")
(defvar-local futon3c-chat--continued nil
  "Non-nil after first exchange (use --resume for subsequent).")

;;; Stream-json event parsing

(defun futon3c-chat--parse-stream-event (json-line)
  "Parse a stream-json event line and return a progress string or nil."
  (condition-case nil
      (let* ((evt (json-parse-string json-line
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
             (type (alist-get 'type evt)))
        (cond
         ;; assistant message with thinking or text content
         ((string= type "assistant")
          (let* ((msg (alist-get 'message evt))
                 (content (and msg (alist-get 'content msg))))
            (when (listp content)
              (cl-loop for block in content
                       when (and (listp block)
                                 (string= (alist-get 'type block) "thinking"))
                       return (let ((text (alist-get 'thinking block)))
                                (when (and (stringp text) (> (length text) 0))
                                  (format "thinking: %s"
                                          (truncate-string-to-width
                                           (replace-regexp-in-string "[\n\r]+" " " text)
                                           70))))))))
         ;; tool use
         ((string= type "tool_use")
          (let* ((tool (or (alist-get 'tool evt) evt))
                 (name (alist-get 'name tool)))
            (when (stringp name)
              (format "using: %s" name))))
         ;; tool result
         ((string= type "tool_result")
          "tool result received")
         (t nil)))
    (error nil)))

(defun futon3c-chat--extract-stream-result (raw)
  "Extract final response text from stream-json output RAW.
Looks for the last `result' event; falls back to concatenating
assistant text blocks."
  (let ((result nil)
        (text-parts nil))
    (dolist (line (split-string raw "\n" t))
      (condition-case nil
          (let* ((evt (json-parse-string line
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil))
                 (type (alist-get 'type evt)))
            (cond
             ((string= type "result")
              (let ((r (alist-get 'result evt)))
                (when (stringp r)
                  (setq result r))))
             ((string= type "assistant")
              (let* ((msg (alist-get 'message evt))
                     (content (and msg (alist-get 'content msg))))
                (when (listp content)
                  (dolist (block content)
                    (when (and (listp block)
                               (string= (alist-get 'type block) "text"))
                      (let ((txt (alist-get 'text block)))
                        (when (stringp txt)
                          (push txt text-parts))))))))))
        (error nil)))
    (or result
        (when text-parts
          (string-join (nreverse text-parts) ""))
        (string-trim raw))))

;;; Claude CLI call

(defun futon3c-chat--build-claude-args (text)
  "Build argv for `claude` with TEXT."
  (let* ((args (list "-p" text
                     "--permission-mode" "bypassPermissions"))
         (args (if futon3c-chat--continued
                   (append args (list "--resume" futon3c-chat-session-id))
                 (if futon3c-chat-session-id
                     (append args (list "--session-id" futon3c-chat-session-id))
                   args)))
         (args (if futon3c-chat-model
                   (append args (list "--model" futon3c-chat-model))
                 args))
         (args (append args (list "--output-format" "stream-json")))
         (args (append args (list "--append-system-prompt"
                                  (concat "This conversation is via the futon3c Emacs "
                                          "chat interface (futon3c-chat.el). "
                                          "The user is Joe. Transport: emacs-chat. "
                                          "Responses display in an Emacs buffer. "
                                          (futon3c-chat--build-modeline))))))
    args))

(defun futon3c-chat--call-claude-async (text callback)
  "Call Claude with TEXT and invoke CALLBACK with response string.
Uses stream-json output with a process filter for live progress."
  (let* ((args (futon3c-chat--build-claude-args text))
         (chat-buffer (current-buffer))
         (outbuf (generate-new-buffer " *futon3c-chat-claude*"))
         (process-environment
          (cl-remove-if (lambda (s)
                          (or (string-prefix-p "CLAUDECODE=" s)
                              (string-prefix-p "CLAUDE_CODE_ENTRYPOINT=" s)))
                        process-environment))
         (proc nil))
    (setq proc
          (make-process
           :name "futon3c-chat-claude"
           :buffer outbuf
           :command (cons futon3c-chat-claude-command args)
           :noquery t
           :connection-type 'pipe
           :filter (futon3c-ui-make-streaming-filter
                    #'futon3c-chat--parse-stream-event
                    chat-buffer)
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let* ((exit-code (process-exit-status p))
                      (raw (with-current-buffer (process-buffer p)
                             (buffer-string)))
                      (response (if (= exit-code 0)
                                    (string-trim
                                     (futon3c-chat--extract-stream-result raw))
                                  (format "[Error (exit %d): %s]"
                                          exit-code
                                          (string-trim raw)))))
                 (when (buffer-live-p (process-buffer p))
                   (kill-buffer (process-buffer p)))
                 (when (buffer-live-p chat-buffer)
                   (with-current-buffer chat-buffer
                     (when (eq futon3c-ui--pending-process p)
                       (setq futon3c-ui--pending-process nil))
                     (when (= exit-code 0)
                       (setq futon3c-chat--continued t))
                     (funcall callback response))))))))
    proc))

;;; Session

(defun futon3c-chat--ensure-session-id ()
  "Ensure a session ID exists. Generate and persist if needed."
  (futon3c-ui-ensure-session-id
   futon3c-chat-session-file
   futon3c-chat-session-id
   (lambda (sid) (setq futon3c-chat-session-id sid)))
  ;; If still nil, generate a new UUID
  (unless futon3c-chat-session-id
    (let ((uuid (format "%08x-%04x-%04x-%04x-%012x"
                        (random (expt 16 8))
                        (random (expt 16 4))
                        (logior #x4000 (random #x0fff))
                        (logior #x8000 (random #x3fff))
                        (random (expt 16 12)))))
      (setq futon3c-chat-session-id uuid)
      (when futon3c-chat-session-file
        (write-region uuid nil futon3c-chat-session-file nil 'silent)))))

;;; Modeline

(defun futon3c-chat--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list (format "emacs-chat (active, session %s)"
                                  (or futon3c-chat-session-id "unknown"))))
        (irc-up (futon3c-ui-irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (claude code)" transports)
    (format "Available transports: [%s]. Current: emacs-chat."
            (string-join (reverse transports) ", "))))

;;; Mode

(defvar futon3c-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'futon3c-chat-send-input)
    (define-key map (kbd "C-c C-k") #'futon3c-chat-clear)
    map))

(define-derived-mode futon3c-chat-mode nil "Chat"
  "Chat with Claude via CLI.
Type after the prompt, RET to send.
\\{futon3c-chat-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0))

(defun futon3c-chat-send-input ()
  "Send input to Claude and display response."
  (interactive)
  (futon3c-ui-send-input #'futon3c-chat--call-claude-async "claude"))

(defun futon3c-chat-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (futon3c-ui-clear #'futon3c-chat--init))

(defun futon3c-chat--init ()
  "Initialize."
  (futon3c-chat--ensure-session-id)
  (futon3c-ui-init-buffer
   (list :title "futon3c chat"
         :session-id futon3c-chat-session-id
         :modeline-fn #'futon3c-chat--build-modeline
         :face-alist `(("claude" . futon3c-chat-claude-face))
         :agent-name "claude"
         :thinking-text "claude is thinking..."
         :thinking-prop 'futon3c-thinking)))

;;;###autoload
(defun futon3c-chat ()
  "Start or switch to chat."
  (interactive)
  (let ((buf (get-buffer-create futon3c-chat--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'futon3c-chat-mode)
        (futon3c-chat-mode)
        (futon3c-chat--init)))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'futon3c-chat)
;;; futon3c-chat.el ends here
