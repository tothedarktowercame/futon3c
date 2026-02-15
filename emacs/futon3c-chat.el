;;; futon3c-chat.el --- Chat with Claude via claude CLI -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs chat buffer backed by `claude -p`.
;; Asynchronous: type, RET, Claude responds without blocking Emacs UI.
;; Session continuity via --session-id and --continue.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/futon3c-chat.el")
;;   M-x futon3c-chat

(require 'json)

;;; Configuration

(defgroup futon3c-chat nil
  "Chat with Claude via CLI."
  :group 'communication)

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

;;; Faces

(defface futon3c-chat-joe-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for joe."
  :group 'futon3c-chat)

(defface futon3c-chat-claude-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Claude."
  :group 'futon3c-chat)

(defface futon3c-chat-text-face
  '((t :foreground "#f8f8f2"))
  "Face for message text."
  :group 'futon3c-chat)

(defface futon3c-chat-prompt-face
  '((t :foreground "#ffb86c" :weight bold))
  "Face for input prompt."
  :group 'futon3c-chat)

(defface futon3c-chat-thinking-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for thinking indicator."
  :group 'futon3c-chat)

;;; Internal state

(defvar futon3c-chat--buffer-name "*futon3c-chat*")
(defvar-local futon3c-chat--prompt-marker nil)
(defvar-local futon3c-chat--input-start nil)
(defvar-local futon3c-chat--separator-start nil
  "Marker at start of the separator line above prompt.")
(defvar-local futon3c-chat--continued nil
  "Non-nil after first exchange (use --continue for subsequent).")
(defvar-local futon3c-chat--pending-process nil
  "Current in-flight Claude subprocess for this chat buffer.")

;;; Claude CLI call

(defun futon3c-chat--build-claude-args (text)
  "Build argv for `claude` with TEXT."
  (let* ((args (list "-p" text
                     "--permission-mode" "bypassPermissions"))
         ;; First message: use --session-id to create session
         ;; Subsequent: use --resume to target the specific session
         (args (if futon3c-chat--continued
                   (append args (list "--resume" futon3c-chat-session-id))
                 (if futon3c-chat-session-id
                     (append args (list "--session-id" futon3c-chat-session-id))
                   args)))
         (args (if futon3c-chat-model
                   (append args (list "--model" futon3c-chat-model))
                 args))
         (args (append args (list "--append-system-prompt"
                                  (concat "This conversation is via the futon3c Emacs "
                                          "chat interface (futon3c-chat.el). "
                                          "The user is Joe. Transport: emacs-chat. "
                                          "Responses display in an Emacs buffer. "
                                          (futon3c-chat--build-modeline)))))
         (process-environment
          ;; Remove CLAUDECODE to avoid nesting check
          (cl-remove-if (lambda (s)
                          (or (string-prefix-p "CLAUDECODE=" s)
                              (string-prefix-p "CLAUDE_CODE_ENTRYPOINT=" s)))
                        process-environment))
    args))

(defun futon3c-chat--call-claude-async (text callback)
  "Call Claude with TEXT and invoke CALLBACK with response string."
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
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let* ((exit-code (process-exit-status p))
                      (raw (with-current-buffer (process-buffer p)
                             (buffer-string)))
                      (response (if (= exit-code 0)
                                    (string-trim raw)
                                  (format "[Error (exit %d): %s]"
                                          exit-code
                                          (string-trim raw)))))
                 (when (buffer-live-p (process-buffer p))
                   (kill-buffer (process-buffer p)))
                 (when (buffer-live-p chat-buffer)
                   (with-current-buffer chat-buffer
                     (when (eq futon3c-chat--pending-process p)
                       (setq futon3c-chat--pending-process nil))
                     (when (= exit-code 0)
                       ;; After first successful call, use --resume for rest.
                       (setq futon3c-chat--continued t))
                     (funcall callback response)))))))))
    proc))

;;; Display

(defun futon3c-chat--insert-message (name text)
  "Insert a message above the prompt, keeping prompt pinned at window bottom."
  (let ((inhibit-read-only t)
        (face (if (string= name "claude") 'futon3c-chat-claude-face 'futon3c-chat-joe-face))
        (at-end (>= (point) (marker-position futon3c-chat--input-start))))
    (save-excursion
      (goto-char (marker-position futon3c-chat--prompt-marker))
      (insert (propertize (format "%s: " name) 'face face)
              (propertize (format "%s\n\n" text) 'face 'futon3c-chat-text-face)))
    (when at-end
      (goto-char (point-max))
      (futon3c-chat--scroll-to-bottom))))

(defun futon3c-chat--scroll-to-bottom ()
  "Scroll window so input prompt is pinned near the bottom."
  (when-let ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -2))))

;;; Send

(defun futon3c-chat--remove-thinking ()
  "Remove any thinking indicators from the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward "claude is thinking\\.\\.\\.\n" nil t)
        (delete-region (match-beginning 0) (match-end 0))))))

(defun futon3c-chat-send-input ()
  "Send input to Claude and display response."
  (interactive)
  (when (process-live-p futon3c-chat--pending-process)
    (user-error "Claude is still responding; wait for current turn to finish"))
  (let ((text (buffer-substring-no-properties
               (marker-position futon3c-chat--input-start)
               (point-max))))
    (when (not (string-empty-p (string-trim text)))
      (let ((trimmed (string-trim text))
            (chat-buffer (current-buffer)))
        (delete-region (marker-position futon3c-chat--input-start) (point-max))
        (futon3c-chat--insert-message "joe" trimmed)
        (save-excursion
          (goto-char (marker-position futon3c-chat--prompt-marker))
          (insert (propertize "claude is thinking...\n"
                              'face 'futon3c-chat-thinking-face
                              'futon3c-thinking t)))
        (redisplay)
        (condition-case err
            (setq futon3c-chat--pending-process
                  (futon3c-chat--call-claude-async
                   trimmed
                   (lambda (response)
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (futon3c-chat--remove-thinking)
                         (futon3c-chat--insert-message "claude" response)
                         (goto-char (point-max))
                         (futon3c-chat--scroll-to-bottom))))))
          (error
           (setq futon3c-chat--pending-process nil)
           (futon3c-chat--remove-thinking)
           (futon3c-chat--insert-message
            "claude"
            (format "[Error launching claude process: %s]"
                    (error-message-string err)))))))))

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

(defun futon3c-chat-clear ()
  "Clear display and re-draw header. Session continues."
  (interactive)
  (when (process-live-p futon3c-chat--pending-process)
    (kill-process futon3c-chat--pending-process)
    (setq futon3c-chat--pending-process nil))
  (erase-buffer)
  (futon3c-chat--init))

(defun futon3c-chat--ensure-session-id ()
  "Ensure a session ID exists. Generate and persist if needed."
  (unless futon3c-chat-session-id
    (if (and futon3c-chat-session-file
             (file-exists-p futon3c-chat-session-file))
        ;; Reuse existing session
        (setq futon3c-chat-session-id
              (string-trim (with-temp-buffer
                             (insert-file-contents futon3c-chat-session-file)
                             (buffer-string))))
      ;; Generate new UUID
      (let ((uuid (format "%08x-%04x-%04x-%04x-%012x"
                          (random (expt 16 8))
                          (random (expt 16 4))
                          (logior #x4000 (random #x0fff))
                          (logior #x8000 (random #x3fff))
                          (random (expt 16 12)))))
        (setq futon3c-chat-session-id uuid)
        (when futon3c-chat-session-file
          (write-region uuid nil futon3c-chat-session-file nil 'silent))))))

(defun futon3c-chat--irc-available-p ()
  "Check if IRC relay is running on port 6667."
  (condition-case nil
      (let ((proc (open-network-stream "irc-check" nil "127.0.0.1" 6667)))
        (delete-process proc)
        t)
    (error nil)))

(defun futon3c-chat--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list (format "emacs-chat (active, session %s)"
                                  (or futon3c-chat-session-id "unknown"))))
        (irc-up (futon3c-chat--irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (claude code)" transports)
    (format "Available transports: [%s]. Current: emacs-chat."
            (string-join (reverse transports) ", "))))

(defun futon3c-chat--init ()
  "Initialize."
  (futon3c-chat--ensure-session-id)
  (insert (propertize "futon3c chat " 'face 'bold)
          (propertize (format "(session: %s)\n" futon3c-chat-session-id)
                      'face 'font-lock-comment-face)
          (propertize (format "  %s\n" (futon3c-chat--build-modeline))
                      'face 'font-lock-comment-face)
          (propertize "RET send | C-c C-k clear\n\n"
                      'face 'font-lock-comment-face))
  (setq futon3c-chat--prompt-marker (point-marker))
  (setq futon3c-chat--separator-start (point-marker))
  (insert (propertize (make-string 72 ?─) 'face 'font-lock-comment-face) "\n")
  (insert (propertize "> " 'face 'futon3c-chat-prompt-face))
  (setq futon3c-chat--input-start (point-marker))
  ;; Set type t AFTER init inserts, so marker stays before separator
  ;; but advances when messages are inserted at runtime
  (set-marker-insertion-type futon3c-chat--prompt-marker t)
  (goto-char (point-max))
  (futon3c-chat--scroll-to-bottom))

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
