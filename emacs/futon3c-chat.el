;;; futon3c-chat.el --- Chat with Claude via futon3c API -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs chat buffer backed by futon3c's /api/alpha/invoke endpoint.
;; Asynchronous: type, RET, Claude responds without blocking Emacs UI.
;; Routes through the agent registry — same invoke-fn as IRC.
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/futon3c-ui.el")
;;   (load "/home/joe/code/futon3c/emacs/futon3c-chat.el")
;;   M-x futon3c-chat

(require 'cl-lib)
(require 'futon3c-ui)

;;; Configuration

(defgroup futon3c-chat nil
  "Chat with Claude via futon3c API."
  :group 'futon3c-ui)

(defcustom futon3c-chat-api-url "http://localhost:7070"
  "Base URL for the futon3c API server."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-agent-id "claude-1"
  "Agent ID to invoke via the API."
  :type 'string
  :group 'futon3c-chat)

(defcustom futon3c-chat-session-file "/tmp/futon-session-id"
  "File storing Claude session ID (shared with IRC relay)."
  :type 'string
  :group 'futon3c-chat)

;;; Face (Claude-specific; shared faces are in futon3c-ui)

(defface futon3c-chat-claude-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Claude."
  :group 'futon3c-chat)

;;; Internal state

(defvar futon3c-chat--buffer-name "*futon3c-chat*")

;;; Claude API call

(defun futon3c-chat--call-claude-async (text callback)
  "Send TEXT to Claude via POST /api/alpha/invoke.
Uses curl to POST to the futon3c API server. The server's invoke-fn
calls `claude -p' with the correct session-id (managed by the registry).
CALLBACK receives the response string."
  (let* ((chat-buffer (current-buffer))
         (url (concat futon3c-chat-api-url "/api/alpha/invoke"))
         (json-body (json-serialize
                     `(:agent-id ,futon3c-chat-agent-id
                       :prompt ,text)))
         (outbuf (generate-new-buffer " *futon3c-invoke*"))
         (proc nil))
    (setq proc
          (make-process
           :name "futon3c-invoke"
           :buffer outbuf
           :command (list "curl" "-sS" "--max-time" "300"
                          "-H" "Content-Type: application/json"
                          "-d" json-body url)
           :noquery t
           :connection-type 'pipe
           :filter (lambda (proc string)
                     (when (buffer-live-p (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (goto-char (point-max))
                         (insert string))))
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (condition-case err
                   (let* ((exit-code (process-exit-status p))
                          (raw (if (buffer-live-p (process-buffer p))
                                   (with-current-buffer (process-buffer p)
                                     (buffer-string))
                                 ""))
                          (response
                           (if (/= exit-code 0)
                               (format "[curl error (exit %d): %s]"
                                       exit-code (string-trim raw))
                             (condition-case parse-err
                                 (let* ((json-obj (json-parse-string
                                                   raw
                                                   :object-type 'alist
                                                   :null-object nil
                                                   :false-object nil))
                                        (ok (alist-get 'ok json-obj))
                                        (result (alist-get 'result json-obj))
                                        (sid (alist-get 'session-id json-obj))
                                        (err-msg (or (alist-get 'message json-obj)
                                                     (alist-get 'error json-obj))))
                                   (when (and ok sid (buffer-live-p chat-buffer))
                                     (with-current-buffer chat-buffer
                                       (futon3c-ui-update-session-id sid))
                                     (when futon3c-chat-session-file
                                       (write-region sid nil futon3c-chat-session-file nil 'silent)))
                                   (if ok
                                       (let ((r (and (stringp result)
                                                     (not (string-empty-p (string-trim result)))
                                                     result)))
                                         (or r "[empty response]"))
                                     (format "[Error: %s]" (or err-msg raw))))
                               (error
                                (format "[JSON parse error: %s\nRaw: %s]"
                                        (error-message-string parse-err)
                                        (truncate-string-to-width raw 200)))))))
                     (message "futon3c-chat: exit=%d raw-len=%d" exit-code (length raw))
                     (when (buffer-live-p (process-buffer p))
                       (kill-buffer (process-buffer p)))
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (when (eq futon3c-ui--pending-process p)
                           (setq futon3c-ui--pending-process nil))
                         (funcall callback response))))
                 (error
                  (message "futon3c-chat sentinel error: %s" (error-message-string err))
                  (when (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (setq futon3c-ui--pending-process nil)
                      (futon3c-ui-remove-thinking)
                      (futon3c-ui-insert-message
                       "claude"
                       (format "[Sentinel error: %s]" (error-message-string err)))))))))))
    proc))

;;; Modeline

(defun futon3c-chat--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list "emacs-chat (active, via /api/alpha/invoke)"))
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
  "Chat with Claude via futon3c API.
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
  "Initialize.
Load existing session-id from file if available."
  (let ((existing-sid
         (when (and futon3c-chat-session-file
                    (file-exists-p futon3c-chat-session-file))
           (let ((s (string-trim
                     (with-temp-buffer
                       (insert-file-contents futon3c-chat-session-file)
                       (buffer-string)))))
             (unless (string-empty-p s) s)))))
    (futon3c-ui-init-buffer
     (list :title "futon3c chat"
           :session-id (or existing-sid
                           (format "%s (awaiting session)" futon3c-chat-agent-id))
           :modeline-fn #'futon3c-chat--build-modeline
           :face-alist `(("claude" . futon3c-chat-claude-face))
           :agent-name "claude"
           :thinking-text "claude is thinking..."
           :thinking-prop 'futon3c-thinking))))

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
