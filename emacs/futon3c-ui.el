;;; futon3c-ui.el --- Shared UI primitives for futon3c chat buffers -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Common faces, buffer state, display, and streaming
;;   infrastructure shared by futon3c-chat.el (Claude) and codex-repl.el (Codex).

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;; Faces

(defgroup futon3c-ui nil
  "Shared UI for futon3c chat buffers."
  :group 'communication)

(defface futon3c-ui-joe-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for joe."
  :group 'futon3c-ui)

(defface futon3c-ui-text-face
  '((t :foreground "#f8f8f2"))
  "Face for message text."
  :group 'futon3c-ui)

(defface futon3c-ui-prompt-face
  '((t :foreground "#ffb86c" :weight bold))
  "Face for input prompt."
  :group 'futon3c-ui)

(defface futon3c-ui-thinking-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for thinking/progress indicator."
  :group 'futon3c-ui)

;;; Buffer state

(defvar-local futon3c-ui--prompt-marker nil
  "Marker before the separator line. Messages insert here.")

(defvar-local futon3c-ui--input-start nil
  "Marker at start of user input (after \"> \").")

(defvar-local futon3c-ui--separator-start nil
  "Marker at start of the separator line above prompt.")

(defvar-local futon3c-ui--pending-process nil
  "Current in-flight subprocess for this chat buffer.")

(defvar-local futon3c-ui--face-alist nil
  "Alist mapping speaker name to face, e.g. ((\"claude\" . face)).")

(defvar-local futon3c-ui--agent-name nil
  "Name of the agent in this buffer (\"claude\" or \"codex\").")

(defvar-local futon3c-ui--thinking-text nil
  "The thinking indicator text, e.g. \"claude is thinking...\".")

(defvar-local futon3c-ui--thinking-property nil
  "Text property symbol used to mark thinking lines.")

(defvar-local futon3c-ui--insert-message-hook nil
  "Hook called with (NAME TEXT) before inserting a message.
Functions on this hook may modify TEXT by returning a replacement string.
If a function returns nil, the original TEXT is used.")

;;; Display

(defun futon3c-ui-insert-message (name text)
  "Insert a message from NAME with TEXT above the prompt.
Uses `futon3c-ui--face-alist' to pick the name face.
Runs `futon3c-ui--insert-message-hook' which may transform TEXT."
  (let* ((inhibit-read-only t)
         (face (or (cdr (assoc name futon3c-ui--face-alist))
                   'futon3c-ui-joe-face))
         (at-end (>= (point) (marker-position futon3c-ui--input-start)))
         ;; Run hooks — allow text transformation
         (transformed text))
    (dolist (fn futon3c-ui--insert-message-hook)
      (when-let ((result (funcall fn name transformed)))
        (setq transformed result)))
    (save-excursion
      (goto-char (marker-position futon3c-ui--prompt-marker))
      (insert (propertize (format "%s: " name) 'face face)
              (propertize (format "%s\n\n" transformed) 'face 'futon3c-ui-text-face)))
    (when at-end
      (goto-char (point-max))
      (futon3c-ui-scroll-to-bottom))))

(defun futon3c-ui-scroll-to-bottom ()
  "Scroll window so input prompt is pinned near the bottom."
  (when-let ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -2))))

;;; Thinking / Progress

(defun futon3c-ui-insert-thinking ()
  "Insert the thinking indicator above the prompt."
  (save-excursion
    (goto-char (marker-position futon3c-ui--prompt-marker))
    (insert (propertize (concat (or futon3c-ui--thinking-text "thinking...") "\n")
                        'face 'futon3c-ui-thinking-face
                        (or futon3c-ui--thinking-property 'futon3c-ui-thinking) t))))

(defun futon3c-ui-remove-thinking ()
  "Remove thinking/progress indicators from the buffer."
  (let ((inhibit-read-only t)
        (prop (or futon3c-ui--thinking-property 'futon3c-ui-thinking))
        (text (or futon3c-ui--thinking-text "thinking...")))
    ;; Fast path: exact text match
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (concat text "\n") nil t)
        (delete-region (match-beginning 0) (match-end 0))))
    ;; Fallback: remove lines marked with the thinking property
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((next (or (next-single-property-change (point) prop nil (point-max))
                        (point-max))))
          (if (get-text-property (point) prop)
              (delete-region (line-beginning-position)
                             (min (point-max) (1+ (line-end-position))))
            (goto-char next)))))))

(defun futon3c-ui-update-progress (status-text)
  "Replace the thinking/progress line with STATUS-TEXT."
  (let ((inhibit-read-only t)
        (prop (or futon3c-ui--thinking-property 'futon3c-ui-thinking)))
    ;; Remove existing progress lines
    (futon3c-ui-remove-thinking)
    ;; Insert new progress line
    (save-excursion
      (goto-char (marker-position futon3c-ui--prompt-marker))
      (insert (propertize (concat status-text "\n")
                          'face 'futon3c-ui-thinking-face
                          prop t)))))

;;; Streaming filter

(defun futon3c-ui-make-streaming-filter (parse-event-fn chat-buffer)
  "Return a process `:filter' that splits NDJSON lines.
PARSE-EVENT-FN: called with a JSON string, should return a status
string to display, or nil to skip.
CHAT-BUFFER: the chat buffer to update progress in.
The filter also appends raw output to the process buffer for the sentinel."
  (let ((line-buffer ""))
    (lambda (proc output)
      ;; Append to process buffer so sentinel can read full output
      (when (buffer-live-p (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (insert output)))
      ;; Split into lines and parse
      (setq line-buffer (concat line-buffer output))
      (let ((lines (split-string line-buffer "\n")))
        ;; Last element is incomplete line (or empty string if output ended with \n)
        (setq line-buffer (car (last lines)))
        ;; Process all complete lines
        (dolist (line (butlast lines))
          (when (and (not (string-empty-p (string-trim line)))
                     (buffer-live-p chat-buffer))
            (condition-case nil
                (when-let ((status (funcall parse-event-fn line)))
                  (with-current-buffer chat-buffer
                    (futon3c-ui-update-progress status)))
              (error nil))))))))

;;; Send

(defun futon3c-ui-send-input (call-async-fn agent-name)
  "Generic send: extract input, display it, call CALL-ASYNC-FN.
CALL-ASYNC-FN: (text callback) -> process.
AGENT-NAME: \"claude\" or \"codex\", used for display."
  (when (process-live-p futon3c-ui--pending-process)
    (user-error "%s is still responding; wait for current turn to finish"
                (capitalize agent-name)))
  (let ((text (buffer-substring-no-properties
               (marker-position futon3c-ui--input-start)
               (point-max))))
    (when (not (string-empty-p (string-trim text)))
      (let ((trimmed (string-trim text))
            (chat-buffer (current-buffer)))
        (delete-region (marker-position futon3c-ui--input-start) (point-max))
        (futon3c-ui-insert-message "joe" trimmed)
        (futon3c-ui-insert-thinking)
        (redisplay)
        (condition-case err
            (setq futon3c-ui--pending-process
                  (funcall call-async-fn
                           trimmed
                           (lambda (response)
                             (when (buffer-live-p chat-buffer)
                               (with-current-buffer chat-buffer
                                 (futon3c-ui-remove-thinking)
                                 (futon3c-ui-insert-message agent-name response)
                                 (goto-char (point-max))
                                 (futon3c-ui-scroll-to-bottom))))))
          (error
           (setq futon3c-ui--pending-process nil)
           (futon3c-ui-remove-thinking)
           (futon3c-ui-insert-message
            agent-name
            (format "[Error launching %s process: %s]"
                    agent-name
                    (error-message-string err)))))))))

;;; Init / Clear

(defun futon3c-ui-clear (init-fn)
  "Kill pending process, erase buffer, call INIT-FN."
  (when (process-live-p futon3c-ui--pending-process)
    (kill-process futon3c-ui--pending-process)
    (setq futon3c-ui--pending-process nil))
  (erase-buffer)
  (funcall init-fn))

(defun futon3c-ui-init-buffer (config)
  "Initialize a chat buffer with CONFIG plist.
CONFIG keys:
  :title       - buffer title string (e.g. \"futon3c chat\")
  :session-id  - current session ID string
  :modeline-fn - 0-arg function returning modeline string
  :prompt-face - face for the \"> \" prompt
  :face-alist  - alist of (name . face) for speakers
  :agent-name  - \"claude\" or \"codex\"
  :thinking-text   - e.g. \"claude is thinking...\"
  :thinking-prop   - symbol for text property"
  (let ((title (plist-get config :title))
        (session-id (plist-get config :session-id))
        (modeline-fn (plist-get config :modeline-fn))
        (prompt-face (or (plist-get config :prompt-face) 'futon3c-ui-prompt-face))
        (face-alist (plist-get config :face-alist))
        (agent-name (plist-get config :agent-name))
        (thinking-text (plist-get config :thinking-text))
        (thinking-prop (plist-get config :thinking-prop)))
    ;; Set buffer-local state
    (setq futon3c-ui--face-alist
          (append face-alist (list (cons "joe" 'futon3c-ui-joe-face))))
    (setq futon3c-ui--agent-name agent-name)
    (setq futon3c-ui--thinking-text thinking-text)
    (setq futon3c-ui--thinking-property thinking-prop)
    ;; Insert header
    (insert (propertize (concat title " ") 'face 'bold)
            (propertize (format "(session: %s)\n" (or session-id "pending"))
                        'face 'font-lock-comment-face))
    (when modeline-fn
      (insert (propertize (format "  %s\n" (funcall modeline-fn))
                          'face 'font-lock-comment-face)))
    (insert (propertize "RET send | C-c C-k clear\n\n"
                        'face 'font-lock-comment-face))
    ;; Set markers
    (setq futon3c-ui--prompt-marker (point-marker))
    (setq futon3c-ui--separator-start (point-marker))
    (insert (propertize (make-string 72 ?─) 'face 'font-lock-comment-face) "\n")
    (insert (propertize "> " 'face prompt-face))
    (setq futon3c-ui--input-start (point-marker))
    ;; Marker advances when messages are inserted
    (set-marker-insertion-type futon3c-ui--prompt-marker t)
    (goto-char (point-max))
    (futon3c-ui-scroll-to-bottom)))

;;; Session helpers

(defun futon3c-ui-ensure-session-id (session-file current-id setter-fn)
  "Load session ID from SESSION-FILE if CURRENT-ID is nil.
Calls SETTER-FN with the loaded ID. Does NOT generate new UUIDs
\(that's the caller's responsibility)."
  (unless current-id
    (when (and session-file (file-exists-p session-file))
      (let ((sid (string-trim (with-temp-buffer
                                (insert-file-contents session-file)
                                (buffer-string)))))
        (unless (string-empty-p sid)
          (funcall setter-fn sid))))))

;;; IRC check (shared)

(defun futon3c-ui-irc-available-p ()
  "Check if IRC relay is running on port 6667."
  (condition-case nil
      (let ((proc (open-network-stream "irc-check" nil "127.0.0.1" 6667)))
        (delete-process proc)
        t)
    (error nil)))

;;; Base keymap

(defvar futon3c-ui-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ignore) ;; placeholder, overridden per-mode
    map)
  "Base keymap inherited by chat modes.")

(provide 'futon3c-ui)
;;; futon3c-ui.el ends here
