;;; agent-chat.el --- Shared UI primitives for agent chat buffers -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Common faces, buffer state, display, and streaming
;;   infrastructure shared by claude-repl.el (Claude) and codex-repl.el (Codex).

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;; Faces

(defgroup agent-chat nil
  "Shared UI for futon3c chat buffers."
  :group 'communication)

(defcustom agent-chat-agency-base-url
  (or (getenv "FUTON3C_EVIDENCE_BASE")
      (getenv "FUTON3C_SELF_URL")
      (format "http://127.0.0.1:%s" (or (getenv "FUTON3C_PORT") "7070")))
  "Agency base URL used for availability checks in chat headers."
  :type 'string
  :group 'agent-chat)

(defcustom agent-chat-irc-host
  (or (getenv "FUTON3C_BIND_HOST") "127.0.0.1")
  "IRC host used for local availability checks."
  :type 'string
  :group 'agent-chat)

(defcustom agent-chat-irc-port
  (let ((raw (getenv "FUTON3C_IRC_PORT")))
    (if (and raw (string-match-p "\\`[0-9]+\\'" raw))
        (string-to-number raw)
      6667))
  "IRC port used for local availability checks."
  :type 'integer
  :group 'agent-chat)

(defface agent-chat-joe-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for joe."
  :group 'agent-chat)

(defface agent-chat-text-face
  '((t :foreground "#f8f8f2"))
  "Face for message text."
  :group 'agent-chat)

(defface agent-chat-prompt-face
  '((t :foreground "#ffb86c" :weight bold))
  "Face for input prompt."
  :group 'agent-chat)

(defface agent-chat-thinking-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for thinking/progress indicator."
  :group 'agent-chat)

;;; Buffer state

(defvar-local agent-chat--prompt-marker nil
  "Marker before the separator line. Messages insert here.")

(defvar-local agent-chat--input-start nil
  "Marker at start of user input (after \"> \").")

(defvar-local agent-chat--separator-start nil
  "Marker at start of the separator line above prompt.")

(defvar-local agent-chat--pending-process nil
  "Current in-flight subprocess for this chat buffer.")

(defvar-local agent-chat--session-id nil
  "Current session ID displayed in the buffer header.")

(defvar-local agent-chat--face-alist nil
  "Alist mapping speaker name to face, e.g. ((\"claude\" . face)).")

(defvar-local agent-chat--agent-name nil
  "Name of the agent in this buffer (\"claude\" or \"codex\").")

(defvar-local agent-chat--thinking-text nil
  "The thinking indicator text, e.g. \"claude is thinking...\".")

(defvar-local agent-chat--thinking-property nil
  "Text property symbol used to mark thinking lines.")

(defvar-local agent-chat--insert-message-hook nil
  "Hook called with (NAME TEXT) before inserting a message.
Functions on this hook may modify TEXT by returning a replacement string.
If a function returns nil, the original TEXT is used.")

;;; Display

(defun agent-chat-insert-message (name text)
  "Insert a message from NAME with TEXT above the prompt.
Uses `agent-chat--face-alist' to pick the name face.
Runs `agent-chat--insert-message-hook' which may transform TEXT."
  (let* ((inhibit-read-only t)
         (face (or (cdr (assoc name agent-chat--face-alist))
                   'agent-chat-joe-face))
         (at-end (>= (point) (marker-position agent-chat--input-start)))
         ;; Run hooks — allow text transformation
         (transformed text))
    (dolist (fn agent-chat--insert-message-hook)
      (when-let ((result (funcall fn name transformed)))
        (setq transformed result)))
    (save-excursion
      (goto-char (marker-position agent-chat--prompt-marker))
      (insert (propertize (format "%s: " name) 'face face)
              (propertize (format "%s\n\n" transformed) 'face 'agent-chat-text-face)))
    (when at-end
      (goto-char (point-max))
      (agent-chat-scroll-to-bottom))))

(defun agent-chat-scroll-to-bottom ()
  "Scroll window so input prompt is pinned near the bottom."
  (when-let ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -2))))

;;; Thinking / Progress

(defun agent-chat-insert-thinking ()
  "Insert the thinking indicator above the prompt."
  (save-excursion
    (goto-char (marker-position agent-chat--prompt-marker))
    (insert (propertize (concat (or agent-chat--thinking-text "thinking...") "\n")
                        'face 'agent-chat-thinking-face
                        (or agent-chat--thinking-property 'agent-chat-thinking) t))))

(defun agent-chat-remove-thinking ()
  "Remove thinking/progress indicators from the buffer."
  (let ((inhibit-read-only t)
        (prop (or agent-chat--thinking-property 'agent-chat-thinking))
        (text (or agent-chat--thinking-text "thinking...")))
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

(defun agent-chat-update-progress (status-text)
  "Replace the thinking/progress line with STATUS-TEXT."
  (let ((inhibit-read-only t)
        (prop (or agent-chat--thinking-property 'agent-chat-thinking)))
    ;; Remove existing progress lines
    (agent-chat-remove-thinking)
    ;; Insert new progress line
    (save-excursion
      (goto-char (marker-position agent-chat--prompt-marker))
      (insert (propertize (concat status-text "\n")
                          'face 'agent-chat-thinking-face
                          prop t)))))

;;; Streaming filter

(defun agent-chat-make-streaming-filter (parse-event-fn chat-buffer)
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
                    (agent-chat-update-progress status)))
              (error nil))))))))

;;; Send

(defun agent-chat-send-input (call-async-fn agent-name &optional hooks)
  "Generic send: extract input, display it, call CALL-ASYNC-FN.
CALL-ASYNC-FN: (text callback) -> process.
AGENT-NAME: \"claude\" or \"codex\", used for display.
Optional HOOKS plist supports:
  :before-send   (fn text) called after user text is inserted.
  :on-response   (fn text) called after agent response is inserted.
  :on-launch-error (fn text) called when process launch fails."
  (when (process-live-p agent-chat--pending-process)
    (user-error "%s is still responding; wait for current turn to finish"
                (capitalize agent-name)))
  (let ((text (buffer-substring-no-properties
               (marker-position agent-chat--input-start)
               (point-max))))
    (when (not (string-empty-p (string-trim text)))
      (let ((trimmed (string-trim text))
            (chat-buffer (current-buffer))
            (before-send (plist-get hooks :before-send))
            (on-response (plist-get hooks :on-response))
            (on-launch-error (plist-get hooks :on-launch-error)))
        (delete-region (marker-position agent-chat--input-start) (point-max))
        (agent-chat-insert-message "joe" trimmed)
        (when (functionp before-send)
          (funcall before-send trimmed))
        (agent-chat-insert-thinking)
        (redisplay)
        (condition-case err
            (setq agent-chat--pending-process
                  (funcall call-async-fn
                           trimmed
                           (lambda (response)
                             (when (buffer-live-p chat-buffer)
                               (with-current-buffer chat-buffer
                                 (agent-chat-remove-thinking)
                                 (agent-chat-insert-message agent-name response)
                                 (when (functionp on-response)
                                   (funcall on-response response))
                                 (goto-char (point-max))
                                 (agent-chat-scroll-to-bottom))))))
          (error
           (setq agent-chat--pending-process nil)
           (agent-chat-remove-thinking)
           (let ((msg (format "[Error launching %s process: %s]"
                              agent-name
                              (error-message-string err))))
             (agent-chat-insert-message agent-name msg)
             (when (functionp on-launch-error)
               (funcall on-launch-error msg)))))))))

;;; Init / Clear

(defun agent-chat-clear (init-fn)
  "Kill pending process, erase buffer, call INIT-FN."
  (when (process-live-p agent-chat--pending-process)
    (kill-process agent-chat--pending-process)
    (setq agent-chat--pending-process nil))
  (erase-buffer)
  (funcall init-fn))

(defun agent-chat-init-buffer (config)
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
        (prompt-face (or (plist-get config :prompt-face) 'agent-chat-prompt-face))
        (face-alist (plist-get config :face-alist))
        (agent-name (plist-get config :agent-name))
        (thinking-text (plist-get config :thinking-text))
        (thinking-prop (plist-get config :thinking-prop)))
    ;; Set buffer-local state
    (setq agent-chat--face-alist
          (append face-alist (list (cons "joe" 'agent-chat-joe-face))))
    (setq agent-chat--agent-name agent-name)
    (setq agent-chat--thinking-text thinking-text)
    (setq agent-chat--thinking-property thinking-prop)
    (setq agent-chat--session-id (or session-id "pending"))
    ;; Insert header
    (insert (propertize (concat title " ") 'face 'bold)
            (propertize (format "(session: %s)\n" agent-chat--session-id)
                        'face 'font-lock-comment-face))
    (when modeline-fn
      (insert (propertize (format "  %s\n" (funcall modeline-fn))
                          'face 'font-lock-comment-face)))
    (insert (propertize "RET send | C-c C-c interrupt | C-c C-k clear | C-c C-n new session\n\n"
                        'face 'font-lock-comment-face))
    ;; Set markers
    (setq agent-chat--prompt-marker (point-marker))
    (setq agent-chat--separator-start (point-marker))
    (insert (propertize (make-string 72 ?─) 'face 'font-lock-comment-face) "\n")
    (insert (propertize "> " 'face prompt-face))
    (setq agent-chat--input-start (point-marker))
    ;; Marker advances when messages are inserted
    (set-marker-insertion-type agent-chat--prompt-marker t)
    (agent-chat-enable-markdown-font-lock)
    (goto-char (point-max))
    (agent-chat-scroll-to-bottom)))

;;; Session helpers

(defun agent-chat-ensure-session-id (session-file current-id setter-fn)
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

(defun agent-chat-update-session-id (new-id)
  "Update the session ID displayed in the buffer header.
Replaces the `(session: ...)' text in the first line."
  (when (and new-id (not (equal new-id agent-chat--session-id)))
    (let ((inhibit-read-only t)
          (old-text (format "(session: %s)" agent-chat--session-id))
          (new-text (format "(session: %s)" new-id)))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward old-text (line-end-position 2) t)
          (replace-match (propertize new-text 'face 'font-lock-comment-face) t t)))
      (setq agent-chat--session-id new-id))))

;;; Transport availability checks (shared)

(defun agent-chat-port-open-p (host port)
  "Return non-nil when HOST:PORT accepts a TCP connection."
  (condition-case nil
      (let ((proc (open-network-stream "futon3c-port-check" nil host port)))
        (delete-process proc)
        t)
    (error nil)))

(defun agent-chat--url-host-port (url default-port)
  "Extract host/port from URL, falling back to DEFAULT-PORT when missing."
  (let ((clean (and (stringp url) (string-trim url))))
    (if (and clean
             (string-match
              "\\`[a-zA-Z]+://\\([^/:?#]+\\)\\(?::\\([0-9]+\\)\\)?\\(?:[/?#].*\\)?\\'"
              clean))
        (cons (match-string 1 clean)
              (if-let ((port (match-string 2 clean)))
                (string-to-number port)
                default-port))
      (cons "127.0.0.1" default-port))))

(defun agent-chat-irc-available-p ()
  "Check if IRC relay is reachable on configured host/port."
  (and (integerp agent-chat-irc-port)
       (> agent-chat-irc-port 0)
       (agent-chat-port-open-p agent-chat-irc-host agent-chat-irc-port)))

(defun agent-chat-agency-available-p ()
  "Check if futon3c agency HTTP server is reachable."
  (pcase-let ((`(,host . ,port)
               (agent-chat--url-host-port agent-chat-agency-base-url 7070)))
    (agent-chat-port-open-p host port)))

;;; Interrupt

(defun agent-chat-interrupt ()
  "Interrupt the current in-flight request.
Kills the pending process, removes thinking indicator, and inserts
an [interrupted] message."
  (interactive)
  (if (process-live-p agent-chat--pending-process)
      (let ((proc agent-chat--pending-process))
        (setq agent-chat--pending-process nil)
        (kill-process proc)
        (agent-chat-remove-thinking)
        (agent-chat-insert-message
         "system" "[interrupted]")
        (goto-char (point-max))
        (message "%s interrupted" (or agent-chat--agent-name "agent")))
    (message "Nothing to interrupt")))

;;; Markdown font-lock

(defface agent-chat-markdown-bold
  '((t :weight bold))
  "Face for **bold** markdown text."
  :group 'agent-chat)

(defface agent-chat-markdown-italic
  '((t :slant italic))
  "Face for *italic* markdown text."
  :group 'agent-chat)

(defface agent-chat-markdown-code
  '((t :inherit fixed-pitch :background "#3a3a3a" :foreground "#a9dc76"))
  "Face for `inline code` markdown text."
  :group 'agent-chat)

(defface agent-chat-markdown-heading
  '((t :weight bold :foreground "#bd93f9" :height 1.1))
  "Face for # headings."
  :group 'agent-chat)

(defface agent-chat-markdown-fence
  '((t :inherit fixed-pitch :background "#2a2a2a" :foreground "#a9dc76" :extend t))
  "Face for fenced code blocks."
  :group 'agent-chat)

(defvar agent-chat-markdown-keywords
  `((,(rx bol (group (+ "#") " " (* nonl))) 0 'agent-chat-markdown-heading prepend)
    (,(rx (group "**" (+? (not (any "*"))) "**")) 0 'agent-chat-markdown-bold prepend)
    (,(rx (not (any "*")) (group "*" (+? (not (any "*" "\n"))) "*") (not (any "*")))
     1 'agent-chat-markdown-italic prepend)
    (,(rx (group "`" (+? (not (any "`" "\n"))) "`")) 0 'agent-chat-markdown-code prepend)
    (,(rx (group bol "```" (* nonl) "\n" (*? anything) bol "```" (* blank) eol))
     0 'agent-chat-markdown-fence prepend))
  "Font-lock keywords for lightweight GFM highlighting.")

(defun agent-chat-enable-markdown-font-lock ()
  "Enable subtle markdown highlighting in the current buffer."
  (font-lock-add-keywords nil agent-chat-markdown-keywords t)
  (setq-local font-lock-multiline t)
  (font-lock-mode 1))

;;; Base keymap

(defvar agent-chat-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ignore) ;; placeholder, overridden per-mode
    map)
  "Base keymap inherited by chat modes.")

(provide 'agent-chat)
;;; agent-chat.el ends here
