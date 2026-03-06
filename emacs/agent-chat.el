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
Applies faces via overlays so they survive font-lock refontification.
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
      (let ((name-start (point)))
        (insert (format "%s: " name))
        (let ((name-end (point)))
          (insert (format "%s\n\n" transformed))
          (let ((text-end (point)))
            (overlay-put (make-overlay name-start name-end) 'face face)
            (overlay-put (make-overlay name-end text-end) 'face 'agent-chat-text-face)))))
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

(defun agent-chat-update-progress (status-text &optional face)
  "Replace the thinking/progress line with STATUS-TEXT.
Optional FACE overrides `agent-chat-thinking-face'."
  (let ((inhibit-read-only t)
        (prop (or agent-chat--thinking-property 'agent-chat-thinking)))
    ;; Remove existing progress lines
    (agent-chat-remove-thinking)
    ;; Insert new progress line
    (save-excursion
      (goto-char (marker-position agent-chat--prompt-marker))
      (insert (propertize (concat status-text "\n")
                          'face (or face 'agent-chat-thinking-face)
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
    (let ((prompt-start (point)))
      (insert "> ")
      (overlay-put (make-overlay prompt-start (point)) 'face prompt-face))
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

(defcustom agent-chat-hide-markup nil
  "When non-nil, hide markdown delimiters (backticks, stars, fences).
Delimiters are hidden via the `display' text property — the underlying
text is preserved for copy/paste.  Toggle and `font-lock-flush' to
see the change."
  :type 'boolean
  :group 'agent-chat)

(defcustom agent-chat-fontify-code-natively t
  "When non-nil, fontify fenced code blocks using their language mode.
Falls back to `agent-chat-markdown-fence' face when the mode is
unavailable."
  :type 'boolean
  :group 'agent-chat)

(defcustom agent-chat-code-lang-modes
  '(("clojure" . clojure-mode) ("clj" . clojure-mode)
    ("edn" . clojure-mode) ("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode) ("python" . python-mode)
    ("bash" . sh-mode) ("shell" . sh-mode) ("sh" . sh-mode)
    ("javascript" . js-mode) ("js" . js-mode)
    ("json" . js-mode) ("ruby" . ruby-mode)
    ("rust" . rust-mode) ("go" . go-mode)
    ("html" . html-mode) ("css" . css-mode)
    ("sql" . sql-mode) ("yaml" . yaml-mode)
    ("markdown" . markdown-mode) ("md" . markdown-mode))
  "Alist mapping fenced-block language tags to major modes."
  :type '(alist :key-type string :value-type symbol)
  :group 'agent-chat)

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

(defface agent-chat-markdown-fence-delimiter
  '((t :foreground "#6272a4"))
  "Face for fence delimiter lines (``` markers) when not hidden."
  :group 'agent-chat)

;; --- Markup hiding helpers ---

(defun agent-chat--hide-region (start end)
  "Apply display \"\" to START..END when `agent-chat-hide-markup' is non-nil."
  (when agent-chat-hide-markup
    (add-text-properties start end '(display ""))))

;; --- Native code block fontification ---

(defun agent-chat--resolve-lang-mode (lang)
  "Return a major-mode symbol for LANG, or nil if unavailable."
  (when (and lang (not (string-empty-p lang)))
    (let ((mode (or (cdr (assoc (downcase lang) agent-chat-code-lang-modes))
                    (intern (concat (downcase lang) "-mode")))))
      (when (fboundp mode) mode))))

(defun agent-chat--fontify-code-natively (lang code-start code-end)
  "Fontify the region CODE-START..CODE-END using LANG's major mode.
Copies face properties from a temporary fontification buffer back
to the chat buffer as overlays (resilient to font-lock refontification).
Returns non-nil on success."
  (let ((lang-mode (agent-chat--resolve-lang-mode lang)))
    (when lang-mode
      (let* ((code (buffer-substring-no-properties code-start code-end))
             (chat-buf (current-buffer))
             (temp-buf (get-buffer-create
                        (format " *agent-chat-code:%s*" (symbol-name lang-mode)))))
        (with-current-buffer temp-buf
          (let ((inhibit-modification-hooks t))
            (erase-buffer)
            (insert code))
          (unless (eq major-mode lang-mode)
            (delay-mode-hooks (funcall lang-mode)))
          (font-lock-ensure))
        ;; Copy face properties back as overlays
        (let ((pos 1)
              (len (length code)))
          (while (< pos (1+ len))
            (let* ((next (or (with-current-buffer temp-buf
                               (next-single-property-change pos 'face))
                             (1+ len)))
                   (face-val (with-current-buffer temp-buf
                               (get-text-property pos 'face))))
              (when face-val
                (let ((ov (make-overlay (+ code-start (1- pos))
                                        (min code-end (+ code-start (1- next)))
                                        chat-buf)))
                  (overlay-put ov 'face face-val)
                  (overlay-put ov 'agent-chat-code t)))
              (setq pos next))))
        t))))

;; --- Font-lock matcher functions ---

(defun agent-chat--match-heading (limit)
  "Font-lock matcher for # headings.
Groups: 1=prefix (# + space), 2=heading text."
  (re-search-forward (rx bol (group (+ "#") " ") (group (* nonl))) limit t))

(defun agent-chat--match-bold (limit)
  "Font-lock matcher for **bold**.
Groups: 1=open **, 2=text, 3=close **."
  (re-search-forward
   (rx (group "**") (group (+? (not (any "*")))) (group "**"))
   limit t))

(defun agent-chat--match-italic (limit)
  "Font-lock matcher for *italic* (not **bold**).
Groups: 1=preceding-char + open *, 2=text, 3=close * + following-char."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward
                 (rx (group (not (any "*")) "*")
                     (group (+? (not (any "*" "\n"))))
                     (group "*" (not (any "*"))))
                 limit t))
      (unless (and (> (match-beginning 1) 0)
                   (eq (char-before (match-beginning 1)) ?*))
        (setq found t)))
    found))

(defun agent-chat--match-inline-code (limit)
  "Font-lock matcher for \\=`code\\=`.
Groups: 1=open `, 2=text, 3=close `.  Skips fenced regions."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward
                 (rx (group "`") (group (+? (not (any "`" "\n")))) (group "`"))
                 limit t))
      (unless (or (get-text-property (match-beginning 0) 'agent-chat-in-fence)
                  (eq (char-before (match-beginning 1)) ?`)
                  (eq (char-after (match-end 3)) ?`))
        (setq found t)))
    found))

(defun agent-chat--match-fence (limit)
  "Font-lock matcher for fenced code blocks.
Groups: 1=opening fence line, 2=language tag, 3=code content, 4=closing fence."
  (re-search-forward
   (rx bol (group "```" (group (* (any alnum "-" "_" "+"))) (* blank) "\n")
       (group (*? anything))
       (group bol "```" (* blank) eol))
   limit t))

;; --- Fontify functions (called as font-lock FACESPEC side effects) ---

(defun agent-chat--fontify-heading ()
  "Apply heading face, optionally hide # prefix.  Return nil."
  (agent-chat--hide-region (match-beginning 1) (match-end 1))
  (add-text-properties (match-beginning 2) (match-end 2)
                        '(face agent-chat-markdown-heading))
  (unless agent-chat-hide-markup
    (add-text-properties (match-beginning 1) (match-end 1)
                          '(face agent-chat-markdown-heading)))
  nil)

(defun agent-chat--fontify-bold ()
  "Apply bold face, optionally hide **.  Return nil."
  (agent-chat--hide-region (match-beginning 1) (match-end 1))
  (agent-chat--hide-region (match-beginning 3) (match-end 3))
  (add-text-properties (match-beginning 2) (match-end 2)
                        '(face agent-chat-markdown-bold))
  nil)

(defun agent-chat--fontify-italic ()
  "Apply italic face, optionally hide *.  Return nil.
Group 1 includes preceding non-* char; group 3 includes following non-* char.
Only hide the * character itself within each group."
  (agent-chat--hide-region (1- (match-end 1)) (match-end 1))
  (agent-chat--hide-region (match-beginning 3) (1+ (match-beginning 3)))
  (add-text-properties (match-beginning 2) (match-end 2)
                        '(face agent-chat-markdown-italic))
  nil)

(defun agent-chat--fontify-inline-code ()
  "Apply code face, optionally hide backticks.  Return nil."
  (agent-chat--hide-region (match-beginning 1) (match-end 1))
  (agent-chat--hide-region (match-beginning 3) (match-end 3))
  (add-text-properties (match-beginning 2) (match-end 2)
                        '(face agent-chat-markdown-code))
  nil)

(defun agent-chat--fontify-fence ()
  "Fontify fenced code block with native highlighting.  Return nil.
Marks code content with `agent-chat-in-fence' so inline-code matcher
skips it.  Uses overlays for native syntax faces."
  (let ((fence-open-start (match-beginning 1))
        (fence-open-end (match-end 1))
        (lang (match-string-no-properties 2))
        (code-start (match-beginning 3))
        (code-end (match-end 3))
        (fence-close-start (match-beginning 4))
        (fence-close-end (match-end 4)))
    ;; Mark code region so inline-code matcher skips it
    (when (> code-end code-start)
      (add-text-properties code-start code-end '(agent-chat-in-fence t)))
    ;; Remove stale code overlays in this region
    (dolist (ov (overlays-in (or code-start fence-open-start)
                             (or code-end fence-close-end)))
      (when (overlay-get ov 'agent-chat-code)
        (delete-overlay ov)))
    ;; Fence delimiters: hide or dim
    (if agent-chat-hide-markup
        (progn
          (agent-chat--hide-region fence-open-start fence-open-end)
          (agent-chat--hide-region fence-close-start fence-close-end))
      (add-text-properties fence-open-start fence-open-end
                            '(face agent-chat-markdown-fence-delimiter))
      (add-text-properties fence-close-start fence-close-end
                            '(face agent-chat-markdown-fence-delimiter)))
    ;; Code content: native fontification or fence face
    (when (> code-end code-start)
      (add-text-properties code-start code-end
                            '(face agent-chat-markdown-fence))
      (when agent-chat-fontify-code-natively
        (agent-chat--fontify-code-natively lang code-start code-end))))
  nil)

;; --- Font-lock keyword list ---

(defvar agent-chat-markdown-keywords
  '((agent-chat--match-fence (0 (agent-chat--fontify-fence) nil t))
    (agent-chat--match-heading (0 (agent-chat--fontify-heading) nil t))
    (agent-chat--match-bold (0 (agent-chat--fontify-bold) nil t))
    (agent-chat--match-italic (0 (agent-chat--fontify-italic) nil t))
    (agent-chat--match-inline-code (0 (agent-chat--fontify-inline-code) nil t)))
  "Font-lock keywords for GFM highlighting with markup hiding and native code.")

(defun agent-chat-enable-markdown-font-lock ()
  "Enable markdown highlighting in the current buffer.
Respects `agent-chat-hide-markup' and `agent-chat-fontify-code-natively'."
  (setq-local font-lock-extra-managed-props '(display agent-chat-in-fence))
  (font-lock-add-keywords nil agent-chat-markdown-keywords t)
  (setq-local font-lock-multiline t)
  (font-lock-mode 1))

;;; Toggle commands

(defun agent-chat-toggle-markup-hiding ()
  "Toggle hiding of markdown delimiters and refontify."
  (interactive)
  (setq agent-chat-hide-markup (not agent-chat-hide-markup))
  (font-lock-flush)
  (message "Markup hiding %s" (if agent-chat-hide-markup "enabled" "disabled")))

;;; Base keymap

(defvar agent-chat-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ignore) ;; placeholder, overridden per-mode
    map)
  "Base keymap inherited by chat modes.")

(provide 'agent-chat)
;;; agent-chat.el ends here
