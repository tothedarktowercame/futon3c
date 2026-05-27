;;; smart-cursor.el --- Emacs client for the emacs-cursor peripheral -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)
(require 'trace)
(require 'url)

(declare-function websocket-open "websocket" (url &rest args))
(declare-function websocket-openp "websocket" (websocket))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-close "websocket" (websocket))
(declare-function websocket-frame-text "websocket" (frame))
(declare-function websocket-frame-p "websocket" (frame))

(defgroup smart-cursor nil
  "Emacs client for the futon3c emacs-cursor peripheral."
  :group 'applications)

(defcustom smart-cursor-ws-url "ws://localhost:7070/agency/ws"
  "WebSocket URL for the futon3c Agency endpoint."
  :type 'string
  :group 'smart-cursor)

(defcustom smart-cursor-http-url nil
  "HTTP base URL for futon3c introspection.
When nil, derives from `smart-cursor-ws-url'."
  :type '(choice (const nil) string)
  :group 'smart-cursor)

(defcustom smart-cursor-agent-id "claude-1"
  "Agent ID to attach the smart cursor client to.
This must be an agent already known to the futon3c registry."
  :type 'string
  :group 'smart-cursor)

(defcustom smart-cursor-editor-id nil
  "Stable editor identifier for this Emacs session.
When nil, derives from `server-name' or `system-name'."
  :type '(choice (const nil) string)
  :group 'smart-cursor)

(defcustom smart-cursor-debug-buffer-name "*Smart Cursor*"
  "Name of the local smart-cursor debug buffer."
  :type 'string
  :group 'smart-cursor)

(defcustom smart-cursor-remote-glyph "◆"
  "Glyph used to display the remote cursor."
  :type 'string
  :group 'smart-cursor)

(defcustom smart-cursor-caption-enabled t
  "Whether to show a short visible caption beside the companion cursor."
  :type 'boolean
  :group 'smart-cursor)

(defcustom smart-cursor-jump-caption-seconds 4.0
  "How long a recent jump timing stays visible in the companion caption."
  :type 'number
  :group 'smart-cursor)

(defcustom smart-cursor-transition-history-limit 12
  "Maximum number of recent buffer/window transitions to retain."
  :type 'integer
  :group 'smart-cursor)

(defcustom smart-cursor-transition-fast-ms 80
  "Upper bound in milliseconds for a transition classified as `fast'."
  :type 'integer
  :group 'smart-cursor)

(defcustom smart-cursor-transition-warm-ms 220
  "Upper bound in milliseconds for a transition classified as `warm'."
  :type 'integer
  :group 'smart-cursor)

(defcustom smart-cursor-beacon-interval-seconds 1.0
  "Interval in seconds between heartbeat beacon samples."
  :type 'number
  :group 'smart-cursor)

(defcustom smart-cursor-beacon-history-limit 20
  "Maximum number of recent heartbeat beacon samples to retain."
  :type 'integer
  :group 'smart-cursor)

(defcustom smart-cursor-trace-buffer-name "*smart-cursor-trace*"
  "Buffer name for interleaved smart-cursor and native Emacs trace output."
  :type 'string
  :group 'smart-cursor)

(defcustom smart-cursor-native-trace-functions
  '(switch-to-buffer set-window-buffer select-window other-window)
  "Functions traced into `smart-cursor-trace-buffer-name' when enabled."
  :type '(repeat function)
  :group 'smart-cursor)

(defcustom smart-cursor-send-idle-delay 0.15
  "Idle delay before shipping a fresh cursor-context snapshot."
  :type 'number
  :group 'smart-cursor)

(defcustom smart-cursor-visible-text-limit 2400
  "Maximum visible text sent in each cursor-context snapshot."
  :type 'integer
  :group 'smart-cursor)

(defface smart-cursor-remote-cursor-face
  '((t :foreground "#ff4fa3" :weight bold))
  "Face for the remote cursor glyph."
  :group 'smart-cursor)

(defface smart-cursor-remote-region-face
  '((t :background "#ff4fa3" :extend t :inherit shadow))
  "Face for the remote cursor region."
  :group 'smart-cursor)

(defface smart-cursor-caption-face
  '((t :foreground "#1b1b1b"
       :background "#ff9fcb"
       :box (:line-width (-1 . -1) :color "#ff9fcb")
       :height 0.9))
  "Face for the visible companion cursor caption."
  :group 'smart-cursor)

(defvar smart-cursor--ws nil)
(defvar smart-cursor--connected nil)
(defvar smart-cursor--ready nil)
(defvar smart-cursor--peripheral-started nil)
(defvar smart-cursor--session-id nil)
(defvar smart-cursor--send-timer nil)
(defvar smart-cursor--remote-state nil)
(defvar smart-cursor--last-context nil)
(defvar smart-cursor--last-event nil)
(defvar smart-cursor--last-error nil)
(defvar smart-cursor--last-minibuffer nil)
(defvar smart-cursor--ws-received-at-ms nil
  "Dynamically bound to the ms timestamp at which the current WS frame
arrived in `smart-cursor--on-message'. Available to downstream handlers
that need a T2 boundary stamp for paired in-system timing runs.")
(defvar smart-cursor--last-transition nil)
(defvar smart-cursor--recent-transitions nil)
(defvar smart-cursor--recent-beacons nil)
(defvar smart-cursor--beacon-timer nil)
(defvar smart-cursor--native-trace-enabled nil)
(defvar smart-cursor--command-start-time nil)
(defvar smart-cursor--command-start-buffer nil)
(defvar smart-cursor--command-start-window nil)
(defvar smart-cursor--command-start-command nil)
(defvar smart-cursor--debug-request nil)
(defvar smart-cursor--buffer-overlays (make-hash-table :test #'eq))

(defvar smart-cursor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s c") #'smart-cursor-connect)
    (define-key map (kbd "C-c C-s d") #'smart-cursor-disconnect)
    (define-key map (kbd "C-c C-s b") #'smart-cursor-show-buffer)
    (define-key map (kbd "C-c C-s m") #'smart-cursor-set-mode)
    (define-key map (kbd "C-c C-s n") #'smart-cursor-toggle-native-trace)
    (define-key map (kbd "C-c C-s s") #'smart-cursor-send-context-now)
    (define-key map (kbd "C-c C-s t") #'smart-cursor-show-transitions)
    (define-key map (kbd "C-c C-s T") #'smart-cursor-show-trace)
    map)
  "Keymap for `smart-cursor-mode'.")

(defvar smart-cursor-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'smart-cursor-show-buffer)
    (define-key map (kbd "c") #'smart-cursor-connect)
    (define-key map (kbd "d") #'smart-cursor-disconnect)
    (define-key map (kbd "m") #'smart-cursor-set-mode)
    (define-key map (kbd "n") #'smart-cursor-toggle-native-trace)
    (define-key map (kbd "s") #'smart-cursor-send-context-now)
    (define-key map (kbd "t") #'smart-cursor-show-transitions)
    (define-key map (kbd "T") #'smart-cursor-show-trace)
    map)
  "Keymap for `smart-cursor-inspector-mode'.")

(define-derived-mode smart-cursor-inspector-mode special-mode "Smart-Cursor"
  "Major mode for the smart-cursor debug buffer.")

(defun smart-cursor--require-websocket ()
  "Ensure websocket support is available."
  (unless (featurep 'websocket)
    (require 'websocket nil t))
  (unless (featurep 'websocket)
    (user-error "websocket.el is required for smart-cursor"))
  (require 'websocket-fix nil t))

(defun smart-cursor--editor-id ()
  "Return the current smart-cursor editor id."
  (or smart-cursor-editor-id
      (and (boundp 'server-name) server-name)
      (system-name)
      "emacs"))

(defun smart-cursor--http-base-url ()
  "Return the futon3c HTTP base URL."
  (or smart-cursor-http-url
      (let ((base (replace-regexp-in-string "\\`ws\\(s?\\)://" "http\\1://" smart-cursor-ws-url)))
        (replace-regexp-in-string "/agency/ws\\'" "" base))))

(defun smart-cursor--new-session-id ()
  "Return a fresh local session identifier."
  (format "smart-cursor-%s-%d"
          (format-time-string "%Y%m%d%H%M%S")
          (emacs-pid)))

(defun smart-cursor--trim-text (text limit)
  "Trim TEXT to LIMIT characters."
  (if (and text (> (length text) limit))
      (concat (substring text 0 limit) "...")
    (or text "")))

(defun smart-cursor--now-ms ()
  "Return current time in milliseconds."
  (round (* 1000.0 (float-time))))

(defun smart-cursor--trace-buffer ()
  "Return the shared smart-cursor trace buffer."
  (get-buffer-create smart-cursor-trace-buffer-name))

(defun smart-cursor--append-trace-line (tag payload)
  "Append a timestamped trace line with TAG and PAYLOAD."
  (with-current-buffer (smart-cursor--trace-buffer)
    (goto-char (point-max))
    (insert (format "[%s] [%s] %s\n"
                    (format-time-string "%FT%T.%3N%z")
                    tag
                    payload))))

(defun smart-cursor--alist-get (key alist)
  "Get KEY from ALIST or hash table using symbol or string variants."
  (cond
   ((hash-table-p alist)
    (or (gethash key alist)
        (gethash (if (symbolp key) (symbol-name key) key) alist)
        (and (stringp key) (gethash (intern key) alist))
        (and (symbolp key) (gethash (intern (symbol-name key)) alist))))
   (t
    (or (alist-get key alist)
        (alist-get (if (symbolp key) (symbol-name key) key) alist nil nil #'equal)))))

(defun smart-cursor--json-encode (value)
  "Encode VALUE as JSON."
  (json-encode value))

(defun smart-cursor--read-json-response (url)
  "Fetch URL synchronously and parse its JSON body as an alist."
  (let ((buffer (url-retrieve-synchronously url t t 5)))
    (unless buffer
      (user-error "Smart Cursor could not reach %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (user-error "Smart Cursor got a malformed HTTP response from %s" url))
          (json-parse-buffer :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
      (kill-buffer buffer))))

(defun smart-cursor--read-json-hash-response (url)
  "Fetch URL synchronously and parse its JSON body as a hash table."
  (let ((buffer (url-retrieve-synchronously url t t 5)))
    (unless buffer
      (user-error "Smart Cursor could not reach %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (user-error "Smart Cursor got a malformed HTTP response from %s" url))
          (json-parse-buffer :object-type 'hash-table
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
      (kill-buffer buffer))))

(defun smart-cursor--fetch-agent-record (agent-id)
  "Return the registry record for AGENT-ID, or nil."
  (let* ((url (format "%s/api/alpha/agents" (smart-cursor--http-base-url)))
         (payload (smart-cursor--read-json-hash-response url))
         (agents (and (hash-table-p payload)
                      (gethash "agents" payload))))
    (and (hash-table-p agents)
         (gethash agent-id agents))))

(defun smart-cursor--assert-safe-agent! (agent-id)
  "Refuse unsafe bridge attachment to AGENT-ID.
The smart-cursor bridge is currently safe only for local agents that do not
already have a WS bridge, because WS sender registration is singleton by
agent-id."
  (let* ((url (format "%s/api/alpha/agents" (smart-cursor--http-base-url)))
         (payload (smart-cursor--read-json-hash-response url))
         (agents (and (hash-table-p payload)
                      (gethash "agents" payload)))
         (record (and (hash-table-p agents)
                      (gethash agent-id agents))))
    (unless record
      (user-error "Smart Cursor target agent is not registered: %s" agent-id))
    (let ((invoke-local (smart-cursor--alist-get 'invoke-local? record))
          (invoke-ws (smart-cursor--alist-get 'invoke-ws-available? record))
          (route (smart-cursor--alist-get 'invoke-route record)))
      (when invoke-ws
        (user-error "Smart Cursor refuses to attach to %s: an existing WS bridge is active" agent-id))
      (unless invoke-local
        (user-error "Smart Cursor currently supports only local agents; %s routes via %s"
                    agent-id
                    (or route "unknown"))))
    record))

(defun smart-cursor--send-json (payload)
  "Send PAYLOAD over the active WebSocket."
  (when (and smart-cursor--ws
             (websocket-openp smart-cursor--ws))
    (websocket-send-text smart-cursor--ws
                         (smart-cursor--json-encode payload))))

(defun smart-cursor--send-ready ()
  "Complete the WS readiness handshake."
  (smart-cursor--send-json
   `((type . "ready")
     (agent_id . ,smart-cursor-agent-id)
     (session_id . ,smart-cursor--session-id))))

(defun smart-cursor--send-peripheral-start ()
  "Start the emacs-cursor peripheral on the current WS connection."
  (smart-cursor--send-json
   '(("type" . "peripheral_start")
     ("peripheral_id" . "emacs-cursor"))))

(defun smart-cursor--send-tool-action (tool args)
  "Send TOOL and ARGS as a tool_action frame."
  (smart-cursor--send-json
   `((type . "tool_action")
     (tool . ,tool)
     (args . ,args))))

(defun smart-cursor--local-mark ()
  "Return the current active mark, or nil."
  (when (use-region-p)
    (mark)))

(defun smart-cursor--visible-bounds ()
  "Return a safe visible window range as (START . END)."
  (let* ((win-start (window-start))
         (win-end-raw (window-end nil t))
         (point-max* (point-max))
         (start (max (point-min) (min win-start point-max*)))
         (end-candidate (max (point-min) (min win-end-raw point-max*)))
         (end (max start end-candidate)))
    (cons start end)))

(defun smart-cursor--visible-text ()
  "Return the current window's visible text."
  (pcase-let ((`(,start . ,end) (smart-cursor--visible-bounds)))
    (let ((text (buffer-substring-no-properties start end)))
      (smart-cursor--trim-text text smart-cursor-visible-text-limit))))

(defun smart-cursor--current-buffer-map ()
  "Return a structured description of the current buffer/window."
  (pcase-let ((`(,start . ,end) (smart-cursor--visible-bounds)))
    `((name . ,(buffer-name))
      (file . ,(buffer-file-name))
      (major-mode . ,(symbol-name major-mode))
      (read-only . ,buffer-read-only)
      (modified . ,(buffer-modified-p))
      (visible . ((start . ,start)
                  (end . ,end)
                  (text . ,(smart-cursor--visible-text)))))))

(defun smart-cursor--user-cursor-map ()
  "Return a structured description of the local cursor."
  `((point . ,(point))
    (line . ,(line-number-at-pos))
    (column . ,(current-column))
    (mark . ,(smart-cursor--local-mark))))

(defun smart-cursor--remote-cursor-map ()
  "Return the latest known remote cursor/focus."
  (let* ((state smart-cursor--remote-state)
         (buffer-surface (smart-cursor--alist-get 'buffer-surface state))
         (agent-cursor (and (listp buffer-surface)
                            (smart-cursor--alist-get 'agent-cursor buffer-surface)))
         (focus (smart-cursor--alist-get 'focus state)))
    (cond
     ((listp agent-cursor) agent-cursor)
     ((listp focus) focus)
     (t nil))))

(defun smart-cursor--companion-cursor-map ()
  "Return the cursor position for the visible companion cursor.
Prefer a true remote cursor, but in `follow` mode fall back to the user's
current cursor so the embodiment remains visible on screen."
  (let* ((state smart-cursor--remote-state)
         (buffer-surface (smart-cursor--alist-get 'buffer-surface state))
         (mode (smart-cursor--alist-get 'mode state))
         (user-cursor (or (smart-cursor--alist-get 'user-cursor state)
                          (and (listp buffer-surface)
                               (smart-cursor--alist-get 'user-cursor buffer-surface))))
         (remote-cursor (smart-cursor--remote-cursor-map)))
    (cond
     ((smart-cursor--alist-get 'point remote-cursor) remote-cursor)
     ((and (equal mode "follow") (listp user-cursor)) user-cursor)
     (t nil))))

(defun smart-cursor--caption-string ()
  "Return a short visible caption for the companion cursor."
  (when smart-cursor-caption-enabled
    (let* ((state smart-cursor--remote-state)
           (mode (or (smart-cursor--alist-get 'mode state) "follow"))
           (minibuffer-surface (smart-cursor--alist-get 'minibuffer-surface state))
           (status (and (listp minibuffer-surface)
                        (smart-cursor--alist-get 'status minibuffer-surface)))
           (transition smart-cursor--last-transition)
           (transition-age-ms (and transition
                                   (numberp (smart-cursor--alist-get 'at-ms transition))
                                   (- (smart-cursor--now-ms)
                                      (smart-cursor--alist-get 'at-ms transition))))
           (recent-transition
            (and transition-age-ms
                 (<= transition-age-ms (* 1000 smart-cursor-jump-caption-seconds))
                 transition))
           (label
            (if recent-transition
                (format " %s %s %s %sms %s "
                        smart-cursor-agent-id
                        mode
                        (or (smart-cursor--alist-get 'band recent-transition) "?")
                        (or (smart-cursor--alist-get 'elapsed-ms recent-transition) "?")
                        (or (smart-cursor--alist-get 'command recent-transition) "jump"))
              (format " %s %s%s "
                      smart-cursor-agent-id
                      mode
                      (if status
                          (format " [%s]" status)
                        "")))))
      (propertize label 'face 'smart-cursor-caption-face))))

(defun smart-cursor--record-command-start ()
  "Capture timing baselines for the next command."
  (when smart-cursor-mode
    (setq smart-cursor--command-start-time (smart-cursor--now-ms)
          smart-cursor--command-start-buffer (buffer-name)
          smart-cursor--command-start-window (selected-window)
          smart-cursor--command-start-command (format "%s" this-command))))

(defun smart-cursor--remember-transition (transition)
  "Add TRANSITION to the recent transition ring."
  (let ((limit (max 1 smart-cursor-transition-history-limit)))
    (setq smart-cursor--recent-transitions
          (seq-take (cons transition smart-cursor--recent-transitions) limit))))

(defun smart-cursor--remember-beacon (beacon)
  "Add BEACON to the recent heartbeat ring."
  (let ((limit (max 1 smart-cursor-beacon-history-limit)))
    (setq smart-cursor--recent-beacons
          (seq-take (cons beacon smart-cursor--recent-beacons) limit))))

(defun smart-cursor-enable-native-trace ()
  "Enable native Emacs tracing for key buffer/window functions."
  (interactive)
  (unless smart-cursor--native-trace-enabled
    (dolist (fn smart-cursor-native-trace-functions)
      (trace-function-background fn smart-cursor-trace-buffer-name))
    (setq smart-cursor--native-trace-enabled t)
    (smart-cursor--append-trace-line
     "smart-cursor"
     (format "native-trace enabled for %S" smart-cursor-native-trace-functions))))

(defun smart-cursor-disable-native-trace ()
  "Disable native Emacs tracing previously enabled by smart-cursor."
  (interactive)
  (when smart-cursor--native-trace-enabled
    (dolist (fn smart-cursor-native-trace-functions)
      (ignore-errors (untrace-function fn)))
    (setq smart-cursor--native-trace-enabled nil)
    (smart-cursor--append-trace-line "smart-cursor" "native-trace disabled")))

(defun smart-cursor-toggle-native-trace ()
  "Toggle native Emacs tracing for smart-cursor instrumentation."
  (interactive)
  (if smart-cursor--native-trace-enabled
      (smart-cursor-disable-native-trace)
    (smart-cursor-enable-native-trace))
  (smart-cursor--render-debug-buffer)
  (message "[Smart Cursor] native trace %s"
           (if smart-cursor--native-trace-enabled "enabled" "disabled")))

(defun smart-cursor--transition-band (elapsed-ms)
  "Classify ELAPSED-MS as `fast', `warm', or `slow'."
  (cond
   ((<= elapsed-ms smart-cursor-transition-fast-ms) "fast")
   ((<= elapsed-ms smart-cursor-transition-warm-ms) "warm")
   (t "slow")))

(defun smart-cursor--capture-beacon ()
  "Capture a heartbeat beacon sample and publish fresh context."
  (when (and smart-cursor-mode
             smart-cursor--connected
             smart-cursor--ready
             smart-cursor--peripheral-started
             (not (minibufferp)))
    (let ((beacon `((at . ,(format-time-string "%FT%T%z"))
                    (at-ms . ,(smart-cursor--now-ms))
                    (buffer . ,(buffer-name))
                    (point . ,(point))
                    (line . ,(line-number-at-pos))
                    (column . ,(current-column))
                    (window-start . ,(window-start))
                    (window-end . ,(cdr (smart-cursor--visible-bounds)))
                    (command . ,(format "%s" real-last-command)))))
      (smart-cursor--remember-beacon beacon)
      (smart-cursor--append-trace-line
       "beacon"
       (format "%S" beacon))
      (smart-cursor-send-context-now)
      (smart-cursor--render-debug-buffer))))

(defun smart-cursor--record-transition-if-needed ()
  "Record a buffer/window jump caused by the just-finished command."
  (when (and smart-cursor-mode
             smart-cursor--command-start-time
             (not (minibufferp)))
    (let* ((from-buffer smart-cursor--command-start-buffer)
           (to-buffer (buffer-name))
           (from-window smart-cursor--command-start-window)
           (to-window (selected-window))
           (buffer-changed (not (equal from-buffer to-buffer)))
           (window-changed (not (eq from-window to-window)))
           (elapsed-ms (max 0 (- (smart-cursor--now-ms)
                                 smart-cursor--command-start-time))))
      (when (or buffer-changed window-changed)
        (let ((transition
               `((kind . ,(cond
                           ((and buffer-changed window-changed) "buffer+window")
                           (buffer-changed "buffer")
                           (t "window")))
                 (command . ,smart-cursor--command-start-command)
                 (from-buffer . ,from-buffer)
                 (to-buffer . ,to-buffer)
                 (elapsed-ms . ,elapsed-ms)
                 (band . ,(smart-cursor--transition-band elapsed-ms))
                 (at . ,(format-time-string "%FT%T%z"))
                 (at-ms . ,(smart-cursor--now-ms)))))
          (setq smart-cursor--last-transition transition)
          (smart-cursor--remember-transition transition)
          (smart-cursor--append-trace-line
           "transition"
           (format "%S" transition)))
        (smart-cursor--schedule-context-send)
        (smart-cursor--render-debug-buffer)))))

(defun smart-cursor--buffer-surface-summary (buffer-map user-cursor remote-cursor)
  "Build a concise buffer surface summary string."
  (format "buffer=%s file=%s user=(line %s col %s point %s) remote=%s"
          (or (smart-cursor--alist-get 'name buffer-map) "?")
          (or (smart-cursor--alist-get 'file buffer-map) "nil")
          (or (smart-cursor--alist-get 'line user-cursor) "?")
          (or (smart-cursor--alist-get 'column user-cursor) "?")
          (or (smart-cursor--alist-get 'point user-cursor) "?")
          (if (listp remote-cursor)
              (format "(line %s col %s point %s)"
                      (or (smart-cursor--alist-get 'line remote-cursor) "?")
                      (or (smart-cursor--alist-get 'column remote-cursor) "?")
                      (or (smart-cursor--alist-get 'point remote-cursor) "?"))
            "nil")))

(defun smart-cursor--debug-map ()
  "Return structured debug information for the current snapshot."
  `((requested . ,smart-cursor--debug-request)
    (connected . ,smart-cursor--connected)
    (ready . ,smart-cursor--ready)
    (peripheral-started . ,smart-cursor--peripheral-started)
    (editor-id . ,(smart-cursor--editor-id))
    (session-id . ,smart-cursor--session-id)
    (last-transition . ,smart-cursor--last-transition)
    (recent-transitions . ,smart-cursor--recent-transitions)
    (recent-beacons . ,smart-cursor--recent-beacons)
    (selected-window . ,(format "%s" (selected-window)))
    (current-time . ,(format-time-string "%FT%T%z"))))

(defun smart-cursor--minibuffer-surface ()
  "Return the local minibuffer surface state."
  (or smart-cursor--last-minibuffer
      '((status . "idle"))))

(defun smart-cursor--build-context ()
  "Build the current cursor-context payload."
  (let* ((buffer-map (smart-cursor--current-buffer-map))
         (user-cursor (smart-cursor--user-cursor-map))
         (remote-cursor (smart-cursor--remote-cursor-map))
         (debug-map (smart-cursor--debug-map)))
    `((editor-id . ,(smart-cursor--editor-id))
      (buffer . ,buffer-map)
      (user-cursor . ,user-cursor)
      (agent-cursor . ,remote-cursor)
      (window . ((selected . t)))
      (minibuffer . ,(smart-cursor--minibuffer-surface))
      (buffer-surface . ,(smart-cursor--buffer-surface-summary
                          buffer-map user-cursor remote-cursor))
      (debug . ,debug-map))))

(defun smart-cursor--overlay-entry (buffer)
  "Return or create the remote cursor overlay entry for BUFFER."
  (or (gethash buffer smart-cursor--buffer-overlays)
      (let ((entry (list :cursor (make-overlay 1 1 buffer nil t)
                         :region (make-overlay 1 1 buffer nil t))))
        (overlay-put (plist-get entry :cursor) 'smart-cursor t)
        (overlay-put (plist-get entry :region) 'face 'smart-cursor-remote-region-face)
        (overlay-put (plist-get entry :region) 'smart-cursor t)
        (puthash buffer entry smart-cursor--buffer-overlays)
        entry)))

(defun smart-cursor--clear-overlays ()
  "Delete all smart-cursor overlays."
  (maphash
   (lambda (_buffer entry)
     (delete-overlay (plist-get entry :cursor))
     (delete-overlay (plist-get entry :region)))
   smart-cursor--buffer-overlays)
  (clrhash smart-cursor--buffer-overlays))

(defun smart-cursor--render-remote-state ()
  "Render the latest remote state into overlays."
  (let* ((state smart-cursor--remote-state)
         (buffer-map (and (listp state) (smart-cursor--alist-get 'buffer state)))
         (buffer-name (and (listp buffer-map)
                           (smart-cursor--alist-get 'name buffer-map)))
         (companion-cursor (smart-cursor--companion-cursor-map))
         (caption (smart-cursor--caption-string)))
    (smart-cursor--clear-overlays)
    (when-let* ((buffer-name buffer-name)
                (buffer (get-buffer buffer-name))
                ((listp companion-cursor))
                (point (smart-cursor--alist-get 'point companion-cursor)))
      (with-current-buffer buffer
        (let* ((entry (smart-cursor--overlay-entry buffer))
               (cursor-ov (plist-get entry :cursor))
               (region-ov (plist-get entry :region))
               (mark (smart-cursor--alist-get 'mark companion-cursor))
               (pt (max (point-min) (min point (point-max))))
               (mk (and (numberp mark)
                        (max (point-min) (min mark (point-max))))))
          (overlay-put cursor-ov
                       'after-string
                       (concat (propertize smart-cursor-remote-glyph
                                           'face 'smart-cursor-remote-cursor-face)
                               (or caption "")))
          (move-overlay cursor-ov pt pt buffer)
          (if mk
              (move-overlay region-ov (min pt mk) (max pt mk) buffer)
            (delete-overlay region-ov)))))))

(defun smart-cursor--render-debug-buffer ()
  "Refresh the local smart-cursor debug buffer."
  (let ((buf (get-buffer-create smart-cursor-debug-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'smart-cursor-inspector-mode)
        (smart-cursor-inspector-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Smart Cursor\n============\n\n"))
        (insert (format "connected:          %s\n" smart-cursor--connected))
        (insert (format "ready:              %s\n" smart-cursor--ready))
        (insert (format "peripheral-started: %s\n" smart-cursor--peripheral-started))
        (insert (format "agent-id:           %s\n" smart-cursor-agent-id))
        (insert (format "editor-id:          %s\n" (smart-cursor--editor-id)))
        (insert (format "session-id:         %s\n\n" smart-cursor--session-id))
        (insert (format "native-trace:       %s\n" smart-cursor--native-trace-enabled))
        (insert (format "trace-buffer:       %s\n\n" smart-cursor-trace-buffer-name))
        (insert "last-transition:\n")
        (insert (pp-to-string smart-cursor--last-transition))
        (insert "\nrecent-transitions:\n")
        (insert (pp-to-string smart-cursor--recent-transitions))
        (insert "\nrecent-beacons:\n")
        (insert (pp-to-string smart-cursor--recent-beacons))
        (insert "\n")
        (insert "last-context:\n")
        (insert (pp-to-string smart-cursor--last-context))
        (insert "\nremote-state:\n")
        (insert (pp-to-string smart-cursor--remote-state))
        (insert "\nlast-event:\n")
        (insert (pp-to-string smart-cursor--last-event))
        (insert "\nlast-error:\n")
        (insert (pp-to-string smart-cursor--last-error))
        (goto-char (point-min)))))
  (when-let ((window (get-buffer-window smart-cursor-debug-buffer-name)))
    (set-window-point window (point-min))))

(defun smart-cursor-show-buffer ()
  "Display the smart-cursor debug buffer."
  (interactive)
  (smart-cursor--render-debug-buffer)
  (pop-to-buffer smart-cursor-debug-buffer-name))

(defun smart-cursor-show-transitions ()
  "Display the smart-cursor debug buffer focused on transition history."
  (interactive)
  (smart-cursor--render-debug-buffer)
  (pop-to-buffer smart-cursor-debug-buffer-name)
  (goto-char (point-min))
  (when (search-forward "recent-transitions:" nil t)
    (beginning-of-line)))

(defun smart-cursor-show-trace ()
  "Display the interleaved smart-cursor/native trace buffer."
  (interactive)
  (pop-to-buffer (smart-cursor--trace-buffer)))

(defun smart-cursor--maybe-send-context ()
  "Send a fresh cursor-context snapshot when the peripheral is active."
  (setq smart-cursor--send-timer nil)
  (when (and smart-cursor-mode
             smart-cursor--connected
             smart-cursor--ready
             smart-cursor--peripheral-started
             (not (minibufferp)))
    (let ((context (smart-cursor--build-context)))
      (setq smart-cursor--last-context context)
      (smart-cursor--send-tool-action "cursor-context" (vector context))
      (smart-cursor--render-debug-buffer))))

(defun smart-cursor--schedule-context-send (&rest _)
  "Schedule a debounced cursor-context update."
  (when smart-cursor--send-timer
    (cancel-timer smart-cursor--send-timer))
  (setq smart-cursor--send-timer
        (run-with-idle-timer smart-cursor-send-idle-delay nil
                             #'smart-cursor--maybe-send-context)))

(defun smart-cursor-send-context-now ()
  "Send a cursor-context snapshot immediately."
  (interactive)
  (when smart-cursor--send-timer
    (cancel-timer smart-cursor--send-timer)
    (setq smart-cursor--send-timer nil))
  (smart-cursor--maybe-send-context))

(defun smart-cursor--send-minibuffer-response (payload)
  "Send PAYLOAD back through the cursor-minibuffer tool."
  (setq smart-cursor--last-minibuffer payload)
  (smart-cursor--send-tool-action "cursor-minibuffer" (vector payload))
  (smart-cursor--schedule-context-send))

(defun smart-cursor--format-elisp-value (value)
  "Render VALUE to a stable printable string."
  (let ((print-length nil)
        (print-level nil))
    (prin1-to-string value)))

(defun smart-cursor--send-eval-result (request-id sexp status payload)
  "Send a structured eval result for SEXP under REQUEST-ID."
  (smart-cursor--send-minibuffer-response
   `((request-id . ,request-id)
     (command . "eval-sexp")
     (sexp . ,sexp)
     (status . ,status)
     ,@payload
     (at . ,(format-time-string "%FT%T%z")))))

(defun smart-cursor--send-script-result (request-id status payload)
  "Send a structured run-script result under REQUEST-ID."
  (smart-cursor--send-minibuffer-response
   `((request-id . ,request-id)
     (command . "run-script")
     (status . ,status)
     ,@payload
     (at . ,(format-time-string "%FT%T%z")))))

(defun smart-cursor--script-step-map (raw)
  "Normalize RAW script step to an alist."
  (cond
   ((hash-table-p raw)
    (let (alist)
      (maphash (lambda (k v)
                 (push (cons (if (symbolp k) k (intern k)) v) alist))
               raw)
      alist))
   ((listp raw) raw)
   (t nil)))

(defun smart-cursor--execute-script-step (step)
  "Execute one non-editing smart-cursor script STEP."
  (let* ((spec (smart-cursor--script-step-map step))
         (op (or (smart-cursor--alist-get 'op spec)
                 (smart-cursor--alist-get 'command spec))))
    (pcase op
      ((or "switch-buffer" :switch-buffer)
       (let* ((buffer-name (smart-cursor--alist-get 'buffer spec))
              (buffer (and (stringp buffer-name) (get-buffer buffer-name))))
         (unless buffer
           (user-error "run-script switch-buffer target not found: %s" buffer-name))
         (switch-to-buffer buffer)
         `((op . "switch-buffer") (buffer . ,buffer-name))))
      ((or "goto-char" :goto-char)
       (let* ((point (smart-cursor--alist-get 'point spec))
              (pt (and (numberp point)
                       (max (point-min) (min point (point-max))))))
         (unless pt
           (user-error "run-script goto-char requires numeric point"))
         (goto-char pt)
         `((op . "goto-char") (point . ,(point)))))
      ((or "goto-line" :goto-line)
       (let ((line (smart-cursor--alist-get 'line spec)))
         (unless (numberp line)
           (user-error "run-script goto-line requires numeric line"))
         (goto-char (point-min))
         (forward-line (max 0 (1- line)))
         `((op . "goto-line") (line . ,(line-number-at-pos)) (point . ,(point)))))
      ((or "forward-line" :forward-line)
       (let ((count (or (smart-cursor--alist-get 'count spec) 1)))
         (unless (numberp count)
           (user-error "run-script forward-line requires numeric count"))
         (forward-line count)
         `((op . "forward-line") (count . ,count) (line . ,(line-number-at-pos)) (point . ,(point)))))
      ((or "forward-word" :forward-word)
       (let ((count (or (smart-cursor--alist-get 'count spec) 1)))
         (unless (numberp count)
           (user-error "run-script forward-word requires numeric count"))
         (forward-word count)
         `((op . "forward-word")
           (count . ,count)
           (point . ,(point))
           (line . ,(line-number-at-pos))
           (column . ,(current-column)))))
      ((or "backward-word" :backward-word)
       (let ((count (or (smart-cursor--alist-get 'count spec) 1)))
         (unless (numberp count)
           (user-error "run-script backward-word requires numeric count"))
         (backward-word count)
         `((op . "backward-word")
           (count . ,count)
           (point . ,(point))
           (line . ,(line-number-at-pos))
           (column . ,(current-column)))))
      ((or "read-word" :read-word "word-at-point" :word-at-point)
       `((op . "read-word")
         (word . ,(thing-at-point 'word t))
         (point . ,(point))
         (line . ,(line-number-at-pos))
         (column . ,(current-column))))
      ((or "recenter" :recenter)
       (recenter)
       '((op . "recenter")))
      ((or "wait" :wait)
       (let ((seconds (or (smart-cursor--alist-get 'seconds spec) 0.1)))
         (unless (numberp seconds)
           (user-error "run-script wait requires numeric seconds"))
         (sleep-for seconds)
         `((op . "wait") (seconds . ,seconds))))
      ((or "snapshot" :snapshot)
       `((op . "snapshot")
         (buffer . ,(smart-cursor--current-buffer-map))
         (user-cursor . ,(smart-cursor--user-cursor-map))))
      (_
       (user-error "run-script unsupported op: %s" op)))))

(defun smart-cursor--execute-script-step-timed (step)
  "Execute STEP and attach timing and optional label metadata."
  (let* ((spec (smart-cursor--script-step-map step))
         (label (smart-cursor--alist-get 'label spec))
         (started-at-ms (smart-cursor--now-ms))
         (started-at (format-time-string "%FT%T%z"))
         (result (smart-cursor--execute-script-step step))
         (ended-at-ms (smart-cursor--now-ms)))
    `((started-at . ,started-at)
      (started-at-ms . ,started-at-ms)
      (elapsed-ms . ,(max 0 (- ended-at-ms started-at-ms)))
      ,@(when label `((label . ,label)))
      ,@result)))

(defun smart-cursor--execute-run-script (payload)
  "Execute a bounded non-editing script PAYLOAD for Emacs e2e driving."
  (let* ((request-id (smart-cursor--alist-get 'request-id payload))
         (steps (smart-cursor--alist-get 'steps payload))
         (script-name (or (smart-cursor--alist-get 'name payload)
                          (smart-cursor--alist-get 'script payload)
                          "run-script"))
         (server-sent-at-ms (smart-cursor--alist-get 'server-sent-at-ms payload))
         (ws-received-at-ms smart-cursor--ws-received-at-ms)
         (trace-outfile (smart-cursor--alist-get 'trace-outfile payload)))
    (condition-case err
        (progn
          (unless (listp steps)
            (user-error "run-script requires a list of steps"))
          (let ((results nil)
                (script-start-ms (smart-cursor--now-ms))
                (script-start-at (format-time-string "%FT%T%z")))
            (dolist (step steps)
              (let ((result (smart-cursor--execute-script-step-timed step)))
                (push result results)
                (smart-cursor--append-trace-line
                 "run-step"
                 (format "%S" result))))
            (let* ((ordered-results (nreverse results))
                   (script-end-ms (smart-cursor--now-ms))
                   (reply-payload
                    `((name . ,script-name)
                      (started-at . ,script-start-at)
                      (started-at-ms . ,script-start-ms)
                      (elapsed-ms . ,(max 0 (- script-end-ms script-start-ms)))
                      ,@(when server-sent-at-ms
                          `((server-sent-at-ms . ,server-sent-at-ms)))
                      ,@(when ws-received-at-ms
                          `((ws-received-at-ms . ,ws-received-at-ms)))
                      (results . ,ordered-results)
                      (buffer . ,(smart-cursor--current-buffer-map))
                      (user-cursor . ,(smart-cursor--user-cursor-map)))))
              (smart-cursor--append-trace-line
               "run-script"
               (format "%S" reply-payload))
              (when (stringp trace-outfile)
                (condition-case write-err
                    (with-temp-file trace-outfile
                      (let ((print-length nil)
                            (print-level nil))
                        (prin1
                         `((request-id . ,request-id)
                           (command . "run-script")
                           (status . "ok")
                           ,@reply-payload
                           (at . ,(format-time-string "%FT%T%z")))
                         (current-buffer))))
                  (error
                   (message "[Smart Cursor] run-script trace-outfile error: %s"
                            (error-message-string write-err)))))
              (smart-cursor--send-script-result
               request-id "ok" reply-payload))
            (smart-cursor-send-context-now)
            (message "[Smart Cursor] run-script ok")))
      (error
       (smart-cursor--send-script-result
        request-id "error"
        `((error . ,(error-message-string err))
          ,@(when server-sent-at-ms
              `((server-sent-at-ms . ,server-sent-at-ms)))
          ,@(when ws-received-at-ms
              `((ws-received-at-ms . ,ws-received-at-ms)))
          (buffer . ,(smart-cursor--current-buffer-map))
          (user-cursor . ,(smart-cursor--user-cursor-map))))
       (smart-cursor-send-context-now)
       (message "[Smart Cursor] run-script error: %s"
                (error-message-string err))))))

(defun smart-cursor--execute-eval-sexp (payload)
  "Execute an explicit `eval-sexp' PAYLOAD from the smart cursor bridge."
  (let* ((request-id (smart-cursor--alist-get 'request-id payload))
         (sexp (or (smart-cursor--alist-get 'sexp payload)
                   (smart-cursor--alist-get 'elisp payload)
                   (smart-cursor--alist-get 'expr payload))))
    (cond
     ((not (stringp sexp))
      (smart-cursor--send-eval-result
       request-id sexp "error"
       `((error . "eval-sexp requires a string `sexp' field"))))
     (t
      (condition-case err
          (let* ((read-result (read-from-string sexp))
                 (form (car read-result))
                 (index (cdr read-result))
                 (trailing (string-trim (substring sexp index)))
                 (value nil))
            (when (not (string-empty-p trailing))
              (user-error "eval-sexp expects exactly one form"))
            (setq value (eval form t))
            (smart-cursor--send-eval-result
             request-id sexp "ok"
             `((result . ,(smart-cursor--format-elisp-value value))
               (value-type . ,(format "%S" (type-of value)))
               (buffer . ,(smart-cursor--current-buffer-map))
               (user-cursor . ,(smart-cursor--user-cursor-map))))
            (smart-cursor-send-context-now)
            (message "[Smart Cursor] eval-sexp ok"))
        (error
         (smart-cursor--send-eval-result
          request-id sexp "error"
         `((error . ,(error-message-string err))
            (buffer . ,(smart-cursor--current-buffer-map))
            (user-cursor . ,(smart-cursor--user-cursor-map)))))
         (smart-cursor-send-context-now)
         (message "[Smart Cursor] eval-sexp error: %s"
                  (error-message-string err)))))))

(defun smart-cursor--handle-minibuffer-event (payload)
  "Handle a server-emitted minibuffer PAYLOAD."
  (let* ((prompt (or (smart-cursor--alist-get 'prompt payload)
                     (smart-cursor--alist-get 'message payload)
                     "Smart cursor command"))
         (initial (smart-cursor--alist-get 'initial payload))
         (request-id (smart-cursor--alist-get 'request-id payload))
         (command (smart-cursor--alist-get 'command payload))
         (mode (smart-cursor--alist-get 'mode payload))
         (capture (smart-cursor--alist-get 'capture payload)))
    (setq smart-cursor--last-minibuffer payload)
    (pcase command
      ((or "show-buffer" :show-buffer)
       (smart-cursor-show-buffer))
      ((or "refresh-context" :refresh-context)
       (smart-cursor-send-context-now))
      ((or "set-mode" :set-mode)
       (when mode
         (smart-cursor-set-mode mode)))
      ((or "run-script" :run-script)
       (smart-cursor--execute-run-script payload))
      ((or "eval-sexp" :eval-sexp)
       (smart-cursor--execute-eval-sexp payload))
      ((or "message" :message)
       (message "[Smart Cursor] %s" (or (smart-cursor--alist-get 'message payload) prompt)))
      (_ nil))
    (when (and (not (member command '("message" :message
                                      "run-script" :run-script
                                      "eval-sexp" :eval-sexp
                                      "refresh-context" :refresh-context
                                      "show-buffer" :show-buffer
                                      "set-mode" :set-mode)))
               (or capture (stringp prompt)))
      (run-at-time
       0 nil
       (lambda ()
         (let* ((response (when capture
                            (read-string (format "[Smart Cursor] %s: " prompt)
                                         initial)))
                (reply `((request-id . ,request-id)
                         (command . ,command)
                         (prompt . ,prompt)
                         (response . ,response)
                         (at . ,(format-time-string "%FT%T%z")))))
           (message "[Smart Cursor] %s" prompt)
           (smart-cursor--send-minibuffer-response reply)))))))

(defun smart-cursor--handle-peripheral-event (frame)
  "Handle a server-emitted peripheral_event FRAME."
  (let* ((event (smart-cursor--alist-get 'event frame))
         (payload (smart-cursor--alist-get 'payload frame)))
    (pcase event
      ("cursor_state"
       (setq smart-cursor--remote-state payload)
       (smart-cursor--render-remote-state))
      ("speak"
       (message "[Smart Cursor] %s"
                (or (smart-cursor--alist-get 'message payload) payload)))
      ("minibuffer"
       (smart-cursor--handle-minibuffer-event payload))
      (_
       (setq smart-cursor--last-error (list :unknown-event event :payload payload))))
    (smart-cursor--render-debug-buffer)))

(defun smart-cursor--on-open (_ws)
  "Handle WebSocket open."
  (setq smart-cursor--connected t
        smart-cursor--ready nil
        smart-cursor--peripheral-started nil
        smart-cursor--last-error nil)
  (smart-cursor--send-ready)
  (smart-cursor--render-debug-buffer))

(defun smart-cursor--on-message (_ws frame)
  "Handle incoming WebSocket FRAME."
  (when (and frame (websocket-frame-p frame))
    (let* ((smart-cursor--ws-received-at-ms (smart-cursor--now-ms))
           (text (websocket-frame-text frame))
           (json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol)
           (parsed (condition-case err
                       (json-parse-string text
                                          :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)
                     (error
                      (setq smart-cursor--last-error
                            (list :parse-error (error-message-string err)
                                  :payload text))
                      nil))))
      (when parsed
        (setq smart-cursor--last-event parsed)
        (pcase (smart-cursor--alist-get 'type parsed)
          ("ready_ack"
           (setq smart-cursor--ready t)
           (smart-cursor--send-peripheral-start))
          ("peripheral_started"
           (setq smart-cursor--peripheral-started t)
           (setq smart-cursor--session-id
                 (or (smart-cursor--alist-get 'session_id parsed)
                     smart-cursor--session-id))
           (smart-cursor-send-context-now))
          ("peripheral_event"
           (smart-cursor--handle-peripheral-event parsed))
          ("tool_result"
           (let ((result (smart-cursor--alist-get 'result parsed)))
             (when (and (listp result)
                        (smart-cursor--alist-get 'cursor result))
               (setq smart-cursor--remote-state
                     (smart-cursor--alist-get 'cursor result))
               (smart-cursor--render-remote-state))))
          ("error"
           (setq smart-cursor--last-error parsed)
           (message "[Smart Cursor] error: %s"
                    (or (smart-cursor--alist-get 'message parsed) parsed)))
          (_ nil))
        (smart-cursor--render-debug-buffer)))))

(defun smart-cursor--on-close (_ws)
  "Handle WebSocket close."
  (setq smart-cursor--connected nil
        smart-cursor--ready nil
        smart-cursor--peripheral-started nil)
  (smart-cursor--clear-overlays)
  (smart-cursor--render-debug-buffer))

(defun smart-cursor--on-error (_ws type err)
  "Handle websocket error callback TYPE ERR."
  (setq smart-cursor--last-error (list :type type :error (format "%s" err)))
  (smart-cursor--render-debug-buffer))

(defun smart-cursor--connection-url ()
  "Build the WS URL for the current smart-cursor session."
  (format "%s?agent-id=%s&session-id=%s"
          smart-cursor-ws-url
          (url-hexify-string smart-cursor-agent-id)
          (url-hexify-string smart-cursor--session-id)))

(defun smart-cursor-connect (&optional agent-id)
  "Connect the smart cursor client to futon3c.
When AGENT-ID is provided, attach to that registry agent."
  (interactive
   (list (read-string "Attach smart cursor to agent: " smart-cursor-agent-id)))
  (smart-cursor--require-websocket)
  (when (and agent-id (not (string-empty-p agent-id)))
    (setq smart-cursor-agent-id agent-id))
  (smart-cursor--assert-safe-agent! smart-cursor-agent-id)
  (setq smart-cursor--session-id (smart-cursor--new-session-id))
  (smart-cursor-disconnect)
  (setq smart-cursor--ws
        (websocket-open
         (smart-cursor--connection-url)
         :on-open (lambda (ws) (smart-cursor--on-open ws))
         :on-message (lambda (ws frame) (smart-cursor--on-message ws frame))
         :on-close (lambda (ws) (smart-cursor--on-close ws))
         :on-error (lambda (ws type err) (smart-cursor--on-error ws type err))))
  (smart-cursor--render-debug-buffer))

(defun smart-cursor-disconnect ()
  "Disconnect the smart cursor client."
  (interactive)
  (when smart-cursor--send-timer
    (cancel-timer smart-cursor--send-timer)
    (setq smart-cursor--send-timer nil))
  (when (and smart-cursor--ws
             (websocket-openp smart-cursor--ws))
    (ignore-errors
      (smart-cursor--send-json '(("type" . "peripheral_stop")
                                  ("reason" . "client-disconnect"))))
    (ignore-errors
      (websocket-close smart-cursor--ws)))
  (setq smart-cursor--ws nil
        smart-cursor--connected nil
        smart-cursor--ready nil
        smart-cursor--peripheral-started nil)
  (smart-cursor--clear-overlays)
  (smart-cursor--render-debug-buffer))

(defun smart-cursor-status ()
  "Show the current smart-cursor status."
  (interactive)
  (message "smart-cursor connected=%s ready=%s peripheral=%s agent=%s session=%s"
           smart-cursor--connected
           smart-cursor--ready
           smart-cursor--peripheral-started
           smart-cursor-agent-id
           smart-cursor--session-id))

(defun smart-cursor-set-mode (mode)
  "Set the remote smart-cursor MODE."
  (interactive
   (list (intern (completing-read "Smart cursor mode: "
                                  '("follow" "observe" "scout")
                                  nil t nil nil "follow"))))
  (smart-cursor--send-tool-action "cursor-mode" (vector (symbol-name mode)))
  (smart-cursor--render-debug-buffer))

(define-minor-mode smart-cursor-mode
  "Global mode for shipping Emacs context to the emacs-cursor peripheral."
  :init-value nil
  :global t
  :lighter " SmartCursor"
  :keymap smart-cursor-mode-map
  (if smart-cursor-mode
      (progn
        (add-hook 'pre-command-hook #'smart-cursor--record-command-start)
        (add-hook 'post-command-hook #'smart-cursor--schedule-context-send)
        (add-hook 'post-command-hook #'smart-cursor--record-transition-if-needed)
        (add-hook 'window-scroll-functions #'smart-cursor--schedule-context-send)
        (setq smart-cursor--beacon-timer
              (run-with-timer smart-cursor-beacon-interval-seconds
                              smart-cursor-beacon-interval-seconds
                              #'smart-cursor--capture-beacon))
        (smart-cursor--render-debug-buffer))
    (when smart-cursor--beacon-timer
      (cancel-timer smart-cursor--beacon-timer)
      (setq smart-cursor--beacon-timer nil))
    (remove-hook 'pre-command-hook #'smart-cursor--record-command-start)
    (remove-hook 'post-command-hook #'smart-cursor--schedule-context-send)
    (remove-hook 'post-command-hook #'smart-cursor--record-transition-if-needed)
    (remove-hook 'window-scroll-functions #'smart-cursor--schedule-context-send)
    (smart-cursor-disconnect)))

(provide 'smart-cursor)
;;; smart-cursor.el ends here
