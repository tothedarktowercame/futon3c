;;; zai-repl.el --- Chat with Z.AI via OpenAI-compatible API -*- lexical-binding: t; -*-

;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/agent-chat.el")
;;   (load "/home/joe/code/futon3c/emacs/zai-repl.el")
;;   M-x zai-repl

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'agent-chat)
(require 'agent-chat-invariants)

;;; Configuration

(defgroup zai-repl nil
  "Chat with Z.AI models via the OpenAI-compatible HTTP API."
  :group 'agent-chat)

(defcustom zai-repl-buffer-name "*zai-repl*"
  "Buffer name used for the Z.AI REPL instance."
  :type 'string
  :group 'zai-repl)

(defcustom zai-repl-agent-id "zai-api"
  "Agency agent id associated with this Z.AI REPL buffer."
  :type 'string
  :group 'zai-repl)

(defcustom zai-repl-agency-url agent-chat-agency-base-url
  "Base URL for the futon3c Agency API."
  :type 'string
  :group 'zai-repl)

(defcustom zai-repl-use-agency nil
  "When non-nil, send turns through Agency /api/alpha/invoke-stream."
  :type 'boolean
  :group 'zai-repl)

(defcustom zai-repl-session-file nil
  "Optional file storing this Z.AI REPL's local session id."
  :type '(choice (const nil) file)
  :group 'zai-repl)

(defcustom zai-repl-api-base-url
  (or (getenv "ZAI_BASE_URL")
      "https://api.z.ai/api/coding/paas/v4")
  "Base URL for the Z.AI OpenAI-compatible API.
For metered non-coding API accounts, set this to:
  https://api.z.ai/api/paas/v4"
  :type 'string
  :group 'zai-repl)

(defcustom zai-repl-model
  (or (getenv "ZAI_MODEL") "glm-5.2")
  "Z.AI model name sent in chat completion requests."
  :type 'string
  :group 'zai-repl)

(defcustom zai-repl-max-tokens 4096
  "Maximum output tokens requested from Z.AI."
  :type 'integer
  :group 'zai-repl)

(defcustom zai-repl-temperature 0.7
  "Sampling temperature requested from Z.AI."
  :type 'number
  :group 'zai-repl)

(defcustom zai-repl-thinking-type "disabled"
  "Z.AI thinking mode for chat requests.
Use \"enabled\" for harder reasoning turns; \"disabled\" is better for a
responsive REPL and avoids returning only reasoning_content on short outputs."
  :type '(choice (const "disabled") (const "enabled"))
  :group 'zai-repl)

(defcustom zai-repl-reasoning-effort "none"
  "Reasoning effort sent when using GLM-5.2.
Z.AI accepts values like \"none\", \"minimal\", \"high\", and \"max\"."
  :type 'string
  :group 'zai-repl)

(defcustom zai-repl-key-file "~/.zaikey"
  "File containing the Z.AI API key, used when ZAI_API_KEY is unset."
  :type 'file
  :group 'zai-repl)

(defcustom zai-repl-key-file-fallbacks '("~/.zai-key")
  "Additional files checked for the Z.AI API key."
  :type '(repeat file)
  :group 'zai-repl)

(defcustom zai-repl-system-prompt
  "You are Z.AI, chatting inside Joe's Emacs zai-repl. Use markdown for structure and fenced code blocks with language labels."
  "System prompt prepended to every local Z.AI conversation."
  :type 'string
  :group 'zai-repl)

;;; Faces and state

(defface zai-repl-zai-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Z.AI responses."
  :group 'zai-repl)

(defface zai-repl-tool-line-face
  '((t :inverse-video t))
  "Face for tool-use lines in zai REPLs.
Inverse video rather than the shared orange, matching Z.AI branding
(operator choice, 2026-07-04)."
  :group 'zai-repl)

(defvar-local zai-repl--messages nil
  "Local conversation history as a list of OpenAI-compatible message plists.")

(defvar-local zai-repl--session-id nil
  "Local display-only session id for this stateless API conversation.")

(defvar-local zai-repl--pending-tool-uses nil
  "Alist of tool-call id to display detail for the active Agency turn.")

;;; Helpers

(defun zai-repl--api-key ()
  "Return the Z.AI API key from env or `zai-repl-key-file'."
  (or (getenv "ZAI_API_KEY")
      (cl-loop for candidate in (cons zai-repl-key-file
                                      zai-repl-key-file-fallbacks)
               for file = (expand-file-name candidate)
               when (file-exists-p file)
               return (string-trim
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))))

(defun zai-repl--endpoint ()
  "Return the chat completions endpoint URL."
  (format "%s/chat/completions"
          (string-remove-suffix "/" zai-repl-api-base-url)))

(defun zai-repl--read-session-file ()
  "Return a nonempty session id from `zai-repl-session-file', or nil.
Any read error also yields nil (callers fall back to a fresh local id);
the id is display-only, so a failed read must never break buffer setup."
  (condition-case nil
      (when (and (stringp zai-repl-session-file)
                 (file-exists-p zai-repl-session-file))
        (let ((sid (string-trim
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents zai-repl-session-file))
                      (buffer-string)))))
          (and (not (string-empty-p sid)) sid)))
    (error nil)))

(defun zai-repl--new-session-id ()
  "Return a fresh local Z.AI session id."
  (format "zai-local-%s-%06d"
          (format-time-string "%Y%m%dT%H%M%S")
          (random 1000000)))

(defun zai-repl--session-id ()
  "Return or allocate a display-only local session id."
  (or zai-repl--session-id
      (setq zai-repl--session-id
            (or (zai-repl--read-session-file)
                (zai-repl--new-session-id)))))

(defun zai-repl--persist-session-id ()
  "Persist the current local Z.AI session id when configured."
  (when (and (stringp zai-repl-session-file)
             (stringp zai-repl--session-id)
             (not (string-empty-p zai-repl-session-file))
             (not (string-empty-p zai-repl--session-id)))
    (when-let ((dir (file-name-directory zai-repl-session-file)))
      (make-directory dir t))
    (write-region zai-repl--session-id nil zai-repl-session-file nil 'silent)))

(defun zai-repl--dispatch-clock-id ()
  "Return the most specific buffer clock id for Agency invoke payloads."
  (or (agent-chat-normalize-excursion-id agent-chat--excursion-id)
      (agent-chat-normalize-mission-id agent-chat--mission-id)
      (agent-chat-normalize-campaign-id agent-chat--campaign-id)))

(defun zai-repl--message-vector ()
  "Return the full outbound message vector for the current request."
  (vconcat
   (list `(:role "system" :content ,zai-repl-system-prompt))
   (nreverse zai-repl--messages)))

(defun zai-repl--payload-json ()
  "Return JSON request body for Z.AI chat completions."
  (json-serialize
   `(:model ,zai-repl-model
     :messages ,(zai-repl--message-vector)
     :max_tokens ,zai-repl-max-tokens
     :temperature ,zai-repl-temperature
     :thinking (:type ,zai-repl-thinking-type)
     :reasoning_effort ,zai-repl-reasoning-effort)))

(defun zai-repl--extract-response (raw)
  "Extract assistant text from RAW Z.AI response JSON."
  (let* ((obj (json-parse-string raw :object-type 'alist
                                 :array-type 'list
                                 :null-object nil
                                 :false-object nil))
         (err (alist-get 'error obj))
         (choices (alist-get 'choices obj)))
    (cond
     (err
      (format "[Z.AI API error: %s]"
              (or (alist-get 'message err)
                  (alist-get 'code err)
                  (format "%S" err))))
     ((and (or (listp choices) (vectorp choices)) (> (length choices) 0))
      (let* ((choice (if (vectorp choices) (aref choices 0) (car choices)))
             (message (alist-get 'message choice))
             (content (alist-get 'content message))
             (reasoning (alist-get 'reasoning_content message)))
        (cond
         ((and (stringp content) (not (string-empty-p content))) content)
         ((and (stringp reasoning) (not (string-empty-p reasoning)))
          (format "[Z.AI returned reasoning_content but no final content]\n\n%s"
                  reasoning))
         (t "[Z.AI returned an empty message]"))))
     (t
      (format "[Unexpected Z.AI response: %s]"
              (truncate-string-to-width (string-trim raw) 500))))))

(defun zai-repl--call (text callback)
  "Send TEXT to Z.AI asynchronously and call CALLBACK with assistant text."
  (if zai-repl-use-agency
      (zai-repl--call-agency-streaming text callback)
    (zai-repl--call-direct text callback)))

(defun zai-repl--call-direct (text callback)
  "Send TEXT directly to Z.AI asynchronously and call CALLBACK with assistant text."
  (let ((key (zai-repl--api-key)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (error "Z.AI API key missing; set ZAI_API_KEY or create %s" zai-repl-key-file))
    (push `(:role "user" :content ,text) zai-repl--messages)
    (let* ((chat-buffer (current-buffer))
           (body-file (make-temp-file "zai-repl-body" nil ".json"
                                      (zai-repl--payload-json)))
           (outbuf (generate-new-buffer " *zai-repl-curl*"))
           (proc
            (make-process
             :name "zai-repl-curl"
             :buffer outbuf
             :command (list "curl" "-sS" "--max-time" "1800"
                            "-H" "Content-Type: application/json"
                            "-H" (concat "Authorization: Bearer " key)
                            "-d" (concat "@" body-file)
                            (zai-repl--endpoint))
             :noquery t
             :connection-type 'pipe
             :sentinel
             (lambda (p _event)
               (when (memq (process-status p) '(exit signal))
                 (unwind-protect
                     (let* ((raw (if (buffer-live-p (process-buffer p))
                                     (with-current-buffer (process-buffer p)
                                       (buffer-string))
                                   ""))
                            (response
                             (if (zerop (process-exit-status p))
                                 (condition-case err
                                     (zai-repl--extract-response raw)
                                   (error
                                    (format "[Could not parse Z.AI response: %s\n%s]"
                                            (error-message-string err)
                                            (truncate-string-to-width
                                             (string-trim raw) 500))))
                               (format "[curl error exit %d: %s]"
                                       (process-exit-status p)
                                       (truncate-string-to-width
                                        (string-trim raw) 500)))))
                       (when (and (zerop (process-exit-status p))
                                  (not (string-prefix-p "[" response)))
                         (with-current-buffer chat-buffer
                           (push `(:role "assistant" :content ,response)
                                 zai-repl--messages)))
                       (funcall callback response))
                   (ignore-errors (delete-file body-file))
                   (when (buffer-live-p (process-buffer p))
                      (kill-buffer (process-buffer p)))))))))
      proc)))

(defun zai-repl--tool-preview (detail)
  "Return a compact display string for a tool-use DETAIL alist."
  (let* ((name (alist-get 'name detail))
         (input (alist-get 'input detail))
         (path (and (listp input)
                    (or (alist-get 'path input)
                        (alist-get 'base_dir input))))
         (cmd (and (listp input) (alist-get 'command input)))
         (pattern (and (listp input) (alist-get 'pattern input)))
         (text (or path cmd pattern "")))
    (if (string-empty-p text)
        (format "[%s]" name)
      (format "[%s] %s" name (truncate-string-to-width text 100)))))

(defun zai-repl--stream-tool-use (json-obj)
  "Render a tool-use event from Agency JSON-OBJ."
  (let* ((tools (alist-get 'tools json-obj))
         (tool-list (cond
                     ((vectorp tools) (append tools nil))
                     ((listp tools) tools)
                     (tools (list (format "%s" tools)))
                     (t nil)))
         (details-raw (alist-get 'tool_details json-obj))
         (details (cond
                   ((vectorp details-raw) (append details-raw nil))
                   ((listp details-raw) details-raw)
                   (t nil)))
         (tool-names (mapconcat (lambda (x) (format "%s" x)) tool-list ", "))
         (tool-text (if details
                        (concat "\n"
                                (mapconcat #'zai-repl--tool-preview details "\n")
                                "\n")
                      (format "\n[%s]\n" tool-names))))
    (dolist (detail details)
      (when-let ((tid (alist-get 'id detail)))
        (push (cons tid detail) zai-repl--pending-tool-uses)))
    (unless agent-chat--streaming-started
      (agent-chat-begin-streaming-message "zai"))
    (agent-chat-stream-text tool-text 'zai-repl-tool-line-face)
    (agent-chat-update-progress
     (format "using %s" (if (string-empty-p tool-names) "tools" tool-names))
     'agent-chat-prompt-face)))

(defun zai-repl--handle-agency-event (json-obj final-text-cell streamed-text-cell)
  "Render one Agency stream JSON-OBJ and update FINAL-TEXT-CELL."
  (let ((type (alist-get 'type json-obj)))
    (cond
     ((equal type "text")
      (let ((txt (or (alist-get 'text json-obj) "")))
        (setcar final-text-cell (concat (car final-text-cell) txt))
        (unless (string-empty-p txt)
          (setcar streamed-text-cell t))
        (unless agent-chat--streaming-started
          (agent-chat-begin-streaming-message "zai"))
        (agent-chat-stream-text txt)))
     ((equal type "tool_use")
      (zai-repl--stream-tool-use json-obj))
     ((equal type "tool_result")
      (when agent-chat--streaming-started
        (agent-chat-stream-text "")))
     ((equal type "done")
      (when-let ((sid (alist-get 'session-id json-obj)))
        (setq zai-repl--session-id sid)
        (zai-repl--persist-session-id))
      (unless (alist-get 'ok json-obj)
        (setcar final-text-cell
                (format "[z.ai invoke failed: %s]"
                        (or (alist-get 'message json-obj)
                            (alist-get 'error json-obj)
                            "unknown error"))))
      (when (and (string-empty-p (car final-text-cell))
                 (alist-get 'result json-obj))
        (setcar final-text-cell (alist-get 'result json-obj)))))))

(defun zai-repl--call-agency-streaming (text callback)
  "Send TEXT through Agency /api/alpha/invoke-stream and stream the response."
  (let* ((chat-buffer (current-buffer))
         (url (concat (string-remove-suffix "/" zai-repl-agency-url)
                      "/api/alpha/invoke-stream"))
         (json-body (json-serialize
                     (append
                      `(:agent-id ,zai-repl-agent-id
                        :prompt ,text
                        :surface "emacs-repl"
                        :caller ,(or (getenv "USER") user-login-name "joe"))
                      (when-let ((clock-id (zai-repl--dispatch-clock-id)))
                        `(:mission-id ,clock-id)))))
         (outbuf (generate-new-buffer " *zai-repl-stream*"))
         (line-buffer "")
         (final-text-cell (list ""))
         (streamed-text-cell (list nil)))
    (setq zai-repl--pending-tool-uses nil)
    (make-process
     :name "zai-repl-stream"
     :buffer outbuf
     :command (list "curl" "-N" "-sS" "--max-time" "1800"
                    "-H" "Content-Type: application/json"
                    "-d" json-body url)
     :noquery t
     :connection-type 'pipe
     :filter
     (lambda (p output)
       (when (buffer-live-p (process-buffer p))
         (with-current-buffer (process-buffer p)
           (goto-char (point-max))
           (insert output)))
       (setq line-buffer (concat line-buffer output))
       (let ((lines (split-string line-buffer "\n")))
         (setq line-buffer (car (last lines)))
         (dolist (line (butlast lines))
           (when (and (not (string-empty-p (string-trim line)))
                      (buffer-live-p chat-buffer))
             (condition-case nil
                 (let ((json-obj (json-parse-string
                                  line
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil)))
                   (with-current-buffer chat-buffer
                     (zai-repl--handle-agency-event
                      json-obj final-text-cell streamed-text-cell)))
               (error nil))))))
     :sentinel
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (when (buffer-live-p chat-buffer)
           (with-current-buffer chat-buffer
             (when agent-chat--streaming-started
               (agent-chat-end-streaming-message))
             (if (car streamed-text-cell)
                 (progn
                   (agent-chat-remove-thinking)
                   (agent-chat-finish-turn!)
                   (agent-chat-scroll-to-bottom))
               (funcall callback (car final-text-cell)))))
         (when (buffer-live-p (process-buffer p))
           (kill-buffer (process-buffer p))))))))

;;; Mode

(defvar zai-repl-mode-map
  (make-sparse-keymap))

(define-key zai-repl-mode-map (kbd "RET") #'zai-repl-send-input)
(define-key zai-repl-mode-map (kbd "C-l") #'recenter-top-bottom)
(define-key zai-repl-mode-map (kbd "C-c C-c") #'agent-chat-interrupt)
(define-key zai-repl-mode-map (kbd "C-c C-k") #'zai-repl-clear)
(define-key zai-repl-mode-map (kbd "C-c C-n") #'zai-repl-new-session)
(define-key zai-repl-mode-map (kbd "C-c C-m") #'agent-chat-clock-in)
(define-key zai-repl-mode-map (kbd "C-c C-e") #'agent-chat-excurse)
(define-key zai-repl-mode-map (kbd "C-c C-o") #'agent-chat-clock-menu)
(define-key zai-repl-mode-map (kbd "C-c .") #'agent-chat-mark-menu)
(define-key zai-repl-mode-map (kbd "C-c ,") #'agent-chat-mark-menu-2)

(defvar zai-repl--font-lock-keywords
  ;; Tool-use lines from both render paths: the direct-stream preview
  ;; "[read_file] /path" and the agent-follow ledger preview
  ;; "[read_file /path]". Highlighting must come FROM font-lock: applying
  ;; face text-properties at insert time loses to refontification, which
  ;; strips properties it thinks it owns (diagnosed live 2026-07-04:
  ;; 52 of 55 tool lines had their faces stripped).
  '(("^\\[[a-z_].*$" 0 'zai-repl-tool-line-face t)
    ;; Tool preview on the same line as a follow-mode name prefix
    ;; ("zai-7⇐claude-16: [run_shell …") — color the bracketed part.
    ("⇐[^:\n]*: \\(\\[[a-z_].*\\)$" 1 'zai-repl-tool-line-face t)))

(define-derived-mode zai-repl-mode nil "ZAI-REPL"
  "Chat with Z.AI through its OpenAI-compatible API."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local line-move-visual nil)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (font-lock-add-keywords nil zai-repl--font-lock-keywords))

(defun zai-repl--build-modeline ()
  "Build Z.AI REPL modeline text."
  (format "%s %s | model: %s | local history: %d turns"
          (agent-chat-mission-segment)
          (if zai-repl-use-agency
              (format "Agency: %s/api/alpha/invoke-stream" (string-remove-suffix "/" zai-repl-agency-url))
            (format "API: %s" (zai-repl--endpoint)))
          zai-repl-model
          (/ (length zai-repl--messages) 2)))

(defun zai-repl--init-display ()
  "Draw the Z.AI REPL header and prompt."
  (agent-chat-init-buffer
   (list :title "z.ai repl"
         :session-id (zai-repl--session-id)
         :modeline-fn #'zai-repl--build-modeline
         :face-alist `(("zai" . zai-repl-zai-face))
         :agent-name "zai"
         :agent-id zai-repl-agent-id
         :campaign-id agent-chat--campaign-id
         :mission-id agent-chat--mission-id
         :excursion-id agent-chat--excursion-id
         :thinking-text "z.ai is thinking..."
         :thinking-prop 'zai-repl-thinking))
  (agent-chat-invariants-setup))

(defun zai-repl-send-input ()
  "Send input to Z.AI and display the response."
  (interactive)
  (agent-chat-send-input #'zai-repl--call "zai"))

(defun zai-repl-clear ()
  "Clear display and redraw the current local Z.AI session."
  (interactive)
  (agent-chat-clear #'zai-repl--init-display))

(defun zai-repl-new-session (&optional target)
  "Start a fresh local Z.AI conversation.
With prefix argument, prompt for a clock TARGET."
  (interactive (list (when current-prefix-arg
                       (agent-chat-read-clock-target))))
  (setq zai-repl--messages nil)
  (setq zai-repl--session-id nil)
  (when (and zai-repl-session-file
             (file-exists-p zai-repl-session-file))
    (delete-file zai-repl-session-file))
  (agent-chat-set-clock! target nil t)
  (agent-chat-clear #'zai-repl--init-display)
  (zai-repl--persist-session-id)
  (agent-chat-insert-message "system" "[new local Z.AI session]")
  (goto-char (point-max)))

;;;###autoload
(defun zai-repl (&optional target)
  "Open a Z.AI REPL buffer.
With prefix argument, prompt for a clock TARGET."
  (interactive (list (when current-prefix-arg
                       (agent-chat-read-clock-target))))
  (let ((buf (get-buffer-create zai-repl-buffer-name)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'zai-repl-mode)
      (zai-repl-mode))
    (when target
      (agent-chat-set-clock! target nil t))
    (unless (and (markerp agent-chat--prompt-marker)
                 (marker-position agent-chat--prompt-marker))
      (zai-repl--init-display)
      (zai-repl--persist-session-id))
    buf))

(defun zai-repl--roster-zai-ids ()
  "Registered zai agent ids from the Agency roster, or nil on any failure."
  (condition-case nil
      (let* ((url (concat (string-remove-suffix "/" zai-repl-agency-url)
                          "/api/alpha/agents"))
             (data (with-temp-buffer
                     (call-process "curl" nil t nil "-sS" "--max-time" "5" url)
                     (goto-char (point-min))
                     (json-parse-buffer :object-type 'alist)))
             ids)
        (dolist (pair (alist-get 'agents data) (nreverse ids))
          (let ((id (symbol-name (car pair))))
            (when (string-prefix-p "zai" id)
              (push id ids)))))
    (error nil)))

;;;###autoload
(defun zai-repl-for-agent (agent-id)
  "Open a Z.AI REPL buffer bound to Agency AGENT-ID (e.g. \"zai-8\").
Completes over the zai agents currently registered with the Agency.
The buffer is named *zai-repl:AGENT-ID* and shares the agent's server-side
session file, so it attaches to the same identity the Agency invokes."
  (interactive
   (list (completing-read "Agency zai agent: "
                          (or (zai-repl--roster-zai-ids) '("zai-8"))
                          nil nil nil nil "zai-8")))
  (let ((agent-id (string-trim agent-id)))
    (zai-repl--open-instance (format "*zai-repl:%s*" agent-id)
                             agent-id
                             (format "/tmp/futon-zai-session-id-%s" agent-id))))

;;;###autoload
(defun zai-repl--open-instance (buffer-name agent-id session-file &optional target session-id)
  "Open a named Z.AI REPL BUFFER-NAME bound to Agency AGENT-ID.
SESSION-FILE stores the local session id.  TARGET optionally clocks the buffer
into a campaign, mission, or excursion."
  (let ((buf (get-buffer-create buffer-name)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'zai-repl-mode)
      (zai-repl-mode))
    (setq-local zai-repl-buffer-name buffer-name)
    (setq-local zai-repl-agent-id agent-id)
    (setq-local zai-repl-session-file session-file)
    (setq-local zai-repl-use-agency t)
    (when (and (stringp session-id)
               (not (string-empty-p session-id)))
      (setq-local zai-repl--session-id session-id))
    (when target
      (agent-chat-set-clock! target nil t))
    (unless (and (markerp agent-chat--prompt-marker)
                 (marker-position agent-chat--prompt-marker))
      (zai-repl--init-display)
      (zai-repl--persist-session-id))
    buf))

(provide 'zai-repl)
;;; zai-repl.el ends here
