;;; agent-chat.el --- Shared UI primitives for agent chat buffers -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Common faces, buffer state, display, and streaming
;;   infrastructure shared by claude-repl.el (Claude) and codex-repl.el (Codex).

(require 'cl-lib)
(require 'browse-url)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-util)

(defconst agent-chat--source-file
  (or load-file-name (buffer-file-name))
  "Path used to recover a stable workspace root for chat rendering.")

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

(defface agent-chat-tool-line-face
  '((t :foreground "#ffb86c"))
  "Face for tool-use lines like [Read], [Edit], etc."
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

(defvar-local agent-chat--agent-id nil
  "Registry agent-id for walkie-talkie calls (e.g. \"claude-1\", \"codex-1\").
Distinct from agent-name which is the display name.")

(defvar-local agent-chat--thinking-text nil
  "The thinking indicator text, e.g. \"claude is thinking...\".")

(defvar-local agent-chat--thinking-property nil
  "Text property symbol used to mark thinking lines.")

(defvar-local agent-chat--turn-start-time nil
  "Epoch time (float) when the current turn started.")

(defvar-local agent-chat--on-turn-end nil
  "Function called with elapsed seconds (float) after each turn completes.
Set by agent-chat-invariants or other modules.")

(defvar-local agent-chat--insert-message-hook nil
  "Hook called with (NAME TEXT) before inserting a message.
Functions on this hook may modify TEXT by returning a replacement string.
If a function returns nil, the original TEXT is used.")

(defvar-local agent-chat--streaming-marker nil
  "Point marker for appending streamed agent output.")

(defvar-local agent-chat--streaming-started nil
  "Non-nil when streaming output has started for the current turn.")

(defvar-local agent-chat--last-evidence-id nil
  "Last evidence entry ID for the current chat buffer.")

(defvar-local agent-chat--evidence-session-id nil
  "Session ID associated with `agent-chat--last-evidence-id`.")

(defvar-local agent-chat--last-emitted-session-id nil
  "Last session ID for which a session-start evidence entry was emitted.")

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
            (overlay-put (make-overlay name-end text-end) 'face 'agent-chat-text-face)
            (agent-chat--decorate-markdown-links name-end text-end)
            ;; Highlight tool-use lines in orange.
            ;; Matches: [Read], [Edit], [Bash], [Glob], [Grep], [Write],
            ;; [Agent], [WebFetch], and also "Using Bash", "Reading Files", etc.
            (save-excursion
              (goto-char name-end)
              (while (re-search-forward
                      "^\\(?:\\[[A-Z][A-Za-z]*\\]\\|\\(?:Using\\|Reading\\|Editing\\|Searching\\|Inspecting\\) [A-Z][A-Za-z]*\\)"
                      text-end t)
                (let* ((bol (line-beginning-position))
                       (eol (line-end-position))
                       (ov (make-overlay bol eol)))
                  (overlay-put ov 'face 'agent-chat-tool-line-face)
                  (overlay-put ov 'priority 10))))))))
    (when at-end
      (goto-char (point-max))
      (agent-chat-scroll-to-bottom))))

(defun agent-chat-scroll-to-bottom ()
  "Scroll window so input prompt is pinned near the bottom.
Only scrolls when the end of the buffer is already visible in the
window, meaning the user is following the output. If the user has
scrolled away, their view and cursor are left undisturbed."
  (when-let ((win (get-buffer-window (current-buffer))))
    (when (pos-visible-in-window-p (point-max) win)
      (set-window-point win (point-max))
      (with-selected-window win
        (recenter -2)))))

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

(defun agent-chat-begin-streaming-message (name &optional face)
  "Insert NAME prefix above prompt and set streaming marker."
  (let ((inhibit-read-only t)
        (name-face (or face
                       (cdr (assoc name agent-chat--face-alist))
                       'agent-chat-joe-face)))
    (agent-chat-remove-thinking)
    (save-excursion
      (goto-char (marker-position agent-chat--prompt-marker))
      (let ((name-start (point)))
        (insert (format "%s: " name))
        (let ((name-end (point)))
          (overlay-put (make-overlay name-start name-end) 'face name-face)
          (setq agent-chat--streaming-marker (copy-marker (point)))
          (setq agent-chat--streaming-started t))))))

(defun agent-chat-stream-text (text &optional face)
  "Insert TEXT at the streaming marker with FACE or text face."
  (when (and (stringp text)
             (not (string-empty-p text))
             agent-chat--streaming-marker
             (marker-position agent-chat--streaming-marker))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (marker-position agent-chat--streaming-marker))
        (let ((start (point)))
          (insert text)
          (overlay-put (make-overlay start (point))
                       'face (or face 'agent-chat-text-face))
          (set-marker agent-chat--streaming-marker (point))))
      (agent-chat-scroll-to-bottom))))

(defun agent-chat-end-streaming-message ()
  "Finalize streamed message and clear streaming state."
  (when (and agent-chat--streaming-marker
             (marker-position agent-chat--streaming-marker))
    (let ((inhibit-read-only t))
      (agent-chat-remove-thinking)
      (save-excursion
        (goto-char (marker-position agent-chat--streaming-marker))
        (insert "\n\n"))))
  (when agent-chat--streaming-marker
    (set-marker agent-chat--streaming-marker nil))
  (setq agent-chat--streaming-marker nil
        agent-chat--streaming-started nil))

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

;;; Walkie-talkie commands (!par, !psr, !pur, etc.)
;;
;; The ! prefix is for walkie-talkie actions — the infrastructure does
;; the tool work (HTTP calls, pattern search, backpack management) and
;; the agent does the judgement work (selecting patterns, writing
;; reflections).  Prompts use plain language to avoid triggering the
;; agent's own slash-command skills.

(defvar-local agent-chat--walkie-talkie-base-url nil
  "Base URL for walkie-talkie HTTP endpoints.
Falls back to `agent-chat-agency-base-url' if nil.")

(defun agent-chat--walkie-talkie-url ()
  "Return the base URL for walkie-talkie endpoints."
  (or agent-chat--walkie-talkie-base-url
      agent-chat-agency-base-url))

(defun agent-chat--walkie-command-p (text)
  "Return (COMMAND . ARGS) if TEXT starts with !, else nil."
  (when (string-match "\\`!\\([a-z]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'" text)
    (cons (match-string 1 text)
          (match-string 2 text))))

(defun agent-chat--extract-conversation ()
  "Extract conversation history from the chat buffer as plain text.
Returns the buffer content up to the separator line."
  (buffer-substring-no-properties
   (point-min)
   (if agent-chat--separator-start
       (marker-position agent-chat--separator-start)
     (point-max))))

;; --- HTTP helpers ---

(defun agent-chat--walkie-post (path payload callback)
  "POST PAYLOAD (alist) to PATH under the walkie-talkie base URL.
CALLBACK is called with (STATUS-SYMBOL BODY-STRING).
STATUS-SYMBOL is `ok' or `error'."
  (let* ((url (concat (agent-chat--walkie-talkie-url) path))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-serialize payload) 'utf-8)))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (let ((msg (format "%s" (plist-get status :error))))
             (kill-buffer)
             (funcall callback 'error msg))
         (goto-char url-http-end-of-headers)
         (let ((body (buffer-substring-no-properties (point) (point-max))))
           (kill-buffer)
           (funcall callback 'ok body))))
     nil t t)))

(defun agent-chat--walkie-get (path callback)
  "GET PATH under the walkie-talkie base URL.
CALLBACK is called with (STATUS-SYMBOL BODY-STRING)."
  (let ((url (concat (agent-chat--walkie-talkie-url) path)))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (progn (kill-buffer)
                  (funcall callback 'error
                           (format "%s" (plist-get status :error))))
         (goto-char url-http-end-of-headers)
         (let ((body (buffer-substring-no-properties (point) (point-max))))
           (kill-buffer)
           (funcall callback 'ok body))))
     nil t t)))

;; --- !par: session reflection ---

(defun agent-chat--par-prompt (&optional hint)
  "Build a reflection prompt from conversation context.
Uses plain language to avoid triggering agent slash-command skills."
  (let ((conversation (agent-chat--extract-conversation)))
    (concat
     "Reflect on this session. Analyze the conversation below and produce "
     "a structured reflection with:\n"
     "- summary: one-line summary of the session\n"
     "- patterns_used: any recurring approaches applied (explicit or inferred)\n"
     "- what_went_well: specific successes (list)\n"
     "- what_could_improve: friction points (list)\n"
     "- suggestions: actionable improvements (list)\n\n"
     "IMPORTANT: Do NOT use any slash commands or tool calls. "
     "Just write the reflection directly as markdown text.\n\n"
     (if hint (format "Summary hint: %s\n\n" hint) "")
     "Conversation:\n"
     "```\n"
     (truncate-string-to-width conversation 8000 nil nil "...")
     "\n```\n\n"
     "Output in readable markdown. Keep it concise — focus on learning, "
     "not narration.")))

(defun agent-chat--handle-par (call-async-fn agent-name hint chat-buffer hooks)
  "Handle !par walkie-talkie command.
Sends a reflection prompt to the agent, displays the result, and
posts it to the evidence landscape as a PAR."
  (let ((prompt (agent-chat--par-prompt hint))
        (on-response (plist-get hooks :on-response))
        (author (or agent-chat--agent-id agent-chat--agent-name agent-name "unknown")))
    (setq agent-chat--pending-process
          (funcall call-async-fn
                   prompt
                   (lambda (response)
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (agent-chat-remove-thinking)
                         (agent-chat-insert-message agent-name response)
                         ;; Post to evidence endpoint
                         (agent-chat--walkie-post
                          "/api/alpha/evidence/par"
                          `(:summary ,response :author ,author)
                          (lambda (status body)
                            (when (buffer-live-p chat-buffer)
                              (with-current-buffer chat-buffer
                                (agent-chat-insert-message
                                 "system"
                                 (if (eq status 'ok)
                                     (format "[Reflection logged: %s]"
                                             (string-trim body))
                                   (format "[Failed to log reflection: %s]"
                                           body)))))))
                         (when (functionp on-response)
                           (funcall on-response response))
                         (goto-char (point-max))
                         (agent-chat-scroll-to-bottom))))))))

;; --- !psr: pattern selection ---

(defun agent-chat--psr-select-prompt (query candidates-text)
  "Build a pattern selection prompt from search results.
QUERY is the user's search term. CANDIDATES-TEXT is the formatted
list of matching patterns from the pattern library."
  (let ((conversation (agent-chat--extract-conversation)))
    (concat
     "The user wants to select a working pattern for their current task.\n\n"
     "Search query: " query "\n\n"
     "Matching patterns from the library:\n"
     candidates-text "\n\n"
     "Recent conversation context:\n"
     "```\n"
     (truncate-string-to-width conversation 4000 nil nil "...")
     "\n```\n\n"
     "Pick the SINGLE most relevant pattern from the candidates above. "
     "Reply with EXACTLY this format (no other text before it):\n\n"
     "SELECTED: <pattern-id>\n"
     "CONFIDENCE: high|medium|low\n"
     "RATIONALE: <one paragraph explaining why this pattern fits>\n\n"
     "IMPORTANT: Do NOT use any slash commands or tool calls. "
     "Just write the selection directly as plain text.")))

(defun agent-chat--parse-psr-response (response)
  "Parse SELECTED/CONFIDENCE/RATIONALE from agent response.
Returns alist with :pattern-id :confidence :rationale, or nil on failure."
  (let ((pattern-id nil)
        (confidence nil)
        (rationale nil))
    (when (string-match "SELECTED:[[:space:]]*\\(.+\\)" response)
      (setq pattern-id (string-trim (match-string 1 response))))
    (when (string-match "CONFIDENCE:[[:space:]]*\\(high\\|medium\\|low\\)" response)
      (setq confidence (string-trim (match-string 1 response))))
    (when (string-match "RATIONALE:[[:space:]]*\\(.+\\)" response)
      (setq rationale (string-trim (match-string 1 response))))
    (when pattern-id
      `((:pattern-id . ,pattern-id)
        (:confidence . ,(or confidence "medium"))
        (:rationale . ,(or rationale ""))))))

(defun agent-chat--handle-psr (call-async-fn agent-name query chat-buffer hooks)
  "Handle !psr walkie-talkie command.
Searches the pattern library via HTTP, presents candidates to the
agent for selection, and posts the result as evidence."
  (if (not query)
      (when (buffer-live-p chat-buffer)
        (with-current-buffer chat-buffer
          (agent-chat-remove-thinking)
          (agent-chat-insert-message "system" "[Usage: !psr <search query>]")))
    (let ((on-response (plist-get hooks :on-response))
          (author (or agent-chat--agent-id agent-chat--agent-name agent-name "unknown")))
      (agent-chat--walkie-get
       (format "/api/alpha/patterns/search?q=%s" (url-hexify-string query))
       (lambda (status body)
         (when (buffer-live-p chat-buffer)
           (with-current-buffer chat-buffer
             (if (eq status 'error)
                 (progn
                   (agent-chat-remove-thinking)
                   (agent-chat-insert-message
                    "system" (format "[Pattern search failed: %s]" body)))
               (let ((prompt (agent-chat--psr-select-prompt query body)))
                 (setq agent-chat--pending-process
                       (funcall
                        call-async-fn prompt
                        (lambda (response)
                          (when (buffer-live-p chat-buffer)
                            (with-current-buffer chat-buffer
                              (agent-chat-remove-thinking)
                              (agent-chat-insert-message agent-name response)
                              (let ((parsed (agent-chat--parse-psr-response response)))
                                (if parsed
                                    (agent-chat--walkie-post
                                     "/api/alpha/evidence/psr"
                                     `(:pattern-id ,(alist-get :pattern-id parsed)
                                       :query ,query
                                       :rationale ,(alist-get :rationale parsed)
                                       :confidence ,(alist-get :confidence parsed)
                                       :author ,author)
                                     (lambda (st bd)
                                       (when (buffer-live-p chat-buffer)
                                         (with-current-buffer chat-buffer
                                           (agent-chat-insert-message
                                            "system"
                                            (if (eq st 'ok)
                                                (format "[Pattern selected: %s — %s]"
                                                        (alist-get :pattern-id parsed)
                                                        (string-trim bd))
                                              (format "[Failed to log: %s]" bd)))))))
                                  (agent-chat-insert-message
                                   "system"
                                   "[Could not parse pattern selection from response]")))
                              (when (functionp on-response)
                                (funcall on-response response))
                              (goto-char (point-max))
                              (agent-chat-scroll-to-bottom)))))))))))))))

;; --- !pur: pattern use record ---

(defun agent-chat--pur-prompt (pattern-id)
  "Build a pattern outcome prompt.
PATTERN-ID is the active pattern from the backpack."
  (let ((conversation (agent-chat--extract-conversation)))
    (concat
     "You have been working with the pattern: " pattern-id "\n\n"
     "Review the conversation below and assess the outcome of "
     "applying this pattern.\n\n"
     "Conversation:\n"
     "```\n"
     (truncate-string-to-width conversation 4000 nil nil "...")
     "\n```\n\n"
     "Reply with EXACTLY this format (no other text before it):\n\n"
     "OUTCOME: success|partial|failure\n"
     "ACTIONS: <what was done while using this pattern>\n"
     "PREDICTION-ERROR: low|medium|high\n"
     "NOTES: <brief reflection on how well the pattern fit>\n\n"
     "IMPORTANT: Do NOT use any slash commands or tool calls. "
     "Just write the assessment directly as plain text.")))

(defun agent-chat--parse-pur-response (response)
  "Parse OUTCOME/ACTIONS/PREDICTION-ERROR/NOTES from agent response."
  (let ((outcome nil) (actions nil) (pred-err nil) (notes nil))
    (when (string-match "OUTCOME:[[:space:]]*\\(success\\|partial\\|failure\\)" response)
      (setq outcome (match-string 1 response)))
    (when (string-match "ACTIONS:[[:space:]]*\\(.+\\)" response)
      (setq actions (string-trim (match-string 1 response))))
    (when (string-match "PREDICTION-ERROR:[[:space:]]*\\(low\\|medium\\|high\\)" response)
      (setq pred-err (match-string 1 response)))
    (when (string-match "NOTES:[[:space:]]*\\(.+\\)" response)
      (setq notes (string-trim (match-string 1 response))))
    (when outcome
      `((:outcome . ,outcome)
        (:actions . ,(or actions ""))
        (:prediction-error . ,(or pred-err "medium"))
        (:notes . ,(or notes ""))))))

(defun agent-chat--handle-pur (call-async-fn agent-name _args chat-buffer hooks)
  "Handle !pur walkie-talkie command.
Checks the agent's backpack for an active pattern, asks the agent
to assess the outcome, and posts the result as evidence."
  (let ((on-response (plist-get hooks :on-response))
        (author (or agent-chat--agent-id agent-chat--agent-name agent-name "unknown")))
    ;; Step 1: check backpack
    (agent-chat--walkie-get
     (format "/api/alpha/backpack/%s" (url-hexify-string author))
     (lambda (status body)
       (when (buffer-live-p chat-buffer)
         (with-current-buffer chat-buffer
           (if (eq status 'error)
               (progn
                 (agent-chat-remove-thinking)
                 (agent-chat-insert-message
                  "system" (format "[Backpack check failed: %s]" body)))
             (let* ((parsed (ignore-errors
                              (json-parse-string body :object-type 'alist)))
                    (backpack (alist-get 'backpack parsed))
                    (pattern-id (or (alist-get 'backpack/active-pattern backpack)
                                    (and (hash-table-p backpack)
                                         (gethash "backpack/active-pattern" backpack)))))
               (if (not pattern-id)
                   (progn
                     (agent-chat-remove-thinking)
                     (agent-chat-insert-message
                      "system" "[No active pattern in backpack. Use !psr first.]"))
                 ;; Step 2: ask agent to assess outcome
                 (let ((prompt (agent-chat--pur-prompt pattern-id)))
                   (setq agent-chat--pending-process
                         (funcall call-async-fn
                                  prompt
                                  (lambda (response)
                                    (when (buffer-live-p chat-buffer)
                                      (with-current-buffer chat-buffer
                                        (agent-chat-remove-thinking)
                                        (agent-chat-insert-message agent-name response)
                                        ;; Step 3: parse and post evidence
                                        (let ((result (agent-chat--parse-pur-response response)))
                                          (if result
                                              (agent-chat--walkie-post
                                               "/api/alpha/evidence/pur"
                                               `(:pattern-id ,pattern-id
                                                 :outcome ,(alist-get :outcome result)
                                                 :actions ,(alist-get :actions result)
                                                 :prediction-error ,(alist-get :prediction-error result)
                                                 :author ,author)
                                               (lambda (status body)
                                                 (when (buffer-live-p chat-buffer)
                                                   (with-current-buffer chat-buffer
                                                     (agent-chat-insert-message
                                                      "system"
                                                      (if (eq status 'ok)
                                                          (format "[Outcome recorded for %s: %s — %s]"
                                                                  pattern-id
                                                                  (alist-get :outcome result)
                                                                  (string-trim body))
                                                        (format "[Failed to log outcome: %s]"
                                                                body)))))))
                                            (agent-chat-insert-message
                                             "system"
                                             "[Could not parse outcome from response]")))
                                        (when (functionp on-response)
                                          (funcall on-response response))
                                        (goto-char (point-max))
                                        (agent-chat-scroll-to-bottom))))))))))))))))

;;; Send

(defun agent-chat-send-input (call-async-fn agent-name &optional hooks)
  "Generic send: extract input, display it, call CALL-ASYNC-FN.
CALL-ASYNC-FN: (text callback) -> process.
AGENT-NAME: \"claude\" or \"codex\", used for display.
Optional HOOKS plist supports:
  :before-send   (fn text) called after user text is inserted.
  :on-response   (fn text) called after agent response is inserted.
  :on-launch-error (fn text) called when process launch fails.

Walkie-talkie commands (e.g. !par) are intercepted: the agent
generates the content (same as /par on CLI), and the result is
additionally posted to the evidence HTTP endpoint."
  (when (process-live-p agent-chat--pending-process)
    (user-error "%s is still responding; wait for current turn to finish"
                (capitalize agent-name)))
  (let ((text (buffer-substring-no-properties
               (marker-position agent-chat--input-start)
               (point-max))))
    (when (not (string-empty-p (string-trim text)))
      (let* ((trimmed (string-trim text))
             (walkie (agent-chat--walkie-command-p trimmed))
             (chat-buffer (current-buffer))
             (before-send (plist-get hooks :before-send))
             (on-response (plist-get hooks :on-response))
             (on-launch-error (plist-get hooks :on-launch-error)))
        (delete-region (marker-position agent-chat--input-start) (point-max))
        (agent-chat-insert-message "joe" trimmed)
        (when (functionp before-send)
          (funcall before-send trimmed))
        (agent-chat-insert-thinking)
        (setq agent-chat--turn-start-time (float-time))
        (redisplay)
        (condition-case err
            (pcase (car walkie)
              ("par"
               (agent-chat--handle-par call-async-fn agent-name
                                       (cdr walkie) chat-buffer hooks))
              ("psr"
               (agent-chat--handle-psr call-async-fn agent-name
                                       (cdr walkie) chat-buffer hooks))
              ("pur"
               (agent-chat--handle-pur call-async-fn agent-name
                                       (cdr walkie) chat-buffer hooks))
              (_
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
                                    ;; Turn timing and invariants
                                    (let ((elapsed (if agent-chat--turn-start-time
                                                       (- (float-time)
                                                          agent-chat--turn-start-time)
                                                     0)))
                                      (setq agent-chat--turn-start-time nil)
                                      (when (functionp agent-chat--on-turn-end)
                                        (condition-case turn-err
                                            (funcall agent-chat--on-turn-end elapsed)
                                          (error
                                           (message "agent-chat turn-end hook error: %s"
                                                    (error-message-string turn-err))))))
                                    (goto-char (point-max))
                                    (agent-chat-scroll-to-bottom))))))))
          (error
           (setq agent-chat--pending-process nil)
           (agent-chat-remove-thinking)
           (let ((msg (format "[Error launching %s process: %s]"
                              agent-name
                              (error-message-string err))))
             (agent-chat-insert-message agent-name msg)
             (when (functionp on-launch-error)
               (funcall on-launch-error msg)))))))))

(defun agent-chat--handle-par (call-async-fn agent-name hint chat-buffer hooks)
  "Handle !par walkie-talkie command.
Builds a PAR prompt from conversation context, sends it to the agent
via CALL-ASYNC-FN (agent generates the PAR, same as /par on CLI),
and additionally posts the result to the evidence HTTP endpoint.
HINT is the optional user-supplied text after !par.
CHAT-BUFFER is the chat buffer. HOOKS is the hooks plist."
  (let ((par-prompt (agent-chat--par-prompt hint))
        (on-response (plist-get hooks :on-response)))
    (setq agent-chat--pending-process
          (funcall call-async-fn
                   par-prompt
                   (lambda (response)
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (agent-chat-remove-thinking)
                         (agent-chat-insert-message agent-name response)
                         ;; Post to evidence endpoint
                         (agent-chat--post-par
                          response
                          (or agent-name "unknown")
                          (lambda (result)
                            (when (buffer-live-p chat-buffer)
                              (with-current-buffer chat-buffer
                                (agent-chat-insert-message
                                 "system"
                                 (format "[PAR logged to evidence landscape: %s]"
                                         (string-trim result)))))))
                         (when (functionp on-response)
                           (funcall on-response response))
                         (goto-char (point-max))
                         (agent-chat-scroll-to-bottom))))))))

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
  :agent-id    - registry agent-id (e.g. \"claude-1\") for walkie-talkie
  :thinking-text   - e.g. \"claude is thinking...\"
  :thinking-prop   - symbol for text property"
  (let ((title (plist-get config :title))
        (session-id (plist-get config :session-id))
        (modeline-fn (plist-get config :modeline-fn))
        (prompt-face (or (plist-get config :prompt-face) 'agent-chat-prompt-face))
        (face-alist (plist-get config :face-alist))
        (agent-name (plist-get config :agent-name))
        (agent-id (plist-get config :agent-id))
        (thinking-text (plist-get config :thinking-text))
        (thinking-prop (plist-get config :thinking-prop)))
    ;; Set buffer-local state
    (setq agent-chat--face-alist
          (append face-alist (list (cons "joe" 'agent-chat-joe-face))))
    (setq agent-chat--agent-name agent-name)
    (setq agent-chat--agent-id agent-id)
    (setq agent-chat--thinking-text thinking-text)
    (setq agent-chat--thinking-property thinking-prop)
    (setq agent-chat--session-id (or session-id "pending"))
    (setq-local agent-chat-hide-markup agent-chat-hide-markup-default)
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

;;; Evidence helpers

(defun agent-chat-evidence-enabled-p (evidence-url)
  "Return non-nil when EVIDENCE-URL is configured."
  (and (stringp evidence-url)
       (not (string-empty-p evidence-url))))

(defun agent-chat-evidence-base-url (evidence-url)
  "Return evidence API base URL without trailing /api/alpha/evidence."
  (let ((value (string-remove-suffix "/" (or evidence-url ""))))
    (replace-regexp-in-string "/api/alpha/evidence\\'" "" value)))

(defun agent-chat--parse-json-string (text)
  "Parse TEXT as JSON plist, or nil on error."
  (when (and (stringp text) (not (string-empty-p text)))
    (condition-case nil
        (json-parse-string text :object-type 'plist)
      (error nil))))

(defun agent-chat-evidence-request-json (method url timeout &optional payload)
  "Send METHOD to URL with JSON PAYLOAD and return plist with :status and :json."
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data (when payload
                             (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t timeout)
                   (error nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status) 0)))
          (re-search-forward "\n\n" nil 'move)
          (let* ((body (buffer-substring-no-properties (point) (point-max)))
                 (parsed (agent-chat--parse-json-string body)))
            (kill-buffer buffer)
            (list :status status :json parsed)))))))

(defun agent-chat-evidence-post-entry-id (evidence-url timeout payload)
  "POST PAYLOAD to evidence API and return created evidence id, or nil."
  (when (agent-chat-evidence-enabled-p evidence-url)
    (let* ((url (format "%s/api/alpha/evidence"
                        (agent-chat-evidence-base-url evidence-url)))
           (response (agent-chat-evidence-request-json "POST" url timeout payload))
           (status (plist-get response :status))
           (parsed (plist-get response :json)))
      (when (and (integerp status) (<= 200 status) (< status 300))
        (or (plist-get parsed :evidence/id)
            (plist-get (plist-get parsed :entry) :evidence/id))))))

(defun agent-chat-evidence-fetch-latest-id (evidence-url timeout sid)
  "Fetch most recent evidence id for session SID."
  (when (and (agent-chat-evidence-enabled-p evidence-url)
             (stringp sid)
             (not (string-empty-p sid)))
    (let* ((query (format "session-id=%s&limit=1"
                          (url-hexify-string sid)))
           (url (format "%s/api/alpha/evidence?%s"
                        (agent-chat-evidence-base-url evidence-url)
                        query))
           (response (agent-chat-evidence-request-json "GET" url timeout nil))
           (status (plist-get response :status))
           (entries (and (integerp status)
                         (<= 200 status)
                         (< status 300)
                         (plist-get (plist-get response :json) :entries)))
           (entry (and (listp entries) (car entries))))
      (and (listp entry)
           (plist-get entry :evidence/id)))))

(defun agent-chat-sync-evidence-anchor! (evidence-url timeout sid session-var last-id-var &optional force)
  "Refresh evidence anchor state for SID into SESSION-VAR and LAST-ID-VAR."
  (when (and (stringp sid)
             (not (string-empty-p sid))
             (or force
                 (not (equal sid (symbol-value session-var)))
                 (null (symbol-value last-id-var))))
    (set session-var sid)
    (set last-id-var (agent-chat-evidence-fetch-latest-id evidence-url timeout sid))))

(defun agent-chat-emit-session-start-evidence!
    (evidence-url timeout sid session-var last-id-var last-emitted-var source tags)
  "Emit a lightweight session-start evidence entry for SID."
  (when (and (stringp sid) (not (string-empty-p sid)))
    (unless (equal sid (symbol-value session-var))
      (set session-var sid)
      (set last-id-var nil))
    (agent-chat-sync-evidence-anchor! evidence-url timeout sid session-var last-id-var t)
    (when (and (not (equal sid (symbol-value last-emitted-var)))
               (not (and (stringp (symbol-value last-id-var))
                         (not (string-empty-p (symbol-value last-id-var)))))
               (agent-chat-evidence-enabled-p evidence-url))
      (let ((payload `((subject . ((ref/type . "session")
                                   (ref/id . ,sid)))
                       (type . "coordination")
                       (claim-type . "goal")
                       (author . ,(or (getenv "USER") user-login-name "joe"))
                       (session-id . ,sid)
                       (body . ((event . "session-start")
                                (source . ,source)
                                (mode . "emacs")))
                       (tags . ,(apply #'vector tags)))))
        (when-let ((new-id (agent-chat-evidence-post-entry-id evidence-url timeout payload)))
          (set last-id-var new-id))
        (set last-emitted-var sid)))
    (unless (and (stringp (symbol-value last-id-var))
                 (not (string-empty-p (symbol-value last-id-var))))
      (agent-chat-sync-evidence-anchor! evidence-url timeout sid session-var last-id-var t))))

(defun agent-chat-emit-turn-evidence!
    (evidence-url timeout log-turns sid role text assistant-author transport tags session-var last-id-var)
  "Emit turn evidence for ROLE and TEXT using the shared evidence helpers."
  (when (and log-turns
             (stringp text)
             (not (string-empty-p (string-trim text)))
             (stringp sid)
             (not (string-empty-p sid))
             (agent-chat-evidence-enabled-p evidence-url))
    (agent-chat-sync-evidence-anchor! evidence-url timeout sid session-var last-id-var)
    (let* ((trimmed (string-trim text))
           (is-user (string= role "user"))
           (is-error (string-prefix-p "[Error" trimmed))
           (claim-type (cond
                        (is-user "question")
                        (is-error "correction")
                        (t "observation")))
           (author (if is-user
                       (or (getenv "USER") user-login-name "joe")
                     assistant-author))
           (role-tag (if is-user "user" "assistant"))
           (payload `((subject . ((ref/type . "session")
                                  (ref/id . ,sid)))
                      (type . "coordination")
                      (claim-type . ,claim-type)
                      (author . ,author)
                      (session-id . ,sid)
                      (body . ((event . "chat-turn")
                               (transport . ,transport)
                               (role . ,role)
                               (text . ,trimmed)))
                      (tags . ,(apply #'vector (append tags (list role-tag)))))))
      (when (and (stringp (symbol-value last-id-var))
                 (not (string-empty-p (symbol-value last-id-var))))
        (setq payload (append payload
                              `((in-reply-to . ,(symbol-value last-id-var))))))
      (when-let ((new-id (agent-chat-evidence-post-entry-id evidence-url timeout payload)))
        (set session-var sid)
        (set last-id-var new-id)))))

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

;;; Popup display (posframe on GUI, transient window on terminal)

(defcustom agent-chat-popup-timeout 0
  "Seconds before popup auto-hides. 0 means manual dismiss only."
  :type 'number
  :group 'agent-chat)

(defcustom agent-chat-popup-max-width 80
  "Maximum character width for popup frames."
  :type 'integer
  :group 'agent-chat)

(defcustom agent-chat-popup-max-height 20
  "Maximum line height for popup frames."
  :type 'integer
  :group 'agent-chat)

(defvar agent-chat--popup-buffer " *agent-chat-popup*"
  "Buffer name used for transient popup display.")

(defvar agent-chat--popup-window nil
  "Window used for terminal-mode popup fallback.")

(defun agent-chat-popup-show (text &optional title)
  "Show TEXT in a floating popup near point.
Uses posframe in GUI Emacs, a transient bottom window in terminal.
Optional TITLE appears as a header line."
  (let ((buf (get-buffer-create agent-chat--popup-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (setq-local mode-line-format nil)
        (when title
          (setq-local header-line-format
                      (propertize (format " %s" title) 'face 'bold)))
        (special-mode)))
    (if (display-graphic-p)
        ;; GUI: posframe child frame
        (when (require 'posframe nil t)
          (posframe-show buf
                         :position (point)
                         :max-width agent-chat-popup-max-width
                         :max-height agent-chat-popup-max-height
                         :border-width 1
                         :border-color "#555555"
                         :internal-border-width 8
                         :timeout (if (zerop agent-chat-popup-timeout)
                                      0
                                    agent-chat-popup-timeout)))
      ;; Terminal: transient bottom window
      (let ((win (or (and (window-live-p agent-chat--popup-window)
                          agent-chat--popup-window)
                     (display-buffer-in-side-window
                      buf
                      '((side . bottom)
                        (window-height . fit-window-to-buffer)
                        (window-parameters . ((no-other-window . t)
                                              (no-delete-other-windows . t))))))))
        (when (window-live-p win)
          (setq agent-chat--popup-window win)
          (fit-window-to-buffer win agent-chat-popup-max-height 3))))))

(defun agent-chat-popup-hide ()
  "Dismiss the popup."
  (interactive)
  (if (display-graphic-p)
      (when (require 'posframe nil t)
        (posframe-hide agent-chat--popup-buffer))
    (when (window-live-p agent-chat--popup-window)
      (quit-window nil agent-chat--popup-window)
      (setq agent-chat--popup-window nil))))

(defun agent-chat-popup-toggle (text &optional title)
  "Toggle popup display. Show TEXT if hidden, hide if visible."
  (if (if (display-graphic-p)
          (and (require 'posframe nil t)
               (get-buffer agent-chat--popup-buffer)
               (posframe--find-visible-frame
                (get-buffer agent-chat--popup-buffer)))
        (window-live-p agent-chat--popup-window))
      (agent-chat-popup-hide)
    (agent-chat-popup-show text title)))

;;; Interrupt

(defun agent-chat--server-interrupt (agent-id &optional base-url)
  "POST interrupt-invoke to the server for AGENT-ID.
Uses BASE-URL if provided, otherwise `agent-chat-agency-base-url'.
Best-effort: failures are logged but do not block the local interrupt."
  (when (and (stringp agent-id) (not (string-empty-p agent-id)))
    (let ((url (format "%s/api/alpha/agents/%s/interrupt-invoke"
                       (string-remove-suffix "/"
                         (or base-url agent-chat-agency-base-url))
                       agent-id)))
      (make-process
       :name "agent-chat-interrupt"
       :buffer nil
       :command (list "curl" "-sS" "--max-time" "5"
                      "-X" "POST"
                      "-H" "Content-Type: application/json"
                      "-d" "{}" url)
       :noquery t
       :sentinel (lambda (_p event)
                   (when (string-match-p "finished" event)
                     (message "server-side interrupt sent for %s" agent-id)))))))

(defun agent-chat-interrupt ()
  "Interrupt the current in-flight request.
Kills the pending process, sends server-side interrupt to stop the
agent's backing subprocess, removes thinking indicator, and inserts
an [interrupted] message."
  (interactive)
  (if (process-live-p agent-chat--pending-process)
      (let ((proc agent-chat--pending-process))
        (setq agent-chat--pending-process nil)
        (kill-process proc)
        ;; Tell the server to kill the agent's subprocess tree
        (agent-chat--server-interrupt
         agent-chat--agent-id
         (cond ((local-variable-p 'claude-repl-api-url) claude-repl-api-url)
               ((local-variable-p 'codex-repl-api-url) codex-repl-api-url)))
        (when agent-chat--streaming-started
          (agent-chat-end-streaming-message))
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

(defcustom agent-chat-hide-markup-default t
  "Default local value for `agent-chat-hide-markup` in chat buffers."
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

(defface agent-chat-markdown-link
  '((t :inherit link))
  "Face for markdown links in chat buffers."
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

(defun agent-chat--match-link (limit)
  "Font-lock matcher for markdown links.
Groups: 1=label, 2=target. Skips fenced regions."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward
                 (rx "[" (group (+? (not (any "]" "\n"))))
                     "]("
                     (group (+? (not (any ")" "\n"))))
                     ")")
                 limit t))
      (unless (get-text-property (match-beginning 0) 'agent-chat-in-fence)
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

(defun agent-chat-open-link-at-point ()
  "Open the markdown link target at point, if any."
  (interactive)
  (let ((target (get-text-property (point) 'agent-chat-link-target)))
    (unless (stringp target)
      (user-error "No link at point"))
    (pcase-let ((`(,kind ,path ,line)
                 (agent-chat--parse-link-target target)))
      (cond
       ((eq kind 'file)
        (find-file path)
        (when line
          (goto-char (point-min))
          (forward-line (1- line))))
       ((eq kind 'url)
        (browse-url path))
       (t
        (user-error "Unsupported link target: %s" target))))))

(defun agent-chat--parse-link-target (target)
  "Parse markdown link TARGET.
Return (KIND PATH LINE), where KIND is `file', `url', or nil."
  (cond
   ((and (stringp target)
         (string-match "\\`\\(/[^#]+\\)\\(?:#L\\([0-9]+\\)\\)?\\'" target))
    (list 'file
          (match-string 1 target)
          (when-let ((line (match-string 2 target)))
            (string-to-number line))))
   ((and (stringp target)
         (string-match-p "\\`https?://" target))
    (list 'url target nil))
   (t
    (list nil target nil))))

(defun agent-chat--workspace-root ()
  "Return the nearest workspace root for the current buffer, or nil."
  (when-let ((root (or (locate-dominating-file default-directory "AGENTS.md")
                       (when buffer-file-name
                         (locate-dominating-file buffer-file-name "AGENTS.md"))
                       (and agent-chat--source-file
                            (locate-dominating-file agent-chat--source-file
                                                    "AGENTS.md")))))
    (file-name-as-directory (expand-file-name root))))

(defun agent-chat--link-display-text (label target)
  "Return clean display text for markdown link LABEL and TARGET."
  (pcase-let ((`(,kind ,path ,line)
               (agent-chat--parse-link-target target)))
    (cond
     ((eq kind 'file)
      (let* ((root (agent-chat--workspace-root))
             (display-path
              (if (and root
                       (string-prefix-p (file-name-as-directory root) path))
                  (file-relative-name path root)
                (abbreviate-file-name path))))
        (if line
            (format "%s:%d" display-path line)
          display-path)))
     ((eq kind 'url)
      (or label target))
     (t
      (or label target)))))

(defvar agent-chat-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-chat-open-link-at-point)
    (define-key map (kbd "RET") #'agent-chat-open-link-at-point)
    map)
  "Keymap used for markdown links in chat buffers.")

(defun agent-chat--apply-link-properties (start end target &optional display-text)
  "Apply interactive link properties to START..END for TARGET.
When DISPLAY-TEXT is non-nil, render that instead of the raw markdown."
  (dolist (ov (overlays-in start end))
    (when (overlay-get ov 'agent-chat-link)
      (delete-overlay ov)))
  (add-text-properties
   start end
   `(mouse-face highlight
                help-echo ,target
                keymap ,agent-chat-link-map
                follow-link t
                agent-chat-link-target ,target))
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'agent-chat-link t)
    (overlay-put ov 'priority 20)
    (overlay-put ov 'face 'agent-chat-markdown-link)
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo target)
    (overlay-put ov 'keymap agent-chat-link-map)
    (overlay-put ov 'follow-link t)
    (when display-text
      (overlay-put ov 'display display-text))))

(defun agent-chat--decorate-markdown-links (start end)
  "Apply markdown link styling in region START..END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward
            (rx "[" (group (+? (not (any "]" "\n"))))
                "]("
                (group (+? (not (any ")" "\n"))))
                ")")
            end t)
      (let* ((beg (match-beginning 0))
             (fin (match-end 0))
             (label (match-string-no-properties 1))
             (target (match-string-no-properties 2))
             (display-text (agent-chat--link-display-text label target)))
        (agent-chat--apply-link-properties
         beg fin target display-text)))))

(defun agent-chat--fontify-link ()
  "Apply link styling and open behavior to markdown links.  Return nil."
  (let* ((beg (match-beginning 0))
         (fin (match-end 0))
         (label (match-string-no-properties 1))
         (target (match-string-no-properties 2))
         (display-text (agent-chat--link-display-text label target)))
    (agent-chat--apply-link-properties
     beg fin target display-text))
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
    (agent-chat--match-link (0 (agent-chat--fontify-link) nil t))
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
    ;; Preserve normal Emacs screen recentering across chat surfaces.
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c C-k") #'ignore) ;; placeholder, overridden per-mode
    map)
  "Base keymap inherited by chat modes.")

(provide 'agent-chat)
;;; agent-chat.el ends here
