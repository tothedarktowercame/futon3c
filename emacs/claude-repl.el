;;; claude-repl.el --- Chat with Claude via futon3c API -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs chat buffer backed by futon3c's /api/alpha/invoke endpoint.
;; Asynchronous: type, RET, Claude responds without blocking Emacs UI.
;; Routes through the agent registry — same invoke-fn as IRC.
;; Logs every turn to the evidence landscape (same pattern as codex-repl.el).
;;
;; Usage:
;;   (load "/home/joe/code/futon3c/emacs/agent-chat.el")
;;   (load "/home/joe/code/futon3c/emacs/claude-repl.el")
;;   M-x claude-repl

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'agent-chat)
(require 'agent-chat-invariants)

;;; Configuration

(defgroup claude-repl nil
  "Chat with Claude via futon3c API."
  :group 'agent-chat)

(defcustom claude-repl-api-url agent-chat-agency-base-url
  "Base URL for the futon3c API server."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-agent-id "claude-1"
  "Agent ID to invoke via the API."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-session-file "/tmp/futon-session-id"
  "File storing Claude session ID (shared with IRC relay)."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-evidence-url
  (or (getenv "FUTON3C_EVIDENCE_URL")
      (format "%s/api/alpha/evidence"
              (string-remove-suffix "/" agent-chat-agency-base-url)))
  "Evidence API endpoint for logging chat turns."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-evidence-log-turns t
  "When non-nil, log user/assistant turns into the evidence API."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-evidence-timeout 1
  "Timeout in seconds for evidence API requests."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-drawbridge-url "http://localhost:6768"
  "Drawbridge REPL URL used as a fallback for session reset."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-drawbridge-token nil
  "Admin token for Drawbridge REPL.
If nil, reads from .admintoken in the project root at first use."
  :type '(choice (const nil) string)
  :group 'claude-repl)

;;; Face (Claude-specific; shared faces are in agent-chat)

(defface claude-repl-claude-face
  '((t :foreground "#8be9fd" :weight bold))
  "Face for Claude."
  :group 'claude-repl)

;;; Workspace detection

(defun claude-repl--workspace ()
  "Return the daemon name if running inside a named daemon, else nil."
  (let ((d (daemonp)))
    (when (stringp d) d)))

;;; Internal state

(defvar claude-repl--buffer-name "*claude-repl*")
(defvar claude-repl--last-evidence-id nil
  "Last evidence entry ID, used for in-reply-to chaining.")
(defvar claude-repl--evidence-session-id nil
  "Session ID associated with `claude-repl--last-evidence-id'.")
(defvar claude-repl--last-emitted-session-id nil
  "Last session ID for which a session-start evidence entry was emitted.")

;;; Auto-registration

(defun claude-repl--find-idle-agent ()
  "Find an existing idle claude agent from the registry.
Workspace-aware: prefers agents whose emacs-socket matches this
workspace. Never steals an agent bound to a different workspace.
Falls back to unbound idle agents, then returns nil."
  (let* ((my-socket (or (claude-repl--workspace)
                        (and (boundp 'server-name) server-name)))
         (url (concat claude-repl-api-url "/api/alpha/agents"))
         (result (with-temp-buffer
                   (let ((exit (call-process "curl" nil t nil
                                             "-sS" "--max-time" "5" url)))
                     (when (= exit 0)
                       (goto-char (point-min))
                       (condition-case nil
                           (json-parse-buffer :object-type 'alist)
                         (error nil)))))))
    (when-let ((agents-val (alist-get 'agents result)))
      (let* ((entries (cond
                       ((hash-table-p agents-val)
                        (let (pairs)
                          (maphash (lambda (k v) (push (cons (symbol-name k) v) pairs))
                                   agents-val)
                          pairs))
                       ((listp agents-val)
                        (mapcar (lambda (pair)
                                  (cons (symbol-name (car pair)) (cdr pair)))
                                agents-val))))
             ;; Filter to idle claude agents
             (claude-entries
              (seq-filter
               (lambda (pair)
                 (and (string-prefix-p "claude-" (car pair))
                      (let ((status (alist-get 'status (cdr pair))))
                        (or (null status)
                            (equal status "idle")))))
               entries))
             ;; Partition by socket affinity
             (agent-socket (lambda (pair)
                             (let ((meta (alist-get 'metadata (cdr pair))))
                               (cond
                                ((hash-table-p meta) (gethash "emacs-socket" meta))
                                ((listp meta) (or (alist-get 'emacs-socket meta)
                                                  (alist-get 'emacs_socket meta)))))))
             ;; 1. Agents bound to MY workspace (best match)
             (mine (seq-filter
                    (lambda (pair)
                      (equal (funcall agent-socket pair) my-socket))
                    claude-entries))
             ;; 2. Unbound agents (no emacs-socket set)
             (unbound (seq-filter
                       (lambda (pair)
                         (null (funcall agent-socket pair)))
                       claude-entries))
             ;; Never consider agents bound to OTHER workspaces
             (candidates (or mine unbound))
             (sorted (sort candidates
                           (lambda (a b)
                             (string< (car a) (car b))))))
        (when sorted (caar sorted))))))

(defun claude-repl--rebind-socket (agent-id socket-name)
  "Rebind AGENT-ID's invoke-fn to use SOCKET-NAME for blackboard calls."
  (when socket-name
    (let* ((url (format "%s/api/alpha/agents/%s/rebind"
                        claude-repl-api-url agent-id))
           (json-body (json-serialize `(:emacs-socket ,socket-name))))
      (with-temp-buffer
        (call-process "curl" nil t nil
                      "-sS" "--max-time" "5"
                      "-X" "POST"
                      "-H" "Content-Type: application/json"
                      "-d" json-body url)))))

(defun claude-repl--auto-register ()
  "Find or register a Claude agent on the futon3c server.
First tries to reuse an existing idle claude agent (preserving identity).
Only creates a new one via POST /agents/auto if none are idle.
In both cases, rebinds the agent's socket to this Emacs daemon."
  (let* ((socket-name (or (claude-repl--workspace)
                          (and (boundp 'server-name) server-name)))
         (agent-id
          (or
           ;; First: reuse an existing idle agent
           (claude-repl--find-idle-agent)
           ;; Second: register a new one
           (let* ((url (concat claude-repl-api-url "/api/alpha/agents/auto"))
                  (json-body (json-serialize
                              (if socket-name
                                  `(:type "claude" :emacs-socket ,socket-name)
                                '(:type "claude"))))
                  (result (with-temp-buffer
                            (let ((exit (call-process "curl" nil t nil
                                                      "-sS" "--max-time" "5"
                                                      "-H" "Content-Type: application/json"
                                                      "-d" json-body url)))
                              (when (= exit 0)
                                (goto-char (point-min))
                                (condition-case nil
                                    (json-parse-buffer :object-type 'alist)
                                  (error nil)))))))
             (when (and result (alist-get 'ok result))
               (alist-get 'agent-id result))))))
    (when (and (stringp agent-id) (not (string-empty-p agent-id)))
      ;; Rebind socket so blackboard targets this Emacs daemon
      (claude-repl--rebind-socket agent-id socket-name)
      (setq-local claude-repl-agent-id agent-id)
      (setq-local claude-repl-session-file
                  (format "/tmp/futon-session-id-%s" agent-id))
      ;; Update displayed session from the agent's session file.
      ;; Capture path before with-temp-buffer (which loses buffer-local binding).
      (let ((sf claude-repl-session-file))
        (when (and (file-exists-p sf)
                   (fboundp 'agent-chat-update-session-id))
          (let ((sid (string-trim
                      (with-temp-buffer
                        (insert-file-contents-literally sf)
                        (buffer-string)))))
            (unless (string-empty-p sid)
              (agent-chat-update-session-id sid)))))
      (message "claude-repl: registered as %s (socket: %s)" agent-id
               (or socket-name "default"))
      agent-id)))

;;; Evidence logging

(defun claude-repl--emit-session-start-evidence! (sid)
  "Emit a lightweight session-start evidence entry for SID."
  (agent-chat-emit-session-start-evidence!
   claude-repl-evidence-url
   claude-repl-evidence-timeout
   sid
   'claude-repl--evidence-session-id
   'claude-repl--last-evidence-id
   'claude-repl--last-emitted-session-id
   "claude-repl"
   '("claude" "session-start" "chat")))

(defun claude-repl--emit-turn-evidence! (role text)
  "Emit a turn evidence event for ROLE (\"user\" or \"assistant\") and TEXT."
  (agent-chat-emit-turn-evidence!
   claude-repl-evidence-url
   claude-repl-evidence-timeout
   claude-repl-evidence-log-turns
   agent-chat--session-id
   role
   text
   claude-repl-agent-id
   "emacs-claude-repl"
   '("claude" "chat" "turn")
   'claude-repl--evidence-session-id
   'claude-repl--last-evidence-id))

(defun claude-repl--emit-user-turn-evidence! (text)
  "Emit evidence for user TEXT."
  (claude-repl--emit-turn-evidence! "user" text))

(defun claude-repl--emit-assistant-turn-evidence! (text)
  "Emit evidence for assistant TEXT."
  (claude-repl--emit-turn-evidence! "assistant" text))

;;; Streaming invoke

(defun claude-repl--call-claude-streaming (text callback)
  "Send TEXT to Claude via POST /api/alpha/invoke-stream.
Streams NDJSON events incrementally. Text events are displayed as they
arrive. Tool-use events update the progress line. The done event
triggers evidence emission and session-id update.
CALLBACK is called with the final response text on completion."
  (claude-repl--call-claude-streaming-with-retry text callback 0))

(defun claude-repl--call-claude-streaming-with-retry (text callback retry-attempt)
  "Send TEXT to Claude with RETRY-ATTEMPT tracking for agent re-registration."
  (let* ((chat-buffer (current-buffer))
         (url (concat claude-repl-api-url "/api/alpha/invoke-stream"))
         (full-prompt (format "Agent: %s\n\nUser message:\n%s"
                              claude-repl-agent-id text))
         (json-body (json-serialize
                     `(:agent-id ,claude-repl-agent-id
                       :prompt ,full-prompt
                       :surface "emacs-repl"
                       :caller ,(or (getenv "USER") user-login-name "joe"))))
         (outbuf (generate-new-buffer " *futon3c-invoke-stream*"))
         (line-buffer ""))
    (let ((proc
           (make-process
            :name "futon3c-invoke-stream"
            :buffer outbuf
            :command (list "curl" "-N" "-sS" "--max-time" "1800"
                           "-H" "Content-Type: application/json"
                           "-d" json-body url)
            :noquery t
            :connection-type 'pipe
            :filter
            (lambda (p output)
              ;; Append to process buffer for sentinel
              (when (buffer-live-p (process-buffer p))
                (with-current-buffer (process-buffer p)
                  (goto-char (point-max))
                  (insert output)))
              ;; Split into NDJSON lines
              (setq line-buffer (concat line-buffer output))
             (let ((lines (split-string line-buffer "\n")))
               (setq line-buffer (car (last lines)))
               (dolist (line (butlast lines))
                 (when (not (string-empty-p (string-trim line)))
                   (condition-case nil
                       (let* ((json-obj (json-parse-string
                                         line
                                         :object-type 'alist
                                         :null-object nil
                                         :false-object nil))
                              (type (alist-get 'type json-obj)))
                         (when (buffer-live-p chat-buffer)
                           (with-current-buffer chat-buffer
                             (cond
                              ((equal type "text")
                               (unless agent-chat--streaming-started
                                 (agent-chat-begin-streaming-message "claude"))
                               (agent-chat-stream-text
                                (alist-get 'text json-obj)))
                              ((equal type "tool_use")
                               (let* ((tools (alist-get 'tools json-obj))
                                      (tool-names
                                       (if (vectorp tools)
                                           (mapconcat #'identity
                                                      (append tools nil) ", ")
                                         (format "%s" tools))))
                                 (if agent-chat--streaming-started
                                     ;; Mid-stream tool use: append inline rather
                                     ;; than replacing progress (avoids remove-thinking
                                     ;; interfering with streamed text)
                                     (let* ((tool-text (format "\n[%s]\n" tool-names))
                                            (start-pos (and agent-chat--streaming-marker
                                                            (marker-position agent-chat--streaming-marker))))
                                       (agent-chat-stream-text tool-text)
                                       ;; Re-overlay the tool line in orange with high priority
                                       ;; so it wins over the text-face overlay.
                                       (when (and start-pos agent-chat--streaming-marker)
                                         (let ((ov (make-overlay start-pos
                                                                 (marker-position agent-chat--streaming-marker))))
                                           (overlay-put ov 'face 'agent-chat-tool-line-face)
                                           (overlay-put ov 'priority 10))))
                                   (agent-chat-update-progress
                                    (format "using %s" tool-names)
                                    'agent-chat-prompt-face))))))))
                     (error nil))))))
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               ;; If interrupted, just clean up
               (if (and (eq (process-status p) 'signal)
                        (not (eq agent-chat--pending-process p)))
                   (progn
                     (when (buffer-live-p chat-buffer)
                       (with-current-buffer chat-buffer
                         (when agent-chat--streaming-started
                           (agent-chat-end-streaming-message))))
                     (when (buffer-live-p (process-buffer p))
                       (kill-buffer (process-buffer p))))
                 (condition-case err
                     (let* ((raw (if (buffer-live-p (process-buffer p))
                                     (with-current-buffer (process-buffer p)
                                       (buffer-string))
                                   ""))
                            ;; Find the done event in the raw output
                            (done-event nil)
                            (retried nil))
                       ;; Parse done event from raw NDJSON
                       (dolist (line (split-string raw "\n"))
                         (when (and (not (string-empty-p (string-trim line)))
                                    (string-match-p "\"type\"[[:space:]]*:[[:space:]]*\"done\"" line))
                           (condition-case nil
                               (setq done-event
                                     (json-parse-string line
                                                        :object-type 'alist
                                                        :null-object nil
                                                        :false-object nil))
                             (error nil))))
                       (when (buffer-live-p (process-buffer p))
                         (kill-buffer (process-buffer p)))
                       (when (buffer-live-p chat-buffer)
                         (with-current-buffer chat-buffer
                           (when (eq agent-chat--pending-process p)
                             (setq agent-chat--pending-process nil))
                           (cond
                            ;; Successful done event
                            ((and done-event (alist-get 'ok done-event))
                             (let ((sid (alist-get 'session-id done-event))
                                   (result (or (alist-get 'result done-event)
                                               "[empty response]")))
                               (when sid
                                 (agent-chat-update-session-id sid)
                                 (when claude-repl-session-file
                                   (write-region sid nil claude-repl-session-file nil 'silent))
                                 (claude-repl--emit-session-start-evidence! sid))
                               (if agent-chat--streaming-started
                                   (progn
                                     ;; Text already displayed — just finalize
                                     (agent-chat-end-streaming-message)
                                     ;; Emit evidence directly (skip callback to avoid re-insert)
                                     (claude-repl--emit-assistant-turn-evidence! result)
                                     (agent-chat-invariants-turn-ended)
                                     (goto-char (point-max))
                                     (agent-chat-scroll-to-bottom))
                                 ;; No streaming happened — use callback for full insert
                                 (funcall callback result))))
                            ;; Error done event — check for agent-not-found
                            (done-event
                             (let ((err-msg (or (alist-get 'message done-event)
                                                (alist-get 'error done-event))))
                               (when agent-chat--streaming-started
                                 (agent-chat-end-streaming-message))
                               (if (and (stringp err-msg)
                                        (string-match-p "not registered\\|agent-not-found" err-msg))
                                   (if (and (= retry-attempt 0)
                                            (claude-repl--auto-register))
                                       (progn
                                         (message "claude-repl: re-registered as %s — retrying..."
                                                  claude-repl-agent-id)
                                         (setq retried t))
                                     (funcall callback (format "[Error: %s]" err-msg)))
                                 (funcall callback (format "[Error: %s]" err-msg)))))
                            ;; No done event found (curl error, etc.)
                            (t
                             (when agent-chat--streaming-started
                               (agent-chat-end-streaming-message))
                             (let ((exit-code (process-exit-status p)))
                               (funcall callback
                                        (format "[curl error (exit %d): %s]"
                                                exit-code
                                                (string-trim (truncate-string-to-width raw 200)))))))
                           ;; Handle retry
                           (when retried
                             (claude-repl--call-claude-streaming-with-retry
                              text callback (1+ retry-attempt))))))
                   (error
                    (message "claude-repl streaming sentinel error: %s" (error-message-string err))
                    (when (buffer-live-p chat-buffer)
                      (with-current-buffer chat-buffer
                        (setq agent-chat--pending-process nil)
                        (when agent-chat--streaming-started
                          (agent-chat-end-streaming-message))
                        (agent-chat-remove-thinking)
                        (agent-chat-insert-message
                         "claude"
                         (format "[Sentinel error: %s]" (error-message-string err)))))))))))))
      proc)))

;;; Modeline

(defun claude-repl--build-modeline ()
  "Build dynamic transport modeline for system prompt."
  (let ((transports (list "emacs-chat (active, via /api/alpha/invoke)"))
        (irc-up (agent-chat-irc-available-p)))
    (when irc-up
      (push "irc (#futon :6667, available)" transports))
    (push "cli (claude code)" transports)
    (format "Available transports: [%s]. Current: emacs-chat."
            (string-join (reverse transports) ", "))))

;;; Mode

(defvar claude-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-repl-send-input)
    (define-key map (kbd "C-c C-c") #'agent-chat-interrupt)
    (define-key map (kbd "C-c C-k") #'claude-repl-clear)
    (define-key map (kbd "C-c C-n") #'claude-repl-new-session)
    map))

(define-derived-mode claude-repl-mode nil "Claude-REPL"
  "Chat with Claude via futon3c API.
Type after the prompt, RET to send, C-c C-n for fresh session.
\\{claude-repl-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0))

(defun claude-repl-send-input ()
  "Send input to Claude and display response."
  (interactive)
  (agent-chat-send-input
   #'claude-repl--call-claude-streaming
   "claude"
   (list :before-send #'claude-repl--emit-user-turn-evidence!
         :on-response #'claude-repl--emit-assistant-turn-evidence!)))

(defun claude-repl-clear ()
  "Clear display and re-draw header. Session and agent identity continue."
  (interactive)
  (let ((agent-id claude-repl-agent-id)
        (session-file claude-repl-session-file)
        (ws-applied (and (local-variable-p 'claude-repl--workspace-applied)
                         claude-repl--workspace-applied)))
    (agent-chat-clear
     (lambda ()
       ;; Restore identity before init redraws the buffer
       (setq-local claude-repl-agent-id agent-id)
       (setq-local claude-repl-session-file session-file)
       (when ws-applied
         (setq-local claude-repl--workspace-applied t))
       (claude-repl--init-display)))))

(defun claude-repl--drawbridge-token ()
  "Return the Drawbridge admin token, reading .admintoken if needed."
  (or claude-repl-drawbridge-token
      (let ((f (expand-file-name ".admintoken"
                                 (locate-dominating-file default-directory ".admintoken"))))
        (when (file-exists-p f)
          (setq claude-repl-drawbridge-token
                (string-trim (with-temp-buffer
                               (insert-file-contents f)
                               (buffer-string))))))))

(defun claude-repl--reset-via-api ()
  "Try to reset session via the reset-session HTTP endpoint.
Returns (ok . old-session-id) on success, nil on failure."
  (let* ((url (format "%s/api/alpha/agents/%s/reset-session"
                       claude-repl-api-url claude-repl-agent-id))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data "{}")
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t 10)
                   (error nil)))
         (result nil))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (condition-case nil
              (let* ((json-obj (json-parse-string
                                (buffer-substring (point) (point-max))
                                :object-type 'alist :null-object nil)))
                (when (alist-get 'ok json-obj)
                  (setq result (cons t (alist-get 'old-session-id json-obj)))))
            (error nil))))
      (kill-buffer buffer))
    result))

(defun claude-repl--reset-via-drawbridge ()
  "Try to reset session via Drawbridge REPL eval.
Returns (ok . old-session-id) on success, nil on failure."
  (let* ((clj-code (format "(let [r (swap-vals! futon3c.agency.registry/!registry update \"%s\" assoc :agent/session-id nil)] (get-in (first r) [\"%s\" :agent/session-id]))"
                           claude-repl-agent-id claude-repl-agent-id))
         (url (format "%s/eval" claude-repl-drawbridge-url))
         (url-request-method "POST")
         (token (claude-repl--drawbridge-token))
         (url-request-extra-headers
          `(("x-admin-token" . ,token)
            ("Content-Type" . "text/plain")))
         (url-request-data (encode-coding-string clj-code 'utf-8))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t 10)
                   (error nil)))
         (result nil))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (condition-case nil
              (let* ((json-obj (json-parse-string
                                (buffer-substring (point) (point-max))
                                :object-type 'alist :null-object nil)))
                (when (alist-get 'ok json-obj)
                  (setq result (cons t (alist-get 'value json-obj)))))
            (error nil))))
      (kill-buffer buffer))
    result))

(defun claude-repl-new-session ()
  "Reset the agent session so the next message starts a fresh conversation.
Useful when a session becomes poisoned (e.g. API rejects the conversation
history). Tries the reset-session endpoint first, falls back to Drawbridge."
  (interactive)
  (let* ((api-result (claude-repl--reset-via-api))
         (result (or api-result (claude-repl--reset-via-drawbridge)))
         (ok (car result))
         (old-sid (cdr result)))
    ;; Clear local session state regardless of server response
    (setq agent-chat--session-id nil)
    (when claude-repl-session-file
      (when (file-exists-p claude-repl-session-file)
        (delete-file claude-repl-session-file)))
    ;; Update the buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "(session: [^)]*)" (line-end-position 2) t)
          (replace-match (propertize "(session: new)"
                                     'face 'font-lock-comment-face)
                         t t))))
    (agent-chat-insert-message
     "system"
     (cond
      (ok
       (format "[Session reset — was %s. Next message starts fresh.]"
               (or old-sid "unknown")))
      (t
       "[Session reset locally only — could not reach server. Next message may still fail.]")))
    (goto-char (point-max))
    (message "claude-repl: session reset (server=%s, was %s)"
             (if ok "yes" "no") (or old-sid "nil"))))

(defun claude-repl--init-display ()
  "Draw the buffer header and prompt. Does NOT register or change identity.
Used by `claude-repl-clear' to redraw without losing the agent binding."
  (let* ((sf claude-repl-session-file)
         (existing-sid
          (when (and sf (file-exists-p sf))
            (let ((s (string-trim
                      (with-temp-buffer
                        (insert-file-contents-literally sf)
                        (buffer-string)))))
              (unless (string-empty-p s) s))))
         (title (if (claude-repl--workspace)
                    (format "claude repl [%s]" (claude-repl--workspace))
                  "claude repl")))
    (agent-chat-init-buffer
     (list :title title
           :session-id (or existing-sid
                           (format "%s (awaiting session)" claude-repl-agent-id))
           :modeline-fn #'claude-repl--build-modeline
           :face-alist `(("claude" . claude-repl-claude-face))
           :agent-name "claude"
           :agent-id claude-repl-agent-id
           :thinking-text "claude is thinking..."
           :thinking-prop 'claude-repl-thinking))
    (agent-chat-invariants-setup)))

(defun claude-repl--init ()
  "Initialize: register agent, set up workspace, draw buffer.
When running inside a named daemon (e.g. workspace1), use the
workspace name to disambiguate agent-id, session file, and buffer.
Then auto-register with the server and load existing session-id."
  (let ((ws (claude-repl--workspace)))
  ;; Auto-register: ask server for next available claude-N (I-1 compliant).
  ;; Each repl gets its own agent identity. If registration succeeds,
  ;; it sets claude-repl-agent-id. If it fails, keep default (claude-1).
    (claude-repl--auto-register)
  ;; Derive workspace-specific session file for disambiguation
    (when ws
    (unless (local-variable-p 'claude-repl--workspace-applied)
      (setq-local claude-repl-session-file
                  (format "/tmp/futon-session-id-%s" claude-repl-agent-id))
      (setq-local claude-repl--workspace-applied t)))
  (claude-repl--init-display)
  (let* ((sf claude-repl-session-file)
         (existing-sid
          (when (and sf (file-exists-p sf))
            (let ((s (string-trim
                      (with-temp-buffer
                        (insert-file-contents-literally sf)
                        (buffer-string)))))
              (unless (string-empty-p s) s)))))
    (when existing-sid
      (claude-repl--emit-session-start-evidence! existing-sid))))

;;;###autoload
(defun claude-repl ()
  "Start or switch to chat."
  (interactive)
  (let* ((ws (claude-repl--workspace))
         (bufname (if ws
                     (format "*claude-repl[%s]*" ws)
                   claude-repl--buffer-name))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-repl-mode)
        (claude-repl-mode)
        (claude-repl--init)))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(defun claude-repl-reconnect ()
  "Re-register this buffer's agent with the server.
Use after reloading claude-repl.el or when the agent binding is stale."
  (interactive)
  (claude-repl--auto-register)
  (message "claude-repl: now %s (session file: %s)"
           claude-repl-agent-id claude-repl-session-file))

(defun claude-repl-connect (agent-id)
  "Open a repl buffer connected to an existing AGENT-ID.
Skips auto-registration — binds directly to the named agent.
Use this to reconnect to an agent whose buffer was killed."
  (interactive
   (list (read-string "Agent ID: " "claude-2")))
  (let* ((ws (claude-repl--workspace))
         (bufname (format "*claude-repl:%s*" agent-id))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-repl-mode)
        (claude-repl-mode))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local claude-repl-agent-id agent-id)
      (setq-local claude-repl-session-file
                  (format "/tmp/futon-session-id-%s" agent-id))
      (setq-local claude-repl--workspace-applied t)
      ;; Rebind socket so the agent routes blackboard calls here
      (claude-repl--rebind-socket agent-id
                                  (or ws (and (boundp 'server-name) server-name)))
      (claude-repl--init-display))
    (pop-to-buffer buf)
    (goto-char (point-max))
    (message "claude-repl: connected to %s" agent-id)))

(provide 'claude-repl)
;;; claude-repl.el ends here
