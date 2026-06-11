;;; futon-agency-ws.el --- Shared Agency WebSocket connector -*- lexical-binding: t; -*-

;; One persistent WS connection to futon3c's /agency/ws, with a
;; subscribe/dispatch layer so multiple consumers (the *agents-ws* HUD,
;; smart-cursor overlays, bells) share a single socket instead of each
;; owning plumbing.
;;
;; Born from the M-agency-hardening "Solve Agents Flickers" diagnosis
;; (2026-06-10): the *agents* blackboard HUD flickers because three racing
;; async-emacsclient pushers overwrite each other out of order, while the
;; server's `agents_status` WS broadcast reaches zero connected clients.
;; This connector is the single ordered reader; the HUD renderer below is
;; its first subscriber. v1 renders to *agents-ws* so the two channels can
;; be compared side by side before any cutover of the emacsclient push.
;;
;; The connection identifies as a synthetic OBSERVER id (default
;; "emacs-hud"), never as a real agent — borrowing a live agent's identity
;; is the I-1 violation the orthogonal-WS-sender lesson warns about.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(declare-function websocket-open "websocket" (url &rest args))
(declare-function websocket-openp "websocket" (websocket))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-close "websocket" (websocket))
(declare-function websocket-frame-text "websocket" (frame))
(declare-function websocket-frame-p "websocket" (frame))
(declare-function posframe-show "posframe" (buffer-or-name &rest args))
(defvar futon-agent-cursor--marker)

(defgroup futon-agency-ws nil
  "Shared WebSocket connector for the futon3c Agency."
  :group 'applications)

(defcustom futon-agency-ws-url "ws://localhost:7070/agency/ws"
  "WebSocket URL for the futon3c Agency endpoint."
  :type 'string
  :group 'futon-agency-ws)

(defcustom futon-agency-ws-observer-id "emacs-hud"
  "Synthetic observer identity for this connection.
Must NOT be a real agent id; observers receive broadcasts and answer
nothing."
  :type 'string
  :group 'futon-agency-ws)

(defcustom futon-agency-ws-reconnect t
  "When non-nil, reconnect automatically with capped backoff."
  :type 'boolean
  :group 'futon-agency-ws)

(defvar futon-agency-ws--ws nil)
(defvar futon-agency-ws--ready nil)
(defvar futon-agency-ws--session-id nil)
(defvar futon-agency-ws--subscribers (make-hash-table :test #'equal)
  "Frame type (string) -> list of handler functions (frame-alist -> _).
The key \"*\" receives every frame.")
(defvar futon-agency-ws--reconnect-timer nil)
(defvar futon-agency-ws--reconnect-delay 1)
(defvar futon-agency-ws--last-frame nil)
(defvar futon-agency-ws--frame-count 0)

(defcustom futon-agency-completion-watched-agents '("fable-2" "claude-3")
  "Agent ids whose invoking→idle transitions should raise a completion bubble."
  :type '(repeat string)
  :group 'futon-agency-ws)

(defcustom futon-agency-completion-posframe-timeout 5
  "Seconds before an agent completion posframe disappears."
  :type 'number
  :group 'futon-agency-ws)

(defvar futon-agency-completion--previous-status nil
  "Previous agents_status snapshot as an alist of (agent-id . status).")

(defvar futon-agency-completion--bubble-buffer " *futon-agent-completion* ")

(defun futon-agency-ws--require-websocket ()
  (unless (require 'websocket nil t)
    (user-error "futon-agency-ws needs the `websocket' package")))

(defun futon-agency-ws-subscribe (type handler)
  "Subscribe HANDLER to frames of TYPE (a string; \"*\" for all).
HANDLER receives the parsed frame alist. Idempotent per (TYPE, HANDLER)."
  (let ((existing (gethash type futon-agency-ws--subscribers)))
    (unless (memq handler existing)
      (puthash type (cons handler existing) futon-agency-ws--subscribers))))

(defun futon-agency-ws-unsubscribe (type handler)
  "Remove HANDLER from TYPE's subscriber list."
  (puthash type (delq handler (gethash type futon-agency-ws--subscribers))
           futon-agency-ws--subscribers))

(defun futon-agency-ws--dispatch (frame)
  "Dispatch parsed FRAME to type and wildcard subscribers.
A handler error never kills the socket loop."
  (setq futon-agency-ws--last-frame frame
        futon-agency-ws--frame-count (1+ futon-agency-ws--frame-count))
  (let ((type (alist-get 'type frame)))
    (dolist (handler (append (and type (gethash type futon-agency-ws--subscribers))
                             (gethash "*" futon-agency-ws--subscribers)))
      (condition-case err
          (funcall handler frame)
        (error (message "[futon-agency-ws] handler error on %s: %s"
                        type (error-message-string err)))))))

(defun futon-agency-ws--on-message (_ws frame)
  (when (and frame (websocket-frame-p frame))
    (let ((parsed (condition-case nil
                      (json-parse-string (websocket-frame-text frame)
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil)
                    (error nil))))
      (when parsed
        (when (equal (alist-get 'type parsed) "ready_ack")
          (setq futon-agency-ws--ready t
                futon-agency-ws--reconnect-delay 1))
        (futon-agency-ws--dispatch parsed)))))

(defun futon-agency-ws--send-json (payload)
  (when (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws))
    (websocket-send-text futon-agency-ws--ws (json-encode payload))))

(defun futon-agency-ws--on-open (_ws)
  (futon-agency-ws--send-json
   `((type . "ready")
     (agent_id . ,futon-agency-ws-observer-id)
     (session_id . ,futon-agency-ws--session-id))))

(defun futon-agency-ws--schedule-reconnect ()
  (when (and futon-agency-ws-reconnect
             (not futon-agency-ws--reconnect-timer))
    (setq futon-agency-ws--reconnect-timer
          (run-at-time futon-agency-ws--reconnect-delay nil
                       (lambda ()
                         (setq futon-agency-ws--reconnect-timer nil
                               futon-agency-ws--reconnect-delay
                               (min 30 (* 2 futon-agency-ws--reconnect-delay)))
                         (futon-agency-ws-connect))))))

(defun futon-agency-ws--on-close (_ws)
  (setq futon-agency-ws--ready nil)
  (futon-agency-ws--schedule-reconnect))

(defun futon-agency-ws-connect ()
  "Open (or re-open) the shared Agency WS connection."
  (interactive)
  (futon-agency-ws--require-websocket)
  (futon-agency-ws-disconnect 'no-cancel)
  (setq futon-agency-ws--session-id
        (format "%s-%06x" futon-agency-ws-observer-id (random #x1000000)))
  (setq futon-agency-ws--ws
        (websocket-open
         (format "%s?agent-id=%s&session-id=%s"
                 futon-agency-ws-url
                 (url-hexify-string futon-agency-ws-observer-id)
                 (url-hexify-string futon-agency-ws--session-id))
         :on-open #'futon-agency-ws--on-open
         :on-message #'futon-agency-ws--on-message
         :on-close #'futon-agency-ws--on-close
         :on-error (lambda (_ws _type err)
                     (message "[futon-agency-ws] %s" err)))))

(defun futon-agency-ws-disconnect (&optional no-cancel)
  "Close the connection. With NO-CANCEL, leave any reconnect timer alone."
  (interactive)
  (unless no-cancel
    (when futon-agency-ws--reconnect-timer
      (cancel-timer futon-agency-ws--reconnect-timer)
      (setq futon-agency-ws--reconnect-timer nil)))
  (when (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws))
    (ignore-errors (websocket-close futon-agency-ws--ws)))
  (setq futon-agency-ws--ws nil
        futon-agency-ws--ready nil))

(defun futon-agency-ws-status ()
  "Echo connector status."
  (interactive)
  (message "futon-agency-ws open=%s ready=%s frames=%d observer=%s"
           (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws) t)
           futon-agency-ws--ready
           futon-agency-ws--frame-count
           futon-agency-ws-observer-id))

;;;; Completion bubbles ------------------------------------------------------

(defun futon-agency-completion--status-map (frame)
  "Return FRAME's agents as an alist of (agent-id . status)."
  (mapcar (lambda (entry)
            (let ((agent (if (symbolp (car entry))
                             (symbol-name (car entry))
                           (format "%s" (car entry))))
                  (info (cdr entry)))
              (cons agent (alist-get 'status info))))
          (or (alist-get 'agents frame) '())))

(defun futon-agency-completion--done-agents (previous current &optional watched)
  "Return watched agents moving from invoking in PREVIOUS to idle in CURRENT."
  (let ((watched (or watched futon-agency-completion-watched-agents)))
    (cl-loop for agent in watched
             when (and (equal (cdr (assoc agent previous)) "invoking")
                       (equal (cdr (assoc agent current)) "idle"))
             collect agent)))

(defun futon-agency-completion--cursor-marker ()
  "Return the placed agent cursor marker, or nil."
  (and (boundp 'futon-agent-cursor--marker)
       (markerp futon-agent-cursor--marker)
       (marker-buffer futon-agent-cursor--marker)
       futon-agent-cursor--marker))

(defun futon-agency-completion--show (agent)
  "Render AGENT's completion bubble without moving point or stealing focus."
  (let ((text (format "%s done — what next?" agent))
        (marker (futon-agency-completion--cursor-marker)))
    (if (and marker (require 'posframe nil t))
        (with-current-buffer (marker-buffer marker)
          (posframe-show futon-agency-completion--bubble-buffer
                         :string text
                         :position marker
                         :timeout futon-agency-completion-posframe-timeout
                         :accept-focus nil))
      (message "%s" text))))

(defun futon-agency-completion--handle-agents-status (frame)
  "Detect watched invoking→idle transitions in an agents_status FRAME."
  (let* ((current (futon-agency-completion--status-map frame))
         (done (futon-agency-completion--done-agents
                futon-agency-completion--previous-status current)))
    (setq futon-agency-completion--previous-status current)
    (dolist (agent done)
      (futon-agency-completion--show agent))))

(defun futon-agency-completion-bubbles-enable ()
  "Subscribe completion bubbles to agents_status frames and connect the socket."
  (interactive)
  (futon-agency-ws-subscribe "agents_status" #'futon-agency-completion--handle-agents-status)
  (unless (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws))
    (futon-agency-ws-connect)))

(defun futon-agency-completion-bubbles-disable ()
  "Unsubscribe completion bubbles from agents_status frames."
  (interactive)
  (futon-agency-ws-unsubscribe "agents_status" #'futon-agency-completion--handle-agents-status)
  (setq futon-agency-completion--previous-status nil))

;;;; The first subscriber: the agents HUD ------------------------------------

(defcustom futon-agency-hud-buffer-name "*agents-ws*"
  "Buffer the WS-fed agents HUD renders into.
v1 deliberately does NOT use *agents* — render side by side with the
flickery blackboard HUD first; cut over only after comparison."
  :type 'string
  :group 'futon-agency-ws)

(defface futon-agency-hud-idle-face
  '((t :inherit success))
  "Face for idle agents.")

(defface futon-agency-hud-busy-face
  '((t :inherit warning))
  "Face for invoking/busy agents.")

(defvar futon-agency-hud--last-count nil)
(defvar futon-agency-hud--render-count 0)

(defun futon-agency-hud--render (frame)
  "Render an agents_status FRAME into the HUD buffer.
Single writer; never selects the window or steals point."
  (let ((agents (alist-get 'agents frame))
        (count (alist-get 'count frame)))
    (setq futon-agency-hud--last-count count
          futon-agency-hud--render-count (1+ futon-agency-hud--render-count))
    (with-current-buffer (get-buffer-create futon-agency-hud-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize (format "AGENTS (ws) — %s registered — %s\n"
                                    (or count "?")
                                    (format-time-string "%H:%M:%S"))
                            'face 'mode-line-emphasis))
        (insert (make-string 46 ?─) "\n")
        (dolist (entry agents)
          (let* ((aid (symbol-name (car entry)))
                 (info (cdr entry))
                 (status (or (alist-get 'status info) "?"))
                 (type (or (alist-get 'type info) "?"))
                 (activity (alist-get 'invoke-activity info))
                 (busy (member status '("invoking" "busy"))))
            (insert (propertize (format "  %-12s" aid)
                                'face (if busy 'futon-agency-hud-busy-face
                                        'futon-agency-hud-idle-face))
                    (format " %-9s %-7s" status type)
                    (if activity (format " %s" activity) "")
                    "\n")))
        (insert "\n" (propertize
                      (format "renders: %d (one writer, one socket)"
                              futon-agency-hud--render-count)
                      'face 'shadow))))))

(defun futon-agency-hud-enable ()
  "Subscribe the HUD renderer and connect the shared socket."
  (interactive)
  (futon-agency-ws-subscribe "agents_status" #'futon-agency-hud--render)
  (futon-agency-ws-subscribe "agents_status" #'futon-agency-completion--handle-agents-status)
  (unless (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws))
    (futon-agency-ws-connect)))

(defun futon-agency-hud-disable ()
  "Unsubscribe the HUD renderer (socket stays for other subscribers)."
  (interactive)
  (futon-agency-ws-unsubscribe "agents_status" #'futon-agency-hud--render))

(provide 'futon-agency-ws)
;;; futon-agency-ws.el ends here
