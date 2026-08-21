;;; agent-repl-park.el --- Buffer-side parked-on continuations, any frontend -*- lexical-binding: t; -*-

;; E-repl-continuations Car 2 (Emacs side / model B): when an agent parks its
;; REPL turn on dispatched work and the join completes, the futon3c backend puts
;; the assembled resume prompt in a durable ready-inbox that the buffer POLLS
;; (GET /api/alpha/parked/ready — lease → deliver → ACK), and sends a `park-ready'
;; WS frame as a wake-up POKE so the poll happens immediately rather than on the
;; next tick. The resume launches as its own unsolicited `continuation:' turn
;; (never via the operator input area), so the continuation streams into the
;; buffer natively — no manual wake, and no stolen operator reply slot. Closes
;; the "I'll get back to you" -> silence gap.

;; RENAMED 2026-08-20 from `claude-repl-park.el'.  The module stopped being
;; claude-specific when the resume path was moved onto `agent-repl-registry';
;; a name asserting otherwise is a stale claim about what the code does.
;; Compatibility aliases for the old public names are at the end of this file.

;;; Code:

(require 'claude-repl)
(require 'agent-repl-registry)
(require 'futon-agency-ws)
(require 'ring)
(require 'seq)

(defgroup agent-repl-park nil
  "Buffer-side parked-on continuations for the Claude REPL."
  :group 'agent-chat)

;; Old-name variable aliases must precede their referents so that a value an
;; operator already set under the pre-rename name is inherited, not shadowed.
(defvaralias 'claude-repl-park-auto-send 'agent-repl-park-auto-send)
(defvaralias 'claude-repl-park-poll-interval 'agent-repl-park-poll-interval)
(defvaralias 'claude-repl-park-participating-modes
  'agent-repl-park-participating-modes)

(defcustom agent-repl-park-auto-send t
  "When non-nil, a ready resume auto-sends in place; nil stages it for review."
  :type 'boolean :group 'agent-repl-park)

(defcustom agent-repl-park-poll-interval 3
  "Seconds between polls of the ready-inbox across participating REPL buffers."
  :type 'number :group 'agent-repl-park)

(defcustom agent-repl-park-participating-modes
  '(claude-repl-mode zai-repl-mode codex-repl-mode)
  "Major modes whose buffers participate in parked-on resume delivery.
Mode symbols do not load their defining libraries; an absent frontend is inert."
  :type '(repeat symbol) :group 'agent-repl-park)

(defvar agent-repl-park--poll-timer nil)
(defvar agent-repl-park--last nil "Most recent ready item handled, for debugging.")

;; Bug 3: dedup ring of recently-handled park-ids. The server redelivers an
;; unacked lease after its deadline; this ring lets the buffer skip a duplicate
;; (and ACK it so the lease clears). Sized generously vs. the 3s poll interval.
(defvar agent-repl-park--seen-ids (make-ring 32)
  "Ring of park-ids recently handled by this Emacs, for idempotent redelivery.")

(defun agent-repl-park--seen-p (park-id)
  "Non-nil when PARK-ID was recently handled (in the dedup ring)."
  ;; ring.el has no `ring-map' (void-function in every process filter,
  ;; 2026-07-13); `ring-member' is the built-in membership test.
  (and (stringp park-id)
       (ring-member agent-repl-park--seen-ids park-id)))

(defun agent-repl-park--remember! (park-id)
  "Add PARK-ID to the dedup ring."
  (when (stringp park-id)
    (ring-insert agent-repl-park--seen-ids park-id)))

(cl-defun agent-repl-park--ack (park-id)
  "POST /api/alpha/parked/ready/ack for PARK-ID (confirm delivery, clear lease).
Best-effort: failure means the server will redeliver after the lease deadline,
and the dedup ring will skip the duplicate, so correctness is preserved even
when the ACK is lost."
  (ignore-errors
    (let ((url (format "%s/api/alpha/parked/ready/ack"
                       (string-remove-suffix "/" claude-repl-api-url)))
          (url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data (format "{\"park-id\":\"%s\"}" park-id)))
      (url-retrieve url (lambda (_status)
                          (let ((buf (current-buffer)))
                            (unwind-protect
                                (ignore)
                              (when (buffer-live-p buf) (kill-buffer buf)))))
                    nil t t))))

(defun agent-repl-park--resume-in-buffer (buf prompt park-id)
  "Resume the parked turn in BUF as its own unsolicited continuation turn.
Bug 2 (E-park-delivery-losses): the busy gate is now SERVER-SIDE —
handle-parked-ready checks the registry's :invoking status before leasing, so a
mid-turn agent never receives a resume. This buffer does NOT gate on
agent-chat--streaming-started (it was unreliable in pouch-driven buffers).
Bug 3: idempotent delivery — if PARK-ID was recently handled (dedup ring), skip
it and ACK so the stale lease clears. After a successful send, ACK to confirm.
One-slot-shift fix: the prompt is never inserted into the operator input area;
it is launched through `agent-chat-send-unsolicited-input' as `continuation:',
so any operator input typed while it streams queues behind it and gets its own
reply slot."
  (when (and (buffer-live-p buf) (stringp prompt) (not (string-empty-p prompt)))
    (cond
     ((and (stringp park-id) (agent-repl-park--seen-p park-id))
      (message "[park] skipping duplicate park %s in %s (already handled)"
               park-id (buffer-name buf))
      (agent-repl-park--ack park-id))            ; clear the stale lease
     (t
      (with-current-buffer buf
        (setq agent-repl-park--last (list :park-id park-id :buffer (buffer-name buf)))
        (when (fboundp 'agent-chat--ensure-prompt-markers!)
          (agent-chat--ensure-prompt-markers!))
        ;; Bug 3: remember this park-id BEFORE sending, so a fast redelivery is
        ;; deduped even before the ACK round-trips.
        (agent-repl-park--remember! park-id)
        (if agent-repl-park-auto-send
            (progn (message "[park] resuming %s in place (park %s)"
                            (buffer-name buf) park-id)
                   (agent-chat-send-unsolicited-input
                    (agent-repl-capability :sender)
                    (or (agent-repl-capability :agent-name) "agent")
                    prompt
                    "continuation"
                    (agent-repl-capability :hooks))
                   ;; Bug 3: ACK delivery after send returns without error. If the
                   ;; ACK is lost, the server redelivers; the dedup ring skips it.
                   (agent-repl-park--ack park-id))
          (message "[park] resume available in %s, but auto-send is disabled"
                   (buffer-name buf))))))))

;;;; WS path (bonus — instant when the agency WS is connected) -----------------

(defun agent-repl-park--eligible-modes ()
  "Modes that may be resumed in place.

Registered frontends (`agent-repl-registered-modes') union the legacy
`agent-repl-park-participating-modes' custom, so an operator override still
works and an unregistered frontend is simply never polled."
  (delete-dups
   (append (agent-repl-registered-modes)
           (and (boundp 'agent-repl-park-participating-modes)
                agent-repl-park-participating-modes))))

(defun agent-repl-park--target-buffer (agent session)
  (or (and (stringp session) (not (string-empty-p session))
           (claude-repl-find-buffer-by-session-id session))
      (and (stringp agent) (not (string-empty-p agent))
           (claude-repl-find-buffer-by-agent-id agent))
      (seq-find
       (lambda (buf)
         (and (buffer-live-p buf)
              (with-current-buffer buf
                (and (memq major-mode (agent-repl-park--eligible-modes))
                     (or (and (stringp session) (not (string-empty-p session))
                              (equal agent-chat--session-id session))
                         (and (stringp agent) (not (string-empty-p agent))
                              (equal agent-chat--agent-id agent)))))))
       (buffer-list))))

(defun agent-repl-park--on-ready (frame)
  "Handle a `park-ready' WS FRAME as a wake-up POKE: poll the inbox now.
The frame is not an authoritative delivery — the resume lives in the durable
ready-inbox and is fetched through the ordinary lease → deliver → ACK path, so
the server busy gate, lease redelivery, and park-id dedup all apply. A lost or
misrouted frame costs one poll interval of latency, never the resume."
  (let ((buf (agent-repl-park--target-buffer (alist-get 'agent frame)
                                              (alist-get 'session frame))))
    (when (buffer-live-p buf)
      (agent-repl-park--poll-buffer-async buf))))

;;;; Poll path (primary — works without the WS) -------------------------------

(defvar-local agent-repl-park--poll-inflight nil
  "Non-nil while an async ready-inbox poll is outstanding for this buffer.")
(defvar-local agent-repl-followup--poll-inflight nil)

(defun agent-repl-followup--ack (id api-url)
  (ignore-errors
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data (format "{\"followup-id\":\"%s\"}" id)))
      (url-retrieve (format "%s/api/alpha/followups/ready/ack"
                            (string-remove-suffix "/" api-url))
                    (lambda (_status) (kill-buffer (current-buffer))) nil t t))))

(defun agent-repl-followup--poll-buffer-async (buf)
  "Poll and deliver one typed external followup to BUF."
  (with-current-buffer buf
    (unless agent-repl-followup--poll-inflight
      (let* ((agent (or (bound-and-true-p agent-chat--agent-id)
                        (bound-and-true-p claude-repl-agent-id)))
             (session agent-chat--session-id)
             (api-url (or (bound-and-true-p claude-repl-api-url)
                          (agent-repl-api-base-url)))
             (url (format "%s/api/alpha/followups/ready?agent=%s&session=%s"
                          (string-remove-suffix "/" api-url)
                          (url-hexify-string (or agent ""))
                          (url-hexify-string (or session "")))))
        (setq agent-repl-followup--poll-inflight t)
        (url-retrieve
         url
         (lambda (status)
           (let ((response-buffer (current-buffer)))
             (when (buffer-live-p buf)
               (with-current-buffer buf (setq agent-repl-followup--poll-inflight nil)))
             (unwind-protect
                 (unless (plist-get status :error)
                   (goto-char (point-min))
                   (when (search-forward "\n\n" nil t)
                     (let* ((json (ignore-errors
                                    (json-parse-string
                                     (buffer-substring-no-properties (point) (point-max))
                                     :object-type 'plist :null-object nil :false-object nil)))
                            (ready (plist-get json :ready)))
                       (when (and (vectorp ready) (> (length ready) 0)
                                  (buffer-live-p buf))
                         (let* ((item (aref ready 0))
                                (id (plist-get item :followup-id)))
                           (unless (agent-repl-park--seen-p id)
                             (agent-repl-park--remember! id)
                             (with-current-buffer buf
                               (agent-chat-send-unsolicited-input
                                (agent-repl-capability :sender)
                                (or (agent-repl-capability :agent-name) "agent")
                                (plist-get item :prompt) "followup"
                                (agent-repl-capability :hooks))))
                           (agent-repl-followup--ack id api-url))))))
               (when (buffer-live-p response-buffer) (kill-buffer response-buffer)))))
         nil t t)))))

(defun agent-repl-park--poll-buffer-async (buf)
  "Async-poll BUF's ready-inbox and, on a ready item, resume in place.
Uses `url-retrieve' so a slow/hung server can NEVER freeze the UI (the old
synchronous GET is what made you reach for C-g)."
  (with-current-buffer buf
    (unless agent-repl-park--poll-inflight
      (let* ((agent (or (bound-and-true-p agent-chat--agent-id)
                        (bound-and-true-p claude-repl-agent-id)))
             (session agent-chat--session-id)
             (api-url (or (bound-and-true-p claude-repl-api-url)
                          (agent-repl-api-base-url)))
             (url (format "%s/api/alpha/parked/ready?agent=%s&session=%s"
                          (string-remove-suffix "/" api-url)
                          (url-hexify-string (or agent ""))
                          (url-hexify-string (or session ""))))
             (url-request-method "GET"))
        (setq agent-repl-park--poll-inflight t)
        (url-retrieve
         url
         (lambda (status)
           (let ((resp-buf (current-buffer)))
             (when (buffer-live-p buf)
               (with-current-buffer buf (setq agent-repl-park--poll-inflight nil)))
             (unwind-protect
                 (unless (plist-get status :error)
                   (goto-char (point-min))
                   (when (search-forward "\n\n" nil t)
                     ;; JSON parses as a PLIST (keyword keys); items too.
                     (let* ((body (buffer-substring-no-properties (point) (point-max)))
                            (json (ignore-errors
                                    (json-parse-string body :object-type 'plist
                                                       :null-object nil :false-object nil)))
                            (ready (plist-get json :ready)))
                       (when (and (vectorp ready) (> (length ready) 0) (buffer-live-p buf))
                         (let ((item (aref ready 0)))    ; one per poll; extras next tick
                           (agent-repl-park--resume-in-buffer
                            buf (plist-get item :prompt) (plist-get item :park-id)))))))
               (when (buffer-live-p resp-buf) (kill-buffer resp-buf)))))
         nil t t)))))

(defun agent-repl-park--poll-once ()
  "Poll every participating REPL buffer's ready-inbox asynchronously.
Bug 2: the mid-turn busy gate is now server-side.  `handle-parked-ready'
checks the registry's :invoking status.  The old
`agent-chat--streaming-started' gate was unreliable in pouch-driven buffers,
so it has been removed.  The server withholds items from busy agents; the
buffer polls unconditionally and trusts that gate."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (memq major-mode (agent-repl-park--eligible-modes))
                   ;; The ready-inbox is POLL-AND-CONSUME: never poll a buffer
                   ;; we could not then drive, or the item is destroyed.
                   (agent-repl-drivable-p major-mode))
          (agent-repl-park--poll-buffer-async buf)
          (agent-repl-followup--poll-buffer-async buf))))))

;;;; Enable / disable ---------------------------------------------------------

;;;; Within-turn unification: DEFER a parked segment's finalization -----------

(defun agent-repl-park--turn-parked-p ()
  "Return non-nil when this agent/session has more of a unified turn coming.
This means an outstanding park or a ready resume is already in the inbox
(`more-pending', race-free even for a fast dependency).
`agent-chat-finish-turn!' uses this to defer a parked segment's finalization,
so the unified turn finalizes exactly once."
  (ignore-errors
    (let* ((url (format "%s/api/alpha/parked?agent=%s&session=%s"
                        (string-remove-suffix "/" claude-repl-api-url)
                        (url-hexify-string (or claude-repl-agent-id ""))
                        (url-hexify-string (or agent-chat--session-id ""))))
           (resp (agent-chat-evidence-request-json "GET" url 5 nil)))
      (eq t (plist-get (plist-get resp :json) :more-pending)))))

(defun agent-repl-park--install-continued-fn ()
  "Wire the within-turn defer-detector into the current buffer."
  (setq-local agent-chat-turn-continued-fn #'agent-repl-park--turn-parked-p))

(defvar agent-repl-park--subscribed nil)

(defun agent-repl-park-enable ()
  "Start polling the ready-inbox and subscribe to `park-ready' WS frames."
  (interactive)
  (unless agent-repl-park--subscribed
    (futon-agency-ws-subscribe "park-ready" #'agent-repl-park--on-ready)
    (setq agent-repl-park--subscribed t))
  (unless agent-repl-park--poll-timer
    (setq agent-repl-park--poll-timer
          (run-with-timer agent-repl-park-poll-interval
                          agent-repl-park-poll-interval
                          #'agent-repl-park--poll-once)))
  ;; Within-turn unification: install the defer-detector in every repl buffer
  ;; (existing + future), so a parked segment finalizes once (on the continuation).
  (add-hook 'claude-repl-mode-hook #'agent-repl-park--install-continued-fn)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'claude-repl-mode)
          (agent-repl-park--install-continued-fn)))))
  (message "[park] enabled (poll %ss + WS park-ready + within-turn defer)"
           agent-repl-park-poll-interval))

(defun agent-repl-park-disable ()
  "Stop polling and unsubscribe."
  (interactive)
  (when agent-repl-park--poll-timer
    (cancel-timer agent-repl-park--poll-timer)
    (setq agent-repl-park--poll-timer nil))
  (when agent-repl-park--subscribed
    (futon-agency-ws-unsubscribe "park-ready" #'agent-repl-park--on-ready)
    (setq agent-repl-park--subscribed nil)))

(agent-repl-park-enable)

;;;; Compatibility with the pre-2026-08-20 name ------------------------------
;; One in-tree caller (`claude-repl-bootstrap.el') and any operator init that
;; set the old defcustoms.  Aliases keep both working; the old name is not
;; deprecated-with-a-warning because there is nothing for a user to fix.

(defalias 'claude-repl-park-enable #'agent-repl-park-enable)
(defalias 'claude-repl-park-disable #'agent-repl-park-disable)
(defalias 'claude-repl-park--turn-parked-p #'agent-repl-park--turn-parked-p)

(provide 'claude-repl-park)
(provide 'agent-repl-park)
;;; agent-repl-park.el ends here
