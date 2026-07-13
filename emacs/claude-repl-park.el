;;; claude-repl-park.el --- Buffer-side parked-on continuations -*- lexical-binding: t; -*-

;; E-repl-continuations Car 2 (Emacs side / model B): when an agent parks its
;; REPL turn on dispatched work and the join completes, the futon3c backend makes
;; the assembled resume prompt available two ways — a `park-ready' WS push (instant,
;; when the agency WS is connected) and a ready-inbox the buffer POLLS
;; (GET /api/alpha/parked/ready, works today without the WS). Either way this
;; RESUMES THE TURN IN PLACE: it injects the resume prompt as input and sends it
;; through the normal turn machinery, so the continuation streams into the buffer
;; natively — no manual wake. Closes the "I'll get back to you" -> silence gap.

;;; Code:

(require 'claude-repl)
(require 'futon-agency-ws)
(require 'ring)

(defgroup claude-repl-park nil
  "Buffer-side parked-on continuations for the Claude REPL."
  :group 'agent-chat)

(defcustom claude-repl-park-auto-send t
  "When non-nil, a ready resume auto-sends in place; nil stages it for review."
  :type 'boolean :group 'claude-repl-park)

(defcustom claude-repl-park-poll-interval 3
  "Seconds between polls of the ready-inbox across live Claude REPL buffers."
  :type 'number :group 'claude-repl-park)

(defvar claude-repl-park--poll-timer nil)
(defvar claude-repl-park--last nil "Most recent ready item handled, for debugging.")

;; Bug 3: dedup ring of recently-handled park-ids. The server redelivers an
;; unacked lease after its deadline; this ring lets the buffer skip a duplicate
;; (and ACK it so the lease clears). Sized generously vs. the 3s poll interval.
(defvar claude-repl-park--seen-ids (make-ring 32)
  "Ring of park-ids recently handled by this Emacs, for idempotent redelivery.")

(defun claude-repl-park--seen-p (park-id)
  "Non-nil when PARK-ID was recently handled (in the dedup ring)."
  ;; ring.el has no `ring-map' (void-function in every process filter,
  ;; 2026-07-13); `ring-member' is the built-in membership test.
  (and (stringp park-id)
       (ring-member claude-repl-park--seen-ids park-id)))

(defun claude-repl-park--remember! (park-id)
  "Add PARK-ID to the dedup ring."
  (when (stringp park-id)
    (ring-insert claude-repl-park--seen-ids park-id)))

(cl-defun claude-repl-park--ack (park-id)
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

(defun claude-repl-park--resume-in-buffer (buf prompt park-id)
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
     ((and (stringp park-id) (claude-repl-park--seen-p park-id))
      (message "[park] skipping duplicate park %s in %s (already handled)"
               park-id (buffer-name buf))
      (claude-repl-park--ack park-id))            ; clear the stale lease
     (t
      (with-current-buffer buf
        (setq claude-repl-park--last (list :park-id park-id :buffer (buffer-name buf)))
        (when (fboundp 'agent-chat--ensure-prompt-markers!)
          (agent-chat--ensure-prompt-markers!))
        ;; Bug 3: remember this park-id BEFORE sending, so a fast redelivery is
        ;; deduped even before the ACK round-trips.
        (claude-repl-park--remember! park-id)
        (if claude-repl-park-auto-send
            (progn (message "[park] resuming %s in place (park %s)"
                            (buffer-name buf) park-id)
                   (agent-chat-send-unsolicited-input
                    #'claude-repl--call-claude-streaming
                    "claude"
                    prompt
                    "continuation"
                    (list :before-send (lambda (text)
                                         (claude-repl--emit-user-turn-evidence! text)
                                         (claude-repl--store-upsert-session)
                                         (claude-repl--open-frame text))
                          :on-response (lambda (text)
                                         (claude-repl--emit-assistant-turn-evidence! text)
                                         (claude-repl--emit-turn-commits-evidence!)
                                         (claude-repl--close-frame "done"))))
                   ;; Bug 3: ACK delivery after send returns without error. If the
                   ;; ACK is lost, the server redelivers; the dedup ring skips it.
                   (claude-repl-park--ack park-id))
          (message "[park] resume available in %s, but auto-send is disabled"
                   (buffer-name buf))))))))

;;;; WS path (bonus — instant when the agency WS is connected) -----------------

(defun claude-repl-park--target-buffer (agent session)
  (or (and (stringp session) (not (string-empty-p session))
           (claude-repl-find-buffer-by-session-id session))
      (and (stringp agent) (not (string-empty-p agent))
           (claude-repl-find-buffer-by-agent-id agent))))

(defun claude-repl-park--on-ready (frame)
  "Handle a `park-ready' WS FRAME (alist: agent/session/park-id/prompt)."
  (claude-repl-park--resume-in-buffer
   (claude-repl-park--target-buffer (alist-get 'agent frame) (alist-get 'session frame))
   (alist-get 'prompt frame)
   (alist-get 'park-id frame)))

;;;; Poll path (primary — works without the WS) -------------------------------

(defvar-local claude-repl-park--poll-inflight nil
  "Non-nil while an async ready-inbox poll is outstanding for this buffer.")

(defun claude-repl-park--poll-buffer-async (buf)
  "Async-poll BUF's ready-inbox and, on a ready item, resume in place.
Uses `url-retrieve' so a slow/hung server can NEVER freeze the UI (the old
synchronous GET is what made you reach for C-g)."
  (with-current-buffer buf
    (unless claude-repl-park--poll-inflight
      (let* ((agent claude-repl-agent-id)
             (session agent-chat--session-id)
             (url (format "%s/api/alpha/parked/ready?agent=%s&session=%s"
                          (string-remove-suffix "/" claude-repl-api-url)
                          (url-hexify-string (or agent ""))
                          (url-hexify-string (or session ""))))
             (url-request-method "GET"))
        (setq claude-repl-park--poll-inflight t)
        (url-retrieve
         url
         (lambda (status)
           (let ((resp-buf (current-buffer)))
             (when (buffer-live-p buf)
               (with-current-buffer buf (setq claude-repl-park--poll-inflight nil)))
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
                           (claude-repl-park--resume-in-buffer
                            buf (plist-get item :prompt) (plist-get item :park-id)))))))
               (when (buffer-live-p resp-buf) (kill-buffer resp-buf)))))
         nil t t)))))

(defun claude-repl-park--poll-once ()
  "Async-poll the ready-inbox for every live Claude REPL buffer (non-blocking).
Bug 2: the mid-turn busy gate is now SERVER-SIDE (handle-parked-ready checks the
registry :invoking status). The old agent-chat--streaming-started gate here was
unreliable in pouch-driven buffers, so it has been REMOVED — the server withholds
items from busy agents, so the buffer polls unconditionally and trusts the gate."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'claude-repl-mode)
          (claude-repl-park--poll-buffer-async buf))))))

;;;; Enable / disable ---------------------------------------------------------

;;;; Within-turn unification: DEFER a parked segment's finalization -----------

(defun claude-repl-park--turn-parked-p ()
  "Non-nil when this buffer's agent/session has more of a unified turn coming — an
outstanding park OR a ready resume already in the inbox (`more-pending', race-free
even for a fast dep).  `agent-chat-finish-turn!' uses this to DEFER a parked
segment's finalization so the unified turn finalizes exactly once."
  (ignore-errors
    (let* ((url (format "%s/api/alpha/parked?agent=%s&session=%s"
                        (string-remove-suffix "/" claude-repl-api-url)
                        (url-hexify-string (or claude-repl-agent-id ""))
                        (url-hexify-string (or agent-chat--session-id ""))))
           (resp (agent-chat-evidence-request-json "GET" url 5 nil)))
      (eq t (plist-get (plist-get resp :json) :more-pending)))))

(defun claude-repl-park--install-continued-fn ()
  "Wire the within-turn defer-detector into the current buffer."
  (setq-local agent-chat-turn-continued-fn #'claude-repl-park--turn-parked-p))

(defvar claude-repl-park--subscribed nil)

(defun claude-repl-park-enable ()
  "Start polling the ready-inbox and subscribe to `park-ready' WS frames."
  (interactive)
  (unless claude-repl-park--subscribed
    (futon-agency-ws-subscribe "park-ready" #'claude-repl-park--on-ready)
    (setq claude-repl-park--subscribed t))
  (unless claude-repl-park--poll-timer
    (setq claude-repl-park--poll-timer
          (run-with-timer claude-repl-park-poll-interval
                          claude-repl-park-poll-interval
                          #'claude-repl-park--poll-once)))
  ;; Within-turn unification: install the defer-detector in every repl buffer
  ;; (existing + future), so a parked segment finalizes once (on the continuation).
  (add-hook 'claude-repl-mode-hook #'claude-repl-park--install-continued-fn)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'claude-repl-mode)
          (claude-repl-park--install-continued-fn)))))
  (message "[park] enabled (poll %ss + WS park-ready + within-turn defer)"
           claude-repl-park-poll-interval))

(defun claude-repl-park-disable ()
  "Stop polling and unsubscribe."
  (interactive)
  (when claude-repl-park--poll-timer
    (cancel-timer claude-repl-park--poll-timer)
    (setq claude-repl-park--poll-timer nil))
  (when claude-repl-park--subscribed
    (futon-agency-ws-unsubscribe "park-ready" #'claude-repl-park--on-ready)
    (setq claude-repl-park--subscribed nil)))

(claude-repl-park-enable)

(provide 'claude-repl-park)
;;; claude-repl-park.el ends here
