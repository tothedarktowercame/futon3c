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

(defun claude-repl-park--resume-in-buffer (buf prompt park-id)
  "Resume the parked turn in BUF by injecting PROMPT and sending it in place.
Note: this buffer is driven by the server-side pouch, so we do NOT gate on the
buffer's `agent-chat--streaming-started' flag (it never resets here); the backend
pops each ready item exactly once, so a resume is delivered once."
  (when (and (buffer-live-p buf) (stringp prompt) (not (string-empty-p prompt)))
    (with-current-buffer buf
      (setq claude-repl-park--last (list :park-id park-id :buffer (buffer-name buf)))
      (when (fboundp 'agent-chat--ensure-prompt-markers!)
        (agent-chat--ensure-prompt-markers!))
      ;; Within-turn unification (race-free): the parked segment already rendered
      ;; its turn-end flair above the input.  Absorb it into THIS unified turn —
      ;; carry its elapsed forward and delete the divider — so the final segment
      ;; renders a single totaled flair, and the park/resume is invisible to Joe.
      (when (boundp 'agent-chat--accum-elapsed)
        (setq agent-chat--accum-elapsed
              (+ (or agent-chat--accum-elapsed 0)
                 (or (and (boundp 'agent-chat--last-flair-elapsed)
                          agent-chat--last-flair-elapsed)
                     0)))
        (when (boundp 'agent-chat--last-flair-elapsed)
          (setq agent-chat--last-flair-elapsed 0)))
      ;; Carry the parked segment's OUTPUT text forward too, so the final segment's
      ;; turn-evidence embeds the unified output — the per-turn pattern tag is built
      ;; over the whole turn, not just the first segment.
      (when (boundp 'agent-chat--accum-text)
        (setq agent-chat--accum-text
              (concat (or agent-chat--accum-text "")
                      (or (and (boundp 'agent-chat--last-assistant-text)
                               agent-chat--last-assistant-text)
                          "")))
        (when (boundp 'agent-chat--last-assistant-text)
          (setq agent-chat--last-assistant-text "")))
      (when (fboundp 'agent-chat--delete-existing-turn-flair-before-prompt)
        (agent-chat--delete-existing-turn-flair-before-prompt))
      ;; Insert at the MANAGED input position, re-derived after the flair removal,
      ;; so the prompt markers stay consistent and the next flair lands correctly.
      (if (and (fboundp 'agent-chat--ensure-prompt-markers!)
               (agent-chat--ensure-prompt-markers!)
               (markerp agent-chat--input-start))
          (progn (goto-char (marker-position agent-chat--input-start))
                 (delete-region (point) (point-max)))
        (goto-char (point-max)))
      ;; The assembled prompt self-describes (it carries the joined dep results),
      ;; so sending it drives the ordinary turn flow → the continuation streams in.
      (insert prompt)
      (if claude-repl-park-auto-send
          (progn (message "[park] resuming %s in place (park %s)"
                          (buffer-name buf) park-id)
                 ;; attribute the injected turn to "continuation", not the operator
                 (let ((agent-chat-user-speaker "continuation"))
                   (claude-repl-send-input)))
        (message "[park] resume staged in %s — RET to send" (buffer-name buf))))))

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
                     ;; evidence JSON parses as a PLIST (keyword keys); items too.
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
  "Async-poll the ready-inbox for every live, idle Claude REPL buffer (non-blocking)."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Gate BEFORE the GET: a mid-turn buffer retains its item (GET pops), so
        ;; a resume is never sent while a turn is streaming — it fires next idle poll.
        (when (and (eq major-mode 'claude-repl-mode)
                   (not (and (boundp 'agent-chat--streaming-started)
                             agent-chat--streaming-started)))
          (claude-repl-park--poll-buffer-async buf))))))

;;;; Enable / disable ---------------------------------------------------------

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
  (message "[park] enabled (poll %ss + WS park-ready + within-turn flair)"
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
