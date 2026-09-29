;;; turn-stepper.el --- 2-up side window stepping through operator-turn frames -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A read-only side window (`*象 stepper*') that steps through the frames
;; produced by futon3c/scripts/turn_frames.py for one REPL session:
;;
;;   SAID      — the operator turn (time + text)
;;   PARSE     — one line per fragment: combined intent (or "disagree: a / b")
;;   PATTERNS  — matched ids, proposals grouped under parent, rejected count
;;   HAPPENED  — what happened between this turn and the next
;;
;; Modelled on futon0/contrib/stack-hud.el's `*Stack Context*` side window.
;;
;; Usage: `M-x turn-stepper' in a claude-repl / codex-repl buffer.  The
;; session id is read from the buffer-local `agent-chat--session-id'
;; (defvar-local in agent-chat.el, used by claude-repl buffers).
;;
;; Keys in the stepper buffer:
;;   n / p   next / previous frame
;;   g       refresh (re-run turn_frames.py asynchronously)
;;   q       quit the window
;;   RET     on the SAID heading: scroll the REPL window to that turn
;;
;; This file never writes to the REPL, the evidence store, or the analysis
;; files.  It is not required anywhere; load it manually.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defgroup turn-stepper nil
  "Side window stepping through operator-turn frames."
  :group 'convenience)

(defcustom turn-stepper-script
  (expand-file-name "../scripts/turn_frames.py"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Path to the turn_frames.py script."
  :type 'file
  :group 'turn-stepper)

(defcustom turn-stepper-python "python3"
  "Python interpreter used to run `turn-stepper-script'."
  :type 'string
  :group 'turn-stepper)

(defcustom turn-stepper-buffer-name "*象 stepper*"
  "Name of the dedicated stepper buffer."
  :type 'string
  :group 'turn-stepper)

(defcustom turn-stepper-window-side 'right
  "Preferred side for the stepper window.
The value is passed to `display-buffer-in-side-window'."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'turn-stepper)

(defcustom turn-stepper-window-width 0.42
  "Width (fraction of frame) of the stepper side window."
  :type 'number
  :group 'turn-stepper)

;;; ---------------------------------------------------------------- state

(defvar turn-stepper--cache (make-hash-table :test 'equal)
  "Session id -> list of frames (most recent fetch).")

(defvar-local turn-stepper--session-id nil
  "Session id displayed in this stepper buffer.")

(defvar-local turn-stepper--frames nil
  "Frames (list of alists) for this stepper buffer.")

(defvar-local turn-stepper--index 0
  "Index of the frame currently shown (0-based).")

(defvar-local turn-stepper--source-buffer nil
  "REPL buffer this stepper is attached to.")

;;; ---------------------------------------------------------------- helpers

(defun turn-stepper--session-id-in-buffer (buffer)
  "Return the session id stored in BUFFER, or nil.
REPL buffers keep it in the buffer-local variable
`agent-chat--session-id' (agent-chat.el)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (boundp 'agent-chat--session-id)
           (stringp agent-chat--session-id)
           (not (string-empty-p agent-chat--session-id))
           (not (equal agent-chat--session-id "pending"))
           agent-chat--session-id))))

(defun turn-stepper--parse-frames (json-string)
  "Parse JSON-STRING (output of turn_frames.py) into a list of frame alists.
Uses json.el (not the native parser) so keys come back as symbols;
JSON null and false both read as nil."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-null nil)
        (json-false nil))
    (let ((data (json-read-from-string json-string)))
      (if (listp data) data (list data)))))

(defun turn-stepper--clamp-index (index count)
  "Clamp INDEX into [0, COUNT-1]; return 0 when COUNT is not positive."
  (if (<= count 0) 0 (max 0 (min index (1- count)))))

(defun turn-stepper--short-id (session-id)
  "First 8 characters of SESSION-ID."
  (if (and (stringp session-id) (> (length session-id) 8))
      (substring session-id 0 8)
    (or session-id "?")))

(defun turn-stepper--aget (key alist)
  "Assoc KEY (a symbol, as interned by json.el) in ALIST, tolerating junk."
  (and (listp alist) (cdr (assq key alist))))

;;; ---------------------------------------------------------------- rendering

(defun turn-stepper--render-parse (parse)
  "Render the PARSE section of a frame's parse alist as a string."
  (let ((status (turn-stepper--aget (quote status) parse))
        (fragments (turn-stepper--aget (quote fragments) parse)))
    (if (not (equal status "analyzed"))
        (format "  (%s)\n" (or status "missing"))
      (if (null fragments)
          "  (no fragments)\n"
        (mapconcat
         (lambda (frag)
           (let* ((labels (turn-stepper--aget (quote labels) frag))
                  (combined (turn-stepper--aget (quote combined) frag))
                  (disagree (turn-stepper--aget (quote disagree) frag))
                  (intents (delq nil (mapcar (lambda (l)
                                               (turn-stepper--aget (quote intent) l))
                                             labels)))
                  (head (if (and disagree (> (length intents) 1))
                            (format "disagree: %s"
                                    (string-join intents " / "))
                          (or combined "(unlabelled)")))
                  (sources (mapconcat
                            (lambda (l)
                              (format "%s→%s"
                                      (turn-stepper--aget (quote source) l)
                                      (turn-stepper--aget (quote intent) l)))
                            labels ", ")))
             (format "  [%s] %s\n      (%s)"
                     head
                     (or (turn-stepper--aget (quote text) frag) "")
                     sources)))
         fragments
         "\n")))))

(defun turn-stepper--pattern-id (item)
  "Best-effort id string for a matched-pattern ITEM (string or alist)."
  (cond
   ((stringp item) item)
   ((listp item) (or (turn-stepper--aget (quote id) item)
                     (format "%s" item)))
   (t (format "%s" item))))

(defun turn-stepper--render-patterns (patterns)
  "Render the PATTERNS section of a frame's patterns alist as a string."
  (let ((matched (turn-stepper--aget (quote matched) patterns))
        (rejected (turn-stepper--aget (quote rejected) patterns))
        (proposed (turn-stepper--aget (quote proposed_by_parent) patterns)))
    (concat
     (if matched
         (concat "  matched: "
                 (string-join (mapcar #'turn-stepper--pattern-id matched) ", ")
                 "\n")
       "  matched: (none)\n")
     (if proposed
         (mapconcat
          (lambda (group)
            (let ((parent (car group))
                  (children (cdr group)))
              (concat (format "  proposed under %s:\n" parent)
                      (mapconcat
                       (lambda (c)
                         (format "    - %s — %s (fragment %s)"
                                 (or (turn-stepper--aget (quote id) c) "?")
                                 (or (turn-stepper--aget (quote title) c) "?")
                                 (or (turn-stepper--aget (quote fragment) c) "?")))
                       children "\n"))))
          proposed "\n")
       "  proposed: (none)")
     (format "\n  rejected: %d\n" (length rejected)))))

(defun turn-stepper--render-happened-row (row)
  "Render one HAPPENED ROW alist as a single-line string."
  (let* ((at (or (turn-stepper--aget (quote at) row) "?"))
         (type (or (turn-stepper--aget (quote type) row) "?"))
         (summary (turn-stepper--aget (quote summary) row))
         (detail
          (cond
           ((and (listp summary)
                 (equal (turn-stepper--aget (quote event) summary) "turn-commits"))
            ;; Commits made in any repo during the turn window — never
            ;; labelled as the agent's own commits.
            (let ((commits (turn-stepper--aget (quote commits) summary)))
              (format "%d commit(s) in window: %s"
                      (length commits)
                      (string-join
                       (mapcar (lambda (c)
                                 (format "%s %s %s: %s"
                                         (or (turn-stepper--aget (quote repo) c) "?")
                                         (turn-stepper--short-id
                                          (turn-stepper--aget (quote sha) c))
                                         (or (turn-stepper--aget (quote author) c) "?")
                                         (or (turn-stepper--aget (quote subject) c) "?")))
                               commits)
                       " | "))))
           ((and (listp summary) (turn-stepper--aget (quote intent) summary))
            (format "intent=%s fragment=%s target=%s"
                    (turn-stepper--aget (quote intent) summary)
                    (or (turn-stepper--aget (quote fragment-id) summary) "?")
                    (or (turn-stepper--aget (quote target) summary) "?")))
           ((listp summary)
            (let ((text (turn-stepper--aget (quote text) summary)))
              (if text
                  (truncate-string-to-width text 80 nil nil "…")
                (format "%s" (or (turn-stepper--aget (quote event) summary) "")))))
           (t ""))))
    (format "  %s  %-28s %s" at type detail)))

(defun turn-stepper--render-frame (frame _index _count _session-id)
  "Render FRAME (alist) as display text.
_INDEX, _COUNT and _SESSION-ID are accepted for callers that track
position; the header line (set by `turn-stepper--display-current')
carries that information."
  (let* ((turn (turn-stepper--aget (quote turn) frame))
         (parse (turn-stepper--aget (quote parse) frame))
         (patterns (turn-stepper--aget (quote patterns) frame))
         (happened (turn-stepper--aget (quote happened) frame)))
    (concat
     (propertize (format "SAID — %s\n" (or (turn-stepper--aget (quote at) turn) "?"))
                 'turn-stepper-heading t
                 'face '(:weight bold))
     (format "%s\n\n" (or (turn-stepper--aget (quote text) turn) "(no text)"))
     (propertize "PARSE\n" 'face '(:weight bold))
     (turn-stepper--render-parse parse)
     "\n\n"
     (propertize "PATTERNS\n" 'face '(:weight bold))
     (turn-stepper--render-patterns patterns)
     "\n"
     (propertize "HAPPENED\n" 'face '(:weight bold))
     (if happened
         (mapconcat #'turn-stepper--render-happened-row happened "\n")
       "  (nothing between this turn and the next)")
     "\n")))

;;; ---------------------------------------------------------------- mode

(defvar turn-stepper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'turn-stepper-next)
    (define-key map (kbd "p") #'turn-stepper-previous)
    (define-key map (kbd "g") #'turn-stepper-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'turn-stepper-visit-turn)
    map)
  "Keymap for `turn-stepper-mode'.")

(define-derived-mode turn-stepper-mode special-mode "象-Stepper"
  "Major mode for the operator-turn frame stepper."
  (setq truncate-lines t))

(defun turn-stepper--display-current ()
  "Render the current frame into the stepper buffer."
  (let ((inhibit-read-only t)
        (count (length turn-stepper--frames)))
    (setq turn-stepper--index (turn-stepper--clamp-index
                               turn-stepper--index count))
    (erase-buffer)
    (if (zerop count)
        (insert "No operator-turn frames for this session.\n")
      (insert (turn-stepper--render-frame
               (nth turn-stepper--index turn-stepper--frames)
               turn-stepper--index count turn-stepper--session-id)))
    (goto-char (point-min))
    (setq header-line-format
          (format "frame %d / %d · session %s"
                  (if (zerop count) 0 (1+ turn-stepper--index))
                  count
                  (turn-stepper--short-id turn-stepper--session-id)))))

(defun turn-stepper-next ()
  "Show the next frame."
  (interactive)
  (setq turn-stepper--index
        (turn-stepper--clamp-index (1+ turn-stepper--index)
                                   (length turn-stepper--frames)))
  (turn-stepper--display-current))

(defun turn-stepper-previous ()
  "Show the previous frame."
  (interactive)
  (setq turn-stepper--index
        (turn-stepper--clamp-index (1- turn-stepper--index)
                                   (length turn-stepper--frames)))
  (turn-stepper--display-current))

(defvar agent-chat-user-speaker)

(defun turn-stepper--operator-prefix ()
  "How an operator turn's first line begins in a REPL buffer."
  (concat (if (and (boundp 'agent-chat-user-speaker) (stringp agent-chat-user-speaker))
              agent-chat-user-speaker "joe")
          ": "))

(defun turn-stepper--goto-turn-in-buffer (turn-text buffer)
  "Scroll BUFFER to the last occurrence of (the first ~60 chars of) TURN-TEXT.
Return non-nil when found; when not found, leave BUFFER's point unchanged
and return nil."
  (when (and (buffer-live-p buffer) (stringp turn-text)
             (not (string-empty-p turn-text)))
    (let ((needle (substring turn-text 0 (min 60 (length turn-text))))
          (prefix (turn-stepper--operator-prefix)))
      (with-current-buffer buffer
        (let ((old (point)))
          (goto-char (point-max))
          ;; Only an occurrence on the operator's own line counts: the same
          ;; words quoted later in an agent reply or a resume payload would
          ;; otherwise win, being nearer the end.
          (if (let (found)
                (while (and (not found) (search-backward needle nil t))
                  (when (save-excursion
                          (let ((bol (line-beginning-position)))
                            (and (string-prefix-p prefix
                                                  (buffer-substring-no-properties
                                                   bol (min (point-max) (+ bol (length prefix)))))
                                 (<= (- (point) bol) (+ (length prefix) 12)))))
                    (setq found t)))
                found)
              (let ((pos (point))
                    (win (get-buffer-window buffer t)))
                (when win (set-window-point win pos))
                pos)
            (goto-char old)
            nil))))))

(defun turn-stepper-visit-turn ()
  "On the SAID heading, scroll the source REPL window to this turn.
If the turn text is not found in the REPL buffer, say so and do not move."
  (interactive)
  (unless (get-text-property (line-beginning-position) 'turn-stepper-heading)
    (user-error "RET works on the SAID heading line"))
  (let* ((frame (nth turn-stepper--index turn-stepper--frames))
         (turn (turn-stepper--aget (quote turn) frame))
         (text (turn-stepper--aget (quote text) turn))
         (source turn-stepper--source-buffer))
    (if (and source (turn-stepper--goto-turn-in-buffer text source))
        (message "Scrolled REPL to turn at %s" (turn-stepper--aget (quote at) turn))
      (message "Turn text not found in the REPL buffer"))))

;;; ---------------------------------------------------------------- fetching

(defun turn-stepper--loading-buffer (session-id source)
  "Display a stepper buffer showing a loading message for SESSION-ID."
  (let ((buf (get-buffer-create turn-stepper-buffer-name)))
    (with-current-buffer buf
      (turn-stepper-mode)
      (setq turn-stepper--session-id session-id
            turn-stepper--source-buffer source
            turn-stepper--frames nil
            turn-stepper--index 0)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "loading frames for session %s …\n"
                        (turn-stepper--short-id session-id))))
      (setq header-line-format
            (format "loading… · session %s"
                    (turn-stepper--short-id session-id))))
    (turn-stepper--show-window buf)
    buf))

(defun turn-stepper--show-window (buffer)
  "Display BUFFER in a side window, stack-hud style."
  (display-buffer
   buffer
   `(display-buffer-in-side-window
     (side . ,turn-stepper-window-side)
     (window-width . ,turn-stepper-window-width)
     (slot . 0))))

(defun turn-stepper--start-fetch (session-id source)
  "Run turn_frames.py asynchronously for SESSION-ID.
On completion, cache the frames and display the most recent frame.
SOURCE is the REPL buffer the stepper is attached to."
  (turn-stepper--loading-buffer session-id source)
  (let* ((output (generate-new-buffer " *turn-stepper-output*"))
         (proc
          (make-process
           :name "turn-stepper-frames"
           :buffer output
           :command (list turn-stepper-python turn-stepper-script session-id)
           :noquery t
           ;; Keep stderr (warnings) out of the JSON on stdout.
           :stderr (get-buffer-create " *turn-stepper-frames-stderr*")
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (if (zerop (process-exit-status proc))
                       (let ((frames
                              (with-current-buffer (process-buffer proc)
                                (turn-stepper--parse-frames (buffer-string)))))
                         (puthash session-id frames turn-stepper--cache)
                         (turn-stepper--open session-id source
                                             (1- (length frames))))
                     (message "turn-stepper: turn_frames.py failed: %s"
                              (with-current-buffer (process-buffer proc)
                                (string-trim (buffer-string)))))
                 (when (buffer-live-p output)
                   (kill-buffer output))))))))
    proc))

(defun turn-stepper--open (session-id source &optional index)
  "Open (or reuse) the stepper buffer on frame INDEX for SESSION-ID."
  (let ((frames (gethash session-id turn-stepper--cache))
        (buf (get-buffer-create turn-stepper-buffer-name)))
    (with-current-buffer buf
      (turn-stepper-mode)
      (setq turn-stepper--session-id session-id
            turn-stepper--source-buffer source
            turn-stepper--frames frames
            turn-stepper--index (turn-stepper--clamp-index
                                 (or index 0) (length frames)))
      (turn-stepper--display-current))
    (turn-stepper--show-window buf)
    buf))

(defun turn-stepper-refresh ()
  "Re-run turn_frames.py for this stepper's session (asynchronously)."
  (interactive)
  (unless turn-stepper--session-id
    (user-error "No session attached to this stepper"))
  (turn-stepper--start-fetch turn-stepper--session-id
                             turn-stepper--source-buffer))

;;; ---------------------------------------------------------------- entry

;;;###autoload
(defun turn-stepper ()
  "Open the operator-turn frame stepper beside the current REPL buffer.
The session id is taken from the buffer-local `agent-chat--session-id'.
Frames are cached per session; use `g' in the stepper to refresh."
  (interactive)
  (let* ((source (current-buffer))
         (session-id (turn-stepper--session-id-in-buffer source)))
    (unless session-id
      (user-error
       "No session id here (agent-chat--session-id unset); not a REPL buffer?"))
    (if (gethash session-id turn-stepper--cache)
        (turn-stepper--open session-id source
                            (1- (length (gethash session-id
                                                 turn-stepper--cache))))
      (turn-stepper--start-fetch session-id source))))

(provide 'turn-stepper)
;;; turn-stepper.el ends here
