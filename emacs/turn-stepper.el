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

(defvar turn-stepper--reload-failed nil
  "Non-nil while the stepper reopens its old frames after a failed reload.")

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

(defun turn-stepper--ready-frames (frames)
  "FRAMES without the newest turns 象 has not read yet.
Those turns are still in progress (no reading, no reply), so they are
left out until a refresh finds them read.  An older turn 象 never read
stays, shown as missing."
  (let ((rev (reverse frames)))
    (while (and rev (not (equal "analyzed"
                                (turn-stepper--aget
                                 (quote status) (turn-stepper--aget (quote parse) (car rev))))))
      (setq rev (cdr rev)))
    (nreverse rev)))

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

(defun turn-stepper--render-operators (operators)
  "Render IBOL operator words and how they meet 象's marks.
A word inside one of 象's marks is the intersection: the operational
content of that mark.  The marks themselves are underlined in PARSE."
  (let ((hits (turn-stepper--aget (quote hits) operators)))
    (concat
     (if (null hits)
         "  (no operator words)\n"
       (mapconcat
        (lambda (h)
          (let ((cue (turn-stepper--aget (quote cue_intent) h)))
            (format "  %-12s %-18s %s\n"
                    (turn-stepper--aget (quote ibol) h)
                    (format "\"%s\"" (turn-stepper--aget (quote text) h))
                    (cond ((null cue) "outside 象's marks")
                          ((eq (turn-stepper--aget (quote agree) h) t)
                           (format "in 象 %s \"%s\" (agrees)" cue
                                   (turn-stepper--aget (quote cue_text) h)))
                          (t (format "in 象 %s \"%s\"; chip says %s" cue
                                     (turn-stepper--aget (quote cue_text) h)
                                     (turn-stepper--aget (quote chip_intent) h)))))))
        hits "")))))

(defun turn-stepper--underline-cues (text cues)
  "Return TEXT with each of 象's CUES (strings) underlined where it occurs."
  (let ((out (copy-sequence text)) (case-fold-search nil))
    (dolist (cue cues out)
      (when (and (stringp cue) (not (string-empty-p cue)))
        (let ((i (string-search cue out)))
          (when i
            (add-face-text-property i (+ i (length cue))
                                    '(:underline (:style wave :color "purple")) nil out)))))))

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
                     (turn-stepper--underline-cues
                      (or (turn-stepper--aget (quote text) frag) "")
                      (turn-stepper--aget (quote cues) frag))
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
     (propertize "OPERATORS\n" 'face '(:weight bold))
     (turn-stepper--render-operators (turn-stepper--aget (quote operators) frame))
     "\n"
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
    (define-key map (kbd "r") #'turn-stepper-rewind)
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
    (turn-stepper--show-window buf source)
    buf))

(defun turn-stepper--show-window (buffer &optional source)
  "Show BUFFER in one window only, split off beside SOURCE's window.
The stepper belongs to the REPL it reads, not to the frame: a window
showing it anywhere else (another frame, a frame side window) is closed."
  (let* ((src-wins (and (buffer-live-p source)
                        (get-buffer-window-list source nil 'visible)))
         ;; A REPL shown in several frames: stay beside the copy that
         ;; already has the stepper, else use the selected frame's.
         (src-win (or (seq-find (lambda (w)
                                  (let ((n (window-in-direction turn-stepper-window-side w)))
                                    (and n (eq (window-buffer n) buffer))))
                                src-wins)
                      (and (buffer-live-p source) (get-buffer-window source))
                      (car src-wins)))
         (anchor (or src-win (selected-window)))
         (keep (window-in-direction turn-stepper-window-side anchor)))
    (unless (and keep (eq (window-buffer keep) buffer)
                 (not (window-parameter keep 'window-side)))
      (setq keep nil))
    (dolist (w (get-buffer-window-list buffer nil t))
      (unless (eq w keep)
        (condition-case nil
            (delete-window w)
          ;; The only window of its frame: show something else there.
          (error (with-selected-window w (switch-to-prev-buffer w 'kill))))))
    ;; The action function is called directly, not through `display-buffer':
    ;; when the split fails, display-buffer falls back to taking over some
    ;; other window (another REPL), which is worse than not showing at all.
    (or keep
        (display-buffer-in-direction
         buffer `((window . ,anchor) (direction . ,turn-stepper-window-side)
                  (window-width . ,turn-stepper-window-width)))
        (display-buffer-in-direction
         buffer `((window . ,anchor) (direction . below)))
        (progn (message "turn-stepper: no room beside %s; not shown"
                        (if (buffer-live-p source) (buffer-name source) "the REPL"))
               nil))))

(defcustom turn-stepper-reload-retries 3
  "Times a failed reload is retried (futon1b busy is usually brief)."
  :type 'integer :group 'turn-stepper)

(defcustom turn-stepper-reload-retry-after 60
  "Seconds to wait before retrying a failed reload."
  :type 'integer :group 'turn-stepper)

(defun turn-stepper--retry-fetch (session-id source tries)
  "Retry a failed reload of SESSION-ID if its stepper is still on screen."
  (let ((buf (get-buffer turn-stepper-buffer-name)))
    (when (and buf (get-buffer-window buf t)
               (equal session-id (buffer-local-value 'turn-stepper--session-id buf))
               (not (get-process "turn-stepper-frames")))
      (turn-stepper--start-fetch session-id source t tries))))

(defun turn-stepper--start-fetch (session-id source &optional quiet tries)
  "Run turn_frames.py asynchronously for SESSION-ID.
On completion, cache the frames and display the most recent frame, unless
the stepper was on an older frame of this session: then it stays there.
SOURCE is the REPL buffer the stepper is attached to.  QUIET keeps the
current frame on screen instead of a loading message.  TRIES is how many
more times a failure is retried (default `turn-stepper-reload-retries')."
  (unless quiet
    (turn-stepper--loading-buffer session-id source))
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
                                (turn-stepper--ready-frames
                                 (turn-stepper--parse-frames (buffer-string))))))
                         (puthash session-id frames turn-stepper--cache)
                         (turn-stepper--open session-id source
                                             (turn-stepper--index-after-reload
                                              session-id (length frames))
                                             quiet))
                     (progn
                       (let ((left (if tries tries turn-stepper-reload-retries)))
                         (message "turn-stepper: reload failed (%s); frames kept%s"
                                  (turn-stepper--last-error-line)
                                  (if (> left 0)
                                      (format ", retrying in %ds (%d left)"
                                              turn-stepper-reload-retry-after left)
                                    ", no retries left"))
                         (when (> left 0)
                           (run-at-time turn-stepper-reload-retry-after nil
                                        #'turn-stepper--retry-fetch
                                        session-id source (1- left))))
                       ;; Put back what was showing; a busy evidence store
                       ;; must not cost the frames already read.
                       (when (gethash session-id turn-stepper--cache)
                         (let ((turn-stepper--reload-failed t))
                          (turn-stepper--open session-id source
                                             (turn-stepper--index-after-reload
                                              session-id (length (gethash session-id turn-stepper--cache)))
                                             t)))))
                 (when (buffer-live-p output)
                   (kill-buffer output))))))))
    proc))

(defun turn-stepper--index-after-reload (session-id count)
  "Frame to show after a reload giving COUNT frames for SESSION-ID.
The newest frame, unless the stepper is on an older frame of the same
session: someone reading back stays where they are."
  (let ((buf (get-buffer turn-stepper-buffer-name)))
    (or (and buf
             (with-current-buffer buf
               (and (equal turn-stepper--session-id session-id)
                    turn-stepper--frames
                    (< turn-stepper--index (1- (length turn-stepper--frames)))
                    turn-stepper--index)))
        (1- count))))

(defun turn-stepper--reading-landed (path)
  "A 象 reading for the record at PATH has arrived: reload a stepper on it.
Only a stepper that is on screen and reads PATH's session is reloaded;
a closed stepper stays closed."
  (let ((buf (get-buffer turn-stepper-buffer-name)))
    (when (and buf (get-buffer-window buf t))
      (let ((session (ignore-errors
                       (alist-get 'session_id
                                  (json-read-file path)))))
        (with-current-buffer buf
          (when (and session (equal session turn-stepper--session-id)
                     (not (get-process "turn-stepper-frames")))
            (turn-stepper--start-fetch session turn-stepper--source-buffer t)))))))

(add-hook 'session-mode-analysis-landed-functions #'turn-stepper--reading-landed)

(defun turn-stepper--last-error-line ()
  "Last non-empty line the frames script wrote to stderr."
  (let ((b (get-buffer " *turn-stepper-frames-stderr*")))
    (or (and b (with-current-buffer b
                 (car (last (seq-remove
                             (lambda (l) (or (string-empty-p (string-trim l))
                                             (string-prefix-p "Process " l)))
                             (split-string (buffer-string) "\n"))))))
        "no error output")))

(defun turn-stepper--open (session-id source &optional index quiet)
  "Open (or reuse) the stepper buffer on frame INDEX for SESSION-ID.
QUIET re-renders without touching windows (a reload of a visible stepper)."
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
    (unless (and quiet (get-buffer-window buf t))
      (turn-stepper--show-window buf source))
    buf))

(defun turn-stepper-refresh ()
  "Re-run turn_frames.py for this stepper's session (asynchronously)."
  (interactive)
  (unless turn-stepper--session-id
    (user-error "No session attached to this stepper"))
  ;; Keep the current frame on screen while reloading.
  (turn-stepper--start-fetch turn-stepper--session-id
                             turn-stepper--source-buffer
                             (and turn-stepper--frames t)))

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

;;; ---------------------------------------------------------------- rewind
;; Rewinding a turn, first as a view.  Each repo's state at the start of a
;; turn is its last commit before the turn's time (on the first-parent line
;; of HEAD), so the pin is derived from git's own history rather than
;; recorded; what the turn changed is pin..(last commit before the next
;; turn).  Commits by other seats in the same window are included, because
;; the repos are shared -- the view says so rather than pretending otherwise.
;; Uncommitted edits are not covered.

(defcustom turn-stepper-code-root "~/code/"
  "Directory holding the repos named in turn-commits rows."
  :type 'directory :group 'turn-stepper)

(defcustom turn-stepper-rewind-dir "/tmp/xiang-rewind/"
  "Where read-only worktrees at a pin are created."
  :type 'directory :group 'turn-stepper)

(defun turn-stepper--git (repo &rest args)
  "Run git ARGS in REPO; trimmed stdout, or nil on failure."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory repo)))
      (when (and (file-directory-p repo)
                 (zerop (apply #'call-process "git" nil '(t nil) nil args)))
        (string-trim (buffer-string))))))

(defun turn-stepper--last-commit-before (repo at)
  "Last first-parent commit of HEAD in REPO committed before AT, or nil."
  (let ((sha (turn-stepper--git repo "rev-list" "-1" "--first-parent"
                                (concat "--before=" at) "HEAD")))
    (and sha (not (string-empty-p sha)) sha)))

(defun turn-stepper--frame-repos (frame)
  "Repo names with commits recorded in FRAME's turn-commits rows."
  (let (repos)
    (dolist (h (turn-stepper--aget 'happened frame))
      (let ((s (turn-stepper--aget 'summary h)))
        (when (equal (turn-stepper--aget 'event s) "turn-commits")
          (dolist (c (turn-stepper--aget 'commits s))
            (let ((r (turn-stepper--aget 'repo c)))
              (when r (cl-pushnew r repos :test #'equal)))))))
    (nreverse repos)))

(defun turn-stepper--rewind-plan (frame next-at)
  "Per repo: the pin at FRAME's start and the commits up to NEXT-AT.
Returns a list of plists (:repo :path :pin :end :commits), commits as
\"SHA<TAB>AUTHOR<TAB>AGENT-SESSION<TAB>SUBJECT\" lines oldest first; the
session is the commit's Agent-Session trailer, empty when unsigned."
  (let ((at (turn-stepper--aget 'at (turn-stepper--aget 'turn frame))))
    (delq nil
          (mapcar
           (lambda (repo)
             (let* ((path (expand-file-name repo turn-stepper-code-root))
                    (pin (turn-stepper--last-commit-before path at))
                    (end (if next-at (turn-stepper--last-commit-before path next-at)
                           (turn-stepper--git path "rev-parse" "HEAD"))))
               (when (and pin end)
                 (list :repo repo :path path :pin pin :end end
                       :commits (and (not (equal pin end))
                                     (split-string
                                      (or (turn-stepper--git path "log" "--reverse" "--first-parent"
                                                             "--format=%H%x09%an%x09%(trailers:key=Agent-Session,valueonly,separator=%x2C)%x09%s"
                                                             (concat pin ".." end))
                                          "")
                                      "\n" t))))))
           (turn-stepper--frame-repos frame)))))

(defvar-local turn-stepper--rewind-plan nil)
(defvar-local turn-stepper--rewind-session nil)

(defun turn-stepper--own-commits (plan-entry session)
  "Full shas in PLAN-ENTRY signed by SESSION, newest first."
  (let (out)
    (dolist (c (plist-get plan-entry :commits) out)
      (pcase-let ((`(,sha ,_ ,sess . ,_) (split-string c "\t")))
        (when (and session (equal sess session)) (push sha out))))))

(defun turn-stepper--revert (plan-entry session)
  "Revert PLAN-ENTRY's commits signed by SESSION as new commits.
Returns (:repo R :reverted N) or (:repo R :refused WHY)."
  (let* ((path (plist-get plan-entry :path))
         (repo (plist-get plan-entry :repo))
         (shas (turn-stepper--own-commits plan-entry session)))
    (cond
     ((null shas) (list :repo repo :refused "no commits signed by this session"))
     ((not (string-empty-p (or (turn-stepper--git path "status" "--porcelain"
                                                  "--untracked-files=no")
                               "?")))
      (list :repo repo :refused "uncommitted edits in the checkout"))
     ((not (apply #'turn-stepper--git path "revert" "--no-edit" shas))
      (turn-stepper--git path "revert" "--abort")
      (list :repo repo :refused "revert conflicted; aborted, nothing changed"))
     (t (list :repo repo :reverted (length shas))))))

(defun turn-stepper-rewind-apply ()
  "Revert this session's commits in each repo of the rewind view."
  (interactive)
  (let* ((plan turn-stepper--rewind-plan)
         (session turn-stepper--rewind-session)
         (todo (cl-remove-if-not (lambda (p) (turn-stepper--own-commits p session)) plan)))
    (if (null todo)
        (message "Nothing to revert: no commit in this window is signed by this session")
      (when (yes-or-no-p
             (format "Revert %s? "
                     (mapconcat (lambda (p) (format "%d commit(s) in %s"
                                                    (length (turn-stepper--own-commits p session))
                                                    (plist-get p :repo)))
                                todo ", ")))
        (message "%s"
                 (mapconcat (lambda (p)
                              (let ((r (turn-stepper--revert p session)))
                                (if (plist-get r :reverted)
                                    (format "%s: reverted %d" (plist-get r :repo)
                                            (plist-get r :reverted))
                                  (format "%s: refused (%s)" (plist-get r :repo)
                                          (plist-get r :refused)))))
                            todo "; "))))))

(defun turn-stepper-rewind ()
  "Show what rewinding to the start of the current frame's turn would undo."
  (interactive)
  (let* ((frame (nth turn-stepper--index turn-stepper--frames))
         (next (nth (1+ turn-stepper--index) turn-stepper--frames))
         (next-at (and next (turn-stepper--aget 'at (turn-stepper--aget 'turn next))))
         (at (turn-stepper--aget 'at (turn-stepper--aget 'turn frame)))
         (plan (turn-stepper--rewind-plan frame next-at))
         (session turn-stepper--session-id)
         (buf (get-buffer-create "*象 rewind*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Rewind to the start of the turn at %s\n" at))
        (insert (format "Window: %s → %s\n\n" at (or next-at "now")))
        (if (null plan)
            (insert "No commits in this turn's window: nothing to rewind in git.\n")
          (dolist (p plan)
            (insert (propertize (format "%s  pin %s\n" (plist-get p :repo)
                                        (substring (plist-get p :pin) 0 12))
                                'turn-stepper-rewind p 'face 'bold))
            (dolist (c (plist-get p :commits))
              (pcase-let ((`(,sha ,author ,sess ,subject) (split-string c "\t")))
                (insert (propertize
                         (format "  %-10s %s  %s  %s\n"
                                 (cond ((equal sess session) "R undoes")
                                       ((string-empty-p (or sess "")) "unsigned")
                                       (t "other seat"))
                                 (substring sha 0 8) author subject)
                         'turn-stepper-rewind p))))
            (insert "\n")))
        (insert "Every commit in the window is listed: the repos are shared.  R reverts\n"
                "only commits signed by this session (Agent-Session trailer), newest\n"
                "first, as new commits; other seats' and unsigned commits are left alone.\n"
                "Uncommitted edits are not covered.\n"
                "w = read-only worktree at the pin, d = diff pin..end (on a repo's lines),\n"
                "R = revert this session's commits in every repo listed, q = quit\n"))
      (turn-stepper-rewind-mode)
      (setq turn-stepper--rewind-plan plan
            turn-stepper--rewind-session session)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun turn-stepper--rewind-at-point ()
  (or (get-text-property (point) 'turn-stepper-rewind)
      (user-error "Not on a repo's lines")))

(defun turn-stepper-rewind-worktree ()
  "Open a detached, read-only worktree of the repo at point, at its pin."
  (interactive)
  (let* ((p (turn-stepper--rewind-at-point))
         (dir (expand-file-name (format "%s-%s" (plist-get p :repo)
                                        (substring (plist-get p :pin) 0 12))
                                turn-stepper-rewind-dir)))
    (unless (file-directory-p dir)
      (make-directory turn-stepper-rewind-dir t)
      (unless (turn-stepper--git (plist-get p :path) "worktree" "add" "--detach"
                                 dir (plist-get p :pin))
        (user-error "git worktree add failed for %s" (plist-get p :repo))))
    (let ((b (dired dir)))
      (with-current-buffer b (setq buffer-read-only t))
      b)))

(defun turn-stepper-rewind-diff ()
  "Show pin..end for the repo at point: everything the rewind would undo."
  (interactive)
  (let* ((p (turn-stepper--rewind-at-point))
         (out (turn-stepper--git (plist-get p :path) "diff" "--stat" "-p"
                                 (plist-get p :pin) (plist-get p :end)))
         (b (get-buffer-create (format "*象 rewind diff %s*" (plist-get p :repo)))))
    (with-current-buffer b
      (let ((inhibit-read-only t)) (erase-buffer) (insert (or out "")))
      (diff-mode) (setq buffer-read-only t) (goto-char (point-min)))
    (pop-to-buffer b)))

(defvar turn-stepper-rewind-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") #'turn-stepper-rewind-worktree)
    (define-key map (kbd "d") #'turn-stepper-rewind-diff)
    (define-key map (kbd "R") #'turn-stepper-rewind-apply)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode turn-stepper-rewind-mode special-mode "象-rewind"
  "What rewinding a turn would undo, per repo.")

(provide 'turn-stepper)
;;; turn-stepper.el ends here
