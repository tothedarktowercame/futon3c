;;; futon-agent-cursor.el --- The agent's visible typing body -*- lexical-binding: t; -*-

;; M-smart-emacs-cursor A4/A1′: two minds in one editor need two visible
;; bodies. This is the agent's: a marker-anchored ◆ cursor with an identity
;; badge, and a typing primitive that inserts text AT that cursor,
;; character-chunk by character-chunk, on a timer — so authorship is
;; something the operator SEES at the moment of change. The operator's
;; point is never moved, never borrowed (edits at a marker adjust window
;; points by ordinary Emacs semantics only).
;;
;; v1 is deliberately small and driven from outside (A3: the plain door):
;;   (futon-agent-cursor-goto BUFFER POS [CAPTION])
;;   (futon-agent-cursor-type TEXT [ON-DONE])
;;   (futon-agent-cursor-dismiss)
;; Integration with smart-cursor.el's WS-fed companion cursor is v2.

;;; Code:

(require 'cl-lib)

(defgroup futon-agent-cursor nil
  "A visible, identity-bearing cursor for the agent."
  :group 'applications)

(defcustom futon-agent-cursor-id "fable-2"
  "Identity shown on the agent cursor's badge."
  :type 'string
  :group 'futon-agent-cursor)

(defcustom futon-agent-cursor-chunk 3
  "Characters inserted per typing tick."
  :type 'integer
  :group 'futon-agent-cursor)

(defcustom futon-agent-cursor-interval 0.04
  "Seconds between typing ticks."
  :type 'number
  :group 'futon-agent-cursor)

(defface futon-agent-cursor-glyph-face
  '((t :foreground "orange" :weight bold))
  "Face for the agent cursor glyph.")

(defface futon-agent-cursor-badge-face
  '((t :foreground "black" :background "orange" :weight bold :height 0.85))
  "Face for the agent cursor identity badge.")

(defvar futon-agent-cursor--marker nil)
(defvar futon-agent-cursor--overlay nil)
(defvar futon-agent-cursor--timer nil)
(defvar futon-agent-cursor--pending nil)
(defvar futon-agent-cursor--on-done nil)
(defvar futon-agent-cursor--typing nil
  "Non-nil while a typing run is in progress.")

(defun futon-agent-cursor--badge (caption)
  (concat
   (propertize "◆" 'face 'futon-agent-cursor-glyph-face)
   (propertize (format " %s%s " futon-agent-cursor-id
                       (if caption (concat ": " caption) ""))
               'face 'futon-agent-cursor-badge-face)))

(defun futon-agent-cursor-goto (buffer pos &optional caption take-operator)
  "Place the agent cursor in BUFFER at POS, showing CAPTION on the badge.

With TAKE-OPERATOR non-nil, also move the window's point so the view
genuinely travels (A4 nuance, spoken 2026-06-11: redisplay anchors a
window to its point, so a view cannot move while the point stays behind —
and moving the operator ON THEIR EXPLICIT REQUEST is service, not
impersonation; the forbidden thing is doing it unasked)."
  (with-current-buffer buffer
    (futon-agent-cursor-dismiss)
    (setq futon-agent-cursor--marker (copy-marker pos t))
    (setq futon-agent-cursor--overlay (make-overlay pos pos buffer nil t))
    (overlay-put futon-agent-cursor--overlay 'futon-agent-cursor t)
    (overlay-put futon-agent-cursor--overlay 'before-string
                 (futon-agent-cursor--badge caption))
    ;; Search every frame for a window showing BUFFER (the daemon's notion
    ;; of "selected frame" is not the operator's).
    (when-let ((win (get-buffer-window buffer t)))
      (if take-operator
          (progn
            (set-window-point win pos)
            (with-selected-window win (recenter 8)))
        (unless (pos-visible-in-window-p pos win)
          (set-window-start win (max (point-min)
                                     (save-excursion (goto-char pos)
                                                     (forward-line -8)
                                                     (point)))))))
    pos))

(defun futon-agent-cursor--tick ()
  (if (or (null futon-agent-cursor--pending)
          (string-empty-p futon-agent-cursor--pending)
          (null futon-agent-cursor--marker))
      (futon-agent-cursor--finish)
    (let* ((n (min futon-agent-cursor-chunk (length futon-agent-cursor--pending)))
           (chunk (substring futon-agent-cursor--pending 0 n)))
      (setq futon-agent-cursor--pending (substring futon-agent-cursor--pending n))
      (with-current-buffer (marker-buffer futon-agent-cursor--marker)
        (save-excursion
          (goto-char futon-agent-cursor--marker)
          (insert chunk))
        ;; insertion-type t marker advances past the insert; keep the
        ;; glyph overlay riding at the marker.
        (move-overlay futon-agent-cursor--overlay
                      (marker-position futon-agent-cursor--marker)
                      (marker-position futon-agent-cursor--marker))))))

(defun futon-agent-cursor--finish ()
  (when futon-agent-cursor--timer
    (cancel-timer futon-agent-cursor--timer)
    (setq futon-agent-cursor--timer nil))
  (setq futon-agent-cursor--typing nil)
  (when futon-agent-cursor--overlay
    (overlay-put futon-agent-cursor--overlay 'before-string
                 (futon-agent-cursor--badge "done")))
  (when futon-agent-cursor--on-done
    (let ((fn futon-agent-cursor--on-done))
      (setq futon-agent-cursor--on-done nil)
      (funcall fn))))

(defun futon-agent-cursor-type (text &optional on-done)
  "Type TEXT at the agent cursor, visibly, on a timer.
Call ON-DONE (no args) when the run completes. Returns immediately."
  (unless (and futon-agent-cursor--marker
               (marker-buffer futon-agent-cursor--marker))
    (user-error "Agent cursor is not placed; call futon-agent-cursor-goto first"))
  (when futon-agent-cursor--timer (cancel-timer futon-agent-cursor--timer))
  (setq futon-agent-cursor--pending text
        futon-agent-cursor--on-done on-done
        futon-agent-cursor--typing t)
  (overlay-put futon-agent-cursor--overlay 'before-string
               (futon-agent-cursor--badge "typing…"))
  (setq futon-agent-cursor--timer
        (run-at-time 0 futon-agent-cursor-interval #'futon-agent-cursor--tick))
  t)

(defvar futon-agent-cursor--delete-remaining 0)

(defun futon-agent-cursor--delete-tick ()
  (if (or (<= futon-agent-cursor--delete-remaining 0)
          (null futon-agent-cursor--marker))
      (futon-agent-cursor--finish)
    (let ((n (min futon-agent-cursor-chunk futon-agent-cursor--delete-remaining)))
      (setq futon-agent-cursor--delete-remaining
            (- futon-agent-cursor--delete-remaining n))
      (with-current-buffer (marker-buffer futon-agent-cursor--marker)
        (let ((pos (marker-position futon-agent-cursor--marker)))
          (delete-region pos (min (point-max) (+ pos n)))
          (move-overlay futon-agent-cursor--overlay pos pos))))))

(defun futon-agent-cursor-delete (n &optional on-done)
  "Visibly delete N characters forward from the agent cursor.
The revision primitive (VERIFY row 8): same identity rules as typing —
the badge shows the act, the operator's point is never touched.
Call ON-DONE when the run completes. Returns immediately."
  (unless (and futon-agent-cursor--marker
               (marker-buffer futon-agent-cursor--marker))
    (user-error "Agent cursor is not placed; call futon-agent-cursor-goto first"))
  (when futon-agent-cursor--timer (cancel-timer futon-agent-cursor--timer))
  (setq futon-agent-cursor--delete-remaining n
        futon-agent-cursor--on-done on-done
        futon-agent-cursor--typing t)
  (overlay-put futon-agent-cursor--overlay 'before-string
               (futon-agent-cursor--badge "revising…"))
  (setq futon-agent-cursor--timer
        (run-at-time 0 futon-agent-cursor-interval #'futon-agent-cursor--delete-tick))
  t)

(defun futon-agent-cursor-dismiss ()
  "Remove the agent cursor and stop any typing run."
  (interactive)
  (when futon-agent-cursor--timer
    (cancel-timer futon-agent-cursor--timer)
    (setq futon-agent-cursor--timer nil))
  (when futon-agent-cursor--overlay
    (delete-overlay futon-agent-cursor--overlay)
    (setq futon-agent-cursor--overlay nil))
  (when futon-agent-cursor--marker
    (set-marker futon-agent-cursor--marker nil)
    (setq futon-agent-cursor--marker nil))
  (setq futon-agent-cursor--pending nil
        futon-agent-cursor--typing nil
        futon-agent-cursor--on-done nil))

(provide 'futon-agent-cursor)
;;; futon-agent-cursor.el ends here

;;;; Come-and-look: the operator summons the agent's attention -----------

(defvar futon-look-inbox "/tmp/futon-voice-inbox.jsonl"
  "Where look events land — the same inbox the agent already watches.")

(defun futon-look (&optional note)
  "Summon the agent to look at what the operator sees, here.
Captures the active region (or the paragraph at point), file, and line
into the agent's inbox, and places the agent cursor at point — the
summons IS the consent (M-smart-emacs-cursor A4). With prefix arg,
prompt for a NOTE to ride along."
  (interactive (list (when current-prefix-arg (read-string "Note for the agent: "))))
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (save-excursion
                   (buffer-substring-no-properties
                    (progn (backward-paragraph) (point))
                    (progn (forward-paragraph) (point))))))
         (payload `((at . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))
                    (kind . "look")
                    (file . ,(or (buffer-file-name) (buffer-name)))
                    (line . ,(line-number-at-pos))
                    (note . ,(or note ""))
                    (text . ,(string-trim text)))))
    (with-temp-buffer
      (insert (json-encode payload) "\n")
      (append-to-file (point-min) (point-max) futon-look-inbox))
    (futon-agent-cursor-goto (current-buffer) (point) "looking with you")
    (message "futon-look: sent %d chars from %s:%d"
             (length text) (buffer-name) (line-number-at-pos))))
