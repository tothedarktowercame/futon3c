;;; agent-mission-control.el --- Mission-Control digest of agent buffers -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Single-frame digest showing the last few lines of every active
;;   agent dialogue buffer (claude-repl, codex-repl, *invoke:*).  Modeled on
;;   macOS Mission Control / Exposé: pop up, glance, dismiss.
;;
;; Entry points:
;;   M-x agent-mission-control          — raise the digest in the current frame
;;   M-x agent-mission-control-refresh  — re-render in place (also bound to `g')
;;   M-x agent-mission-control-quit     — dismiss frame (also bound to `q')
;;
;; Sway integration: scripts/agent-mission-control-terminal launches a
;;   floating, scratchpad-managed emacsclient frame that auto-renders on
;;   creation (via `server-after-make-frame-hook').

;;; Code:

(require 'cl-lib)
(require 'server)
(require 'subr-x)

(defgroup agent-mission-control nil
  "Mission-Control-style digest of agent dialogue buffers."
  :group 'applications)

(defcustom agent-mission-control-frame-name "Agent Mission Control"
  "Frame name used for the dedicated digest frame."
  :type 'string :group 'agent-mission-control)

(defcustom agent-mission-control-buffer-name "*agent-mission-control*"
  "Buffer name for the digest."
  :type 'string :group 'agent-mission-control)

(defcustom agent-mission-control-tail-lines 5
  "Number of trailing non-empty lines to show per source buffer."
  :type 'integer :group 'agent-mission-control)

(defcustom agent-mission-control-line-max-width 200
  "Maximum displayed width of any single dialogue line.
Lines longer than this are truncated with an ellipsis.  truncate-lines
also applies in the digest buffer, so this is mainly a safety bound for
extremely long single-line outputs."
  :type 'integer :group 'agent-mission-control)

(defcustom agent-mission-control-buffer-patterns
  '("\\`\\*claude-repl\\(:[^*]*\\)?\\*\\'"
    "\\`\\*codex-repl\\(:[^*]*\\)?\\*\\'")
  "Regexps matched against buffer names to include in the digest.
Buffers matching any pattern, that are live and non-empty, are shown.
The default matches `*claude-repl*' / `*claude-repl:claude-N*' and
`*codex-repl*' / `*codex-repl:codex-N*'.  `*invoke:*' buffers are
excluded because they are debug projections, not dialogue."
  :type '(repeat regexp) :group 'agent-mission-control)

(defconst agent-mission-control--frame-parameter 'agent-mission-control
  "Frame parameter marking the dedicated MC frame.")

(defface agent-mission-control-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for buffer-name headers in the digest."
  :group 'agent-mission-control)

(defface agent-mission-control-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for meta info (timestamps, separators)."
  :group 'agent-mission-control)

(defvar agent-mission-control-mode-map (make-sparse-keymap)
  "Keymap for `agent-mission-control-mode' buffers.
Bindings are installed below via `define-key' so reloading this file
updates them in place rather than replacing the keymap object.")

(define-key agent-mission-control-mode-map (kbd "q")        #'agent-mission-control-quit)
(define-key agent-mission-control-mode-map (kbd "g")        #'agent-mission-control-refresh)
(define-key agent-mission-control-mode-map (kbd "r")        #'agent-mission-control-refresh)
(define-key agent-mission-control-mode-map (kbd "n")        #'agent-mission-control-next-section)
(define-key agent-mission-control-mode-map (kbd "p")        #'agent-mission-control-previous-section)
(define-key agent-mission-control-mode-map (kbd "<down>")   #'agent-mission-control-next-section)
(define-key agent-mission-control-mode-map (kbd "<up>")     #'agent-mission-control-previous-section)
(define-key agent-mission-control-mode-map (kbd "<tab>")    #'agent-mission-control-next-section)
(define-key agent-mission-control-mode-map (kbd "<backtab>") #'agent-mission-control-previous-section)
(define-key agent-mission-control-mode-map (kbd "RET")      #'agent-mission-control-visit-section)

(define-derived-mode agent-mission-control-mode special-mode "Agent-MC"
  "Major mode for the agent mission-control digest buffer."
  (setq-local truncate-lines t
              buffer-read-only t))

(defun agent-mission-control--matching-buffers ()
  "Return live, non-empty buffers whose names match the configured patterns.
Order follows `buffer-list' (most-recently-selected first)."
  (cl-remove-if-not
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (and name
            (> (buffer-size buf) 0)
            (cl-some (lambda (re) (string-match-p re name))
                     agent-mission-control-buffer-patterns))))
   (buffer-list)))

(defun agent-mission-control--tail-region (buf n)
  "Return the substring of BUF covering the last N non-empty lines.
Properties (faces) are preserved.  Returns the empty string when BUF
has no non-empty lines."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (let ((end (point))
            (start (point))
            (count 0))
        (while (and (> (point) (point-min)) (< count n))
          (forward-line -1)
          (let ((line-start (point))
                (line-end (line-end-position)))
            (unless (string-blank-p
                     (buffer-substring-no-properties line-start line-end))
              (cl-incf count))
            (setq start line-start)))
        (if (< start end)
            (buffer-substring start end)
          "")))))

(defun agent-mission-control--truncate-line (line)
  "Truncate LINE to `agent-mission-control-line-max-width' columns."
  (let ((max agent-mission-control-line-max-width))
    (if (and (numberp max) (> max 0) (> (length line) max))
        (concat (substring line 0 (max 0 (- max 1))) "…")
      line)))

(defun agent-mission-control--insert-tail (buf)
  "Insert the tail snippet of BUF at point, with per-line width truncation."
  (let* ((raw (agent-mission-control--tail-region
               buf agent-mission-control-tail-lines))
         (lines (split-string raw "\n")))
    ;; split-string with default separator preserves trailing empty string;
    ;; drop a single trailing empty so we don't add a blank line before
    ;; the section separator.
    (when (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (insert (agent-mission-control--truncate-line line))
      (insert "\n"))))

(defun agent-mission-control--buffer-mission-label (buf)
  "Return clocked-in mission label for BUF."
  (with-current-buffer buf
    (let ((mission (and (boundp 'agent-chat--mission-id)
                        agent-chat--mission-id)))
      (if (and (stringp mission) (not (string-empty-p mission)))
          mission
        "no mission"))))

(defun agent-mission-control--buffer-agent-label (buf)
  "Return agent label for BUF including clocked-in mission."
  (with-current-buffer buf
    (let ((agent (or (and (boundp 'agent-chat--agent-id)
                          agent-chat--agent-id)
                     (and (boundp 'agent-chat--agent-name)
                          agent-chat--agent-name)
                     (buffer-name buf))))
      (format "%s [mission:%s]"
              agent
              (agent-mission-control--buffer-mission-label buf)))))

(defun agent-mission-control--render ()
  "Erase and re-render the digest buffer.  Returns the buffer."
  (let* ((buf (get-buffer-create agent-mission-control-buffer-name))
         (sources (agent-mission-control--matching-buffers)))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-mission-control-mode)
        (agent-mission-control-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "Agent Mission Control — %d buffer%s — %s\n"
                         (length sources)
                         (if (= 1 (length sources)) "" "s")
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 'face 'agent-mission-control-meta-face))
        (insert (propertize
                 "(q dismiss · g refresh · last "
                 'face 'agent-mission-control-meta-face))
        (insert (propertize
                 (format "%d" agent-mission-control-tail-lines)
                 'face 'agent-mission-control-meta-face))
        (insert (propertize " lines per buffer)\n\n"
                            'face 'agent-mission-control-meta-face))
        (if (null sources)
            (insert (propertize
                     "No matching agent buffers found.\n"
                     'face 'agent-mission-control-meta-face))
          (dolist (src sources)
            (insert (propertize
                     (format "── %s — %s ──\n"
                             (buffer-name src)
                             (agent-mission-control--buffer-agent-label src))
                     'face 'agent-mission-control-header-face
                     'agent-mc-section (buffer-name src)))
            (agent-mission-control--insert-tail src)
            (insert "\n")))
        (goto-char (point-min))))
    buf))

(defun agent-mission-control--frame-p (&optional frame)
  "Return non-nil when FRAME is the dedicated MC frame."
  (let ((f (or frame (selected-frame))))
    (or (frame-parameter f agent-mission-control--frame-parameter)
        (string= (frame-parameter f 'name)
                 agent-mission-control-frame-name))))

(defun agent-mission-control--find-frame ()
  "Return the live MC frame, if any."
  (cl-find-if #'agent-mission-control--frame-p (frame-list)))

(defun agent-mission-control-refresh ()
  "Re-render the digest in place."
  (interactive)
  (agent-mission-control--render)
  (message "Agent Mission Control refreshed"))

(defun agent-mission-control--section-start-p (pos)
  "Return non-nil when POS is the start of a section header."
  (and pos
       (get-text-property pos 'agent-mc-section)
       (or (= pos (point-min))
           (not (get-text-property (1- pos) 'agent-mc-section)))))

(defun agent-mission-control-next-section ()
  "Move point to the next buffer-section header."
  (interactive)
  (let ((target nil)
        (pos (point)))
    ;; If point is already inside a header, step past it before searching.
    (when (get-text-property pos 'agent-mc-section)
      (setq pos (or (next-single-property-change pos 'agent-mc-section)
                    (point-max))))
    (while (and pos (< pos (point-max)) (not target))
      (setq pos (next-single-property-change pos 'agent-mc-section))
      (when (and pos (get-text-property pos 'agent-mc-section))
        (setq target pos)))
    (if target
        (progn (goto-char target) (beginning-of-line))
      (message "No further sections"))))

(defun agent-mission-control-previous-section ()
  "Move point to the previous buffer-section header."
  (interactive)
  (let ((target nil)
        (pos (point)))
    ;; If we're already on a header line, step before it first.
    (when (agent-mission-control--section-start-p (line-beginning-position))
      (setq pos (max (point-min) (1- (line-beginning-position)))))
    (while (and pos (> pos (point-min)) (not target))
      (setq pos (previous-single-property-change pos 'agent-mc-section))
      (when (and pos
                 (> pos (point-min))
                 (get-text-property (1- pos) 'agent-mc-section)
                 (not (get-text-property pos 'agent-mc-section)))
        ;; pos is just past the end of a header; back up to its start.
        (setq pos (previous-single-property-change pos 'agent-mc-section)))
      (when (and pos
                 (get-text-property pos 'agent-mc-section)
                 (agent-mission-control--section-start-p pos))
        (setq target pos)))
    (if target
        (progn (goto-char target) (beginning-of-line))
      (message "No previous sections"))))

(defun agent-mission-control-quit ()
  "Dismiss the MC frame, or quit-window if not in a dedicated frame."
  (interactive)
  (if (agent-mission-control--frame-p)
      (delete-frame)
    (quit-window)))

(defun agent-mission-control--pcre-quote (s)
  "Escape PCRE metacharacters in S for use in a Sway `[title=...]' criterion."
  (replace-regexp-in-string "[][.*?+^$|(){}\\\\]"
                            (lambda (m) (concat "\\\\" m))
                            s))

(defun agent-mission-control--swaymsg-focus (frame)
  "Focus FRAME via Sway IPC.  Returns t on success, nil otherwise.
Wayland normally rejects cross-window focus requests from Emacs because
no XDG activation token is available; `swaymsg' bypasses that since the
WM itself is the actor."
  (let ((name (and (frame-live-p frame) (frame-parameter frame 'name))))
    (when (and name
               (stringp name)
               (not (string-empty-p name))
               (executable-find "swaymsg")
               (getenv "SWAYSOCK"))
      (let ((quoted (agent-mission-control--pcre-quote name)))
        (zerop
         (call-process "swaymsg" nil nil nil
                       (format "[title=\"^%s$\"]" quoted)
                       "focus"))))))

(defun agent-mission-control--focus-frame (frame)
  "Focus FRAME at the WM level, preferring Sway IPC.
Falls back to `select-frame-set-input-focus' on non-Sway setups."
  (or (agent-mission-control--swaymsg-focus frame)
      (progn (select-frame-set-input-focus frame) t)))

(defun agent-mission-control--section-at-point ()
  "Return the source-buffer name for the section containing point, or nil."
  (save-excursion
    (let ((found nil)
          (continue t))
      (forward-line 0)
      (while continue
        (let ((s (get-text-property (point) 'agent-mc-section)))
          (cond
           (s (setq found s continue nil))
           ((bobp) (setq continue nil))
           (t (forward-line -1)))))
      found)))

(defun agent-mission-control-visit-section ()
  "Dismiss the MC frame and jump to the buffer for the section at point.
If the buffer is already visible in some window, raise that frame and
select that window.  Otherwise, switch to the buffer in another frame
\(creating no new windows by default).

Focus is settled on the target window before the MC frame is deleted,
so the WM doesn't pick a fallback frame between operations."
  (interactive)
  (let* ((name (agent-mission-control--section-at-point))
         (target (and name (get-buffer name))))
    (unless target
      (user-error "No section at point"))
    (let* ((mc-frame (and (agent-mission-control--frame-p) (selected-frame)))
           (existing (get-buffer-window target t))
           (existing-frame (and (window-live-p existing) (window-frame existing)))
           (other-frame
            (or (and existing-frame
                     (not (eq existing-frame mc-frame))
                     existing-frame)
                (cl-find-if
                 (lambda (f) (and (frame-live-p f) (not (eq f mc-frame))))
                 (frame-list)))))
      (cond
       ;; Already visible in a non-MC frame — go there.
       ((and (window-live-p existing)
             (not (eq existing-frame mc-frame)))
        (select-window existing)
        (agent-mission-control--focus-frame existing-frame))
       ;; Not visible (or only visible in the MC frame itself) — pick
       ;; another frame and switch its selected window to the target.
       ;; Avoid `with-selected-frame': its unwind restores the MC buffer
       ;; via `set-buffer', which makes the WM follow focus back to the
       ;; chat session after `delete-frame'.
       (other-frame
        (select-frame other-frame)
        (switch-to-buffer target)
        (agent-mission-control--focus-frame other-frame))
       ;; No other frame — display within the MC frame as a fallback
       ;; (and don't delete it, since we'd leave nothing to display in).
       (t
        (switch-to-buffer target)
        (setq mc-frame nil)))
      ;; Delete the MC frame last so the WM keeps focus where we set it.
      (when (and mc-frame
                 (frame-live-p mc-frame)
                 (not (eq (selected-frame) mc-frame))
                 (> (length (frame-list)) 1))
        (delete-frame mc-frame)))))

;;;###autoload
(defun agent-mission-control ()
  "Show or raise the agent mission-control digest in a dedicated frame."
  (interactive)
  (let* ((buf (agent-mission-control--render))
         (existing (agent-mission-control--find-frame))
         (frame (or existing
                    (make-frame
                     `((name . ,agent-mission-control-frame-name))))))
    (set-frame-parameter frame agent-mission-control--frame-parameter t)
    (with-selected-frame frame
      (delete-other-windows)
      (let ((win (selected-window)))
        (set-window-buffer win buf)
        (set-window-dedicated-p win t)
        (set-window-parameter win 'no-other-window t)))
    (select-frame-set-input-focus frame)
    buf))

(defun agent-mission-control--maybe-configure (&optional frame)
  "Auto-configure FRAME when it was created as the MC frame.
This fires on `server-after-make-frame-hook' so a terminal-launched
emacsclient frame (via the launcher script) renders immediately."
  (let ((f (or frame (selected-frame))))
    (when (and (frame-live-p f)
               (agent-mission-control--frame-p f))
      (with-selected-frame f
        (let ((buf (agent-mission-control--render)))
          (delete-other-windows)
          (let ((win (selected-window)))
            (set-window-buffer win buf)
            (set-window-dedicated-p win t)
            (set-window-parameter win 'no-other-window t)))))))

(unless (memq #'agent-mission-control--maybe-configure
              after-make-frame-functions)
  (add-hook 'after-make-frame-functions
            #'agent-mission-control--maybe-configure))

(unless (memq #'agent-mission-control--maybe-configure
              server-after-make-frame-hook)
  (add-hook 'server-after-make-frame-hook
            #'agent-mission-control--maybe-configure))

(provide 'agent-mission-control)

;;; agent-mission-control.el ends here
