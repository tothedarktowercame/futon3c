;;; session-overview.el --- Scratch-style nested view of a session's weak scopes -*- lexical-binding: t; -*-

;; The session analog of `mission-mode-overview' (its donor): a session is a
;; tree of WEAK SCOPES — each operator turn is a scope, weakly typed by the
;; tags it touches (Joe's reframe, M-points-de-fuite, 2026-06-26: a session's
;; structure is not missing, just weak).  Where a mission's scopes come from
;; substrate-2 hyperedges annotated onto a .md buffer, a session's scopes are
;; computed live from the transcript by `session_scope_view.py', which emits a
;; JSON donor-tree (session -> sub-arcs -> leaf turns).  This pane renders that
;; tree with the SAME foldable, depth-coloured idiom as the mission overview —
;; reusing mission-mode's fold primitives and depth faces so the two panels read
;; as siblings.  `g' re-runs the detector, so the view refreshes "live" as the
;; session grows.
;;
;; Usage:  M-x session-overview   (regenerates from the most-recent session,
;; then opens the side panel).  Keys in the panel: TAB fold/pin · t expand/
;; collapse all · RET show the turn's full text · g regenerate+rebuild · q quit.

;;; Code:

(require 'json)
(require 'mission-mode)   ; donor: --block-fold/--block-unfold, --depth-face, depth faces

(defgroup session-overview nil
  "Scratch-style nested view of a session's weak scopes."
  :group 'convenience)

(defcustom session-overview-python
  "/home/joe/code/futon6/.venv/bin/python"
  "Python interpreter that has the transcript-provenance deps."
  :type 'string :group 'session-overview)

(defcustom session-overview-script
  "/home/joe/code/futon6/scripts/session_scope_view.py"
  "The session-scope detector; emits the .org tree and the .json donor-shape."
  :type 'string :group 'session-overview)

(defcustom session-overview-org-file
  "/home/joe/code/futon2/holes/session-scope-view.org"
  "Where the detector writes the org tree; the .json sits beside it."
  :type 'string :group 'session-overview)

(defface session-overview-badge-face
  '((t :background "#475569" :foreground "white" :weight bold :height 0.85))
  "Badge chip for a session sub-arc (its dominant tag)." :group 'session-overview)

(defface session-overview-field-face
  '((t :inherit font-lock-doc-face))
  "Face for the concentration-field line." :group 'session-overview)

;; Mined War-Machine field: forward = build (the policy half, teal, like the
;; mission phase badge), correction = steer (amber), reach/want = the belly
;; (lavender, echoing the R19 `s-belly' colour in the aif-wiring explainer).
(defface session-overview-build-face
  '((t :background "#0f766e" :foreground "white" :weight bold :height 0.85))
  "Chip for a forward ▶build move mined from a turn." :group 'session-overview)
(defface session-overview-steer-face
  '((t :background "#b45309" :foreground "white" :weight bold :height 0.85))
  "Chip for a ✎steer (correction) move mined from a turn." :group 'session-overview)
(defface session-overview-want-face
  '((t :foreground "#7c5cff" :weight bold))
  "Face for the ◀wanted (reach/belly) referents in the header." :group 'session-overview)
(defface session-overview-reach-face
  '((t :background "#7c5cff" :foreground "white" :weight bold :height 0.85))
  "Chip for a ◀reach (應-voice) move mined from an agent turn." :group 'session-overview)
(defface session-overview-agent-face
  '((((background light)) :foreground "#5b3fd6")
    (((background dark)) :foreground "#b3a4ff"))
  "Face for an agent (應-voice) turn's head — distinguishes it from operator turns."
  :group 'session-overview)

;; Comb spine: foreground glyphs, one per arc, so a mission's tooth reads as a
;; colored bar across the session stream (E-the-dark-tower-2 pivot layer).
(defface session-overview-spine-build '((t :foreground "#0f766e" :weight bold))
  "Comb-spine glyph for a ▶build arc." :group 'session-overview)
(defface session-overview-spine-steer '((t :foreground "#b45309" :weight bold))
  "Comb-spine glyph for a ✎steer arc." :group 'session-overview)
(defface session-overview-spine-none '((t :inherit shadow))
  "Comb-spine glyph for an untouched arc." :group 'session-overview)
(defface session-overview-spine-mention
  '((((background light)) :foreground "#475569") (((background dark)) :foreground "#94a3b8"))
  "Comb-spine glyph for a CURRENT mention (engaged, no mined move-type) — keeps the comb live."
  :group 'session-overview)

;; All per-overview state is BUFFER-LOCAL so multiple session-overview buffers
;; (one per session — claude-1, claude-11, …) coexist without clobbering each other.
(defvar-local session-overview--blocks nil
  "Foldable blocks in the panel: each arc folds the run of turn lines under it.")
(defvar-local session-overview--data nil
  "The most recently loaded session-scope tree (alist).")
(defvar-local session-overview--filter nil
  "When non-nil, a mission name pinned from the comb: the arcs below narrow to
just the turns whose mined moves touch that mission (deterministic cross-filter).")
(defvar-local session-overview--session-id nil
  "The session id THIS overview buffer is showing (set on open; used by `g').")
(defvar-local session-overview--label nil
  "Human label for this overview's session (agent name, e.g. \"claude-11\").")

(defun session-overview--json-file (&optional sid)
  "Per-session JSON path for SID (8-char prefix); the legacy single file if nil."
  (let ((base (file-name-sans-extension session-overview-org-file)))
    (if sid (format "%s-%s.json" base (substring sid 0 (min 8 (length sid))))
      (concat base ".json"))))

(defun session-overview--org-file (sid)
  "Per-session ORG out-path passed to the detector (its .json lands beside it)."
  (format "%s-%s.org" (file-name-sans-extension session-overview-org-file)
          (substring sid 0 (min 8 (length sid)))))

(defun session-overview--label-for (sid)
  "A human label for SID: the agent name of the claude-repl buffer driving it,
else the 8-char session prefix."
  (or (seq-some
       (lambda (buf)
         (with-current-buffer buf
           (and (eq major-mode 'claude-repl-mode)
                (equal (bound-and-true-p agent-chat--session-id) sid)
                (string-match "\\*claude-repl:\\(.*\\)\\*" (buffer-name))
                (match-string 1 (buffer-name)))))
       (buffer-list))
      (substring sid 0 (min 8 (length sid)))))

(defun session-overview--current-session-id ()
  "The session id of the session Emacs is driving — \"this session\".
Prefers the current buffer's own id, else the most-recently-active live
claude-repl buffer (`buffer-list' is in recency order).  Returns nil if no
live REPL is found, in which case the detector falls back to mtime-max.
This is the robust fix for the mtime tie between concurrently-written peer
sessions (e.g. claude-1 vs claude-8 both flushing the same minute)."
  (or (and (boundp 'agent-chat--session-id)
           (stringp agent-chat--session-id)
           (not (string-empty-p agent-chat--session-id))
           agent-chat--session-id)
      (seq-some
       (lambda (buf)
         (with-current-buffer buf
           (and (eq major-mode 'claude-repl-mode)
                (boundp 'agent-chat--session-id)
                (stringp agent-chat--session-id)
                (not (string-empty-p agent-chat--session-id))
                agent-chat--session-id)))
       (buffer-list))))

(defvar session-overview--inflight (make-hash-table :test 'equal)
  "session-id -> the detector process currently running for it (dedup).")
(defvar session-overview--pending (make-hash-table :test 'equal)
  "session-id -> on-done callback queued while a detector was already running.")

(defun session-overview--regenerate-async (sid on-done)
  "Run the detector for SID asynchronously — NEVER blocks Emacs.  ON-DONE (0-arg)
is funcalled on success.  If a detector for SID is already in flight, QUEUE this
request (latest wins) and relaunch when the running one finishes — so the newest
turn always renders, never silently dropped.  Silent — no minibuffer chatter."
  (if (gethash sid session-overview--inflight)
      (puthash sid on-done session-overview--pending)
    (let ((default-directory (file-name-directory session-overview-script)))
      (puthash sid
               (make-process
                :name (format "so-detect-%s" (substring sid 0 8))
                :buffer (get-buffer-create "*session-overview-log*")
                :noquery t
                :command (list session-overview-python session-overview-script
                               (session-overview--org-file sid) "--session-id" sid)
                :sentinel (lambda (p _e)
                            (when (memq (process-status p) '(exit signal))
                              (remhash sid session-overview--inflight)
                              (when (and (eq 0 (process-exit-status p))
                                         (functionp on-done))
                                (funcall on-done))
                              ;; a turn arrived mid-run → relaunch for the latest state
                              (when-let ((queued (gethash sid session-overview--pending)))
                                (remhash sid session-overview--pending)
                                (session-overview--regenerate-async sid queued)))))
               session-overview--inflight))))

(defun session-overview--load (&optional sid)
  "Read the JSON donor-tree for SID into an alist (lists, symbol keys)."
  (let ((f (session-overview--json-file sid)))
    (when (file-readable-p f)
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (json-read-file f)))))

;; --- Folding: an arc line folds the run of turn lines beneath it.  Reuse
;; mission-mode's overlay primitives; keep our own block list + block-at so the
;; two panes don't share state.

(defun session-overview--block-at (pos)
  (seq-find (lambda (b)
              (or (and (>= pos (plist-get b :parent)) (< pos (plist-get b :beg)))
                  (and (>= pos (plist-get b :beg)) (< pos (plist-get b :end)))))
            session-overview--blocks))

(defun session-overview-toggle ()
  "Toggle (pin) the fold of the arc at point."
  (interactive)
  (if-let ((block (session-overview--block-at (point))))
      (if (plist-get block :ov)
          (progn (mission-mode--block-unfold block) (plist-put block :pinned t))
        (plist-put block :pinned nil)
        (mission-mode--block-fold block))
    (message "session-overview: no foldable arc at point")))

(defun session-overview-toggle-all ()
  "Expand or collapse all arcs."
  (interactive)
  (let ((any-folded (seq-some (lambda (b) (plist-get b :ov)) session-overview--blocks)))
    (dolist (b session-overview--blocks)
      (if any-folded
          (progn (mission-mode--block-unfold b) (plist-put b :pinned t))
        (plist-put b :pinned nil)
        (mission-mode--block-fold b)))))

(defun session-overview--peek ()
  "Reveal the folded arc under point; re-fold unpinned arcs left behind."
  (let ((here (session-overview--block-at (point))))
    (dolist (b session-overview--blocks)
      (cond
       ((eq b here) (mission-mode--block-unfold b))
       ((and (not (plist-get b :pinned)) (not (plist-get b :ov)))
        (mission-mode--block-fold b))))))

(defvar-local session-overview--last-line nil)

(defun session-overview--sync ()
  "Peek-open the folded arc under point as point moves."
  (let ((line (line-number-at-pos)))
    (unless (eql line session-overview--last-line)
      (setq session-overview--last-line line)
      (session-overview--peek))))

(defun session-overview-visit ()
  "Show the full text of the turn at point (the session analog of visiting a scope)."
  (interactive)
  (if-let ((full (get-text-property (line-beginning-position) 'session-overview-full)))
      (let ((buf (get-buffer-create "*session-turn*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert full)
            (goto-char (point-min))
            (view-mode 1)))
        (display-buffer buf '((display-buffer-below-selected) (window-height . 0.3))))
    (message "session-overview: no turn at point")))

(defvar session-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'session-overview-visit)
    (define-key map (kbd "TAB") #'session-overview-toggle)
    (define-key map (kbd "t")   #'session-overview-toggle-all)
    (define-key map (kbd "f")   #'session-overview-pin-filter)
    (define-key map (kbd "g")   #'session-overview-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `session-overview-mode'.")

(define-derived-mode session-overview-mode special-mode "SessionOverview"
  "Visual abstract of a session's weak scopes; arcs fold their turns."
  (setq-local truncate-lines t cursor-in-non-selected-windows nil)
  (add-hook 'post-command-hook #'session-overview--sync nil t))

(defun session-overview--chip (glyph move face)
  (propertize (format " %s%s%s" glyph (alist-get 'op move)
                      (if (alist-get 'ref move) (format ":%s" (alist-get 'ref move)) ""))
              'face face))

(defface session-overview-thread-face
  '((((background light)) :foreground "#0e7490") (((background dark)) :foreground "#67e8f9"))
  "Face for an interleaved thread chip on a turn (the recurrent pattern it belongs to)."
  :group 'session-overview)

(defun session-overview--thread-chips (turn)
  "The recurrent threads this TURN belongs to, interleaved as ⟨sigil pattern⟩ chips."
  (mapconcat (lambda (th)
               (propertize (format " ⟨%s %s⟩"
                                   (or (alist-get 'sigil th) "·") (alist-get 'pattern th))
                           'face 'session-overview-thread-face))
             (alist-get 'threads turn) ""))

(defun session-overview--move-chips (turn)
  "Mined-move chips for TURN: ▶build (teal), ✎steer (amber), ◀reach (lavender)."
  (concat
   (mapconcat (lambda (f) (session-overview--chip "▶" f 'session-overview-build-face))
              (alist-get 'forward turn) "")
   (mapconcat (lambda (c) (session-overview--chip "✎" c 'session-overview-steer-face))
              (alist-get 'correction turn) "")
   (mapconcat (lambda (r) (session-overview--chip "◀" r 'session-overview-reach-face))
              (alist-get 'reach turn) "")))

(defun session-overview--insert-comb (pivot)
  "Insert the mission-comb pivot: each mission a TOOTH whose colored SPINE runs across
the arcs, lit where the session touched it (▶build teal / ◀reach lavender / ✎steer amber).
Reads the non-linear drift at a glance — a mission with two lit clusters was resumed.
Inserted as a foldable block kept OPEN by default (it is the at-a-glance layer)."
  (let* ((rows (alist-get 'rows pivot))
         (n (alist-get 'n_arcs pivot))
         (parent (point)))
    (insert (propertize (format " ▦ mission comb — %d missions × %d arcs  (time →, drift visible)\n"
                                (length rows) n)
                        'face 'mission-mode-title-face))
    (let ((beg (point)))
      (dolist (r rows)
        (let ((m (alist-get 'mission r))
              (start (point))
              (span (alist-get 'span r)))
          (insert (if (equal m session-overview--filter) "▣" " ")
                  (truncate-string-to-width (or m "?") 16 nil ?\s "…") " ")
          (dolist (cell (alist-get 'spine r))
            (insert (pcase cell
                      ("build"   (propertize "▶" 'face 'session-overview-spine-build))
                      ("reach"   (propertize "◀" 'face 'session-overview-want-face))
                      ("steer"   (propertize "◆" 'face 'session-overview-spine-steer))
                      ("mention" (propertize "●" 'face 'session-overview-spine-mention))
                      (_         (propertize "·" 'face 'session-overview-spine-none)))))
          (insert (propertize (format "  ▶%d◀%d✎%d●%d" (alist-get 'n_build r)
                                      (alist-get 'n_reach r) (alist-get 'n_steer r)
                                      (or (alist-get 'n_mention r) 0))
                              'face 'mission-mode-meta-face)
                  "\n")
          (add-text-properties
           start (point)
           (list 'session-overview-mission m
                 'session-overview-full
                 (format "%s — touched arcs %s · ▶%s build ◀%s reach ✎%s steer" m
                         (if span (format "%d–%d" (1+ (car span)) (1+ (cadr span))) "—")
                         (alist-get 'n_build r) (alist-get 'n_reach r) (alist-get 'n_steer r))))))
      (let ((end (point)))
        (when (> end beg)
          (push (list :parent parent :beg beg :end end :ov nil :pinned t :comb t)
                session-overview--blocks)))
      (insert "\n"))))

(defun session-overview--insert-arc (arc)
  "Insert one sub-ARC as a foldable block: a chip+label parent line, then its turns."
  (let* ((label (alist-get 'label arc))
         (tags  (alist-get 'tags arc))
         (turns (alist-get 'turns arc))
         (nf    (or (alist-get 'n_forward arc) 0))
         (nc    (or (alist-get 'n_correction arc) 0))
         (nr    (or (alist-get 'n_reach arc) 0))
         (parent (point)))
    (insert (propertize (format " %s " (upcase (truncate-string-to-width label 14 nil nil "…")))
                        'face 'session-overview-badge-face)
            " "
            (propertize (format "%d turns" (length turns))
                        'face (mission-mode--depth-face 1))
            (if (> (+ nf nc nr) 0)
                (propertize (format "  ▶%d◀%d✎%d" nf nr nc) 'face 'mission-mode-meta-face)
              "")
            (if tags
                (propertize (format "  :%s:" (mapconcat #'identity tags ":"))
                            'face 'mission-mode-meta-face)
              "")
            "\n")
    (let ((beg (point)))
      (dolist (turn turns)
        (let* ((head (alist-get 'head turn))
               (ttags (alist-get 'tags turn))
               (full (alist-get 'full turn))
               (agent (equal (alist-get 'role turn) "agent"))
               (tnum (alist-get 'turn_num turn))
               (move (alist-get 'move turn))
               (start (point)))
          (insert " "
                  (propertize (if tnum (format "t%-3d" tnum) "    ") 'face 'mission-mode-meta-face)
                  ;; reproducible move (mining-distilled basins): ▶build ◀reach ✎steer
                  (pcase move
                    ("build" (propertize " ▶" 'face 'session-overview-spine-build))
                    ("reach" (propertize " ◀" 'face 'session-overview-want-face))
                    ("steer" (propertize " ✎" 'face 'session-overview-spine-steer))
                    (_ "  "))
                  (if agent (propertize " 應 " 'face 'session-overview-want-face) " ")
                  (propertize (format "«%s…»" (truncate-string-to-width head 40 nil nil "…"))
                              'face (if agent 'session-overview-agent-face (mission-mode--depth-face 2)))
                  (propertize (format "  :%s:" (mapconcat #'identity ttags ":"))
                              'face 'mission-mode-meta-face)
                  (session-overview--move-chips turn)
                  (session-overview--thread-chips turn)
                  "\n")
          (add-text-properties start (point)
                               (list 'session-overview-full full 'help-echo full))))
      (let ((end (point)))
        (when (> end beg)
          (push (list :parent parent :beg beg :end end :ov nil :pinned nil)
                session-overview--blocks))))))

(defun session-overview--turn-touches-p (turn mission)
  "Non-nil if any mined move on TURN references MISSION."
  (seq-some (lambda (key)
              (seq-some (lambda (mv) (equal (alist-get 'ref mv) mission))
                        (alist-get key turn)))
            '(forward correction reach)))

(defun session-overview--insert-filtered (arcs mission)
  "Narrow view: just the turns (across all arcs) whose moves touch MISSION, in order.
Each line keeps its arc label as context.  This is the comb→timeline cross-filter."
  (let ((hits nil))
    (dolist (arc arcs)
      (dolist (turn (alist-get 'turns arc))
        (when (session-overview--turn-touches-p turn mission)
          (push (cons (alist-get 'label arc) turn) hits))))
    (setq hits (nreverse hits))
    (insert (propertize (format " ▣ pinned: %s — %d turns  (f clears)\n" mission (length hits))
                        'face 'mission-mode-title-face))
    (dolist (lt hits)
      (let* ((arc-label (car lt)) (turn (cdr lt))
             (agent (equal (alist-get 'role turn) "agent"))
             (start (point))
             (full (alist-get 'full turn)))
        (insert " "
                (propertize (format "[%s]" (truncate-string-to-width arc-label 10 nil nil "…"))
                            'face 'mission-mode-meta-face)
                " "
                (if agent (propertize "應 " 'face 'session-overview-want-face) "")
                (propertize (format "«%s…»" (truncate-string-to-width (alist-get 'head turn) 36 nil nil "…"))
                            'face (if agent 'session-overview-agent-face (mission-mode--depth-face 2)))
                (session-overview--move-chips turn)
                "\n")
        (add-text-properties start (point)
                             (list 'session-overview-full full 'help-echo full))))))

(defun session-overview--render (data)
  "Populate the *session-overview* buffer from DATA, honoring `session-overview--filter'.
Re-runnable without a python regenerate (used by the comb cross-filter)."
  (let ((inhibit-read-only t))
    (unless (derived-mode-p 'session-overview-mode) (session-overview-mode))
    (erase-buffer)
    (setq session-overview--blocks nil
          session-overview--last-line nil)
    (let ((sess  (alist-get 'session data))
          (nops  (alist-get 'n_ops data))
          (arcs  (alist-get 'arcs data))
          (field (alist-get 'field data))
          (mined (alist-get 'mined data)))
      (insert (propertize (format " session %s — %d weak scopes\n" sess nops)
                          'face 'mission-mode-title-face))
      (insert (propertize
               " keys: TAB fold · t all · f pin-comb/narrow · RET text · g regen · q quit\n"
               'face 'mission-mode-meta-face))
      (if mined
          ;; The reflow against the mined WM field: ▶built (policy) vs ◀wanted
          ;; (belly) — overlap is a coherent WM, divergence is the steering signal.
          (let ((refs (lambda (rows face)
                        (mapconcat (lambda (mc)
                                     (propertize (format "%s(%d)" (car mc) (cadr mc)) 'face face))
                                   rows "  "))))
            (insert (propertize
                     (format " WM field: ▶%d build · ◀%d reach (%d pinned) · ✎%d steer\n"
                             (alist-get 'n_forward mined) (alist-get 'n_reach mined)
                             (or (alist-get 'n_reach_pinned mined) 0)
                             (alist-get 'n_correction mined))
                     'face 'mission-mode-title-face))
            (insert "  " (propertize "▶built " 'face 'session-overview-build-face) " "
                    (funcall refs (alist-get 'built mined) 'session-overview-field-face) "\n")
            (insert "  " (propertize "◀wanted" 'face 'session-overview-want-face) " "
                    (funcall refs (alist-get 'wanted mined) 'session-overview-want-face) "\n\n"))
        (when field
          (insert (propertize
                   (concat " concentration-field: "
                           (mapconcat (lambda (mc) (format "%s(%d)" (car mc) (cadr mc)))
                                      field " ")
                           "\n\n")
                   'face 'session-overview-field-face))))
      ;; The pivot/comb layer (E-the-dark-tower-2): missions × session, atop the linear arcs.
      (when-let ((pivot (alist-get 'pivot data)))
        (when (alist-get 'rows pivot)
          (session-overview--insert-comb pivot)))
      ;; The arcs below — full, or narrowed to a pinned comb tooth.
      (if session-overview--filter
          (session-overview--insert-filtered arcs session-overview--filter)
        (dolist (arc arcs)
          (session-overview--insert-arc arc))))
    ;; Default: arcs folded (the comb block is :comb → stays open as the at-a-glance layer).
    (dolist (b session-overview--blocks)
      (unless (plist-get b :comb)
        (mission-mode--block-fold b)))
    (goto-char (point-min))))

(defun session-overview-pin-filter ()
  "Pin the comb tooth at point to narrow the arcs below to that mission; toggles off
if already pinned (or if point is not on a comb tooth)."
  (interactive)
  (let ((m (get-text-property (line-beginning-position) 'session-overview-mission)))
    (setq session-overview--filter
          (and m (not (equal m session-overview--filter)) m))
    (when session-overview--data
      (session-overview--render session-overview--data))
    (message (if session-overview--filter
                 (format "session-overview: narrowed to %s (f clears)" session-overview--filter)
               "session-overview: filter cleared"))))

(defun session-overview--show (sid label &optional select)
  "Open/refresh `*session-overview:LABEL*' for SID with ZERO blocking.
Renders whatever data is already on disk instantly, kicks an ASYNC detector, and
re-renders in place when it lands.  With SELECT, pop to and select its window."
  (let ((buf (get-buffer-create (format "*session-overview:%s*" label))))
    (with-current-buffer buf
      (unless (derived-mode-p 'session-overview-mode) (session-overview-mode))
      (setq session-overview--session-id sid
            session-overview--label label)
      (let ((data (session-overview--load sid)))   ; instant: show prior data, no wait
        (if data
            (progn (setq session-overview--data data) (session-overview--render data))
          (let ((inhibit-read-only t)) (erase-buffer)
               (insert (format "  session-overview:%s — detecting… (auto-updates)\n" label))))))
    (when select
      (display-buffer buf '((display-buffer-in-side-window)
                            (side . right) (window-width . 56)
                            (inhibit-same-window . t)))
      (when-let ((w (get-buffer-window buf))) (select-window w)))
    (session-overview--regenerate-async         ; async: re-render when ready
     sid
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((data (session-overview--load sid)) (pt (point)))
             (when data
               (setq session-overview--data data session-overview--filter nil)
               (session-overview--render data)
               (goto-char (min pt (point-max)))))))))
    buf))

;;;###autoload
(defun session-overview (&optional sid label)
  "Show a session's weak scopes as a nested, foldable visual abstract.
With no args, resolves the session from the current claude-repl buffer (else the
most-recent live REPL).  SID/LABEL pin a specific session.  Each session gets its
OWN buffer `*session-overview:<label>*', so several coexist (claude-1, claude-11…),
and each AUTO-UPDATES (non-blocking) as its session takes turns.
Point peeks an arc; TAB pins it; `t' toggles all; RET shows a turn's text; `f' pins
a comb tooth to narrow; `g' force-regenerates THIS buffer's session."
  (interactive)
  (let* ((sid (or sid (session-overview--current-session-id)))
         (label (or label (and sid (session-overview--label-for sid)))))
    (unless sid
      (user-error "session-overview: no session id (open from a claude-repl buffer)"))
    (session-overview--show sid label t)))

(defun session-overview-refresh ()
  "Force-regenerate and re-render THIS overview buffer's session (bound to `g')."
  (interactive)
  (if session-overview--session-id
      (session-overview--show session-overview--session-id session-overview--label nil)
    (call-interactively #'session-overview)))

(defun session-overview--auto-refresh (&rest _)
  "Advice on `agent-chat-finish-turn!': after a turn, refresh — NON-BLOCKING — any
open overview buffer for THIS session.  The async detector updates the panel when
it lands, so the operator never waits around."
  (let ((sid (and (boundp 'agent-chat--session-id)
                  (stringp agent-chat--session-id)
                  (not (string-empty-p agent-chat--session-id))
                  agent-chat--session-id)))
    (when sid
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (derived-mode-p 'session-overview-mode)
                     (equal session-overview--session-id sid))
            (session-overview--show sid session-overview--label nil)))))))

;; Auto-update is GLOBAL+idempotent (advice-add de-dups): every open overview
;; follows its session live, without each needing its own hook.
(with-eval-after-load 'agent-chat
  (advice-add 'agent-chat-finish-turn! :after #'session-overview--auto-refresh))
(when (fboundp 'agent-chat-finish-turn!)
  (advice-add 'agent-chat-finish-turn! :after #'session-overview--auto-refresh))

;; --- §5.1: keep the served WebArxana orbit LIVE for a chosen target ---

(defcustom session-overview-live-orbit nil
  "When set to (SESSION-ID . MISSION), keep the served WebArxana orbit
\(/wa/thread-orbits.json) LIVE for that one session × target: regenerate it
\(async, debounced) on each of THAT session's turn-ends, so the orbit winds as
it works.  Single served file → bound to ONE (session . mission) so concurrent
mesh agents don't fight over it.  The WebArxana client polls, so it follows live."
  :type '(choice (const :tag "off" nil)
                 (cons (string :tag "session-id") (string :tag "mission/excursion id")))
  :group 'session-overview)

(defvar session-overview--orbit-inflight (make-hash-table :test 'equal)
  "session-id -> the reflow process producing its served orbit (debounce/dedup).")

(defun session-overview--orbit-producer (&rest _)
  "Advice on `agent-chat-finish-turn!': for the ONE session in `session-overview-live-orbit',
regenerate the served WebArxana orbit — async + debounced — so the live map winds as it works.
reflow.py is ~10-12s (only the comb embeds)."
  (let* ((cfg session-overview-live-orbit)
         (want (car-safe cfg)) (mission (cdr-safe cfg))
         (sid (and (boundp 'agent-chat--session-id) (stringp agent-chat--session-id)
                   (not (string-empty-p agent-chat--session-id)) agent-chat--session-id)))
    (when (and (consp cfg) sid mission (equal sid want)
               (not (gethash sid session-overview--orbit-inflight)))
      (let ((default-directory "/home/joe/code/futon6/"))
        (puthash sid
                 (make-process
                  :name (format "wa-orbit-%s" (substring sid 0 8))
                  :buffer (get-buffer-create "*session-overview-log*")
                  :noquery t
                  :command (list "/home/joe/code/futon6/.venv/bin/python" "scripts/reflow.py"
                                 sid mission "--wa"
                                 "/home/joe/code/futon4/data/webarxana/public/wa/thread-orbits.json")
                  :sentinel (lambda (p _e)
                              (when (memq (process-status p) '(exit signal))
                                (remhash sid session-overview--orbit-inflight))))
                 session-overview--orbit-inflight)))))

(with-eval-after-load 'agent-chat
  (advice-add 'agent-chat-finish-turn! :after #'session-overview--orbit-producer))
(when (fboundp 'agent-chat-finish-turn!)
  (advice-add 'agent-chat-finish-turn! :after #'session-overview--orbit-producer))

(provide 'session-overview)
;;; session-overview.el ends here
