;;; latex-wysiwyg.el --- two-way point sync between a LaTeXML page and Emacs  -*- lexical-binding: t; -*-

;; Slice S1 of M-latex-wysiwyg: READ-ONLY two-way navigation.
;;
;;   click a paragraph in the browser  ->  point moves in the right .tex buffer
;;   move point in a mapped .tex buffer -> the browser highlights that block
;;
;; S1 writes NOTHING. There is no edit path in this file, deliberately: the
;; gate for this slice is that navigation is accurate, and it should be
;; possible to run it against the real paper with no risk to the sources.
;;
;; Transport: Emacs runs its own websocket server on `latex-wysiwyg-port'.
;; It does NOT reuse the Agency socket (`futon-agency-ws.el'), because that
;; would mean teaching the JVM a new message type and restarting it. It does
;; NOT extend `smart-cursor.el' either -- that holds a deliberate non-editing
;; invariant its e2e harness depends on, and this is a separate actuator.
;;
;; Positions arrive as latexml-oxide `data-sourcepos' values, "F:L:C-F:L:C",
;; where F indexes the source files in \input encounter order. The mapping is
;; supplied by the build step in scopes.json; see `latex-wysiwyg-load-map'.

;;; Code:

(require 'json)
(require 'cl-lib)

(declare-function websocket-server "websocket" (port &rest plist))
(declare-function websocket-server-close "websocket" (conn))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-frame-text "websocket" (frame))

(defgroup latex-wysiwyg nil
  "Two-way point sync between a LaTeXML-rendered page and Emacs."
  :group 'tex)

(defcustom latex-wysiwyg-port 7079
  "Port for the WYSIWYG websocket server.
Deliberately not 7070: Agency owns that, and this must not disturb it."
  :type 'integer :group 'latex-wysiwyg)

(defcustom latex-wysiwyg-follow t
  "When non-nil, display the buffer a browser click lands in."
  :type 'boolean :group 'latex-wysiwyg)

(defcustom latex-wysiwyg-echo t
  "When non-nil, echo each incoming jump in the minibuffer."
  :type 'boolean :group 'latex-wysiwyg)

(defvar latex-wysiwyg--server nil "The websocket server process, or nil.")
(defvar latex-wysiwyg--clients nil "Currently connected browser sockets.")
(defvar latex-wysiwyg--file-map nil
  "Vector mapping a sourcepos file index to an absolute file name.")
(defvar latex-wysiwyg--last-jump nil
  "Plist describing the most recent jump; the e2e gate reads this.")
(defvar latex-wysiwyg--hellos nil
  "Version strings announced by connected browsers.
A browser running a stale cached script is otherwise invisible from here.")

(defvar latex-wysiwyg--suppress nil
  "Non-nil while applying a remote jump, to avoid echoing it back.")

;;; ---------------------------------------------------------------- mapping

(defun latex-wysiwyg-load-map (scopes-json)
  "Load the file-index mapping from SCOPES-JSON produced by the build step."
  (interactive "fscopes.json: ")
  (let* ((json-object-type 'alist)
         (data (json-read-file scopes-json))
         (dir  (or (cdr (assq 'src-dir data))
                   (file-name-directory (expand-file-name scopes-json))))
         (files (cdr (assq 'files data))))
    (setq latex-wysiwyg--source-sha (cdr (assq 'source-sha data)))
    ;; A null entry means "synthesised source with nowhere to jump" (e.g. the
    ;; generated bibliography). Keep it as nil so the index positions stay
    ;; aligned, and refuse cleanly rather than visiting a nonexistent file.
    (setq latex-wysiwyg--file-map
          (vconcat (mapcar (lambda (f)
                             (and f (not (eq f :null))
                                  (stringp f)
                                  (expand-file-name f dir)))
                           files)))
    (when (called-interactively-p 'interactive)
      (message "latex-wysiwyg: %d source files mapped"
               (length latex-wysiwyg--file-map)))
    latex-wysiwyg--file-map))

(defvar latex-wysiwyg--source-sha nil
  "Alist of basename -> sha recorded when the page was built.")

(defun latex-wysiwyg-stale-sources ()
  "Mapped files whose contents differ from when the page was built.
A stale page refuses every edit with `literal-not-found\='  -- correct, but
inscrutable. Editing against a view of text that no longer exists is the
failure mode this detects (2026-08-08: a page 100 minutes out of date)."
  (let (stale)
    (dolist (entry latex-wysiwyg--source-sha)
      (let* ((name (symbol-name (car entry)))
             (want (cdr entry))
             (path (cl-find-if (lambda (f) (and f (equal (file-name-nondirectory f) name)))
                               (append latex-wysiwyg--file-map nil))))
        (when (and path (file-exists-p path))
          (let ((got (substring (secure-hash 'sha256
                                             (with-temp-buffer
                                               (insert-file-contents-literally path)
                                               (buffer-string)))
                                0 16)))
            (unless (equal got want) (push name stale))))))
    (nreverse stale)))


(defun latex-wysiwyg--refresh-sha (file)
  "Re-record FILE's hash after WE changed it.
Without this the staleness guard is single-use: the first successful edit
changes the source, the recorded hash no longer matches, and every later edit
is refused as `page-stale'. Emacs made the change and the splice preserves
line count, so the page's anchors remain valid -- only EXTERNAL drift should
count as stale."
  (let* ((name (intern (file-name-nondirectory file)))
         (new (substring (secure-hash 'sha256
                                      (with-temp-buffer
                                        (insert-file-contents-literally file)
                                        (buffer-string)))
                         0 16))
         (cell (assq name latex-wysiwyg--source-sha)))
    (when cell (setcdr cell new))
    new))

(defun latex-wysiwyg--generated-p (file)
  "Non-nil if FILE announces itself as machine-generated."
  (with-temp-buffer
    (insert-file-contents file nil 0 400)
    (goto-char (point-min))
    (re-search-forward "GENERATED FROM\\|DO NOT EDIT" nil t)))

(defun latex-wysiwyg--file-for (index)
  "Absolute file name for sourcepos file INDEX, or nil."
  (when (and latex-wysiwyg--file-map
             (>= index 0)
             (< index (length latex-wysiwyg--file-map)))
    (aref latex-wysiwyg--file-map index)))

(defun latex-wysiwyg--index-for (file)
  "Sourcepos index for FILE, or nil."
  (when (and file latex-wysiwyg--file-map)
    (cl-position (expand-file-name file) latex-wysiwyg--file-map :test #'equal)))

;;; -------------------------------------------------------------- inbound

(defun latex-wysiwyg--fresh-buffer (file)
  "Visit FILE, reverting silently when the buffer is stale but unmodified.
External editors (the rocket loop) write the tex on disk; without this,
every outside edit makes the next navigation or scope/edit prompt.
A modified stale buffer is a genuine conflict and keeps Emacs's prompt."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      ;; keep the buffer tracking disk from now on: the rocket loop writes
      ;; the tex externally as part of normal operation
      (unless auto-revert-mode (auto-revert-mode 1))
      (when (and (not (buffer-modified-p))
                 (not (verify-visited-file-modtime (current-buffer))))
        (revert-buffer :ignore-auto :noconfirm :preserve-modes)))
    buf))

(cl-defun latex-wysiwyg--goto (index line col)
  "Move point to LINE/COL of the file at sourcepos INDEX.
Returns a plist describing where we landed, for the e2e gate."
  (let ((file (latex-wysiwyg--file-for index)))
    (unless (and file (file-exists-p file))
      ;; Three different situations, and conflating them cost an hour: the
      ;; index may be a genuinely synthesised source (nil in the map), or
      ;; beyond the map entirely (the map was replaced or never loaded), or
      ;; mapped to a file that has since moved.
      (let ((reason (cond ((>= index (length (or latex-wysiwyg--file-map [])))
                           "map-stale")
                          ((null file) "synthesised-source")
                          (t "no-such-file"))))
        (setq latex-wysiwyg--last-jump (list :refused reason :index index))
        (latex-wysiwyg--broadcast
         (list (cons "type" "scope/reject") (cons "reason" reason)
               (cons "file" index)))
        (when latex-wysiwyg-echo
          (message "latex-wysiwyg: %s for index %s%s" reason index
                   (if (equal reason "map-stale")
                       "  (re-run latex-wysiwyg-start with scopes.json)" "")))
        (cl-return-from latex-wysiwyg--goto latex-wysiwyg--last-jump)))
    (let ((buf (latex-wysiwyg--fresh-buffer file)))
      (with-current-buffer buf
        (goto-char (point-min))
        (forward-line (1- line))
        ;; oxide columns are 1-based; clamp so a stale column cannot error.
        (move-to-column (max 0 (1- col)))
        (setq latex-wysiwyg--last-jump
              (list :file (file-name-nondirectory file)
                    :line (line-number-at-pos)
                    :column (current-column)
                    :point (point)
                    :paragraph-line
                    (save-excursion
                      (backward-paragraph)
                      (forward-line (if (looking-at-p "^\\s-*$") 1 0))
                      (line-number-at-pos))
                    :context (buffer-substring-no-properties
                              (line-beginning-position)
                              (min (point-max) (+ (line-beginning-position) 60))))))
      (when latex-wysiwyg-follow
        (let ((latex-wysiwyg--suppress t))
          (display-buffer buf)
          ;; Keep the window's point in step with the buffer's.
          (dolist (w (get-buffer-window-list buf nil t))
            (set-window-point w (with-current-buffer buf (point))))))
      (when latex-wysiwyg-echo
        (message "latex-wysiwyg: %s:%d:%d"
                 (plist-get latex-wysiwyg--last-jump :file)
                 (plist-get latex-wysiwyg--last-jump :line)
                 (plist-get latex-wysiwyg--last-jump :column)))
      latex-wysiwyg--last-jump)))

(defun latex-wysiwyg--on-message (_ws frame)
  "Handle one inbound FRAME."
  (condition-case err
      (let* ((text (websocket-frame-text frame))
             (msg  (json-parse-string text :object-type 'alist))
             (type (alist-get 'type msg)))
        (cond
         ((equal type "scope/point")
          (latex-wysiwyg--goto (alist-get 'file msg)
                               (alist-get 'line msg)
                               (alist-get 'col msg)))
         ((equal type "scope/edit")
          ;; Stash the text BEFORE deciding anything. A refusal used to journal
          ;; only its reason, so the words the author typed were lost -- which
          ;; defeats the point of journalling at all.
          (setq latex-wysiwyg--attempt
                (list (cons "kind" "run")
                      (cons "pairs" (vector (list (cons "old" (or (alist-get 'old msg) ""))
                                                  (cons "new" (or (alist-get 'new msg) "")))))))
          (unwind-protect
              (latex-wysiwyg--edit (alist-get 'file msg) (alist-get 'line msg)
                                   (alist-get 'col msg)  (alist-get 'old msg)
                                   (alist-get 'new msg))
            (setq latex-wysiwyg--attempt nil)))
         ((equal type "scope/edit-para")
          (let* ((o (alist-get 'old msg)) (n (alist-get 'new msg))
                 (lits (lambda (v) (delq nil (mapcar
                                     (lambda (e) (and (equal (cdr (assq 'k e)) "lit")
                                                      (cdr (assq 't e))))
                                     (append v nil)))))
                 (pairs (latex-wysiwyg--changed-pairs (funcall lits o) (funcall lits n))))
            (setq latex-wysiwyg--attempt
                  (list (cons "kind" "paragraph") (cons "pairs" pairs)))
            (unwind-protect
                (latex-wysiwyg--edit-template (alist-get 'file msg) (alist-get 'line msg)
                                              (alist-get 'col msg) o n)
              (setq latex-wysiwyg--attempt nil))))
         ((equal type "scope/delete-atom")
          (latex-wysiwyg--atom-op (alist-get 'file msg) (alist-get 'line msg)
                                  (alist-get 'col msg) (alist-get 'old msg)
                                  (alist-get 'atom msg) 'delete nil))
         ((equal type "scope/edit-atom")
          (latex-wysiwyg--atom-op (alist-get 'file msg) (alist-get 'line msg)
                                  (alist-get 'col msg) (alist-get 'old msg)
                                  (alist-get 'atom msg) 'retex (alist-get 'tex msg)))
         ((equal type "hello")
          (let ((v (alist-get 'version msg))
                (ua (alist-get 'ua msg)))
            (push (list :version v :ua (and ua (substring ua 0 (min 60 (length ua)))))
                  latex-wysiwyg--hellos)
            (message "latex-wysiwyg: browser connected, client %s" v)
            (latex-wysiwyg--announce-state)))
         ((equal type "ping")
          (latex-wysiwyg--broadcast (list (cons "type" "pong"))))
         (t (message "latex-wysiwyg: ignoring message type %s" type))))
    (error (message "latex-wysiwyg: bad frame: %s" (error-message-string err)))))

;;; ------------------------------------------------------------- outbound

(defun latex-wysiwyg--broadcast (alist)
  "Send ALIST as JSON to every connected client."
  (let ((text (json-encode alist)))
    (dolist (ws latex-wysiwyg--clients)
      (ignore-errors (websocket-send-text ws text)))))

(defvar latex-wysiwyg--idle-timer nil)
(defvar latex-wysiwyg--last-sent nil "Last (buffer . line) broadcast.")

(defun latex-wysiwyg--post-command ()
  "Deliberately empty. See `latex-wysiwyg--idle-report'.

Doing websocket I/O from `post-command-hook' puts network work on EVERY
command in EVERY buffer -- a global tax on the editor, and a hang or a spin
in it wedges Emacs itself rather than just this feature. Point reporting is
not urgent; it belongs on an idle timer."
  nil)

(defun latex-wysiwyg--idle-report ()
  "Report point to the browser, at most once per resting position."
  (when (and latex-wysiwyg--clients (not latex-wysiwyg--suppress))
    (let ((index (latex-wysiwyg--index-for (buffer-file-name))))
      (when index
        (let ((now (cons (current-buffer) (line-number-at-pos))))
          (unless (equal now latex-wysiwyg--last-sent)
            (setq latex-wysiwyg--last-sent now)
            (latex-wysiwyg--broadcast
             (list (cons "type" "scope/point")
                   (cons "file" index)
                   (cons "line" (line-number-at-pos))
                   (cons "col" (1+ (current-column)))))))))))

(defun latex-wysiwyg--post-command-legacy ()
  "Previous implementation, retained only for reference."
  (unless latex-wysiwyg--suppress
    (let ((index (latex-wysiwyg--index-for (buffer-file-name))))
      (when (and index latex-wysiwyg--clients)
        (latex-wysiwyg--broadcast
         (list (cons "type" "scope/point")
               (cons "file" index)
               (cons "line" (line-number-at-pos))
               (cons "col"  (1+ (current-column)))))))))


;;; ----------------------------------------------------------------- editing
;; Slice S2: Class A editing, at RUN granularity.
;;
;; Measured on draft8: only 11% of prose paragraphs are wholly literal (no
;; \emph, \parencite, math...), so a paragraph is the wrong unit -- almost
;; nothing would be editable. Maximal runs of >=40 characters with no inline
;; markup cover 90% of the prose across 391 runs, so the RUN is the unit.
;;
;; A run is literal by construction, so it appears verbatim in the source
;; modulo hard wrapping and TeX escapes. We locate it with a whitespace- and
;; escape-tolerant regexp and require a UNIQUE match. Anything else refuses.
;; The refusal is the feature: we never guess where an edit belongs.

(defcustom latex-wysiwyg-allow-edits nil
  "When non-nil, honour scope/edit messages. Off by default.
S2 writes to real sources; arming this is a deliberate act."
  :type 'boolean :group 'latex-wysiwyg)

(defcustom latex-wysiwyg-rocket-url "http://127.0.0.1:8130/edit"
  "Rocket receiver endpoint for captured edits; nil disables capture.
A successful run edit POSTs {old, new, file, line} so the direct edit
feeds the writing-pattern loop exactly as a voice note does (the
finalized text is supplied, so processing is generalize-and-sweep).
Atom and whole-paragraph commits are not captured yet."
  :type '(choice (const nil) string)
  :group 'latex-wysiwyg)

(defun latex-wysiwyg--rocket-capture (old new file line)
  "Async, failure-silent POST of an applied edit to the rocket receiver."
  (when latex-wysiwyg-rocket-url
    (ignore-errors
      (let ((url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/json")))
            (url-request-data
             (encode-coding-string
              (json-encode `(("old" . ,old) ("new" . ,new)
                             ("file" . ,(file-name-nondirectory file))
                             ("line" . ,line)))
              'utf-8)))
        (url-retrieve latex-wysiwyg-rocket-url (lambda (_) nil) nil t t)))))

(defcustom latex-wysiwyg-save-after-edit t
  "When non-nil, save the buffer after a successful edit."
  :type 'boolean :group 'latex-wysiwyg)

(defvar latex-wysiwyg--last-edit nil "Plist describing the last edit attempt.")

(defcustom latex-wysiwyg-refill t
  "Re-wrap a paragraph after editing it.
Splicing text in without re-wrapping leaves one very long line -- a browser
edit produced a 1980-character line, and long lines make `redisplay_internal'
expensive enough to bog the whole editor down. Re-wrapping is safe now that
page staleness is detected and reported."
  :type 'boolean :group 'latex-wysiwyg)

(defun latex-wysiwyg--refill (beg end)
  "Re-wrap BEG..END using the paragraph's own prevailing width.
Filling to the global `fill-column' would rewrap to a different measure from
the rest of the file and produce a needlessly large diff."
  (when latex-wysiwyg-refill
    (let* ((width (save-excursion
                    (goto-char beg)
                    (let ((m 0))
                      (while (< (point) end)
                        (setq m (max m (- (line-end-position) (line-beginning-position))))
                        (forward-line 1))
                      m)))
           (fill-column (max 60 (min 100 (if (> width 0) width 80))))
           (fill-paragraph-function nil))
      (save-excursion (fill-region beg end nil t)))))

(defun latex-wysiwyg--rx-for (text)
  "A regexp matching TEXT in LaTeX source, tolerant of wrapping and escapes."
  (let ((out '())
        (chars (append text nil)))
    (while chars
      (let ((c (car chars)))
        (cond
         ;; any whitespace in the rendered run may be a hard line break
         ;; A rendered space can come from ordinary whitespace, a hard line
         ;; break, a tie (~), or a TeX spacing command. "Figure~\\ref{...}"
         ;; renders "Figure 3" -- matching only [ \t\n] broke every literal
         ;; that touched a tie, which is most of them.
         ((memq c '(?\s ?\t ?\n))
          (while (and chars (memq (car chars) '(?\s ?\t ?\n))) (pop chars))
          (push "\\(?:[ \t\n]\\|~\\|\\\\[,;:!>]\\|\\\\ \\)+" out))
         ;; characters the source must escape
         ((eq c ?%)  (push "\\\\%"  out) (pop chars))
         ((eq c ?&)  (push "\\\\&"  out) (pop chars))
         ((eq c ?#)  (push "\\\\#"  out) (pop chars))
         ((eq c ?_)  (push "\\\\_"  out) (pop chars))
         ;; en/em dash and curly quotes come from ligatures
         ((eq c ?\u2013) (push "--"  out) (pop chars))
         ((eq c ?\u2014) (push "---" out) (pop chars))
         ((memq c '(?\u201c ?\u201d)) (push "\\(?:``\\|''\\|\"\\)" out) (pop chars))
         ((memq c '(?\u2018 ?\u2019)) (push "[`']" out) (pop chars))
         (t (push (regexp-quote (char-to-string c)) out) (pop chars)))))
    (apply #'concat (nreverse out))))

(defun latex-wysiwyg--blank-or-comment-p ()
  "Non-nil if the current line is blank or contains only a TeX comment."
  (looking-at-p "[ \t]*\\(?:%\\|$\\)"))

(defun latex-wysiwyg--paragraph-bounds ()
  "Bounds of the paragraph containing point, as (START . END).

A paragraph is the maximal run of lines that are neither blank nor
comment-only. Emacs's own `backward-paragraph' is wrong here for two reasons:
called from the exact first character of a paragraph it overshoots into the
previous one, and it treats a lone `% [tag]' comment line as paragraph text.
intro-generated.tex puts exactly such a tag before every paragraph, so the
derived region was consistently one paragraph too early -- which surfaced as
`literal-not-found' on 71 of 122 paragraphs."
  (save-excursion
    (beginning-of-line)
    ;; An anchor may land on the comment or blank line just above its text.
    (while (and (not (eobp)) (latex-wysiwyg--blank-or-comment-p))
      (forward-line 1))
    (let ((start (progn
                   (while (and (not (bobp))
                               (save-excursion
                                 (forward-line -1)
                                 (not (latex-wysiwyg--blank-or-comment-p))))
                     (forward-line -1))
                   (point)))
          (end (progn
                 (while (and (not (eobp))
                             (save-excursion
                               (forward-line 1)
                               (and (not (eobp))
                                    (not (latex-wysiwyg--blank-or-comment-p)))))
                   (forward-line 1))
                 (line-end-position))))
      (cons start end))))

(cl-defun latex-wysiwyg--edit (index line col old new)
  "Replace OLD with NEW in the paragraph anchored at INDEX/LINE/COL."
  (let ((file (latex-wysiwyg--file-for index)))
    (unless latex-wysiwyg-allow-edits
      (cl-return-from latex-wysiwyg--edit
        (latex-wysiwyg--refuse-edit "edits-disarmed" index)))
    (when (latex-wysiwyg-stale-sources)
      (cl-return-from latex-wysiwyg--edit
        (latex-wysiwyg--refuse-edit "page-stale" index)))
    (unless (and file (file-exists-p file))
      (cl-return-from latex-wysiwyg--edit
        (latex-wysiwyg--refuse-edit "unmapped-source" index)))
    (unless (and (stringp old) (stringp new) (> (length (string-trim old)) 0))
      (cl-return-from latex-wysiwyg--edit
        (latex-wysiwyg--refuse-edit "empty-run" index)))
    (with-current-buffer (latex-wysiwyg--fresh-buffer file)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column (max 0 (1- col)))
        (let* ((bounds (latex-wysiwyg--paragraph-bounds))
               (rx (latex-wysiwyg--rx-for old))
               (hits '()))
          (save-excursion
            (goto-char (car bounds))
            (while (re-search-forward rx (cdr bounds) t)
              (push (cons (match-beginning 0) (match-end 0)) hits)))
          (cond
           ((null hits)
            (latex-wysiwyg--refuse-edit "quote-not-found" index))
           ((> (length hits) 1)
            (latex-wysiwyg--refuse-edit "quote-ambiguous" index))
           (t
            (let ((beg (caar hits)) (fin (cdar hits)))
              (undo-boundary)
              (goto-char beg)
              (delete-region beg fin)
              (insert new)
              (latex-wysiwyg--refill beg (point))
              (undo-boundary)
              (when latex-wysiwyg-save-after-edit (save-buffer))
              (latex-wysiwyg--refresh-sha file)
              (setq latex-wysiwyg--last-edit
                    (list :applied t
                          :file (file-name-nondirectory file)
                          :line line :from beg :to (+ beg (length new))
                          :old old :new new))
              (latex-wysiwyg--rocket-capture old new file line)
              (latex-wysiwyg--journal
               (list (cons "kind" "run") (cons "applied" t)
                     (cons "file" (file-name-nondirectory file))
                     (cons "line" line)
                     (cons "pairs" (vector (list (cons "old" old)
                                                 (cons "new" new))))))
              (latex-wysiwyg--broadcast
               (list (cons "type" "scope/ack")
                     (cons "file" index) (cons "line" line)))
              latex-wysiwyg--last-edit))))))))

(defun latex-wysiwyg--announce-state ()
  "Tell every client whether edits are armed.
The browser must be able to show this BEFORE the user types: a refusal
after the fact is not a substitute for an honest indicator."
  (latex-wysiwyg--broadcast
   (list (cons "type" "state")
         (cons "armed" (if latex-wysiwyg-allow-edits t :false))
         (cons "saves" (if latex-wysiwyg-save-after-edit t :false)))))

(defvar latex-wysiwyg--attempt nil
  "Text of the edit currently being attempted, for journalling refusals.")

(defun latex-wysiwyg--refuse-edit (reason index)
  "Record and announce a refused edit."
  (setq latex-wysiwyg--last-edit (list :applied nil :reason reason :index index))
  (latex-wysiwyg--journal
   (append (list (cons "applied" :false) (cons "reason" reason)
                 (cons "index" index))
           ;; The refused text is the whole point: a refusal is exactly when
           ;; the author's words exist nowhere else -- the browser reverts and
           ;; nothing reaches disk.
           (if (consp latex-wysiwyg--attempt) latex-wysiwyg--attempt nil)))
  (latex-wysiwyg--broadcast
   (list (cons "type" "scope/reject") (cons "reason" reason) (cons "file" index)))
  (message "latex-wysiwyg: edit refused (%s)" reason)
  latex-wysiwyg--last-edit)

(defun latex-wysiwyg-last-edit ()
  "Return the last edit attempt as a plist. The e2e gate reads this."
  latex-wysiwyg--last-edit)

(defun latex-wysiwyg-arm-edits (&optional off)
  "Arm scope/edit handling; with prefix OFF, disarm."
  (interactive "P")
  (setq latex-wysiwyg-allow-edits (not off))
  (latex-wysiwyg--announce-state)
  (message "latex-wysiwyg: edits %s" (if latex-wysiwyg-allow-edits "ARMED" "disarmed")))



;;; ------------------------------------------------------ inline text macros
;; A macro whose body is plain text (\suppfindingref{6} -> "Supplement~1,
;; Box~6") expands into the rendered paragraph with NO element in the DOM. The
;; browser cannot tell it from literal text, but the source holds the call, so
;; matching the literal fails -- 16 of 122 paragraphs on this paper.
;;
;; Treat such an expansion as an INVISIBLE ATOM: split the literal around it so
;; the call lands in an atom gap and is preserved verbatim. You may edit around
;; a macro expansion, never inside it -- exactly the rule that already governs
;; formulae and citations.

(defvar latex-wysiwyg--text-macros nil
  "List of regexps matching the RENDERED form of plain-text macros.")

(defun latex-wysiwyg--scan-text-macros ()
  "Derive rendered-form detectors from \\newcommand definitions in the sources."
  (let (out)
    (dotimes (i (length (or latex-wysiwyg--file-map [])))
      (let ((f (aref latex-wysiwyg--file-map i)))
        (when (and f (file-exists-p f))
          (with-temp-buffer
            (insert-file-contents f)
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\\\newcommand{\\\\\\([a-zA-Z]+\\)}\\(?:\\[\\([0-9]\\)\\]\\)?{\\([^\n]*\\)}[ \t]*$"
                    nil t)
              (let ((body (match-string 3)))
                ;; plain text only: no nested macro that would yield an element
                (when (and (not (string-match-p "\\\\\\(footnote\\|emph\\|textbf\\|ref\\|cite\\)" body))
                           (string-match-p "[A-Za-z]" body)
                           (< (length body) 120))
                  (let ((rx (regexp-quote body)))
                    ;; ~ renders as a space; #n stands for an argument
                    (setq rx (replace-regexp-in-string "~" " " rx t t))
                    ;; A trailing #n has no following text to stop a lazy
                    ;; match, so ".+?" matched a single character: "Box~#1"
                    ;; consumed "Box 1" of "Box 10" and stranded the "0".
                    ;; Bound it by delimiters instead; interior arguments keep
                    ;; the lazy form, which the following literal text stops.
                    (setq rx (replace-regexp-in-string
                              "#[0-9]\\'" "[^] \t\n,.;:)]+" rx t t))
                    (setq rx (replace-regexp-in-string "#[0-9]" ".+?" rx t t))
                    (push rx out)))))))))
    (setq latex-wysiwyg--text-macros (delete-dups (nreverse out)))))

(defun latex-wysiwyg--split-macro-text (lits)
  "Split each literal in LITS around any rendered text-macro expansion."
  (if (null latex-wysiwyg--text-macros)
      lits
    (let ((big (concat "\\(?:" (mapconcat #'identity latex-wysiwyg--text-macros "\\|") "\\)"))
          out)
      (dolist (lit lits)
        (let ((start 0))
          (while (string-match big lit start)
            (let ((before (substring lit start (match-beginning 0))))
              (when (string-match-p "[^ \t\n]" before) (push before out)))
            (setq start (match-end 0)))
          (let ((tail (substring lit start)))
            (when (string-match-p "[^ \t\n]" tail) (push tail out)))))
      (nreverse out))))


;;; ------------------------------------------------------- atom attribution
;; Editing or deleting a formula needs to know WHICH source span holds it.
;; Two things make that non-obvious: two atoms can sit adjacent with no text
;; between them (one gap, two atoms -- unattributable), and a plain-text macro
;; splits a literal and adds a gap with no DOM atom at all.
;;
;; So build the slot sequence explicitly, keeping provenance: walk the client
;; template, splitting literals at macro expansions and marking each resulting
;; gap `dom' or `macro'. Decompose with the literals; the source gaps then
;; correspond, in order, to RUNS of gap slots. A gap is attributable to a
;; single formula exactly when its run is one `dom' slot and nothing else.
;;
;; Both operations take the UNCHANGED template plus an index. That sidesteps
;; the merged-literal problem: when a browser deletes an atom it also merges
;; the two literals around it, and realigning those is guesswork.

(defun latex-wysiwyg--slots (template)
  "Slot list for TEMPLATE: (lit TEXT), (gap dom N) or (gap macro)."
  (let ((slots '()) (n -1))
    (dolist (e (append template nil) (nreverse slots))
      (if (equal (cdr (assq 'k e)) "atom")
          (progn (setq n (1+ n)) (push (list 'gap 'dom n) slots))
        (let ((pieces (latex-wysiwyg--split-macro-text
                       (list (or (cdr (assq 't e)) "")))))
          (let ((first t))
            (dolist (piece pieces)
              (unless first (push (list 'gap 'macro) slots))
              (setq first nil)
              (push (list 'lit piece) slots))))))))

(defun latex-wysiwyg--gap-runs (slots)
  "Runs of gap slots, in order: one entry per source gap between literals.
Each entry is the list of gap slots occupying that gap."
  (let ((runs '()) (cur '()) (seen-lit nil))
    (dolist (sl slots)
      (if (eq (car sl) 'lit)
          (progn (when seen-lit (push (nreverse cur) runs))
                 (setq cur '() seen-lit t))
        (push sl cur)))
    (when seen-lit (push (nreverse cur) runs))
    (nreverse runs)))

(defun latex-wysiwyg--gap-for-atom (slots atom-index)
  "Index of the source gap holding DOM atom ATOM-INDEX, or nil if ambiguous.
Gap i is the source between literal i-1 and literal i, matching the ordering
`latex-wysiwyg--decompose' produces."
  (let ((runs (latex-wysiwyg--gap-runs slots)) (i 0) (found nil))
    (dolist (run runs found)
      (setq i (1+ i))
      (when (and (= (length run) 1)
                 (eq (nth 1 (car run)) 'dom)
                 (equal (nth 2 (car run)) atom-index))
        (setq found i)))))

(cl-defun latex-wysiwyg--atom-op (index line col template atom-index op tex)
  "Delete or retex DOM atom ATOM-INDEX in a paragraph. OP is `delete' or `retex'."
  (let ((file (latex-wysiwyg--file-for index)))
    (unless latex-wysiwyg-allow-edits
      (cl-return-from latex-wysiwyg--atom-op
        (latex-wysiwyg--refuse-edit "edits-disarmed" index)))
    (unless (and file (file-exists-p file))
      (cl-return-from latex-wysiwyg--atom-op
        (latex-wysiwyg--refuse-edit "unmapped-source" index)))
    (when (latex-wysiwyg-stale-sources)
      (cl-return-from latex-wysiwyg--atom-op
        (latex-wysiwyg--refuse-edit "page-stale" index)))
    (with-current-buffer (latex-wysiwyg--fresh-buffer file)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column (max 0 (1- col)))
        (let* ((b (latex-wysiwyg--paragraph-bounds))
               (beg (car b)) (fin (cdr b))
               (original (buffer-substring-no-properties beg fin))
               (slots (latex-wysiwyg--slots template))
               (lits (delq nil (mapcar (lambda (sl) (and (eq (car sl) 'lit) (nth 1 sl)))
                                       slots)))
               (parts (latex-wysiwyg--decompose beg fin lits))
               (gap-i (latex-wysiwyg--gap-for-atom slots atom-index)))
          (unless parts
            (cl-return-from latex-wysiwyg--atom-op
              (latex-wysiwyg--refuse-edit "literal-not-found" index)))
          (unless gap-i
            (cl-return-from latex-wysiwyg--atom-op
              (latex-wysiwyg--refuse-edit "atom-not-attributable" index)))
          (let* ((spans (car parts))
                 (atoms (cdr parts))
                 (olds (mapcar (lambda (sp) (buffer-substring-no-properties
                                             (car sp) (cdr sp))) spans))
                 (roundtrip (let ((acc "") (a (copy-sequence atoms)) (l (copy-sequence olds)))
                              (while (or a l)
                                (when a (setq acc (concat acc (pop a))))
                                (when l (setq acc (concat acc (pop l)))))
                              acc)))
            (unless (equal roundtrip original)
              (cl-return-from latex-wysiwyg--atom-op
                (latex-wysiwyg--refuse-edit "decomposition-unsound" index)))
            (let* ((removed (nth gap-i atoms))
                   (replacement
                    (cond ((eq op 'delete) "")
                          (t (latex-wysiwyg--retex removed tex))))
                   (new-atoms (let ((copy (copy-sequence atoms)))
                                (setf (nth gap-i copy) replacement) copy))
                   (rebuilt (let ((acc "") (a new-atoms) (l (copy-sequence olds)))
                              (while (or a l)
                                (when a (setq acc (concat acc (pop a))))
                                (when l (setq acc (concat acc (pop l)))))
                              acc)))
              (undo-boundary)
              (goto-char beg)
              (delete-region beg fin)
              (insert rebuilt)
              (latex-wysiwyg--refill beg (point))
              (undo-boundary)
              (when latex-wysiwyg-save-after-edit (save-buffer))
              (latex-wysiwyg--refresh-sha file)
              ;; Retention, per Arxana: an edited-out formula is kept in the
              ;; journal so the removal is recoverable and reviewable, not
              ;; merely gone.
              (latex-wysiwyg--journal
               (list (cons "kind" (if (eq op 'delete) "atom-delete" "atom-retex"))
                     (cons "applied" t)
                     (cons "file" (file-name-nondirectory file))
                     (cons "line" line)
                     (cons "pairs" (vector (list (cons "old" removed)
                                                 (cons "new" replacement))))))
              (setq latex-wysiwyg--last-edit
                    (list :applied t :op op :file (file-name-nondirectory file)
                          :line line :removed removed :inserted replacement))
              (latex-wysiwyg--broadcast
               (list (cons "type" "scope/ack") (cons "file" index) (cons "line" line)))
              latex-wysiwyg--last-edit)))))))

(defun latex-wysiwyg--retex (old tex)
  "Rewrite math source OLD to hold TEX, keeping OLD's delimiters."
  (cond
   ((string-match "\\`\\(\\s-*\\)\\$\\(.*\\)\\$\\(\\s-*\\)\\'" old)
    (concat (match-string 1 old) "$" tex "$" (match-string 3 old)))
   ((string-match "\\`\\(\\s-*\\)\\\\(\\(.*\\)\\\\)\\(\\s-*\\)\\'" old)
    (concat (match-string 1 old) "\\(" tex "\\)" (match-string 3 old)))
   (t (concat "$" tex "$"))))

;;; --------------------------------------------------- template (paragraph)
;; S3: paragraph-level editing over a TEMPLATE of literals and opaque atoms.
;;
;; A paragraph renders as  L1 A1 L2 A2 L3 ...  where Li are literal text runs
;; and Ai are opaque atoms (math, citations, cross-references, macro output).
;; Run-level editing fragmented this into micro-regions -- a four-letter word
;; after a formula was below the threshold and silently uneditable.
;;
;; Atom source bounds are NOT taken from their own data-sourcepos: measured on
;; draft8, only 5% of <math> ranges actually bracket their TeX (the starts are
;; wrong). Instead the LITERALS bracket the ATOMS: match L1, L2, ... in order,
;; and whatever lies between consecutive literal matches is that atom's source,
;; preserved verbatim.
;;
;; Safety: after decomposing, we REASSEMBLE the original literals and atoms and
;; require the result to equal the paragraph source exactly. Only a provably
;; lossless decomposition is allowed to write. That check is the guarantee.

(defun latex-wysiwyg--decompose (beg end lits)
  "Split the source between BEG and END using ordered literal strings LITS.
Returns (LIT-SPANS . ATOM-STRINGS) or nil if any literal cannot be placed."
  (save-excursion
    (let ((spans '()) (atoms '()) (pos beg) (ok t))
      (dolist (lit lits)
        (when ok
          (goto-char pos)
          (if (re-search-forward (latex-wysiwyg--rx-for lit) end t)
              (progn
                (push (buffer-substring-no-properties pos (match-beginning 0)) atoms)
                (push (cons (match-beginning 0) (match-end 0)) spans)
                (setq pos (match-end 0)))
            (setq ok nil))))
      (when ok
        (push (buffer-substring-no-properties pos end) atoms)
        (cons (nreverse spans) (nreverse atoms))))))

(defun latex-wysiwyg--unplaceable (beg end lits)
  "Return the literals from LITS that cannot be matched in order in BEG..END."
  (save-excursion
    (let ((pos beg) (bad '()))
      (dolist (lit lits)
        (goto-char pos)
        (if (re-search-forward (latex-wysiwyg--rx-for lit) end t)
            (setq pos (match-end 0))
          (push lit bad)))
      (nreverse bad))))

(defun latex-wysiwyg-audit-decompose (json-file)
  "Audit paragraph decomposition for every entry in JSON-FILE.
Each entry is {file,line,col,lits}. Reports how many paragraphs decompose and
what the unplaceable literals look like -- the coverage number for S3."
  (let* ((json-array-type 'list) (json-object-type 'alist)
         (rows (json-read-file json-file))
         (ok 0) (bad 0) (samples '()))
    (dolist (r rows)
      (let* ((idx (alist-get 'file r))
             (line (alist-get 'line r))
             (col (alist-get 'col r))
             (lits (append (alist-get 'lits r) nil))
             (f (latex-wysiwyg--file-for idx)))
        (when (and f (file-exists-p f))
          (with-current-buffer (find-file-noselect f)
            (save-excursion
              (goto-char (point-min)) (forward-line (1- line))
              (move-to-column (max 0 (1- col)))
              (let* ((b (latex-wysiwyg--paragraph-bounds))
                     (miss (latex-wysiwyg--unplaceable
                            (car b) (cdr b)
                            (latex-wysiwyg--split-macro-text lits))))
                (if miss
                    (progn (setq bad (1+ bad))
                           (when (< (length samples) 6)
                             (push (list line (car miss)) samples)))
                  (setq ok (1+ ok)))))))))
    (list :decomposed ok :failed bad :samples (nreverse samples))))


(defun latex-wysiwyg--subsequence-p (small big)
  "Non-nil if SMALL appears in BIG in order (deletions only, no reordering)."
  ;; NB (cdr (member x b)) is nil both when X is absent AND when X is the last
  ;; element, so testing the tail conflated "no match" with "matched at the
  ;; end" -- every paragraph whose final atom survived was reported reordered.
  (let ((b big))
    (catch 'no
      (dolist (x small t)
        (let ((m (member x b)))
          (unless m (throw 'no nil))
          (setq b (cdr m)))))))

(defun latex-wysiwyg--rebuild-from (template atom-src)
  "Rebuild paragraph source from TEMPLATE using ATOM-SRC for surviving atoms.
TEMPLATE is the new client template; ATOM-SRC is a list of source strings for
the atoms it still contains, in order."
  (let ((acc "") (srcs atom-src))
    (dolist (e (append template nil) acc)
      (if (equal (cdr (assq 'k e)) "atom")
          (setq acc (concat acc (or (pop srcs) "")))
        (setq acc (concat acc (or (cdr (assq 't e)) "")))))))

(cl-defun latex-wysiwyg--edit-template (index line col old new)
  "Rewrite a paragraph from its OLD template to its NEW one.
OLD and NEW are vectors of alists with `k' (\"lit\" or \"atom\") and `t'."
  (let ((file (latex-wysiwyg--file-for index)))
    (unless latex-wysiwyg-allow-edits
      (cl-return-from latex-wysiwyg--edit-template
        (latex-wysiwyg--refuse-edit "edits-disarmed" index)))
    (when (latex-wysiwyg-stale-sources)
      (cl-return-from latex-wysiwyg--edit-template
        (latex-wysiwyg--refuse-edit "page-stale" index)))
    (unless (and file (file-exists-p file))
      (cl-return-from latex-wysiwyg--edit-template
        (latex-wysiwyg--refuse-edit "unmapped-source" index)))
    (let* ((seq (lambda (v) (mapcar (lambda (e) (cdr (assq 'k e))) (append v nil))))
           (lit (lambda (v) (delq nil (mapcar
                                       (lambda (e) (and (equal (cdr (assq 'k e)) "lit")
                                                        (cdr (assq 't e))))
                                       (append v nil))))))
      ;; Deleting a formula or citation is a LEGITIMATE edit -- refusing it is
      ;; what Arxana's retraction work calls silent retreat (Lakatos monster
      ;; barring: the move is allowed, but it must be visible). So we permit
      ;; deletions, identify exactly which atoms went, and retain their source
      ;; verbatim in the journal. What we still refuse is reordering or
      ;; inventing atoms, where no honest alignment exists.
      (let* ((atext (lambda (v) (delq nil (mapcar
                                  (lambda (e) (and (equal (cdr (assq 'k e)) "atom")
                                                   (cdr (assq 't e))))
                                  (append v nil)))))
             (oa (funcall atext old)) (na (funcall atext new)))
        (unless (latex-wysiwyg--subsequence-p na oa)
          (cl-return-from latex-wysiwyg--edit-template
            (latex-wysiwyg--refuse-edit "atoms-reordered" index)))
        ;; SAFETY GATE. Permitting the deletion at the check is not the same as
        ;; rebuilding correctly afterwards: the source gap that held the atom
        ;; has to be attributed and dropped, and gaps are ambiguous when two
        ;; atoms are adjacent or when an invisible macro-text split sits
        ;; between them. Until that attribution is implemented and tested,
        ;; refuse -- but journal the attempt, so the text is not lost.
        (unless (= (length na) (length oa))
          (cl-return-from latex-wysiwyg--edit-template
            (latex-wysiwyg--refuse-edit "deletion-not-yet-supported" index))))
      (with-current-buffer (latex-wysiwyg--fresh-buffer file)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column (max 0 (1- col)))
          (let* ((b (latex-wysiwyg--paragraph-bounds))
                 (beg (car b)) (fin (cdr b))
                 (original (buffer-substring-no-properties beg fin))
                 (parts (latex-wysiwyg--decompose
                         beg fin (latex-wysiwyg--split-macro-text (funcall lit old)))))
            (unless parts
              (let ((latex-wysiwyg--attempt
                     (format "could not place: %S"
                             (car (latex-wysiwyg--unplaceable beg fin (funcall lit old))))))
                (message "latex-wysiwyg: %s" latex-wysiwyg--attempt)
                (cl-return-from latex-wysiwyg--edit-template
                  (latex-wysiwyg--refuse-edit "literal-not-found" index))))
            (let* ((spans (car parts))
                   (atoms (cdr parts))
                   (olds (mapcar (lambda (sp) (buffer-substring-no-properties
                                               (car sp) (cdr sp))) spans))
                   ;; PROOF: reassembling the pieces must reproduce the source.
                   (roundtrip
                    (let ((acc "") (a atoms) (l olds))
                      (while (or a l)
                        (when a (setq acc (concat acc (pop a))))
                        (when l (setq acc (concat acc (pop l)))))
                      acc)))
              (unless (equal roundtrip original)
                (cl-return-from latex-wysiwyg--edit-template
                  (latex-wysiwyg--refuse-edit "decomposition-unsound" index)))
              (let* ((news (latex-wysiwyg--split-macro-text (funcall lit new)))
                     (_ (unless (= (length news) (length olds))
                          (cl-return-from latex-wysiwyg--edit-template
                            (latex-wysiwyg--refuse-edit "macro-text-edited" index))))
                     (rebuilt
                      (let ((acc "") (a atoms) (l news))
                        (while (or a l)
                          (when a (setq acc (concat acc (pop a))))
                          (when l (setq acc (concat acc (pop l)))))
                        acc)))
                (if (equal rebuilt original)
                    (setq latex-wysiwyg--last-edit
                          (list :applied nil :reason "no-change" :index index))
                  (undo-boundary)
                  (goto-char beg)
                  (delete-region beg fin)
                  (insert rebuilt)
                  (latex-wysiwyg--refill beg (point))
                  (undo-boundary)
                  (when latex-wysiwyg-save-after-edit (save-buffer))
                  (latex-wysiwyg--refresh-sha file)
                  (setq latex-wysiwyg--last-edit
                        (list :applied t :file (file-name-nondirectory file)
                              :line line :atoms (length atoms)
                              :literals (length news)))
                  (latex-wysiwyg--journal
                   (list (cons "kind" "paragraph") (cons "applied" t)
                         (cons "file" (file-name-nondirectory file))
                         (cons "line" line)
                         (cons "atoms" (length atoms))
                         (cons "pairs" (latex-wysiwyg--changed-pairs olds news))))
                  (latex-wysiwyg--broadcast
                   (list (cons "type" "scope/ack") (cons "file" index)
                         (cons "line" line))))
                latex-wysiwyg--last-edit))))))))



;;; ------------------------------------------------------------- journalling
;; Every edit attempt is appended to a journal, applied or refused, with BOTH
;; the old and the new text. Two reasons:
;;
;;   1. Mining. The (old, new) pairs are a corpus of the author's own rewriting
;;      operations -- what actually gets changed, and into what.
;;   2. Recovery. Edits made through the browser have no other trace: they land
;;      in a buffer and are saved. On 2026-08-08 a test harness restored a
;;      snapshot over a session's worth of them and nothing could be recovered.
;;      A journal makes that a replayable inconvenience instead of a loss.
;;
;; Refusals are journalled too, and are arguably the more interesting half:
;; they record edits the author WANTED to make that the system could not.
;;
;; JSONL, one record per line: readable from Python, Clojure and Emacs alike,
;; and appendable without rewriting the file.

(defcustom latex-wysiwyg-journal-file nil
  "Where to append the edit journal. Defaults to wysiwyg-edits.jsonl
next to the first mapped source file."
  :type '(choice (const nil) file) :group 'latex-wysiwyg)

(defun latex-wysiwyg--journal-path ()
  (or latex-wysiwyg-journal-file
      (let ((f (and latex-wysiwyg--file-map
                    (> (length latex-wysiwyg--file-map) 0)
                    (aref latex-wysiwyg--file-map 0))))
        (and f (expand-file-name "wysiwyg-edits.jsonl"
                                 (file-name-directory f))))))

(defun latex-wysiwyg--journal (record)
  "Append RECORD (an alist) to the edit journal."
  (let ((path (latex-wysiwyg--journal-path)))
    (when path
      (condition-case err
          (let ((line (json-encode
                       (append
                        (list (cons "ts" (format-time-string "%FT%T%z"))
                              (cons "session" (emacs-pid)))
                        record))))
            (write-region (concat line "\n") nil path t 'silent))
        (error (message "latex-wysiwyg: journal write failed: %s"
                        (error-message-string err)))))))

(defun latex-wysiwyg--changed-pairs (olds news)
  "Pairs of (old . new) literals that actually differ, as a vector.
This is the mining unit: one rewriting operation per pair."
  (let ((out '()))
    (while (or olds news)
      (let ((o (pop olds)) (n (pop news)))
        (unless (equal o n)
          (push (list (cons "old" (or o "")) (cons "new" (or n ""))) out))))
    (vconcat (nreverse out))))

;;; ------------------------------------------------- single-writer discipline
;; Emacs must be the ONLY writer of the mapped sources. Anything that changes a
;; file behind Emacs's back produces "File X changed on disk. Reread from disk?
;; (y or n)" -- a MODAL prompt that blocks the whole daemon, including the
;; websocket server, so the editor deadlocks against its own test harness.
;; (Learned the hard way 2026-08-08: a test's out-of-band restore both blocked
;; Emacs and discarded live edits.)
;;
;; Two measures: hot-reload mapped buffers when they change externally and are
;; unmodified, and provide an Emacs-mediated restore so nothing else needs to
;; touch the file.

(defun latex-wysiwyg--enable-autorevert ()
  "Hot-reload mapped buffers on external change, instead of prompting.
Only unmodified buffers auto-revert; a buffer with unsaved edits is left alone
and Emacs still warns, which is the behaviour we want."
  (require 'autorevert)
  (dotimes (i (length (or latex-wysiwyg--file-map [])))
    (let ((f (aref latex-wysiwyg--file-map i)))
      (when (and f (file-exists-p f))
        (with-current-buffer (find-file-noselect f)
          (setq-local auto-revert-verbose nil)
          (auto-revert-mode 1))))))

(defun latex-wysiwyg-restore-file (file backup)
  "Restore FILE from BACKUP *through Emacs*, so it stays the single writer.
Uses the buffer, so the restore is a normal undoable change and no
changed-on-disk prompt is ever raised."
  (interactive "fFile: \nfBackup: ")
  (with-current-buffer (latex-wysiwyg--fresh-buffer file)
    (let ((inhibit-read-only t))
      (undo-boundary)
      (erase-buffer)
      (insert-file-contents backup)
      (save-buffer)
      (undo-boundary))
    ;; A restore is an Emacs write like any other: re-record the hash, or the
    ;; staleness guard fires on our own action and refuses every later edit.
    (latex-wysiwyg--refresh-sha file)
    (list :restored (file-name-nondirectory file)
          :bytes (buffer-size))))


;;; ------------------------------------------------------------- re-snapping
;; The page is a snapshot: when a source or a figure changes it silently shows
;; the old text (and the old figure) while Emacs refuses edits as `page-stale'.
;; Watch the sources and the figures directory and re-snap on change.
;;
;; Everything here is asynchronous. A synchronous rebuild would block the
;; editor for the length of a conversion, and putting slow work on Emacs's
;; critical path is the mistake that wedged this session once already.

(defcustom latex-wysiwyg-rebuild-script nil
  "Path to rebuild-page.sh. Set by `latex-wysiwyg-watch'."
  :type '(choice (const nil) file) :group 'latex-wysiwyg)

(defvar latex-wysiwyg--watches nil "Active file-notify descriptors.")
(defvar latex-wysiwyg--rebuild-timer nil)
(defvar latex-wysiwyg--rebuilding nil)
(defvar latex-wysiwyg--scopes-file nil "scopes.json this session was started from.")

(defun latex-wysiwyg--rebuild-now ()
  "Run the rebuild script asynchronously, then reload the map and tell the page."
  (unless latex-wysiwyg--rebuilding
    (setq latex-wysiwyg--rebuilding t)
    (let ((buf (get-buffer-create " *latex-wysiwyg-rebuild*")))
      (with-current-buffer buf (erase-buffer))
      (make-process
       :name "latex-wysiwyg-rebuild" :buffer buf :noquery t
       :command (list "bash" latex-wysiwyg-rebuild-script)
       :sentinel
       (lambda (_proc event)
         (setq latex-wysiwyg--rebuilding nil)
         (when (string-prefix-p "finished" event)
           (when latex-wysiwyg--scopes-file
             (latex-wysiwyg-load-map latex-wysiwyg--scopes-file)
             (latex-wysiwyg--scan-text-macros))
           (latex-wysiwyg--broadcast (list (cons "type" "build/done")))
           (message "latex-wysiwyg: page re-snapped%s"
                    (with-current-buffer " *latex-wysiwyg-rebuild*"
                      (if (string-match "(\\([0-9]+\\) errors)" (buffer-string))
                          (format " (%s errors)" (match-string 1 (buffer-string))) ""))))))))) 

(defun latex-wysiwyg--on-change (event)
  "Debounced re-snap when a watched file changes."
  (let ((action (nth 1 event)) (file (nth 2 event)))
    (when (and (memq action '(created changed renamed attribute-changed))
               (stringp file)
               (not (string-match-p "/\\.#\\|~\\'\\|/#" file)))
      (when latex-wysiwyg--rebuild-timer (cancel-timer latex-wysiwyg--rebuild-timer))
      ;; Debounce: a figure re-render or an editor save can touch several files
      ;; in quick succession, and each conversion costs seconds.
      (setq latex-wysiwyg--rebuild-timer
            (run-with-timer 2.0 nil #'latex-wysiwyg--rebuild-now)))))

;;;###autoload
(defun latex-wysiwyg-watch (&optional script)
  "Watch the mapped sources and the figures directory; re-snap on change."
  (interactive)
  (require 'filenotify)
  (latex-wysiwyg-unwatch)
  (when script (setq latex-wysiwyg-rebuild-script script))
  (unless (and latex-wysiwyg-rebuild-script
               (file-exists-p latex-wysiwyg-rebuild-script))
    (user-error "latex-wysiwyg: set `latex-wysiwyg-rebuild-script' first"))
  (let ((targets '()))
    (dotimes (i (length (or latex-wysiwyg--file-map [])))
      (let ((f (aref latex-wysiwyg--file-map i)))
        (when (and f (file-exists-p f)) (push f targets))))
    ;; figures live in a directory: watch the directory, not each file, so a
    ;; newly rendered figure is noticed too
    (let ((figs (expand-file-name
                 "figures" (file-name-directory (or (car targets) default-directory)))))
      (when (file-directory-p figs) (push figs targets)))
    (dolist (tgt (delete-dups targets))
      (push (file-notify-add-watch tgt '(change attribute-change) #'latex-wysiwyg--on-change)
            latex-wysiwyg--watches)))
  (message "latex-wysiwyg: watching %d paths; page re-snaps on change"
           (length latex-wysiwyg--watches))
  latex-wysiwyg--watches)

;;;###autoload
(defun latex-wysiwyg-unwatch ()
  "Stop watching for source changes."
  (interactive)
  (dolist (d latex-wysiwyg--watches) (ignore-errors (file-notify-rm-watch d)))
  (setq latex-wysiwyg--watches nil)
  (when latex-wysiwyg--rebuild-timer
    (cancel-timer latex-wysiwyg--rebuild-timer)
    (setq latex-wysiwyg--rebuild-timer nil)))

;;; ---------------------------------------------------------------- server

;;;###autoload
(defun latex-wysiwyg-start (&optional scopes-json)
  "Start the WYSIWYG websocket server, loading SCOPES-JSON if given."
  (interactive "fscopes.json: ")
  (require 'websocket)
  (when scopes-json
    (setq latex-wysiwyg--scopes-file (expand-file-name scopes-json))
    (latex-wysiwyg-load-map scopes-json))
  (when latex-wysiwyg--server (latex-wysiwyg-stop))
  (setq latex-wysiwyg--clients nil)
  (setq latex-wysiwyg--server
        (websocket-server
         latex-wysiwyg-port
         :host 'local
         :on-open    (lambda (ws) (push ws latex-wysiwyg--clients))
         :on-message #'latex-wysiwyg--on-message
         :on-close   (lambda (ws) (setq latex-wysiwyg--clients
                                        (delq ws latex-wysiwyg--clients)))))
  ;; No post-command-hook: an idle timer cannot tax or wedge the editor.
  (remove-hook 'post-command-hook #'latex-wysiwyg--post-command)
  (latex-wysiwyg-unwatch)
  (when latex-wysiwyg--idle-timer (cancel-timer latex-wysiwyg--idle-timer))
  (setq latex-wysiwyg--idle-timer
        (run-with-idle-timer 0.4 t #'latex-wysiwyg--idle-report))
  (latex-wysiwyg--enable-autorevert)
  (latex-wysiwyg--scan-text-macros)
  (dotimes (i (length (or latex-wysiwyg--file-map [])))
    (let ((f (aref latex-wysiwyg--file-map i)))
      (when (and f (file-exists-p f) (latex-wysiwyg--generated-p f))
        (message "latex-wysiwyg: WARNING %s is GENERATED -- edits there will be lost on re-render"
                 (file-name-nondirectory f)))))
  (let ((stale (latex-wysiwyg-stale-sources)))
    (when stale
      (message "latex-wysiwyg: PAGE IS STALE for %s -- rebuild before editing"
               (mapconcat #'identity stale ", "))))
  (message "latex-wysiwyg: listening on ws://localhost:%d (%d files mapped)"
           latex-wysiwyg-port (length (or latex-wysiwyg--file-map [])))
  latex-wysiwyg--server)

;;;###autoload
(defun latex-wysiwyg-stop ()
  "Stop the WYSIWYG websocket server."
  (interactive)
  (remove-hook 'post-command-hook #'latex-wysiwyg--post-command)
  (latex-wysiwyg-unwatch)
  (when latex-wysiwyg--idle-timer
    (cancel-timer latex-wysiwyg--idle-timer)
    (setq latex-wysiwyg--idle-timer nil))
  (when latex-wysiwyg--server
    (ignore-errors (websocket-server-close latex-wysiwyg--server)))
  (setq latex-wysiwyg--server nil
        latex-wysiwyg--clients nil)
  (message "latex-wysiwyg: stopped"))

(defun latex-wysiwyg-status ()
  "Show what the server currently believes: map, journal, armed state."
  (interactive)
  (let ((st (list :port latex-wysiwyg-port
                  :armed latex-wysiwyg-allow-edits
                  :clients (length latex-wysiwyg--clients)
                  :journal (latex-wysiwyg--journal-path)
                  :files (append latex-wysiwyg--file-map nil))))
    (when (called-interactively-p 'interactive) (message "%S" st))
    st))

(defun latex-wysiwyg-clients-info ()
  "Report connected clients and the script versions they announced."
  (interactive)
  (let ((info (list :clients (length latex-wysiwyg--clients)
                    :hellos (delete-dups (copy-sequence latex-wysiwyg--hellos)))))
    (when (called-interactively-p 'interactive) (message "%S" info))
    info))

(defun latex-wysiwyg-last-jump ()
  "Return the most recent jump as a plist. The e2e gate calls this."
  latex-wysiwyg--last-jump)

(provide 'latex-wysiwyg)
;;; latex-wysiwyg.el ends here
