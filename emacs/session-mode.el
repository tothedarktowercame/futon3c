;;; session-mode.el --- Deterministic NNexus-style markup of the live session buffer -*- lexical-binding: t; -*-

;; The third member of the control-layer mode-family (with mission-mode + session-overview).
;; Where session-overview reflows a session against the HEAVY mined runs, session-mode marks up the LIVE
;; buffer with the DETERMINISTIC spotter (M-points-de-fuite's "light annotation": parse a grammar, not a
;; model).  It recognizes symbolic acts already in the flow against the CONTROLLED vocabulary that
;; `session_typology.py' emits (spot-vocab.json + typology.json), and colours each by its determinism TIER:
;;
;;   explicit   — !{A -> B : op} mint signs, futonic glyphs (verbatim — certain)
;;   recognized — mission-clock / mission-mention / pattern-ref (controlled-vocab match — deterministic)
;;   cued       — correction / reach (the light lexicons — CANDIDATES, shown as wavy underlines)
;;
;; The clock vs mention split is the M-autoclock-in rule made visible: the FIRST on-disk mission named in
;; the buffer is the clocked one (teal); every other on-disk mission is mentioned-but-not-clocked (slate).
;; That is Joe's ask — "a correction in one colour, a mission mentioned-but-not-clocked in another" — done
;; deterministically, so it is reproducible and cheap (no model, no paid pass).
;;
;; Usage:  M-x session-mode  in any conversation buffer (e.g. *claude-repl:claude-1*).
;; Keys (C-c s …):  C-c s g refresh · C-c s c toggle correction/reach cue markers.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'agent-chat nil t)   ; for agent-chat--session-id / agent-chat--on-turn-end (soft)

(defgroup session-mode nil
  "Deterministic NNexus-style markup of the live session buffer."
  :group 'convenience)

(defcustom session-mode-vocab-file
  "/home/joe/code/futon6/data/c-vector/spot-vocab.json"
  "Controlled-vocabulary file (missions + patterns) emitted by session_typology.py."
  :type 'string :group 'session-mode)

(defcustom session-mode-show-cues t
  "When non-nil, also mark the CUED tier (correction/reach cue phrases) as wavy underlines.
These are low-precision CANDIDATES (the mission's override layer is for exactly these), so they
are styled faintly to distinguish them from the deterministic recognized/explicit tiers."
  :type 'boolean :group 'session-mode)

(defcustom session-mode-show-reach-cues nil
  "When non-nil, also mark reach cues (\"let's\", \"we could\", …).  Off by default — the reach
lexicon fires on nearly every turn, so it is the noisiest candidate layer."
  :type 'boolean :group 'session-mode)

;; --- Faces, keyed by typology tier/type (colours mirror typology.json) ---
(defface session-mode-clock-face
  '((((background light)) :background "#cdeee9" :weight bold)
    (((background dark)) :background "#0f3d38" :weight bold))
  "mission-clock: the first on-disk mission named — the session is clocked into it (teal)."
  :group 'session-mode)
(defface session-mode-mention-face
  '((((background light)) :background "#e2e8f0")
    (((background dark)) :background "#2b3138"))
  "mission-mention: an on-disk mission named but NOT clocked into (slate)." :group 'session-mode)
(defface session-mode-pattern-face
  '((t :foreground "#7c3aed" :underline t))
  "pattern-ref: a *.flexiarg pattern named (violet)." :group 'session-mode)
(defface session-mode-mint-face
  '((t :background "#16a34a" :foreground "white" :weight bold))
  "dsl-mint: an explicit !{A -> B : op} mint sign (green — the writable hidden layer)."
  :group 'session-mode)
(defface session-mode-glyph-face
  '((t :foreground "#b45309" :weight bold))
  "Explicit futonic glyph (香/應/…)." :group 'session-mode)
(defface session-mode-correction-face
  '((t :underline (:color "#b45309" :style wave)))
  "correction cue (CANDIDATE — amber wave)." :group 'session-mode)
(defface session-mode-reach-face
  '((t :underline (:color "#7c5cff" :style wave)))
  "reach cue (CANDIDATE — lavender wave)." :group 'session-mode)
(defface session-mode-sigil-face
  '((t :inherit font-lock-comment-face :slant italic))
  "The per-turn sigil appended after a \"Cooked for …\" line." :group 'session-mode)

;; --- Controlled vocabulary (loaded once, cached) ---
(defvar session-mode--missions nil "Hash set of on-disk mission/excursion names.")
(defvar session-mode--patterns nil "Hash set of on-disk pattern (flexiarg) names.")
(defvar session-mode--typology nil "type -> alist(glyph tier recognizer colour), from typology.json.")

(defcustom session-mode-typology-file
  "/home/joe/code/futon6/data/c-vector/typology.json"
  "The controlled typology (act-types + determinism tiers) emitted by session_typology.py."
  :type 'string :group 'session-mode)

(defun session-mode--load-typology ()
  "Load typology.json into `session-mode--typology' (idempotent)."
  (when (and (null session-mode--typology) (file-readable-p session-mode-typology-file))
    (let ((json-array-type 'list)
          (h (make-hash-table :test 'equal)))
      (dolist (ty (alist-get 'types (json-read-file session-mode-typology-file)))
        (puthash (alist-get 'type ty) ty h))
      (setq session-mode--typology h)))
  session-mode--typology)

(defun session-mode--load-vocab ()
  "Load the controlled vocabulary into hash sets (idempotent; returns non-nil on success)."
  (when (and (null session-mode--missions) (file-readable-p session-mode-vocab-file))
    (let* ((json-array-type 'list)
           (v (json-read-file session-mode-vocab-file))
           (mh (make-hash-table :test 'equal :size 600))
           (ph (make-hash-table :test 'equal :size 1200)))
      (dolist (m (alist-get 'missions v)) (puthash m t mh))
      (dolist (p (alist-get 'patterns v)) (puthash p t ph))
      ;; The vocab file is M-only; also recognise on-disk EXCURSIONS (E-) and
      ;; CAMPAIGNS (C-) so they highlight/comb the same way missions do.
      (dolist (f (append (file-expand-wildcards "/home/joe/code/*/holes/[MEC]-*.md")
                         (file-expand-wildcards "/home/joe/code/*/holes/*/[MEC]-*.md")))
        (puthash (file-name-base f) t mh))
      (setq session-mode--missions mh session-mode--patterns ph)))
  session-mode--missions)

;; --- The AUTHORITATIVE per-turn sigil source: each A→B turn is mapped to patterns by
;; embedding retrieval (futon3a), stored live as `context-retrieval` evidence in XTDB.  We read
;; that back per turn and resolve the top pattern's two-part sigil (okipona word + truth hanzi)
;; from patterns-index.tsv.  This is the real "grab the sigil off the tag as it flies by" — NOT
;; the deterministic control-glyph summary (that was the wrong artifact). ---

(defcustom session-mode-pattern-index-file
  "/home/joe/code/futon3/resources/sigils/patterns-index.tsv"
  "TSV: pattern \\t okipona \\t truth(hanzi) \\t rationale \\t hotwords — the pattern→sigil map."
  :type 'string :group 'session-mode)

(defcustom session-mode-api-base "http://localhost:7070"
  "Base URL of the futon3c API serving the evidence store."
  :type 'string :group 'session-mode)

(defvar session-mode--pattern-sigils nil
  "Hash pattern-id -> (okipona . truth), from patterns-index.tsv.")
(defvar-local session-mode--turn-sigils nil
  "Alist (query-prefix-normalized . sigil-display) for this session's turns, from XTDB.")

(defun session-mode--load-pattern-sigils ()
  "Load pattern-id -> (okipona . truth) sigils from the index TSV (idempotent)."
  (when (and (null session-mode--pattern-sigils)
             (file-readable-p session-mode-pattern-index-file))
    (let ((h (make-hash-table :test 'equal :size 1400)))
      (with-temp-buffer
        (insert-file-contents session-mode-pattern-index-file)
        (goto-char (point-min))
        (while (not (eobp))
          (unless (eq (char-after) ?#)
            (let ((cols (split-string (buffer-substring-no-properties
                                       (line-beginning-position) (line-end-position))
                                      "\t")))
              (when (and (car cols) (> (length (car cols)) 0))
                (puthash (nth 0 cols) (cons (or (nth 1 cols) "") (or (nth 2 cols) "")) h))))
          (forward-line 1)))
      (setq session-mode--pattern-sigils h)))
  session-mode--pattern-sigils)

(defun session-mode--norm (s)
  (downcase (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or s "")))))

(defun session-mode--sigil-for-pattern (pid)
  "The display sigil for pattern-id PID: «truth okipona» collection/name.
Keep the FULL pid (with collection) — distinct patterns can share a short name
\(e.g. futon-theory/rapid-debugging vs storage/rapid-debugging) with DIFFERENT sigils,
so dropping the collection makes two correct, different sigils look contradictory."
  (let* ((s (and session-mode--pattern-sigils (gethash pid session-mode--pattern-sigils)))
         (okipona (and s (car s))) (truth (and s (cdr s))))
    (concat (when (and truth (> (length truth) 0)) truth)
            (when (and okipona (> (length okipona) 0)) (concat " " okipona))
            (when (or (and truth (> (length truth) 0)) (and okipona (> (length okipona) 0))) " ")
            "· " pid)))

(defun session-mode--fetch-turn-sigils ()
  "Fetch this session's per-turn pattern retrievals from XTDB and build the
query→sigil alist.  Session id is the buffer-local `agent-chat--session-id'."
  (let ((sid (and (boundp 'agent-chat--session-id) (stringp agent-chat--session-id)
                  agent-chat--session-id))
        (result nil))
    (when sid
      (session-mode--load-pattern-sigils)
      ;; Build the alist inside the temp buffer, but ASSIGN the buffer-local var
      ;; back in THIS buffer (the temp buffer's local would be discarded).
      (setq result
            (with-temp-buffer
              (when (= 0 (call-process
                          "curl" nil t nil "-s" "--max-time" "5"
                          (format "%s/api/alpha/evidence?tag=context-retrieval&session-id=%s&limit=400"
                                  session-mode-api-base sid)))
                (goto-char (point-min))
                (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
                       (data (ignore-errors (json-read)))
                       (out nil))
                  (dolist (e (cdr (assoc "entries" data)))
                    (let* ((body (cdr (assoc "evidence/body" e)))
                           (query (cdr (assoc "query" body)))
                           (top (car (cdr (assoc "results" body))))
                           (pid (cdr (assoc "id" top)))
                           (qn (session-mode--norm query)))
                      (when (and query pid (> (length qn) 8))
                        (push (cons (substring qn 0 (min 50 (length qn)))
                                    (session-mode--sigil-for-pattern pid))
                              out))))
                  out)))))
    (setq session-mode--turn-sigils result)))

(defun session-mode--region-pattern-sigil (beg end)
  "The retrieved-pattern sigil for the turn in BEG..END.  Extract the turn's clean USER
MESSAGE — the head of the region, after the \"──── *mission*\" rule and \"joe:\", bounded
by the \"claude:\" agent marker — and match it against the retrieval queries by shared
prefix.  (The query is user-msg + trailing context concatenated, so a fixed prefix of it
spills into agent text for SHORT messages; matching the buffer-delimited user message
instead is robust both ways.)"
  (let* ((head (session-mode--norm
                (buffer-substring-no-properties beg (min end (+ beg 800)))))
         (jstart (if (string-match "joe: " head) (match-end 0) 0))
         (rest (substring head jstart))
         (aend (if (string-match "\\bclaude[-0-9]*:\\|\\bcodex[-0-9]*:" rest)
                   (match-beginning 0) (length rest)))
         (um (string-trim (substring rest 0 (min aend 200)))))
    (when (>= (length um) 12)
      (cdr (seq-find
            (lambda (qs)
              (let* ((qp (car qs)) (n (min (length um) (length qp))))
                (and (>= n 12) (string= (substring um 0 n) (substring qp 0 n)))))
            session-mode--turn-sigils)))))

;; --- Spotting regexes (the SAME lexicons session_typology.py uses) ---
(defconst session-mode--mission-re "\\b\\([MEC]-[a-z][a-z0-9-]\\{3,\\}\\)\\b")
(defconst session-mode--word-re "\\b\\([a-z][a-z0-9-]\\{4,\\}\\)\\b")
(defconst session-mode--mint-re "!{[^}]+}")
(defconst session-mode--glyph-re "[香應咅鹽間専專蒲團]")
(defconst session-mode--correction-re
  (concat "\\b\\(not only\\|not just\\|not that\\|actually\\|no,\\|nope\\|isn'?t\\|wrong\\|"
          "instead\\|rather\\|i'?d say\\|let'?s not\\|don'?t\\|shouldn'?t\\|the issue is\\|too\\)\\b"))
(defconst session-mode--reach-re
  (concat "\\b\\(let'?s\\|we could\\|we should\\|shall we\\|i'?ll\\|we can\\|maybe we\\|how about\\|"
          "what if\\|i think we\\|we want\\|the goal is\\|next we\\)\\b"))

(defvar-local session-mode--overlays nil "Markup overlays this mode owns in the buffer.")

(defun session-mode--clear ()
  (mapc #'delete-overlay session-mode--overlays)
  (setq session-mode--overlays nil))

(defun session-mode--ov (beg end face type token &optional help)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'face face)
    (overlay-put o 'session-mode t)
    (overlay-put o 'session-mode-type type)
    (overlay-put o 'session-mode-token token)
    (overlay-put o 'mouse-face 'highlight)
    (when help (overlay-put o 'help-echo help))
    (push o session-mode--overlays)))

(defun session-mode--clocked-mission ()
  "The clocked mission = the FIRST on-disk mission token in the buffer (M-autoclock-in rule)."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward session-mode--mission-re nil t)
        (when (gethash (match-string-no-properties 1) session-mode--missions)
          (throw 'found (match-string-no-properties 1))))
      nil)))

(defun session-mode--scan ()
  "Apply deterministic markup over the whole buffer.  Counts spots by type (returns an alist)."
  (let ((counts nil) (case-fold-search t)
        (clocked (session-mode--clocked-mission)))
    (cl-flet ((tally (k) (setf (alist-get k counts 0) (1+ (alist-get k counts 0)))))
      ;; explicit: mint signs + glyphs
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward session-mode--mint-re nil t)
          (session-mode--ov (match-beginning 0) (match-end 0) 'session-mode-mint-face
                            "dsl-mint" (match-string-no-properties 0) "dsl-mint (explicit)")
          (tally 'mint)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward session-mode--glyph-re nil t)
          (session-mode--ov (match-beginning 0) (match-end 0) 'session-mode-glyph-face
                            "glyph" (match-string-no-properties 0) "futonic glyph (explicit)")
          (tally 'glyph)))
      ;; recognized: missions (clock vs mention) + patterns
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward session-mode--mission-re nil t)
          (let ((tok (match-string-no-properties 1)))
            (when (gethash tok session-mode--missions)
              (if (equal tok clocked)
                  (progn (session-mode--ov (match-beginning 1) (match-end 1) 'session-mode-clock-face
                                           "mission-clock" tok
                                           (format "%s — clocked-in (first mention)" tok))
                         (tally 'clock))
                (session-mode--ov (match-beginning 1) (match-end 1) 'session-mode-mention-face
                                  "mission-mention" tok
                                  (format "%s — mentioned, not clocked-in" tok))
                (tally 'mention))))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward session-mode--word-re nil t)
          (when (gethash (match-string-no-properties 1) session-mode--patterns)
            (session-mode--ov (match-beginning 1) (match-end 1) 'session-mode-pattern-face
                              "pattern-ref" (match-string-no-properties 1)
                              (format "%s — pattern (recognized)" (match-string-no-properties 1)))
            (tally 'pattern))))
      ;; cued: correction (+ optionally reach) — candidates, wavy
      (when session-mode-show-cues
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward session-mode--correction-re nil t)
            (session-mode--ov (match-beginning 1) (match-end 1) 'session-mode-correction-face
                              "correction" (match-string-no-properties 1)
                              "correction cue (candidate — needs override/model)")
            (tally 'correction)))
        (when session-mode-show-reach-cues
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward session-mode--reach-re nil t)
              (session-mode--ov (match-beginning 1) (match-end 1) 'session-mode-reach-face
                                "reach" (match-string-no-properties 1) "reach cue (candidate)")
              (tally 'reach)))))
      ;; The per-turn SIGIL: append each just-cooked turn's PATTERN sigil after its
      ;; "Cooked for …" line — the top pattern the futon3a embedding retrieval surfaced
      ;; for that turn (read from XTDB context-retrieval evidence), resolved to its
      ;; two-part «hanzi okipona» sigil.  Non-destructive after-string overlay.
      (save-excursion
        (goto-char (point-min))
        (let ((last (point-min)))
          (while (re-search-forward "^Cooked for .*$" nil t)
            ;; Capture the Cooked-line bounds BEFORE the nested search clobbers match-data.
            (let* ((cooked-beg (match-beginning 0))
                   (cooked-end (match-end 0))
                   ;; A REAL turn-flair "Cooked for …" is immediately followed by the
                   ;; "──── *mission*" rule line.  Agent output that merely QUOTES a
                   ;; "Cooked for …" line (e.g. these very explanations) is not — skip it,
                   ;; else the quoted line corrupts both the sigil and the region boundary.
                   (real (save-excursion (goto-char cooked-end) (forward-line 1)
                                         (looking-at-p "^─"))))
              (when real
                (let ((sig (session-mode--region-pattern-sigil last cooked-beg)))
                  (when (and sig (> (length sig) 0))
                    (let ((o (make-overlay cooked-end cooked-end)))
                      (overlay-put o 'after-string
                                   (propertize (concat "   〘 " sig " 〙") 'face 'session-mode-sigil-face))
                      (overlay-put o 'session-mode t)
                      (push o session-mode--overlays)
                      (tally 'sigil))))
                (setq last cooked-end)))))))
    counts))

(defun session-mode--region-sigil (beg end clocked)
  "Compose the recognized-act SIGIL for the turn in BEG..END: dominant mission, patterns,
and explicit/cued counts.  Deterministic — the same spotter, summarized."
  (let ((case-fold-search t) (miss nil) (pats nil) (corr 0) (mint 0) (glyphs nil))
    (save-match-data
     (save-excursion
      (goto-char beg)
      (while (re-search-forward session-mode--mission-re end t)
        (let ((tok (match-string-no-properties 1)))
          (when (and (gethash tok session-mode--missions) (not (member tok miss)))
            (push tok miss))))
      (goto-char beg)
      (while (re-search-forward session-mode--word-re end t)
        (let ((w (match-string-no-properties 1)))
          (when (and (gethash w session-mode--patterns) (not (member w pats)))
            (push w pats))))
      (goto-char beg)
      (while (re-search-forward session-mode--mint-re end t) (setq mint (1+ mint)))
      (goto-char beg)
      (while (re-search-forward session-mode--glyph-re end t)
        (cl-pushnew (match-string-no-properties 0) glyphs :test #'equal))
      (goto-char beg)
      (while (re-search-forward session-mode--correction-re end t) (setq corr (1+ corr)))))
    (setq miss (nreverse miss) pats (nreverse pats))
    (let ((parts nil))
      ;; clocked mission first (⊙), else first mentioned (○)
      (when miss
        (let ((m (or (car (member clocked miss)) (car miss))))
          (push (concat (if (equal m clocked) "⊙" "○") m) parts)))
      (when pats (push (concat "◇" (string-join (seq-take pats 2) ",")) parts))
      (when (> mint 0) (push (format "!%d" mint) parts))
      (when glyphs (push (concat "" (string-join (seq-take (nreverse glyphs) 4) "")) parts))
      (when (> corr 0) (push (format "✎%d" corr) parts))
      (if parts (concat "⟦ " (string-join (nreverse parts) " · ") " ⟧") ""))))

(defun session-mode-refresh ()
  "Re-apply the deterministic markup over the whole buffer; report the spot counts."
  (interactive)
  (unless (session-mode--load-vocab)
    (user-error "session-mode: no vocab at %s (run session_typology.py)" session-mode-vocab-file))
  (session-mode--fetch-turn-sigils)   ; refresh the per-turn pattern sigils from XTDB
  (session-mode--clear)
  (let ((c (session-mode--scan)))
    ;; Only chatter the spot-count summary when the operator ASKED (C-c s g);
    ;; the automatic after-change/turn-end refresh stays silent (was distracting).
    (when (called-interactively-p 'interactive)
      (message "session-mode: ⊙%d clock · ○%d mention · ◇%d pattern · !%d mint · 香%d · ✎%d corr · ◀%d reach"
               (alist-get 'clock c 0) (alist-get 'mention c 0) (alist-get 'pattern c 0)
               (alist-get 'mint c 0) (alist-get 'glyph c 0) (alist-get 'correction c 0)
               (alist-get 'reach c 0)))
    c))

(defun session-mode-toggle-cues ()
  "Toggle the cued tier (correction/reach candidate markers)."
  (interactive)
  (setq session-mode-show-cues (not session-mode-show-cues))
  (session-mode-refresh)
  (message "session-mode: cue markers %s" (if session-mode-show-cues "ON" "OFF")))

;; --- Hover: posframe detail card on cursor-over + raise the matching comb tooth in the
;; session-overview panel (the two surfaces cross-reference by mission name). ---

(defvar session-mode--posframe-available
  (or (require 'posframe nil t)
      (let ((dir "/home/joe/.emacs-graph/straight/build/posframe"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (require 'posframe nil t))))
  "Cached availability of posframe.")

(defconst session-mode--hover-buffer " *session-mode-hover*")
(defvar-local session-mode--hover-last nil)

(defun session-mode--ov-at-point ()
  "The session-mode markup overlay (with a type) at point, if any."
  (seq-find (lambda (o) (overlay-get o 'session-mode-type)) (overlays-at (point))))

(defun session-mode--mission-comb-summary (mission)
  "A one-line comb summary for MISSION pulled from the session-overview pivot, or nil."
  (when (and (boundp 'session-overview--data) session-overview--data)
    (let ((rows (alist-get 'rows (alist-get 'pivot session-overview--data))))
      (seq-some
       (lambda (r)
         (when (equal (alist-get 'mission r) mission)
           (let ((span (alist-get 'span r)))
             (format "  comb: arcs %s · ▶%s build ◀%s reach ✎%s steer\n"
                     (if span (format "%d–%d" (1+ (aref (vconcat span) 0))
                                      (1+ (aref (vconcat span) 1))) "—")
                     (alist-get 'n_build r) (alist-get 'n_reach r) (alist-get 'n_steer r)))))
       rows))))

(defun session-mode--hover-text (ov)
  (let* ((type (overlay-get ov 'session-mode-type))
         (token (overlay-get ov 'session-mode-token))
         (ty (and (session-mode--load-typology) (gethash type session-mode--typology)))
         (glyph (or (and ty (alist-get 'glyph ty)) "•"))
         (tier (or (and ty (alist-get 'tier ty)) "?"))
         (recog (and ty (alist-get 'recognizer ty))))
    (concat
     (format "%s %s   [tier: %s]\n" glyph type tier)
     (format "  token: %s\n" token)
     (when recog (format "  %s\n" recog))
     (when (member type '("mission-clock" "mission-mention"))
       (or (session-mode--mission-comb-summary token) "")))))

(defun session-mode--hover-hide ()
  (when (and session-mode--posframe-available (display-graphic-p))
    (posframe-hide session-mode--hover-buffer)))

(defun session-mode--hover-show (ov)
  (if (and session-mode--posframe-available (display-graphic-p))
      (posframe-show session-mode--hover-buffer
                     :string (session-mode--hover-text ov)
                     :position (point)
                     :poshandler #'posframe-poshandler-window-bottom-right-corner
                     :max-width 72 :border-width 1 :border-color "#7c5cff"
                     :background-color (face-attribute 'default :background)
                     :timeout 12)
    (message "%s" (string-trim (session-mode--hover-text ov)))))

(defun session-mode--raise-overview (mission)
  "If the *session-overview* panel is visible, scroll its comb to MISSION's tooth."
  (when-let ((win (get-buffer-window "*session-overview*")))
    (with-selected-window win
      (goto-char (point-min))
      (when (fboundp 'text-property-search-forward)
        (when-let ((m (text-property-search-forward 'session-overview-mission mission t)))
          (goto-char (prop-match-beginning m))
          (beginning-of-line)
          (recenter 3))))))

(defun session-mode--hover-post-command ()
  "Show/hide the detail card as point moves between marked items; raise the overview tooth."
  (let ((ov (session-mode--ov-at-point)))
    (cond
     ((null ov)
      (when session-mode--hover-last
        (setq session-mode--hover-last nil)
        (session-mode--hover-hide)))
     ((not (eq ov session-mode--hover-last))
      (setq session-mode--hover-last ov)
      (session-mode--hover-show ov)
      (let ((type (overlay-get ov 'session-mode-type)))
        (when (member type '("mission-clock" "mission-mention"))
          (session-mode--raise-overview (overlay-get ov 'session-mode-token))))))))

(defvar session-mode--idle-timer nil)

(defun session-mode--after-change (&rest _)
  "Debounced rescan as the buffer grows (new turns arrive)."
  (when (timerp session-mode--idle-timer) (cancel-timer session-mode--idle-timer))
  (let ((buf (current-buffer)))
    (setq session-mode--idle-timer
          (run-with-idle-timer
           0.6 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (bound-and-true-p session-mode) (session-mode-refresh)))))))))

;; Turn-end refresh.  claude-repl streams turn text with modification hooks
;; INHIBITED, so `after-change' never fires for a new turn; and a deferred
;; `run-at-time' timer does NOT fire until the next redisplay/input, because the
;; agent runs headless (`claude -p' driving the daemon) — so the sigil would only
;; appear when the OPERATOR next interacts.  The robust trigger is `advice-add
;; :after' on `agent-chat-finish-turn!' (the shared turn-end entry, called once per
;; turn AFTER the "Cooked for" flair is inserted) — run the refresh SYNCHRONOUSLY
;; there (its curl is ~100ms; fine on the turn-end path), no timer involved.
(defun session-mode--latest-flair-region ()
  "Return (BEG . END) of the bottom-most REAL \"Cooked for\" flair's turn region
\(previous real flair end .. this flair beg), or nil."
  (save-excursion
    (goto-char (point-max))
    (let (flair-beg)
      (while (and (not flair-beg) (re-search-backward "^Cooked for .*$" nil t))
        (when (save-excursion (forward-line 1) (looking-at-p "^─"))
          (setq flair-beg (match-beginning 0))))
      (when flair-beg
        (goto-char flair-beg)
        (let (prev)
          (while (and (not prev) (re-search-backward "^Cooked for .*$" nil t))
            (when (save-excursion (forward-line 1) (looking-at-p "^─"))
              (setq prev (match-end 0))))
          (cons (or prev (point-min)) flair-beg))))))

(defun session-mode--place-latest-sigil ()
  "Place ONLY the latest flair's pattern sigil (cheap — no full buffer scan).
Returns non-nil if a sigil was placed."
  (let ((region (session-mode--latest-flair-region)))
    (when region
      (let* ((flair-end (save-excursion (goto-char (cdr region)) (line-end-position)))
             (sig (session-mode--region-pattern-sigil (car region) (cdr region))))
        (when sig
          (dolist (o (overlays-in (cdr region) (1+ flair-end)))
            (when (and (overlay-get o 'session-mode) (overlay-get o 'after-string))
              (delete-overlay o)
              (setq session-mode--overlays (delq o session-mode--overlays))))
          (let ((ov (make-overlay flair-end flair-end)))
            (overlay-put ov 'after-string
                         (propertize (concat "   〘 " sig " 〙") 'face 'session-mode-sigil-face))
            (overlay-put ov 'session-mode t)
            (push ov session-mode--overlays))
          t)))))

(defun session-mode--finish-turn-advice (&rest _)
  "Decorate the just-ended turn's flair, in the buffer that ended it.
The current turn's `context-retrieval' evidence is written server-side async AFTER
turn-end, so it isn't queryable immediately.  Poll with CHEAP fetches (no scan) until
the retrieval lands, then place ONLY the new sigil — no full re-scan on the turn-end
path, so the only remaining cost is the unavoidable wait for the server's write."
  (when (bound-and-true-p session-mode)
    (let ((tries 0) (placed nil))
      (while (and (not placed) (< tries 12))
        (ignore-errors (session-mode--fetch-turn-sigils))
        (setq placed (ignore-errors (session-mode--place-latest-sigil)))
        (unless placed (setq tries (1+ tries)) (sleep-for 0.3)))
      (ignore-errors
        (write-region
         (format "%s buf=%s tries=%d placed=%s\n"
                 (format-time-string "%H:%M:%S") (buffer-name) tries (and placed t))
         nil "/tmp/sm-turnend.log" 'append 'silent)))))

(defvar session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s g") #'session-mode-refresh)
    (define-key map (kbd "C-c s c") #'session-mode-toggle-cues)
    (define-key map (kbd "C-c s o") #'session-overview)   ; this session's overview panel
    map)
  "Keymap for `session-mode'.")

;;;###autoload
(define-minor-mode session-mode
  "Deterministically mark up the live session buffer with recognized symbolic acts.
Highlights on-disk missions (clocked vs mentioned), patterns, explicit mint signs and
glyphs, and (faintly) correction/reach cues — all from a controlled vocabulary, no model."
  :lighter " 香"
  :keymap session-mode-map
  (if session-mode
      (progn
        (session-mode-refresh)
        (add-hook 'after-change-functions #'session-mode--after-change nil t)
        (add-hook 'post-command-hook #'session-mode--hover-post-command nil t)
        ;; Global, idempotent advice (a no-op in buffers without session-mode);
        ;; never removed, so other session-mode buffers keep working.
        (advice-add 'agent-chat-finish-turn! :after #'session-mode--finish-turn-advice))
    (remove-hook 'after-change-functions #'session-mode--after-change t)
    (remove-hook 'post-command-hook #'session-mode--hover-post-command t)
    (when (timerp session-mode--idle-timer) (cancel-timer session-mode--idle-timer))
    (session-mode--hover-hide)
    (session-mode--clear)))

(provide 'session-mode)
;;; session-mode.el ends here
