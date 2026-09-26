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
(require 'subr-x)
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

(defvar-local session-mode--idle-timer nil)

(defun session-mode--after-change (&rest changes)
  "Rescan transcript changes; draft typing is handled by the local tagger."
  (unless (and (session-mode--tag-input-start)
               (numberp (car changes))
               (>= (car changes) (session-mode--tag-input-start)))
    (when (timerp session-mode--idle-timer) (cancel-timer session-mode--idle-timer))
    (let ((buf (current-buffer)))
      (setq session-mode--idle-timer
            (run-with-idle-timer
             0.6 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (bound-and-true-p session-mode) (session-mode-refresh))))))))))

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
        (session-mode-turn-tags-mode 1)
        (add-hook 'after-change-functions #'session-mode--after-change nil t)
        (add-hook 'post-command-hook #'session-mode--hover-post-command nil t)
        ;; Global, idempotent advice (a no-op in buffers without session-mode);
        ;; never removed, so other session-mode buffers keep working.
        (advice-add 'agent-chat-finish-turn! :after #'session-mode--finish-turn-advice))
    (remove-hook 'after-change-functions #'session-mode--after-change t)
    (remove-hook 'post-command-hook #'session-mode--hover-post-command t)
    (when (timerp session-mode--idle-timer) (cancel-timer session-mode--idle-timer))
    (session-mode--hover-hide)
    (unless global-session-mode-turn-tags-mode (session-mode-turn-tags-mode -1))
    (session-mode--clear)))

;; --- Local passage tags: draft feedback, with no retrieval/model calls. ---
(defconst session-mode-turn-intent-vocabulary
  '(("approve" "I agree" "I approve" "that's a good fit" "that's great" "looks good" "good news" "sounds good" "you're right" "Yes you can do this" "I definitely like the idea")
    ("disagree" "I disagree" "I don't agree" "I do not agree" "that's wrong" "misunderstood my intent" "doesn't match my intent" "does not match my intent" "of no use whatsoever" "a bad design" "wasn't very useful" "was not very useful")
    ("clarify" "I don't understand" "help me understand" "I'd like to know" "I'd like to see some examples" "what's the story" "What gives" "I'm slightly confused" "How should we proceed" "what's our strategy")
    ("propose" "I suggest" "We could perhaps" "maybe we could" "Maybe we should" "I wonder if" "I think it would be good")
    ("extend" "in parallel" "Another thing we should pay attention to" "we should also" "we could also" "that's another analysis" "could be a further set of tasks")
    ("prioritize" "as a matter of priority" "please do that next step" "needs to be the next" "first instance" "we have 7 minutes")
    ("delegate" "please bell" "you can bell" "let's ask" "ask Zai" "we should ask" "should be sent to" "for dispatches" "you will be responsible for")
    ("verify" "we should check" "we need to check" "we need to be able to validate" "we'll have to check" "we have to audit" "I want a reproduction")
    ("constrain" "don't do that" "I am not asking you to" "I don't want to spend" "please use aliases" "we will not do any deep dives" "I don't want a repeat" "not going to decide things by fiat")
    ("defer" "we'll do it when we get time" "we can come back to" "at some point" "for now" "defer processing")
    ("continue" "please continue" "go on" "get on with it" "let's continue" "Please do 1, 2, and 3")
    ("redirect" "rather than" "let's trim" "we will instead focus" "I'd like to return to" "what we should do is" "I want to alter" "I would want" "I would prefer" "what I want instead" "I meant that")
    ("explain" "here's why" "my main point" "what I mean" "the broader long term idea" "the use cases would be" "my use case")
    ("report-problem" "is currently broken" "I still see an HTTP error" "it's broken" "login doesn't work" "overlaps existing UI elements" "point of major concern" "not getting any markup" "totally underlined")
    ("collect" "collect information" "getting logs" "keep a record" "record the turns")
    ("qualify" "with the caveat" "to the extent that it is possible")
    ("ask-action" "can you please" "please publish" "please sort this out" "I would like to have" "please update"))
  "Agent-curated intent phrases from recorded operator turns (2026-09-22).
See analysis/audits/intent-vocabulary-2026-09-22.md in futon0 for evidence.
Single conjunctions such as but do not identify intent.")

(defcustom session-mode-turn-vocabulary
  (copy-tree session-mode-turn-intent-vocabulary)
  "Literal phrases grouped by communicative intent; multiple tags can apply.
These are agent-inferred cues, not verified whole-turn intent.  Operator !c
corrections override the vocabulary and remain recorded as human labels."
  :type '(repeat (cons (string :tag "Tag") (repeat (string :tag "Phrase"))))
  :group 'session-mode)

(defcustom session-mode-turn-rules-file
  (expand-file-name "session-turn-vocabulary.json" user-emacs-directory)
  "Saved live phrase vocabulary.  JSON data, never evaluated as Lisp."
  :type 'file :group 'session-mode)
(defvar session-mode--vocabulary-loaded-file nil)
(defvar session-mode-learned-cues nil "Provenance for automatically acquired phrase hypotheses.")

(defun session-mode--validate-turn-vocabulary (rules)
  "Validate saved RULES before replacing any live vocabulary."
  (unless (and (listp rules)
               (cl-every (lambda (entry)
                           (and (consp entry) (stringp (car entry))
                                (string-match-p "\\`[[:alpha:]][[:alnum:]_-]*\\'" (car entry))
                                (consp (cdr entry))
                                (cl-every (lambda (phrase)
                                            (and (stringp phrase)
                                                 (not (string-empty-p (string-trim phrase)))))
                                          (cdr entry)))) rules))
    (user-error "Invalid saved turn vocabulary; live rules unchanged"))
  rules)

(defun session-mode--load-live-vocabulary ()
  "Load saved phrase rules once per configured file."
  (let ((file (expand-file-name session-mode-turn-rules-file)))
    (unless (equal file session-mode--vocabulary-loaded-file)
      (when (file-exists-p file)
        (let* ((json-object-type 'alist) (json-array-type 'list)
               (json-key-type 'symbol) (data (json-read-file file)))
          (unless (memq (alist-get 'version data) '(1 2))
            (user-error "Unsupported turn vocabulary version; live rules unchanged"))
          (setq session-mode-turn-vocabulary
                (session-mode--validate-turn-vocabulary (alist-get 'rules data))
                session-mode-turn-corrections (alist-get 'corrections data)
                session-mode-learned-cues (alist-get 'learned_cues data))))
      (setq session-mode--vocabulary-loaded-file file))))

(defun session-mode--save-live-vocabulary (rules &optional corrections learned)
  "Atomically save RULES before publishing them in the running Emacs."
  (session-mode--validate-turn-vocabulary rules)
  (let* ((file (expand-file-name session-mode-turn-rules-file))
         (directory (file-name-directory file)) temp)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".turn-vocabulary-" directory)))
          (with-temp-file temp
            (insert (json-encode `((version . 2) (rules . ,(vconcat (mapcar #'vconcat rules)))
                                   (corrections . ,(vconcat (or corrections session-mode-turn-corrections)))
                                   (learned_cues . ,(vconcat (or learned session-mode-learned-cues))))))
            (insert "\n"))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp))))
  (setq session-mode-turn-vocabulary rules
        session-mode-turn-corrections (or corrections session-mode-turn-corrections)
        session-mode-learned-cues (or learned session-mode-learned-cues)
        session-mode--vocabulary-loaded-file (expand-file-name session-mode-turn-rules-file)))

(defun session-mode-turn-add-rule (tag phrase)
  "Add literal PHRASE under TAG, save it, and refresh all active drafts."
  (interactive "sTag: \nsLiteral phrase: ")
  (session-mode--load-live-vocabulary)
  (setq tag (downcase (string-trim tag)) phrase (string-trim phrase))
  (session-mode--validate-turn-vocabulary (list (list tag phrase)))
  (let* ((rules (copy-tree session-mode-turn-vocabulary))
         (entry (assoc tag rules)))
    (unless (and entry (cl-find phrase (cdr entry) :test #'string-equal-ignore-case))
      (if entry (setcdr entry (append (cdr entry) (list phrase)))
        (setq rules (append rules (list (list tag phrase)))))
      (session-mode--save-live-vocabulary rules)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when session-mode-turn-tags-mode (session-mode-turn-tags-refresh))))
  (message "Phrase %S → %s; saved live vocabulary" phrase tag))

(defface session-mode-turn-agree-face
  '((t :underline (:color "#27864b" :style wave)))
  "Provisional agreement phrase." :group 'session-mode)
(defface session-mode-turn-object-face
  '((t :underline (:color "#c05050" :style wave)))
  "Provisional objection phrase." :group 'session-mode)
(defface session-mode-turn-other-face
  '((t :underline (:color "#9270c9" :style wave)))
  "Other provisional turn phrase." :group 'session-mode)

(defvar-local session-mode--draft-tag-overlays nil)
(defvar-local session-mode--sent-tag-overlays nil)
(defvar-local session-mode--tag-timer nil)
(defvar-local session-mode--draft-tags nil
  "Current draft's distinct cue tags, in passage order.")
(defvar session-mode-turn-tags-mode)
(defvar global-session-mode-turn-tags-mode)

(defun session-mode--turn-matches (text)
  "Return (START END TAG PHRASE) matches in TEXT, with zero-based offsets.
Match case-insensitively with word/underscore boundaries and either apostrophe.
Literal phrases do not absorb their targets or force a single turn category."
  (let ((case-fold-search t) hits)
    (with-temp-buffer
      ;; Fix boundaries independently of the conversation's major-mode syntax.
      (set-syntax-table (standard-syntax-table))
      (insert text)
      (dolist (entry session-mode-turn-vocabulary)
        (dolist (phrase (cdr entry))
          (unless (string-empty-p phrase)
            (goto-char (point-min))
            (let ((regexp (replace-regexp-in-string
                           "'" "['’]" (regexp-quote phrase) t t)))
              (while (re-search-forward regexp nil t)
                (let ((beg (match-beginning 0)) (end (match-end 0)))
                  (when (and (or (= beg (point-min))
                                 (not (string-match-p "[[:alnum:]_]"
                                                      (char-to-string (char-before beg)))))
                             (or (= end (point-max))
                                 (not (string-match-p "[[:alnum:]_]"
                                                      (char-to-string (char-after end))))))
                    (push (list (1- beg) (1- end) (car entry)
                                (buffer-substring-no-properties beg end)) hits)))))))))
    (sort (delete-dups hits) (lambda (a b) (< (car a) (car b))))))

(defun session-mode--tag-input-start ()
  "Return this buffer's valid agent-chat draft start, or nil."
  (when (and (boundp 'agent-chat--input-start)
             (markerp agent-chat--input-start)
             (eq (marker-buffer agent-chat--input-start) (current-buffer))
             (<= (point-min) (marker-position agent-chat--input-start) (point-max)))
    (marker-position agent-chat--input-start)))

(defun session-mode--paint-turn-tags (beg end _draft)
  "Decorate BEG..END without changing text; return (TAGS . OVERLAYS).
Only underline existing characters: no inserted display strings or line shifts."
  (let* ((text (buffer-substring-no-properties beg end))
         (hits (session-mode--turn-matches text)) tags overlays)
    (dolist (hit hits)
      (let* ((tag (nth 2 hit))
             (ov (make-overlay (+ beg (nth 0 hit)) (+ beg (nth 1 hit)) nil nil nil)))
        (cl-pushnew tag tags :test #'equal)
        (overlay-put ov 'session-mode-turn-tag tag)
        (overlay-put ov 'priority 30)
        (overlay-put ov 'face (pcase tag
                               ((or "agree" "approve") 'session-mode-turn-agree-face)
                               ((or "object" "disagree") 'session-mode-turn-object-face)
                               (_ 'session-mode-turn-other-face)))
        (overlay-put ov 'help-echo (format "%s → %s (provisional phrase cue)" (nth 3 hit) tag))
        (push ov overlays)))
    (setq tags (nreverse tags))
    (cons tags overlays)))

(defun session-mode--cancel-tag-timer ()
  (when (timerp session-mode--tag-timer) (cancel-timer session-mode--tag-timer))
  (setq session-mode--tag-timer nil))

(defun session-mode-turn-tags-refresh ()
  "Refresh only the local draft; never fetch evidence or scan the transcript."
  (interactive)
  (session-mode--cancel-tag-timer)
  (mapc #'delete-overlay session-mode--draft-tag-overlays)
  (setq session-mode--draft-tag-overlays nil session-mode--draft-tags nil)
  (when (and session-mode-turn-tags-mode (session-mode--tag-input-start))
    (save-match-data
      (let ((result (session-mode--paint-turn-tags
                     (session-mode--tag-input-start) (point-max) t)))
        (setq session-mode--draft-tags (car result)
              session-mode--draft-tag-overlays (cdr result))))))

(defun session-mode--tags-after-change (&rest _)
  "Debounce draft feedback independently in each conversation buffer."
  (session-mode--cancel-tag-timer)
  (setq session-mode--tag-timer
        (run-with-idle-timer
         0.15 nil
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when session-mode-turn-tags-mode (session-mode-turn-tags-refresh)))))
         (current-buffer))))

(defun session-mode--tag-sent-message (original name text)
  "Preserve tags on the latest operator message inserted by ORIGINAL.
Use the real inserted span, including any agent-chat text transformations."
  (if (not (and session-mode-turn-tags-mode
                (equal name agent-chat-user-speaker)
                (markerp agent-chat--prompt-marker)
                (eq (marker-buffer agent-chat--prompt-marker) (current-buffer))))
      (funcall original name text)
    (let ((start (copy-marker agent-chat--prompt-marker nil)))
      (unwind-protect
          (prog1 (funcall original name text)
            (setq session-mode--last-operator-text
                  (buffer-substring-no-properties
                   (+ (marker-position start) (length name) 2)
                   (- (marker-position agent-chat--prompt-marker) 2)))
            (when session-mode--last-operator-region
              (set-marker (car session-mode--last-operator-region) nil)
              (set-marker (cdr session-mode--last-operator-region) nil))
            (setq session-mode--last-operator-region
                  (cons (copy-marker (+ (marker-position start) (length name) 2) t)
                        (copy-marker (- (marker-position agent-chat--prompt-marker) 2) nil)))
            (mapc #'delete-overlay session-mode--sent-tag-overlays)
            (setq session-mode--sent-tag-overlays
                  (cdr (session-mode--paint-turn-tags
                        (+ (marker-position start) (length name) 2)
                        ;; agent-chat appends two newlines after message text.
                        (- (marker-position agent-chat--prompt-marker) 2) nil)))
            (session-mode-turn-tags-refresh))
        (set-marker start nil)))))

(defun session-mode-describe-turn-intent ()
  "Show matched intent cues on request, without inserting display text."
  (interactive)
  (session-mode--refresh-analysis-on-navigation)
  (let ((hits (seq-filter (lambda (o) (overlay-get o 'session-mode-turn-tag))
                          (overlays-at (point)))))
    (when (seq-some (lambda (o) (overlay-get o 'session-mode-inferred)) hits)
      (setq hits (seq-filter (lambda (o) (overlay-get o 'session-mode-inferred)) hits)))
    (if hits
        (message "%s" (string-join (mapcar (lambda (o) (overlay-get o 'help-echo)) hits) "; "))
      (message "Draft intent cues: %s"
               (if session-mode--draft-tags (string-join session-mode--draft-tags ", ")
                 "no recognized phrase; intent not inferred")))))

;;;###autoload
(define-minor-mode session-mode-turn-tags-mode
  "Underline phrase cues without inserting a draft classification summary.
Also annotate the latest sent operator turn.  Drafts use local cues only;
sent turns can request agent interpretation through `session-turn-analysis'.
Kept separate from full session markup so typing never triggers retrieval."
  ;; A red 象 says this buffer's operator turns are on the record: captured
  ;; and sent for interpretation.  No 象 means off the record.
  :lighter (:propertize " 象" face (:foreground "red" :weight bold))
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c s i") #'session-mode-describe-turn-intent)
            map)
  (if session-mode-turn-tags-mode
      (progn
        (session-mode--load-live-vocabulary)
        (setq session-mode--analysis-display-stamp nil)
        (add-hook 'after-change-functions #'session-mode--tags-after-change nil t)
        (add-hook 'kill-buffer-hook #'session-mode--cancel-tag-timer nil t)
        (add-hook 'post-command-hook #'session-mode--refresh-analysis-on-navigation nil t)
        (session-mode-turn-tags-refresh))
    (remove-hook 'after-change-functions #'session-mode--tags-after-change t)
    (remove-hook 'post-command-hook #'session-mode--refresh-analysis-on-navigation t)
    (remove-hook 'kill-buffer-hook #'session-mode--cancel-tag-timer t)
    (session-mode--cancel-tag-timer)
    (mapc #'delete-overlay (append session-mode--draft-tag-overlays session-mode--sent-tag-overlays))
    (setq session-mode--draft-tag-overlays nil session-mode--sent-tag-overlays nil
          session-mode--draft-tags nil)))

(defun session-mode--tags-after-init (&rest _)
  "Enable local tags after an agent-chat buffer creates its input marker."
  (when global-session-mode-turn-tags-mode (session-mode-turn-tags-mode 1)))

;;;###autoload
(define-minor-mode global-session-mode-turn-tags-mode
  "Enable local turn tags in current and future initialized agent-chat buffers.
On by default (Joe, 2026-09-26): every operator turn is captured and
interpreted.  Turn it off, or `session-mode-turn-tags-mode' in one buffer,
to talk off the record."
  :global t :group 'session-mode :init-value t
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (session-mode--tag-input-start)
        (session-mode-turn-tags-mode (if global-session-mode-turn-tags-mode 1 -1))))))

(defvar session-mode-turn-corrections nil
  "Saved operator sentence labels; independent of inferred phrase cues.")
(defvar-local session-mode--last-operator-text nil)
(defvar-local session-mode--last-operator-region nil)

(defun session-mode--refresh-sent-tags ()
  "Reclassify the captured latest operator passage without changing its text."
  (when (and session-mode-turn-tags-mode session-mode--last-operator-region
             (eq (marker-buffer (car session-mode--last-operator-region)) (current-buffer))
             (eq (marker-buffer (cdr session-mode--last-operator-region)) (current-buffer))
             (equal session-mode--last-operator-text
                    (buffer-substring-no-properties
                     (car session-mode--last-operator-region)
                     (cdr session-mode--last-operator-region))))
    (mapc #'delete-overlay session-mode--sent-tag-overlays)
    (setq session-mode--sent-tag-overlays
          (cdr (session-mode--paint-turn-tags
                (car session-mode--last-operator-region)
                (cdr session-mode--last-operator-region) nil)))))

(defun session-mode--sentences (text)
  "Split TEXT with Emacs sentence motion; return nonempty sentence strings."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((sentence-end-double-space nil) result)
      (while (< (point) (point-max))
        (skip-chars-forward " \t\n")
        (let ((start (point)))
          (forward-sentence)
          (when (= start (point)) (goto-char (point-max)))
          (let ((sentence (string-trim (buffer-substring-no-properties start (point)))))
            (unless (string-empty-p sentence) (push sentence result)))))
      (nreverse result))))

(defun session-mode-correct-sentences (text labels)
  "Save ordered human LABELS for TEXT and reclassify its existing phrase cues.
Also retain whole sentences as exact examples; no invented keyword extraction.
A future refiner can learn new phrases from `session-mode-turn-corrections'."
  (session-mode--load-live-vocabulary)
  (let ((sentences (session-mode--sentences text)) (rules (copy-tree session-mode-turn-vocabulary))
        (changes (make-hash-table :test #'equal)) pairs)
    (unless (= (length sentences) (length labels))
      (user-error "%d sentences but %d labels; nothing changed. Sentences: %s"
                  (length sentences) (length labels) (string-join sentences " | ")))
    (cl-mapc
     (lambda (sentence label)
       (session-mode--validate-turn-vocabulary (list (list label sentence)))
       (push `((text . ,sentence) (label . ,label)) pairs)
       ;; Reassign cues actually present; retain multiple human labels when the
       ;; same phrase occurs in differently labelled sentences in this correction.
       (dolist (phrase (cons sentence (mapcar (lambda (hit) (nth 3 hit))
                                            (session-mode--turn-matches sentence))))
         (let ((key (downcase phrase)))
           (puthash key (cl-adjoin label (gethash key changes) :test #'equal) changes))))
     sentences labels)
    (setq rules
          (delq nil (mapcar (lambda (entry)
                             (let ((phrases (cl-remove-if
                                             (lambda (p) (gethash (downcase p) changes)) (cdr entry))))
                               (when phrases (cons (car entry) phrases)))) rules)))
    (maphash (lambda (phrase tags)
               (dolist (tag tags)
                 (let ((entry (assoc tag rules)))
                   (if entry (setcdr entry (append (cdr entry) (list phrase)))
                     (setq rules (append rules (list (list tag phrase)))))))) changes)
    (let* ((record `((at . ,(format-time-string "%FT%TZ" nil t))
                     (author . "joe") (text . ,text)
                     (sentences . ,(vconcat (nreverse pairs)))
                     (method . "human-labels; existing-cue reassignment; exact-sentence examples")))
           (records (append session-mode-turn-corrections (list record))))
      (session-mode--save-live-vocabulary rules records))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when session-mode-turn-tags-mode
          (session-mode-turn-tags-refresh)
          (session-mode--refresh-sent-tags))))
    (message "Saved sentence labels: %s; existing cues reclassified" (string-join labels " / "))))

(defun session-mode--consume-tag-command (original &rest args)
  "Handle final-line !c LABEL1 LABEL2 ... as ordered sentence supervision.
Correct the preceding draft, or the latest operator turn captured by this mode.
The directive never starts an agent turn and never sends a preceding draft."
  (let ((start (session-mode--tag-input-start)))
    (if (not start) (apply original args)
      (let* ((text (string-trim-right (buffer-substring-no-properties start (point-max))))
             (line-start (or (and (string-match "[^\n]*\\'" text) (match-beginning 0)) 0))
             (line (string-trim (substring text line-start))))
        ;; Validate before agent-chat deletes the input region on send.
        (when session-mode-turn-tags-mode (session-mode--split-failure-marker text))
        (if (not (string-match-p "\\`!c\\(?:[ \t]\\|\\'\\)" line)) (apply original args)
          (let* ((labels (split-string (substring line 2) "[ \t]+" t))
                 (draft (string-trim (substring text 0 line-start)))
                 (target (if (string-empty-p draft) session-mode--last-operator-text draft)))
            (unless (and labels (cl-every (lambda (label)
                                           (string-match-p "\\`[[:alpha:]][[:alnum:]_-]*\\'" label)) labels))
              (user-error "Usage: !c LABEL1 LABEL2 ... (one label per sentence); nothing sent"))
            (unless target
              (user-error "No captured operator turn; put !c beneath the text to classify"))
            (session-mode-correct-sentences target (mapcar #'downcase labels))
            (delete-region (+ start line-start) (point-max))
            (unless session-mode-turn-tags-mode (session-mode-turn-tags-mode 1))
            (session-mode-turn-tags-refresh)))))))

(with-eval-after-load 'agent-chat
  (advice-add 'agent-chat-send-input :around #'session-mode--consume-tag-command)
  (advice-add 'agent-chat-init-buffer :after #'session-mode--tags-after-init)
  (advice-add 'agent-chat-insert-message :around #'session-mode--tag-sent-message))

(require 'session-turn-analysis)

(provide 'session-mode)
;;; session-mode.el ends here
