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

(require 'agent-turn-origin)
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

(defcustom session-mode-rnode-vocabulary-file
  "/home/joe/code/futon0/analysis/audits/rnode-tree/rnode-vocabulary.json"
  "Generated deterministic R-node cue vocabulary."
  :type 'file :group 'session-mode)

(defcustom session-mode-rnode-tags t
  "When non-nil, lightly tag R-node cues in operator regions."
  :type 'boolean :group 'session-mode)

(defcustom session-mode-rnode-cues-file
  (expand-file-name "~/.emacs-graph/rnode-cues.json")
  "Atomic store of recurrent R-node cue proposals and promotion state."
  :type 'file :group 'session-mode)

(defcustom session-mode-xiang-decisions-file
  (expand-file-name "~/.emacs-graph/xiang-decisions.jsonl")
  "Append-only log of 象 cue and turn policy decisions."
  :type 'file :group 'session-mode)

(defcustom session-mode-xiang-source-precision
  '((operator-correction . 4.0) (second-distinct-seat . 1.5)
    (same-seat-repeat . 0.5) (first-proposal . 1.0))
  "Precision weights used to turn independent R-node evidence into counts."
  :type '(alist :key-type symbol :value-type number) :group 'session-mode)

(defcustom session-mode-xiang-cost-wrong-red 1.0
  "Risk cost of painting an incorrect learned R-node cue red."
  :type 'number :group 'session-mode)

(defcustom session-mode-xiang-cost-missed-red 0.3
  "Risk cost of failing to paint a correct R-node cue."
  :type 'number :group 'session-mode)

(defcustom session-mode-xiang-temperature 0.0
  "Softmax temperature tau for 象 policy selection; nonpositive means argmin."
  :type 'number :group 'session-mode)

(defcustom session-mode-xiang-token-cost 1.0
  "Cost assigned to requesting one turn interpretation."
  :type 'number :group 'session-mode)

(defcustom session-mode-xiang-intent-value 5.0
  "Value of intent interpretation on a substantive operator turn."
  :type 'number :group 'session-mode)

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

(defface session-mode-command-word-face
  '((t :underline t))
  "A command keyword (yes, undo) inside operator text that is not a command."
  :group 'session-mode)
(defface session-mode-command-face
  '((t :foreground "#dc2626" :weight bold))
  "An operator message that is a whole command (`undo', `yes 2', ...) (red)."
  :group 'session-mode)

;; Reply-proforma marks (㊥, 🈸, ...) coloured by the loop stage of their intent,
;; in the stage colours of the Minard figures.  The 🈀-block marks are colour
;; emoji, which ignore the foreground, so only the ㊀-block marks change colour.
(defface session-mode-mark-perceive-face
  '((((background light)) :foreground "#2a78d6" :weight bold)
    (((background dark)) :foreground "#3987e5" :weight bold))
  "Mark whose intent is a PERCEIVE stage (blue)." :group 'session-mode)
(defface session-mode-mark-believe-face
  '((((background light)) :foreground "#eb6834" :weight bold)
    (((background dark)) :foreground "#d95926" :weight bold))
  "Mark whose intent is a BELIEVE stage (orange)." :group 'session-mode)
(defface session-mode-mark-evaluate-face
  '((((background light)) :foreground "#1baf7a" :weight bold)
    (((background dark)) :foreground "#199e70" :weight bold))
  "Mark whose intent is an EVALUATE stage (green)." :group 'session-mode)
(defface session-mode-mark-select-face
  '((((background light)) :foreground "#eda100" :weight bold)
    (((background dark)) :foreground "#c98500" :weight bold))
  "Mark whose intent is a SELECT stage (amber)." :group 'session-mode)
(defface session-mode-mark-act-face
  '((((background light)) :foreground "#e87ba4" :weight bold)
    (((background dark)) :foreground "#d55181" :weight bold))
  "Mark whose intent is an ACT stage (pink)." :group 'session-mode)
(defface session-mode-mark-annotator-face
  '((((background light)) :foreground "#66665e" :weight bold)
    (((background dark)) :foreground "#b5b9bd" :weight bold))
  "Mark outside the loop: gist and unresolved (grey)." :group 'session-mode)

;; --- Controlled vocabulary (loaded once, cached) ---
(defvar session-mode--missions nil "Hash set of on-disk mission/excursion names.")
(defvar session-mode--patterns nil "Hash set of on-disk pattern (flexiarg) names.")
(defvar session-mode--typology nil "type -> alist(glyph tier recognizer colour), from typology.json.")
(defvar session-mode--rnode-vocabulary nil
  "Compiled R-node rows loaded from `session-mode-rnode-vocabulary-file'.")
(defvar session-mode--rnode-vocabulary-key nil
  "File and modification-time key for the compiled R-node vocabulary.")
(defvar session-mode--rnode-missing-reported nil
  "Non-nil after reporting one missing R-node vocabulary message.")
(defvar session-mode--learned-rnode-vocabulary nil
  "Compiled active cues loaded from `session-mode-rnode-cues-file'.")
(defvar session-mode--learned-rnode-key nil
  "File and modification-time key for active learned R-node cues.")
(defvar session-mode--rnode-store-error-reported nil
  "Non-nil after reporting one unreadable learned R-node cue store.")
(defconst session-mode--rnode-cache-format-version 2
  "Format of compiled R-node vocabulary rows kept across live reloads.")

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

(defconst session-mode--marks
  ;; mark intent stage; stages from legend-rows in futon3/src-cljs/futon3/turnfeed/core.cljs
  '(("㊩" "report-problem" perceive) ("🈖" "explain" perceive) ("㊢" "report" perceive)
    ("🈯" "clarify" believe) ("㊟" "qualify" believe) ("㊣" "approve" believe)
    ("🈚" "disagree" believe) ("㊮" "collect" believe) ("🈹" "retract" believe)
    ("🈲" "constrain" evaluate) ("🈕" "extend" evaluate) ("㊫" "explore" evaluate)
    ("㊭" "propose" select) ("㊝" "prioritize" select) ("🈘" "redirect" select)
    ("🈝" "defer" select) ("㊯" "delegate" select) ("🈡" "withdraw" select)
    ("🈸" "ask-action" act) ("🈰" "continue" act) ("㊬" "verify" act)
    ("㊥" "gist" annotator) ("🈳" "unresolved" annotator))
  "Reply-proforma marks with their intent and loop stage.")

(defconst session-mode--mark-re
  (regexp-opt (mapcar #'car session-mode--marks)))
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

(defconst session-mode--command-word-re "\\b\\(yes\\|undo\\)\\b"
  "Command keywords marked inside operator text.")

(defun session-mode--command-p (text)
  "Non-nil when operator TEXT as a whole is an `undo' or acceptance command."
  (or (and (fboundp 'agent-chat--undo-command) (agent-chat--undo-command text))
      (and (fboundp 'agent-chat--acceptance-command)
           (agent-chat--acceptance-command text))))

(defun session-mode--operator-regions ()
  "Return (BEG . END) for each sent operator message and the unsent input.
A sent message starts after a line-initial \"joe: \" and ends before the next
line that starts a speaker name, a \"Cooked for\" line or a rule line."
  (let (regions)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^joe: " nil t)
        (let ((beg (point)))
          (if (re-search-forward "^\\(?:[[:alnum:]_-]+: \\|Cooked for \\|─\\)" nil t)
              (goto-char (match-beginning 0))
            (goto-char (point-max)))
          (push (cons beg (point)) regions))))
    (when (and (boundp 'agent-chat--input-start)
               (markerp agent-chat--input-start)
               (marker-position agent-chat--input-start))
      (push (cons (marker-position agent-chat--input-start) (point-max)) regions))
    (nreverse regions)))

(defun session-mode--mark-commands (tally)
  "Mark command keywords in operator text; call TALLY with `command' per command."
  (let ((case-fold-search t))
    (pcase-dolist (`(,beg . ,end) (session-mode--operator-regions))
      (let* ((text (buffer-substring-no-properties beg end))
             (lead (progn (string-match "\\`[[:space:]]*" text) (match-end 0))))
        (if (session-mode--command-p text)
            (let ((b (+ beg lead))
                  (e (save-excursion (goto-char end) (skip-chars-backward " \t\n")
                                     (point))))
              (session-mode--ov b e 'session-mode-command-face "command"
                                (string-trim text) "command (read by the REPL, not sent as prose)")
              (funcall tally 'command))
          (save-excursion
            (goto-char beg)
            (while (re-search-forward session-mode--command-word-re end t)
              (session-mode--ov (match-beginning 1) (match-end 1)
                                'session-mode-command-word-face "command-word"
                                (match-string-no-properties 1)
                                "command keyword, not a command here"))))))))

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
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward session-mode--mark-re nil t)
          (pcase-let ((`(,mark ,intent ,stage) (assoc (match-string-no-properties 0) session-mode--marks)))
            (session-mode--ov (match-beginning 0) (match-end 0)
                              (intern (format "session-mode-mark-%s-face" stage))
                              "mark" mark (format "%s %s — %s" mark intent (upcase (symbol-name stage))))
            (tally 'mark))))
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
      ;; explicit: operator commands (red) vs command keywords in prose (underline)
      (session-mode--mark-commands #'tally)
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
    ("withdraw" "withdraw that pattern" "rule should be removed")
    ("redirect" "rather than" "let's trim" "we will instead focus" "I'd like to return to" "what we should do is" "I want to alter" "I would want" "I would prefer" "what I want instead" "I meant that")
    ("explain" "here's why" "my main point" "what I mean" "the broader long term idea" "the use cases would be" "my use case")
    ("report-problem" "is currently broken" "I still see an HTTP error" "it's broken" "login doesn't work" "overlaps existing UI elements" "point of major concern" "not getting any markup" "totally underlined")
    ("collect" "collect information" "getting logs" "keep a record" "record the turns")
    ("qualify" "with the caveat" "to the extent that it is possible")
    ("ask-action" "can you please" "please publish" "please sort this out" "I would like to have" "please update"))
  "Agent-curated intent phrases from recorded operator turns (2026-09-22).
See analysis/audits/intent-vocabulary-2026-09-22.md in futon0 for evidence.
The `withdraw' phrases are Joe's exact words in evidence
emacs-4c7d6a17516feb370bfadaeca20f6939 and
emacs-495da0160da7cbbe11e87e98bae8d56f (2026-09-27).  Its chip is
`op-drop-stack'.  A cue is an interpretation only; it has no effect by itself.
Single conjunctions such as but do not identify intent.")

(defconst session-mode-turn-vocabulary-version 3
  "Version of the literal intent vocabulary written to analysis requests.")

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
          (unless (memq (alist-get 'version data) '(1 2 3))
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
            (insert (json-encode `((version . ,session-mode-turn-vocabulary-version)
                                   (rules . ,(vconcat (mapcar #'vconcat rules)))
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
(defvar session-mode--past-tag-overlays)      ; session-turn-analysis.el
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
        ;; An intent cue and an R-node term never share characters; the
        ;; intent reading wins whichever painter ran first.
        (mapc #'delete-overlay
              (session-mode--overlays-with-property
               (overlay-start ov) (overlay-end ov) 'session-mode-rnode-tag))
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
            ;; The previous turn keeps its underlines: they move to the past
            ;; list instead of being deleted, so an earlier turn can still be
            ;; cited by its marks (Joe, 2026-09-29).
            (if (boundp 'session-mode--past-tag-overlays)
                (setq session-mode--past-tag-overlays
                      (append session-mode--sent-tag-overlays session-mode--past-tag-overlays))
              (mapc #'delete-overlay session-mode--sent-tag-overlays))
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
;; Marks are painted under turn-tags mode too, the mode REPL buffers run;
;; jit-lock repaints them as text is shown or changed, on overlays that
;; font-lock does not clear.
(defun session-mode--paint-marks (beg end)
  "Give each reply-proforma mark between BEG and END its stage face."
  (dolist (o (overlays-in beg end))
    (when (overlay-get o 'session-mode-mark) (delete-overlay o)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward session-mode--mark-re end t)
      (pcase-let ((`(,mark ,intent ,stage) (assoc (match-string-no-properties 0) session-mode--marks))
                  (o (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put o 'session-mode-mark t)
        (overlay-put o 'evaporate t)
        (overlay-put o 'face (intern (format "session-mode-mark-%s-face" stage)))
        (overlay-put o 'help-echo (format "%s %s — %s" mark intent (upcase (symbol-name stage))))))))

(defun session-mode--rnode-cue-regexp (cue)
  "Compile CUE with evaluator-compatible boundaries and ellipsis span."
  (let ((parts (split-string cue "\\(?:\\.\\.\\.\\|…\\)" t "[ \t\n]+")))
    (when parts
      (concat "\\_<"
              (mapconcat #'regexp-quote parts "\\(?:.\\|\n\\)\\{0,40\\}")
              "\\_>"))))

(defun session-mode--empty-rnode-cue-store ()
  "Return a fresh empty R-node cue store."
  (list (cons 'version 1)
        (cons 'entries nil)
        (cons 'conflicts nil)
        (cons 'corrections nil)))

(defun session-mode--read-rnode-cue-store ()
  "Read the R-node cue store; return nil on corrupt input without throwing."
  (if (not (file-exists-p session-mode-rnode-cues-file))
      (session-mode--empty-rnode-cue-store)
    (condition-case err
        (let ((json-object-type 'alist) (json-array-type 'list))
          (let ((data (json-read-file session-mode-rnode-cues-file)))
            (unless (= 1 (alist-get 'version data))
              (error "unsupported version"))
            data))
      (error
       (unless session-mode--rnode-store-error-reported
         (setq session-mode--rnode-store-error-reported t)
         (message "session-mode: cannot read R-node cue store %s: %s"
                  session-mode-rnode-cues-file (error-message-string err)))
       nil))))

(defun session-mode--write-rnode-cue-store (data)
  "Atomically write R-node cue store DATA."
  (let* ((file (expand-file-name session-mode-rnode-cues-file))
         (directory (file-name-directory file)) temp
         (entries
          (mapcar
           (lambda (entry)
             (let ((copy (copy-tree entry)))
               (dolist (field '(turns seats seats_by_turn justifications))
                 (setf (alist-get field copy) (vconcat (alist-get field copy))))
               copy))
           (alist-get 'entries data)))
         (json `((version . 1)
                 (entries . ,(vconcat entries))
                 (conflicts . ,(vconcat (alist-get 'conflicts data)))
                 (corrections . ,(vconcat (alist-get 'corrections data))))))
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".rnode-cues-" directory)))
          (with-temp-file temp
            (insert (json-encode json) "\n"))
          (rename-file temp file t)
          (setq temp nil))
      (when (and temp (file-exists-p temp)) (delete-file temp)))
    (setq session-mode--learned-rnode-key nil
          session-mode--learned-rnode-vocabulary nil
          session-mode--rnode-store-error-reported nil)))

(defun session-mode--xiang-precision (source)
  "Return configured precision for evidence SOURCE."
  (float (or (alist-get source session-mode-xiang-source-precision) 0.0)))

(defun session-mode--xiang-entry-sources (entry)
  "Describe the independent precision sources represented by ENTRY."
  (let (seen sources)
    (cl-mapc
     (lambda (_turn seat)
       (let ((source (cond ((null sources) 'first-proposal)
                           ((member seat seen) 'same-seat-repeat)
                           (t 'second-distinct-seat))))
         (push seat seen)
         (setq sources (append sources (list source)))))
     (alist-get 'turns entry) (alist-get 'seats_by_turn entry))
    ;; Version-1 stores have only distinct seats. Reconstruct the only ordering
    ;; they retained: first seat, then repeats, with each later seat distinct.
    (unless sources
      (let ((turns (alist-get 'turns entry)) (seats (alist-get 'seats entry)))
        (dotimes (i (length turns))
          (setq sources
                (append sources
                        (list (cond ((zerop i) 'first-proposal)
                                    ((< i (length seats)) 'second-distinct-seat)
                                    (t 'same-seat-repeat))))))))
    sources))

(defun session-mode--xiang-belief (text entries corrections)
  "Return the Dirichlet belief and R7 source account for cue TEXT."
  (let ((alpha '((none . 1.0))) sources)
    (dolist (entry entries)
      (when (equal text (alist-get 'text entry))
        (let* ((node (intern (alist-get 'node entry)))
               (prior (if (eq t (alist-get 'seed entry)) 2.0 0.0)))
          (setf (alist-get node alpha nil nil #'eq)
                (+ prior (or (alist-get node alpha nil nil #'eq) 0.0)))
          (dolist (source (session-mode--xiang-entry-sources entry))
            (let ((weight (session-mode--xiang-precision source)))
              (cl-incf (alist-get node alpha nil nil #'eq) weight)
              (push `((source . ,(symbol-name source)) (node . ,(symbol-name node))
                      (weight . ,weight)) sources))))))
    (dolist (correction corrections)
      (when (equal text (alist-get 'text correction))
        (let* ((name (or (alist-get 'node correction) "none"))
               (node (intern name)) (weight (session-mode--xiang-precision
                                              'operator-correction)))
          (cl-incf (alist-get node alpha 0.0 nil #'eq) weight)
          (push `((source . "operator-correction") (node . ,name)
                  (weight . ,weight)) sources))))
    (list alpha (nreverse sources))))

(defun session-mode--xiang-entropy (probabilities)
  "Return entropy in nats for PROBABILITIES."
  (- (apply #'+ (mapcar (lambda (p) (if (> p 0.0) (* p (log p)) 0.0))
                         probabilities))))

(defun session-mode--xiang-epistemic (alphas)
  "Expected entropy reduction from one Dirichlet-multinomial observation.
This exact one-step calculation compares entropy of the current posterior mean
with predictive-probability-weighted entropy after incrementing each outcome."
  (let* ((values (mapcar (lambda (pair) (float (cdr pair))) alphas))
         (total (apply #'+ values))
         (before (session-mode--xiang-entropy
                  (mapcar (lambda (a) (/ a total)) values)))
         (after
          (cl-loop for observed from 0 below (length values)
                   for predictive = (/ (nth observed values) total)
                   sum (* predictive
                          (session-mode--xiang-entropy
                           (cl-loop for a in values for i from 0
                                    collect (/ (+ a (if (= i observed) 1.0 0.0))
                                               (1+ total))))))))
    (max 0.0 (- before after))))

(defun session-mode--xiang-seeded-unit (alphas)
  "Return a stable pseudo-random unit value derived solely from ALPHAS."
  (let* ((ordered (sort (copy-sequence alphas)
                        (lambda (a b) (string< (symbol-name (car a))
                                                (symbol-name (car b))))))
         (seed (mapconcat (lambda (pair) (format "%s:%.6f" (car pair) (cdr pair)))
                          ordered ","))
         ;; The leading digest word is a reproducible draw without global RNG state.
         (prefix (substring (secure-hash 'sha256 seed) 0 8)))
    (/ (string-to-number prefix 16) 4294967296.0)))

(defun session-mode--xiang-choose (options alphas)
  "Choose one of OPTIONS by softmax over -G, deterministically seeded by ALPHAS."
  (if (<= session-mode-xiang-temperature 0)
      (car (sort (copy-sequence options)
                 (lambda (a b) (< (alist-get 'G a) (alist-get 'G b)))))
    (let* ((tau (float session-mode-xiang-temperature))
           (minimum (apply #'min (mapcar (lambda (o) (alist-get 'G o)) options)))
           (weights (mapcar (lambda (o) (exp (/ (- minimum (alist-get 'G o)) tau)))
                            options))
           (total (apply #'+ weights))
           (draw (* total (session-mode--xiang-seeded-unit alphas)))
           (remaining options) (remaining-weights weights) chosen)
      (while (and remaining (not chosen))
        (if (<= draw (car remaining-weights))
            (setq chosen (car remaining))
          (setq draw (- draw (car remaining-weights))
                remaining (cdr remaining)
                remaining-weights (cdr remaining-weights))))
      (or chosen (car (last options))))))

(defun session-mode--xiang-log (record)
  "Append one JSON decision RECORD, failing softly."
  (condition-case err
      (let* ((file (expand-file-name session-mode-xiang-decisions-file))
             (directory (file-name-directory file)))
        (make-directory directory t)
        (write-region (concat (json-encode record) "\n") nil file t 'silent))
    (error (message "session-mode: could not append 象 decision: %s"
                    (error-message-string err)) nil)))

(defun session-mode--xiang-cue-policy (text entries corrections)
  "Score, select and log the R-node policy for TEXT; return chosen option."
  (let* ((belief (session-mode--xiang-belief text entries corrections))
         (alphas (car belief)) (sources (cadr belief))
         (total (apply #'+ (mapcar #'cdr alphas)))
         (top (car (sort (copy-sequence alphas)
                         (lambda (a b) (> (cdr a) (cdr b))))))
         (top-name (symbol-name (car top)))
         (p-top (/ (cdr top) total))
         (epistemic (session-mode--xiang-epistemic alphas))
         (active (seq-some (lambda (e) (and (equal text (alist-get 'text e))
                                             (equal "active" (alist-get 'status e))))
                           entries))
         (node-top (not (equal top-name "none")))
         (promote-risk (* (if node-top (- 1.0 p-top) p-top)
                          session-mode-xiang-cost-wrong-red))
         (keep-risk (* p-top (if node-top session-mode-xiang-cost-missed-red
                               session-mode-xiang-cost-wrong-red)))
         (options (list `((option . "promote") (risk . ,promote-risk)
                          (epistemic . 0.0) (G . ,promote-risk))
                        `((option . "keep") (risk . ,keep-risk)
                          (epistemic . ,epistemic)
                          (G . ,(- keep-risk epistemic)))))
         chosen-row chosen correction-none changed)
    (when active
      (let ((risk (* (if node-top p-top (- 1.0 p-top))
                     session-mode-xiang-cost-missed-red 2.0)))
        (setq options
              (list (car options)
                    `((option . "retire") (risk . ,risk)
                      (epistemic . 0.0) (G . ,risk))
                    (cadr options)))))
    (setq chosen-row (session-mode--xiang-choose options alphas)
          chosen (alist-get 'option chosen-row)
          correction-none
          (seq-some (lambda (c) (and (equal text (alist-get 'text c))
                                     (equal "none" (alist-get 'node c))))
                    corrections))
    (cond
     ((and (equal chosen "promote") node-top)
      (dolist (entry entries)
        (when (equal text (alist-get 'text entry))
          (let ((status (if (equal top-name (alist-get 'node entry))
                            "active" "candidate")))
            (unless (equal status (alist-get 'status entry)) (setq changed t))
            (setf (alist-get 'status entry) status)))))
     ((equal chosen "keep")
      (dolist (entry entries)
        (when (and (equal text (alist-get 'text entry))
                   (not (eq t (alist-get 'seed entry))))
          (unless (equal "candidate" (alist-get 'status entry)) (setq changed t))
          (setf (alist-get 'status entry) "candidate"))))
     ((and (equal chosen "retire") active)
      (dolist (entry entries)
        (when (and (equal text (alist-get 'text entry))
                   (or (not (eq t (alist-get 'seed entry))) correction-none))
          (unless (equal "retired" (alist-get 'status entry)) (setq changed t))
          (setf (alist-get 'status entry) "retired")))))
    (session-mode--xiang-log
     `((at . ,(format-time-string "%FT%TZ" nil t)) (kind . "cue") (subject . ,text)
       (options . ,(vconcat options)) (tau . ,session-mode-xiang-temperature)
       (chosen . ,chosen)
       (rnode_terms . ((R1 . ((top . ,top-name) (p_top . ,p-top)
                              (alpha . ,alphas)))
                       (R7 . ,(vconcat sources)) (R6 . ,(vconcat (mapcar
                                                                  (lambda (o) (alist-get 'option o))
                                                                  options)))
                       (R5 . ,(vconcat (mapcar (lambda (o) (alist-get 'G o)) options)))
                       (R14 . ,session-mode-xiang-temperature)
                       (R3/R17 . ((changed . ,(if changed t :json-false))
                                  (status_change . ,(if changed chosen "none"))))))))
    (list chosen changed options alphas)))

(defun session-mode--rnode-conflicts (entries)
  "Return display conflicts among ENTRIES without affecting policy selection."
  (let ((by-text (make-hash-table :test #'equal)) conflicts)
    (dolist (entry entries) (push (alist-get 'node entry)
                                  (gethash (alist-get 'text entry) by-text)))
    (maphash (lambda (text nodes)
               (setq nodes (delete-dups nodes))
               (when (> (length nodes) 1)
                 (push `((text . ,text) (nodes . ,(vconcat (sort nodes #'string<))))
                       conflicts)))
             by-text)
    (nreverse conflicts)))

(defun session-mode--xiang-turn-rnode-info (text)
  "Sum one-step information value for uncertain stored cues present in TEXT."
  (let ((store (session-mode--read-rnode-cue-store)) (case-fold-search t) total seen)
    (when store
      (dolist (entry (alist-get 'entries store))
        (let ((cue (alist-get 'text entry)))
          (when (and (not (member cue seen))
                     (string-match-p (regexp-quote cue) text))
            (push cue seen)
            (pcase-let ((`(,alphas ,_sources)
                         (session-mode--xiang-belief
                          cue (alist-get 'entries store) (alist-get 'corrections store))))
              (setq total (+ (or total 0.0)
                             (session-mode--xiang-epistemic alphas))))))))
      (dolist (row (session-mode--load-rnode-vocabulary))
        (dolist (compiled (nth 3 row))
          (let ((cue (downcase (car compiled))))
            (when (and (not (member cue seen))
                       (string-match-p (nth 1 compiled) text))
              (push cue seen)
              (let* ((seed-entry
                      (list (cons 'text cue) (cons 'node (car row)) (cons 'seed t)
                            (cons 'turns nil) (cons 'seats nil)
                            (cons 'seats_by_turn nil)))
                     (entries (alist-get 'entries store))
                     (has-seed (seq-some
                                (lambda (entry)
                                  (and (equal cue (alist-get 'text entry))
                                       (equal (car row) (alist-get 'node entry))
                                       (eq t (alist-get 'seed entry))))
                                entries))
                     (belief (session-mode--xiang-belief
                              cue (if has-seed entries (cons seed-entry entries))
                              (alist-get 'corrections store))))
                (setq total (+ (or total 0.0)
                               (session-mode--xiang-epistemic (car belief)))))))))
    (or total 0.0)))

(defun session-mode--xiang-turn-policy (text &optional evidence-id)
  "Choose and log `ask' or `cue-only' for operator turn TEXT."
  (let* ((substantive (not (string-empty-p (string-trim (or text "")))))
         (rnode-info (if substantive (session-mode--xiang-turn-rnode-info text) 0.0))
         (intent-value (if substantive session-mode-xiang-intent-value 0.0))
         (ask-g (- session-mode-xiang-token-cost (+ intent-value rnode-info)))
         (options (list `((option . "ask") (risk . ,session-mode-xiang-token-cost)
                          (epistemic . ,(+ intent-value rnode-info)) (G . ,ask-g))
                        '((option . "cue-only") (risk . 0.0)
                          (epistemic . 0.0) (G . 0.0))))
         (seed `((ask . ,(+ 1.0 intent-value rnode-info))
                 (cue-only . 1.0)))
         (chosen-row (session-mode--xiang-choose options seed))
         (chosen (alist-get 'option chosen-row)))
    (session-mode--xiang-log
     `((at . ,(format-time-string "%FT%TZ" nil t)) (kind . "turn")
       (subject . ,(or evidence-id "unrecorded")) (options . ,(vconcat options))
       (tau . ,session-mode-xiang-temperature) (chosen . ,chosen)
       (rnode_terms . ((R1 . ((substantive . ,(if substantive t :json-false))))
                       (R7 . []) (R6 . ["ask" "cue-only"])
                       (R5 . [,ask-g 0.0]) (R14 . ,session-mode-xiang-temperature)
                       (R3/R17 . ((changed . :json-false)))))))
    chosen))

(defun session-mode--record-rnode-cues (data)
  "Merge validated R-node cues from analysis DATA into the recurrence store.
Returns non-nil when the store was updated; every error is reported softly."
  (condition-case err
      (let* ((store (session-mode--read-rnode-cue-store))
             (entries (and store (alist-get 'entries store)))
             (evidence-id (alist-get 'evidence_id data))
             (seat (alist-get 'labeller data))
             (seen-at (or (alist-get 'created_at data)
                          (format-time-string "%FT%TZ" nil t)))
             changed)
        (when (and store (stringp evidence-id) (not (string-empty-p evidence-id))
                   (stringp seat) (not (string-empty-p seat)))
          (dolist (cue (alist-get 'rnode_cues data))
            (let* ((text (downcase (string-trim (alist-get 'text cue))))
                   (node (alist-get 'node cue))
                   (seed (session-mode--rnode-seed-definition text))
                   (entry (seq-find
                           (lambda (e) (and (equal text (alist-get 'text e))
                                            (equal node (alist-get 'node e))))
                           entries)))
              (unless entry
                (setq entry (list (cons 'text text)
                                  (cons 'node node)
                                  (cons 'label (or (nth 1 seed) (alist-get 'label cue)))
                                  (cons 'stage (or (nth 2 seed) (alist-get 'stage cue)))
                                  (cons 'seed (if (and seed (equal node (car seed)))
                                                  t :json-false))
                                  (cons 'proposals 0)
                                  (cons 'turns nil)
                                  (cons 'seats nil)
                                  (cons 'seats_by_turn nil)
                                  (cons 'first_seen seen-at)
                                  (cons 'last_seen seen-at)
                                  (cons 'justifications nil)
                                  (cons 'status (if (and seed (equal node (car seed)))
                                                    "active" "candidate")))
                      entries (append entries (list entry))))
              (unless (member evidence-id (alist-get 'turns entry))
                (when (and (alist-get 'turns entry)
                           (null (alist-get 'seats_by_turn entry)))
                  (let ((known (alist-get 'seats entry)))
                    (setf (alist-get 'seats_by_turn entry)
                          (cl-loop for i below (length (alist-get 'turns entry))
                                   collect (or (nth i known) (car known))))))
                (cl-incf (alist-get 'proposals entry))
                (setf (alist-get 'turns entry)
                      (append (alist-get 'turns entry) (list evidence-id))
                      (alist-get 'seats_by_turn entry)
                      (append (alist-get 'seats_by_turn entry) (list seat))
                      (alist-get 'last_seen entry) seen-at)
                (unless (member seat (alist-get 'seats entry))
                  (setf (alist-get 'seats entry)
                        (append (alist-get 'seats entry) (list seat))))
                (let ((justification (alist-get 'justification cue)))
                  (when (and (stringp justification)
                             (not (member justification (alist-get 'justifications entry)))
                             (< (length (alist-get 'justifications entry)) 3))
                    (setf (alist-get 'justifications entry)
                          (append (alist-get 'justifications entry)
                                  (list justification)))))
                (setq changed t))))
          (when changed
            (setf (alist-get 'entries store) entries
                  (alist-get 'conflicts store)
                  (session-mode--rnode-conflicts entries))
            (dolist (text (delete-dups
                           (mapcar (lambda (cue)
                                     (downcase (string-trim (alist-get 'text cue))))
                                   (alist-get 'rnode_cues data))))
              (session-mode--xiang-cue-policy
               text entries (alist-get 'corrections store)))
            (session-mode--write-rnode-cue-store store)))
        changed)
    (error
     (message "session-mode: could not record R-node cues: %s"
              (error-message-string err))
     nil)))

(defun session-mode--rnode-seed-definition (text)
  "Return static vocabulary metadata when TEXT is a generated seed cue."
  (condition-case nil
      (let ((json-object-type 'alist) (json-array-type 'list) found)
        (dolist (row (alist-get 'nodes (json-read-file session-mode-rnode-vocabulary-file)))
          (when (seq-some (lambda (cue) (equal (downcase cue) text))
                          (alist-get 'cues row))
            (setq found (list (alist-get 'id row) (alist-get 'label row)
                              (alist-get 'stage row)))))
        found)
    (error nil)))

(defun session-mode--rnode-node-definition (node)
  "Return (NODE LABEL STAGE) from the generated vocabulary."
  (condition-case nil
      (let ((json-object-type 'alist) (json-array-type 'list))
        (when-let* ((row (seq-find
                          (lambda (candidate) (equal node (alist-get 'id candidate)))
                          (alist-get 'nodes (json-read-file
                                             session-mode-rnode-vocabulary-file)))))
          (list node (alist-get 'label row) (alist-get 'stage row))))
    (error nil)))

(defun session-mode-xiang-correct-rnode (text node-or-none)
  "Record Joe's correction of cue TEXT to NODE-OR-NONE and rerun its policy."
  (interactive
   (list (downcase (string-trim (read-string "R-node cue text: ")))
         (completing-read "Correct node (or none): "
                          (cons "none"
                                (condition-case nil
                                    (let ((json-object-type 'alist)
                                          (json-array-type 'list))
                                      (mapcar (lambda (row) (alist-get 'id row))
                                              (alist-get 'nodes
                                                         (json-read-file
                                                          session-mode-rnode-vocabulary-file))))
                                  (error nil)))
                          nil t nil nil "none")))
  (setq text (downcase (string-trim text))
        node-or-none (if (or (null node-or-none) (equal node-or-none "none"))
                         "none" node-or-none))
  (condition-case err
      (let* ((store (session-mode--read-rnode-cue-store))
             (entries (and store (alist-get 'entries store)))
             (corrections (and store (alist-get 'corrections store)))
             (seed (session-mode--rnode-seed-definition text))
             (definition (and (not (equal node-or-none "none"))
                              (session-mode--rnode-node-definition node-or-none)))
             (existing (seq-find (lambda (entry)
                                   (and (equal text (alist-get 'text entry))
                                        (equal (if (equal node-or-none "none")
                                                   (car seed) node-or-none)
                                               (alist-get 'node entry))))
                                 entries)))
        (when (and (not (equal node-or-none "none")) (not definition))
          (user-error "Unknown R-node %s" node-or-none))
        (when (and store (not (string-empty-p text)))
          (unless (or existing (and (equal node-or-none "none") (not seed)))
            (let ((node (if (equal node-or-none "none") (car seed) node-or-none)))
              (setq existing
                    (list (cons 'text text) (cons 'node node)
                          (cons 'label (nth 1 (or seed definition)))
                          (cons 'stage (nth 2 (or seed definition)))
                          (cons 'seed (if seed t :json-false))
                          (cons 'proposals 0) (cons 'turns nil) (cons 'seats nil)
                          (cons 'seats_by_turn nil) (cons 'first_seen nil)
                          (cons 'last_seen nil) (cons 'justifications nil)
                          (cons 'status (if seed "active" "candidate")))
                    entries (append entries (list existing)))))
          (setq corrections
                (append corrections
                        (list `((text . ,text) (node . ,node-or-none)
                                (at . ,(format-time-string "%FT%TZ" nil t))))))
          (setf (alist-get 'entries store) entries
                (alist-get 'corrections store) corrections
                (alist-get 'conflicts store) (session-mode--rnode-conflicts entries))
          (session-mode--xiang-cue-policy text entries corrections)
          (session-mode--write-rnode-cue-store store)
          t))
    (error (message "session-mode: could not correct R-node cue: %s"
                    (error-message-string err)) nil)))

(defun session-mode--load-learned-rnode-vocabulary ()
  "Return compiled active cues from the recurrence store, failing softly."
  (let* ((attrs (file-attributes session-mode-rnode-cues-file))
         (key (list session-mode--rnode-cache-format-version
                    session-mode-rnode-cues-file
                    (and attrs (file-attribute-modification-time attrs)))))
    (unless (equal key session-mode--learned-rnode-key)
      (let ((store (session-mode--read-rnode-cue-store)))
        (setq session-mode--learned-rnode-vocabulary
              (delq nil
                    (mapcar
                     (lambda (entry)
                       (when (equal "active" (alist-get 'status entry))
                         (when-let* ((rx (session-mode--rnode-cue-regexp
                                          (alist-get 'text entry))))
                           (list (alist-get 'node entry) (alist-get 'label entry)
                                 (alist-get 'stage entry)
                                 (list (list (alist-get 'text entry) rx "learned"))))))
                     (and store (alist-get 'entries store))))
              session-mode--learned-rnode-key key)))
    session-mode--learned-rnode-vocabulary))

(defun session-mode--load-rnode-vocabulary ()
  "Load and compile the generated R-node vocabulary, failing softly."
  (if (not (file-readable-p session-mode-rnode-vocabulary-file))
      (progn
        (unless session-mode--rnode-missing-reported
          (setq session-mode--rnode-missing-reported t)
          (message "session-mode: R-node vocabulary missing at %s"
                   session-mode-rnode-vocabulary-file))
        (setq session-mode--rnode-vocabulary nil
              session-mode--rnode-vocabulary-key nil)
        nil)
    (let* ((store-attrs (file-attributes session-mode-rnode-cues-file))
           (key (list session-mode--rnode-cache-format-version
                      session-mode-rnode-vocabulary-file
                      (file-attribute-modification-time
                       (file-attributes session-mode-rnode-vocabulary-file))
                      (and store-attrs (file-attribute-modification-time store-attrs)))))
      (unless (equal key session-mode--rnode-vocabulary-key)
        (let* ((json-object-type 'alist) (json-array-type 'list)
               (store (session-mode--read-rnode-cue-store))
               (retired-seeds
                (mapcar (lambda (entry) (cons (alist-get 'node entry)
                                              (alist-get 'text entry)))
                        (seq-filter (lambda (entry)
                                      (and (eq t (alist-get 'seed entry))
                                           (equal "retired" (alist-get 'status entry))))
                                    (and store (alist-get 'entries store))))))
          (setq session-mode--rnode-vocabulary
                (mapcar
                 (lambda (row)
                   (let ((id (alist-get 'id row))
                         (label (alist-get 'label row))
                         (stage (alist-get 'stage row)))
                     (list id label stage
                           (delq nil
                                 (mapcar
                                  (lambda (cue)
                                    (unless (member (cons id (downcase cue)) retired-seeds)
                                      (when-let* ((rx (session-mode--rnode-cue-regexp cue)))
                                        (list cue rx "provisional"))))
                                  (alist-get 'cues row))))))
                 (alist-get 'nodes (json-read-file session-mode-rnode-vocabulary-file)))
                session-mode--rnode-vocabulary-key key
                session-mode--rnode-missing-reported nil)))
      session-mode--rnode-vocabulary)))

(defcustom session-mode-rnode-red t
  "When non-nil, R-node cue tags are plain red text, so they stand out while the
vocabulary is being tried; when nil, a dotted underline in the stage colour."
  :type 'boolean :group 'session-mode)

(defface session-mode-rnode-red-face '((t :foreground "red" :underline nil))
  "R-node cue tag while `session-mode-rnode-red' is on.  Red alone marks it: the
explicit nil underline, at a priority above the intent tags, removes theirs.")

(defun session-mode--rnode-stage-face (stage)
  "Return the R-node tag face: red while `session-mode-rnode-red', else a
dotted underline in STAGE's transcript colour."
  (if session-mode-rnode-red 'session-mode-rnode-red-face
  (let* ((face-stage (if (equal stage "assurance") "annotator" stage))
         (face (intern (format "session-mode-mark-%s-face" face-stage)))
         (colour (face-foreground face nil t)))
    `(:underline (:style dots :color ,colour)))))

(defun session-mode--paint-rnode-region (beg region-end vocab intent-phrases)
  "Paint VOCAB in one operator region, excluding its quoted tail."
  (let ((end (save-excursion
               (goto-char beg)
               (if (re-search-forward "^[ \t]*>>>" region-end t)
                   (match-beginning 0)
                 region-end)))
        (seen (make-hash-table :test #'equal)))
    (dolist (row vocab)
      (pcase-let ((`(,id ,label ,stage ,cues) row))
        (dolist (cue cues)
          (let ((key (list id (downcase (car cue)))))
            (unless (or (gethash (downcase (car cue)) intent-phrases)
                        (gethash key seen))
              (puthash key t seen)
              (save-excursion
                (goto-char beg)
                (while (re-search-forward (nth 1 cue) end t)
                  (unless (session-mode--overlays-with-property
                           (match-beginning 0) (match-end 0) 'session-mode-turn-tag)
                  (let ((o (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put o 'session-mode-rnode-tag t)
                    (overlay-put o 'evaporate t)
                    (overlay-put o 'priority 40) ; above intent tags (30)
                    (overlay-put o 'face (session-mode--rnode-stage-face stage))
                    (overlay-put o 'help-echo
                                 (format "%s %s (%s) — cue “%s” — %s"
                                         id label (upcase stage) (car cue)
                                         (nth 2 cue))))))))))))))

(defun session-mode--overlays-with-property (beg end prop)
  "Return overlays carrying PROP that share a character with BEG..END."
  (seq-filter (lambda (o) (and (overlay-get o prop)
                               (< (overlay-start o) end)
                               (> (overlay-end o) beg)))
              (overlays-in beg end)))

(defun session-mode--paint-rnode-tags (jit-beg jit-end)
  "Paint R-node cues in operator regions overlapping JIT-BEG..JIT-END."
  ;; Each overlapping operator region is repainted whole, so a JIT boundary
  ;; never splits a multiword cue and repeated calls stay idempotent; regions
  ;; outside the chunk are left alone, so typing does not rescan the buffer.
  (let ((case-fold-search t)
        (vocab (and session-mode-rnode-tags
                    (append (session-mode--load-learned-rnode-vocabulary)
                            (session-mode--load-rnode-vocabulary))))
        ;; Intent phrases mark conversational acts, not R-node quantities.
        (intent-phrases (let ((h (make-hash-table :test 'equal)))
                          (dolist (group session-mode-turn-vocabulary h)
                            (dolist (phrase (cdr group))
                              (puthash (downcase phrase) t h))))))
    (pcase-dolist (`(,beg . ,region-end) (session-mode--operator-regions))
      (when (and (< beg jit-end) (> region-end jit-beg))
        (remove-overlays beg region-end 'session-mode-rnode-tag t)
        (when vocab
          (session-mode--paint-rnode-region beg region-end vocab intent-phrases))))))

(define-minor-mode session-mode-turn-tags-mode
  "Underline phrase cues without inserting a draft classification summary.
Also annotate the latest sent operator turn.  Drafts use local cues only;
sent turns can request agent interpretation through `session-turn-analysis'.
Kept separate from full session markup so typing never triggers retrieval."
  ;; A 象 says this buffer's operator turns are on the record: captured and
  ;; sent for interpretation.  No 象 means off the record.  Red means the
  ;; interpretation is working or not yet known to fail; pink means the last
  ;; dispatch or reap failed (see `session-mode--analysis-lighter').
  :lighter (:eval (session-mode--analysis-lighter))
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
        (jit-lock-register #'session-mode--paint-marks)
        (jit-lock-register #'session-mode--paint-rnode-tags)
        (session-mode--paint-rnode-tags (point-min) (point-max))
        (session-mode-turn-tags-refresh))
    (jit-lock-unregister #'session-mode--paint-marks)
    (jit-lock-unregister #'session-mode--paint-rnode-tags)
    (remove-overlays (point-min) (point-max) 'session-mode-mark t)
    (remove-overlays (point-min) (point-max) 'session-mode-rnode-tag t)
    (remove-hook 'after-change-functions #'session-mode--tags-after-change t)
    (remove-hook 'post-command-hook #'session-mode--refresh-analysis-on-navigation t)
    (remove-hook 'kill-buffer-hook #'session-mode--cancel-tag-timer t)
    (session-mode--cancel-tag-timer)
    (mapc #'delete-overlay (append session-mode--draft-tag-overlays session-mode--sent-tag-overlays
                                   (bound-and-true-p session-mode--past-tag-overlays)))
    (when (boundp 'session-mode--past-tag-overlays) (setq session-mode--past-tag-overlays nil))
    (setq session-mode--draft-tag-overlays nil session-mode--sent-tag-overlays nil
          session-mode--draft-tags nil)))

(defvar session-mode--analysis-health)        ; session-turn-analysis.el
(defvar session-mode--analysis-health-detail)

(defvar session-mode--xiang-off)              ; session-turn-analysis.el

(defun session-mode--analysis-lighter ()
  "The 象 lighter: red while turns are sent for interpretation, pink while
delegated analysis is known to be failing, grey while `象-off' holds."
  (let ((failing (eq session-mode--analysis-health 'failing))
        (off (bound-and-true-p session-mode--xiang-off))
        (autorunner (bound-and-true-p codex-repl--autorunner-enabled)))
    (propertize " 象"
                'face `(:foreground ,(cond ((or off autorunner) "gray50")
                                           (failing "hot pink") (t "red")))
                'help-echo
                (concat (cond
                         (off (format "象 is OFF: turns are captured, not sent.\nRe-arm when: %s\nM-x 象-on to resume"
                                      (plist-get off :rearm)))
                         (autorunner "Autorunner on: its repeated prompt is captured, not sent.\nOther text you type is still read; M-x stop-codex-autorunner resumes")
                         (failing "Turns are captured, but interpretation is FAILING")
                         (t "Turns are captured and sent for interpretation"))
                        (if session-mode--analysis-health-detail
                            (concat "\nLast: " session-mode--analysis-health-detail)
                          "")))))

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
                     (author . "joe")
                     (origin . ,(agent-turn-origin-stamp "joe" "session-mode/correction"
                                 '(:kind "operator" :actor "joe")))
                     (text . ,text)
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
