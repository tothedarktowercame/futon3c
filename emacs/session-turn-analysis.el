;;; session-turn-analysis.el --- Standoff structure for sent turns -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defvar agent-chat--last-evidence-id nil
  "Most recently acknowledged chat evidence id in the current REPL buffer.")

(defcustom session-mode-turn-analysis-directory
  (expand-file-name "session-turn-analysis" user-emacs-directory)
  "Private durable records for operator passages, cues and agent interpretations."
  :type 'directory :group 'session-mode)

(defcustom session-mode-turn-analysis-policy 'all
  "Which sent operator turns request interpretation from their receiving agent.
`all' analyzes every ordinary turn; `unmatched' requests only sentences with
no lexical cues; `never' records structure without requesting interpretation."
  :type '(choice (const all) (const unmatched) (const never)) :group 'session-mode)

(defvar session-mode--xiang-off nil
  "Plist (:since TIME :rearm TEXT :policy OLD) while 象 is switched off.")

(defun session-mode--xiang-off-log ()
  (expand-file-name "xiang-off.log" session-mode-turn-analysis-directory))

(defun 象-off (rearm)
  "Stop sending turns to the 象 seats until REARM holds.
REARM is the condition, in writing, under which 象 comes back on
(war-room/wr-26); it is required.  Turns are still recorded.  The
seats stay registered; a reading already running finishes.  Lasts
until `象-on' or an Emacs restart."
  (interactive (list (read-string "Re-arm 象 when: ")))
  (when (string-blank-p rearm)
    (user-error "象-off needs a re-arm condition"))
  (unless session-mode--xiang-off
    (setq session-mode--xiang-off
          (list :since (current-time) :rearm rearm
                :policy session-mode-turn-analysis-policy)))
  (setq session-mode--xiang-off (plist-put session-mode--xiang-off :rearm rearm)
        session-mode-turn-analysis-policy 'never)
  (make-directory session-mode-turn-analysis-directory t)
  (write-region (format "%s off  re-arm when: %s\n"
                        (format-time-string "%FT%T%z") rearm)
                nil (session-mode--xiang-off-log) t 'silent)
  (message "象 off. Re-arm when: %s" rearm))

(defun 象-on ()
  "Resume sending turns to the 象 seats, undoing `象-off'."
  (interactive)
  (if (not session-mode--xiang-off)
      (message "象 is not switched off (policy: %s)"
               session-mode-turn-analysis-policy)
    (let ((rearm (plist-get session-mode--xiang-off :rearm)))
      (setq session-mode-turn-analysis-policy
            (plist-get session-mode--xiang-off :policy)
            session-mode--xiang-off nil)
      (write-region (format "%s on   (was: %s)\n"
                            (format-time-string "%FT%T%z") rearm)
                    nil (session-mode--xiang-off-log) t 'silent)
      (message "象 on (policy: %s)" session-mode-turn-analysis-policy))))

(defun session-mode--analysis-requested-p (record)
  "Whether RECORD should request interpretation under the current policy."
  (and (or (eq session-mode-turn-analysis-policy 'all)
           (and (eq session-mode-turn-analysis-policy 'unmatched)
                (> (length (alist-get 'unmatched record)) 0)))
       (equal "ask"
              (session-mode--xiang-turn-policy
               (or (alist-get 'source_text record) "")
               (or (alist-get 'evidence_id record)
                   agent-chat--last-evidence-id)))))

(defun session-mode--record-requests-analysis-p (path)
  "Return whether the already-scored turn record at PATH requests analysis."
  (condition-case nil
      (let ((json-object-type 'alist))
        (equal "requested" (alist-get 'analysis_status (json-read-file path))))
    (error nil)))

(defconst session-mode--analysis-tool
  (expand-file-name "../scripts/session_turn_analysis.py"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defvar-local session-mode--last-analysis-request nil)

(defcustom session-mode-secret-scan-script
  (expand-file-name "../scripts/secret_scan.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the standalone scanner applied before an operator turn is captured.
This protects the capture record, the analysis seat 象, and public feeds built
from capture records.  The agent addressed by the operator has already received
the turn; preventing that delivery is outside this capture-time safeguard."
  :type 'string
  :group 'session-mode)

(defun session-mode--redact-secrets (text)
  "Return (REDACTED . KINDS) after scanning TEXT with the standalone tool.
KINDS contains distinct redaction kinds in first-seen order.  Signal a
`user-error' without including TEXT when the scanner cannot establish a safe
result; an unscanned turn must not be recorded or dispatched to 象."
  (unless (and (stringp session-mode-secret-scan-script)
               (file-readable-p session-mode-secret-scan-script))
    (user-error "Secret scan failed closed: scanner script is missing or unreadable"))
  (let ((python (executable-find "python3")))
    (unless python
      (user-error "Secret scan failed closed: python3 is unavailable"))
    (let ((output (generate-new-buffer " *session-secret-scan*")) status)
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert (or text ""))
              (setq status
                    (call-process-region
                     (point-min) (point-max) python nil (list output nil) nil
                     session-mode-secret-scan-script)))
            (cond
             ((equal status 0) (cons text nil))
             ((equal status 1)
              (with-current-buffer output
                (let ((redacted (buffer-string)) kinds)
                  (goto-char (point-min))
                  (while (re-search-forward "\\[REDACTED:\\([^]]+\\)\\]" nil t)
                    (cl-pushnew (match-string-no-properties 1) kinds :test #'equal))
                  (unless kinds
                    (user-error "Secret scan failed closed: scanner reported findings without redactions"))
                  (cons redacted (nreverse kinds)))))
             (t
              (user-error "Secret scan failed closed: scanner exited with status %s"
                          status))))
        (kill-buffer output)))))

(defvar session-mode--withdrawal-disabled-messaged-sessions
  (make-hash-table :test #'equal)
  "Sessions already told once that inferred withdrawals lack a grant.")

(defconst session-mode-withdrawal-notice-max-attempts 5
  "Maximum publication or REPL-delivery attempts for one withdrawal outcome.")

(defconst session-mode-turn-interpretation-version 3
  "Version of the delegated interpretation brief and its withdraw semantics.")

(defun session-mode--sentence-spans (text)
  "Return sentence spans in TEXT, using zero-based Unicode character offsets."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((sentence-end-double-space nil) spans)
      (while (< (point) (point-max))
        (skip-chars-forward " \t\r\n")
        (let ((start (point)))
          (forward-sentence)
          (when (= start (point)) (goto-char (point-max)))
          (let* ((raw (buffer-substring-no-properties start (point)))
                 (clean (string-trim-right raw)))
            (unless (string-empty-p clean)
              (push `((start . ,(1- start)) (end . ,(+ (1- start) (length clean)))
                      (text . ,clean)) spans)))))
      (nreverse spans))))

(defun session-mode--structure-turn (text)
  "Record sentence structure and lexical observations, not inferred intent."
  (let ((matches (session-mode--turn-matches text)) (index 0) sentences gaps)
    (dolist (span (session-mode--sentence-spans text))
      (let* ((start (alist-get 'start span)) (end (alist-get 'end span))
             (hits (cl-remove-if-not
                    (lambda (h) (and (< (car h) end) (> (cadr h) start))) matches))
             (sid (format "s%d" (cl-incf index))))
        (unless hits (push sid gaps))
        (push (append `((id . ,sid) (status . ,(if hits "cue-only" "unresolved"))
                        (cues . ,(vconcat
                                  (mapcar (lambda (h)
                                            `((start . ,(car h)) (end . ,(cadr h))
                                              (label . ,(nth 2 h))
                                              (text . ,(substring text (car h) (cadr h)))
                                              (method . "literal-phrase"))) hits)))) span)
              sentences)))
    `((version . 1) (source_text . ,text)
      (offset_unit . "unicode-codepoints-zero-based-end-exclusive")
      (sentences . ,(vconcat (nreverse sentences)))
      (unmatched . ,(vconcat (nreverse gaps))))))

(defconst session-mode--quote-fence ">>>"
  "Line that opens, and optionally closes, a block quote in an operator turn.")

(defvar session-mode--last-quotes nil
  "Quoted blocks removed from the most recent turn, in the order they appeared.
The interpretation must not see them -- they are not Joe speaking -- but the
feed shows them, so the text has to survive somewhere. Kept beside the record
rather than inside `source_text', whose offsets every fragment is checked
against.")

(defun session-mode--elide-quotes (text)
  "Replace >>> blocks in TEXT with the single word QUOTE.

Joe's convention (2026-09-23): a >>> block is material he is showing the
agent -- a snippet, a rendering, someone else's words -- not something he
said. Interpreting it would tag its contents as his speech acts, so the
record keeps a placeholder and the interpretation has nothing to mistake.
A block runs to a closing >>> or, failing that, to the end of the turn."
  (setq session-mode--last-quotes nil)   ; a turn with no quote clears the last
  (if (not (string-match-p (concat "^[ \t]*" session-mode--quote-fence)
                           (or text "")))
      text
    (let* ((lines (split-string (or text "") "\n"))
           ;; The fence opens whether or not the quoted material starts on the
           ;; same line: Joe writes both ">>>" alone and ">>> Here's a ..."
           (opens (concat "^[ \t]*" session-mode--quote-fence))
           (closes (concat "^[ \t]*" session-mode--quote-fence "[ \t]*$"))
           (in-quote nil) (out '()) (quoted '()) (current '()))
      (dolist (line lines)
        (cond
         ((and in-quote (string-match-p closes line))
          (setq in-quote nil)
          (push (string-join (nreverse current) "\n") quoted)
          (setq current nil))
         (in-quote (push line current))  ; kept aside: it is not Joe speaking
         ((string-match-p opens line)
          (setq in-quote t)
          ;; content may follow the fence on its own line
          (let ((rest (string-trim (replace-regexp-in-string
                                    (concat "^[ \t]*" session-mode--quote-fence)
                                    "" line))))
            (when (not (string-empty-p rest)) (push rest current)))
          (push "QUOTE" out))
         (t (push line out))))
      (when current (push (string-join (nreverse current) "\n") quoted))
      (setq session-mode--last-quotes (nreverse quoted))
      (string-trim (string-join (nreverse out) "\n")))))

(defun session-mode--record-turn (text &optional failed original-text)
  "Persist TEXT's structure before requesting interpretation; return its path.
A leading surface marker is stripped first, so `source_text' and every offset
computed against it describe what the operator said rather than how it
reached the buffer. The surface itself is kept in the record's metadata.
Secrets are redacted before parsing, storage, analysis dispatch, or publication.
The addressed agent has already received the original turn and is out of scope."
  (let* ((text-scan (session-mode--redact-secrets text))
         (original-scan (and original-text
                             (session-mode--redact-secrets original-text)))
         (redaction-kinds (delete-dups
                           (append (copy-sequence (cdr text-scan))
                                   (copy-sequence (cdr original-scan)))))
         (text (car text-scan))
         (original-text (and original-scan (car original-scan)))
         (split (agent-chat-split-surface-marker text))
         (surface (car split))
         (text (session-mode--elide-quotes (cdr split)))
         (original-text (and original-text
                             (cdr (agent-chat-split-surface-marker original-text))))
         (record (session-mode--structure-turn text))
         (directory (file-name-as-directory session-mode-turn-analysis-directory)))
    (make-directory directory t)
    (set-file-modes directory #o700)
    (let ((path (make-temp-file (expand-file-name "turn-" directory) nil ".json"))
          ;; Capture buffer-local identity before with-temp-file changes buffers.
          (metadata `((created_at . ,(format-time-string "%FT%TZ" nil t))
                      (vocabulary_version . ,session-mode-turn-vocabulary-version)
                      (interpretation_version . ,session-mode-turn-interpretation-version)
                      (tagging_failed . ,(if failed t :json-false))
                      (original_text . ,(or original-text text))
                      (agent_id . ,agent-chat--agent-id)
                      (session_id . ,agent-chat--session-id)
                      (turn_id . ,agent-chat--current-turn-id)
                      (secrets_redacted . ,(vconcat redaction-kinds))
                      ;; `agent-chat--start-turn' runs :before-send before it
                      ;; calls us, so this is the acknowledged operator row,
                      ;; not an id reconstructed from text later.
                      (evidence_id . ,agent-chat--last-evidence-id)
                      (origin . "operator")
                      (surface . ,(if surface (symbol-name surface) "typed"))
                      (quotes . ,(vconcat session-mode--last-quotes))
                      (analysis_status . ,(if (or failed (session-mode--analysis-requested-p record))
                                             "requested" "not-requested")))))
      (condition-case err
          (with-temp-file path
            (let ((coding-system-for-write 'utf-8-unix))
              (insert (json-encode (append record metadata)))))
        (error (delete-file path) (signal (car err) (cdr err))))
      (setq session-mode--last-analysis-request path)
      (when redaction-kinds
        (display-warning
         'session-mode
         (format "Redacted secrets (%s) from your turn before capture; the turn itself still reached %s unredacted."
                 (string-join redaction-kinds ", ")
                 (or agent-chat--agent-id "the addressed agent"))
         :warning))
      path)))

(defun session-mode--analysis-instruction (path)
  "Give the current receiving agent a bounded task tied to PATH."
  (format
   (concat "\n\n[Session-mode structural analysis request — machine-added, not Joe's words]\n"
           "After handling the user's request, interpret the whole operator turn, including sentences with lexical cues. "
           "Do not delegate or start another conversation. Original text, offsets and unresolved sentences: %s\n"
           "Use python3 %s template REQUEST to obtain the JSON shape. "
           "Fill every sentence with one or more fragment annotations (multiple intents allowed), or an explicit unresolved reason. "
           "Interpretation spans can cover full sentences, but are NEVER themselves displayed as underlines. "
           "Each fragment must have a separate display_cues array of exact short keyword spans (at most 8 words / 80 characters each). "
           "Leave most of each long sentence unmarked. If intent is implicit, use an empty display_cues array and explain no_surface_cue. "
           "Each fragment has exact source offsets/text, intent, target, rationale and relations "
           "(context, condition, contrast, action, rationale, goal, or dependency). "
           "Use meaningful intent vocabulary; do not treat conjunctions alone as intent. "
           "Select content-bearing cues naming the action, object, constraint or success criterion, not just discourse openers like I wonder if. "
           "For pattern alignment, compare the full passage and target to the pattern context/IF/THEN, never match on the intent label alone. "
           "Suggested intents: %s. "
           "Intent withdraw means the operator ends or takes back an earlier act, his own or an agent's; it is not disagreement or redirection. "
           "The record may carry a happened_summary field: a machine-added note of what the agent did while answering this turn (first reply lines and commits with line counts). It is context for reading the turn, not the operator's words. "
           "For a withdraw fragment, set target to the named act id when the turn names one; set it to seat-active-card only when the turn refers to this/the pattern/card in the current seat; otherwise set target to null. Never guess a withdrawal target. "
           "A withdraw label is an interpretation only and terminates nothing. This brief is interpretation version %d. "
           "Candidate flexiarg refs are optional: read any cited canonical pattern and explain the fit; do not invent IDs. "
           "Record inferred interpretations, not human-approved labels. "
           "To improve future draft tagging, optionally propose top-level reusable_cues with exact start/end/text, intent and rationale for reuse. "
           "Propose only short communicative phrases that generalize, not project names or arbitrary subject words. "
           "Emacs persists unassigned phrases as provisional cue hypotheses with provenance; existing assignments and human corrections win. Do not edit the vocabulary file directly. "
           "R-node reading is optional and most fragments have none. Read /home/joe/code/futon0/analysis/audits/rnode-tree/rnode-definitions.edn once per seat session, and re-read it whenever unsure. "
           "A fragment may include rnode: {\"node\", \"quantity\", \"operation\", \"justification\"}; use a node from that file, one of its operations, and a one-line justification naming the quantity as in the admission example. "
           "Optionally propose top-level rnode_cues: [{\"text\", \"start\", \"end\", \"node\", \"operation\", \"justification\"}] using exact source spans. "
           "Seeds stay inside the definitions file: never show the operator cue lists. "
           "Save the filled JSON to a temporary file and validate/publish with: "
           "python3 %s complete REQUEST ANALYSIS.json. Replace REQUEST with the record path above. "
           "If you cannot do this, say so; the record remains requested, never silently complete.\n"
           "[End structural analysis request]")
   path (shell-quote-argument session-mode--analysis-tool)
   (string-join (mapcar #'car session-mode-turn-vocabulary) ", ")
   session-mode-turn-interpretation-version
   (shell-quote-argument session-mode--analysis-tool)))

(defvar-local session-mode--analysis-display-stamp nil)

(defun session-mode--refresh-analysis-on-navigation ()
  "Pick up late-written results when navigating, without work on typing."
  (when (and session-mode-turn-tags-mode session-mode--last-analysis-request
             (not (memq this-command '(self-insert-command newline newline-and-indent))))
    (let* ((path session-mode--last-analysis-request)
           (attrs (file-attributes (concat path ".analysis.json")))
           (stamp (and attrs (list path (file-attribute-modification-time attrs)))))
      (when (and stamp (not (equal stamp session-mode--analysis-display-stamp)))
        (session-mode--display-analysis path)))))

(defun session-mode--fragment-help (fragment labeller)
  "Describe FRAGMENT's meaning and candidate patterns independently of its cue."
  (let ((refs (alist-get 'pattern_refs fragment)))
    (format "%s: %s [agent %s; inferred]. %s"
            (alist-get 'intent fragment) (alist-get 'target fragment) labeller
            (if refs
                (string-join
                 (mapcar (lambda (ref)
                           (format "Flexiarg candidate %s — %s"
                                   (alist-get 'id ref) (alist-get 'rationale ref))) refs) "; ")
              "No justified flexiarg alignment recorded."))))

(defun session-mode--learn-analysis-cues (data path)
  "Persist explicitly proposed reusable cues in DATA, retaining PATH provenance.
Existing phrase assignments, including human corrections, always take precedence."
  (session-mode--load-live-vocabulary)
  (let ((rules (copy-tree session-mode-turn-vocabulary))
        (learned (copy-tree session-mode-learned-cues)) changed)
    (dolist (cue (alist-get 'reusable_cues data))
      (let ((phrase (alist-get 'text cue)) (intent (alist-get 'intent cue)))
        (unless (cl-some (lambda (group) (member-ignore-case phrase (cdr group))) rules)
          (session-mode--validate-turn-vocabulary (list (list intent phrase)))
          (let ((group (assoc intent rules)))
            (if group (setcdr group (append (cdr group) (list phrase)))
              (setq rules (append rules (list (list intent phrase))))))
          (push `((phrase . ,phrase) (intent . ,intent) (source . ,path)
                  (labeller . ,(alist-get 'labeller data))
                  (rationale . ,(alist-get 'rationale cue)) (method . "agent-inferred")) learned)
          (setq changed t))))
    (when changed
      (session-mode--save-live-vocabulary rules nil learned)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when session-mode-turn-tags-mode (session-mode-turn-tags-refresh)))))))

(defun session-mode--display-analysis (path)
  "Underline validated agent fragments for the latest sent turn, if available."
  (let ((result (concat path ".analysis.json")))
    (when (and session-mode-turn-tags-mode
               (equal path session-mode--last-analysis-request)
               (file-exists-p result))
      (condition-case err
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (data (json-read-file result))
                 (source (alist-get 'source_text data))
                 ;; The buffer keeps the surface marker voxterm inserted; the
                 ;; record does not, so compare on the stripped text and shift
                 ;; the paint origin past the marker. Without this every
                 ;; dictated turn fails the comparison and loses both its
                 ;; underlines and its cues.
                 (sent (or session-mode--last-operator-text ""))
                 (stripped (cdr (agent-chat-split-surface-marker sent)))
                 (marker-width (- (length sent) (length stripped))))
            (when (equal (alist-get 'status data) "analyzed")
              ;; Learning is not a side effect of painting. A proposed cue is
              ;; vocabulary with provenance and it must survive a turn whose
              ;; underlines cannot be drawn -- a later turn already sent, a
              ;; region whose markers have gone. Measured 2026-09-23: 10 of 38
              ;; analyses learned nothing for exactly that reason.
              (session-mode--learn-analysis-cues data result)
              (session-mode--record-rnode-cues data))
            (when (and (equal (alist-get 'status data) "analyzed")
                       (equal source stripped)
                       session-mode--last-operator-region
                       (marker-buffer (car session-mode--last-operator-region)))
              (setq session-mode--analysis-display-stamp
                    (list path (file-attribute-modification-time (file-attributes result))))
              (let ((base (+ (marker-position (car session-mode--last-operator-region))
                             marker-width)))
                ;; Refresh makes repeated callbacks idempotent and removes
                ;; legacy full-interpretation underlines before painting cues.
                (session-mode--refresh-sent-tags)
                (setq session-mode--sent-tag-overlays
                      (append (session-mode--paint-analysis-cues data source base)
                              session-mode--sent-tag-overlays)))))
        (error (message "Turn analysis display failed: %s" (error-message-string err)))))))

(defun session-mode--paint-analysis-cues (data source base)
  "Underline DATA's validated display cues; SOURCE starts at buffer position BASE.
Return the overlays made."
  (let (made)
    (dolist (sentence (alist-get 'sentences data))
      (dolist (fragment (alist-get 'fragments sentence))
        ;; No fallback for old results lacking explicit display cues.
        (dolist (cue (alist-get 'display_cues fragment))
          (let ((start (alist-get 'start cue)) (end (alist-get 'end cue))
                (intent (alist-get 'intent fragment)))
            (when (and (integerp start) (integerp end) (<= 0 start) (< start end)
                       (<= end (length source))
                       (<= (- end start) 80)
                       (<= (length (split-string (alist-get 'text cue))) 8)
                       (equal (substring source start end) (alist-get 'text cue))
                       (<= (+ base end) (point-max)))
              (let ((ov (make-overlay (+ base start) (+ base end))))
                (overlay-put ov 'session-mode-turn-tag intent)
                (overlay-put ov 'face '(:underline (:style wave :color "purple")))
                (overlay-put ov 'priority 31)
                (overlay-put ov 'session-mode-inferred t)
                (overlay-put ov 'help-echo
                             (session-mode--fragment-help fragment (alist-get 'labeller data)))
                (push ov made)))))))
    made))

(defvar-local session-mode--past-tag-overlays nil
  "Underlines on earlier operator turns, painted from their stored readings.")

(defun session-mode-repaint-past-turns ()
  "Underline every earlier operator turn in this buffer that 象 has read.
Only the latest turn is underlined as it is sent; this paints the rest from
the analysis files of this buffer's session, finding each turn by its text
on a line that begins with the operator's speaker label.  Read-only: no
record is written.  Returns the number of turns painted."
  (interactive)
  (mapc #'delete-overlay session-mode--past-tag-overlays)
  (setq session-mode--past-tag-overlays nil)
  (let ((session (bound-and-true-p agent-chat--session-id))
        (speaker (concat (or (bound-and-true-p agent-chat-user-speaker) "joe") ": "))
        (latest (and session-mode--last-analysis-request
                     (concat session-mode--last-analysis-request ".analysis.json")))
        (json-object-type 'alist) (json-array-type 'list)
        (painted 0))
    (unless session (user-error "This buffer has no agent session id"))
    (dolist (result (directory-files session-mode-turn-analysis-directory t
                                     "\\`turn-[^.]+\\.json\\.analysis\\.json\\'"))
      (unless (equal result latest)     ; the latest turn has its own overlays
        (condition-case nil
            (let* ((record (json-read-file (string-remove-suffix ".analysis.json" result))))
              (when (equal (alist-get 'session_id record) session)
                (let* ((data (json-read-file result))
                       (source (alist-get 'source_text data)))
                  (when (and (equal (alist-get 'status data) "analyzed")
                             (stringp source) (not (string-empty-p source)))
                    (save-excursion
                      (goto-char (point-min))
                      (let (found)
                        (while (and (not found) (search-forward source nil t))
                          (let ((beg (match-beginning 0)))
                            ;; The turn's first line starts with "joe: ", possibly
                            ;; followed by voxterm's surface marker.
                            (when (save-excursion
                                    (goto-char beg)
                                    (let ((bol (line-beginning-position)))
                                      (and (string-prefix-p speaker
                                                            (buffer-substring-no-properties
                                                             bol (min (point-max) (+ bol (length speaker)))))
                                           (<= (- beg bol) (+ (length speaker) 12)))))
                              (setq found beg))))
                        (when found
                          (setq painted (1+ painted))
                          (setq session-mode--past-tag-overlays
                                (append (session-mode--paint-analysis-cues data source found)
                                        session-mode--past-tag-overlays)))))))))
          (error nil))))
    (when (called-interactively-p 'interactive)
      (message "Underlined %d earlier turn%s" painted (if (= painted 1) "" "s")))
    painted))

(defun session-mode-inspect-turn-analysis ()
  "Open the latest structural record or completed agent interpretation."
  (interactive)
  (unless session-mode--last-analysis-request (user-error "No structural record in this buffer yet"))
  (let ((result (concat session-mode--last-analysis-request ".analysis.json")))
    (find-file-other-window (if (file-exists-p result) result session-mode--last-analysis-request))))

(defun session-mode--split-failure-marker (text)
  "Return (clean-text . failed) for a final standalone !x token in TEXT.
An inline mention or quoted !x is ordinary text.  A marker alone needs a draft."
  (let ((trimmed (string-trim-right text)))
    (if (string-match "\\(?:\\`\\|[ \t\n]\\)!x\\'" trimmed)
        (let ((clean (string-trim-right (substring trimmed 0 (match-beginning 0)))))
          (when (string-empty-p (string-trim clean))
            (user-error "Put !x after the turn whose tagging failed; nothing sent"))
          (cons clean t))
      (cons text nil))))

(defcustom session-mode-analysis-agent "象"
  "Agent id that interprets operator turns, or nil for the receiving agent.
One delegate across every lane (Joe, 2026-09-23): turn tagging is a
structure with exactly one producer, and eight seats producing it in
parallel is the arrangement delegation exists to end -- see
cycle-machine/single-producer.
With nil the structural analysis request rides on the prompt of whichever
agent Joe is talking to, so the interpretation costs that agent part of its
turn. Set to an agent id -- \"kimi-2\" -- and the request is dispatched to
that seat instead as a work bell, leaving the conversation uninterrupted.
The record is written either way; only who fills it changes.
Default 象 (Joe, 2026-09-26): a Kimi seat reserved for this job, named so
that no agent mistakes it for a kimi-N available for ordinary dispatch --
/agents/auto only reclaims ids of the form kimi-N."
  :type '(choice (const :tag "The receiving agent" nil) string)
  :group 'session-mode)

(defcustom session-mode-analysis-alternate "象-sonnet"
  "Seat that takes turn analysis while `session-mode-analysis-agent' is out
of quota, and the other way round (Joe, 2026-09-27).  象 is Kimi; this one
is Sonnet, so a five-hour usage limit on either provider leaves the other
working.  Reserved like 象: a claude-N id could be reclaimed by
/agents/auto.  nil disables failover."
  :type '(choice (const :tag "No failover" nil) string)
  :group 'session-mode)

(defcustom session-mode-analysis-bench-minutes 60
  "How long a seat that hit its usage limit is passed over.
After this the seat is tried again; if it is still limited the dispatch
fails over once more, quietly."
  :type 'integer
  :group 'session-mode)

(defvar session-mode--analysis-benched nil
  "Alist of (AGENT . TIME): seats out of quota, not to be used before TIME.")

(defcustom session-mode-analysis-pool '("象-1" "象-2" "象-3" "象-4")
  "Kimi seats that share turn analysis.
nil: `session-mode-analysis-agent' alone.
Joe, 2026-09-30: turns waited a median 5.5 min in one seat's queue for a
2.5 min reading.  Each turn goes to the pool seat with the fewest readings
outstanding.  Differing pattern names across seats are coalesced later
(M-象-cascade); the multiplicity is kept, since it is information too.
The seats share a provider, so a usage limit benches the whole pool and
`session-mode-analysis-alternate' takes over."
  :type '(repeat string) :group 'session-mode)

(defun session-mode--analysis-benched-p (agent)
  "Non-nil if AGENT hit its usage limit and its bench has not expired."
  (let ((until (alist-get agent session-mode--analysis-benched nil nil #'equal)))
    (and until (time-less-p nil until))))

(defun session-mode--bench-analysis-seat (agent)
  "Pass over AGENT for `session-mode-analysis-bench-minutes'.
A pool seat benches the whole pool: the seats share one provider's limit."
  (dolist (a (if (member agent session-mode-analysis-pool)
                 session-mode-analysis-pool
               (list agent)))
    (setf (alist-get a session-mode--analysis-benched nil nil #'equal)
          (time-add nil (* 60 session-mode-analysis-bench-minutes)))))

(defvar session-mode--analysis-outstanding (make-hash-table :test #'equal)
  "Turn record path -> (SEAT . DISPATCH-TIME) for readings not yet landed.")

(defun session-mode--analysis-note-dispatch (path seat)
  (puthash (file-name-nondirectory path) (cons seat (float-time))
           session-mode--analysis-outstanding))

(defun session-mode--analysis-note-done (path)
  (remhash (file-name-nondirectory path) session-mode--analysis-outstanding))

(defun session-mode--analysis-load (seat)
  "Readings dispatched to SEAT in the last hour that have not landed."
  (let ((n 0) (cutoff (- (float-time) 3600)))
    (maphash (lambda (_ v) (when (and (equal (car v) seat) (> (cdr v) cutoff))
                             (setq n (1+ n))))
             session-mode--analysis-outstanding)
    n))

(defun session-mode--analysis-pool-seat ()
  "The least-loaded unbenched pool seat, first in pool order on a tie; or nil."
  (let (best best-load)
    (dolist (seat session-mode-analysis-pool best)
      (unless (session-mode--analysis-benched-p seat)
        (let ((load (session-mode--analysis-load seat)))
          (when (or (null best) (< load best-load))
            (setq best seat best-load load)))))))

(defun session-mode--analysis-other-seat (agent)
  "The seat to use when AGENT is out of usage.
A pool seat: another unbenched pool seat, else the alternate.  Otherwise
the seat that is not AGENT, among the analysis seat and its alternate."
  (cond ((member agent session-mode-analysis-pool)
         (or (session-mode--analysis-pool-seat) session-mode-analysis-alternate))
        ((equal agent session-mode-analysis-agent) session-mode-analysis-alternate)
        (t (or (session-mode--analysis-pool-seat) session-mode-analysis-agent))))

(defun session-mode--analysis-seat ()
  "The seat to dispatch to now.
With a pool: its least-loaded unbenched seat, else the alternate.
Without: the analysis agent unless it is benched."
  (let ((primary session-mode-analysis-agent)
        (alternate session-mode-analysis-alternate))
    (cond
     (session-mode-analysis-pool
      (or (session-mode--analysis-pool-seat)
          (and alternate (not (session-mode--analysis-benched-p alternate)) alternate)
          (car session-mode-analysis-pool)))
     ((and alternate
           (session-mode--analysis-benched-p primary)
           (not (session-mode--analysis-benched-p alternate)))
      alternate)
     (t primary))))

(defun session-mode--quota-failure-p (out)
  "Non-nil if reaper output OUT says the seat ran out of usage."
  (string-match-p "usage limit\\|quota\\|HTTP 429\\|rate.limit" out))

(defcustom session-mode-analysis-reset-every 20
  "Clear the analysis seat's conversation after this many dispatches.
Each brief carries the whole instruction, so a fresh conversation is fully
reseeded by the next dispatch; without the reset the seat's context fills
one operator turn at a time (Joe, 2026-09-26).  The reset waits for the
seat to be idle, so it may land a dispatch or two late.  nil never resets."
  :type '(choice (const :tag "Never" nil) integer)
  :group 'session-mode)

(defvar session-mode--analysis-dispatch-count 0
  "Dispatches to the analysis seat since its conversation was last cleared.")

(defcustom session-mode-analysis-sender
  (expand-file-name "../scripts/agency_send.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to futon3c's agency_send.py, used when delegating the analysis.
Absolute, because the dispatch runs in whatever buffer the turn landed in:
a bare \"agency_send.py\" resolved against that buffer's `default-directory'
and python3 exited 2 on the missing file everywhere but futon3c/scripts."
  :type 'string
  :group 'session-mode)

(defcustom session-mode-analysis-caller "turn-capture"
  "Sender named on every turn-analysis dispatch.
Deliberately not the agent the turn was addressed to: a registered sender
receives an auto-bellback when the job finishes, which put a \"RE: your
bell\" turn in the buffer Joe was reading for every turn he typed (Joe,
2026-09-25: turn them off). Nobody reads that bell; the reaper picks the
result up from the job. Keep this an id that is not on the Agency roster."
  :type 'string
  :group 'session-mode)

(defcustom session-mode-analysis-requisition "M-futon-seams"
  "Requisition TARGET sent with every turn-analysis brief.
A seat may refuse work that does not name what it is for. The dispatcher
appends the turn's own id as the purpose, so the line reads
  Requisition: M-futon-seams — interpret operator turn turn-oxKV5y

The target is deliberately CONSTANT. A Kimi seat keeps one conversation
per target and compacts it when the target changes, so a per-turn target
would pay for a compaction on every single turn. The cost of holding it
constant is that one turn's reading can colour the next; the brief says
plainly that the record is everything the seat knows, and the learned cue
vocabulary is persisted to a file rather than carried in conversation, so
nothing depends on that history."
  :type 'string
  :group 'session-mode)

(defcustom session-mode-analysis-reap-late-after 600
  "Seconds between reaps once the first three have found the job unfinished.
With 象 backed up, readings land well after nine minutes; the reaper used
to stop there, so the landing ran no hook and neither the stepper nor the
turn trace ever heard of it (claude-17, 2026-09-30)."
  :type 'integer :group 'session-mode)

(defcustom session-mode-analysis-reap-late-tries 6
  "Reaps at `session-mode-analysis-reap-late-after' after the first three."
  :type 'integer :group 'session-mode)

(defcustom session-mode-analysis-reap-after 180
  "Seconds after a dispatch before asking what became of its job.
Long enough that an ordinary interpretation has finished, so the usual
answer is \"still running\" and nothing is written."
  :type 'integer
  :group 'session-mode)

(defcustom session-mode-analysis-store-busy-retry-delays '(60 180 600)
  "Seconds to wait between analysis redispatches after futon1b is busy.
Each delay permits one more attempt.  The evidence-store boundary prevents the
agent from running when its clock decision was not recorded, and the turn's
event identity is stable, so these retries neither duplicate analysis nor
admit an unclocked turn.  After the final delay the failed job is reported
normally."
  :type '(repeat integer)
  :group 'session-mode)

(defconst session-mode--dispatch-reaper
  (expand-file-name "../scripts/turn_dispatch_reap.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Asks what became of a dispatched analysis, beside the analysis tool.")

(defconst session-mode--seat-resetter
  (expand-file-name "../scripts/reset_seat_if_idle.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Clears the analysis seat's conversation when it is idle.")

(defvar session-mode--analysis-health nil
  "What the last evidence says about delegated turn analysis.
nil until something is known; `ok' once a reap finds a turn analysed;
`failing' after a dispatch that did not deliver or a job that was refused,
failed or unreachable.  Delivery alone changes nothing: exit 0 means the
bell was accepted, not that the turn was interpreted.")

(defvar session-mode--analysis-health-detail nil
  "One line saying why `session-mode--analysis-health' has its value.")

(defun session-mode--set-analysis-health (health detail)
  "Record HEALTH with DETAIL and redraw the 象 lighter."
  (setq session-mode--analysis-health health
        session-mode--analysis-health-detail
        (format "%s — %s" (format-time-string "%H:%M") detail))
  (force-mode-line-update t))

(defun session-mode--record-dispatch-job (path job-id)
  "Note on the record at PATH that its dispatch created JOB-ID."
  (call-process "python3" nil nil nil
                session-mode--dispatch-reaper "--set-job" path job-id))

(defun session-mode--withdrawal-response-reason (response)
  "Return RESPONSE's typed reason as a string, when present."
  (let* ((body (plist-get response :json))
         (reason (and (listp body) (plist-get body :reason))))
    (when reason (replace-regexp-in-string "^:" "" (format "%s" reason)))))

(defun session-mode--write-analysis-record (path record)
  "Atomically replace PATH with JSON RECORD."
  (let ((temp (make-temp-file
               (expand-file-name ".turn-withdrawal-" (file-name-directory path)))))
    (unwind-protect
        (progn
          (with-temp-file temp
            (let ((coding-system-for-write 'utf-8-unix))
              (insert (json-serialize record :null-object :null
                                      :false-object :false)
                      "\n")))
          (rename-file temp path t)
          (setq temp nil))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(defun session-mode--withdrawal-fragments (analysis)
  "Return (FRAGMENT-ID . FRAGMENT) pairs for withdraw intents in ANALYSIS."
  (let (found)
    (dolist (sentence (alist-get 'sentences analysis))
      (let ((sid (alist-get 'id sentence)) (index 0))
        (dolist (fragment (alist-get 'fragments sentence))
          (when (equal (alist-get 'intent fragment) "withdraw")
            (push (cons (format "%s:%d" sid index) fragment) found))
          (setq index (1+ index)))))
    (nreverse found)))

(defun session-mode--post-inferred-withdrawal (record-id fragment-id record fragment)
  "POST one withdraw FRAGMENT and return its durable outcome alist."
  (let* ((target (alist-get 'target fragment))
         (field (lambda (k) (let ((v (alist-get k record))) (unless (eq v :null) v))))
         (agent (funcall field 'agent_id))
         (session (funcall field 'session_id))
         (version (funcall field 'interpretation_version))
         (idempotency-key (format "%s:%s" record-id fragment-id)))
    (if (null target)
        `((fragment_id . ,fragment-id) (status . 422)
          (reason . "target-unresolved")
          (idempotency_key . ,idempotency-key))
      (let* ((payload `((caller . "xiang") (agent . ,agent) (session . ,session)
                        (interpretation-id . ,(or (funcall field 'turn_id) record-id))
                        (interpretation-version . ,version)
                        (idempotency-key . ,idempotency-key)))
             (payload (if (equal target "seat-active-card")
                          payload
                        (append payload `((target . ,target)))))
             (response
              (condition-case err
                  (if (fboundp 'agent-chat-evidence-request-json)
                      (agent-chat-evidence-request-json
                       "POST"
                       (format "%s/api/alpha/withdrawal/provisional"
                               (string-remove-suffix "/" agent-chat-agency-base-url))
                       3 payload)
                    (list :status 0 :error "HTTP helper unavailable"))
                (error (list :status 0 :error (error-message-string err)))))
             (status (or (plist-get response :status) 0))
             (body (plist-get response :json))
             (reason (or (session-mode--withdrawal-response-reason response)
                         (plist-get response :error)))
             (effect-id (or (plist-get (plist-get body :record) :id)
                            (plist-get body :effect-id))))
        `((fragment_id . ,fragment-id) (status . ,status)
          (reason . ,(or reason :null)) (effect_id . ,(or effect-id :null))
          (idempotency_key . ,idempotency-key))))))

(defun session-mode--post-negation-interpretation (fragment-id record fragment)
  "Store 象's reading of withdraw FRAGMENT and return its outcome alist.
The exact source `text' in FRAGMENT is sent.  A card-relative target is not a
disclosure id, so only an act id is sent as `target'."
  (let* ((field (lambda (key)
                  (let ((value (alist-get key record)))
                    (unless (eq value :null) value))))
         (operator-evidence-id (funcall field 'evidence_id)))
    (if (not (and (stringp operator-evidence-id)
                  (not (string-empty-p operator-evidence-id))))
        `((fragment_id . ,fragment-id) (status . 0)
          (evidence_id . :null) (resolution . :null)
          (reason . "no-evidence-id"))
      (let* ((target (alist-get 'target fragment))
             (payload `((caller . "xiang")
                        (operator-evidence-id . ,operator-evidence-id)
                        (fragment-id . ,fragment-id)
                        (fragment-text . ,(alist-get 'text fragment))
                        (analysis-version . ,(funcall field 'interpretation_version))))
             (payload (if (and (stringp target) (string-prefix-p "act:" target))
                          (append payload `((target . ,target)))
                        payload))
             (response
              (condition-case err
                  (if (fboundp 'agent-chat-evidence-request-json)
                      (agent-chat-evidence-request-json
                       "POST"
                       (format "%s/api/alpha/interpretation/negation"
                               (string-remove-suffix "/" agent-chat-agency-base-url))
                       3 payload)
                    (list :status 0 :error "HTTP helper unavailable"))
                (error (list :status 0 :error (error-message-string err)))))
             (status (or (plist-get response :status) 0))
             (body (plist-get response :json))
             (entry (and (listp body) (plist-get body :entry)))
             (entry-body (and (listp entry) (plist-get entry :evidence/body)))
             (evidence-id (and (listp entry) (plist-get entry :evidence/id)))
             (resolution (and (listp entry-body) (plist-get entry-body :resolution)))
             (reason (or (session-mode--withdrawal-response-reason response)
                         (plist-get response :error))))
        `((fragment_id . ,fragment-id) (status . ,status)
          (evidence_id . ,(or evidence-id :null))
          (resolution . ,(or resolution :null))
          (reason . ,(or reason :null)))))))

(defun session-mode--withdrawal-notice (outcome)
  "Return the fixed notice alist for OUTCOME, or nil when it has no notice."
  (let ((status (alist-get 'status outcome))
        (reason (alist-get 'reason outcome))
        (effect-id (alist-get 'effect_id outcome)))
    (cond
     ((and (= (or status 0) 200) (stringp effect-id)
           (string-prefix-p "act:" effect-id))
      `((kind . "effect") (effect_id . ,effect-id)
        (text . ,(format "withdraw inferred: effect %s (undo to reverse)" effect-id))))
     ((and (= (or status 0) 403) (equal reason "no-grant"))
      '((kind . "no-grant") (text . "withdraw inferred: off (no grant)")))
     ((and (= (or status 0) 422)
           (member reason '("target-unresolved" "target-not-visible")))
      '((kind . "unresolved")
        (text . "withdraw inferred: unresolved (no target)"))))))

(defun session-mode--withdrawal-seat-buffer (record)
  "Return the live buffer matching RECORD's exact agent and session."
  (let ((agent (alist-get 'agent_id record))
        (session (alist-get 'session_id record)))
    (cl-find-if
     (lambda (buffer)
       (and (buffer-live-p buffer)
            (local-variable-p 'agent-chat--agent-id buffer)
            (local-variable-p 'agent-chat--session-id buffer)
            (equal agent (buffer-local-value 'agent-chat--agent-id buffer))
            (equal session (buffer-local-value 'agent-chat--session-id buffer))))
     (buffer-list))))

(defun session-mode--notice-attempt-failed! (outcome attempts-key reason-key reason)
  "Record one failed delivery on OUTCOME, bounded by the configured maximum."
  (let ((attempts (1+ (or (alist-get attempts-key outcome) 0))))
    (session-mode--alist-set! outcome attempts-key attempts)
    (when (>= attempts session-mode-withdrawal-notice-max-attempts)
      (session-mode--alist-set! outcome reason-key reason))))

(defun session-mode--alist-set! (alist key value)
  "Set KEY to VALUE in ALIST in place, including when KEY is new."
  (if-let ((cell (assq key alist)))
      (setcdr cell value)
    (nconc alist (list (cons key value))))
  value)

(defun session-mode--deliver-withdrawal-notice! (record outcome)
  "Publish and display OUTCOME once. Return non-nil when OUTCOME changed."
  (when-let ((notice (session-mode--withdrawal-notice outcome)))
    (let ((changed nil)
          (now (lambda () (format-time-string "%FT%TZ" nil t))))
      (when (and (not (alist-get 'header_notice_published_at outcome))
                 (not (alist-get 'header_notice_give_up_reason outcome)))
        (let* ((payload `((caller . "xiang")
                          (agent . ,(alist-get 'agent_id record))
                          (session . ,(alist-get 'session_id record))
                          (notice-id . ,(alist-get 'idempotency_key outcome))
                          (kind . ,(alist-get 'kind notice))))
               (payload (if-let ((effect-id (alist-get 'effect_id notice)))
                            (append payload `((effect-id . ,effect-id)))
                          payload))
               (response
                (condition-case err
                    (if (fboundp 'agent-chat-evidence-request-json)
                        (agent-chat-evidence-request-json
                         "POST"
                         (format "%s/api/alpha/turn-notice"
                                 (string-remove-suffix "/" agent-chat-agency-base-url))
                         3 payload)
                      (list :status 0 :error "HTTP helper unavailable"))
                  (error (list :status 0 :error (error-message-string err)))))
               (status (or (plist-get response :status) 0)))
          (if (<= 200 status 299)
              (session-mode--alist-set!
               outcome 'header_notice_published_at (funcall now))
            (session-mode--notice-attempt-failed!
             outcome 'header_notice_attempts 'header_notice_give_up_reason
             (format "http-%s%s" status
                     (if-let ((error (plist-get response :error)))
                         (format ":%s" error) ""))))
          (setq changed t)))
      (when (and (not (alist-get 'repl_notice_delivered_at outcome))
                 (not (alist-get 'repl_notice_give_up_reason outcome)))
        (if-let ((buffer (session-mode--withdrawal-seat-buffer record)))
            (condition-case err
                (with-current-buffer buffer
                  (if (fboundp 'agent-chat-insert-message)
                      (progn
                        (agent-chat-insert-message "system" (alist-get 'text notice))
                        (session-mode--alist-set!
                         outcome 'repl_notice_delivered_at (funcall now)))
                    (session-mode--notice-attempt-failed!
                     outcome 'repl_notice_attempts 'repl_notice_give_up_reason
                     "insert-helper-unavailable")))
              (error
               (session-mode--notice-attempt-failed!
                outcome 'repl_notice_attempts 'repl_notice_give_up_reason
                (error-message-string err))))
          (session-mode--notice-attempt-failed!
           outcome 'repl_notice_attempts 'repl_notice_give_up_reason
           "no-matching-buffer"))
        (setq changed t))
      changed)))

(defvar session-mode--withdrawal-pending-paths nil
  "Turn records with a notice not yet published or displayed.
A record is reaped once, when its analysis completes, so a notice that failed
then is retried on later reaps of OTHER records; each outcome stops after
`session-mode-withdrawal-notice-max-attempts'.  Kept in memory only: an Emacs
restart drops the retries, and the give-up is then not recorded.")

(defun session-mode--withdrawal-notice-pending-p (outcome)
  "Non-nil if OUTCOME has a notice still to publish or display."
  (and (session-mode--withdrawal-notice outcome)
       (or (and (not (alist-get 'header_notice_published_at outcome))
                (not (alist-get 'header_notice_give_up_reason outcome)))
           (and (not (alist-get 'repl_notice_delivered_at outcome))
                (not (alist-get 'repl_notice_give_up_reason outcome))))))

(defun session-mode--retry-pending-withdrawal-notices (except)
  "Retry pending notices on every remembered record but EXCEPT."
  (dolist (path (copy-sequence session-mode--withdrawal-pending-paths))
    (unless (equal path except)
      (condition-case nil
          (session-mode--process-withdrawals path)
        (error (setq session-mode--withdrawal-pending-paths
                     (delete path session-mode--withdrawal-pending-paths)))))))

(defun session-mode--process-withdrawals (path)
  "Apply newly analysed withdraw interpretations for operator record PATH.
Every outcome is written onto PATH.  This function never changes analysis
health and never retries a failed route call."
  (let ((analysis-path (concat path ".analysis.json")))
    (when (and (file-exists-p path) (file-exists-p analysis-path))
      (let* ((json-object-type 'alist) (json-array-type 'list)
             ;; The record is rewritten, so it must round-trip exactly: the
             ;; legacy reader turns [] and {} into nil, written back as null,
             ;; and the Python readers iterate those fields.
             (record (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8))
                         (insert-file-contents path))
                       (json-parse-buffer :object-type 'alist :array-type 'array
                                          :null-object :null :false-object :false)))
             (analysis (json-read-file analysis-path))
             (operator-p (equal (alist-get 'origin record) "operator"))
             (record-id (file-name-base path))
             (existing (alist-get 'withdrawal_effects record))
             (existing (if (eq existing :null) nil (append existing nil)))
             (done (mapcar (lambda (outcome) (alist-get 'fragment_id outcome)) existing))
             (negations-value (alist-get 'negation_interpretations record))
             (negations (if (eq negations-value :null) nil
                          (append negations-value nil)))
             (negation-done
              (mapcar (lambda (outcome) (alist-get 'fragment_id outcome)) negations))
             outcomes negation-outcomes changed)
        (when operator-p
          (dolist (pair (session-mode--withdrawal-fragments analysis))
            (unless (member (car pair) done)
              (let ((outcome (session-mode--post-inferred-withdrawal
                              record-id (car pair) record (cdr pair))))
                (when (and (= (or (alist-get 'status outcome) 0) 403)
                           (equal (alist-get 'reason outcome) "no-grant")
                           (not (gethash 'emacs
                                         session-mode--withdrawal-disabled-messaged-sessions)))
                  (puthash 'emacs t
                           session-mode--withdrawal-disabled-messaged-sessions)
                  (message "象: inferred withdrawals are off until Joe's grant exists"))
                (push outcome outcomes)))
            (unless (member (car pair) negation-done)
              (push (session-mode--post-negation-interpretation
                     (car pair) record (cdr pair))
                    negation-outcomes)))
          (when outcomes
            (setq existing (append existing (nreverse outcomes))
                  changed t))
          (when negation-outcomes
            (setq negations (append negations (nreverse negation-outcomes))
                  changed t))
          (dolist (outcome existing)
            (when (session-mode--deliver-withdrawal-notice! record outcome)
              (setq changed t)))
          (when changed
            (setf (alist-get 'withdrawal_effects record) (vconcat existing))
            (setf (alist-get 'negation_interpretations record) (vconcat negations))
            (session-mode--write-analysis-record path record))
          (setq session-mode--withdrawal-pending-paths
                (delete path session-mode--withdrawal-pending-paths))
          (when (seq-some #'session-mode--withdrawal-notice-pending-p existing)
            (push path session-mode--withdrawal-pending-paths)))))))

(defun session-mode--set-withdrawal-processing-error (path value)
  "Set or (VALUE nil) remove `withdrawal_processing_error' on record PATH.
Best effort: a record that cannot be read is reported by `message'."
  (condition-case err
      (let* ((record (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8))
                         (insert-file-contents path))
                       (json-parse-buffer :object-type 'alist :array-type 'array
                                          :null-object :null :false-object :false)))
             (present (assq 'withdrawal_processing_error record)))
        (cond
         (value
          (setf (alist-get 'withdrawal_processing_error record) value)
          (session-mode--write-analysis-record path record))
         (present
          (session-mode--write-analysis-record
           path (assq-delete-all 'withdrawal_processing_error record)))))
    (error (message "象: could not record processing error on %s: %s"
                    (file-name-base path) (error-message-string err)))))

(defun session-mode--handle-reap-output (path out)
  "Handle successful analysis reap OUT for PATH without coupling side effects.
A failure to process withdrawals is written onto the record and leaves the
analysis health failing; it is not discarded."
  (when (string-match-p "analyzed" out)
    (let ((failure
           (condition-case err
               (progn (session-mode--process-withdrawals path) nil)
             (error err))))
      (if failure
          (progn
            (session-mode--set-withdrawal-processing-error
             path `((at . ,(format-time-string "%FT%TZ" nil t))
                    (error . ,(symbol-name (car failure)))
                    (message . ,(truncate-string-to-width
                                 (error-message-string failure) 500))))
            (message "象: withdrawal processing failed for %s: %s"
                     (file-name-base path) (error-message-string failure)))
        (session-mode--set-withdrawal-processing-error path nil))
      (session-mode--retry-pending-withdrawal-notices path)
      (session-mode--set-analysis-health
       (if failure 'failing 'ok)
       (format "%s: %s" (file-name-base path)
               (if failure "withdrawal processing failed" "analysed"))))))

(defvar session-mode-analysis-landed-functions nil
  "Called with a record's path when the reaper finds its 象 reading done.")

(add-hook 'session-mode-analysis-landed-functions #'session-mode--analysis-note-done)

(defun session-mode--store-busy-failure-p (out)
  "Non-nil when reaper output OUT reports transient futon1b admission load."
  (and (string-match-p "REFUSED\\|FAILED" out)
       (string-match-p
        "futon1b busy\\|clock/store-busy\\|Turn not started: futon1b"
        out)))

(defun session-mode--retry-analysis-after-store-busy (path agent delays)
  "Archive PATH's failed attempt and redispatch it to AGENT.
DELAYS is the tail of the bounded backoff schedule for later failures."
  (let ((status (call-process "python3" nil nil nil
                              session-mode--dispatch-reaper "--retry" path)))
    (if (zerop status)
        (session-mode--dispatch-analysis path agent delays)
      (session-mode--set-analysis-health
       'failing (format "%s: could not prepare store-busy retry"
                        (file-name-base path)))
      (display-warning
       'session-mode
       (format "Turn analysis retry could not reset %s to `requested'."
               (file-name-base path))
       :warning))))

(defun session-mode--reap-dispatch (path &optional agent tries store-busy-delays)
  "Ask what became of PATH's dispatch and write the answer onto the record.
A refusal and a busy seat both left `requested' before this existed.
AGENT is the seat it went to: if that seat ran out of usage, bench it and
send the turn to the other seat instead of warning.  A job still running
is asked about again: three times at `session-mode-analysis-reap-after',
then `session-mode-analysis-reap-late-tries' times at
`session-mode-analysis-reap-late-after'.  TRIES counts the reaps left.
A single reap that found it running left the lighter's health unchanged
for good.  STORE-BUSY-DELAYS is the remaining bounded redispatch schedule."
  (let ((buf (generate-new-buffer " *session-analysis-reap*")))
    (make-process
     :name "session-analysis-reap" :buffer buf :noquery t
     :sentinel (lambda (proc _e)
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (let ((out (string-trim (buffer-string))))
                       (cond
                        ((and agent
                              (string-match-p "REFUSED\\|FAILED" out)
                              (session-mode--quota-failure-p out)
                              (let ((other (session-mode--analysis-other-seat agent)))
                                (session-mode--bench-analysis-seat agent)
                                (when (and other
                                           (not (session-mode--analysis-benched-p other)))
                                  (call-process "python3" nil nil nil
                                                session-mode--dispatch-reaper
                                                "--retry" path)
                                  (message "象: %s is out of usage; %s now takes turn analysis"
                                           agent other)
                                  ;; Not `ok' -- nothing is analysed yet -- but
                                  ;; no longer failing: the other seat has it.
                                  (session-mode--set-analysis-health
                                   nil (format "%s out of usage; %s took %s"
                                               agent other (file-name-base path)))
                                  (session-mode--dispatch-analysis path other)
                                  t))))
                        ((and (session-mode--store-busy-failure-p out)
                              (not (eq store-busy-delays :exhausted))
                              (or store-busy-delays
                                  session-mode-analysis-store-busy-retry-delays))
                         (let* ((delays (or store-busy-delays
                                           session-mode-analysis-store-busy-retry-delays))
                                (delay (car delays)))
                           (session-mode--set-analysis-health
                            nil (format "%s: futon1b busy; retrying in %ss"
                                        (file-name-base path) delay))
                           (run-at-time
                            delay nil
                            #'session-mode--retry-analysis-after-store-busy
                            path agent (or (cdr delays) :exhausted))))
                        ((string-match-p "REFUSED\\|FAILED" out)
                         (session-mode--analysis-note-done path)
                         (session-mode--set-analysis-health
                          'failing (format "%s: job refused or failed"
                                           (file-name-base path)))
                         (display-warning
                          'session-mode
                          (format "Turn analysis was not done: %s" out)
                          :warning))
                        ((string-match-p "unreachable" out)
                         (session-mode--set-analysis-health
                          'failing (format "%s: job status unreachable"
                                           (file-name-base path))))
                        ((string-match-p "analyzed" out)
                         (session-mode--handle-reap-output path out)
                         (run-hook-with-args 'session-mode-analysis-landed-functions path))
                        ;; Unfinished (running, or queued behind other
                        ;; turns): ask again, three times at the short
                        ;; interval, then at the long one.
                        ((and (string-match-p "running\\|queued" out)
                              (> (or tries (+ 3 session-mode-analysis-reap-late-tries)) 1))
                         (let ((left (1- (or tries (+ 3 session-mode-analysis-reap-late-tries)))))
                           (run-at-time (if (> left session-mode-analysis-reap-late-tries)
                                            session-mode-analysis-reap-after
                                          session-mode-analysis-reap-late-after)
                                        nil #'session-mode--reap-dispatch path agent
                                        left))))))
                   (when (buffer-live-p (process-buffer proc))
                     (kill-buffer (process-buffer proc)))))
     :command (list "python3" session-mode--dispatch-reaper "--apply" path))))

(defun session-mode--dispatch-analysis (path &optional agent store-busy-delays)
  "Ask AGENT to interpret the turn recorded at PATH.
AGENT defaults to `session-mode--analysis-seat': the analysis agent, or
its alternate while the analysis agent is out of usage.
STORE-BUSY-DELAYS, when non-nil, is the remaining retry schedule inherited
from a transient evidence-store failure.  Fire and forget: the dispatch must not delay the conversation, and a seat
that is busy or absent leaves the record `requested', which is the honest
state -- never silently complete."
  (let* ((agent (or agent (session-mode--analysis-seat)))
         (_ (session-mode--analysis-note-dispatch path agent))
         (brief (concat
                 ;; A requisition line, because a seat may refuse work without
                 ;; one. kimi-1 began refusing on 2026-09-24 and every dispatch
                 ;; to it failed: the brief had no way to say what the work was
                 ;; for. Stated here rather than assumed, and harmless to a seat
                 ;; that does not read it.
                 "Requisition: " session-mode-analysis-requisition
                 " — interpret operator turn "
                 (file-name-base path) "\n\n"
                 "Interpret one operator turn. This is the whole task; there is no "
                 "conversation attached to it.\n\n"
                 "The turn, its sentence offsets and its metadata (including which "
                 "surface it came from) are in the record named below. Read it first.\n"
                 (session-mode--analysis-instruction path)
                 "\n\nThree things the instruction above does not say, because it "
                 "was written for an agent that had just received the turn in "
                 "conversation:\n"
                 "- You did NOT receive this turn. Everything you know about it is in "
                 "the record, so read the whole file rather than the first sentence.\n"
                 "- Joe is not waiting on a reply. Publish the analysis with the "
                 "complete subcommand and bell nothing back unless you could not.\n"
                 "- RECORD WHAT YOU TURNED DOWN. Each fragment takes an "
                 "optional pattern_rejections array: [{\"id\": \"family/name\", "
                 "\"reason\": \"why it does not fit\", \"query\": \"the "
                 "phrasing that surfaced it\"}]. When you read a plausible hit "
                 "and decide against it, put it there rather than only in your "
                 "reply. A citation says one pattern fits; a rejection says a "
                 "near neighbour does not, and where the boundary runs. The "
                 "second kind is what a retrieval system can learn from, and it "
                 "has been thrown away until now.\n"
                 "- A WEAK CITATION IS WORSE THAN AN EMPTY ONE. BM25 always "
                 "returns a top hit; that a pattern scored first does not mean it "
                 "fits. Read its context/IF/THEN and ask whether the operator's "
                 "move is the move it describes -- 'kimi-3 is available' is not "
                 "data-mining/fan-out-independent-runs-across-devices, which is "
                 "about idle GPUs on a rented box. Prefer an honest empty with a "
                 "candidate.\n"
                 "- SEARCH THE PATTERN LIBRARY FOR EVERY FRAGMENT. The instruction "
                 "calls pattern_refs optional; they are the point. An analysis of "
                 "intents and cues alone is textual markup -- it says what Joe did "
                 "without saying which named way of acting he invoked, and the "
                 "library exists to name those. Use:\n"
                 "    python3 /home/joe/code/futon3c/scripts/xlate.py find "
                 "\"defer a decision, sort it out later\" -n 8\n"
                 "  BM25 over 1,400 patterns, 0.3s, works in Chinese too. Measured "
                 "recall@5 is about 0.29, so a miss is normal: try two or three "
                 "phrasings of the MOVE (not of Joe's words) before concluding "
                 "nothing fits. Read the candidate's context/IF/THEN before citing "
                 "it -- an id that does not fit is worse than none, and the tool "
                 "will reject one that does not resolve to a file.\n"
                 "  Leaving pattern_refs empty is a real finding when the library "
                 "has no name for the move. Leaving it empty without searching is "
                 "not; it is the difference the feed now shows Joe in colour.\n"
                 "- EVERY fragment whose pattern_refs you leave empty OWES A "
                 "CANDIDATE. Do NOT decline one on the grounds that the move is "
                 "thin, small, or about the interface. You see one turn; you "
                 "cannot know whether a move recurs, and thinness is a judgement "
                 "only the corpus can make -- a separate pass over all 130-odd "
                 "turns decides ripeness under "
                 "cascade-construction/lift-when-three-align, and it can only "
                 "count moves that were written down. An excused hole is evidence "
                 "destroyed at the one place it was visible.\n"
                 "  Decline only for: a garble rather than a move; quoted material "
                 "that is not Joe speaking; or a move already proposed elsewhere "
                 "(cite which). Then say so in your reply.\n"
                 "  A candidate is cheap. Id, title, parent, one line each of "
                 "IF/HOWEVER/THEN/BECAUSE is enough -- it is a record that a move "
                 "happened and had no name, not a finished pattern.\n"
                 "- WHEN NOTHING FITS, PROPOSE ONE. Write the candidates to "
                 "RECORD.candidates.json beside the record, as\n"
                 "    {\"for\": \"<turn-id>\", \"by\": \"<your agent id>\", "
                 "\"candidates\": [\n"
                 "      {\"id\": \"family/kebab-name\", \"title\": \"...\", "
                 "\"fragment\": \"s1\",\n"
                 "       \"context\": \"...\", \"if\": \"...\", "
                 "\"however\": \"the tension\", \"then\": \"the move\",\n"
                 "       \"parent\": \"family/existing-pattern\", "
                 "\"because\": \"why it holds\", "
                 "\"tried\": \"the composition of existing patterns you tried "
                 "first, and why it failed\"}]}\n"
                 "  Nothing goes into the library. These are proposals, and the "
                 "feed shows them under the cascade as a lineage.\n"
                 "  PARENT IS REQUIRED and it is the point of the exercise: the id "
                 "of the EXISTING library pattern your proposal descends from -- "
                 "the one it specialises, narrows to the operator's case, or "
                 "extends. A proposal that hangs off a known pattern is a "
                 "refinement the library can absorb; one that hangs off nothing "
                 "claims to be a new root, which is a strong claim. Use "
                 "\"parent\": null only when you mean it, and say why in tried. "
                 "The feed renders it as parent ﹥ proposal, so an unparented "
                 "proposal shows as root and invites the question.\n"
                 "  BEFORE proposing, search the proposals too:\n"
                 "    python3 /home/joe/code/futon3c/scripts/xlate.py find "
                 "\"<the move>\" --with-candidates -n 8\n"
                 "  Results prefixed ? are existing proposals. If one already "
                 "names your move, CITE IT in tried and do not mint a second "
                 "name for it -- a vocabulary doubles when nobody checks. And try "
                 "to express the move as a join of existing patterns before "
                 "minting at all; say in tried that you tried.\n"
                 "  python3 .../xlate.py census shows what the whole corpus has "
                 "cited and proposed, and which proposals have recurred three "
                 "times and are therefore ripe.\n"))
         (process-connection-type nil))
    (condition-case err
        (make-process
         :name "session-analysis-dispatch"
         :buffer (generate-new-buffer " *session-analysis-dispatch*")
         :noquery t
         :sentinel
         (lambda (proc event)
           ;; A dispatch that fails must say so. The first live run bounced with
           ;; HTTP 404 -- the delegate seat had left the registry -- and the
           ;; traceback landed in a hidden buffer where nobody would look. That
           ;; is 象/两种规格 on the sending side: delivery is not accomplishment,
           ;; and a silent failure is the one that costs a day.
           (when (memq (process-status proc) '(exit signal))
             (if (/= (process-exit-status proc) 0)
                 (progn
                   (session-mode--set-analysis-health
                    'failing (format "dispatch to %s %s"
                                     agent (string-trim event)))
                  (display-warning
                  'session-mode
                  (format "Analysis dispatch to %s failed (%s). The record stays `requested'.\n%s"
                          agent (string-trim event)
                          (with-current-buffer (process-buffer proc)
                            (string-trim (buffer-string))))
                  :warning))
               ;; Exit 0 means DELIVERED, not done. The seat can still refuse --
               ;; kimi-1 did, on 2026-09-24, for want of a requisition line --
               ;; and that left the record at `requested', indistinguishable
               ;; from a seat that was merely busy. Record the job the bell
               ;; created, then look at what became of it.
               (let ((out (with-current-buffer (process-buffer proc) (buffer-string))))
                 (if (string-match-p "^reset$" out)
                     (setq session-mode--analysis-dispatch-count 1)
                   (setq session-mode--analysis-dispatch-count
                         (1+ session-mode--analysis-dispatch-count)))
                 (when (string-match "\"job-id\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" out)
                   (let ((jid (match-string 1 out)))
                     (session-mode--record-dispatch-job path jid)
                     (run-at-time session-mode-analysis-reap-after nil
                                  #'session-mode--reap-dispatch path agent nil
                                  store-busy-delays)))))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc)))))
         :command (list "sh" "-c"
                        (format "%sprintf %%s %s | python3 %s --to %s --from %s --kind bell --type request --mode work"
                                (if (and session-mode-analysis-reset-every
                                         (>= session-mode--analysis-dispatch-count
                                             session-mode-analysis-reset-every))
                                    (format "python3 %s %s; "
                                            (shell-quote-argument session-mode--seat-resetter)
                                            (shell-quote-argument agent))
                                  "")
                                (shell-quote-argument brief)
                                (shell-quote-argument session-mode-analysis-sender)
                                (shell-quote-argument agent)
                                (shell-quote-argument session-mode-analysis-caller))))
      (error (session-mode--set-analysis-health
              'failing (format "dispatch to %s: %s" agent (error-message-string err)))
             (display-warning 'session-mode
                              (format "Analysis dispatch to %s failed: %s"
                                      agent (error-message-string err)))))))

;;; --- What the agent did (reply-end context for 象) ------------------------

(defconst session-mode--happened-commit-cap 20
  "Most commits listed in a turn's happened summary before \"…and K more\".")

(defun session-mode--git-numstat (repo sha)
  "Return `git show --numstat --format= SHA' output for REPO, or nil.
Line counts only; the diff itself is never read."
  (let ((default-directory (file-name-as-directory (expand-file-name repo))))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "show" "--numstat" "--format=" sha))
        (buffer-string)))))

(defun session-mode--summarize-numstat (text)
  "Summarize git numstat TEXT as (ADDED REMOVED FILES).
Binary lines (\"-\" counts) add to FILES but not to the line counts."
  (let ((added 0) (removed 0) (files 0))
    (dolist (line (split-string (or text "") "\n" t))
      (when (string-match "\\`\\([0-9-]+\\)\t\\([0-9-]+\\)\t" line)
        (cl-incf files)
        (unless (equal (match-string 1 line) "-")
          (cl-incf added (string-to-number (match-string 1 line))))
        (unless (equal (match-string 2 line) "-")
          (cl-incf removed (string-to-number (match-string 2 line))))))
    (list added removed files)))

(defun session-mode--commit-summary-line (commit)
  "One summary line for COMMIT alist: repo, short sha, subject, line counts.
Never includes diff text."
  (let* ((repo (or (alist-get 'repo commit) "?"))
         (sha (or (alist-get 'sha commit) ""))
         (short (substring sha 0 (min 8 (length sha))))
         (subject (or (alist-get 'subject commit) ""))
         (numstat (and (alist-get 'repo-path commit)
                       (not (string-empty-p sha))
                       (session-mode--git-numstat
                        (alist-get 'repo-path commit) sha))))
    (if numstat
        (let ((sums (session-mode--summarize-numstat numstat)))
          (format "- %s %s %s (+%d -%d over %d files)"
                  repo short subject (nth 0 sums) (nth 1 sums) (nth 2 sums)))
      (format "- %s %s %s" repo short subject))))

(defvar-local session-mode--reply-pending-path nil
  "Record path of the operator turn whose reply has not ended yet.")
(defvar-local session-mode--turn-reply-text nil
  "First streamed reply segment of the pending turn, for the summary.")
(defvar-local session-mode--turn-commits-seen nil
  "Commits `agent-chat-finish-turn-commits' returned this turn.
A streamed turn emits its turn-commits (which clears the heads) before
the turn ends, so the summary reads them from here.")

(defun session-mode--dispatch-pending-turn (response)
  "Send the pending turn, if any, to 象 with a summary built from RESPONSE.
Runs at most once per turn: from the reply callback (unstreamed replies)
or from the end of the turn (streamed ones), whichever comes first."
  (let ((path session-mode--reply-pending-path))
    (when path
      (setq session-mode--reply-pending-path nil)
      (session-mode--dispatch-analysis-after-reply path response)
      (session-mode--display-analysis path))))

(defun session-mode--note-reply-segment (text &rest _)
  "Keep the first reply segment TEXT of a pending turn."
  (when (and session-mode--reply-pending-path (not session-mode--turn-reply-text)
             (stringp text))
    (setq session-mode--turn-reply-text text)))

(defun session-mode--note-turn-commits (original &rest args)
  "Call ORIGINAL with ARGS and keep the commits it returns for the summary."
  (let ((commits (apply original args)))
    (when session-mode--reply-pending-path
      (setq session-mode--turn-commits-seen commits))
    commits))

(defun session-mode--on-turn-finished (&rest _)
  "End of a turn: dispatch a still-pending operator turn to 象."
  (condition-case err
      (session-mode--dispatch-pending-turn session-mode--turn-reply-text)
    (error (display-warning
            'session-mode
            (format "象 dispatch at turn end failed: %s; the record stays `requested'"
                    (error-message-string err)))))
  (setq session-mode--turn-reply-text nil
        session-mode--turn-commits-seen nil))

(with-eval-after-load 'agent-chat
  (advice-add 'agent-chat-finish-turn-commits :around #'session-mode--note-turn-commits)
  (advice-add 'agent-chat-finish-turn! :after #'session-mode--on-turn-finished))
(with-eval-after-load 'claude-repl
  (advice-add 'claude-repl--emit-assistant-segment-evidence! :before
              #'session-mode--note-reply-segment))

(defun session-mode--turn-commits-snapshot ()
  "Commits made since turn start, computed from `agent-chat--turn-git-heads'.
Reads the same snapshot `agent-chat-finish-turn-commits' will use, but does
NOT clear it: the turn-commits evidence emit runs after the reply callback
and must still see the heads."
  (if (not (and (boundp 'agent-chat--turn-git-heads)
                agent-chat--turn-git-heads
                (fboundp 'agent-chat--git-head)
                (fboundp 'agent-chat--git-commits-after)))
      session-mode--turn-commits-seen
    (cl-loop for (repo . old-head) in agent-chat--turn-git-heads
             for new-head = (agent-chat--git-head repo)
             when (and new-head (not (equal old-head new-head)))
             append (agent-chat--git-commits-after repo old-head))))

(defun session-mode--turn-happened-summary (response)
  "Build the \"What the agent did\" block for the reply RESPONSE.
First reply lines plus one line per commit made during the turn."
  (let* ((lines (split-string (or response "") "\n"))
         (first5 (cl-subseq lines 0 (min 5 (length lines))))
         (commits (session-mode--turn-commits-snapshot))
         (shown (cl-subseq commits
                           0 (min session-mode--happened-commit-cap
                                  (length commits))))
         (more (- (length commits) (length shown))))
    (concat
     "What the agent did (machine-added context for reading the turn):\n"
     "Reply begins:\n"
     (mapconcat #'identity first5 "\n")
     "\nCommits during the turn (any repo; not necessarily the agent's own):\n"
     (if shown
         (concat (mapconcat #'session-mode--commit-summary-line shown "\n")
                 (when (> more 0)
                   (format "\n…and %d more" more)))
       "(none)"))))

(defun session-mode--record-add-field (path key value)
  "Add KEY/VALUE to the JSON record at PATH, preserving existing fields."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-false :json-false)
         (json-null nil)
         (record (json-read-from-string
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
    (setf (alist-get key record) value)
    (with-temp-file path
      (let ((coding-system-for-write 'utf-8-unix))
        (insert (json-encode record))))))

(defun session-mode--dispatch-analysis-after-reply (path response)
  "Attach a what-happened summary to the record at PATH, then dispatch it.
Runs when the agent's reply to the operator turn arrives, not at send time:
象 reads the turn together with what the agent did in it.  A failure while
building or storing the summary warns once and still dispatches the turn."
  (when (and path session-mode-analysis-agent
             (not (equal session-mode-analysis-agent
                         agent-chat--agent-id))
             ;; 象-off (policy `never') records turns as `not-requested';
             ;; they are kept on disk but must not be belled to the seat.
             (session-mode--record-requests-analysis-p path))
    (let ((summary
           (condition-case err
               (session-mode--turn-happened-summary response)
             (error
              (display-warning
               'session-mode
               (format "象 happened-summary failed (%s); dispatching without it"
                       (error-message-string err))
               :warning)
              nil))))
      (when summary
        (condition-case err
            (session-mode--record-add-field path 'happened_summary summary)
          (error
           (display-warning
            'session-mode
            (format "象 happened-summary could not be stored (%s); dispatching without it"
                    (error-message-string err))
            :warning))))
      (session-mode--dispatch-analysis path))))

(defvar agent-chat--agent-id)
(defvar agent-chat--turn-git-heads)

(defun session-mode--analyze-start-turn (original call agent-name hooks text speaker origin)
  "Wrap only ordinary operator CALLs; keep visible text and hooks unchanged."
  (if (not (and session-mode-turn-tags-mode (eq origin 'operator)
                (equal speaker agent-chat-user-speaker)
                (not (agent-chat--walkie-command-p (string-trim text)))))
      (funcall original call agent-name hooks text speaker origin)
    (let* ((marked (session-mode--split-failure-marker text))
           (failed (cdr marked)))
     (funcall
     original
     (lambda (sent callback)
       (let ((path nil) (prompt sent) (buffer (current-buffer)))
         (condition-case err
             (progn
               (setq path (session-mode--record-turn sent failed text))
               ;; Dispatch to 象 happens when the agent's reply ENDS, not
               ;; here: 象 should read the turn together with what the agent
               ;; did in it.  A streamed reply never calls the callback below
               ;; (claude-repl finishes the turn itself), so the path waits in
               ;; the buffer for whichever end-of-turn comes first.
               (setq session-mode--reply-pending-path path
                     session-mode--turn-reply-text nil
                     session-mode--turn-commits-seen nil)
               (when (and (not session-mode-analysis-agent)
                          (or failed (session-mode--record-requests-analysis-p path)))
                 (setq prompt (concat sent (session-mode--analysis-instruction path)
                                      (when failed
                                        "\nOperator !x feedback: tagging failed. Prioritize substantive keyword analysis of this turn; explain any remaining unclassified passages.\n")))))
           (error (display-warning 'session-mode
                                   (format "Turn structure was NOT recorded: %s" (error-message-string err)))))
         (funcall call prompt
                  (lambda (response)
                    ;; Nothing here may stop the reply reaching the REPL.
                    (condition-case err
                        (if (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (session-mode--dispatch-pending-turn response))
                          ;; Buffer gone: the turn still goes to 象, without a summary.
                          (when (and path (session-mode--record-requests-analysis-p path))
                            (session-mode--dispatch-analysis path)))
                      (error (display-warning
                              'session-mode
                              (format "象 dispatch after reply failed: %s; the record stays `requested'"
                                      (error-message-string err)))))
                    (funcall callback response)))))
     agent-name hooks (car marked) speaker origin))))

(with-eval-after-load 'agent-chat
  (advice-add 'agent-chat--start-turn :around #'session-mode--analyze-start-turn))

(with-eval-after-load 'session-mode
  (define-key session-mode-turn-tags-mode-map (kbd "C-c s a")
              #'session-mode-inspect-turn-analysis))

;; --- Turns captured outside this Emacs ------------------------------------
;; The operator does not always type in the Emacs that records turns: a REPL
;; buffer in another Emacs reaches the same agent, and its turns land in
;; futon1b's evidence store (author joe, event chat-turn) but not here.
;; futon3c/scripts/operator_turn_capture.py reads them there and hands each
;; one to this function, so it is structured and sent for interpretation by
;; the same code as a turn typed in this Emacs.

(defun session-mode-record-external-turn (text agent-id session-id turn-id
                                               &optional evidence-id)
  "Record TEXT as an operator turn to AGENT-ID and request its interpretation.
SESSION-ID, TURN-ID and EVIDENCE-ID are the ones the evidence store gave it.
Returns the record's path. The record's created_at is the capture time; the
caller corrects it to the turn's own time."
  (let ((agent-chat--agent-id agent-id)
        (agent-chat--session-id session-id)
        (agent-chat--current-turn-id turn-id)
        ;; Never the calling buffer's last acknowledged id: that is another turn.
        (agent-chat--last-evidence-id evidence-id)
        (session-mode--last-quotes nil))
    (let ((path (session-mode--record-turn text)))
      (when (and path session-mode-analysis-agent
                 (not (equal session-mode-analysis-agent agent-id)))
        ;; Sent as session-mode-analysis-caller, like a typed turn; the
        ;; record is read back by the reaper, not by a bell.
        (session-mode--dispatch-analysis path))
      path)))

(provide 'session-turn-analysis)
;;; session-turn-analysis.el ends here
