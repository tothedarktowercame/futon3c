;;; session-turn-analysis.el --- Standoff structure for sent turns -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defcustom session-mode-turn-analysis-directory
  (expand-file-name "session-turn-analysis" user-emacs-directory)
  "Private durable records for operator passages, cues and agent interpretations."
  :type 'directory :group 'session-mode)

(defcustom session-mode-turn-analysis-policy 'all
  "Which sent operator turns request interpretation from their receiving agent.
`all' analyzes every ordinary turn; `unmatched' requests only sentences with
no lexical cues; `never' records structure without requesting interpretation."
  :type '(choice (const all) (const unmatched) (const never)) :group 'session-mode)

(defun session-mode--analysis-requested-p (record)
  "Whether RECORD should request interpretation under the current policy."
  (or (eq session-mode-turn-analysis-policy 'all)
      (and (eq session-mode-turn-analysis-policy 'unmatched)
           (> (length (alist-get 'unmatched record)) 0))))

(defconst session-mode--analysis-tool
  (expand-file-name "../scripts/session_turn_analysis.py"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defvar-local session-mode--last-analysis-request nil)

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
reached the buffer. The surface itself is kept in the record's metadata."
  (let* ((split (agent-chat-split-surface-marker text))
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
                      (tagging_failed . ,(if failed t :json-false))
                      (original_text . ,(or original-text text))
                      (agent_id . ,agent-chat--agent-id)
                      (session_id . ,agent-chat--session-id)
                      (turn_id . ,agent-chat--current-turn-id)
                      (surface . ,(if surface (symbol-name surface) "typed"))
                      (quotes . ,(vconcat session-mode--last-quotes))
                      (analysis_status . ,(if (or failed (session-mode--analysis-requested-p record))
                                             "requested" "not-requested")))))
      (condition-case err
          (with-temp-file path
            (let ((coding-system-for-write 'utf-8-unix))
              (insert (json-encode (append record metadata)))))
        (error (delete-file path) (signal (car err) (cdr err))))
      (setq session-mode--last-analysis-request path))))

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
           "Candidate flexiarg refs are optional: read any cited canonical pattern and explain the fit; do not invent IDs. "
           "Record inferred interpretations, not human-approved labels. "
           "To improve future draft tagging, optionally propose top-level reusable_cues with exact start/end/text, intent and rationale for reuse. "
           "Propose only short communicative phrases that generalize, not project names or arbitrary subject words. "
           "Emacs persists unassigned phrases as provisional cue hypotheses with provenance; existing assignments and human corrections win. Do not edit the vocabulary file directly. "
           "Save the filled JSON to a temporary file and validate/publish with: "
           "python3 %s complete REQUEST ANALYSIS.json. Replace REQUEST with the record path above. "
           "If you cannot do this, say so; the record remains requested, never silently complete.\n"
           "[End structural analysis request]")
   path (shell-quote-argument session-mode--analysis-tool)
   (string-join (mapcar #'car session-mode-turn-vocabulary) ", ")
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
              (session-mode--learn-analysis-cues data result))
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
                                   (equal (substring source start end) (alist-get 'text cue)))
                          (let ((ov (make-overlay (+ base start) (+ base end))))
                            (overlay-put ov 'session-mode-turn-tag intent)
                            (overlay-put ov 'face '(:underline (:style wave :color "purple")))
                            (overlay-put ov 'priority 31)
                            (overlay-put ov 'session-mode-inferred t)
                            (overlay-put ov 'help-echo
                                         (session-mode--fragment-help fragment (alist-get 'labeller data)))
                            (push ov session-mode--sent-tag-overlays))))))))))
        (error (message "Turn analysis display failed: %s" (error-message-string err)))))))

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

(defcustom session-mode-analysis-agent "kimi-1"
  "Agent id that interprets operator turns, or nil for the receiving agent.
One delegate across every lane (Joe, 2026-09-23): turn tagging is a
structure with exactly one producer, and eight seats producing it in
parallel is the arrangement delegation exists to end -- see
cycle-machine/single-producer.
With nil the structural analysis request rides on the prompt of whichever
agent Joe is talking to, so the interpretation costs that agent part of its
turn. Set to an agent id -- \"kimi-2\" -- and the request is dispatched to
that seat instead as a work bell, leaving the conversation uninterrupted.
The record is written either way; only who fills it changes."
  :type '(choice (const :tag "The receiving agent" nil) string)
  :group 'session-mode)

(defcustom session-mode-analysis-sender "agency_send.py"
  "Path to futon3c's agency_send.py, used when delegating the analysis."
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

(defcustom session-mode-analysis-reap-after 180
  "Seconds after a dispatch before asking what became of its job.
Long enough that an ordinary interpretation has finished, so the usual
answer is \"still running\" and nothing is written."
  :type 'integer
  :group 'session-mode)

(defconst session-mode--dispatch-reaper
  (expand-file-name "../scripts/turn_dispatch_reap.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Asks what became of a dispatched analysis, beside the analysis tool.")

(defun session-mode--record-dispatch-job (path job-id)
  "Note on the record at PATH that its dispatch created JOB-ID."
  (call-process "python3" nil nil nil
                session-mode--dispatch-reaper "--set-job" path job-id))

(defun session-mode--reap-dispatch (path)
  "Ask what became of PATH's dispatch and write the answer onto the record.
A refusal and a busy seat both left `requested' before this existed."
  (let ((buf (generate-new-buffer " *session-analysis-reap*")))
    (make-process
     :name "session-analysis-reap" :buffer buf :noquery t
     :sentinel (lambda (proc _e)
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (when (string-match-p "REFUSED\\|FAILED" (buffer-string))
                       (display-warning
                        'session-mode
                        (format "Turn analysis was not done: %s"
                                (string-trim (buffer-string)))
                        :warning)))
                   (when (buffer-live-p (process-buffer proc))
                     (kill-buffer (process-buffer proc)))))
     :command (list "python3" session-mode--dispatch-reaper "--apply" path))))

(defun session-mode--dispatch-analysis (path)
  "Ask `session-mode-analysis-agent' to interpret the turn recorded at PATH.
Fire and forget: the dispatch must not delay the conversation, and a seat
that is busy or absent leaves the record `requested', which is the honest
state -- never silently complete."
  (let* ((agent session-mode-analysis-agent)
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
                 (display-warning
                  'session-mode
                  (format "Analysis dispatch to %s failed (%s). The record stays `requested'."
                          session-mode-analysis-agent (string-trim event))
                  :warning)
               ;; Exit 0 means DELIVERED, not done. The seat can still refuse --
               ;; kimi-1 did, on 2026-09-24, for want of a requisition line --
               ;; and that left the record at `requested', indistinguishable
               ;; from a seat that was merely busy. Record the job the bell
               ;; created, then look at what became of it.
               (let ((out (with-current-buffer (process-buffer proc) (buffer-string))))
                 (when (string-match "\"job-id\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" out)
                   (let ((jid (match-string 1 out)))
                     (session-mode--record-dispatch-job path jid)
                     (run-at-time session-mode-analysis-reap-after nil
                                  #'session-mode--reap-dispatch path)))))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc)))))
         :command (list "sh" "-c"
                        (format "printf %%s %s | python3 %s --to %s --from %s --kind bell --type request --mode work"
                                (shell-quote-argument brief)
                                (shell-quote-argument session-mode-analysis-sender)
                                (shell-quote-argument agent)
                                (shell-quote-argument session-mode-analysis-caller))))
      (error (display-warning 'session-mode
                              (format "Analysis dispatch to %s failed: %s"
                                      agent (error-message-string err)))))))

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
               ;; Never ask a seat to interpret a turn addressed to itself. It
               ;; arrives as work while the same words are arriving as
               ;; conversation, and the seat cannot tell which of the two it
               ;; is answering.
               (when (and path session-mode-analysis-agent
                          (not (equal session-mode-analysis-agent
                                      agent-chat--agent-id)))
                 (session-mode--dispatch-analysis path))
               (when (and (not session-mode-analysis-agent)
                          (or failed (session-mode--analysis-requested-p
                                      (session-mode--structure-turn sent))))
                 (setq prompt (concat sent (session-mode--analysis-instruction path)
                                      (when failed
                                        "\nOperator !x feedback: tagging failed. Prioritize substantive keyword analysis of this turn; explain any remaining unclassified passages.\n")))))
           (error (display-warning 'session-mode
                                   (format "Turn structure was NOT recorded: %s" (error-message-string err)))))
         (funcall call prompt
                  (lambda (response)
                    (when (and path (buffer-live-p buffer))
                      (with-current-buffer buffer (session-mode--display-analysis path)))
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

(defun session-mode-record-external-turn (text agent-id session-id turn-id)
  "Record TEXT as an operator turn to AGENT-ID and request its interpretation.
SESSION-ID and TURN-ID are the ones the evidence store gave it. Returns the
record's path. The record's created_at is the capture time; the caller
corrects it to the turn's own time."
  (let ((agent-chat--agent-id agent-id)
        (agent-chat--session-id session-id)
        (agent-chat--current-turn-id turn-id)
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
