(ns futon3c.xiang.turn-record
  "What an operator turn IS, server-side: the pure half of the 象 frontend.

   This is a port of the record-producing half of `emacs/session-turn-analysis.el`
   (M-象-2000) so that a client with no filesystem and no python3 -- a browser,
   an Element widget, a Matrix bridge -- can still produce records that pass
   `packages/turn-seam/conformance.py` and be read by every existing consumer
   of `~/.emacs-graph/session-turn-analysis/` (turn_frames.py, xlate census,
   feed.html, the stepper).

   Everything here is a pure function of its arguments. Offsets are Unicode
   codepoints, zero-based, end-exclusive, as the seam schema requires. Clojure
   strings are UTF-16, so every span is converted at the boundary rather than
   assumed; a turn with an emoji or CJK is the regression test for this file.

   Ported pieces, each named after its Emacs or Python original:
   - `redact-secrets`     scripts/secret_scan.py (the structural rules, the
                          keyword rule and the entropy net; not the .jsonl
                          escape handling, which has no bearing on a turn)
   - `elide-quotes`       session-mode--elide-quotes (>>> fences become QUOTE)
   - `sentence-spans`     session-mode--sentence-spans (forward-sentence with
                          sentence-end-double-space nil; paragraph breaks end
                          a sentence)
   - `turn-matches`       session-mode--turn-matches (literal cue phrases)
   - `structure-turn`     session-mode--structure-turn
   - `make-record`        session-mode--record-turn (minus the file write)
   - `split-surface-marker` agent-chat-split-surface-marker
   - `analysis-brief`     session-mode--analysis-instruction wrapped the way
                          session-mode--dispatch-analysis wraps it
   - `validate-analysis`  session_turn_analysis.py validate()
   - `job-outcome`        turn_dispatch_reap.py's classification"
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Codepoint arithmetic

(defn cp-count
  "Length of S in Unicode codepoints."
  ^long [^String s]
  (.codePointCount s 0 (.length s)))

(defn cp-subs
  "Substring of S between codepoint offsets START and END (end-exclusive)."
  ^String [^String s ^long start ^long end]
  (let [a (.offsetByCodePoints s 0 (int start))
        b (.offsetByCodePoints s 0 (int end))]
    (subs s a b)))

(defn utf16->cp
  "Codepoint offset of UTF-16 index IDX in S."
  ^long [^String s ^long idx]
  (.codePointCount s 0 (int idx)))

(defn cp->utf16
  "UTF-16 index of codepoint offset CP in S."
  ^long [^String s ^long cp]
  (.offsetByCodePoints s 0 (int cp)))

(defn- cp-before
  "Codepoint immediately before UTF-16 index IDX, or nil at the start."
  [^String s ^long idx]
  (when (pos? idx) (.codePointBefore s (int idx))))

(defn- cp-at
  "Codepoint at UTF-16 index IDX, or nil at the end."
  [^String s ^long idx]
  (when (< idx (.length s)) (.codePointAt s (int idx))))

(defn- word-cp?
  "True for the characters Emacs's [[:alnum:]_] class accepts."
  [cp]
  (and cp (or (Character/isLetterOrDigit (int cp)) (= cp (int \_)))))

(defn word-count
  "Whitespace-separated words, as Python's str.split() counts them."
  ^long [s]
  (count (remove str/blank? (str/split (str s) #"\s+"))))

;; ---------------------------------------------------------------------------
;; Secret redaction (scripts/secret_scan.py)

(defn- start-boundary
  "Left boundary that is not preceded by CHARS (secret_scan.py `_start`,
   without the \\n/\\t/\\r escape clause that only matters for .jsonl logs)."
  [chars]
  (str "(?<![" chars "])"))

(def ^:private structural-rules
  [["private-key"
    (re-pattern "(?s)-----BEGIN(?: [A-Z0-9]+)* PRIVATE KEY-----.*?-----END(?: [A-Z0-9]+)* PRIVATE KEY-----")
    0]
   ["aws-access-key"
    (re-pattern (str (start-boundary "A-Z0-9") "(?:AKIA|ASIA)[A-Z0-9]{16}(?![A-Z0-9])")) 0]
   ["github-token"
    (re-pattern (str (start-boundary "A-Za-z0-9_")
                     "(?:gh[opusr]_[A-Za-z0-9]{20,255}|github_pat_[A-Za-z0-9_]{20,255})(?![A-Za-z0-9_])")) 0]
   ["anthropic-key"
    (re-pattern (str (start-boundary "A-Za-z0-9_-") "sk-ant-[A-Za-z0-9_-]{20,}(?![A-Za-z0-9_-])")) 0]
   ["openai-key"
    (re-pattern (str (start-boundary "A-Za-z0-9_-")
                     "(?:sk-proj-[A-Za-z0-9_-]{20,}|sk-[A-Za-z0-9_-]{20,})(?![A-Za-z0-9_-])")) 0]
   ["slack-token"
    (re-pattern (str (start-boundary "A-Za-z0-9-") "xox[abprs]-[A-Za-z0-9-]{10,}(?![A-Za-z0-9-])")) 0]
   ["google-api-key"
    (re-pattern (str (start-boundary "A-Za-z0-9_-") "AIza[A-Za-z0-9_-]{30,}(?![A-Za-z0-9_-])")) 0]
   ["jwt"
    (re-pattern (str (start-boundary "A-Za-z0-9_-")
                     "eyJ[A-Za-z0-9_-]*\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+(?![A-Za-z0-9_-])")) 0]
   ["bearer"
    (re-pattern "(?i)(?:authorization\\s*:\\s*)?bearer\\s+((?=[A-Za-z._~+/-]*[0-9])[A-Za-z0-9._~+/-]{8,})") 1]
   ["url-credentials"
    (re-pattern "[A-Za-z][A-Za-z0-9+.-]*://[^\\s/@:]+:([^\\s/@]+)@[^\\s/]+") 1]])

(def ^:private keyword-rule
  (re-pattern (str "(?i)(?:[A-Za-z0-9]+[_-])*"
                   "(password|passwd|pwd|secret|token|api[_-]?key|access[_-]?key|client[_-]?secret)"
                   "[\"'`]?\\s*[:=]\\s*([\"'`])?")))

(def ^:private unquoted-value (re-pattern "[^\\s,;\\}\\]\"'`]+"))
(def ^:private placeholders
  (re-pattern "(?i)^(?:<redacted>|\\*{3,}|x{3,}|change(?:me)?|changeme\\??|none|null|n/?a|placeholder|example)$"))
(def ^:private high-entropy
  (re-pattern (str (start-boundary "A-Za-z0-9+/_=-") "[A-Za-z0-9+/_=-]{32,}(?![A-Za-z0-9+/_=-])")))
(def ^:private high-entropy-max 256)
(def ^:private hex-run (re-pattern "^[0-9a-fA-F]+$"))
(def ^:private wordlike (re-pattern "[a-z]+[0-9]*|(?:[A-Z][a-z]{2,})+[0-9]*|[A-Z]{1,4}|[0-9]+|[0-9a-f]+"))
(def ^:private uuid-run
  (re-pattern "(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"))

(defn- matcher-findings
  "Findings [kind start end] for one rule over TEXT (UTF-16 offsets)."
  [^String text [kind ^java.util.regex.Pattern pattern group]]
  (let [m (.matcher pattern text)]
    (loop [acc []]
      (if (.find m)
        (recur (conj acc [kind (.start m (int group)) (.end m (int group))]))
        acc))))

(defn- keyword-findings
  "Values assigned to secret-like keywords: password=..., token: \"...\"."
  [^String text]
  (let [m (.matcher ^java.util.regex.Pattern keyword-rule text)]
    (loop [acc []]
      (if-not (.find m)
        acc
        (let [start (.end m)
              kw (str/lower-case (.group m 1))
              quote (.group m 2)
              quoted-end (when quote (.indexOf text ^String quote (int start)))
              quoted-ok? (and quoted-end (>= quoted-end 0)
                              (not (str/includes? (subs text start quoted-end) "\n")))
              end (if quoted-ok?
                    quoted-end
                    (let [vm (.matcher ^java.util.regex.Pattern unquoted-value text)]
                      (if (.find vm (int start))
                        (if (= (.start vm) start) (.end vm) start)
                        start)))
              value (subs text start end)
              trimmed (str/trim value)
              edn-key? (and (pos? (.start m 1))
                            (= \: (.charAt text (dec (.start m 1)))))
              token-ok? (or (not= kw "token")
                            (>= (count value) 16)
                            (and (re-find #"[0-9]" value)
                                 (not (re-matches #"[0-9]+" value))))
              keep? (and (not (str/blank? trimmed))
                         (not (re-matches placeholders trimmed))
                         (not (str/starts-with? value "/"))
                         (not (str/starts-with? value "~"))
                         (not edn-key?)
                         token-ok?)]
          (recur (if keep? (conj acc ["keyword-value" start end]) acc)))))))

(defn- entropy [^String value]
  (let [n (double (count value))
        freqs (vals (frequencies value))]
    (- (reduce + 0.0 (map (fn [c] (let [p (/ c n)] (* p (/ (Math/log p) (Math/log 2.0))))) freqs)))))

(defn- high-entropy-candidate? [^String text [_ start end]]
  (let [value (subs text start end)
        segments (remove str/blank? (str/split value #"[/_\-.=+]"))
        wordlike-n (count (filter #(re-matches wordlike %) segments))
        before (str/lower-case (subs text (max 0 (- start 16)) start))]
    (and (<= (count value) high-entropy-max)
         (not (and (>= (count segments) 3) (>= wordlike-n (* 0.6 (count segments)))))
         (not (and (re-matches hex-run value)
                   (or (<= 7 (count value) 40) (= 64 (count value)))))
         (not (re-matches uuid-run value))
         (re-find #"[A-Z]" value) (re-find #"[a-z]" value) (re-find #"[0-9]" value)
         (not (str/ends-with? before "data:"))
         (not (str/includes? before ";base64,"))
         (not (and (pos? start) (contains? #{\/ \\} (.charAt text (dec start)))))
         (>= (entropy value) 4.0))))

(defn- merge-findings [findings]
  (reduce (fn [merged [kind start end :as item]]
            (let [[pkind pstart pend] (peek merged)]
              (if (or (empty? merged) (>= start pend))
                (conj merged item)
                (let [kind* (cond (and (= pkind "high-entropy") (not= kind "high-entropy")) kind
                                  (and (= kind "high-entropy") (not= pkind "high-entropy")) pkind
                                  (> (- end start) (- pend pstart)) kind
                                  :else pkind)]
                  (conj (pop merged) [kind* pstart (max pend end)])))))
          []
          (sort-by (fn [[k s e]] [s e k]) findings)))

(defn scan-secrets
  "Findings [kind start end] in TEXT, merged, UTF-16 offsets. Never the value."
  [text]
  (let [text (str text)]
    (merge-findings
     (concat (mapcat #(matcher-findings text %) structural-rules)
             (keyword-findings text)
             (filter #(high-entropy-candidate? text %)
                     (matcher-findings text ["high-entropy" high-entropy 0]))))))

(defn redact-secrets
  "Return {:text REDACTED :kinds [KIND ...]} with every finding replaced by
   [REDACTED:kind]. KINDS are distinct, first-seen order, as Emacs collects
   them from the scanner's output. In-process there is no subprocess to fail
   closed on; a thrown exception from this function is the equivalent and the
   caller must not record the turn."
  [text]
  (let [text (str text)
        findings (scan-secrets text)]
    (if (empty? findings)
      {:text text :kinds []}
      (let [sb (StringBuilder.)
            end-pos (reduce (fn [pos [kind start end]]
                              (.append sb (subs text pos start))
                              (.append sb (str "[REDACTED:" kind "]"))
                              end)
                            0 findings)]
        (.append sb (subs text end-pos))
        {:text (str sb) :kinds (vec (distinct (map first findings)))}))))

;; ---------------------------------------------------------------------------
;; Surface marker (agent-chat-split-surface-marker)

(def surface-markers
  "Leading markers that say how a turn was produced; voxterm prepends the
   speaking head to a dictated turn. Only a leading marker counts."
  [["🗣" "dictated"]])

(defn split-surface-marker
  "Return {:surface SURFACE-OR-NIL :text STRIPPED}."
  [text]
  (let [text (str text)]
    (if-let [[marker surface] (some (fn [[m s]] (when (str/starts-with? text m) [m s]))
                                    surface-markers)]
      {:surface surface :text (str/triml (subs text (count marker)))}
      {:surface nil :text text})))

;; ---------------------------------------------------------------------------
;; Quote elision (session-mode--elide-quotes)

(def quote-fence ">>>")

(defn elide-quotes
  "Replace >>> blocks in TEXT with the single word QUOTE.
   Returns {:text ELIDED :quotes [BLOCK ...]} in the order the blocks appeared.
   A block runs to a closing >>> or, failing that, to the end of the turn;
   material on the opening fence line belongs to the block."
  [text]
  (let [text (str text)
        opens (re-pattern (str "^[ \\t]*" (java.util.regex.Pattern/quote quote-fence)))
        closes (re-pattern (str "^[ \\t]*" (java.util.regex.Pattern/quote quote-fence) "[ \\t]*$"))]
    ;; Elisp's ^ matches after any newline; Java's needs (?m) for that.
    (if-not (re-find (re-pattern (str "(?m)" opens)) text)
      {:text text :quotes []}
      (let [lines (str/split text #"\n" -1)
            {:keys [out quoted current in-quote]}
            (reduce (fn [{:keys [in-quote out quoted current] :as st} line]
                      (cond
                        (and in-quote (re-find closes line))
                        (assoc st :in-quote false
                               :quoted (conj quoted (str/join "\n" current))
                               :current [])
                        in-quote (assoc st :current (conj current line))
                        (re-find opens line)
                        (let [rest (str/trim (str/replace-first line opens ""))]
                          (assoc st :in-quote true
                                 :current (if (str/blank? rest) [] [rest])
                                 :out (conj out "QUOTE")))
                        :else (assoc st :out (conj out line))))
                    {:in-quote false :out [] :quoted [] :current []}
                    lines)
            quoted (if (and in-quote (seq current))
                     (conj quoted (str/join "\n" current))
                     quoted)]
        {:text (str/trim (str/join "\n" out)) :quotes quoted}))))

;; ---------------------------------------------------------------------------
;; Sentence spans (session-mode--sentence-spans)

(def ^:private sentence-end
  "Emacs `sentence-end` with sentence-end-double-space nil: a terminator,
   optional closing quotes/brackets, then end of line or a blank."
  (re-pattern "[.?!…‽][\\]\"'”’)}»›]*(?=[ \\t\\r\\n]|$)"))

(def ^:private paragraph-break
  "A blank line ends a paragraph, and forward-sentence stops there."
  (re-pattern "\\n[ \\t]*\\n"))

(defn- find-from
  "[start end] of the first match of PATTERN in S at or after FROM, or nil."
  [^java.util.regex.Pattern pattern ^String s ^long from]
  (let [m (.matcher pattern s)]
    (when (.find m (int from)) [(.start m) (.end m)])))

(defn sentence-spans
  "Sentence spans in TEXT as [{:start :end :text}], codepoint offsets.
   Leading whitespace is skipped and trailing whitespace trimmed, so every
   span's :text equals (cp-subs text start end)."
  [text]
  (let [^String s (str text)
        n (.length s)]
    (loop [pos 0 spans []]
      (let [pos (loop [p pos]
                  (if (and (< p n) (Character/isWhitespace (.charAt s p))) (recur (inc p)) p))]
        (if (>= pos n)
          spans
          (let [[_ term-end] (find-from sentence-end s pos)
                [para-start _] (find-from paragraph-break s pos)
                end (cond
                      (and term-end para-start) (min term-end (max para-start pos))
                      term-end term-end
                      para-start (max para-start pos)
                      :else n)
                end (if (<= end pos) n end)
                raw (subs s pos end)
                clean (str/trimr raw)]
            (recur end
                   (if (str/blank? clean)
                     spans
                     (conj spans {:start (utf16->cp s pos)
                                  :end (utf16->cp s (+ pos (count clean)))
                                  :text clean})))))))))

;; ---------------------------------------------------------------------------
;; Literal cue phrases (session-mode--turn-matches)

(def default-vocabulary
  "session-mode-turn-intent-vocabulary (session-mode.el, 2026-09-22):
   literal phrases grouped by communicative intent. A cue is an
   interpretation only; it has no effect by itself."
  [["approve" ["I agree" "I approve" "that's a good fit" "that's great" "looks good" "good news" "sounds good" "you're right" "Yes you can do this" "I definitely like the idea"]]
   ["disagree" ["I disagree" "I don't agree" "I do not agree" "that's wrong" "misunderstood my intent" "doesn't match my intent" "does not match my intent" "of no use whatsoever" "a bad design" "wasn't very useful" "was not very useful"]]
   ["clarify" ["I don't understand" "help me understand" "I'd like to know" "I'd like to see some examples" "what's the story" "What gives" "I'm slightly confused" "How should we proceed" "what's our strategy"]]
   ["propose" ["I suggest" "We could perhaps" "maybe we could" "Maybe we should" "I wonder if" "I think it would be good"]]
   ["extend" ["in parallel" "Another thing we should pay attention to" "we should also" "we could also" "that's another analysis" "could be a further set of tasks"]]
   ["prioritize" ["as a matter of priority" "please do that next step" "needs to be the next" "first instance" "we have 7 minutes"]]
   ["delegate" ["please bell" "you can bell" "let's ask" "ask Zai" "we should ask" "should be sent to" "for dispatches" "you will be responsible for"]]
   ["verify" ["we should check" "we need to check" "we need to be able to validate" "we'll have to check" "we have to audit" "I want a reproduction"]]
   ["constrain" ["don't do that" "I am not asking you to" "I don't want to spend" "please use aliases" "we will not do any deep dives" "I don't want a repeat" "not going to decide things by fiat"]]
   ["defer" ["we'll do it when we get time" "we can come back to" "at some point" "for now" "defer processing"]]
   ["continue" ["please continue" "go on" "get on with it" "let's continue" "Please do 1, 2, and 3"]]
   ["withdraw" ["withdraw that pattern" "rule should be removed"]]
   ["redirect" ["rather than" "let's trim" "we will instead focus" "I'd like to return to" "what we should do is" "I want to alter" "I would want" "I would prefer" "what I want instead" "I meant that"]]
   ["explain" ["here's why" "my main point" "what I mean" "the broader long term idea" "the use cases would be" "my use case"]]
   ["report-problem" ["is currently broken" "I still see an HTTP error" "it's broken" "login doesn't work" "overlaps existing UI elements" "point of major concern" "not getting any markup" "totally underlined"]]
   ["collect" ["collect information" "getting logs" "keep a record" "record the turns"]]
   ["qualify" ["with the caveat" "to the extent that it is possible"]]
   ["ask-action" ["can you please" "please publish" "please sort this out" "I would like to have" "please update"]]])

(def vocabulary-version 3)
(def interpretation-version 3)

(defn vocabulary-from-json
  "The live vocabulary as session-mode saves it: {\"version\" .. \"rules\"
   [[TAG PHRASE ...] ...]}. Returns [[TAG [PHRASE ...]] ...] or nil when
   the shape is wrong, so a corrupt file falls back to the default."
  [data]
  (let [rules (or (get data "rules") (get data :rules))]
    (when (sequential? rules)
      (let [parsed (keep (fn [entry]
                           (when (and (sequential? entry) (string? (first entry))
                                      (re-matches #"[A-Za-z][A-Za-z0-9_-]*" (first entry))
                                      (every? string? (rest entry)))
                             [(first entry) (vec (remove str/blank? (rest entry)))]))
                         rules)]
        (when (and (seq parsed) (= (count parsed) (count rules)))
          (vec parsed))))))

(defn- phrase-pattern [phrase]
  (re-pattern (str "(?iu)" (-> (java.util.regex.Pattern/quote phrase)
                               ;; either apostrophe, as the Emacs matcher allows
                               (str/replace "'" "\\E['’]\\Q")))))

(defn turn-matches
  "[[START END TAG PHRASE] ...] literal-phrase hits in TEXT, codepoint offsets,
   sorted by start, duplicates removed. Case-insensitive, bounded by
   non-word characters on both sides."
  ([text] (turn-matches text default-vocabulary))
  ([text vocabulary]
   (let [^String s (str text)]
     (->> (for [[tag phrases] vocabulary
                phrase phrases
                :when (not (str/blank? phrase))
                :let [m (.matcher ^java.util.regex.Pattern (phrase-pattern phrase) s)]
                hit (loop [acc []]
                      (if (.find m)
                        (recur (let [beg (.start m) end (.end m)]
                                 (if (and (not (word-cp? (cp-before s beg)))
                                          (not (word-cp? (cp-at s end))))
                                   (conj acc [(utf16->cp s beg) (utf16->cp s end) tag (subs s beg end)])
                                   acc)))
                        acc))]
            hit)
          distinct
          (sort-by (fn [[start end tag _]] [start end tag]))
          vec))))

;; ---------------------------------------------------------------------------
;; Structure (session-mode--structure-turn) and record (session-mode--record-turn)

(def offset-unit "unicode-codepoints-zero-based-end-exclusive")

(defn structure-turn
  "Sentence structure and lexical observations for TEXT, not inferred intent.
   The seam's conformance test is that every :text equals source_text[start:end]."
  ([text] (structure-turn text default-vocabulary))
  ([text vocabulary]
   (let [text (str text)
         matches (turn-matches text vocabulary)
         spans (sentence-spans text)
         sentences (map-indexed
                    (fn [i {:keys [start end] :as span}]
                      (let [hits (filter (fn [[a b _ _]] (and (< a end) (> b start))) matches)
                            sid (str "s" (inc i))]
                        (merge {:id sid
                                :status (if (seq hits) "cue-only" "unresolved")
                                :cues (mapv (fn [[a b tag _]]
                                              {:start a :end b :label tag
                                               :text (cp-subs text a b)
                                               :method "literal-phrase"})
                                            hits)}
                               span)))
                    spans)]
     {:version 1
      :source_text text
      :offset_unit offset-unit
      :sentences (vec sentences)
      :unmatched (vec (keep #(when (= "unresolved" (:status %)) (:id %)) sentences))})))

(defn- iso-now [now-ms]
  (-> (java.time.Instant/ofEpochMilli (long now-ms))
      (.truncatedTo java.time.temporal.ChronoUnit/SECONDS)
      str))

(declare reply-marks)

(defn make-record
  "The turn record for one operator turn, as session-mode--record-turn writes
   it, minus the file. Secrets are redacted before parsing, storage, dispatch
   or publication; the leading surface marker is stripped first so every
   offset describes what the operator said, and the surface rides as metadata.

   OPTS: :text (required), :original-text (what was typed, when it differs:
   the failure marker form), :agent-id, :session-id, :turn-id, :evidence-id
   (the acknowledged operator row), :surface override, :failed? (the operator
   said tagging failed), :origin (default \"operator\"), :vocabulary,
   :now-ms, :redact (fn text -> {:text :kinds}; default `redact-secrets`),
   :analysis-requested? (fn record -> bool; default: every turn)."
  [{:keys [text original-text agent-id session-id turn-id evidence-id surface
           failed? origin vocabulary now-ms redact analysis-requested? operator-id author]
    :or {origin "operator" vocabulary default-vocabulary
         redact redact-secrets}}]
  (let [text-scan (redact (str text))
        original-scan (when original-text (redact (str original-text)))
        kinds (vec (distinct (concat (:kinds text-scan) (:kinds original-scan))))
        {marker-surface :surface stripped :text} (split-surface-marker (:text text-scan))
        {elided :text quotes :quotes} (elide-quotes stripped)
        ;; The original keeps its >>> blocks elided too: 象 reads the record
        ;; file, and quoted material is never 象's to read (Joe, 2026-10-04).
        ;; The quoted text itself goes to the :quotes sidecar, for display.
        original (when original-scan
                   (:text (elide-quotes (:text (split-surface-marker (:text original-scan))))))
        record (structure-turn elided vocabulary)
        requested? (or failed?
                       (if analysis-requested? (boolean (analysis-requested? record)) true))]
    {:record (merge record
                    (when (= origin "agent")
                      ;; An agent's reply: the marks it wrote are read, not inferred.
                      {:proforma_marks (reply-marks elided)})
                    (when-let [a (or author (when (= origin "agent") agent-id) operator-id)]
                      {:author a})
                    (when operator-id {:operator_id operator-id})
                    {:created_at (iso-now (or now-ms (System/currentTimeMillis)))
                     :vocabulary_version vocabulary-version
                     :interpretation_version interpretation-version
                     :tagging_failed (boolean failed?)
                     :original_text (or original elided)
                     :agent_id agent-id
                     :session_id session-id
                     :turn_id turn-id
                     :secrets_redacted kinds
                     :evidence_id evidence-id
                     :origin origin
                     :surface (or surface marker-surface "typed")
                     :quote_count (count quotes)
                     :analysis_status (if requested? "requested" "not-requested")})
     :quotes quotes
     :redacted kinds}))

;; ---------------------------------------------------------------------------
;; The brief (session-mode--analysis-instruction + session-mode--dispatch-analysis)

(def default-brief-paths
  "Absolute paths the brief names, because the delegate seat runs on the
   box where they live. Overridable per deployment."
  {:tool "/home/joe/code/futon3c/scripts/session_turn_analysis.py"
   :find "/home/joe/code/futon3c/scripts/xlate.py"
   :rnode-definitions "/home/joe/code/futon0/analysis/audits/rnode-tree/rnode-definitions.edn"})

(defn- shell-quote [s]
  (let [s (str s)]
    (if (re-matches #"[A-Za-z0-9_./:=@%+-]+" s) s (str "'" (str/replace s "'" "'\\''") "'"))))

(defn analysis-instruction
  "The bounded task tied to RECORD-PATH (session-mode--analysis-instruction)."
  [record-path {:keys [vocabulary paths]
                :or {vocabulary default-vocabulary paths default-brief-paths}}]
  (let [tool (shell-quote (:tool paths))]
    (str
     "\n\n[Session-mode structural analysis request — machine-added, not Joe's words]\n"
     "After handling the user's request, interpret the whole operator turn, including sentences with lexical cues. "
     "Do not delegate or start another conversation. Original text, offsets and unresolved sentences: " record-path "\n"
     "Use python3 " tool " template REQUEST to obtain the JSON shape. "
     "Fill every sentence with one or more fragment annotations (multiple intents allowed), or an explicit unresolved reason. "
     "Interpretation spans can cover full sentences, but are NEVER themselves displayed as underlines. "
     "Each fragment must have a separate display_cues array of exact short keyword spans (at most 8 words / 80 characters each). "
     "Leave most of each long sentence unmarked. If intent is implicit, use an empty display_cues array and explain no_surface_cue. "
     "Each fragment has exact source offsets/text, intent, target, rationale and relations "
     "(context, condition, contrast, action, rationale, goal, or dependency). "
     "Use meaningful intent vocabulary; do not treat conjunctions alone as intent. "
     "Select content-bearing cues naming the action, object, constraint or success criterion, not just discourse openers like I wonder if. "
     "For pattern alignment, compare the full passage and target to the pattern context/IF/THEN, never match on the intent label alone. "
     "Suggested intents: " (str/join ", " (map first vocabulary)) ". "
     "Intent withdraw means the operator ends or takes back an earlier act, his own or an agent's; it is not disagreement or redirection. "
     "The record may carry a happened_summary field: a machine-added note of what the agent did while answering this turn (first reply lines and commits with line counts). It is usually absent: your reading starts when the turn is sent, before the reply exists — never wait for it. When present it is context for reading the turn, not the operator's words. "
     "For a withdraw fragment, set target to the named act id when the turn names one; set it to seat-active-card only when the turn refers to this/the pattern/card in the current seat; otherwise set target to null. Never guess a withdrawal target. "
     "A withdraw label is an interpretation only and terminates nothing. This brief is interpretation version " interpretation-version ". "
     "Candidate flexiarg refs are optional: read any cited canonical pattern and explain the fit; do not invent IDs. "
     "Record inferred interpretations, not human-approved labels. "
     "To improve future draft tagging, optionally propose top-level reusable_cues with exact start/end/text, intent and rationale for reuse. "
     "Propose only short communicative phrases that generalize, not project names or arbitrary subject words. "
     "Emacs persists unassigned phrases as provisional cue hypotheses with provenance; existing assignments and human corrections win. Do not edit the vocabulary file directly. "
     "R-node reading is optional and most fragments have none. Read " (:rnode-definitions paths) " once per seat session, and re-read it whenever unsure. "
     "A fragment may include rnode: {\"node\", \"quantity\", \"operation\", \"justification\"}; use a node from that file, one of its operations, and a one-line justification naming the quantity as in the admission example. "
     "Optionally propose top-level rnode_cues: [{\"text\", \"start\", \"end\", \"node\", \"operation\", \"justification\"}] using exact source spans. "
     "Seeds stay inside the definitions file: never show the operator cue lists. "
     "Save the filled JSON to a temporary file and validate/publish with: "
     "python3 " tool " complete REQUEST ANALYSIS.json. Replace REQUEST with the record path above. "
     "If you cannot do this, say so; the record remains requested, never silently complete.\n"
     "[End structural analysis request]")))

(declare draft-brief-section candidates-brief-section)

(defn analysis-brief
  "The self-contained brief a delegate seat receives for RECORD-ID at
   RECORD-PATH (session-mode--dispatch-analysis). The delegate is assumed to
   know nothing: the record is everything; nobody waits on a reply. With
   :draft and :draft-path, the brief hands over 小象's draft to confirm or
   correct."
  [record-id record-path {:keys [requisition paths draft draft-path candidates candidates-path]
                          :or {requisition "M-futon-seams" paths default-brief-paths}
                          :as opts}]
  (let [find-tool (:find paths)]
    (str
     "Requisition: " requisition " — interpret operator turn " record-id "\n\n"
     "Interpret one operator turn. This is the whole task; there is no "
     "conversation attached to it.\n\n"
     "The turn, its sentence offsets and its metadata (including which "
     "surface it came from) are in the record named below. Read it first.\n"
     (analysis-instruction record-path opts)
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
     "    python3 " find-tool " find "
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
     "    python3 " find-tool " find "
     "\"<the move>\" --with-candidates -n 8\n"
     "  Results prefixed ? are existing proposals. If one already "
     "names your move, CITE IT in tried and do not mint a second "
     "name for it -- a vocabulary doubles when nobody checks. And try "
     "to express the move as a join of existing patterns before "
     "minting at all; say in tried that you tried.\n"
     "  python3 .../xlate.py census shows what the whole corpus has "
     "cited and proposed, and which proposals have recurred three "
     "times and are therefore ripe.\n"
     (when (and draft draft-path) (draft-brief-section draft-path draft))
     (when (and candidates candidates-path) (candidates-brief-section candidates-path candidates)))))

;; ---------------------------------------------------------------------------
;; Reply-proforma marks (session-mode--marks) read straight off an agent reply

(def proforma-marks
  "The reply-proforma marks with their intent and loop stage, as
   session-mode.el lists them (stages from futon3's turnfeed legend). An
   agent writing one declares its own act; 象 reads the declaration rather
   than inferring it, and the mark-act alignment (futon2
   holes/labs/wm-contract/mark-act-alignment.md) says which have a click act."
  [["㊩" "report-problem" "perceive"] ["🈖" "explain" "perceive"] ["㊢" "report" "perceive"]
   ["🈯" "clarify" "believe"] ["㊟" "qualify" "believe"] ["㊣" "approve" "believe"]
   ["🈚" "disagree" "believe"] ["㊮" "collect" "believe"] ["🈹" "retract" "believe"]
   ["🈲" "constrain" "evaluate"] ["🈕" "extend" "evaluate"] ["㊫" "explore" "evaluate"]
   ["㊭" "propose" "select"] ["㊝" "prioritize" "select"] ["🈘" "redirect" "select"]
   ["🈝" "defer" "select"] ["㊯" "delegate" "select"] ["🈡" "withdraw" "select"]
   ["🈸" "ask-action" "act"] ["🈰" "continue" "act"] ["㊬" "verify" "act"]
   ["㊥" "gist" "annotator"] ["🈳" "unresolved" "annotator"]])

(def ^:private mark-table (into {} (map (fn [[m i s]] [m {:intent i :stage s}]) proforma-marks)))

(defn- leading-mark
  "The proforma mark PARAGRAPH opens with, or nil."
  [^String paragraph]
  (some (fn [[m _ _]] (when (str/starts-with? paragraph m) m)) proforma-marks))

(defn reply-marks
  "The marked paragraphs of an agent reply TEXT, in order, with codepoint
   offsets into TEXT: [{:mark :intent :stage :start :end :text}]. A paragraph
   is a run of lines between blank lines, as `agreement-record/reply-asks`
   splits them; it is marked when its first character is a proforma mark.
   An optional `:` after the mark (the \"🈸: yes\" convention) is part of the
   mark, not the text."
  [text]
  (let [^String s (str text)
        m (.matcher #"(?s)[^\n]+(?:\n[^\n]+)*" s)]   ; paragraphs: no blank line inside
    (loop [acc []]
      (if-not (.find m)
        acc
        (let [raw (.group m)
              lead (- (count raw) (count (str/triml raw)))
              para (str/trim raw)
              start (+ (.start m) lead)
              end (+ start (count para))]
          (recur (if-let [mark (leading-mark para)]
                   (let [{:keys [intent stage]} (get mark-table mark)
                         body (str/triml (str/replace-first (subs para (count mark)) #"^:\s*" ""))]
                     (conj acc {:mark mark :intent intent :stage stage
                                :start (utf16->cp s start) :end (utf16->cp s end)
                                :text body}))
                   acc)))))))

(defn agent-brief
  "The brief for an agent-origin record (an agent's reply recorded as a
   turn). Shorter than the operator brief: the marks are the author's own
   declarations and are given; 象 fills fragments and cues per sentence as
   before, cites patterns, and never infers a withdrawal. The withdraw and
   acceptance machinery is for operator turns only."
  [record-id record-path {:keys [requisition paths vocabulary]
                          :or {requisition "M-futon-seams" paths default-brief-paths
                               vocabulary default-vocabulary}}]
  (let [tool (shell-quote (:tool paths))]
    (str
     "Requisition: " requisition " — interpret agent turn " record-id "\n\n"
     "Interpret one AGENT reply, recorded as a turn. This is the whole task; "
     "there is no conversation attached to it. You did NOT write this reply and "
     "nobody is waiting on an answer.\n\n"
     "The reply, its sentence offsets, its metadata and its proforma_marks are in "
     "the record named below. Read it first: " record-path "\n\n"
     "proforma_marks are the author's own declarations (" (str/join " " (map first proforma-marks))
     "), one per marked paragraph, with the intent each mark declares. Take them as given: "
     "a fragment inside a marked paragraph carries that paragraph's declared intent unless "
     "the text plainly does something else, and then say so in rationale.\n"
     "Use python3 " tool " template REQUEST to obtain the JSON shape, then fill every "
     "sentence with one or more fragments (exact offsets/text, intent, target, rationale, "
     "relations) or an explicit unresolved reason. Each fragment has a separate display_cues "
     "array of exact short keyword spans (at most 8 words / 80 characters each); leave most of "
     "each long sentence unmarked. Suggested intents: " (str/join ", " (map first vocabulary)) ". "
     "Do NOT label withdraw on an agent turn; an agent's 🈹 or 🈡 is a declaration the "
     "operator may act on, not an act. This brief is interpretation version "
     interpretation-version ".\n"
     "Search the pattern library for every fragment (python3 " (:find paths)
     " find \"<the move>\" -n 8) and cite only a pattern whose context/IF/THEN fits; record "
     "near misses in pattern_rejections. Leave pattern_refs empty when nothing fits and say so.\n"
     "Publish with: python3 " tool " complete REQUEST ANALYSIS.json. If you cannot, say so; "
     "the record remains requested, never silently complete.\n")))

;; ---------------------------------------------------------------------------
;; Analysis validation (session_turn_analysis.py validate)

(def relation-roles #{"context" "condition" "contrast" "action" "rationale" "goal" "dependency"})
(def min-cue-words 2)

(defn- invalid! [message & [data]]
  (throw (ex-info message (merge {:reason :invalid-analysis} data))))

(defn- required-text [value field]
  (if (and (string? value) (not (str/blank? value)))
    (str/trim value)
    (invalid! (str field " is required") {:field field})))

(defn g
  "Get KEY from a JSON map whether its keys are keywords or strings."
  [m k]
  (if (map? m) (let [v (get m k ::none)] (if (= v ::none) (get m (name k)) v)) nil))

(defn- short-phrase? [phrase]
  (and (<= (cp-count phrase) 80) (<= (word-count phrase) 8) (not (str/includes? phrase "\n"))))

(defn- exact-span? [source start end text]
  (and (int? start) (int? end) (<= 0 start) (< start end) (<= end (cp-count source))
       (= (cp-subs source start end) text)))

(defn sha256
  "Hex SHA-256 of S as UTF-8."
  [^String s]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(defn validate-analysis
  "Validate ANALYSIS against RECORD and return the canonical analysis map, or
   throw ex-info {:reason :invalid-analysis} naming what is wrong.

   OPTS :pattern-source is (fn [id] -> flexiarg content or nil), the library
   lookup validate() does on disk; refs and rejections are checked through it.
   Without it, pattern ids are still shape-checked but not resolved, and the
   result says so in :pattern_check \"unresolved\". R-node fields pass through
   only when :rnode-contract (node -> {:operations #{..} :label :stage}) is
   given; otherwise they are dropped and counted, as validate() counts them."
  [record analysis {:keys [pattern-source rnode-contract now-ms]}]
  (let [source (str (g record :source_text))
        labeller (required-text (g analysis :labeller) "labeller")
        expected (into {} (map (fn [s] [(g s :id) s]) (g record :sentences)))
        sentences (g analysis :sentences)
        _ (when-not (sequential? sentences) (invalid! "sentences must be an array"))
        ids (map #(g % :id) sentences)
        _ (when (or (not= (count ids) (count (set ids))) (not= (set ids) (set (keys expected))))
            (invalid! "one analysis per source sentence is required, including unresolved ones"))
        dropped (atom {:unknown_node 0 :invalid_operation 0 :empty_justification 0
                       :inexact_span 0 :no_contract 0})
        accepted-rnodes (atom 0)
        check-pattern (fn [pid what]
                        (when-not (re-matches #"(?U)[\w-]+(?:/[\w'-]+)+" pid)
                          (invalid! (str "invalid canonical pattern id: " pid) {:field what}))
                        (when pattern-source
                          (let [content (pattern-source pid)]
                            (when-not (string? content)
                              (invalid! (str "unknown " what ": " pid) {:field what}))
                            (when-not (re-find (re-pattern (str "(?m)^@(?:arg|flexiarg|multiarg)\\s+"
                                                                (java.util.regex.Pattern/quote pid)
                                                                "\\s*$"))
                                               content)
                              (invalid! (str "pattern declaration does not match: " pid) {:field what}))
                            content)))
        canonical
        (vec
         (for [entry sentences
               :let [sentence (get expected (g entry :id))
                     s-start (g sentence :start) s-end (g sentence :end)
                     fragments (g entry :fragments)
                     _ (when-not (sequential? fragments) (invalid! "fragments must be an array"))
                     reason (or (g entry :unresolved_reason) "")
                     _ (when-not (string? reason) (invalid! "unresolved_reason must be a string"))
                     _ (when (empty? fragments)
                         (required-text reason "unresolved_reason for an unclassified sentence"))
                     checked
                     (vec
                      (for [fragment fragments
                            :let [start (g fragment :start) end (g fragment :end)
                                  _ (when-not (and (int? start) (int? end)
                                                   (<= s-start start) (< start end) (<= end s-end)
                                                   (= (cp-subs source start end) (g fragment :text)))
                                      (invalid! "fragment offsets/text must match their source sentence exactly"
                                                {:sentence (g entry :id)}))
                                  intent (required-text (g fragment :intent) "intent")
                                  _ (when-not (re-matches #"[a-z][a-z0-9_-]*" intent)
                                      (invalid! "intent must be a vocabulary label, not a sentence"))
                                  target (g fragment :target)
                                  target (if (and (= intent "withdraw") (nil? target))
                                           nil
                                           (required-text target "target"))
                                  rationale (required-text (g fragment :rationale) "rationale")
                                  roles (g fragment :relations)
                                  _ (when-not (and (sequential? roles) (seq roles)
                                                   (every? relation-roles roles))
                                      (invalid! "relations must name at least one documented structural role"))
                                  cues (g fragment :display_cues)
                                  _ (when-not (sequential? cues)
                                      (invalid! "display_cues must be an explicit array, separate from interpretation spans"))
                                  checked-cues
                                  (vec (for [cue cues
                                             :let [a (g cue :start) b (g cue :end)
                                                   _ (when-not (and (int? a) (int? b) (<= start a) (< a b) (<= b end)
                                                                    (= (cp-subs source a b) (g cue :text)))
                                                       (invalid! "display cue offsets/text must match inside their interpretation span"))
                                                   phrase (cp-subs source a b)
                                                   _ (when-not (short-phrase? phrase)
                                                       (invalid! "display cues must be short keyword phrases (at most 8 words / 80 characters)"))]]
                                         {:start a :end b :text phrase}))
                                  no-cue (or (g fragment :no_surface_cue) "")
                                  _ (when (empty? checked-cues)
                                      (required-text no-cue "no_surface_cue when intent has no explicit keyword"))
                                  refs (or (g fragment :pattern_refs) [])
                                  _ (when-not (sequential? refs) (invalid! "pattern_refs must be an array"))
                                  checked-refs
                                  (vec (for [ref refs
                                             :let [pid (required-text (g ref :id) "pattern id")
                                                   content (check-pattern pid "canonical pattern")]]
                                         (cond-> {:id pid
                                                  :rationale (required-text (g ref :rationale) "pattern fit")
                                                  :status "candidate"}
                                           content (assoc :source_sha256 (sha256 content)))))
                                  rejected (or (g fragment :pattern_rejections) [])
                                  _ (when-not (sequential? rejected) (invalid! "pattern_rejections must be an array"))
                                  checked-rejections
                                  (vec (for [ref rejected
                                             :let [pid (required-text (g ref :id) "rejected pattern id")
                                                   _ (check-pattern pid "rejected pattern")]]
                                         {:id pid
                                          :reason (required-text (g ref :reason) "why the rejected pattern does not fit")
                                          :query (str/trim (str (or (g ref :query) "")))}))
                                  rnode (g fragment :rnode)
                                  rnode* (when (map? rnode)
                                           (if-not rnode-contract
                                             (do (swap! dropped update :no_contract inc) nil)
                                             (let [node (g rnode :node)
                                                   definition (get rnode-contract node)
                                                   op (g rnode :operation)
                                                   just (g rnode :justification)]
                                               (cond
                                                 (nil? definition) (do (swap! dropped update :unknown_node inc) nil)
                                                 (not (contains? (set (:operations definition)) op))
                                                 (do (swap! dropped update :invalid_operation inc) nil)
                                                 (not (and (string? just) (not (str/blank? just))))
                                                 (do (swap! dropped update :empty_justification inc) nil)
                                                 :else (do (swap! accepted-rnodes inc)
                                                           {:node node :operation op
                                                            :quantity (g rnode :quantity)
                                                            :justification (str/trim just)
                                                            :label (:label definition)
                                                            :stage (:stage definition)})))))]]
                        (cond-> {:start start :end end :text (g fragment :text)
                                 :intent intent :target target :rationale rationale
                                 :relations (vec roles)
                                 :display_cues checked-cues :no_surface_cue no-cue
                                 :pattern_refs checked-refs
                                 :pattern_rejections checked-rejections}
                          rnode* (assoc :rnode rnode*))))
                     sentence-text (cp-subs source s-start s-end)
                     total (word-count sentence-text)]
               :let [_ (when (> total 8)
                         (let [covered (reduce + 0 (for [item checked cue (:display_cues item)]
                                                     (word-count (:text cue))))
                               budget (max (quot total 2) min-cue-words)]
                           (when (> covered budget)
                             (invalid! (str "display cues must leave most of a sentence unmarked: "
                                            (g entry :id) " marks " covered " of " total " words ("
                                            (quot (* 100 covered) total) "%); the budget here is "
                                            budget " words, so drop about " (- covered budget)
                                            ". Cues on it: "
                                            (str/join ", " (sort (for [item checked cue (:display_cues item)]
                                                                   (pr-str (:text cue))))))
                                       {:sentence (g entry :id)}))))]]
           {:id (g entry :id) :fragments checked :unresolved_reason reason}))
        reusable (or (g analysis :reusable_cues) [])
        _ (when-not (sequential? reusable) (invalid! "reusable_cues must be an array"))
        learned (vec (for [cue reusable
                           :let [start (g cue :start) end (g cue :end)
                                 _ (when-not (exact-span? source start end (g cue :text))
                                     (invalid! "reusable cue must be an exact source span"))
                                 phrase (cp-subs source start end)
                                 _ (when-not (short-phrase? phrase)
                                     (invalid! "reusable cue must be a short phrase"))
                                 intent (required-text (g cue :intent) "reusable cue intent")
                                 _ (when-not (re-matches #"[a-z][a-z0-9_-]*" intent)
                                     (invalid! "invalid reusable intent"))]]
                       {:start start :end end :text phrase :intent intent
                        :rationale (required-text (g cue :rationale) "reuse rationale")}))
        rnode-cues (or (g analysis :rnode_cues) [])
        _ (when-not (sequential? rnode-cues) (invalid! "rnode_cues must be an array"))
        checked-rnode-cues
        (vec (keep (fn [cue]
                     (let [definition (and (map? cue) rnode-contract (get rnode-contract (g cue :node)))
                           start (g cue :start) end (g cue :end)
                           just (g cue :justification)]
                       (cond
                         (not rnode-contract) (do (swap! dropped update :no_contract inc) nil)
                         (nil? definition) (do (swap! dropped update :unknown_node inc) nil)
                         (not (contains? (set (:operations definition)) (g cue :operation)))
                         (do (swap! dropped update :invalid_operation inc) nil)
                         (not (and (string? just) (not (str/blank? just))))
                         (do (swap! dropped update :empty_justification inc) nil)
                         (not (exact-span? source start end (g cue :text)))
                         (do (swap! dropped update :inexact_span inc) nil)
                         :else {:start start :end end :text (cp-subs source start end)
                                :node (g cue :node) :operation (g cue :operation)
                                :justification (str/trim just)
                                :label (:label definition) :stage (:stage definition)})))
                   rnode-cues))]
    {:version 2 :status "analyzed" :method "agent-interpretation"
     :interpretation_version (or (g record :interpretation_version) 1)
     :vocabulary_version (or (g record :vocabulary_version) 1)
     :human_approved false :labeller labeller :reusable_cues learned
     :rnode_cues checked-rnode-cues
     :rnode_validation {:accepted_fragments @accepted-rnodes
                        :accepted_cues (count checked-rnode-cues)
                        :dropped @dropped}
     :pattern_check (if pattern-source "resolved" "unresolved")
     :evidence_id (g record :evidence_id)
     :created_at (str (java.time.Instant/ofEpochMilli (long (or now-ms (System/currentTimeMillis)))))
     :source_text source
     :source_sha256 (sha256 source)
     :offset_unit (g record :offset_unit)
     :sentences canonical}))

;; ---------------------------------------------------------------------------
;; 小象 drafts: a classical best-effort reading given to 象 before it reads
;;
;; The pre-parse is the proforma move on the reader's side: the structure
;; (fragments, offsets, candidate intents) is given, and 象 confirms or
;; corrects instead of producing. Every published fragment then records its
;; basis, so the corpus can measure how often the LLM pass changed anything,
;; and a reading confirmed from a draft can be kept out of 小象's training.

(def draft-labeller "小象")

(defn validate-draft
  "Validate DRAFT (xiaoxiang_preview.py's output: [{start end text intent|nil
   guesses precision}], or a map holding it under :fragments) against RECORD
   and return the canonical draft map. Offsets must be exact codepoint spans
   of source_text; an intent, when present, must be a vocabulary label; a
   fragment with no sure intent keeps its guesses. A fragment may carry a
   :basis (\"declared\" or \"model\") and, when declared, the proforma :mark it
   came from; both are kept, anything else is invalid."
  [record draft {:keys [now-ms]}]
  (let [source (str (g record :source_text))
        fragments (if (map? draft) (or (g draft :fragments) []) draft)
        _ (when-not (sequential? fragments) (invalid! "draft fragments must be an array"))
        sentences (g record :sentences)
        sentence-of (fn [start end]
                      (some (fn [s] (when (and (<= (g s :start) start) (<= end (g s :end))) (g s :id)))
                            sentences))
        checked (vec (for [f fragments
                           :let [start (g f :start) end (g f :end)
                                 _ (when-not (exact-span? source start end (g f :text))
                                     (invalid! "draft fragment offsets/text must match source_text exactly"))
                                 intent (g f :intent)
                                 _ (when (and intent (not (re-matches #"[a-z][a-z0-9_-]*" (str intent))))
                                     (invalid! "draft intent must be a vocabulary label"))
                                 guesses (vec (filter string? (or (g f :guesses) [])))
                                 precision (g f :precision)
                                 basis (g f :basis)
                                 _ (when (and (some? basis) (not (#{"declared" "model"} (str basis))))
                                     (invalid! "draft basis must be \"declared\" or \"model\""))
                                 mark (g f :mark)
                                 _ (when (and (some? mark) (not (contains? mark-table (str mark))))
                                     (invalid! "draft mark must be a proforma mark"))]]
                       (cond-> {:start start :end end :text (g f :text)
                                :intent intent :sure (boolean intent)
                                :guesses guesses
                                :sentence (sentence-of start end)}
                         (number? precision) (assoc :precision (double precision))
                         (some? basis) (assoc :basis (str basis))
                         (some? mark) (assoc :mark (str mark)))))]
    {:version 1 :status "drafted" :method "xiaoxiang-naive-bayes" :labeller draft-labeller
     :created_at (str (java.time.Instant/ofEpochMilli (long (or now-ms (System/currentTimeMillis)))))
     :source_text source :source_sha256 (sha256 source) :offset_unit (g record :offset_unit)
     :fragments checked}))

(def act-bearing-intents
  "Intents a draft may not settle on its own: each starts something the
   operator or an agent must act on, so 象 reads these turns."
  #{"withdraw" "retract" "ask-action" "delegate" "disagree" "constrain" "redirect"})

(def ^:private acceptance-or-undo
  (re-pattern "(?i)^\\s*(?:🈸:\\s*)?(?:yes|undo)\\b"))

(defn routine-draft?
  "True when DRAFT settles RECORD well enough that no LLM reading is needed:
   every fragment has a sure intent, none is act-bearing, every sentence has
   a fragment, the turn is short (at most MAX-SENTENCES, default 3), the
   origin is operator, and the text is not an acceptance or an undo. A
   conservative predicate on purpose: a skipped reading is an unrecorded
   act if the predicate is wrong."
  [record draft & {:keys [max-sentences] :or {max-sentences 3}}]
  (let [fragments (or (g draft :fragments) [])
        sentences (or (g record :sentences) [])
        covered (set (keep :sentence fragments))]
    (boolean
     (and (= "operator" (or (g record :origin) "operator"))
          (seq fragments)
          (<= (count sentences) max-sentences)
          (every? :sure fragments)
          (not-any? #(contains? act-bearing-intents (:intent %)) fragments)
          (every? #(contains? covered (g % :id)) sentences)
          (not (re-find acceptance-or-undo (str (g record :source_text))))
          (not (g record :tagging_failed))))))

(defn- overlaps? [a b]
  (and (< (:start a) (g b :end)) (> (:end a) (g b :start))))

(defn annotate-with-draft
  "Stamp each fragment of the canonical ANALYSIS with its :basis against
   DRAFT: \"xiaoxiang\" (same span, same intent), \"xiang-relabelled\" (same
   span, other intent), \"xiang-resegmented\" (overlaps a draft fragment with
   another span), \"xiang\" (no draft fragment there). Adds :draft_agreement
   counts, including :dropped (draft fragments no published fragment
   overlaps) and :unsure (draft fragments 小象 did not label). Without a
   draft every basis is \"xiang\" and :draft_agreement is nil."
  [analysis draft]
  (if-not draft
    (assoc analysis :draft_agreement nil
           :sentences (mapv (fn [s] (update s :fragments #(mapv (fn [f] (assoc f :basis "xiang")) %)))
                            (:sentences analysis)))
    (let [dfs (or (g draft :fragments) [])
          basis-of (fn [f]
                     (let [same (some (fn [d] (when (and (= (:start f) (g d :start)) (= (:end f) (g d :end))) d)) dfs)]
                       (cond
                         (and same (:intent same) (= (:intent same) (:intent f))) "xiaoxiang"
                         (and same (:intent same)) "xiang-relabelled"
                         same "xiang"              ; 小象 had the span but no label
                         (some #(overlaps? f %) dfs) "xiang-resegmented"
                         :else "xiang")))
          sentences (mapv (fn [s] (update s :fragments #(mapv (fn [f] (assoc f :basis (basis-of f))) %)))
                          (:sentences analysis))
          published (mapcat :fragments sentences)
          counts (frequencies (map :basis published))
          dropped (count (remove (fn [d] (some #(overlaps? % d) published)) dfs))]
      (assoc analysis :sentences sentences
             :draft_agreement {:agreed (get counts "xiaoxiang" 0)
                               :relabelled (get counts "xiang-relabelled" 0)
                               :resegmented (get counts "xiang-resegmented" 0)
                               :new (get counts "xiang" 0)
                               :dropped dropped
                               :unsure (count (remove :intent dfs))
                               :draft_fragments (count dfs)
                               :published_fragments (count published)}))))

(defn candidates-brief-section
  "The paragraph the brief adds when pattern candidates were precomputed at
   CANDIDATES-PATH: BM25 already ran for every fragment (or sentence), and
   the seat reads IF/THEN of the hits instead of searching. The search
   stays available for a phrasing the precompute did not try."
  [candidates-path candidates]
  (let [entries (seq candidates)]
    (str
     "\n\nPATTERN CANDIDATES WERE PRECOMPUTED: " candidates-path "\n"
     "BM25 over the library has already run for each fragment below, one line per hit: id, "
     "score, title, then the pattern's context and conclusion. Read those lines FIRST and cite a "
     "hit only when its context/IF/THEN describes the operator's move; put a near miss in "
     "pattern_rejections with the query that surfaced it. Search again only for a phrasing of the "
     "MOVE that these did not try. A query with no fitting hit is a real finding: leave "
     "pattern_refs empty and propose a candidate as the instruction says.\n"
     (str/join "\n"
               (for [[query hits] entries]
                 (str "  Q " (pr-str (let [q (str query)] (subs q 0 (min 80 (count q))))) "\n"
                      (if (seq hits)
                        (str/join "\n" (for [h hits]
                                          (str "    " (g h :id) " (" (g h :score) ") " (g h :title)
                                               (when-let [c (g h :context)] (str "\n      context: " c))
                                               (when-let [c (g h :conclusion)] (str "\n      conclusion: " c)))))
                        "    (no hits)")))))))

(defn draft-brief-section
  "The paragraph the brief adds when a draft exists at DRAFT-PATH: what is
   firm, what is a proposal, and what costs nothing."
  [draft-path draft]
  (let [fs (or (g draft :fragments) [])]
    (str
     "\n\nA CLASSICAL DRAFT EXISTS: " draft-path "\n"
     "小象 (naive Bayes over 象's past readings, about 0.1 s) has already split this turn into "
     (count fs) " fragment" (when (not= 1 (count fs)) "s") " with exact offsets and, where it was sure, an intent. "
     "Start from the draft rather than from nothing:\n"
     "- Offsets and fragment boundaries in the draft are firm. Keep them unless a boundary is wrong; "
     "a fragment you keep costs you nothing, a split or merge must be exact.\n"
     "- A fragment with an intent is a PROPOSAL (right about half the time or better for that intent). "
     "Accept it or relabel it; say why in rationale when you relabel.\n"
     "- A fragment with intent null carries two guesses; you decide.\n"
     "- Display cues, target, relations, rationale and pattern_refs are yours as before.\n"
     "The draft is not human-approved and your reading is recorded against it fragment by fragment "
     "(agreed / relabelled / resegmented), so a rubber stamp is visible and so is a disagreement.\n"
     (str/join "\n" (map (fn [f] (str "    [" (:start f) "," (:end f) ") "
                                      (if (:intent f) (str (:intent f) (when-let [p (:precision f)] (format " (p %.2f)" p)))
                                          (str "? " (str/join "/" (:guesses f))))
                                      "  " (pr-str (let [t (str (:text f))] (subs t 0 (min 60 (count t)))))))
                         fs)))))

;; ---------------------------------------------------------------------------
;; Reading a dispatched job (turn_dispatch_reap.py)

(def terminal-bad #{"failed" "refused" "error" "cancelled" "timeout"})

(defn job-reason
  "The last thing the job said, which is what a person actually needs."
  [job]
  (or (some (fn [ev]
              (some (fn [k] (when-let [v (g ev k)]
                              (when-not (str/blank? (str v))
                                (str (or (g ev :type) "?") ": " (subs (str v) 0 (min 400 (count (str v))))))))
                    [:text :message :error :reason]))
            (reverse (or (g job :events) [])))
      (some-> (g job :terminal-message) str)
      (some-> (g job :state) str)
      "no events recorded"))

(defn job-outcome
  "Classify JOB (the public view of GET /api/alpha/invoke/jobs/:id) as the
   reaper does: {:outcome :running|:refused|:failed|:unreachable :state :reason}.
   A nil or :unreachable JOB is :unreachable. Only the analysis writer sets
   analyzed; this never does."
  [job]
  (cond
    (nil? job) {:outcome :unreachable :reason "job not found"}
    (g job :unreachable) {:outcome :unreachable :reason (str (g job :unreachable))}
    :else
    (let [state (str/lower-case (str (or (g job :state) (g job :status) "")))
          executed (g (g job :execution) :executed)
          reason (job-reason job)]
      (cond
        (or (contains? terminal-bad state)
            (and (contains? #{"done" "finished"} state) (false? executed)))
        {:outcome (if (contains? #{"failed" "error"} state) :failed :refused)
         :state state :reason reason}
        :else {:outcome :running :state state :reason reason}))))

(defn quota-failure?
  "session-mode--quota-failure-p over a reason string."
  [reason]
  (boolean (re-find #"(?i)usage limit|quota|HTTP 429|rate.limit" (str reason))))

(defn store-busy-failure?
  "session-mode--store-busy-failure-p: transient futon1b admission load."
  [reason]
  (boolean (re-find #"futon1b busy|clock/store-busy|Turn not started: futon1b" (str reason))))

;; ---------------------------------------------------------------------------
;; Withdraw fragments and their notices (session-turn-analysis.el)

(defn withdrawal-fragments
  "[[FRAGMENT-ID FRAGMENT] ...] for withdraw intents in ANALYSIS; the id is
   sentence-id:index, as Emacs names them."
  [analysis]
  (vec (for [sentence (g analysis :sentences)
             [index fragment] (map-indexed vector (g sentence :fragments))
             :when (= "withdraw" (g fragment :intent))]
         [(str (g sentence :id) ":" index) fragment])))

(defn withdrawal-notice
  "The fixed notice for a withdrawal OUTCOME, or nil when it has none."
  [outcome]
  (let [status (or (g outcome :status) 0)
        reason (g outcome :reason)
        effect-id (g outcome :effect_id)]
    (cond
      (and (= 200 status) (string? effect-id) (str/starts-with? effect-id "act:"))
      {:kind "effect" :effect_id effect-id
       :text (str "withdraw inferred: effect " effect-id " (undo to reverse)")}
      (and (= 403 status) (= "no-grant" reason))
      {:kind "no-grant" :text "withdraw inferred: off (no grant)"}
      (and (= 422 status) (contains? #{"target-unresolved" "target-not-visible"} reason))
      {:kind "unresolved" :text "withdraw inferred: unresolved (no target)"})))
