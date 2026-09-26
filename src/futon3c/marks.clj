(ns futon3c.marks
  "Mark recognizer for chat-turn and turn-round evidence.

   Vocabulary v0:
     ✘ correction, ✓ approval, 💡 idea

   Two channels, one vocabulary (M-points-de-fuite author-invariance):
   - operator chat-turns mint the core tags (:correction :approval :idea) —
     the gold channel with γ/label semantics;
   - agent turn-rounds (the zai/zaif per-round narration) mint self-prefixed
     tags (:self-correction :self-idea ...) — declared self-talk acts, kept
     out of the gold channel by construction, promotion earned by usage.

   Marks are queryable through evidence tags, because text indexing strips the
   glyphs.  The recognizer is pure and the boundary wiring is kill-switchable
   with FUTON3C_MARK_RECOGNIZER=false (both channels) and
   FUTON3C_SELF_MARKS=false (agent channel only)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def registry
  "Mark vocabulary. Core tier (✘ ✓ 💡) mints γ events and (✓/✘ only) reward
   labels. The :second-string tier (2026-07-12, from the operator-named
   clustering-probe types, C-c , hydra) is tags-only shadow vocabulary —
   visible in the store so usage can argue promotion; mints no labels
   (mark_labels.py maps ✓/✘ exclusively), no γ events (gamma-mark-glyphs
   maps the core tier only). Absent :tier = core."
  {"✘" {:type :correction :tag :correction}
   "✓" {:type :approval :tag :approval}
   "💡" {:type :idea :tag :idea}
   "🧭" {:type :guidance :tag :guidance :tier :second-string}
   "♟" {:type :tactics :tag :tactics :tier :second-string}
   "⚠" {:type :concern :tag :concern :tier :second-string}
   "📌" {:type :fact :tag :fact :tier :second-string}
   "👏" {:type :encouragement :tag :encouragement :tier :second-string}
   "🙏" {:type :request :tag :request :tier :second-string}
   "🌿" {:type :extension :tag :extension :tier :second-string}
   "📋" {:type :procedural :tag :procedural :tier :second-string}
   "⚖" {:type :hinge :tag :hinge :tier :second-string}})

(def glyph-pattern
  (re-pattern (str/join "|" (map java.util.regex.Pattern/quote (keys registry)))))

(def long-form-pattern
  "Long-form EDN marks, e.g. (✘ :ref E-foo \"why\") — built from the registry
   so second-string glyphs parse too (was hardcoded ✘|✓|💡)."
  (re-pattern (str "\\((" (str/join "|" (map java.util.regex.Pattern/quote (keys registry)))
                   ")(?:\\s+[^()]*)?\\)")))

(defn enabled?
  []
  (not (#{"0" "false" "no" "off"}
        (some-> (System/getenv "FUTON3C_MARK_RECOGNIZER")
                str/lower-case
                str/trim))))

(defn self-marks-enabled?
  []
  (not (#{"0" "false" "no" "off"}
        (some-> (System/getenv "FUTON3C_SELF_MARKS")
                str/lower-case
                str/trim))))

(defn- body-key [entry]
  (if (contains? entry :evidence/body) :evidence/body :body))

(defn- tags-key [entry]
  (if (contains? entry :evidence/tags) :evidence/tags :tags))

(defn- author-of [entry]
  (or (:evidence/author entry) (:author entry)))

(defn- body-of [entry]
  (let [b (get entry (body-key entry))]
    (if (map? b) b {})))

(defn- chat-turn? [entry]
  (let [event (:event (body-of entry))]
    (= "chat-turn" (some-> event name))))

(defn- operator-turn? [entry]
  (= "joe" (str/lower-case (str (author-of entry)))))

(defn- agent-turn-round?
  "A zai/zaif per-round transcript entry (zai-api persist-round!), agent-
   authored. The narration text in these rounds is the agent's self-talk;
   glyphs there are declared self-marks, not operator marks."
  [entry]
  (and (= "turn-round" (some-> (:event (body-of entry)) name))
       (not (operator-turn? entry))))

(defn- turn-text [entry]
  (or (:text (body-of entry))
      (:message (body-of entry))
      (:content (body-of entry))
      ""))

(defn- span-overlaps? [[a b] [c d]]
  (and (< a d) (< c b)))

(defn- word-before
  "The word token immediately preceding idx, lowercased; nil at text start or
   when the preceding token is not a word (punctuation, another glyph)."
  [text idx]
  (some-> (re-find #"([A-Za-z']+)[^A-Za-z']*$" (subs text 0 idx))
          second
          str/lower-case))

(defn- mention-after? [text idx glyph]
  (let [after (subs text (min (count text) (+ idx (count glyph))))]
    (boolean (re-find #"^\s*(?:for\s+(?:example|approval|correction|idea)s?|as\s+(?:an?\s+)?(?:approval|correction|idea)s?)\b"
                      (str/lower-case after)))))

(defn- mention? [text idx glyph]
  ;; A mark is a MENTION only when the text talks ABOUT the glyph: the glyph
  ;; as a noun-phrase object ("with ✘", "a ✓", "the 💡") or immediately named
  ;; ("✘ for example", "✓ for approval"). Anything else is an EVENT. The
  ;; earlier broad before-context regex (mark|add|use|type|write|... anywhere
  ;; in 80 chars) misclassified real approvals like "I fixed the type error ✓"
  ;; — recall loss on an operator-gold channel outranks mention precision.
  (boolean (or (contains? #{"a" "an" "the" "with"} (word-before text idx))
               (mention-after? text idx glyph))))

(defn- parse-ref-token [x]
  (cond
    (nil? x) nil
    (keyword? x) (name x)
    (symbol? x) (name x)
    :else (str x)))

(defn- long-form-at [form-text offset]
  (try
    (let [form (edn/read-string form-text)
          glyph (some-> form first str)
          meta (get registry glyph)]
      (when meta
        (let [tail (rest form)
              pairs (partition-all 2 tail)
              ref (some (fn [[k v]] (when (= :ref k) (parse-ref-token v))) pairs)
              payload (last (filter string? tail))]
          (merge {:glyph glyph
                  :verdict :event
                  :offset offset
                  :ref ref
                  :payload payload}
                 meta))))
    (catch Exception _
      nil)))

(defn- long-form-marks [text]
  (let [matcher (re-matcher long-form-pattern text)]
    (loop [out []]
      (if (.find matcher)
        (let [form-text (.group matcher)
              offset (.start matcher)
              mark (long-form-at form-text offset)]
          (recur (cond-> out mark (conj mark))))
        out))))

(defn- bare-marks [text occupied-spans]
  (let [matcher (re-matcher glyph-pattern text)]
    (loop [out []]
      (if (.find matcher)
        (let [glyph (.group matcher)
              start (.start matcher)
              end (.end matcher)]
          (if (some #(span-overlaps? [start end] %) occupied-spans)
            (recur out)
            (let [meta (registry glyph)]
              (recur (conj out
                           (merge {:glyph glyph
                                   :verdict (if (mention? text start glyph)
                                              :mention
                                              :event)
                                   :offset start
                                   :ref nil
                                   :payload nil}
                                  meta))))))
        out))))

(defn recognize-marks
  "Parse text into mark structures with :verdict :event or :mention."
  [text]
  (let [text (or text "")
        longs (long-form-marks text)
        occupied (mapv (fn [m]
                         [(:offset m)
                          (+ (:offset m)
                             (count (re-find #"\([^)]*\)"
                                             (subs text (:offset m)))))])
                       longs)]
    (->> (concat longs (bare-marks text occupied))
         (sort-by :offset)
         (mapv #(select-keys % [:glyph :verdict :type :ref :payload :offset])))))

(defn event-tags [marks]
  (->> marks
       (filter #(= :event (:verdict %)))
       (map (comp :tag registry :glyph))
       (remove nil?)
       distinct
       vec))

(defn self-event-tags
  "Event tags for the agent self-mark channel: same vocabulary, self-prefixed
   (:correction → :self-correction), so agent marks never collide with the
   operator gold channel that mark_labels.py / gamma-mark-glyphs key on."
  [marks]
  (mapv #(keyword (str "self-" (name %))) (event-tags marks)))

(defn decorate-turn
  "Decorate an operator chat-turn evidence entry with parsed marks.

   Idempotent: recomputes the same :marks vector and de-duplicates tags."
  [entry]
  (if-not (and (map? entry) (chat-turn? entry) (operator-turn? entry))
    entry
    (let [marks (recognize-marks (turn-text entry))
          bkey (body-key entry)
          tkey (tags-key entry)
          tags (vec (distinct (concat (or (get entry tkey) [])
                                      (event-tags marks))))]
      (cond-> entry
        (seq marks)
        (assoc-in [bkey :marks] marks)

        (seq tags)
        (assoc tkey tags)))))

(defn decorate-agent-round
  "Decorate an agent turn-round evidence entry with parsed self-marks.

   Same recognizer, self-prefixed tags (:self-correction, :self-idea, ...).
   Idempotent: recomputes the same :marks vector and de-duplicates tags."
  [entry]
  (if-not (and (map? entry) (agent-turn-round? entry))
    entry
    (let [marks (recognize-marks (turn-text entry))
          bkey (body-key entry)
          tkey (tags-key entry)
          tags (vec (distinct (concat (or (get entry tkey) [])
                                      (self-event-tags marks))))]
      (cond-> entry
        (seq marks)
        (assoc-in [bkey :marks] marks)

        (seq tags)
        (assoc tkey tags)))))

(defn maybe-decorate-turn [entry]
  (cond
    (not (enabled?)) entry
    (and (map? entry) (agent-turn-round? entry))
    (if (self-marks-enabled?) (decorate-agent-round entry) entry)
    :else (decorate-turn entry)))
