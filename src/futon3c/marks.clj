(ns futon3c.marks
  "Operator mark recognizer for chat-turn evidence.

   Vocabulary v0:
     ✘ correction, ✓ approval, 💡 idea

   Marks are queryable through evidence tags, because text indexing strips the
   glyphs.  The recognizer is pure and the boundary wiring is kill-switchable
   with FUTON3C_MARK_RECOGNIZER=false."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def registry
  {"✘" {:type :correction :tag :correction}
   "✓" {:type :approval :tag :approval}
   "💡" {:type :idea :tag :idea}})

(def glyph-pattern
  (re-pattern (str/join "|" (map java.util.regex.Pattern/quote (keys registry)))))

(defn enabled?
  []
  (not (#{"0" "false" "no" "off"}
        (some-> (System/getenv "FUTON3C_MARK_RECOGNIZER")
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

(defn- turn-text [entry]
  (or (:text (body-of entry))
      (:message (body-of entry))
      (:content (body-of entry))
      ""))

(defn- span-overlaps? [[a b] [c d]]
  (and (< a d) (< c b)))

(defn- paren-span-around [text idx]
  (let [open (.lastIndexOf text "(" idx)
        close (.indexOf text ")" idx)]
    (when (and (not= -1 open) (not= -1 close) (< open idx close))
      [open (inc close)])))

(defn- local-context [text idx]
  (if-let [[a b] (paren-span-around text idx)]
    (subs text a b)
    (let [start (max 0 (- idx 80))
          end (min (count text) (+ idx 80))]
      (subs text start end))))

(defn- mention? [text idx glyph]
  (let [in-paren? (boolean (paren-span-around text idx))
        ctx (local-context text idx)
        rel (- idx (max 0 (- idx 80)))
        ;; If local-context chose a parenthetical, recompute the glyph offset
        ;; inside that context.
        rel (or (str/index-of ctx glyph) rel)
        before (subs ctx 0 rel)
        after (subs ctx (min (count ctx) (+ rel (count glyph))))
        before-l (str/lower-case before)
        after-l (str/lower-case after)]
    (boolean
     (or
      ;; "I could mark corrections with ✘ for example"
      (re-find #"\b(mark|marks|marked|glyph|symbol|convention|add|use|using|enter|type|write|with)\b"
               before-l)
      ;; "(maybe we can add a ✓ for approval)"
      (and in-paren?
           (re-find #"\b(for example|for approval|for correction|for idea|as approval|as correction|as idea)\b"
                    after-l))))))

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
  (let [matcher (re-matcher #"\((✘|✓|💡)(?:\s+[^()]*)?\)" text)]
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

(defn maybe-decorate-turn [entry]
  (if (enabled?)
    (decorate-turn entry)
    entry))
