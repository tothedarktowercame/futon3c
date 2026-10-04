(ns futon3c.xiang.reply-target
  "Which agent turn, and which paragraph of it, an operator turn answers.

   Joe (2026-10-04): between two of his turns the agent may have answered
   several bellbacks, so \"the previous agent turn\" is often not what he is
   replying to. He points with the reply proforma: a paragraph that opens
   `<mark>:` answers the agent's paragraph carrying that mark. It declares
   what he responds to, not his own intent, which 象 still reads. A
   paragraph that opens `<mark> ` with no colon marks his own intent; it is
   kept as a hint for 象, not a verdict.

   Classical and pure: the candidates are the agent's turns since Joe's
   previous turn, newest first, as the JVM saw them end."
  (:require [clojure.string :as str]
            [futon3c.xiang.turn-record :as tr]))

(def ^:private marks (mapv first tr/proforma-marks))

(defn- paragraphs
  "Paragraphs of TEXT (runs of lines with no blank line inside), trimmed."
  [text]
  (->> (str/split (str text) #"\n\s*\n")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn- opening
  "[mark pointer?] for a paragraph opening with a proforma mark, else nil.
   pointer? is true when the mark is followed by a colon."
  [para]
  (some (fn [m]
          (when (str/starts-with? para m)
            [m (str/starts-with? (str/triml (subs para (count m))) ":")]))
        marks))

(defn- unquoted
  "PARA with backtick spans and double-quoted spans blanked out, so a mark
   Joe writes about (`🈸:` or \"🈸:\") is not read as one he uses."
  [para]
  (str/replace para #"`[^`]*`|\"[^\"]*\"|“[^”]*”" #(apply str (repeat (count %) \space))))

(defn- inline-pointers
  "Marks used in the colon form after the start of PARA, in order, e.g. the
   🈸 in \"OK, I've tried the hydra, and 🈸:yes\" (turn 392). A bare mark
   inside a sentence is not a pointer: it is often a quotation."
  [para]
  (let [s (unquoted para)]
    (->> marks
         (mapcat (fn [m]
                   (loop [from 0 acc []]
                     (let [at (str/index-of s m from)]
                       (if (nil? at)
                         acc
                         (let [after (subs s (+ at (count m)))]
                           (recur (+ at (count m))
                                  (if (and (pos? at) (str/starts-with? (str/triml after) ":"))
                                    (conj acc [at m])
                                    acc))))))))
         (sort-by first)
         (map second))))

(defn operator-marks
  "Joe's marked paragraphs: [{:index :mark :pointer? :text}]. A paragraph
   may also carry `<mark>:` pointers after its start; each is an entry with
   :pointer? true and :inline? true, and the paragraph's :index."
  [text]
  (vec (mapcat (fn [i para]
                 (concat
                  (when-let [[m pointer?] (opening para)]
                    [{:index i :mark m :pointer? pointer?
                      :text (str/triml (str/replace-first (subs para (count m)) #"^\s*:\s*" ""))}])
                  (for [m (inline-pointers para)]
                    {:index i :mark m :pointer? true :inline? true :text para})))
               (range) (paragraphs text))))

(defn- excerpt [s] (let [s (str/trim (str s))] (if (> (count s) 160) (str (subs s 0 160) "…") s)))

(defn resolve-targets
  "For each `<mark>:` paragraph in operator TEXT, the candidate paragraph it
   answers. CANDIDATES are the agent's turns since the operator's previous
   turn, newest first: [{:turn-id :origin :text}]. Rule :mark-match picks
   the newest candidate with a paragraph opening with the same mark (the
   last such paragraph in that turn); :newest falls back to the newest
   candidate when no paragraph carries the mark; :none when there are no
   candidates.

   Returns {:replies [{:index :mark :rule :turn-id :origin :paragraph
   :excerpt}] :declared-intents [{:index :mark}]}."
  [text candidates]
  (let [ms (operator-marks text)
        cands (vec candidates)]
    {:replies
     (vec (for [{:keys [index mark]} (filter :pointer? ms)]
            (if-let [[c p] (first (for [c cands
                                        :let [ps (filter #(= mark (:mark %)) (tr/reply-marks (:text c)))]
                                        :when (seq ps)]
                                    [c (last ps)]))]
              {:index index :mark mark :rule :mark-match
               :turn-id (:turn-id c) :origin (:origin c)
               :paragraph (excerpt (:text p))}
              (if-let [c (first cands)]
                {:index index :mark mark :rule :newest
                 :turn-id (:turn-id c) :origin (:origin c)}
                {:index index :mark mark :rule :none}))))
     :declared-intents (vec (for [{:keys [index mark]} (remove :pointer? ms)]
                              {:index index :mark mark}))}))
