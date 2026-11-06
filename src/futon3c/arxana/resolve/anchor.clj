(ns futon3c.arxana.resolve.anchor
  "Anchor finder v1 (D2): the three-stage exact/context/fuzzy recovery
  cascade over immutable artifact bytes.

  Repairs over the legacy algorithm (D2-DR2):
  - every stage enumerates ALL candidates; a multiple verified set is
    `ambiguous`, never a silent first-match;
  - the fuzzy tier is explicitly `unverified-fuzzy` with similarity features,
    never reported as verified.

  Positions are Unicode codepoint half-open ranges [start,end) within the
  decoded canonical text (D2/D3 location basis). Byte offsets, UTF-16 units
  and line/column projections are client concerns (D5-DR1)."
  (:require [futon3c.arxana.digest :as digest])
  (:import [java.nio.charset StandardCharsets]))

;;; Codepoint text substrate ----------------------------------------------------

(defn- codepoints
  "int[] of Unicode codepoints for string s."
  ^ints [^String s]
  (.toArray (.codePoints s)))

(defn- slice-string
  "The codepoint range [start,end) of cp-array as a String."
  ^String [^ints cps ^long start ^long end]
  (String. cps (int start) (int (- end start))))

(defn- text-digest
  "SHA-256 hex of the UTF-8 bytes of s — the content-hash basis."
  ^String [^String s]
  (digest/sha256-hex (.getBytes s StandardCharsets/UTF_8)))

;;; Candidate construction --------------------------------------------------------

(def ^:private verification-rank
  "Ordering class: verified before unverified. Ordering never turns a tied
  or multiple set into uniqueness (D2)."
  {:verified-exact 0
   :verified-hash 1
   :unverified-fuzzy 2})

(defn- candidate
  [artifact-revision-id finder-id ^ints cps start end verification score-components]
  (let [matched-text (slice-string cps start end)
        md (text-digest matched-text)]
    {:candidate/id (digest/canonical-digest
                    {:artifact/revision-id artifact-revision-id
                     :range {:start start :end end}
                     :matched-text-digest md})
     :location {:artifact/revision-id artifact-revision-id
                :unit :unicode-codepoint
                :range {:start start :end end}}
     :verification verification
     :score-components score-components
     :matched-text-digest md
     :finder/id finder-id}))

(defn- order-candidates
  "Stable non-semantic ordering (D2): verification class, descending declared
  score, start codepoint, end codepoint, then candidate digest."
  [cands]
  (sort-by (fn [c]
             [(verification-rank (:verification c) 9)
              (- (double (reduce + 0.0 (vals (:score-components c)))))
              (get-in c [:location :range :start])
              (get-in c [:location :range :end])
              (:candidate/id c)])
           cands))

;;; Stage 1: hint/hash ---------------------------------------------------------

(defn- stage-hint
  "If the hinted range is in bounds and its canonical text hash matches, emit
  one verified-exact candidate; else none."
  [^ints cps anchor]
  (let [off (get-in anchor [:anchor/hint :offset])
        len (:anchor/content-length anchor)
        n (alength cps)]
    (if (and (integer? off) (integer? len)
             (<= 0 off) (<= (+ off len) n)
             (= (:anchor/content-hash anchor)
                (text-digest (slice-string cps off (+ off len)))))
      [{:start off :end (+ off len)
        :verification :verified-exact
        :score-components {:hint 1.0}}]
      [])))

;;; Stage 2: context/hash ------------------------------------------------------

(defn- region-matches?
  "True when cps[start,end) equals the codepoints of s."
  [^ints cps ^long start ^long end ^String s]
  (let [^ints want (codepoints s)
        wlen (alength want)]
    (and (= wlen (- end start))
         (<= 0 start) (<= end (alength cps))
         (loop [i 0]
           (cond
             (= i wlen) true
             (not= (aget want i) (aget cps (+ start i))) false
             :else (recur (inc i)))))))

(defn- stage-context
  "Enumerate ALL windows of content-length compatible with the before/after
  context; retain every hash-verifying candidate. A multiple result is
  ambiguity evidence, not a first-match (D2-DR2)."
  [^ints cps anchor]
  (let [len (:anchor/content-length anchor)
        before (or (:anchor/context-before anchor) "")
        after (or (:anchor/context-after anchor) "")
        blen (count (codepoints before))
        alen (count (codepoints after))
        n (alength cps)
        target (:anchor/content-hash anchor)]
    (if-not (and (integer? len) (pos? len))
      []
      (->> (range blen (inc (- n len alen)))
           (filter (fn [i]
                     (and (region-matches? cps (- i blen) i before)
                          (region-matches? cps (+ i len) (+ i len alen) after))))
           (filter (fn [i]
                     (= target (text-digest (slice-string cps i (+ i len))))))
           (mapv (fn [i]
                   {:start i :end (+ i len)
                    :verification :verified-hash
                    :score-components {:context 1.0}}))))))

;;; Stage 3: fuzzy context ------------------------------------------------------

(defn- common-prefix-len
  "Number of leading codepoints of s shared with cps[start,...)."
  ^long [^ints cps ^long start ^String s]
  (let [^ints want (codepoints s)
        wlen (alength want)
        n (alength cps)]
    (loop [i 0]
      (if (or (= i wlen) (>= (+ start i) n)
              (not= (aget want i) (aget cps (+ start i))))
        i
        (recur (inc i))))))

(defn- common-suffix-len
  "Number of trailing codepoints of s shared with cps[...,end)."
  ^long [^ints cps ^long end ^String s]
  (let [^ints want (codepoints s)
        wlen (alength want)]
    (loop [i 0]
      (if (or (= i wlen) (neg? (- end 1 i))
              (not= (aget want (- wlen 1 i)) (aget cps (- end 1 i))))
        i
        (recur (inc i))))))

(defn- stage-fuzzy
  "Bounded fuzzy context recovery. Windows whose length is within the length
  tolerance and whose before/after context similarity clears the configured
  distance threshold emit `unverified-fuzzy` candidates carrying similarity
  features. Never verified (D2 stage 3, D2-DR2)."
  [^ints cps anchor]
  (let [fuzzy (:anchor/fuzzy anchor)
        len (:anchor/content-length anchor)
        before (or (:anchor/context-before anchor) "")
        after (or (:anchor/context-after anchor) "")
        blen (count (codepoints before))
        alen (count (codepoints after))
        n (alength cps)
        length-tol (long (or (:length-tolerance fuzzy) 2))
        min-score (double (or (:min-context-score fuzzy) 0.5))]
    (if-not (and (integer? len) (pos? len))
      []
      (->> (for [l (range (max 1 (- len length-tol)) (inc (+ len length-tol)))
                 i (range 0 (inc (- n l)))
                 :let [bscore (if (zero? blen) 1.0
                                  (/ (double (common-suffix-len cps i before))
                                     (double blen)))
                       ascore (if (zero? alen) 1.0
                                  (/ (double (common-prefix-len cps (+ i l) after))
                                     (double alen)))
                       ctx (/ (+ bscore ascore) 2.0)]
                 :when (and (<= blen i)
                            (>= ctx min-score)
                            ;; fuzzy must not re-emit a hash-verifying span
                            (not= (:anchor/content-hash anchor)
                                  (text-digest (slice-string cps i (+ i l)))))]
             {:start i :end (+ i l)
              :verification :unverified-fuzzy
              :score-components {:context ctx
                                 :length-similarity
                                 (- 1.0 (/ (double (Math/abs ^long (- l len)))
                                           (double (max 1 len))))}})
           (distinct)
           (vec)))))

;;; Anchor finder entry point ----------------------------------------------------

(defn run
  "Run the three-stage anchor cascade for `anchor` against decoded canonical
  `text`. Returns {:outcome kw :stage kw :candidates [...]} where
  :candidates are realized candidate maps (ordered per D2) and :outcome is
  one of :resolved :ambiguous :missing :resolved-unverified.

  `fuzzy?` enables stage 3 (caller supplies the option; D2 default off)."
  [artifact-revision-id finder-id ^String text anchor fuzzy?]
  (let [cps (codepoints text)realize (fn [{:keys [start end verification score-components]}]
                  (candidate artifact-revision-id finder-id cps start end
                             verification score-components))
        hint (stage-hint cps anchor)]
    (cond
      (seq hint)
      {:outcome :resolved :stage :hint/hash
       :candidates (order-candidates (mapv realize hint))}

      :else
      (let [ctx (stage-context cps anchor)]
        (cond
          (seq ctx)
          {:outcome (if (= 1 (count ctx)) :resolved :ambiguous)
           :stage :context/hash
           :candidates (order-candidates (mapv realize ctx))}

          (not fuzzy?)
          {:outcome :missing :stage :context/hash :candidates []}

          :else
          (let [fz (stage-fuzzy cps anchor)]
            (cond
              (empty? fz) {:outcome :missing :stage :fuzzy-context :candidates []}
              (= 1 (count fz)) {:outcome :resolved-unverified
                                :stage :fuzzy-context
                                :candidates (order-candidates (mapv realize fz))}
              :else {:outcome :ambiguous
                     :stage :fuzzy-context
                     :candidates (order-candidates (mapv realize fz))})))))))
