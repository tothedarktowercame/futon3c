(ns futon3c.arxana.resolve.evaluator
  "D2 deterministic strategy evaluator (INSTANTIATE increment 4).

  Accepts bytes only from an ArtifactProvider by content-addressed revision
  and verifies the digest before evaluation (D2-DR3). Finder order is vector
  order and feeds the strategy revision digest. Ambiguity is preserved:
  ordering is evidence, never selection (D2-DR2). Unknown semantics versions
  or finder types are `unsupported`, never successful no-ops.

  The evaluator is digest-identified: `evaluator-version` is the canonical
  SHA-256 over the evaluator/semantics identity map, so a behaviour change is
  a new evaluator identity by construction."
  (:require [clojure.string :as str]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.resolve.anchor :as anchor])
  (:import [java.nio.charset StandardCharsets]))

;;; Evaluator identity -------------------------------------------------------------

(def semantics-version :arxana.resolution/v1)

(def evaluator-version
  "Digest-identified evaluator version (D2: assertions carry the
  evaluator/semantics version; replay compares digests)."
  (str "arxana:evaluator:sha256:"
       (digest/canonical-digest
        {:evaluator/name :arxana.resolve/evaluator
         :evaluator/increment 4
         :arxana/semantics semantics-version
         :cascade-modes [:first-unique :accumulate :all-evidence]
         :finder-types [:arxana.finder/anchor
                        :arxana.finder/symbol-as-term
                        :arxana.finder/filename-mention
                        :arxana.finder/embedding-proximity]})))

(def strategy-revision-prefix "arxana:strategy-revision:sha256:")

(defn strategy-revision-id
  "Content-addressed revision ID for an executable strategy value. Finder
  vector order is part of the digest (D2 grammar)."
  ^String [strategy]
  (str strategy-revision-prefix
       (digest/canonical-digest
        (dissoc strategy :arxana/strategy-revision-id))))

;;; Result construction -------------------------------------------------------------

(defn- result
  "A D2 result envelope value (a protocol value, not durable evidence until
  promoted; D2-DR5/O-D2d)."
  [request status candidates evidence diagnostics]
  {:request/id (:request/id request)
   :protocol/semantics-version semantics-version
   :evaluator/version evaluator-version
   :artifact/revision-id (:artifact/revision-id request)
   :strategy/revision-id (:strategy/revision-id request)
   :result/status status
   :result/candidates (vec candidates)
   :result/evidence (vec evidence)
   :result/evidence-digests
   (mapv (fn [ev]
           (digest/canonical-digest (dissoc ev :evidence/duration-ms)))
         evidence)
   :result/diagnostics (vec diagnostics)})

;;; Literal-occurrence helper (symbol-as-term / filename-mention) -------------------

(defn- literal-candidates
  "All codepoint occurrences of literal `needle` in `text`, retained in full
  (D2: results retain every matching definition/mention). Ordered per D2 by
  construction (ascending start) — a multiple set is ambiguity evidence."
  [artifact-revision-id finder-id text needle score-key]
  (let [^ints cps (.toArray (.codePoints ^String text))
        ^ints nps (.toArray (.codePoints ^String needle))
        n (alength cps)
        m (alength nps)]
    (if (zero? m)
      []
      (->> (range 0 (inc (- n m)))
           (filter (fn [i]
                     (loop [j 0]
                       (cond
                         (= j m) true
                         (not= (aget cps (+ i j)) (aget nps j)) false
                         :else (recur (inc j))))))
           (mapv (fn [i]
                   (let [matched (String. cps (int i) (int m))
                         md (digest/sha256-hex (.getBytes matched StandardCharsets/UTF_8))]
                     {:candidate/id (digest/canonical-digest
                                     {:artifact/revision-id artifact-revision-id
                                      :range {:start i :end (+ i m)}
                                      :matched-text-digest md})
                      :location {:artifact/revision-id artifact-revision-id
                                 :unit :unicode-codepoint
                                 :range {:start i :end (+ i m)}}
                      :verification :verified-hash
                      :score-components {score-key 1.0}
                      :matched-text-digest md
                      :finder/id finder-id})))))))

;;; Finder support validation --------------------------------------------------------

(defn- finder-support
  "Pre-run support check (D2: a strategy is executable only when every finder
  that can be reached is executable). Returns nil when supported, else
  {:status :unsupported|:invalid :detail ...}."
  [finder]
  (let [ftype (:arxana/type finder)
        config (:arxana/config finder)
        fid (:arxana/finder-id finder)]
    (case ftype
      :arxana.finder/anchor
      (when-not (:anchor/content-hash config)
        {:status :invalid
         :detail {:finder/id fid
                  :reason "anchor finder requires :anchor/content-hash config"}})

      :arxana.finder/symbol-as-term
      (when-not (and (seq (:patterns config)) (:language config)
                     (:normalization config) (:term config))
        {:status :invalid
         :detail {:finder/id fid
                  :reason "symbol-as-term requires pinned patterns, language, normalization and term"}})

      :arxana.finder/filename-mention
      (when-not (and (:repository-revision config) (:root config)
                     (:path-normalization config) (:mention-grammar config)
                     (:filename config))
        {:status :invalid
         :detail {:finder/id fid
                  :reason "filename-mention requires pinned repository revision/root, path normalization, mention grammar and filename"}})

      ;; Advisory-only by default; a pinned form is translatable, but no JVM
      ;; executor exists in increment 4 — honestly unsupported, never a
      ;; successful empty result (D2 table + status algebra).
      :arxana.finder/embedding-proximity
      {:status :unsupported
       :detail {:finder/id fid
                :finder/type ftype
                :reason "embedding-proximity has no v1 executor; advisory-only classification, executable form requires a pinned model runtime"}}

      ;; Unknown finder types stay visible and are unsupported, never dropped.
      {:status :unsupported
       :detail {:finder/id fid
                :finder/type ftype
                :reason "unknown finder type; never dropped, never a successful no-op"}})))

;;; Finder evaluation -------------------------------------------------------------

(defn- run-finder
  "Evaluate one (pre-validated) finder against decoded canonical text."
  [artifact-revision-id finder text {:keys [allow-unverified-fuzzy?]}]
  (let [finder-id (:arxana/finder-id finder)
        ftype (:arxana/type finder)
        config (:arxana/config finder)]
    (case ftype
      :arxana.finder/anchor
      (anchor/run artifact-revision-id finder-id text config
                  (boolean allow-unverified-fuzzy?))

      :arxana.finder/symbol-as-term
      (let [cands (vec (mapcat (fn [pat]
                                 (literal-candidates
                                  artifact-revision-id finder-id text
                                  (str/replace pat "<term>" (str (:term config)))
                                  :pattern))
                               (:patterns config)))]
        {:outcome (cond (empty? cands) :missing
                        (= 1 (count cands)) :resolved
                        :else :ambiguous)
         :stage :literal-pattern
         :candidates cands})

      :arxana.finder/filename-mention
      (let [cands (literal-candidates artifact-revision-id finder-id text
                                      (str (:filename config)) :mention)]
        {:outcome (cond (empty? cands) :missing
                        (= 1 (count cands)) :resolved
                        :else :ambiguous)
         :stage :literal-mention
         :candidates cands}))))

;;; Cascade ---------------------------------------------------------------------------

(defn- permits?
  "Finder's :on list (sorted vector on the wire; sets are forbidden in
  canonical payloads) gates continuation on the prior outcome (D2 grammar)."
  [finder prior-outcome]
  (contains? (set (:arxana/on finder)) prior-outcome))

(defn- evidence-event
  "Immutable evidence event for one attempted finder (D2). Ordered by
  evaluator sequence; duration is diagnostic and excluded from digests."
  [seq-no finder artifact-revision-id strategy-revision-id outcome candidate-ids duration-ms diagnostics]
  {:evidence/seq seq-no
   :finder/id (:arxana/finder-id finder)
   :finder/type (:arxana/type finder)
   :finder/config-digest (digest/canonical-digest (:arxana/config finder))
   :artifact/revision-id artifact-revision-id
   :strategy/revision-id strategy-revision-id
   :evidence/outcome outcome
   :evidence/candidate-ids (vec candidate-ids)
   :evidence/duration-ms duration-ms
   :evidence/diagnostics (vec diagnostics)})

(defn- dedupe-key [c]
  [(get-in c [:location :artifact/revision-id])
   (get-in c [:location :range :start])
   (get-in c [:location :range :end])
   (:matched-text-digest c)])

(defn- distinct-by-key [f coll]
  (let [seen (atom #{})]
    (filterv (fn [x]
               (let [k (f x)]
                 (if (contains? @seen k)
                   false
                   (do (swap! seen conj k) true))))
             coll)))

(defn- final-status
  "Derive the final status WITHOUT selecting among multiple viable candidates
  (D2-DR2, cascade algorithm)."
  [candidates]
  (let [viable (remove #(= :unverified-fuzzy (:verification %)) candidates)]
    (cond
      (= 1 (count viable)) :resolved
      (and (empty? viable) (= 1 (count candidates))) :resolved-unverified
      (empty? candidates) :missing
      :else :ambiguous)))

(defn- validate-strategy
  "Structural validation of an executable strategy value."
  [strategy]
  (cond
    (not (map? strategy))
    {:status :invalid :detail {:reason "strategy is not a map"}}

    (not= semantics-version (:arxana/semantics strategy))
    {:status :unsupported
     :detail {:reason "unknown semantics version"
              :received (:arxana/semantics strategy)
              :supported [semantics-version]}}

    (not (contains? #{:first-unique :accumulate :all-evidence}
                    (:arxana/cascade-mode strategy)))
    {:status :invalid
     :detail {:reason "no implicit fallback mode; strategy must name a supported cascade mode"
              :received (:arxana/cascade-mode strategy)}}

    (not (vector? (:arxana/finders strategy)))
    {:status :invalid :detail {:reason "finders must be a vector; order is semantic"}}

    :else nil))

(defn evaluate
  "Run the D2 cascade.

  deps: {:artifact-provider p :strategy-source (fn [strategy-revision-id]
         -> strategy map or nil)}
  request: {:request/id ... :artifact/revision-id ... :strategy/revision-id
            :subject/id ... :options {:max-candidates :allow-unverified-fuzzy?}}

  Returns a D2 result envelope value. `stale-input` is a successful evaluator
  result meaning the named bytes failed integrity verification; it is NOT the
  D3 mutation error `stale` (O-D3b). Preview locators are refused before any
  finder runs; unsupported finders abort with `unsupported` before any stage
  runs (a strategy is executable only when every reachable finder is)."
  [deps request]
  (let [{:keys [artifact-provider strategy-source]} deps
        arid (:artifact/revision-id request)
        srid (:strategy/revision-id request)
        max-cands (long (get-in request [:options :max-candidates] 100))]
    (cond
      ;; Preview locators are refused before any finder runs (D7 preview gate).
      (artifact/preview-id? arid)
      (result request :missing [] []
              [{:code :arxana.artifact/authority-revision-required
                :detail "client-preview segment hash is not an artifact revision identity; ingest first"}])

      (not (artifact/revision-id? arid))
      (result request :invalid [] []
              [{:code :arxana.artifact/malformed-revision-id
                :received arid}])

      :else
      (let [strategy (strategy-source srid)]
        (cond
          (nil? strategy)
          (result request :missing [] []
                  [{:code :arxana.resolve/strategy-absent
                    :strategy/revision-id srid}])

          :else
          (if-let [{:keys [status detail]} (validate-strategy strategy)]
            (result request status [] []
                    [(assoc detail :code :arxana.resolve/strategy-rejected)])
            (let [finders0 (:arxana/finders strategy)
                  finders (mapv (fn [idx f]
                                  (if (:arxana/finder-id f)
                                    f
                                    (assoc f :arxana/finder-id (str srid "#" idx))))
                                (range) finders0)
                  unsupported (some finder-support finders)]
              (if unsupported
                (result request (:status unsupported) [] []
                        [(assoc (:detail unsupported)
                                :code :arxana.resolve/finder-not-executable)])
                (let [{:keys [ok error]} (artifact/verify-bytes artifact-provider arid)]
                  (cond
                    ;; absent artifact revision
                    (and error (= :missing (:error/category error)))
                    (result request :missing [] []
                            [{:code :arxana.artifact/revision-not-found
                              :artifact/revision-id arid}])

                    ;; digest mismatch: stale-input, no finder runs
                    error
                    (result request :stale-input [] []
                            [{:code :arxana.artifact/integrity-mismatch
                              :artifact/revision-id arid}])

                    :else
                    (let [text (String. ^bytes (:bytes ok) StandardCharsets/UTF_8)
                          mode (:arxana/cascade-mode strategy)]
                      (loop [idx 0
                             prior :initial
                             candidates []
                             evidence []
                             diagnostics []
                             stop? false]
                        (if (or stop? (>= idx (count finders)))
                          (let [cands (if (= mode :all-evidence)
                                        candidates
                                        (distinct-by-key dedupe-key candidates))]
                            (result request (final-status cands)
                                    (vec (take max-cands cands))
                                    evidence diagnostics))
                          (let [finder (nth finders idx)]
                            (if-not (or (= :initial prior) (permits? finder prior))
                              (recur (inc idx) prior candidates evidence diagnostics stop?)
                              (let [t0 (System/nanoTime)
                                    ran (run-finder arid finder text (:options request))
                                    dur (/ (double (- (System/nanoTime) t0)) 1e6)
                                    {:keys [outcome stage]} ran
                                    new-cands (:candidates ran)
                                    ev (evidence-event idx finder arid srid
                                                       outcome
                                                       (mapv :candidate/id new-cands)
                                                       dur
                                                       [{:stage stage}])
                                    merged (into candidates new-cands)
                                    verified (remove #(= :unverified-fuzzy (:verification %))
                                                     merged)
                                    stop (and (= mode :first-unique)
                                              (= 1 (count verified)))]
                                (recur (inc idx) outcome merged
                                       (conj evidence ev) diagnostics
                                       stop)))))))))))))))))
