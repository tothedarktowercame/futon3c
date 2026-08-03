(ns futon3c.dispatch-with-recall
  "Ground-control dispatch with bounded, pattern-conditioned memory recall.

  Recall is deliberately best-effort: an empty, busy, failed, or timed-out
  store never prevents the Agency bell. The offered half of the canonical
  memory-use receipt is persisted only after Agency returns a job id."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [futon2.aif.memory-contract :as memory-contract]
            [futon3c.peripheral.memory-recall :as memory-recall]
            [futon3c.substrate.client :as substrate])
  (:import [java.net URLEncoder]
           [java.time Instant]
           [java.util UUID]))

(def default-limit 5)
(def default-query-term-limit 4)
(def default-memory-channel :push)
(def memory-pull-invitation-version "memory-pull-invitation-v1")

(def ^:private memory-channels
  #{:push :push+pull :pull-only :none})

(def memory-pull-invitation
  (str
   "MID-SESSION MEMORY RETRIEVAL\n"
   "[memory-pull-invitation-version=" memory-pull-invitation-version "]\n"
   "The memory store is searchable during this run with memory_search, "
   "pattern_memory, evidence_graph, library_search, psr_search, and "
   "memory_record. Search when you lack a lemma or an approach, including "
   "after work has begun, rather than treating dispatch-time recall as the "
   "only retrieval point. In the final Memory usage section, give exactly "
   "one USED or IGNORED line for every memory id surfaced by these operations."))

;; RAISED 30000 -> 240000 on 2026-07-31 by claude-9, Joe-authorised.
;;
;; WHY. At 30s this budget was SMALLER THAN THE WORK IT HAD TO DO, so recall
;; could not complete for most rows and metric-3 was systematically
;; under-measured. Measured on 2026-07-31 (receipt
;; 4f7eeadd-3072-41ab-94f2-effe22e62ae0):
;;
;;   * `/api/alpha/evidence/text-search` costs 9.1-16.2s per query on a HEALTHY
;;     store (mean 14.2s), and recall issues ONE PER SUBJECT.
;;   * `subjects_for` yields up to 12 subjects, so a worst-case row needs
;;     ~170-195s of text-search alone, plus ~2.3s for receipt-entries.
;;   * Cost tracks how COMMON a term is, not how many subjects there are:
;;     construction-deriv timed out on 6 subjects (deriv/zero/isopen/
;;     differentiableon) while a97A08 completed on 6 (count/outer/roots/...).
;;
;; 3 of the last 4 dispatches before this change recorded
;; `[dispatch-recall-outcome=timeout]`, and across the whole corpus 46 of the
;; 129 offered halves timed out -- ~45% of dispatches losing recall to
;; infrastructure rather than to a genuine absence of memories.
;;
;; WHY NOT 120000, which was the value first proposed: it clears only ~8 common
;; subjects (8 * 14.2 = 114s) and still fails 12-subject rows at ~171s. The
;; failures it would leave are exactly the analysis-heavy rows the bias already
;; falls hardest on, so it would look like a fix while preserving the skew.
;; 240000 covers the measured worst case with roughly 20% headroom.
;;
;; THIS IS A MITIGATION, NOT THE FIX. The real cost is that text-search returns
;; 200-460KB per query. The structural repairs are to rank/cap `subjects_for` by
;; term rarity (common terms cost the most and discriminate least) and to index
;; or paginate the endpoint. Both are out of scope for a budget change.
;;
;; NOTE the interaction with `per-call-timeout-ms` below: it returns the WHOLE
;; budget for a single call, so one hung substrate call can still consume all of
;; it. Raising this constant raises that exposure from 30s to 240s.
(def default-recall-timeout-ms 240000)
(def default-agency-base "http://localhost:7070")
(def default-mission "M-zai-learning-loop")
(def default-problem-root "/home/joe/code/apm-lean/problems")
(def recall-system :v1.2-receipt-instrumented)
(def receipt-ranked-system :v1.2-receipt-ranked-instrumented)
(def default-receipt-alpha 0.5)
(def default-receipt-query-limit 200)
(def default-receipt-stats-timeout-ms 5000)
(def receipt-author "ground-control")

(def ^:private stopwords
  #{"about" "after" "again" "against" "alist" "also" "apm" "attempt"
    "been" "before" "begin" "being" "between" "build" "bundle" "canonical"
    "corpus" "could" "currently" "directory" "does" "each" "end" "every"
    "exists" "extract" "filename" "follow-up" "following" "forall"
    "formalization" "formalization-oriented" "from" "harvested"
    "harvested-filename" "have" "imported" "imported-at" "informal"
    "informal-proof" "informal-solution" "into" "item" "lean" "mathlib"
    "needed" "oriented" "outline" "over" "overview" "problem" "proof"
    "prove" "rightarrow" "root" "runner" "should" "solution" "source"
    "statement" "that" "their" "then" "there" "these" "this" "through"
    "using" "validate" "where" "which" "with" "would"
    ;; v1.3 additions (S4 receipt e-e36e37bd evidence): TeX fragments that
    ;; survive stripping in already-tokenized text + packet boilerplate.
    "cdot" "langle" "rangle" "denote" "select" "sorry" "commit" "your"
    "they" "part"})

(defn- encode [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- trim-base [base]
  (str/replace base #"/+$" ""))

(defn- positive-int [label value]
  (try
    (let [parsed (Long/parseLong value)]
      (when-not (pos? parsed)
        (throw (ex-info (str label " must be positive") {:value value})))
      parsed)
    (catch NumberFormatException _
      (throw (ex-info (str label " must be an integer") {:value value})))))

(defn- bounded-double [label value lower upper]
  (try
    (let [parsed (Double/parseDouble value)]
      (when-not (and (Double/isFinite parsed)
                     (<= lower parsed upper))
        (throw (ex-info (str label " must be between " lower " and " upper)
                        {:value value})))
      parsed)
    (catch NumberFormatException _
      (throw (ex-info (str label " must be a number") {:value value})))))

(defn- parse-memory-channel [value]
  (let [channel (keyword (str/replace value #"^:" ""))]
    (when-not (contains? memory-channels channel)
      (throw (ex-info
              "--memory-channel must be one of :push, :push+pull, :pull-only, :none"
              {:value value})))
    channel))

(defn parse-args
  "Parse the deliberately small CLI without adding another dependency."
  [args]
  (loop [remaining args
         opts {:limit default-limit
               :recall-timeout-ms default-recall-timeout-ms
               :memory-channel default-memory-channel
               :base default-agency-base
               :mission default-mission
               :from "ground-control"
               :receipt-ranking? true
               :receipt-alpha default-receipt-alpha
               :withhold-ids []
               :subjects []}]
    (if-let [arg (first remaining)]
      (cond
        (= "--dry-run" arg)
        (recur (next remaining) (assoc opts :dry-run? true))

        (= "--help" arg)
        (recur (next remaining) (assoc opts :help? true))

        (= "--no-receipt-ranking" arg)
        (recur (next remaining) (assoc opts :receipt-ranking? false))

        (= "--allow-thin" arg)
        (recur (next remaining) (assoc opts :allow-thin? true))

        (contains? #{"--problem" "--to" "--from" "--base" "--mission"
                     "--subject" "--terrain" "--substrate-base" "--limit"
                     "--recall-timeout-ms" "--receipt-alpha" "--withhold"
                     "--memory-channel"}
                   arg)
        (let [value (second remaining)]
          (when (or (nil? value) (str/starts-with? value "--"))
            (throw (ex-info (str arg " requires a value") {:argument arg})))
          (recur
           (nnext remaining)
           (case arg
             "--problem" (assoc opts :problem value)
             "--to" (assoc opts :to value)
             "--from" (assoc opts :from value)
             "--base" (assoc opts :base value)
             "--mission" (assoc opts :mission value)
             "--subject" (update opts :subjects conj value)
             "--withhold" (update opts :withhold-ids conj value)
             "--memory-channel"
             (assoc opts :memory-channel (parse-memory-channel value))
             "--terrain" (assoc opts :terrain value)
             "--substrate-base" (assoc opts :substrate-base value)
             "--limit" (assoc opts :limit (positive-int arg value))
             "--receipt-alpha"
             (assoc opts :receipt-alpha
                    (bounded-double arg value 0.0 1.0))
             "--recall-timeout-ms"
             (assoc opts :recall-timeout-ms (positive-int arg value)))))

        :else
        (throw (ex-info (str "unknown argument: " arg) {:argument arg})))
      opts)))

(defn usage []
  (str
   "Usage: scripts/dispatch_with_recall.clj --problem ID --to AGENT [options]\n"
   "       < problem-packet.txt\n\n"
   "Options:\n"
   "  --limit N                 surfaced memories (default 5)\n"
   "  --subject TEXT            additional recall subject; repeatable\n"
   "  --withhold MEMORY-ID      omit a surfaced memory; repeatable (ablation)\n"
   "  --terrain TEXT            explicit terrain override\n"
   "  --recall-timeout-ms N     total recall budget (default 240000)\n"
   "  --receipt-alpha N         use-rate boost weight, 0..1 (default 0.5)\n"
   "  --no-receipt-ranking      disable use-receipt ranking\n"
   "  --memory-channel MODE     :push (default), :push+pull, :pull-only, or :none\n"
   "  --substrate-base URL      authoritative substrate override\n"
   "  --base URL                Agency base (default http://localhost:7070)\n"
   "  --from ID                 dispatch/receipt author (default ground-control)\n"
   "  --mission ID              mission clock lineage\n"
   "  --dry-run                 print packet and receipt; no bell or write\n"))

(defn- bpm-source-id [problem]
  (when-let [[_ chapter section number]
             (re-matches #"bpm-(\d+)-(\d+)-(\d+)" problem)]
    (str chapter "." section "." number)))

(defn read-bpm-terrains
  "Read the preregistered terrain vocabulary from the starter README."
  [readme]
  (if-not (.isFile (java.io.File. readme))
    {}
    (->> (str/split-lines (slurp readme))
         (keep
          (fn [line]
            (let [columns (mapv str/trim (str/split line #"\|"))]
              (when (and (<= 7 (count columns))
                         (re-matches #"\d+\.\d+\.\d+" (get columns 2 "")))
                [(get columns 2) (get columns 6)]))))
         (into {}))))

(defn- default-terrain-readme []
  (str (System/getProperty "user.dir")
       "/holes/labs/M-zai-learning-loop/bpm-starter/README.md"))

(defn- normalize-math-text
  "TeX-encoded mathematics defeats lexical extraction (meta-draft
  expand-weak-convergence-trigger-vocabulary, 2026-07-27): L^2 / L² read as
  bare letters, convergence arrows vanish. Normalize BEFORE tokenizing."
  [text]
  (-> text
      (str/replace #"L\^?[\u00b22]" "L2 ")
      (str/replace #"L\^?[p1\u00b9]" "Lp ")
      (str/replace #"\\to|\\rightarrow|\u2192|\\longrightarrow" " convergence ")
      (str/replace #"\\infty" " infinity ")
      (str/replace #"\\int" " integral ")
      ;; v1.3 (meta-draft, 1342dee): strip remaining TeX command fragments
      ;; so cdot/langle/rangle etc never become query tokens.
      (str/replace #"\\[a-zA-Z]+" " ")))

(defn- text-keywords [text limit]
  (->> (re-seq #"[A-Za-z][A-Za-z0-9_/-]{3,}" (str/lower-case (normalize-math-text text)))
       (remove stopwords)
       frequencies
       (sort-by (fn [[word count]] [(- count) word]))
       (map first)
       (take limit)
       vec))

(defn query-keywords
  "Return the shipped normalized, stopword-filtered frequency vocabulary.

  This public analysis seam lets frozen-data experiments construct explicit
  query vocabularies without copying the production tokenizer."
  [text limit]
  (when-not (and (integer? limit) (pos? limit))
    (throw (ex-info "query keyword limit must be a positive integer"
                    {:limit limit})))
  (text-keywords text limit))

(defn- problem-statement-text [text]
  (let [marker "## Problem Statement"
        index (str/index-of text marker)]
    (if index (subs text index) text)))

(defn- standard-packet-math-text
  "Retain mathematical fields from the codex-sorry packet, excluding its
  operator preamble and binding/search instructions."
  [packet]
  (when (str/includes? packet "CODEX SORRY LOOP")
    (->> [["Target statement(s):" "Downstream unblocks:"]
          ["Available proved support:" "Suggested route (if any):"]
          ["Suggested route (if any):" "Binding rules:"]]
         (keep
          (fn [[start-marker end-marker]]
            (let [start (str/index-of packet start-marker)
                  end (when start
                        (str/index-of packet end-marker
                                      (+ start (count start-marker))))]
              (when (and start end)
                (subs packet (+ start (count start-marker)) end)))))
         (str/join "\n"))))

(defn- readable-file [path]
  (let [file (java.io.File. path)]
    (when (.isFile file) (slurp file))))

(defn- problem-term-sources
  [{:keys [problem problem-root]} packet]
  (let [root (or problem-root default-problem-root)
        bundle (str (trim-base root) "/" problem)
        problem-path (str bundle "/problem.md")
        outline-path (str bundle "/proof-outline.md")
        problem-text (some-> (readable-file problem-path)
                             problem-statement-text)
        outline-text (readable-file outline-path)]
    (cond-> []
      problem-text
      (conj {:source :problem-md
             :path problem-path
             :terms (text-keywords problem-text 14)})

      outline-text
      (conj {:source :proof-outline-md
             :path outline-path
             :terms (text-keywords outline-text 10)})

      true
      (conj {:source :stdin-packet
             :scope (if (str/includes? packet "CODEX SORRY LOOP")
                      :mathematical-fields
                      :whole-packet-fallback)
             :terms
             (text-keywords
              (or (not-empty (standard-packet-math-text packet))
                  (problem-statement-text packet))
              8)}))))

(defn- query-ladder
  "Queries in DECREASING conjunctive strictness, for a conjunctive backend.

  MEASURED 2026-07-30 across five live rows: the 3-term query returned ZERO
  memories for ALL FIVE, while falling back to 2-term pairs and then singles
  surfaced a memory for THREE of them. The 3-term cap was itself a fix earlier
  the same day (from 36 terms) and it was not enough, because term SELECTION is
  by statement order rather than by signal: a01A04's third term is `recursion`,
  a rare word that floors any conjunction containing it.

  Ordered strictest-first so precision is preferred and breadth is only reached
  when precision returns nothing. Singles are last and deliberately included:
  offering a marginally relevant memory costs little, because the runner reports
  whether it USED one, and that report is the measurement we actually want."
  [terms]
  (let [t (vec (take 3 (remove str/blank? terms)))]
    (->> (concat (when (seq t) [{:tier :triple :q (str/join " " t)}])
                 (for [[i j] [[0 1] [0 2] [1 2]]
                       :when (and (< i (count t)) (< j (count t)))]
                   {:tier :pair :q (str/join " " [(t i) (t j)])})
                 (for [x t] {:tier :single :q x}))
         (reduce (fn [acc m] (if (some #(= (:q %) (:q m)) acc) acc (conj acc m))) [])
         )))

(defn recall-query
  "Build a bounded lexical query from subject ids, preregistered terrain, and
  problem files, with packet terms retained as fallback. The exact problem id
  is also queried as a graph endpoint.

  `:query-term-limit` parameterises the shipped four-term cap for frozen-data
  analysis while preserving four as the production default. `:query-terms` is
  an analysis seam for an explicitly constructed vocabulary; it still passes
  through the same ladder, search, reviewed-attachment projection, and ranking
  path as the shipped query."
  [{:keys [problem subjects terrain query-term-limit query-terms]
    :or {query-term-limit default-query-term-limit}
    :as opts}
   packet terrain-map]
  (when-not (and (integer? query-term-limit) (pos? query-term-limit))
    (throw (ex-info "query-term-limit must be a positive integer"
                    {:query-term-limit query-term-limit})))
  (let [terrain (or terrain (some-> problem bpm-source-id terrain-map))
        term-sources
        (mapv
         (fn [source]
           (update source :terms
                   #(vec (remove #{(str/lower-case problem)} %))))
         (problem-term-sources opts packet))
        source-terms (mapcat :terms term-sources)
        ;; MEASURED 2026-07-30, not guessed. The text-search endpoint is
        ;; CONJUNCTIVE: hits fall off a cliff as terms are added — 1 term = 5
        ;; hits, 3 = 3, 7 = 2, 12 = 1, 29 = 0 — so a 36-term query returned
        ;; NOTHING, every time, and every "recall empty" datum in this lane was
        ;; this bug rather than a corpus or semantics finding.
        ;; The problem id is the worst offender: it appears in almost no
        ;; document, so including it in a conjunction floors the result set. It
        ;; is already queried separately as a GRAPH endpoint (see :endpoints
        ;; below), so it does not belong in the lexical query at all.
        ;; With id dropped and a 3-term cap, a01A04 went 0 hits -> 8 hits
        ;; including 2 directly relevant memories.
        ;; FOLLOW-UP: several short queries unioned would beat one short query;
        ;; this is the minimal measured fix, not the best possible one.
        ;; INTERLEAVE the two vocabularies instead of concatenating them.
        ;;
        ;; MEASURED 2026-07-30 on a01A07. `subjects` are extracted from the
        ;; STATEMENT and are largely identifier names — norm, area, integral,
        ;; differentiableon, closedball. `source-terms` come from the problem
        ;; FILES and carry the MATHEMATICS — disk (x20), mean value, Weierstrass,
        ;; polar, Fubini, Cauchy estimate, circleAverage. Memories are written in
        ;; the second vocabulary, not the first: the query "norm area integral"
        ;; returns 0 memories while "circleAverage" returns exactly the memory
        ;; the runner then found by hand.
        ;;
        ;; Concatenating subjects-then-source-terms and truncating to 3 meant
        ;; source-terms NEVER survived the cap — a regression introduced by the
        ;; 3-term cap itself, which was added earlier the same day to fix a
        ;; 36-term conjunction. Interleaving keeps both vocabularies inside the
        ;; cap so the ladder can pair across them.
        interleave-all
        (fn [a b]
          (loop [a (seq a) b (seq b) out []]
            (cond (and (nil? a) (nil? b)) out
                  (nil? a) (into out b)
                  (nil? b) (into out a)
                  :else (recur (next a) (next b)
                               (conj out (first a) (first b))))))
        generated-terms (concat (when terrain [terrain])
                                (interleave-all subjects source-terms))
        selected-terms (if (seq query-terms)
                         (map (comp str/trim str) query-terms)
                         generated-terms)
        terms (->> selected-terms
                   (remove str/blank?)
                   distinct
                   (take query-term-limit)
                   vec)]
    {:terrain terrain
     :recall-system recall-system
     :term-sources (if (seq query-terms)
                     [{:source :explicit-analysis-terms :terms terms}]
                     term-sources)
     :terms terms
     :query (str/join " " terms)}))

(defn- request-edn
  [method url opts]
  (let [response ((case method :get http/get :post http/post)
                  url (merge {:headers {"Accept" "application/edn"}
                              :throw false}
                             opts))
        body (try
               (edn/read-string (:body response))
               (catch Throwable _ (:body response)))]
    (if (<= 200 (long (:status response)) 299)
      body
      (throw (ex-info "substrate request failed"
                      {:url url :status (:status response) :body body})))))

(defn- substrate-seams
  [base timeout-ms]
  (let [base (trim-base base)
        common {:timeout timeout-ms}]
    {:search
     ;; REVERTED 2026-07-30. I added an over-fetch-and-filter here to stop the
     ;; loop's own receipts crowding memories out of the seed results. It was
     ;; redundant AND destructive:
     ;;   - memory-recall/proposal-search-rows ALREADY filters exactly this way,
     ;;     retaining :evidence/type :memory OR typed pattern-description rows;
     ;;   - my filter kept ONLY :memory, stripping the pattern-description rows
     ;;     the proposer needs to map a query to a pattern;
     ;;   - and it returned a VECTOR where the caller does (:results result),
     ;;     so proposal-search-rows saw nil and produced zero candidates.
     ;; Net effect: from the moment it landed, propose-patterns-by-query returned
     ;; 0 candidates for EVERY query at EVERY ladder tier. Found by probing the
     ;; proposal step directly instead of the endpoint or the seam.
     (fn [query {:keys [limit trace-id]}]
       (request-edn
        :get
        (str base "/api/alpha/evidence/text-search?q=" (encode query)
             "&limit=" (long limit))
        (assoc common :headers
               (cond-> {"Accept" "application/edn"}
                 trace-id (assoc "X-Trace-Id" trace-id)))))
     :projection
     (fn [endpoints {:keys [limit trace-id valid-as-of system-as-of]}]
       (request-edn
        :post
        (str base "/api/alpha/memory/projection")
        (assoc common
               :headers (cond-> {"Accept" "application/edn"
                                 "Content-Type" "application/edn"}
                          trace-id (assoc "X-Trace-Id" trace-id))
               :body
               (pr-str
                (cond-> {:endpoints (vec endpoints) :limit (long limit)}
                  valid-as-of (assoc :valid-as-of valid-as-of)
                  system-as-of (assoc :system-as-of system-as-of))))))
     :entry
     (fn [memory-id]
       (let [response (request-edn
                       :get
                       (str base "/api/alpha/evidence/" (encode memory-id))
                       common)]
         (or (:entry response) response)))}))

(defn- proposal-rank [candidate]
  (let [scores (keep :fts-score (:memory-support candidate))]
    (if (seq scores) (apply min scores) Double/POSITIVE_INFINITY)))

(defn- per-call-timeout-ms
  "Let the outer bounded-recall deadline own the total budget.  Giving an
  individual substrate call a shorter deadline can misclassify a still-bounded
  recall as store-unavailable before the outer deadline expires."
  [recall-timeout-ms]
  (max 250 recall-timeout-ms))

(defn proposal-hit?
  "True when a lexical tier produced either arm that dispatch can surface."
  [proposal]
  (boolean
   (or (seq (:candidates proposal))
       (seq (:content-matches proposal)))))

(defn- receipt-entries
  [base timeout-ms]
  (let [response
        (request-edn
         :get
         (str (trim-base base)
              "/api/alpha/evidence?type=pattern-outcome&author="
              (encode receipt-author)
              "&limit=" default-receipt-query-limit)
         {:timeout timeout-ms})]
    (vec (or (:entries response) response []))))

(defn- memory-patterns
  [memory]
  (->> (concat (:memory/pattern-ids memory)
               (when (str/starts-with?
                      (or (:dispatch/endpoint memory) "")
                      "math/")
                 [(:dispatch/endpoint memory)]))
       (filter string?)
       distinct
       vec))

(defn- empty-use-stat []
  {:offered-count 0
   :used-count 0
   :outcome-count 0
   :outcome-quality {}})

(defn- bump-use-stat
  [stat phase classification]
  (cond-> (or stat (empty-use-stat))
    (= :offered phase)
    (update :offered-count inc)

    (= :used phase)
    (update :used-count inc)

    (= :used phase)
    (update :outcome-count inc)

    (and (= :used phase) classification)
    (update-in [:outcome-quality classification] (fnil inc 0))))

(defn aggregate-use-receipts
  "Aggregate offered/outcome halves for only the current candidate batch.
  Pattern counts are per receipt, not the sum of member-memory counts."
  [entries memories]
  (let [candidate-ids (set (map :memory/id memories))
        patterns-by-memory
        (into {} (map (juxt :memory/id memory-patterns) memories))]
    (reduce
     (fn [stats entry]
       (let [body (:evidence/body entry)
             receipt (:memory-use body)]
         (if (and (= :memory-use (:event body))
                  (map? receipt))
           (let [phase (:phase body)
                 classification (get-in body [:outcome :classification])
                 memory-ids
                 (set
                  (filter
                   candidate-ids
                   (case phase
                     :offered (:memory-use/surfaced-ids receipt)
                     :outcome (:memory-use/used-ids receipt)
                     [])))
                 stat-phase (case phase :offered :offered :outcome :used nil)
                 pattern-ids
                 (set (mapcat patterns-by-memory memory-ids))]
             (if stat-phase
               (-> stats
                   (update :memories
                           (fn [memory-stats]
                             (reduce
                              #(update %1 %2 bump-use-stat
                                       stat-phase classification)
                              memory-stats memory-ids)))
                   (update :patterns
                           (fn [pattern-stats]
                             (reduce
                              #(update %1 %2 bump-use-stat
                                       stat-phase classification)
                              pattern-stats pattern-ids))))
               stats))
           stats)))
     {:memories {} :patterns {}}
     entries)))

(defn- scored-use-stat
  [stat alpha]
  (let [{:keys [offered-count used-count]}
        (merge (empty-use-stat) stat)
        use-rate (if (pos? offered-count)
                   (/ (double used-count) offered-count)
                   0.0)]
    (assoc (merge (empty-use-stat) stat)
           :use-rate use-rate
           :ranking-factor
           (if (pos? offered-count)
             (+ 1.0 (* alpha use-rate))
             1.0))))

(defn rank-memories
  "Apply a bounded receipt boost to the existing deterministic order.
  The base score decays gently with the pre-receipt rank; cold memories keep
  exactly the neutral factor 1.0."
  [memories memory-stats alpha]
  (->> memories
       (map-indexed
        (fn [index memory]
          (let [stat (scored-use-stat
                      (get memory-stats (:memory/id memory))
                      alpha)
                base-score (/ 1.0 (+ 1.0 (* 0.05 index)))]
            (assoc memory
                   :dispatch/pre-receipt-rank (inc index)
                   :dispatch/base-score base-score
                   :dispatch/receipt-stats stat
                   :dispatch/ranking-score
                   (* base-score (:ranking-factor stat))))))
       (sort-by (juxt (comp - :dispatch/ranking-score)
                      :dispatch/pre-receipt-rank
                      :memory/id))
       vec))

(defn pre-cutoff-ranking-audit
  "Describe the complete ranked candidate vector before the surfaced-memory
  cutoff. This is opt-in analysis instrumentation; it does not alter ranking or
  the production recall result unless `recall-now` is explicitly asked to
  include it. Positions and cutoff are one-based."
  [ranked cutoff]
  (mapv
   (fn [index memory]
     (let [receipt-ranked? (contains? memory :dispatch/ranking-score)
           base-score (or (:dispatch/base-score memory)
                          (/ 1.0 (+ 1.0 (* 0.05 index))))]
       (cond->
        {:memory-id (:memory/id memory)
         :position (inc index)
         :score (if receipt-ranked?
                  (:dispatch/ranking-score memory)
                  base-score)
         :score-kind (if receipt-ranked?
                       :receipt-ranked
                       :deterministic-base-order)
         :cutoff-position cutoff
         :within-cutoff? (< index cutoff)}
         (contains? memory :dispatch/pre-receipt-rank)
         (assoc :pre-receipt-rank (:dispatch/pre-receipt-rank memory))

         (contains? memory :dispatch/base-score)
         (assoc :base-score (:dispatch/base-score memory))

         (contains? memory :content-match/score)
         (assoc :content-match-score (:content-match/score memory))

         (:via memory)
         (assoc :via (:via memory))

         (:dispatch/endpoint memory)
         (assoc :endpoint (:dispatch/endpoint memory)))))
   (range)
   ranked))

(defn- recall-now
  [{:keys [problem subjects limit substrate-base recall-timeout-ms
           receipt-ranking? receipt-alpha include-pre-cutoff-ranking?]
    :or {receipt-ranking? true receipt-alpha default-receipt-alpha}
    :as opts}
   packet]
  (let [started-ns (System/nanoTime)
        trace-id (str "dispatch-recall-" (UUID/randomUUID))
        terrain-map (read-bpm-terrains (default-terrain-readme))
        query-data (recall-query opts packet terrain-map)
        substrate-base (or substrate-base (substrate/configured-url))
        per-call-timeout (per-call-timeout-ms recall-timeout-ms)
        {:keys [search projection entry]}
        (substrate-seams substrate-base per-call-timeout)
        batch-recall
        (fn [ctx endpoints recall-opts]
          (memory-recall/recall-by-endpoints
           ctx endpoints (assoc recall-opts :fetch-components projection)))
        propose-with
        (fn [q]
          (memory-recall/propose-patterns-by-query
           {:domain :mathematics}
           q
           {:limit (min 20 (* 2 limit))
            :trace-id trace-id
            :search-evidence search
            :recall-batch-fn batch-recall}))
        ;; Walk the ladder strictest-first, stopping at the first tier that
        ;; returns candidates. Records which tier fired so the offered half
        ;; shows whether precision or breadth produced the result.
        ladder-hit
        (first (for [{:keys [tier q]} (query-ladder (:terms query-data))
                     :let [p (propose-with q)]
                     :when (proposal-hit? p)]
                 (assoc p :recall/tier tier :recall/query-used q)))
        proposals (or ladder-hit
                      (assoc (propose-with (:query query-data))
                             :recall/tier :none :recall/query-used (:query query-data)))
        pattern-ids (->> (:candidates proposals)
                         (sort-by proposal-rank)
                         (map :pattern-id)
                         (take limit))
        pattern-id-set (set pattern-ids)
        endpoints (->> (concat [problem] subjects pattern-ids)
                       (remove str/blank?)
                       distinct
                       (take 20))
        recall-batch
        (memory-recall/recall-by-endpoints
         {:domain :mathematics}
         endpoints
         {:limit limit
          :trace-id trace-id
          :fetch-components projection})
        recalls (:recalls recall-batch)
        candidates
        (->> (concat
              (:content-matches proposals)
              (mapcat
               (fn [recall]
                 (map #(assoc %
                              :via (if (contains? pattern-id-set
                                                  (:endpoint recall))
                                     :pattern
                                     :endpoint)
                              :dispatch/endpoint (:endpoint recall))
                      (:memories recall)))
               recalls))
             (reduce
              (fn [{:keys [seen items] :as acc} memory]
                (if (contains? seen (:memory/id memory))
                  acc
                  {:seen (conj seen (:memory/id memory))
                   :items (conj items memory)}))
             {:seen #{} :items []})
             :items
             vec)
        elapsed-ms (/ (double (- (System/nanoTime) started-ns)) 1000000.0)
        stats-timeout-ms
        (long
         (max 0
              (min default-receipt-stats-timeout-ms
                   (- recall-timeout-ms elapsed-ms 500.0))))
        receipt-stats
        (if (and receipt-ranking?
                 (seq candidates)
                 (>= stats-timeout-ms 250))
          (try
            (aggregate-use-receipts
             (receipt-entries substrate-base stats-timeout-ms)
             candidates)
            (catch Throwable _
              {:memories {} :patterns {}}))
          {:memories {} :patterns {}})
        stats-found?
        (boolean
         (some (fn [[_ stat]]
                 (or (pos? (:offered-count stat))
                     (pos? (:used-count stat))))
               (:memories receipt-stats)))
        ranked
        (if (and receipt-ranking? stats-found?)
          (rank-memories candidates (:memories receipt-stats) receipt-alpha)
          candidates)
        scored-memory-stats
        (when stats-found?
          (into (sorted-map)
                (map (fn [memory]
                       (let [memory-id (:memory/id memory)]
                         [memory-id
                          (scored-use-stat
                           (get-in receipt-stats [:memories memory-id])
                           receipt-alpha)])))
                candidates))
        ranking-audit
        (cond-> {:enabled receipt-ranking?
                 :alpha receipt-alpha
                 :stats-found? stats-found?}
          stats-found?
          (assoc :per-memory scored-memory-stats
                 :per-pattern (into (sorted-map)
                                    (:patterns receipt-stats))))
        active-system
        (if (and receipt-ranking? stats-found?)
          receipt-ranked-system
          recall-system)
        query-data
        (assoc query-data
               :recall-system active-system
               :receipt-ranking ranking-audit)
        memories
        (->> ranked
             (keep
              (fn [memory]
                (if (map? (:memory/body memory))
                  memory
                  (when-let [full-entry (entry (:memory/id memory))]
                    (when (map? (:evidence/body full-entry))
                      (assoc memory
                             :memory/body (:evidence/body full-entry)))))))
             (take limit)
             vec)]
    (cond->
     {:status (if (seq memories) :ok :recall-empty)
      :trace-id trace-id
      :query query-data
      :proposal-count (count (:candidates proposals))
      :lexical-seed (:lexical-seed proposals)
      :index-as-of (:index-as-of proposals)
      :ladder-rung (:recall/tier proposals)
      :ladder-query (:recall/query-used proposals)
      :pattern-ids (vec pattern-ids)
      :endpoints (vec endpoints)
      :memories memories}
      include-pre-cutoff-ranking?
      (assoc :pre-cutoff-ranking
             (pre-cutoff-ranking-audit ranked limit)))))

(defn bounded-recall
  "Run recall under one total wall-clock budget. Every failure is converted to
  a typed empty result so dispatch can continue."
  [opts packet]
  (let [task (future (recall-now opts packet))
        timeout-ms (:recall-timeout-ms opts)
        result (deref task timeout-ms ::timeout)]
    (if (= ::timeout result)
      (do
        (future-cancel task)
        {:status :recall-empty
         :reason :timeout
         :query (recall-query opts packet
                              (read-bpm-terrains (default-terrain-readme)))
         :memories []})
      result)))

(def ^:private max-recall-error-message-length 1000)

(defn- throwable-chain
  [error]
  (take 16 (take-while some? (iterate #(.getCause ^Throwable %) error))))

(defn- store-unavailable-error?
  "Conservatively recognize evidenced HTTP or network transport failures."
  [error]
  (boolean
   (some
    (fn [cause]
      (let [{:keys [url status]} (ex-data cause)]
        (or (and (string? url) (some? status))
            (instance? java.net.ConnectException cause)
            (instance? java.net.NoRouteToHostException cause)
            (instance? java.net.SocketTimeoutException cause)
            (instance? java.net.UnknownHostException cause)
            (instance? java.net.http.HttpTimeoutException cause)
            (instance? java.io.EOFException cause)
            (instance? javax.net.ssl.SSLException cause))))
    (throwable-chain error))))

(defn- bounded-error-message
  [error]
  (let [message (or (.getMessage ^Throwable error) (str error))]
    (if (> (count message) max-recall-error-message-length)
      (subs message 0 max-recall-error-message-length)
      message)))

(defn safe-recall [opts packet]
  (try
    (bounded-recall opts packet)
    (catch Throwable error
      {:status :recall-empty
       :reason (if (store-unavailable-error? error)
                 :store-unavailable
                 :recall-error)
       :error (bounded-error-message error)
       :query (recall-query opts packet
                            (read-bpm-terrains (default-terrain-readme)))
       :memories []})))

(defn apply-withholding
  "Apply an explicit dispatch-time ablation after recall and before packet
  assembly. Preserve the requested ids even when they were not candidates so
  the offered receipt can distinguish a delivered intervention from a miss."
  [recall-result withhold-ids]
  (let [requested (vec (distinct (remove str/blank? withhold-ids)))
        withheld? (set requested)
        memories (vec (remove #(contains? withheld? (:memory/id %))
                              (:memories recall-result)))
        removed (->> (:memories recall-result)
                     (map :memory/id)
                     (filter withheld?)
                     vec)]
    (cond-> (assoc recall-result
                   :memories memories
                   :withheld-memory-ids requested
                   :withholding-delivered-ids removed)
      (and (= :ok (:status recall-result)) (empty? memories))
      (assoc :status :recall-empty
             :reason :all-surfaced-memories-withheld))))

(defn- summarize [value]
  (let [text
        (cond
          (string? value) value
          ;; Actionable payload FIRST: MT1 (2026-07-27) showed a surfaced
          ;; lemma-location memory whose rendered summary described the
          ;; memory without carrying the lemma names — the runner could not
          ;; use it without opening the store. Names before narration.
          (map? value)
          (let [c (if (map? (:content value)) (:content value) value)
                payload
                (->> [(:lemma c)
                      (when-let [s (seq (:supporting-lemmas c))]
                        (str "supporting: " (str/join ", " s)))
                      (:location c)
                      (:use c)
                      (:problem-class c)
                      (:rule c)
                      (:rationale value)
                      (:summary value)]
                     (remove #(or (nil? %) (= "" %)))
                     (str/join " — "))]
            (if (str/blank? payload) (pr-str value) payload))
          (nil? value) ""
          :else (pr-str value))
        single-line (str/replace (str/trim text) #"\s+" " ")]
    (if (> (count single-line) 480)
      (str (subs single-line 0 477) "...")
      single-line)))

(defn render-memory [index memory]
  (let [envelope (:memory/body memory)
        body (if (map? envelope) (:body envelope) envelope)
        name (or (when (map? envelope) (:name envelope))
                 (:memory/hook memory)
                 (:memory/id memory))
        level (or (when (map? envelope) (:level envelope))
                  (when (map? body) (:level body))
                  (:memory/kind memory)
                  :unspecified)]
    (str index ". " name "\n"
         "   level: " (if (keyword? level) (clojure.core/name level) level) "\n"
         "   memory-id: " (:memory/id memory) "\n"
         "   summary: " (summarize body))))

(defn- recall-outcome-label
  [{:keys [status reason]}]
  (cond
    (= :timeout reason) :timeout
    (= :store-unavailable reason) :store-unavailable
    (= :recall-error reason) :recall-error
    (= :ok status) :completed-with-memories
    :else :completed-empty))

(defn- recall-status-block
  [recall-result]
  (let [outcome (recall-outcome-label recall-result)]
    (str
     "DISPATCH-TIME RECALL STATUS\n"
     "[dispatch-recall-outcome=" (name outcome) "]\n"
     (case outcome
       :timeout
       (str "Recall TIMED OUT before completing. This is infrastructure "
            "unavailability, not evidence of a terrain or corpus gap.\n")

       :store-unavailable
       (str "An HTTP or transport failure prevented recall from reaching the "
            "store successfully. This is infrastructure unavailability, not "
            "evidence of a terrain or corpus gap.\n")

       :recall-error
       (str "Recall encountered an ERROR before completing. The cause was not "
            "classified as an HTTP or transport failure. This is not evidence "
            "of a terrain or corpus gap.\n")

       :completed-with-memories
       "Recall completed and supplied the reviewed memories below.\n"

       :completed-empty
       (if (= :all-surfaced-memories-withheld (:reason recall-result))
         (str "Recall COMPLETED, but every matched memory was removed by the "
              "registered dispatch-time withholding intervention. This is not "
              "evidence of a terrain or corpus gap.\n")
         (str "Recall COMPLETED but found no reviewed memories to surface. "
              "Only this status is a genuine empty retrieval result.\n")))
     "OUTCOME-RECEIPT REQUIREMENT: copy the bracketed dispatch-recall-outcome "
     "value verbatim into the final Memory usage section. Do not report an "
     "incomplete recall as \"none surfaced\" or as a terrain gap. When memories "
     "are supplied, give EACH surfaced memory id exactly one line in that section: "
     "`USED <id>: <mechanism>` or `IGNORED <id>: <reason>`. Missing per-id "
     "attribution makes the outcome incomplete and excludes it from use endpoints."
     "\n")))

(defn- assemble-push-packet
  [packet recall-result]
  (str (recall-status-block recall-result)
       (when-let [memories (seq (:memories recall-result))]
         (str "\nPOTENTIALLY RELEVANT MEMORIES "
              "(from prior sessions — use your judgment)\n\n"
              (str/join "\n\n"
                        (map-indexed
                         (fn [index memory]
                           (render-memory (inc index) memory))
                         memories))))
       "\n\n--- PROBLEM PACKET ---\n\n"
       packet))

(defn assemble-packet
  "Assemble the selected memory-channel packet.

  The two-argument arity and `:push` branch preserve the pre-channel packet
  bytes exactly. Pull invitations are fixed, versioned experimental material."
  ([packet recall-result]
   (assemble-packet packet recall-result default-memory-channel))
  ([packet recall-result memory-channel]
   (case memory-channel
     :push (assemble-push-packet packet recall-result)
     :push+pull
     (str (recall-status-block recall-result)
          (when-let [memories (seq (:memories recall-result))]
            (str "\nPOTENTIALLY RELEVANT MEMORIES "
                 "(from prior sessions — use your judgment)\n\n"
                 (str/join "\n\n"
                           (map-indexed
                            (fn [index memory]
                              (render-memory (inc index) memory))
                            memories))))
          "\n\n" memory-pull-invitation
          "\n\n--- PROBLEM PACKET ---\n\n"
          packet)
     :pull-only
     (str memory-pull-invitation
          "\n\n--- PROBLEM PACKET ---\n\n"
          packet)
     :none packet
     (throw (ex-info "unsupported memory channel"
                     {:memory-channel memory-channel})))))

(defn offered-evidence
  "Build the persisted offered-half record using the shared use-receipt
  contract. With no memories this is still a valid recall-empty receipt."
  [{:keys [problem from memory-channel]} recall-result job-id session-id]
  (let [memory-ids (mapv :memory/id (:memories recall-result))
        withheld-ids (vec (:withheld-memory-ids recall-result))
        surfaced-at (str (Instant/now))
        inclusion-reasons
        (into {}
              (map (fn [memory-id]
                     [memory-id
                      "reviewed attachment surfaced by terrain-conditioned dispatch recall"])
                   memory-ids))
        receipt
        (assoc
         (memory-contract/use-receipt
          {:decision-id job-id
           :session-id session-id
           :domain :mathematics
           :surfaced-memory-ids memory-ids
           :used-memory-ids []
           :inclusion-reasons inclusion-reasons
           :memory-use-kinds
           (into {}
                 (keep (fn [memory]
                         (when-let [kind (:memory-use/kind memory)]
                           [(:memory/id memory) kind])))
                 (:memories recall-result))
           :cascade-id (or (:trace-id recall-result)
                           (str "dispatch-recall-empty-" (UUID/randomUUID)))
           :surfaced-at surfaced-at
           :recorded-at surfaced-at})
         :memory-use/surfacing-via
         (mapv (fn [memory]
                 {:memory-id (:memory/id memory)
                  :via (:via memory)})
               (:memories recall-result))
         :memory-use/withheld-ids withheld-ids)]
    {:subject {:ref/type (if (str/starts-with? problem "bpm-")
                           :bpm-problem
                           :apm-problem)
               :ref/id problem}
     :type :pattern-outcome
     :claim-type :observation
     :author from
     :session-id session-id
     :body (cond-> {:event :memory-use
                    :phase :offered
                    :memory-channel (or memory-channel default-memory-channel)
                    :memory-pull-invitation-version
                    memory-pull-invitation-version
                    :recall-system
                    (or (get-in recall-result [:query :recall-system])
                        recall-system)
                    :problem problem
                    :job-id job-id
                    :recall-status (:status recall-result)
                    :recall-query (:query recall-result)
                    :recall-lexical-seed (vec (or (:lexical-seed recall-result) []))
                    :recall-index-as-of (:index-as-of recall-result)
                    :recall-ladder-rung (or (:ladder-rung recall-result) :unavailable)
                    :recall-ladder-query (:ladder-query recall-result)
                    :withheld-memory-ids withheld-ids
                    :withholding-delivered-ids
                    (vec (:withholding-delivered-ids recall-result))
                    :memory-use receipt}
             (:reason recall-result)
             (assoc :recall-reason (:reason recall-result))
             (string? (:error recall-result))
             (assoc :recall-error-message
                    (subs (:error recall-result)
                          0
                          (min (count (:error recall-result))
                               max-recall-error-message-length)))
             ;; Routemap "reason-bearing none": an empty recall names the
             ;; terms that matched nothing — the mint lane's work queue.
             (= :recall-empty (:status recall-result))
             (assoc :mint-candidates
                    (vec (take 8 (remove #(str/starts-with? % "apm-")
                                         (get-in recall-result [:query :terms] []))))))
     :tags (cond-> [:memory :memory-use :memory-offered]
             (= :recall-empty (:status recall-result))
             (conj :recall-empty))}))

(defn- post-json [url body timeout-ms]
  (let [response
        (http/post url {:headers {"Content-Type" "application/json"
                                 "Accept" "application/json"}
                       :body (json/generate-string body)
                       :timeout timeout-ms
                       :throw false})
        parsed (try
                 (json/parse-string (:body response) true)
                 (catch Throwable _ {:raw (:body response)}))]
    (if (<= 200 (long (:status response)) 299)
      parsed
      (throw (ex-info "HTTP write failed"
                      {:url url :status (:status response) :body parsed})))))

(defn- dispatch! [{:keys [base to from mission]} packet]
  (post-json
   (str (trim-base base) "/api/alpha/bell")
   {:agent-id to
    :prompt packet
    :caller from
    :mission-id mission
    :mode "work"}
   30000))

(defn- record-offered! [{:keys [substrate-base]} evidence]
  ;; Receipts persist in the authoritative substrate (:7073), NOT the agency
  ;; base — posting to :base was the original silent failure. The substrate
  ;; endpoint reads an EDN EvidenceEntry (namespaced keys) and requires a
  ;; penholder header; JSON with bare keys is the silent-lost-writes defect
  ;; class (traced 2026-07-26).
  (let [entry {:evidence/id (str "e-" (UUID/randomUUID))
               :evidence/subject (:subject evidence)
               :evidence/type (:type evidence)
               :evidence/claim-type (:claim-type evidence)
               :evidence/at (str (Instant/now))
               :evidence/author (str (:author evidence))
               :evidence/session-id (str (:session-id evidence))
               :evidence/body (:body evidence)
               :evidence/tags (:tags evidence)}
        target (or substrate-base "http://127.0.0.1:7073")
        response (http/post (str (trim-base target) "/api/alpha/evidence")
                            {:headers {"Content-Type" "application/edn"
                                       "Accept" "application/edn"
                                       "x-penholder" "api"}
                             :body (pr-str entry)
                             :timeout 30000
                             :throw false})]
    (if (<= 200 (long (:status response)) 299)
      (:evidence/id entry)
      (throw (ex-info "HTTP write failed"
                      {:url (str (trim-base target) "/api/alpha/evidence")
                       :status (:status response)})))))

(defn- require-input! [opts packet]
  (when-not (:problem opts)
    (throw (ex-info "--problem is required" {})))
  (when-not (:to opts)
    (throw (ex-info "--to is required" {})))
  (when (str/blank? packet)
    (throw (ex-info "problem packet on stdin is empty" {})))
  ;; Thin-packet gate (meta-draft reject-thin-dispatch-packets, 2026-07-27):
  ;; a stub packet reaching Agency burns a runner job on an unactionable
  ;; prompt. Require a real contract unless the operator says otherwise.
  (when (and (not (:allow-thin? opts))
             (< (count packet) 200))
    (throw (ex-info (str "packet is implausibly thin (" (count packet)
                         " chars); pass --allow-thin to override")
                    {:length (count packet)}))))

(defn- no-push-recall-result
  [opts packet]
  {:status :not-invoked
   :reason :memory-channel-no-push
   :query (recall-query opts packet
                        (read-bpm-terrains (default-terrain-readme)))
   :memories []})

(defn run-dispatch!
  "Execute one CLI dispatch. Kept public for focused tests."
  [opts packet]
  (require-input! opts packet)
  (let [memory-channel (or (:memory-channel opts) default-memory-channel)
        recall-result
        (if (contains? #{:push :push+pull} memory-channel)
          (apply-withholding
           (safe-recall opts packet)
           (:withhold-ids opts))
          (no-push-recall-result opts packet))
        assembled (assemble-packet packet recall-result memory-channel)]
    (if (:dry-run? opts)
      (let [evidence (offered-evidence
                      opts recall-result
                      "<dry-run-job-id>" "<dry-run-session-id>")]
        (println "=== ASSEMBLED PACKET (DRY RUN; NOT DISPATCHED) ===")
        (println assembled)
        (println "\n=== OFFERED RECEIPT (DRY RUN; NOT WRITTEN) ===")
        (println (json/generate-string evidence {:pretty true}))
        {:dry-run? true
         :recall recall-result
         :assembled-packet assembled
         :evidence evidence})
      (let [dispatch-response (dispatch! opts assembled)
            job-id (or (:job-id dispatch-response)
                       (:job_id dispatch-response))
            session-id (or (:session-id dispatch-response)
                           (:session_id dispatch-response)
                           job-id)]
        (when-not (and (string? job-id) (not (str/blank? job-id)))
          (throw (ex-info "Agency bell returned no job-id"
                          {:response dispatch-response})))
        (let [evidence (offered-evidence opts recall-result job-id session-id)]
          (try
            (record-offered! opts evidence)
            (catch Throwable error
              (binding [*out* *err*]
                (println "dispatch_with_recall: WARNING — dispatch succeeded but"
                         "offered receipt write failed:"
                         (or (.getMessage error) (str error))))))
          (println job-id)
          {:job-id job-id
           :recall recall-result
           :assembled-packet assembled
           :evidence evidence})))))

(defn -main [& args]
  (try
    (let [opts (parse-args args)]
      (if (:help? opts)
        (println (usage))
        (run-dispatch! opts (slurp *in*))))
    (catch Throwable error
      (binding [*out* *err*]
        (println "dispatch_with_recall:" (or (.getMessage error) (str error)))
        (println (usage)))
      (System/exit 2))
    (finally
      ;; `bounded-recall` uses a future so a timed-out substrate read cannot
      ;; hold the dispatch. Do not leave the CLI waiting on the future pool.
      (shutdown-agents))))
