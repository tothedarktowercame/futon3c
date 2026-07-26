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
(def default-recall-timeout-ms 30000)
(def default-agency-base "http://localhost:7070")
(def default-mission "M-zai-learning-loop")
(def default-problem-root "/home/joe/code/apm-lean/problems")
(def recall-system :v1-enriched)

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
    "using" "validate" "where" "which" "with" "would"})

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

(defn parse-args
  "Parse the deliberately small CLI without adding another dependency."
  [args]
  (loop [remaining args
         opts {:limit default-limit
               :recall-timeout-ms default-recall-timeout-ms
               :base default-agency-base
               :mission default-mission
               :from "ground-control"
               :subjects []}]
    (if-let [arg (first remaining)]
      (cond
        (= "--dry-run" arg)
        (recur (next remaining) (assoc opts :dry-run? true))

        (= "--help" arg)
        (recur (next remaining) (assoc opts :help? true))

        (contains? #{"--problem" "--to" "--from" "--base" "--mission"
                     "--subject" "--terrain" "--substrate-base" "--limit"
                     "--recall-timeout-ms"}
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
             "--terrain" (assoc opts :terrain value)
             "--substrate-base" (assoc opts :substrate-base value)
             "--limit" (assoc opts :limit (positive-int arg value))
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
   "  --terrain TEXT            explicit terrain override\n"
   "  --recall-timeout-ms N     total recall budget (default 30000)\n"
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

(defn- text-keywords [text limit]
  (->> (re-seq #"[A-Za-z][A-Za-z0-9_/-]{3,}" (str/lower-case text))
       (remove stopwords)
       frequencies
       (sort-by (fn [[word count]] [(- count) word]))
       (map first)
       (take limit)
       vec))

(defn- problem-statement-text [text]
  (let [marker "## Problem Statement"
        index (str/index-of text marker)]
    (if index (subs text index) text)))

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
             :terms (text-keywords packet 8)}))))

(defn recall-query
  "Build a bounded lexical query from subject ids, preregistered terrain, and
  problem files, with packet terms retained as fallback. The exact problem id
  is also queried as a graph endpoint."
  [{:keys [problem subjects terrain] :as opts} packet terrain-map]
  (let [terrain (or terrain (some-> problem bpm-source-id terrain-map))
        term-sources
        (mapv
         (fn [source]
           (update source :terms
                   #(vec (remove #{(str/lower-case problem)} %))))
         (problem-term-sources opts packet))
        source-terms (mapcat :terms term-sources)
        terms (->> (concat [problem] subjects
                           (when terrain [terrain])
                           source-terms)
                   distinct
                   (take 36)
                   vec)]
    {:terrain terrain
     :recall-system recall-system
     :term-sources term-sources
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

(defn- recall-now
  [{:keys [problem subjects limit substrate-base recall-timeout-ms] :as opts}
   packet]
  (let [trace-id (str "dispatch-recall-" (UUID/randomUUID))
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
        proposals
        (memory-recall/propose-patterns-by-query
         {:domain :mathematics}
         (:query query-data)
         {:limit (min 20 (* 2 limit))
          :trace-id trace-id
          :search-evidence search
          :recall-batch-fn batch-recall})
        pattern-ids (->> (:candidates proposals)
                         (sort-by proposal-rank)
                         (map :pattern-id)
                         (take limit))
        endpoints (->> (concat [problem] subjects pattern-ids)
                       (remove str/blank?)
                       distinct
                       (take 20))
        recalls
        (mapv
         (fn [endpoint]
           (memory-recall/recall-by-endpoint
            {:domain :mathematics}
            endpoint
            {:limit limit
             :include-bodies? true
             :trace-id trace-id
             :fetch-components projection
             :fetch-entry entry}))
         endpoints)
        memories
        (->> recalls
             (mapcat
              (fn [recall]
                (map #(assoc % :dispatch/endpoint (:endpoint recall))
                     (:memories recall))))
             (reduce
              (fn [{:keys [seen items] :as acc} memory]
                (if (contains? seen (:memory/id memory))
                  acc
                  {:seen (conj seen (:memory/id memory))
                   :items (conj items memory)}))
              {:seen #{} :items []})
             :items
             (take limit)
             vec)]
    {:status (if (seq memories) :ok :recall-empty)
     :trace-id trace-id
     :query query-data
     :proposal-count (count (:candidates proposals))
     :pattern-ids (vec pattern-ids)
     :endpoints (vec endpoints)
     :memories memories}))

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

(defn safe-recall [opts packet]
  (try
    (bounded-recall opts packet)
    (catch Throwable error
      {:status :recall-empty
       :reason :store-unavailable
       :error (or (.getMessage error) (str error))
       :query (recall-query opts packet
                            (read-bpm-terrains (default-terrain-readme)))
       :memories []})))

(defn- summarize [value]
  (let [text
        (cond
          (string? value) value
          (map? value) (or (:problem-class value)
                           (:rule value)
                           (:use value)
                           (:rationale value)
                           (pr-str value))
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

(defn assemble-packet [packet memories]
  (if (seq memories)
    (str "POTENTIALLY RELEVANT MEMORIES "
         "(from prior sessions — use your judgment)\n\n"
         (str/join "\n\n" (map-indexed
                           (fn [index memory]
                             (render-memory (inc index) memory))
                           memories))
         "\n\n--- PROBLEM PACKET ---\n\n"
         packet)
    packet))

(defn offered-evidence
  "Build the persisted offered-half record using the shared use-receipt
  contract. With no memories this is still a valid recall-empty receipt."
  [{:keys [problem from]} recall-result job-id session-id]
  (let [memory-ids (mapv :memory/id (:memories recall-result))
        surfaced-at (str (Instant/now))
        inclusion-reasons
        (into {}
              (map (fn [memory-id]
                     [memory-id
                      "reviewed attachment surfaced by terrain-conditioned dispatch recall"])
                   memory-ids))
        receipt
        (memory-contract/use-receipt
         {:decision-id job-id
          :session-id session-id
          :domain :mathematics
          :surfaced-memory-ids memory-ids
          :used-memory-ids []
          :inclusion-reasons inclusion-reasons
          :cascade-id (or (:trace-id recall-result)
                          (str "dispatch-recall-empty-" (UUID/randomUUID)))
          :surfaced-at surfaced-at
          :recorded-at surfaced-at})]
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
                    :recall-system recall-system
                    :problem problem
                    :job-id job-id
                    :recall-status (:status recall-result)
                    :recall-query (:query recall-result)
                    :memory-use receipt}
             (:reason recall-result)
             (assoc :recall-reason (:reason recall-result)))
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

(defn- record-offered! [{:keys [base]} evidence]
  (post-json (str (trim-base base) "/api/alpha/evidence") evidence 30000))

(defn- require-input! [opts packet]
  (when-not (:problem opts)
    (throw (ex-info "--problem is required" {})))
  (when-not (:to opts)
    (throw (ex-info "--to is required" {})))
  (when (str/blank? packet)
    (throw (ex-info "problem packet on stdin is empty" {}))))

(defn run-dispatch!
  "Execute one CLI dispatch. Kept public for focused tests."
  [opts packet]
  (require-input! opts packet)
  (let [recall-result (safe-recall opts packet)
        assembled (assemble-packet packet (:memories recall-result))]
    (if (:dry-run? opts)
      (let [evidence (offered-evidence
                      opts recall-result
                      "<dry-run-job-id>" "<dry-run-session-id>")]
        (println "=== ASSEMBLED PACKET (DRY RUN; NOT DISPATCHED) ===")
        (println assembled)
        (println "\n=== OFFERED RECEIPT (DRY RUN; NOT WRITTEN) ===")
        (println (json/generate-string evidence {:pretty true}))
        {:dry-run? true :recall recall-result :evidence evidence})
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
          {:job-id job-id :recall recall-result :evidence evidence})))))

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
