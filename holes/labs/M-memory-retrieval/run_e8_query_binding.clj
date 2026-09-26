#!/usr/bin/env -S clojure -M

(ns run-e8-query-binding
  "Frozen-data E8 known-item retrieval ablation. Read-only against :7073."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.dispatch-with-recall :as dispatch])
  (:import [java.security MessageDigest]))

(def store-base "http://127.0.0.1:7073")
(def problem-root "/home/joe/code/apm-lean/problems")
(def apm-root "/home/joe/code/apm-lean")
(def output-path
  "holes/labs/M-memory-retrieval/e8-query-binding-ranked-20260803.json")
(def store-limit 1000)
(def surfaced-limit 5)
(def recall-timeout-ms 240000)
(def max-read-attempts 120)

(defn- request-edn [method url opts]
  (loop [attempt 1]
    (let [response ((case method :get http/get :post http/post)
                    url (merge {:headers {"Accept" "application/edn"}
                                :throw false
                                :timeout recall-timeout-ms}
                               opts))]
      (cond
        (<= 200 (long (:status response)) 299)
        (edn/read-string (:body response))

        (and (= 503 (:status response)) (< attempt max-read-attempts))
        (do (Thread/sleep 1000) (recur (inc attempt)))

        :else
        (throw (ex-info "read-only store request failed"
                        {:method method :url url :status (:status response)
                         :attempt attempt}))))))

(defn- memories-snapshot []
  (request-edn
   :get
   (str store-base "/api/alpha/evidence?type=memory&limit=" store-limit)
   {}))

(defn- receipts-snapshot []
  (request-edn
   :get
   (str store-base
        "/api/alpha/evidence?type=pattern-outcome&author=ground-control&limit=200")
   {}))

(defn- projection [endpoints]
  (request-edn
   :post (str store-base "/api/alpha/memory/projection")
   {:headers {"Accept" "application/edn"
              "Content-Type" "application/edn"}
    :body (pr-str {:endpoints (vec endpoints) :limit 100})}))

(defn- canonical [x]
  (cond
    (map? x) (into (sorted-map-by #(compare (str %1) (str %2)))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (mapv canonical (sort-by str x))
    (sequential? x) (mapv canonical x)
    :else x))

(defn- sha256 [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn- snapshot-sha [x]
  (sha256 (pr-str (canonical x))))

(defn- group-components [projection-result endpoint]
  (:components (some #(when (= endpoint (:endpoint %)) %)
                     (:groups projection-result))))

(defn- reviewed-mathematics-entry-id [component]
  (let [props (get-in component [:edge :hx/props])]
    (when (and (= :reviewed (:attachment-status props))
               (= :mathematics (:domain props))
               (= :current (or (:state props) :current)))
      (get-in props [:roles :entry]))))

(defn- reachable-ids [projection-result endpoint]
  (->> (group-components projection-result endpoint)
       (keep reviewed-mathematics-entry-id)
       distinct
       sort
       vec))

(defn- slurp-if-file [path]
  (when (.isFile (io/file path)) (slurp path)))

(defn- problem-packet [problem]
  (or (slurp-if-file (str problem-root "/" problem "/problem.md"))
      (throw (ex-info "problem packet source missing" {:problem problem}))))

(defn- git-show [revision path]
  (let [process (ProcessBuilder. ["git" "show" (str revision ":" path)])]
    (.directory process (io/file apm-root))
    (let [p (.start process)
          stdout (slurp (.getInputStream p))
          stderr (slurp (.getErrorStream p))]
      (when-not (zero? (.waitFor p))
        (throw (ex-info "git show failed"
                        {:revision revision :path path :stderr stderr})))
      stdout)))

(defn- ordered-distinct [xs]
  (:items
   (reduce (fn [{:keys [seen] :as acc} x]
             (if (contains? seen x)
               acc
               (-> acc (update :seen conj x) (update :items conj x))))
           {:seen #{} :items []} xs)))

(defn- structure-terms [texts]
  (let [text (str/join "\n" texts)
        imports (map second (re-seq #"(?m)^import\s+([A-Za-z0-9_.]+)" text))
        declarations
        (map second
             (re-seq #"(?m)^\s*(?:theorem|lemma|def|abbrev)\s+([A-Za-z][A-Za-z0-9_'.]*)"
                     text))
        qualified (re-seq #"\b[A-Z][A-Za-z0-9_]*(?:\.[A-Za-z0-9_']+)+" text)
        operators (map second (re-seq #"\\([A-Za-z]{4,})" text))]
    (->> (concat declarations imports qualified operators)
         (map str/lower-case)
         (remove #(contains? #{"begin" "right" "left" "mathbb" "mathrm"
                               "mathbf" "operatorname" "text"} %))
         ordered-distinct
         (take 16)
         vec)))

(defn- case-source [problem]
  (if (= problem "lib-young-completion")
    (let [lean (git-show "525253b" "YoungL2.lean")]
      {:packet lean :structure-texts [lean]
       :source {:kind :git-object :revision "525253b" :path "YoungL2.lean"
                :sha256 (sha256 lean)}})
    (let [base (str problem-root "/" problem)
          packet (problem-packet problem)
          tex (or (slurp-if-file (str base "/problem.tex")) "")
          lean (or (slurp-if-file (str base "/lean/Main.lean")) "")
          ;; Imports are structure, but proof bodies are not: retain imports
          ;; plus the source problem statement only.
          imports (str/join "\n" (filter #(str/starts-with? % "import ")
                                          (str/split-lines lean)))
          structure-texts [packet tex imports]]
      {:packet packet :structure-texts structure-texts
       :source {:kind :problem-statement
                :problem-md-sha256 (sha256 packet)
                :problem-tex-sha256 (sha256 tex)
                :imports-sha256 (sha256 imports)}})))

(defn- memory-text [memory-by-id ids]
  (str/join "\n" (map #(pr-str (:evidence/body (get memory-by-id %))) ids)))

(defn- run-recall [problem packet opts]
  (binding [*out* *err*]
    (println "E8 recall" problem (:arm opts) (:query-term-limit opts)))
  (loop [attempt 1]
    (let [result
          (dispatch/safe-recall
           (merge {:problem problem
                   :subjects []
                   :problem-root problem-root
                   :substrate-base store-base
                   :limit surfaced-limit
                   :recall-timeout-ms recall-timeout-ms
                   :receipt-ranking? true
                   :include-pre-cutoff-ranking? true}
                  (dissoc opts :arm))
           packet)]
      (cond
        (not (contains? #{:timeout :store-unavailable :recall-error}
                        (:reason result)))
        result

        (and (= :store-unavailable (:reason result))
             (< attempt max-read-attempts))
        (do
          (binding [*out* *err*]
            (println "E8 retry after store busy" problem (:arm opts) attempt))
          (Thread/sleep 1000)
          (recur (inc attempt)))

        :else
        (throw (ex-info "recall arm did not complete"
                        {:problem problem :opts opts :attempt attempt
                         :result result}))))))

(defn- compact-result [result expected-ids match-rule baseline-ids execution]
  (let [surfaced (mapv :memory/id (:memories result))
        surfaced-set (set surfaced)
        candidate-list (:pre-cutoff-ranking result)
        candidate-by-id (into {} (map (juxt :memory-id identity)) candidate-list)
        expected-set (set expected-ids)
        hit-ids (vec (filter expected-set surfaced))
        hit? (case match-rule
               :all (= expected-set (set hit-ids))
               :any (boolean (seq hit-ids)))
        rank-by-id (zipmap surfaced (range 1 (inc (count surfaced))))
        union (set/union (set baseline-ids) surfaced-set)
        intersection (set/intersection (set baseline-ids) surfaced-set)
        expected-candidates
        (into
         (sorted-map)
         (for [id expected-ids
               :let [candidate (get candidate-by-id id)
                     rank (:position candidate)
                     cutoff (:cutoff-position candidate surfaced-limit)]]
           [id {:present-in-candidates (boolean candidate)
                :rank rank
                :score (:score candidate)
                :score-kind (:score-kind candidate)
                :cutoff-position cutoff
                :surfaced (contains? surfaced-set id)
                :residual (cond
                            (contains? surfaced-set id) :surfaced
                            candidate :cutoff-pollution
                            :else :endpoint-relative-candidate-absence)}]))]
    {:terms (get-in result [:query :terms])
     :query (get-in result [:query :query])
     :ladder-rung (:ladder-rung result)
     :ladder-query (:ladder-query result)
     :execution execution
     :hit hit?
     :hit-ids hit-ids
     :hit-ranks (into (sorted-map) (map (juxt identity rank-by-id)) hit-ids)
     :expected-target-candidates expected-candidates
     :pre-cutoff-candidate-count (count candidate-list)
     :pre-cutoff-candidates candidate-list
     :cutoff-position surfaced-limit
     :surfaced-ids surfaced
     :surfaced-set-size (count surfaced)
     :empty-recall (empty? surfaced)
     :jaccard-vs-a (if (empty? union) 1.0
                       (/ (double (count intersection)) (count union)))}))

(defn- effective-prefix-reusable? [baseline-result candidate-query]
  (and (not= :none (:ladder-rung baseline-result))
       (= (take 3 (get-in baseline-result [:query :terms]))
          (take 3 (:terms candidate-query)))))

(defn- run-case [case memory-by-id ranking-receipt-entries]
  (let [{:keys [problem expected-ids match-rule]} case
        {:keys [packet structure-texts source]} (case-source problem)
        base-opts {:arm :a :query-term-limit 4
                   :ranking-receipt-entries ranking-receipt-entries}
        a (run-recall problem packet base-opts)
        baseline-ids (mapv :memory/id (:memories a))
        a-compact (compact-result a expected-ids match-rule baseline-ids :live-run)
        b-results
        (into (sorted-map)
              (for [n [8 12 16]
                    :let [opts {:arm (keyword (str "b" n)) :query-term-limit n}
                          opts (assoc opts
                                      :ranking-receipt-entries
                                      ranking-receipt-entries)
                          query (dispatch/recall-query
                                 (merge {:problem problem :subjects []
                                         :problem-root problem-root} opts)
                                 packet {})
                          reuse? (effective-prefix-reusable? a query)
                          result
                          (if reuse?
                            (assoc-in a [:query] query)
                            (run-recall problem packet opts))]]
                [(str "b" n)
                 (compact-result result expected-ids match-rule baseline-ids
                                 (if reuse?
                                   :equivalent-prefix-reuse :live-run))]))
        c-terms (structure-terms structure-texts)
        d-terms (dispatch/query-keywords
                 (memory-text memory-by-id expected-ids) 16)
        c (run-recall problem packet
                      {:arm :c :query-term-limit 16 :query-terms c-terms
                       :ranking-receipt-entries ranking-receipt-entries})
        d (run-recall problem packet
                      {:arm :d :query-term-limit 16 :query-terms d-terms
                       :ranking-receipt-entries ranking-receipt-entries})]
    (assoc case
           :source source
           :structure-terms c-terms
           :oracle-terms d-terms
           :arms (merge (sorted-map "a" a-compact)
                        b-results
                        {"c" (compact-result c expected-ids match-rule
                                             baseline-ids :live-run)
                         "d" (compact-result d expected-ids match-rule
                                             baseline-ids :live-run)}))))

(defn- hit-rate [results arm]
  (let [scoreable (filter :scoreable results)]
    {:hits (count (filter #(get-in % [:arms arm :hit]) scoreable))
     :denominator (count scoreable)
     :rate (if (seq scoreable)
             (/ (double (count (filter #(get-in % [:arms arm :hit]) scoreable)))
                (count scoreable))
             0.0)}))

(defn- write-json! [path value]
  (spit path (str (json/generate-string (canonical value) {:pretty true}) "\n")))

(defn -main [& _]
  (let [mem-before (memories-snapshot)
        receipts-before (receipts-snapshot)
        entries (:entries mem-before)
        memory-by-id (into {} (map (juxt :evidence/id identity)) entries)
        control-endpoints
        ["a93J02" "math/weak-convergence-hilbert"
         "e-30e87097-f843-4341-81c0-a49ee7ce0ef4"
         "e-dfea2de9-8979-4f8f-9343-caabb48487e6"
         "e-9751e537-f5b7-4c40-a857-0c0b699b93a2"]
        control-projection (projection control-endpoints)
        own-a93j02 (reachable-ids control-projection "a93J02")
        weak-pattern (reachable-ids control-projection
                                    "math/weak-convergence-hilbert")
        cases
        [{:case 1 :problem "a93A03"
          :expected-label "direction-scoped liminf memory"
          :expected-ids ["e-30e87097-f843-4341-81c0-a49ee7ce0ef4"]
          :match-rule :all}
         {:case 2 :problem "a93J02"
          :expected-label "any current reviewed memory directly attached to a93J02"
          :expected-ids own-a93j02 :match-rule :any}
         {:case 3 :problem "a96A03"
          :expected-label "any current reviewed memory attached to math/weak-convergence-hilbert"
          :expected-ids weak-pattern :match-rule :any}
         {:case 4 :problem "lib-young-completion"
          :expected-label "both missing-dependency memories"
          :expected-ids ["e-dfea2de9-8979-4f8f-9343-caabb48487e6"
                         "e-9751e537-f5b7-4c40-a857-0c0b699b93a2"]
          :match-rule :all}
         {:case 5 :problem "a96A04"
          :expected-label "inventory-assembly-dependencies memory"
          :expected-ids ["e-9751e537-f5b7-4c40-a857-0c0b699b93a2"]
          :match-rule :all}]
        cases
        (mapv
         (fn [case]
           (let [reachable
                 (into {}
                       (for [id (:expected-ids case)]
                         [id (contains?
                              (set (reachable-ids control-projection id)) id)]))
                 ;; The batched control includes direct memory endpoints only
                 ;; for the singleton/pair ids; set-valued cases are warranted
                 ;; by their problem/pattern endpoint components above.
                 reachable
                 (if (contains? #{2 3} (:case case))
                   (zipmap (:expected-ids case) (repeat true))
                   reachable)]
             (assoc case :reachability reachable
                    :scoreable (every? true? (vals reachable)))))
         cases)
        unscoreable (filterv (complement :scoreable) cases)
        _ (when (seq unscoreable)
            (binding [*out* *err*]
              (println "E8 unscoreable cases" (mapv :case unscoreable))))
        ;; Keep the serving JVM load bounded. Parallel queries caused avoidable
        ;; memory pressure in the first dry run and are not part of the ablation.
        results (mapv #(if (:scoreable %)
                         (run-case % memory-by-id (:entries receipts-before))
                         %)
                      cases)
        mem-after (memories-snapshot)
        receipts-after (receipts-snapshot)
        memory-sha-before (snapshot-sha mem-before)
        memory-sha-after (snapshot-sha mem-after)
        receipt-sha-before (snapshot-sha receipts-before)
        receipt-sha-after (snapshot-sha receipts-after)
        _ (when-not (and (= memory-sha-before memory-sha-after)
                         (= receipt-sha-before receipt-sha-after))
            (throw (ex-info "store changed during E8; refusing a mixed-time artifact"
                            {:memory-before memory-sha-before
                             :memory-after memory-sha-after
                             :receipt-before receipt-sha-before
                             :receipt-after receipt-sha-after})))
        arms ["a" "b8" "b12" "b16" "c" "d"]
        output
        {:experiment "E8-query-binding-ranked"
         :analysis-date "2026-08-03"
         :read-only true
         :store {:base store-base
                 :memory-entry-count (:count mem-before)
                 :memory-snapshot-sha256 memory-sha-before
                 :ranking-receipt-snapshot-sha256 receipt-sha-before
                 :unchanged-during-run true}
         :parameters {:surfaced-limit surfaced-limit
                      :query-term-default dispatch/default-query-term-limit
                      :cardinality-sweep [8 12 16]
                      :recall-timeout-ms recall-timeout-ms}
         :case-2-resolution {:rule "current reviewed mathematics-domain memory/assert components on endpoint a93J02"
                             :ids own-a93j02}
         :case-3-resolution {:rule "current reviewed mathematics-domain memory/assert components on endpoint math/weak-convergence-hilbert"
                             :ids weak-pattern}
         :results results
         :aggregate (into (sorted-map)
                          (map (fn [arm] [arm (hit-rate results arm)])) arms)}]
    (write-json! output-path output)
    (println output-path (sha256 (slurp output-path)))
    (shutdown-agents)))

(apply -main *command-line-args*)
