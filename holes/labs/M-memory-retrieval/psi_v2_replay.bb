#!/usr/bin/env bb
;; Psi-v2 dark replay harness.
;;
;; Fixed design: psi-v2-design.md. Store access is bounded GET-only. Frozen
;; exports and results are write-once; reruns replay without touching the
;; store. No live ranking path is imported or modified.

(ns psi-v2-replay
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def repo-root "/home/joe/code/futon3c")
(def lab-root (str repo-root "/holes/labs/M-memory-retrieval"))
(def graph-export-path
  (str repo-root
       "/holes/labs/M-typed-memories/live-graph-export-20260727.edn"))
(def receipts-export-path (str lab-root "/receipts-export-20260728.edn"))
(def results-path (str lab-root "/psi-v2-replay-results-20260728.edn"))
(def base-url "http://127.0.0.1:7073")
(def page-limit 200)
(def max-pages 10)
(def retry-backoff-ms 5000)
(def alpha 0.5)
(def n-min-coeff 5.0)
(def promotion-min 20.0)
;; Fixed before replay: a 1% minimum mass per admitted pattern. Ranking uses
;; mass / uniform-mass so a neutral coefficient remains factor 1.
(def exploration-floor 0.01)

(defn encode [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn parse-body [body]
  (cond
    (map? body) body
    (string? body)
    (try
      (edn/read-string {:default (fn [_tag value] value)} body)
      (catch Exception exception
        {:replay/parse-error (.getMessage exception)}))
    :else nil))

(defn retryable? [status body]
  (or (= 503 status)
      (= :expensive-read-busy (:error body))
      (= :expensive-read-busy (:error/code body))))

(defn get-page [url]
  (loop [attempt-number 1
         attempts []]
    (let [started (System/nanoTime)
          response
          (try
            (http/get url
                      {:headers {"accept" "application/edn"}
                       :timeout 60000
                       :throw false})
            (catch Exception exception
              {:status nil
               :transport-error (.getMessage exception)}))
          elapsed-ms (/ (- (System/nanoTime) started) 1.0e6)
          body (when (:body response) (parse-body (:body response)))
          status (:status response)
          attempt
          {:attempt attempt-number
           :status status
           :elapsed-ms elapsed-ms
           :error (or (:transport-error response)
                      (:replay/parse-error body)
                      (:error/code body)
                      (:error body))}
          attempts-next (conj attempts attempt)]
      (cond
        (= 200 status)
        {:ok? true :body body :attempts attempts-next}

        (and (= attempt-number 1) (retryable? status body))
        (do
          (Thread/sleep retry-backoff-ms)
          (recur 2 attempts-next))

        :else
        {:ok? false
         :body body
         :attempts attempts-next
         :error {:status status
                 :code (or (:error/code body) (:error body))
                 :parse-error (:replay/parse-error body)
                 :transport-error (:transport-error response)}}))))

(defn evidence-url
  [{:keys [type author cursor]}]
  (str base-url
       "/api/alpha/evidence?type=" (encode type)
       "&author=" (encode author)
       "&limit=" page-limit
       (when cursor
         (str "&cursor-at=" (encode (:at cursor))
              "&cursor-id=" (encode (:id cursor))))))

(defn paged-evidence-query [query]
  (loop [page-number 1
         cursor nil
         entries []
         pages []]
    (let [url (evidence-url (assoc query :cursor cursor))
          response (get-page url)
          body (:body response)
          page-entries (vec (or (:entries body) []))
          next-cursor (:next-cursor body)
          page-audit
          {:page page-number
           :url url
           :limit page-limit
           :row-count (count page-entries)
           :attempts (:attempts response)
           :error (:error response)}
          entries-next (into entries page-entries)
          pages-next (conj pages page-audit)]
      (cond
        (not (:ok? response))
        {:entries entries-next
         :pages pages-next
         :truncated? true
         :error (:error response)}

        (nil? next-cursor)
        {:entries entries-next
         :pages pages-next
         :truncated? false}

        (= page-number max-pages)
        {:entries entries-next
         :pages pages-next
         :truncated? true
         :error {:code :page-budget-exhausted
                 :max-pages max-pages}}

        :else
        (recur (inc page-number)
               next-cursor
               entries-next
               pages-next)))))

(defn write-once! [path value]
  (if (.exists (java.io.File. path))
    (let [existing (edn/read-string (slurp path))]
      (when-not (= existing value)
        (throw (ex-info "refusing to overwrite prior replay artifact"
                        {:path path})))
      :existing)
    (do (spit path (pr-str value)) :written)))

(defn pattern-description-entry? [entry]
  (= :pattern-description
     (get-in entry [:evidence/body :event])))

(defn build-export []
  (let [receipts
        (paged-evidence-query
         {:type "pattern-outcome" :author "ground-control"})
        descriptions
        (paged-evidence-query
         {:type "reflection" :author "ground-control"})
        graph-export (edn/read-string (slurp graph-export-path))
        description-entries
        (filterv pattern-description-entry? (:entries descriptions))]
    {:export/version 1
     :captured-at (str (Instant/now))
     :store-read-only? true
     :source {:base-url base-url
              :receipt-type :pattern-outcome
              :author "ground-control"}
     :queries
     {:receipts (dissoc receipts :entries)
      :pattern-descriptions (dissoc descriptions :entries)}
     :truncated?
     (or (:truncated? receipts)
         (:truncated? descriptions)
         (:truncated? graph-export))
     :receipt-count (count (:entries receipts))
     :pattern-description-count (count description-entries)
     :receipts (:entries receipts)
     :pattern-descriptions
     (into
      (sorted-map)
      (keep
       (fn [entry]
         (let [body (:evidence/body entry)
               pattern-id (:pattern-id body)
               description (:description body)]
           (when (and (string? pattern-id)
                      (string? description))
             [pattern-id description]))))
      description-entries)
     :attachment-graph
     {:source-file (.getName (java.io.File. graph-export-path))
      :captured-at (:captured-at graph-export)
      :queries (:queries graph-export)
      :row-count (:row-count graph-export)
      :truncated? (:truncated? graph-export)
      :hyperedges (:hyperedges graph-export)}}))

(defn load-or-export! []
  (if (.exists (java.io.File. receipts-export-path))
    (edn/read-string (slurp receipts-export-path))
    (let [export (build-export)]
      (write-once! receipts-export-path export)
      export)))

(defn memory-use-entry? [entry]
  (let [body (:evidence/body entry)]
    (and (= :memory-use (:event body))
         (contains? #{:offered :outcome} (:phase body))
         (string? (:job-id body))
         (map? (:memory-use body)))))

(defn entry-order [entry]
  [(str (:evidence/at entry)) (str (:evidence/id entry))])

(defn join-receipts [entries]
  (let [eligible (filterv memory-use-entry? entries)
        by-job (group-by #(get-in % [:evidence/body :job-id]) eligible)
        joined
        (->>
         by-job
         (keep
          (fn [[job-id job-entries]]
            (let [offered
                  (sort-by entry-order
                           (filter #(= :offered
                                       (get-in % [:evidence/body :phase]))
                                   job-entries))
                  outcomes
                  (sort-by entry-order
                           (filter #(= :outcome
                                       (get-in % [:evidence/body :phase]))
                                   job-entries))]
              (when (and (seq offered) (seq outcomes))
                {:job-id job-id
                 :offered (last offered)
                 :outcome (last outcomes)}))))
         (sort-by :job-id)
         vec)
        joined-jobs (set (map :job-id joined))
        offered-total
        (count (filter #(= :offered
                           (get-in % [:evidence/body :phase]))
                       eligible))
        outcome-total
        (count (filter #(= :outcome
                           (get-in % [:evidence/body :phase]))
                       eligible))
        joined-count (count joined)]
    {:joined joined
     :audit
     {:eligible-half-count (count eligible)
      :job-count (count by-job)
      :joined-job-count joined-count
      :unjoined-job-count (- (count by-job) (count joined-jobs))
      :unjoined-half-counts
      {:offered (- offered-total joined-count)
       :outcome (- outcome-total joined-count)}}}))

(defn normalized-row [{:keys [job-id offered outcome]}]
  (let [offered-body (:evidence/body offered)
        outcome-body (:evidence/body outcome)
        offered-use (:memory-use offered-body)
        outcome-use (:memory-use outcome-body)]
    {:job-id job-id
     :problem (:problem offered-body)
     :recall-status (:recall-status offered-body)
     :recall-query-terms
     (vec (or (get-in offered-body [:recall-query :terms]) []))
     :surfaced-ids
     (vec (or (:memory-use/surfaced-ids offered-use) []))
     :used-ids
     (vec (or (:memory-use/used-ids outcome-use) []))
     :outcome
     (:outcome outcome-body)
     :offered-evidence-id (:evidence/id offered)
     :outcome-evidence-id (:evidence/id outcome)}))

(defn current-reviewed-attachments [hyperedges]
  (reduce
   (fn [by-memory hyperedge]
     (let [props (:hx/props hyperedge)
           memory-id (get-in props [:roles :entry])
           patterns (vec (or (get-in props [:roles :patterns]) []))]
       (if (and (string? memory-id)
                (= :current (:state props))
                (= :reviewed (:attachment-status props))
                (seq patterns))
         (assoc by-memory memory-id
                (vec (sort (filter string? patterns))))
         by-memory)))
   (sorted-map)
   hyperedges))

(defn edge-type-census [hyperedges]
  (let [pattern-count
        (reduce
         +
         0
         (map #(count (or (get-in % [:hx/props :roles :patterns]) []))
              hyperedges))
        distills-count
        (reduce
         +
         0
         (map
          #(count
            (filter
             (fn [value]
               (and (string? value)
                    (str/starts-with? value "e-")))
             (or (get-in % [:hx/props :roles :distills]) [])))
          hyperedges))]
    (cond-> (sorted-map :pattern-attachment pattern-count)
      (pos? distills-count) (assoc :distills distills-count))))

(defn lexical-terms [values]
  (->> values
       (mapcat #(re-seq #"[a-z0-9_]+" (str/lower-case (str %))))
       (filter #(>= (count %) 3))
       set))

(defn attribution
  [memory-id query-terms attachments descriptions]
  (let [patterns (vec (get attachments memory-id))
        query-set (lexical-terms query-terms)
        matched
        (filterv
         (fn [pattern-id]
           (seq
            (set/intersection
             query-set
             (lexical-terms
              [pattern-id (get descriptions pattern-id "")]))))
         patterns)]
    (cond
      (seq matched)
      {:memory-id memory-id :mode :matched :patterns matched}

      (seq patterns)
      {:memory-id memory-id :mode :fallback-all :patterns patterns}

      :else
      {:memory-id memory-id :mode :unattributable :patterns []})))

(defn row-attributions [row attachments descriptions]
  (let [rows
        (mapv #(attribution % (:recall-query-terms row)
                            attachments descriptions)
              (:surfaced-ids row))
        modes (set (map :mode rows))
        row-mode
        (cond
          (or (empty? rows) (contains? modes :unattributable))
          :unattributable

          (contains? modes :fallback-all)
          :fallback-all

          :else :matched)]
    {:mode row-mode :memories rows}))

(defn empty-stat []
  {:offered 0.0 :used 0.0})

(defn pattern-stats
  [rows attachments descriptions]
  (reduce
   (fn [stats row]
     (reduce
      (fn [inner {:keys [memory-id patterns]}]
        (if (seq patterns)
          (let [fraction (/ 1.0 (double (count patterns)))
                used? (contains? (set (:used-ids row)) memory-id)]
            (reduce
             (fn [by-pattern pattern-id]
               (cond-> (update by-pattern pattern-id
                               #(merge-with + (empty-stat) %
                                            {:offered fraction}))
                 used?
                 (update-in [pattern-id :used] (fnil + 0.0) fraction)))
             inner
             patterns))
          inner))
      stats
      (:memories (row-attributions row attachments descriptions))))
   (sorted-map)
   rows))

(defn memory-stats [rows]
  (reduce
   (fn [stats row]
     (let [used (set (:used-ids row))]
       (reduce
        (fn [inner memory-id]
          (cond-> (update inner memory-id
                          #(merge-with + (empty-stat) %
                                       {:offered 1.0}))
            (contains? used memory-id)
            (update-in [memory-id :used] (fnil + 0.0) 1.0)))
        stats
        (:surfaced-ids row))))
   (sorted-map)
   rows))

(defn coefficient [stat minimum]
  (let [{:keys [offered used]} (merge (empty-stat) stat)
        active? (>= offered minimum)
        ratio (if (pos? offered) (/ used offered) 0.0)]
    {:offered offered
     :used used
     :use-rate ratio
     :active? active?
     :theta (if active? (+ 1.0 (* alpha ratio)) 1.0)}))

(defn floored-theta-state [coefficients]
  (let [pattern-ids (vec (sort (keys coefficients)))
        n (count pattern-ids)
        _ (when (or (zero? n)
                    (> (* n exploration-floor) 1.0))
            (throw (ex-info "infeasible explicit exploration floor"
                            {:pattern-count n
                             :exploration-floor exploration-floor})))
        total (reduce + (map #(get-in coefficients [% :theta])
                             pattern-ids))
        residual (- 1.0 (* n exploration-floor))
        uniform-mass (/ 1.0 n)]
    (into
     (sorted-map)
     (map
      (fn [pattern-id]
        (let [raw (get-in coefficients [pattern-id :theta])
              mass (+ exploration-floor (* residual (/ raw total)))]
          [pattern-id
           {:raw-theta raw
            :mass mass
            :ranking-factor (/ mass uniform-mass)}]))
      pattern-ids))))

(defn base-score [index]
  (/ 1.0 (+ 1.0 (* 0.05 index))))

(defn rank-with-factors [memory-ids factor-fn]
  (->> memory-ids
       (map-indexed
        (fn [index memory-id]
          {:memory-id memory-id
           :base-score (base-score index)
           :factor (double (factor-fn memory-id))
           :score (* (base-score index)
                     (double (factor-fn memory-id)))}))
       (sort-by (juxt (comp - :score) :memory-id))
       (mapv :memory-id)))

(defn rank-s6 [row training-rows]
  (let [stats (memory-stats training-rows)]
    (rank-with-factors
     (:surfaced-ids row)
     (fn [memory-id]
       (:theta (coefficient (get stats memory-id) 0.0))))))

(defn rank-psi-v2
  [row training-rows attachments descriptions]
  (let [stats (pattern-stats training-rows attachments descriptions)
        pattern-ids (->> attachments vals (mapcat identity) distinct sort)
        raw-coefficients
        (into
         (sorted-map)
         (map (fn [pattern-id]
                [pattern-id
                 (coefficient (get stats pattern-id) n-min-coeff)]))
         pattern-ids)
        theta-state (floored-theta-state raw-coefficients)
        by-memory
        (into {} (map (juxt :memory-id identity))
              (:memories
               (row-attributions row attachments descriptions)))]
    (rank-with-factors
     (:surfaced-ids row)
     (fn [memory-id]
       (let [patterns (:patterns (get by-memory memory-id))
             weights (keep #(get-in theta-state [% :ranking-factor])
                           patterns)]
         (if (seq weights)
           (/ (reduce + weights) (double (count weights)))
           1.0))))))

(defn reciprocal-rank [ranking used-ids]
  (let [used (set used-ids)
        index (first (keep-indexed
                      (fn [i memory-id]
                        (when (used memory-id) i))
                      ranking))]
    (if (some? index) (/ 1.0 (inc index)) 0.0)))

(defn row-metrics [ranking used-ids]
  {:reciprocal-rank (reciprocal-rank ranking used-ids)
   :hit-at-1? (boolean (contains? (set used-ids) (first ranking)))})

(defn empty-used-class [row]
  (cond
    (or (= :recall-empty (:recall-status row))
        (empty? (:surfaced-ids row)))
    :recall-empty

    (= :surfaced-not-usable (get-in row [:outcome :result]))
    :surfaced-not-usable

    :else :reasoned-non-use))

(defn score-rows [rows attachments descriptions]
  (mapv
   (fn [row]
     (let [training (filterv #(not= (:job-id row) (:job-id %)) rows)
           rankings
           {:no-psi (:surfaced-ids row)
            :s6-scalar (rank-s6 row training)
            :psi-v2
            (rank-psi-v2 row training attachments descriptions)}]
       {:job-id (:job-id row)
        :problem (:problem row)
        :surfaced-ids (:surfaced-ids row)
        :used-ids (:used-ids row)
        :empty-used-classification
        (when (empty? (:used-ids row)) (empty-used-class row))
        :attribution (row-attributions row attachments descriptions)
        :rankings rankings
        :metrics
        (when (seq (:used-ids row))
          (into {} (map (fn [[arm ranking]]
                          [arm (row-metrics ranking (:used-ids row))]))
                rankings))}))
   rows))

(defn arm-metrics [scored-rows]
  (let [eligible (filterv #(seq (:used-ids %)) scored-rows)
        n (count eligible)]
    (into
     (sorted-map)
     (for [arm [:no-psi :s6-scalar :psi-v2]
           :let [metrics (map #(get-in % [:metrics arm]) eligible)]]
       [arm
        {:n n
         :mrr (if (pos? n)
                (/ (reduce + (map :reciprocal-rank metrics)) n)
                0.0)
         :hit-at-1 (if (pos? n)
                     (/ (count (filter :hit-at-1? metrics))
                        (double n))
                     0.0)}]))))

(defn synthetic-validation! []
  (let [attachments
        {"g1" ["math/good-route"]
         "g2" ["math/good-route"]
         "g3" ["math/good-route"]
         "b1" ["math/bad-route"]}
        descriptions
        {"math/good-route" "good route"
         "math/bad-route" "bad route"}
        target
        {:job-id "fixture-1"
         :recall-query-terms ["good"]
         :surfaced-ids ["b1" "g1"]
         :used-ids ["g1"]}
        training-row
        (fn [job-id]
          {:job-id job-id
           :recall-query-terms ["good"]
           :surfaced-ids ["g1" "g2" "g3" "b1"]
           :used-ids ["g1" "g2" "g3"]})
        training [(training-row "fixture-2")
                  (training-row "fixture-3")]
        stats (pattern-stats training attachments descriptions)
        theta-good (coefficient (get stats "math/good-route")
                                n-min-coeff)
        fixture-coefficients
        {"math/good-route" theta-good
         "math/bad-route"
         (coefficient (get stats "math/bad-route") n-min-coeff)}
        fixture-theta (floored-theta-state fixture-coefficients)
        ranking
        (rank-psi-v2 target training attachments descriptions)]
    (assert (= ["b1" "g1"] (:surfaced-ids target))
            "synthetic no-Psi control order changed")
    (assert (= {:offered 6.0 :used 6.0}
               (get stats "math/good-route"))
            (str "synthetic good-route credits mismatch: " stats))
    (assert (= 1.5 (:theta theta-good))
            (str "synthetic theta mismatch: " theta-good))
    (assert (every? #(>= (:mass %) exploration-floor)
                    (vals fixture-theta))
            (str "synthetic floor violation: " fixture-theta))
    (assert (= ["g1" "b1"] ranking)
            (str "synthetic Psi-v2 ranking mismatch: " ranking))
    {:passed? true
     :row-count 3
     :expected-ranking ["g1" "b1"]
     :actual-ranking ranking
     :good-route-counts (get stats "math/good-route")
     :good-route-theta (:theta theta-good)}))

(defn replay [export]
  (let [joined (join-receipts (:receipts export))
        rows (mapv normalized-row (:joined joined))
        hyperedges (get-in export [:attachment-graph :hyperedges])
        attachments (current-reviewed-attachments hyperedges)
        descriptions (:pattern-descriptions export)
        scored (score-rows rows attachments descriptions)
        global-pattern-stats (pattern-stats rows attachments descriptions)
        pattern-ids (->> attachments vals (mapcat identity) distinct sort)
        raw-coefficients
        (into
         (sorted-map)
         (map (fn [pattern-id]
                [pattern-id
                 (coefficient (get global-pattern-stats pattern-id)
                              n-min-coeff)]))
         pattern-ids)
        theta-state (floored-theta-state raw-coefficients)
        coefficients
        (into
         (sorted-map)
         (map (fn [[pattern-id coefficient-row]]
                [pattern-id
                 (merge coefficient-row (get theta-state pattern-id))]))
         raw-coefficients)
        census (edge-type-census hyperedges)
        empty-rows (filterv #(empty? (:used-ids %)) scored)
        promotable?
        (and (seq coefficients)
             (every? #(>= (:offered %) promotion-min)
                     (vals coefficients)))]
    {:replay/version 1
     :design {:alpha alpha
              :n-min-coeff n-min-coeff
              :promotion-minimum promotion-min
              :exploration-floor exploration-floor
              :floor-space :normalized-pattern-mass
              :ranking-factor :mass-over-uniform-mass
              :leave-one-out? true
              :tie-break :memory-id}
     :synthetic-validation (synthetic-validation!)
     :source
     {:receipt-export (.getName (java.io.File. receipts-export-path))
      :receipt-count (:receipt-count export)
      :truncated? (:truncated? export)}
     :join-audit (:audit joined)
     :joined-row-count (count rows)
     :metric-row-count (count (filter #(seq (:used-ids %)) rows))
     :empty-used-rows
     {:count (count empty-rows)
      :classification-counts
      (frequencies (keep :empty-used-classification empty-rows))
      :rows
      (mapv #(select-keys % [:job-id :problem
                             :empty-used-classification])
            empty-rows)}
     :attribution-mode-census
     (frequencies (map #(get-in % [:attribution :mode]) scored))
     :edge-type-census census
     :theta-r
     {:status (if (= 1 (count census))
                :inactive-degenerate
                :not-fitted-out-of-scope)
      :fitted? false}
     :pattern-coefficients coefficients
     :arm-metrics (arm-metrics scored)
     :row-audit scored
     :promotion (if promotable?
                  :eligible-for-interface-review
                  :below-calibration-minimum)}))

(def export (load-or-export!))
(def result (replay export))
(def result-write (write-once! results-path result))

(println "Psi-v2 dark replay complete")
(println "receipts/joined/metric:"
         (:receipt-count export) "/"
         (:joined-row-count result) "/"
         (:metric-row-count result))
(println "unjoined halves:"
         (get-in result [:join-audit :unjoined-half-counts]))
(println "attribution modes:" (:attribution-mode-census result))
(println "arm metrics:" (:arm-metrics result))
(println "theta-r:" (:theta-r result))
(println "promotion:" (:promotion result))
(println "result artifact:" result-write)
