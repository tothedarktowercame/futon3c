(ns export
  "SEQ-0.3 read-only attachment-state snapshot exporter."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.security MessageDigest]
           [java.time Duration Instant]))

(def store-base
  (str/replace (or (System/getenv "FUTON_SUBSTRATE_URL")
                   (System/getenv "FUTON1A_URL")
                   "http://127.0.0.1:7073") #"/+$" ""))
(def here "holes/labs/M-memory-retrieval/attachment-export")
(def receipts-path
  "holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn")
(def output-path (str here "/attachment-state.json"))
(def memory-limit 1000)
(def projection-limit 100)
(def endpoint-batch-size 20)
(def timeout-ms 30000)
(def max-read-attempts 12)
(def example-job-ids
  ["invoke-1785449302666-400-2956dd68"
   "invoke-1785464073951-441-1c05a75c"
   "invoke-1785473298737-474-6e1af56a"])

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

(defn- request-edn [method url opts]
  (loop [attempt 1]
    (let [response ((case method :get http/get :post http/post)
                    url (merge {:headers {"Accept" "application/edn"}
                                :throw false :timeout timeout-ms}
                               opts))
          body (try (edn/read-string (:body response))
                    (catch Throwable _ (:body response)))]
      (cond
        (<= 200 (long (:status response)) 299) body
        (and (= 503 (:status response)) (< attempt max-read-attempts))
        (do (Thread/sleep 1000) (recur (inc attempt)))
        :else
        (throw (ex-info "read-only store request failed"
                        {:method method :url url :status (:status response)
                         :attempt attempt :body body}))))))

(defn- memory-snapshot []
  (request-edn
   :get (str store-base "/api/alpha/evidence?type=memory&limit=" memory-limit)
   {}))

(defn- review-entry? [entry]
  (= :memory-attachment-review
     (get-in entry [:evidence/body :review/event])))

(defn- review-endpoints [memory-response]
  (->> (:entries memory-response)
       (filter review-entry?)
       (mapcat #(get-in % [:evidence/body :review/pattern-ids]))
       (filter string?) distinct sort vec))

(defn- projection-snapshot [endpoints]
  (mapv
   (fn [batch]
     (request-edn
      :post (str store-base "/api/alpha/memory/projection")
      {:headers {"Accept" "application/edn"
                 "Content-Type" "application/edn"}
       :body (pr-str {:endpoints (vec batch) :limit projection-limit})}))
   (partition-all endpoint-batch-size endpoints)))

(defn- distinct-edges [projection-responses]
  (->> projection-responses
       (mapcat :groups)
       (mapcat :components)
       (reduce (fn [result component]
                 (assoc result (get-in component [:edge :hx/id])
                        (:edge component)))
               (sorted-map))
       vals vec))

(defn- attachment-rows [edges memory-by-id]
  (->> edges
       (mapcat
        (fn [edge]
          (let [props (:hx/props edge)
                memory-id (get-in props [:roles :entry])
                memory-entry (get memory-by-id memory-id)
                review (:review props)]
            (for [pattern-id (sort (get-in props [:roles :patterns]))]
              {:edge-id (:hx/id edge)
               :memory-id memory-id
               :pattern-id pattern-id
               :edge-state (:state props)
               :attachment-status (:attachment-status props)
               :asserted-at (:evidence/at memory-entry)
               :asserted-at-source
               (when (:evidence/at memory-entry) :memory-evidence-entry)
               :reviewed-at (:reviewed-at review)
               :review-evidence-id (:evidence-id review)
               :review-verdict (:verdict review)
               :reviewer (:reviewer review)
               :system-time (:system-time props)}))))
       (sort-by (juxt :pattern-id :memory-id :edge-id)) vec))

(defn- current-reviewed? [row]
  (and (= :current (:edge-state row))
       (= :reviewed (:attachment-status row))))

(defn- pattern-aggregates [endpoints rows]
  (let [current (filter current-reviewed? rows)
        by-pattern (group-by :pattern-id current)]
    (mapv (fn [pattern-id]
            {:pattern-id pattern-id
             :reviewed-attachment-count (count (get by-pattern pattern-id []))})
          endpoints)))

(defn- density-summary [aggregates]
  (let [values (vec (sort (map :reviewed-attachment-count aggregates)))
        n (count values)
        frequencies (frequencies values)]
    {:pattern-count n
     :zero-count (get frequencies 0 0)
     :minimum (first values)
     :median (nth values (quot n 2))
     :maximum (last values)
     :frequency (into (sorted-map) frequencies)}))

(defn- dispatch-offered [entries job-id]
  (some #(when (and (= job-id (get-in % [:evidence/body :job-id]))
                    (= :offered (get-in % [:evidence/body :phase]))) %)
        entries))

(defn- seconds-between [before after]
  (.getSeconds (Duration/between (Instant/parse before) (Instant/parse after))))

(defn- join-example [receipt-entries rows density-by-pattern watermark job-id]
  (let [entry (or (dispatch-offered receipt-entries job-id)
                  (throw (ex-info "worked-example dispatch missing"
                                  {:job-id job-id})))
        dispatch-at (:evidence/at entry)
        surfaced (vec (get-in entry [:evidence/body :memory-use
                                     :memory-use/surfaced-ids]))
        via-by-id (into {} (map (juxt :memory-id :via))
                        (get-in entry [:evidence/body :memory-use
                                       :memory-use/surfacing-via]))
        matches (->> rows
                     (filter #(contains? (set surfaced) (:memory-id %)))
                     (map #(assoc %
                                  :dispatch-surfacing-via
                                  (get via-by-id (:memory-id %))
                                  :endpoint-density-at-snapshot
                                  (get density-by-pattern (:pattern-id %))
                                  :reviewed-by-dispatch
                                  (boolean
                                   (and (:reviewed-at %)
                                        (not (pos? (compare (:reviewed-at %)
                                                            dispatch-at)))))))
                     vec)
        lag (seconds-between dispatch-at watermark)]
    {:job-id job-id
     :dispatch-at dispatch-at
     :offered-evidence-id (:evidence/id entry)
     :surfaced-memory-ids surfaced
     :matched-attachment-rows matches
     :snapshot-lag-seconds lag
     :staleness-bounds-seconds {:lower 0 :upper lag}
     :temporal-verdict
     :snapshot-state-only-not-proof-of-state-at-dispatch}))

(defn -main []
  (let [started (str (Instant/now))
        memories-before (memory-snapshot)
        endpoints (review-endpoints memories-before)
        projections (projection-snapshot endpoints)
        completed (str (Instant/now))
        memories-after (memory-snapshot)
        before-sha (snapshot-sha memories-before)
        after-sha (snapshot-sha memories-after)
        _ (when-not (= before-sha after-sha)
            (throw (ex-info "memory evidence moved during attachment snapshot"
                            {:before before-sha :after after-sha})))
        edges (distinct-edges projections)
        memory-by-id (into {} (map (juxt :evidence/id identity))
                           (:entries memories-before))
        rows (attachment-rows edges memory-by-id)
        aggregates (pattern-aggregates endpoints rows)
        density-by-pattern (into {} (map (juxt :pattern-id
                                               :reviewed-attachment-count))
                                 aggregates)
        receipts (:entries (edn/read-string (slurp receipts-path)))
        projection-audits (mapv :audit projections)
        output
        {:schema-version "seq-0.3/attachment-state-v1"
         :read-only true
         :snapshot-watermark
         {:read-started-at started
          :read-completed-at completed
          :store-base store-base
          :memory-entry-count (:count memories-before)
          :memory-evidence-snapshot-sha256 before-sha
          :memory-evidence-unchanged-during-read true
          :review-event-count (count (filter review-entry?
                                            (:entries memories-before)))
          :discovered-endpoint-count (count endpoints)
          :projection-batch-count (count projections)
          :projection-audits projection-audits
          :projection-edge-snapshot-sha256 (snapshot-sha edges)
          :distinct-memory-assert-edge-count (count edges)
          :component-row-count (reduce + (map :selected-row-count
                                               projection-audits))}
         :read-counts
         {:evidence/type=memory (:count memories-before)
          :evidence/memory-attachment-review
          (count (filter review-entry? (:entries memories-before)))
          :hyperedge/type=memory-assert (count edges)}
         :scope
         {:endpoint-discovery
          "all pattern ids named by type=memory attachment-review evidence"
          :edge-read
          "bounded coherent /api/alpha/memory/projection batches; no whole-store hyperedge scan"
          :density-rule
          "count edge rows with edge-state=current and attachment-status=reviewed"
          :known-boundary
          "an edge whose endpoint has no attachment-review evidence is outside this discovered endpoint universe"}
         :status-counts
         (into (sorted-map)
               (map (fn [[[state status] n]]
                      [(str (name state) "/" (name status)) n]))
               (frequencies
                (map (fn [edge]
                       [(get-in edge [:hx/props :state])
                        (get-in edge [:hx/props :attachment-status])])
                     edges)))
         :attachments rows
         :pattern-aggregates aggregates
         :density-distribution (density-summary aggregates)
         :worked-join-examples
         (mapv #(join-example receipts rows density-by-pattern completed %)
               example-job-ids)}]
    (spit output-path
          (str (json/generate-string (canonical output) {:pretty true}) "\n"))
    (println output-path (sha256 (slurp output-path)))))
