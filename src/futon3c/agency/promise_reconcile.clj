(ns futon3c.agency.promise-reconcile
  "Re-runnable catch-up from retained promise-history evidence to operational edges."
  (:refer-clojure :exclude [run!])
  (:require [futon3c.agency.promise-log :as promise-log]
            [futon3c.agency.promise-state :as promise-state]
            [futon3c.agency.rule-record :as store]
            [futon3c.substrate.client :as substrate])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def page-limit 1000)
(def default-max-pages 100)
(def default-page-sleep-ms 50)
(def history-dual-write-at "2026-11-05T17:12:00Z")
(def edge-page-size 1000)
(def state-edge-limit 10000)
(def history-edge-limit 100000)
(def batch-size 500)

(def ^:dynamic *request-fn* store/request!)
(def ^:dynamic *sleep-fn* (fn [millis] (Thread/sleep millis)))
(def ^:dynamic *read-hyperedges-fn* substrate/hyperedges-by-type)
(def ^:dynamic *batch-post-fn*
  (fn [edges]
    (store/request! (substrate/configured-url) "POST"
                    "/api/alpha/hyperedges/batch" {:hyperedges edges})))

(defn- encode [value] (URLEncoder/encode (str value) "UTF-8"))

(defn- truncated! [pages rows detail]
  (throw (ex-info "Promise reconciliation evidence read stopped before exhaustion"
                  (merge {:reason :truncated-input
                          :source :promise-history
                          :pages pages
                          :rows rows}
                         detail))))

(defn read-evidence-history
  "Read every promise-history evidence page up to MAX-PAGES, sleeping between
   pages. Never returns a partial population."
  [{:keys [base max-pages page-sleep-ms]
    :or {base (substrate/configured-url)
         max-pages default-max-pages
         page-sleep-ms default-page-sleep-ms}}]
  (when-not (pos-int? max-pages)
    (throw (ex-info "Promise reconciliation requires a positive page cap"
                    {:reason :invalid-page-cap :max-pages max-pages})))
  (let [base-path (str "/api/alpha/evidence?tags=" (encode "promise-history")
                       "&limit=" page-limit)]
    (loop [path base-path pages 0 rows [] seen #{}]
      (let [response (*request-fn* base "GET" path nil)
            entries (vec (:entries response))
            pages' (inc pages)
            rows' (into rows entries)
            cursor (:next-cursor response)]
        (when (or (:truncated response) (:partial? response)
                  (and (:incomplete response) (nil? cursor)))
          (truncated! pages' (count rows') {:response-partial? true}))
        (cond
          (nil? cursor) {:rows rows' :pages pages'}
          (>= pages' max-pages)
          (truncated! pages' (count rows') {:next-cursor cursor})
          (or (not (string? (:at cursor)))
              (not (string? (:id cursor)))
              (contains? seen cursor))
          (truncated! pages' (count rows') {:invalid-cursor cursor})
          :else
          (do
            (when (pos? page-sleep-ms) (*sleep-fn* page-sleep-ms))
            (recur (str base-path "&cursor-at=" (encode (:at cursor))
                        "&cursor-id=" (encode (:id cursor)))
                   pages' rows' (conj seen cursor))))))))

(defn- promise-id [row]
  (or (get-in row [:evidence/body :history/promise-id])
      (get-in row [:evidence/body :id])
      (get-in row [:evidence/body :followup-id])))

(defn- edge-read-options [limit]
  {:limit limit
   :page-size edge-page-size
   :request-budget (inc (quot limit edge-page-size))})

(defn- estimated-requests [edges]
  (or (:requests (meta edges))
      (max 1 (long (Math/ceil (/ (double (count edges)) edge-page-size))))))

(defn- read-edges!
  [hx-type limit requests]
  (let [edges (*read-hyperedges-fn* hx-type (edge-read-options limit))]
    (swap! requests + (estimated-requests edges))
    (when (substrate/partial-result? edges)
      (throw (ex-info "Promise reconciliation hyperedge prefetch exceeded its bound"
                      (merge {:reason :truncated-input
                              :source hx-type
                              :rows (count edges)}
                             (meta edges)))))
    (vec edges)))

(defn- edge-map [edges]
  (into {} (map (juxt :hx/id identity)) edges))

(defn- read-history-chunk!
  "Read back only the promise chains represented in CHUNK. The indexed
   type+end route has no usable cursor, so a full window is a refusal."
  [chunk requests]
  (let [base (substrate/configured-url)]
    (reduce
     (fn [readback promise-id]
       (swap! requests inc)
       (let [response (*request-fn*
                       base "GET"
                       (str "/api/alpha/hyperedges?type="
                            (encode "promise/history-event")
                            "&end=" (encode promise-id)
                            "&limit=" page-limit "&include-total=false") nil)
             edges (vec (:hyperedges response))]
         (when (or (>= (count edges) page-limit) (:truncated response)
                   (:partial? response) (:incomplete response)
                   (:next-cursor response))
           (throw (ex-info "Promise history readback exceeded its bound"
                           {:reason :truncated-input
                            :source :promise/history-event
                            :promise-id promise-id
                            :rows (count edges)})))
         (into readback (edge-map edges))))
     {}
     (distinct (map #(get-in % [:hx/props :history/promise-id]) chunk)))))

(defn- before-history-dual-write? [row]
  (.isBefore (Instant/parse (:evidence/at row))
             (Instant/parse history-dual-write-at)))

(defn- state-current? [states state]
  (let [expected (promise-state/state->hyperedge state)
        stored (get states (:hx/id expected))
        stored-head (or (get-in stored [:hx/props :head-sequence]) 0)
        expected-head (:head-sequence state)]
    (or (> stored-head expected-head)
        (and (= stored-head expected-head)
             (promise-log/same-edge? expected stored)))))

(defn- chunk-failures!
  [hx-type limit chunk requests]
  (let [response (try
                   (swap! requests inc)
                   (*batch-post-fn* chunk)
                   (catch Throwable error {:error error}))
        results (:results response)
        post-failed (cond
                      (:error response) (set (map :hx/id chunk))
                      (not= (count chunk) (count results)) (set (map :hx/id chunk))
                      :else (into #{}
                                  (keep-indexed
                                   (fn [index result]
                                     (when-not (:ok result)
                                       (:hx/id (nth chunk index)))))
                                  results))
        readback (if (= :promise/history-event hx-type)
                   (read-history-chunk! chunk requests)
                   (try
                     (edge-map (read-edges! hx-type limit requests))
                     (catch Throwable _ ::unavailable)))]
    (into post-failed
          (keep (fn [expected]
                  (when (or (= ::unavailable readback)
                            (not (promise-log/same-edge?
                                  expected (get readback (:hx/id expected)))))
                    (:hx/id expected))))
          chunk)))

(defn- write-chunks!
  [hx-type limit edges requests]
  (reduce (fn [{:keys [failed chunks]} chunk]
            {:failed (into failed (chunk-failures! hx-type limit chunk requests))
             :chunks (inc chunks)})
          {:failed #{} :chunks 0}
          (partition-all batch-size edges)))

(defn run!
  "Catch operational promise projections up to retained evidence. DRY-RUN?
   performs all reads and decisions but makes no writes or deferred mutations."
  ([] (run! {}))
  ([{:keys [dry-run? all-history?] :as options}]
   (let [started (System/nanoTime)
         {:keys [rows pages]} (read-evidence-history options)
         groups (group-by promise-id rows)
         requests (atom pages)
         state-edges (edge-map (read-edges! :promise/state state-edge-limit requests))
         history-edges (edge-map (read-edges! :promise/history-event
                                              history-edge-limit requests))
         counters (atom {:states-written 0
                         :already-current 0
                         :incomplete-by-reason {}
                         :history-edges-written 0})
         planned-states (atom [])
         planned-history (atom [])
         edge-owners (atom {})]
     ;; Open promises first: they are what current obligations read, and a full
     ;; run writes thousands of edges at several seconds each.
     (doseq [[pid history folded]
             (->> groups
                  (map (fn [[pid history]] [pid history (promise-state/fold-state history)]))
                  (sort-by (fn [[pid _ folded]] [(if (= :open (:status folded)) 0 1) pid])))]
       (if (= :incomplete (:promise/state folded))
         (swap! counters update-in [:incomplete-by-reason (:reason folded)]
                (fnil inc 0))
         (let [current? (state-current? state-edges folded)]
           (if current?
             (swap! counters update :already-current inc)
             (do
               (swap! counters update :states-written inc)
               (let [edge (promise-state/state->hyperedge folded)]
                 (swap! planned-states conj edge)
                 (swap! edge-owners assoc (:hx/id edge) pid))))
           (doseq [row history
                   :when (or all-history?
                             (= :open (:status folded))
                             (not (before-history-dual-write? row)))
                     :let [edge (promise-log/entry->hyperedge row)]
                     :when (not (promise-log/same-edge?
                                 edge (get history-edges (:hx/id edge))))]
               (swap! counters update :history-edges-written inc)
               (swap! planned-history conj edge)
               (swap! edge-owners assoc (:hx/id edge) pid)))))
     ;; The prefetched heads are intentionally not checked again before each
     ;; chunk. A concurrent writer can only make this projection temporarily
     ;; stale; the stable ids and never-rewind prefetch make a rerun converge.
     (let [state-result (if (or dry-run? (empty? @planned-states))
                          {:failed #{} :chunks 0}
                          (write-chunks! :promise/state state-edge-limit
                                         @planned-states requests))
           history-result (if (or dry-run? (empty? @planned-history))
                            {:failed #{} :chunks 0}
                            (write-chunks! :promise/history-event history-edge-limit
                                           @planned-history requests))
           failed (into (:failed state-result) (:failed history-result))
           failed-promises (into #{} (keep @edge-owners) failed)]
       (when-not dry-run?
         (doseq [pid (remove failed-promises (keys groups))]
           (swap! promise-state/!deferred dissoc pid)))
       (let [report (merge {:promises-seen (count groups)
                          :pages-read pages
                          :dry-run? (boolean dry-run?)
                           :duration-ms (quot (- (System/nanoTime) started) 1000000)
                           :requests-made @requests
                           :chunks-sent (+ (:chunks state-result)
                                           (:chunks history-result))
                           :failed-ids-count (count failed)
                           :failed-ids (vec (take 20 (sort failed)))}
                          @counters)]
         (println "promise reconciliation:" (pr-str report))
         report)))))
