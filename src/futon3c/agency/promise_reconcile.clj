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

(def ^:dynamic *request-fn* store/request!)
(def ^:dynamic *sleep-fn* (fn [millis] (Thread/sleep millis)))

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

(defn- get-edge [id]
  (try
    (promise-log/*get-fn* id)
    (catch clojure.lang.ExceptionInfo error
      (if (= 404 (:status (ex-data error))) nil (throw error)))))

(defn- write-edge! [edge]
  (promise-log/*post-fn* edge)
  (let [stored (promise-log/*get-fn* (:hx/id edge))]
    (when-not (promise-log/same-edge? edge stored)
      (throw (ex-info "Promise reconciliation exact readback mismatch"
                      {:reason :promise-reconcile-readback-mismatch
                       :hx/id (:hx/id edge)})))
    stored))

(defn- before-history-dual-write? [row]
  (.isBefore (Instant/parse (:evidence/at row))
             (Instant/parse history-dual-write-at)))

(defn- history-edge-current? [row]
  (let [expected (promise-log/entry->hyperedge row)
        stored (get-edge (:hx/id expected))]
    (promise-log/same-edge? expected stored)))

(defn- state-current? [state]
  (let [stored (get-edge (promise-state/state-id (:promise-id state)))]
    (and stored
         (>= (or (get-in stored [:hx/props :head-sequence]) 0)
             (:head-sequence state)))))

(defn run!
  "Catch operational promise projections up to retained evidence. DRY-RUN?
   performs all reads and decisions but makes no writes or deferred mutations."
  ([] (run! {}))
  ([{:keys [dry-run?] :as options}]
   (let [started (System/nanoTime)
         {:keys [rows pages]} (read-evidence-history options)
         groups (group-by promise-id rows)
         counters (atom {:states-written 0
                         :already-current 0
                         :incomplete-by-reason {}
                         :history-edges-written 0})]
     (doseq [[pid history] (sort-by key groups)]
       (let [folded (promise-state/fold-state history)]
         (if (= :incomplete (:promise/state folded))
           (swap! counters update-in [:incomplete-by-reason (:reason folded)]
                  (fnil inc 0))
           (let [current? (state-current? folded)]
             (if current?
               (swap! counters update :already-current inc)
               (do
                 (swap! counters update :states-written inc)
                 (when-not dry-run?
                   (write-edge! (promise-state/state->hyperedge folded)))))
             (when (= :open (:status folded))
               (doseq [row (filter before-history-dual-write? history)
                       :when (not (history-edge-current? row))]
                 (swap! counters update :history-edges-written inc)
                 (when-not dry-run? (promise-log/write! row))))
             (when-not dry-run? (swap! promise-state/!deferred dissoc pid))))))
     (let [report (merge {:promises-seen (count groups)
                          :pages-read pages
                          :dry-run? (boolean dry-run?)
                          :duration-ms (quot (- (System/nanoTime) started) 1000000)}
                         @counters)]
       (println "promise reconciliation:" (pr-str report))
       report))))
