(ns futon3c.agency.promise-reconcile-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.promise-log :as promise-log]
            [futon3c.agency.promise-reconcile :as reconcile]
            [futon3c.agency.promise-state :as promise-state]))

(defn- row [pid n transition predecessor at]
  {:evidence/id (str "promise-history:" pid ":" n)
   :evidence/type transition
   :evidence/author "debtor"
   :evidence/at at
   :evidence/body (cond-> {:history/format 3
                           :history/payload-edn
                           (pr-str {:record {:id pid :agent "debtor"
                                            :beneficiary "creditor"}})
                           :history/promise-id pid
                           :history/promise-sequence n}
                    predecessor (assoc :history/predecessor predecessor))})

(defn- chain [pid closed?]
  (let [one (row pid 1 :promise/park-made nil "2026-11-05T17:00:00Z")
        two (row pid 2 :promise/woken
                 {:sequence 1 :id (:evidence/id one) :type :promise/park-made}
                 "2026-11-05T17:01:00Z")]
    (cond-> [one two]
      closed? (conj (row pid 3 :promise/released
                         {:sequence 2 :id (:evidence/id two) :type :promise/woken}
                         "2026-11-05T17:02:00Z")))))

(defn- fixture
  ([rows edges writes] (fixture rows edges writes #{}))
  ([rows edges writes failed-ids] (fixture rows edges writes failed-ids #{}))
  ([rows edges writes failed-ids mismatch-ids]
   (let [chunks (atom [])
         gets (atom 0)
         paths (atom [])]
     {:request (fn [_ _ path _]
                 (if (str/starts-with? path "/api/alpha/hyperedges?")
                   (let [encoded (second (re-find #"[?&]end=([^&]+)" path))
                         endpoint (java.net.URLDecoder/decode encoded "UTF-8")]
                     (swap! paths conj path)
                     {:hyperedges
                      (->> (vals @edges)
                           (filter #(and (= :promise/history-event (:hx/type %))
                                         (some #{endpoint} (:hx/endpoints %))))
                           vec)})
                   {:entries rows}))
      :read (fn [hx-type _]
              (->> (vals @edges) (filter #(= hx-type (:hx/type %))) vec))
      :batch (fn [batch]
               (swap! chunks conj batch)
               {:ok (not-any? failed-ids (map :hx/id batch))
                :results
                (mapv (fn [edge]
                        (if (failed-ids (:hx/id edge))
                          {:ok false :hx/id (:hx/id edge) :error "injected"}
                          (let [stored (if (mismatch-ids (:hx/id edge))
                                         (assoc-in edge [:hx/props :history/transition-type]
                                                   :promise/mismatched)
                                         edge)]
                              (swap! writes conj edge)
                              (swap! edges assoc (:hx/id edge) stored)
                              {:ok true :hx/id (:hx/id edge)})))
                      batch)})
   :get (fn [id]
          (swap! gets inc)
          (if-let [edge (get @edges id)] edge
              (throw (ex-info "not found" {:status 404}))))
   :post (fn [edge]
           (swap! writes conj edge)
           (swap! edges assoc (:hx/id edge) edge)
           {:ok true :hx/id (:hx/id edge)})
      :chunks chunks
      :gets gets
      :paths paths})))

(defmacro with-store [f & body]
  `(binding [reconcile/*request-fn* (:request ~f)
             reconcile/*sleep-fn* (fn [_#])
             reconcile/*read-hyperedges-fn* (:read ~f)
             reconcile/*batch-post-fn* (:batch ~f)
             promise-log/*get-fn* (:get ~f)
             promise-log/*post-fn* (:post ~f)]
     ~@body))

(deftest existing-state-ahead-is-never-rewound
  (let [rows (chain "ahead" false)
        edges (atom {(promise-state/state-id "ahead")
                     {:hx/id (promise-state/state-id "ahead")
                      :hx/type :promise/state
                      :hx/props {:promise-id "ahead" :head-sequence 9}}})
        writes (atom [])
        f (fixture rows edges writes)]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0})]
        (is (= 1 (:already-current report)))
        (is (not-any? #(= :promise/state (:hx/type %)) @writes))
        (is (= 9 (get-in @edges [(promise-state/state-id "ahead")
                                 :hx/props :head-sequence])))))))

(deftest incomplete-chain-writes-no-state-and-is-reported
  (let [rows [(second (chain "gap" false))]
        edges (atom {}) writes (atom []) f (fixture rows edges writes)]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0})]
        (is (= {:missing-origin 1} (:incomplete-by-reason report)))
        (is (empty? @writes))))))

(deftest closed-writes-state-but-no-history-copy
  (let [rows (chain "closed" true)
        edges (atom {}) writes (atom []) f (fixture rows edges writes)]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0})]
        (is (= 1 (:states-written report)))
        (is (= 0 (:history-edges-written report)))
        (is (= [:promise/state] (mapv :hx/type @writes)))))))

(deftest all-history-copies-closed-chain-with-per-promise-readback-and-converges
  (let [rows (chain "closed-all" true)
        edges (atom {}) writes (atom []) f (fixture rows edges writes)]
    (with-store f
      (is (= 0 (:history-edges-written
                (reconcile/run! {:page-sleep-ms 0 :dry-run? true}))))
      (is (= 3 (:history-edges-written
                (reconcile/run! {:page-sleep-ms 0 :dry-run? true
                                 :all-history? true}))))
      (let [report (reconcile/run! {:page-sleep-ms 0 :all-history? true})
            readback-paths @(:paths f)]
        (is (= 3 (:history-edges-written report)))
        (is (= 1 (count readback-paths)))
        (is (every? #(and (str/includes? % "type=promise%2Fhistory-event")
                          (str/includes? % "end=closed-all"))
                    readback-paths)))
      (reset! writes [])
      (reset! (:paths f) [])
      (let [rerun (reconcile/run! {:page-sleep-ms 0 :all-history? true})]
        (is (zero? (:history-edges-written rerun)))
        (is (empty? @writes))
        (is (empty? @(:paths f)))))))

(deftest all-history-readback-mismatch-is-reported
  (let [rows (chain "mismatch" true)
        bad-id (:hx/id (promise-log/entry->hyperedge (second rows)))
        edges (atom {}) writes (atom [])
        f (fixture rows edges writes #{} #{bad-id})]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0 :all-history? true})]
        (is (= 1 (:failed-ids-count report)))
        (is (= [bad-id] (:failed-ids report)))
        (is (= 1 (count @(:paths f))))))))

(deftest all-history-full-cursorless-readback-refuses
  (let [rows (chain "readback-overflow" true)
        edges (atom {}) writes (atom []) f (fixture rows edges writes)
        ordinary-request (:request f)]
    (with-store f
      (binding [reconcile/*request-fn*
                (fn [base method path body]
                  (if (str/starts-with? path "/api/alpha/hyperedges?")
                    {:hyperedges
                     (vec (repeat 1000
                                  (promise-log/entry->hyperedge (first rows))))}
                    (ordinary-request base method path body)))]
        ;; A refused readback fails the chunk's edges; the run still finishes.
        (let [report (reconcile/run! {:page-sleep-ms 0 :all-history? true})]
          (is (= 3 (:history-edges-written report)))
          (is (= 3 (:failed-ids-count report))))))))

(deftest history-readback-error-fails-the-chunk-not-the-run
  (let [rows (concat (chain "a" true) (chain "b" true))
        edges (atom {}) writes (atom []) f (fixture rows edges writes)
        ordinary-request (:request f)]
    (with-store f
      (binding [reconcile/*request-fn*
                (fn [base method path body]
                  (if (str/starts-with? path "/api/alpha/hyperedges?")
                    (throw (ex-info "futon1b busy (HTTP 503)" {:status 503}))
                    (ordinary-request base method path body)))]
        (let [report (reconcile/run! {:page-sleep-ms 0 :all-history? true})]
          (is (= 6 (:history-edges-written report)))
          (is (= 6 (:failed-ids-count report))))))))

(deftest open-promise-copies-missing-pre-cutover-history
  (let [rows (chain "open" false)
        edges (atom {}) writes (atom []) f (fixture rows edges writes)]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0})]
        (is (= 1 (:states-written report)))
        (is (= 2 (:history-edges-written report)))
        (is (= {:promise/state 1 :promise/history-event 2}
               (frequencies (map :hx/type @writes))))))))

(deftest dry-run-computes-report-without-writing-or-clearing-deferred
  (let [rows (chain "dry" false)
        edges (atom {}) writes (atom []) f (fixture rows edges writes)
        saved @promise-state/!deferred]
    (try
      (swap! promise-state/!deferred assoc "dry" {:reason :missing-origin})
      (with-store f
        (let [report (reconcile/run! {:page-sleep-ms 0 :dry-run? true})]
          (is (= 1 (:states-written report)))
          (is (= 2 (:history-edges-written report)))
          (is (empty? @writes))
          (is (contains? @promise-state/!deferred "dry"))))
      (finally (reset! promise-state/!deferred saved)))))

(deftest page-cap-refuses-partial-history
  (binding [reconcile/*request-fn*
            (fn [_ _ _ _]
              {:entries [] :next-cursor {:at "2026-11-05T17:00:00Z" :id "next"}})
            reconcile/*sleep-fn* (fn [_])]
    (is (= :truncated-input
           (try (reconcile/read-evidence-history {:max-pages 1 :page-sleep-ms 0}) nil
                (catch clojure.lang.ExceptionInfo error
                  (:reason (ex-data error))))))))

(defn- many-state-rows [n]
  (->> (range n)
       (mapcat (fn [index]
                 (map-indexed
                  (fn [offset history-row]
                    (assoc history-row :evidence/at
                           (format "2026-11-05T18:%02d:00Z" offset)))
                  (chain (format "bulk-%04d" index) (< index 600)))))
       vec))

(defn- history-edge-map [rows]
  (into {} (map (fn [history-row]
                  (let [edge (promise-log/entry->hyperedge history-row)]
                    [(:hx/id edge) edge])))
        rows))

(deftest bulk-state-catch-up-uses-three-open-first-chunks-and-no-point-gets
  (let [rows (many-state-rows 1200)
        edges (atom (history-edge-map rows))
        writes (atom []) f (fixture rows edges writes)]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0})
            chunks @(:chunks f)]
        (is (= [500 500 200] (mapv count chunks)))
        (is (every? #(some #{"promise-status:open"} (:hx/endpoints %))
                    (first chunks)))
        (is (= 3 (:chunks-sent report)))
        (is (= 1200 (:states-written report)))
        (is (zero? @(:gets f)))))))

(deftest one-batch-payload-failure-is-reported-and-later-chunks-run
  (let [rows (many-state-rows 1200)
        failed-id (promise-state/state-id "bulk-0600")
        edges (atom (history-edge-map rows)) writes (atom [])
        f (fixture rows edges writes #{failed-id})]
    (with-store f
      (let [report (reconcile/run! {:page-sleep-ms 0})]
        (is (= [500 500 200] (mapv count @(:chunks f))))
        (is (= 1 (:failed-ids-count report)))
        (is (= [failed-id] (:failed-ids report)))
        (is (= 1199 (count @writes)))))))

(deftest stale-details-and-state-shape-are-rewritten-then-left-current
  (let [base (mapv #(assoc % :evidence/at "2026-11-05T18:00:00Z")
                   (chain "rewrite" false))
        rows (update base 1 assoc-in [:evidence/body :wake/reason] :dependency-ready)
        folded (promise-state/fold-state rows)
        expected-state (promise-state/state->hyperedge folded)
        expected-history (mapv promise-log/entry->hyperedge rows)
        stale-state (update expected-state :hx/props dissoc :checkable?)
        stale-second (update (second expected-history) :hx/props
                             dissoc :history/details-edn)
        edges (atom (assoc (history-edge-map rows)
                           (:hx/id stale-state) stale-state
                           (:hx/id stale-second) stale-second))
        writes (atom []) f (fixture rows edges writes)]
    (with-store f
      (let [first-report (reconcile/run! {:page-sleep-ms 0})]
        (is (= 1 (:states-written first-report)))
        (is (= 1 (:history-edges-written first-report)))
        (is (= #{(:hx/id expected-state) (:hx/id stale-second)}
               (set (map :hx/id @writes))))
        (reset! writes [])
        (reset! (:chunks f) [])
        (let [second-report (reconcile/run! {:page-sleep-ms 0})]
          (is (= 1 (:already-current second-report)))
          (is (zero? (:states-written second-report)))
          (is (zero? (:history-edges-written second-report)))
          (is (empty? @writes)))))))

(deftest edge-prefetch-overflow-refuses-instead-of-using-a-partial-map
  (let [rows (chain "overflow" false)
        edges (atom {}) writes (atom []) f (fixture rows edges writes)]
    (binding [reconcile/*request-fn* (:request f)
              reconcile/*sleep-fn* (fn [_])
              reconcile/*read-hyperedges-fn*
              (fn [hx-type _]
                (if (= :promise/state hx-type)
                  (with-meta [] {:partial? true :next-cursor "more"})
                  []))]
      (is (= {:reason :truncated-input :source :promise/state}
             (try (reconcile/run! {:page-sleep-ms 0}) nil
                  (catch clojure.lang.ExceptionInfo error
                    (select-keys (ex-data error) [:reason :source]))))))))
