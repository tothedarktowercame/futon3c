(ns futon3c.agency.promise-reconcile-test
  (:require [clojure.test :refer [deftest is]]
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

(defn- fixture [rows edges writes]
  {:request (fn [_ _ _ _] {:entries rows})
   :get (fn [id]
          (if-let [edge (get @edges id)] edge
              (throw (ex-info "not found" {:status 404}))))
   :post (fn [edge]
           (swap! writes conj edge)
           (swap! edges assoc (:hx/id edge) edge)
           {:ok true :hx/id (:hx/id edge)})})

(defmacro with-store [f & body]
  `(binding [reconcile/*request-fn* (:request ~f)
             reconcile/*sleep-fn* (fn [_#])
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
