(ns futon3c.agency.clock-busy-ingress-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.agency.clock-decision :as clock]
            [futon3c.agency.clock-store :as state]
            [futon3c.agency.registry :as registry]
            [futon3c.evidence.store :as store]
            [futon3c.social.coordination-ledger :as coordination]
            [futon3c.transport.http :as http]))

(def busy (ex-info "read not served" {:error/code :futon1b-read-unavailable}))
(def backend (atom {:entries {} :order []}))
(def decision {:agent-id "busy-caller" :session-id "session" :status :clocked
               :decision-id "retained-clock" :decided-at "2026-10-29T00:00:00Z"
               :clock {:mission-id "M-existing"}})
(def request {:caller "busy-caller" :agent-id "busy-worker" :surface "bell"
              :mode "brief" :prompt "work" :evidence-store backend})
(defn budget [opts f] ((requiring-resolve 'futon3c.agency.clock-decision/with-store-busy-budget) opts f))
(defn error-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(use-fixtures :each
  (fn [f]
    (binding [clock/*test-store* backend clock/*store-busy-waits-ms* [0 0]
              coordination/*test-evidence-store* backend
              http/*restore-clock-decisions?* false]
      (with-redefs-fn
        {#'bb/project-agents! (constantly nil)
         #'state/!sessions (atom {})
         #'registry/get-agent (fn [_] {:agent/id {:id/value "busy-caller"}
                                       :agent/session-id "session"})
         #'store/count* (fn [& _] 1)
         #'store/query* (fn [& _] [{:evidence/body decision}])
         #'coordination/record-invoke-edge! (fn [& _] nil)
         #'http/configured-invoke-ingress-controller (constantly nil)}
        f))))

(deftest synchronous-ingress-busy-is-503-without-job
  (let [created (atom 0) reads (atom 0)]
    (with-redefs-fn
      {#'store/count* (fn [& _] (swap! reads inc) (throw busy))
       #'http/create-invoke-job-ledger! (fn [_] (swap! created inc) "unexpected")
       ;; Route seam only: exercise the real pre-job boundary and outer HTTP
       ;; exception mapping without opening a work order or scheduling a turn.
       #'http/handle-bell (fn [& _] (#'http/create-invoke-job! request))}
      (fn []
        (let [response ((http/make-handler {}) {:request-method :post :uri "/api/alpha/bell"})
              body (json/parse-string (:body response) true)]
          (is (= 503 (:status response)))
          (is (= "clock-store-busy" (:error body)))
          (is (true? (:dispatch/not-created? body)))
          (is (= 1 @reads))
          (is (zero? @created))
          (is (nil? (state/stored-state "busy-caller" "session"))))))))

(deftest recovered-inheritance-is-kept-and-job-created-once
  (let [created (atom []) reads (atom 0)]
    (with-redefs-fn
      {#'store/count* (fn [& _] (if (= 1 (swap! reads inc)) (throw busy) 1))
       #'http/create-invoke-job-ledger! (fn [r] (swap! created conj r) "one-job")}
      (fn []
        (is (= :clock/store-busy (:error/code (ex-data (error-of #(#'http/create-invoke-job! request))))))
        (is (empty? @created))
        (is (= "one-job" (#'http/create-invoke-job! request)))
        (is (= 1 (count @created)))
        (is (= "M-existing" (get-in @created [0 :inherited-clock :clock :mission-id])))
        (is (= "retained-clock" (get-in @created [0 :inherited-clock :evidence :caller-decision-id])))))))

(deftest coordination-caller-inherits-read-recovery-and-exhaustion
  (doseq [recovers? [true false]]
    (with-redefs-fn {#'state/!sessions (atom {})}
      (fn []
        (let [reads (atom 0) calls (atom [])]
          (with-redefs [store/count* (fn [& _] (let [n (swap! reads inc)] (if (and recovers? (> n 1)) 1 (throw busy))))
                        registry/invoke-agent! (fn [& xs] (swap! calls conj xs) :invoked)]
            (let [run #(coordination/invoke-with-edge! {:from "busy-caller" :to "busy-worker"
                                                       :surface "bell" :prompt "work" :evidence-store backend})]
              (if recovers?
                (do (is (= :invoked (run)))
                    (is (= "M-existing" (get-in (last (first @calls)) [:inherited-clock :clock :mission-id])))
                    (is (= 1 (count @calls))))
                (do (is (= :clock/store-busy (:error/code (ex-data (error-of run)))))
                    (is (empty? @calls))
                    (is (= 3 @reads)))))))))))

(deftest nested-reads-share-budget-and-unrelated-errors-escape
  (let [reads (atom 0)]
    (with-redefs [store/count* (fn [& _] (swap! reads inc) (throw busy))]
      (is (= :clock/store-busy
             (:error/code (ex-data (error-of #(budget {:waits [0 0]}
                                            (fn [] (clock/restore! backend "busy-caller" "session"))))))))
      (is (= 3 @reads))))
  (let [other (ex-info "not busy" {:error/code :clock/recovery-invalid-decision})
        reads (atom 0)]
    (with-redefs [store/count* (fn [& _] (swap! reads inc) (throw other))]
      (is (identical? other (error-of #(clock/restore! backend "busy-caller" "session"))))
      (is (= 1 @reads)))))

(deftest exhausted-operation-deadline-does-not-start-a-read
  (let [reads (atom 0)]
    (with-redefs [store/count* (fn [& _] (swap! reads inc) 0)]
      (is (= :clock/store-busy
             (:error/code (ex-data (error-of #(budget {:timeout-ms 0}
                                            (fn [] (clock/restore! backend "busy-caller" "session"))))))))
      (is (zero? @reads)))))

(deftest deadline-after-slow-read-prevents-late-job
  (let [created (atom 0)]
    (with-redefs-fn
      {#'store/count* (fn [& _] (Thread/sleep 25) 1)
       #'http/create-invoke-job-ledger! (fn [_] (swap! created inc) "late-job")}
      (fn []
        (is (= :clock/store-busy
               (:error/code (ex-data (error-of #(budget {:waits [] :timeout-ms 5}
                                              (fn [] (#'http/create-invoke-job! request))))))))
        (is (zero? @created))))))

(deftest sequential-clock-reads-do-not-renew-retry-allowance
  (let [counts (atom 0) gets (atom 0)]
    (with-redefs [store/count* (fn [& _] (if (= 1 (swap! counts inc)) (throw busy) 0))
                  store/get-entry* (fn [& _] (swap! gets inc) (throw busy))]
      (is (= :clock/store-busy
             (:error/code (ex-data (error-of #(budget {:waits [0]}
                                             (fn [] (clock/record!
                                                      {:agent-id "busy-caller" :session-id "session"
                                                       :turn-id "one" :surface "bell"}))))))))
      (is (= 2 @counts))
      (is (= 1 @gets)))))

(deftest backend-read-timeout-is-capped-by-operation-deadline
  (let [seen (atom nil)]
    (with-redefs-fn
      {#'f1b/raw-get-edn (fn [_ ms _] (reset! seen ms) :read)}
      (fn []
        (binding [f1b/*read-deadline-ns* (+ (System/nanoTime) 1000000000)]
          (is (= :read (#'f1b/get-edn "http://unused" 30000)))
          (is (< 0 @seen 1001)))))))

(deftest synchronous-ingress-tightens-an-inherited-budget
  (let [reads (atom 0) created (atom 0)]
    (with-redefs-fn
      {#'store/count* (fn [& _] (swap! reads inc) (throw busy))
       #'http/create-invoke-job-ledger! (fn [_] (swap! created inc) "unexpected")}
      (fn []
        (is (= :clock/store-busy
               (:error/code (ex-data (error-of #(budget {:waits [0 0] :timeout-ms 600000}
                                              (fn [] (#'http/create-invoke-job! request))))))))
        (is (= 1 @reads))
        (is (zero? @created))))))
