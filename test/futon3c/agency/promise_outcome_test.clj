(ns futon3c.agency.promise-outcome-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.promise-outcome :as outcome]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.promise-log :as promise-log]
            [futon3c.agency.promise-replay :as replay]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.followup-queue :as queue]
            [futon3c.agency.history-constraints :as constraints]))

(def ^:dynamic *store* nil)
(def ^:dynamic *jobs* nil)
(def ^:dynamic *request-paths* nil)
(def ^:dynamic *hyperedges* nil)
(def request {:agent "p5-test" :session "p5-test" :awaiting ["wake-dep"]
              :beneficiary "joe" :deadline "2020-01-01T00:00:00Z"
              :fulfilment-criterion {:kind :job-terminal-ok :job-id "criterion-job"
                                     :machine-evaluable? true}})

(defn- query-param [path key]
  (when-let [value (some #(let [[k v] (str/split % #"=" 2)] (when (= key k) v))
                         (str/split (second (str/split path #"\?" 2)) #"&"))]
    (java.net.URLDecoder/decode value "UTF-8")))

(defn- creation-row? [entry]
  (and (= 1 (get-in entry [:evidence/body :history/promise-sequence]))
       (= 3 (get-in entry [:evidence/body :history/format]))))

(defn- fake-operational-request [_url _method path _body]
  (swap! *request-paths* conj path)
  (let [type (query-param path "type")
        endpoint (query-param path "end")
        entries (vals (:entries @*store*))
        history (filter #(= 3 (get-in % [:evidence/body :history/format])) entries)]
    (cond
      (= "promise/state" type)
      {:hyperedges
       (->> history
            (filter creation-row?)
            (keep (fn [entry]
                    (let [rec (:record (edn/read-string
                                        (get-in entry [:evidence/body :history/payload-edn])))
                          pid (or (:id rec) (:followup-id rec))]
                      (when (or (:fulfilment-criterion rec) (:deadline rec))
                        {:hx/id (str "promise-state:" pid)
                         :hx/type :promise/state
                         :hx/endpoints [pid "promise-obligation:checkable"]
                         :hx/props {:promise-id pid}}))))
            vec)}

      (= "promise/history-event" type)
      {:hyperedges
       (->> history
            (filter #(= endpoint (get-in % [:evidence/body :history/promise-id])))
            (mapv promise-log/entry->hyperedge))}

      :else {:hyperedges []})))

(use-fixtures :each
  (fn [f]
    (let [p (java.io.File/createTempFile "p5-park" ".edn")
          q (java.io.File/createTempFile "p5-followup" ".edn")
          store (atom {:entries {} :order []}) jobs (atom {}) paths (atom [])
          hyperedges (atom {})]
      (with-redefs-fn {#'park/store-path (constantly (str p))}
        #(binding [*store* store *jobs* jobs *request-paths* paths *hyperedges* hyperedges
                   history/*backend* store history/*heads* (atom {})
                   promise-log/*post-fn* (fn [edge]
                                           (swap! hyperedges assoc (:hx/id edge) edge)
                                           {:ok true :hx/id (:hx/id edge)})
                   promise-log/*get-fn* (fn [id] (get @hyperedges id))
                   queue/*path-override* (str q) outcome/*request!* fake-operational-request
                   outcome/*lookup-job* (fn [id] (get @jobs id))]
           (try (park/clear!) (queue/clear!) (history/await-writes! 5000)
                (reset! store {:entries {} :order []})
                (f)
                (finally (history/await-writes! 5000) (.delete p) (.delete q)
                         (reset! @#'park/!parked nil) (reset! @#'queue/!state nil))))))))
(defn rows [] (is (history/await-writes! 5000)) (vals (:entries @*store*)))
(defn outcomes [] (filter #(outcome/types (:evidence/type %)) (rows)))
(defn checks [] (filter #(= outcome/check-type (:evidence/type %)) (rows)))

(deftest successful-criterion-writes-once-and-keeps-wake-separate
  (let [id (:id (park/park! request {})) wakes (atom 0)]
    (history/await-writes! 5000)
    (swap! *jobs* assoc "criterion-job" {:state "done" :finished-at "2019-12-31T23:59:00Z"})
    (park/note-completion! "wake-dep" {:ok true} {:resume! (fn [_] (swap! wakes inc))})
    (let [r (first (outcomes))]
      (is (= 1 @wakes))
      (is (= 1 (count (outcomes))))
      (is (= :promise/fulfilled (:evidence/type r)))
      (is (= id (get-in r [:evidence/body :promise-id])))
      (is (= "done" (get-in r [:evidence/body :job-observation :state])))
      (is (contains? (:entries @*store*) (get-in r [:evidence/body :source-evidence-id])))
      (is (empty? (:records (park/snapshot))))
      ;; Restart: a fresh backend instance sharing only the persisted entries,
      ;; with no outcome process flags or park cache, must not duplicate the outcome.
      (let [restarted (atom @*store*)]
        (outcome/sweep! restarted (System/currentTimeMillis))
        (outcome/sweep! restarted (System/currentTimeMillis))
        (is (= 1 (count (filter #(outcome/types (:evidence/type %)) (vals (:entries @restarted)))))))
    (is (empty? (:incomplete-promises (constraints/normalize-history (rows)))))
    (is (:equal? (replay/compare-state (rows) {:parked (park/snapshot) :followup (queue/snapshot)}))))))

(deftest bad-case-wake-does-not-fulfil-a-failed-different-criterion
  (let [req (assoc request :deadline "2099-01-01T00:00:00Z")
        id (:id (park/park! req {})) wakes (atom 0)]
    (swap! *jobs* assoc "criterion-job" {:state "failed" :terminal-code "invoke-error"})
    (park/note-completion! "wake-dep" {:ok true} {:resume! (fn [_] (swap! wakes inc))})
    (is (= 1 @wakes))
    (is (empty? (outcomes)))
    (is (some #(= :promise/woken (:evidence/type %)) (rows)))
    (is (empty? (:records (park/snapshot))))
    ;; Sweep after the deadline still finds the released promise in retained history.
    (outcome/sweep! *store* 4102444800001)
    (let [out (outcomes)]
      (is (= [:promise/lapsed] (mapv :evidence/type out)))
      (is (= id (get-in (first out) [:evidence/body :promise-id])))
      (is (= "failed" (get-in (first out) [:evidence/body :job-observation :state])))
      (is (= :unfulfilled (get-in (first (checks)) [:evidence/body :verdict]))))))

(deftest prose-and-missing-job-are-never-automatic-outcomes
  (park/park! (assoc request :awaiting [] :fulfilment-criterion {:kind :prose :text "be helpful"})
              {:resume! (fn [_])})
  (park/park! (assoc request :awaiting []) {:resume! (fn [_])})
  (history/await-writes! 5000)
  (outcome/sweep! *store* (System/currentTimeMillis))
  (is (empty? (outcomes))))

(deftest followup-criterion-is-evaluated-and-deadline-sweeps-survive-restart
  (let [req (merge (dissoc request :awaiting) {:type :inbox-zero :dedupe-key "p5" :prompt "test"})
        id (:id (queue/enqueue! req))]
    (history/await-writes! 5000)
    (swap! *jobs* assoc "criterion-job" {:state "failed"})
    (queue/lease-one! "p5-test" "p5-test" (constantly true))
    (queue/ack! id)
    (is (= [:promise/lapsed] (mapv :evidence/type (outcomes))))
    (let [fresh (atom @*store*)]
      (outcome/sweep! fresh (System/currentTimeMillis))
      (is (= 1 (count (filter #(outcome/types (:evidence/type %)) (vals (:entries @fresh)))))))))

(deftest late-success-retains-lapse-and-fulfilment
  (let [r (assoc request :id "test" :fulfilment-criterion
                 {:kind :job-terminal-ok :machine-evaluable? true :job-id "criterion-job"})]
    (is (= [:promise/lapsed :promise/fulfilled]
           (outcome/decide r {:state "done" :finished-at "2020-01-02T00:00:00Z"} 1578009600001)))))

(deftest deadline-checks-use-the-same-observation-as-p5
  (let [base (assoc request :id "deadline-check")
        deadline-ms (.toEpochMilli (java.time.Instant/parse (:deadline base)))]
    (binding [outcome/*lookup-job* (constantly {:state "done" :finished-at "2019-12-31T23:59:59Z"})]
      (outcome/evaluate-due! *store* "source:on-time" base deadline-ms))
    (is (= :fulfilled (get-in (first (checks)) [:evidence/body :verdict])))
    (reset! *store* {:entries {} :order []})
    (binding [outcome/*lookup-job* (constantly {:state "done" :finished-at "2020-01-02T00:00:00Z"})]
      (outcome/evaluate-due! *store* "source:late" base (inc deadline-ms)))
    (is (= :unfulfilled (get-in (first (checks)) [:evidence/body :verdict])))
    (is (= #{:promise/lapsed :promise/fulfilled}
           (set (map :evidence/type (outcomes)))))))

(deftest prose-check-is-due-at-creation-or-its-deadline
  (let [now 10000
        prose {:id "prose" :agent "a" :parked-at-ms now
               :fulfilment-criterion {:kind :prose :text "review" :machine-evaluable? false}}]
    (outcome/evaluate-due! *store* "source:prose" prose now)
    (is (= :unsupported-criterion
           (get-in (first (checks)) [:evidence/body :unable-reason])))
    (reset! *store* {:entries {} :order []})
    (outcome/evaluate-due! *store* "source:future-prose"
                       (assoc prose :id "future" :deadline "2099-01-01T00:00:00Z") now)
    (is (empty? (checks)))))

(deftest unavailable-and-unusual-job-states-are-typed
  (let [deadline "2020-01-01T00:00:00Z"
        now (inc (.toEpochMilli (java.time.Instant/parse deadline)))
        rec (assoc request :id "typed" :deadline deadline)]
    (doseq [[job reason] [[nil :job-not-found]
                          [{:state "deduped"} :job-deduped]
                          [{:state "banana"} :job-state-unknown]]]
      (reset! *store* {:entries {} :order []})
      (binding [outcome/*lookup-job* (constantly job)]
        (outcome/evaluate-due! *store* (str "source:" (name reason)) rec now))
      (is (= reason (get-in (first (checks)) [:evidence/body :unable-reason]))))
    ;; Unknown states do not create a no-deadline check merely by being observed.
    (reset! *store* {:entries {} :order []})
    (binding [outcome/*lookup-job* (constantly {:state "banana"})]
      (outcome/evaluate-due! *store* "source:banana-pending" (dissoc rec :deadline) now))
    (is (empty? (checks)))))

(deftest no-deadline-job-is-due-on-known-terminal-state
  (let [rec (-> request (assoc :id "natural-due") (dissoc :deadline))]
    (binding [outcome/*lookup-job* (constantly {:state "running"})]
      (outcome/evaluate-due! *store* "source:running" rec 1000))
    (is (empty? (checks)))
    (binding [outcome/*lookup-job* (constantly {:state "failed" :terminal-code "x"})]
      (outcome/evaluate-due! *store* "source:running" rec 2000))
    (is (= :unfulfilled (get-in (first (checks)) [:evidence/body :verdict])))))

(deftest job-read-failure-retries-for-one-hour
  (let [deadline-ms (.toEpochMilli (java.time.Instant/parse "2020-01-01T00:00:00Z"))
        rec (assoc request :id "read-failure")]
    (binding [outcome/*lookup-job* (fn [_] (throw (ex-info "offline" {})))]
      (outcome/evaluate-due! *store* "source:read-failure" rec deadline-ms)
      (is (empty? (checks)))
      (outcome/evaluate-due! *store* "source:read-failure" rec (+ deadline-ms (* 61 60 1000))))
    (is (= :job-read-failed
           (get-in (first (checks)) [:evidence/body :unable-reason])))))

(deftest check-is-restart-idempotent-and-conflicts-fail
  (let [rec (-> request (assoc :id "restart") (dissoc :deadline))
        job {:state "failed" :finished-at "2020-01-01T00:00:00Z"}]
    (binding [outcome/*lookup-job* (constantly job)]
      (outcome/evaluate-due! *store* "source:restart" rec 1577836800000)
      (outcome/evaluate-due! *store* "source:restart" rec 1577836801000))
    (is (= 1 (count (checks))))
    (swap! *store* assoc-in [:entries (outcome/check-id "restart")
                             :evidence/body :criterion]
           {:kind :prose :text "different"})
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Conflicting promise fulfilment check"
         (binding [outcome/*lookup-job* (constantly job)]
           (outcome/evaluate-due! *store* "source:restart" rec 1577836802000))))))

(deftest check-validator-refuses-an-unknown-unable-reason
  (let [entry {:evidence/type outcome/check-type
               :evidence/body {:promise-id "p" :source-evidence-id "e"
                               :criterion {:kind :prose} :due-at "2020-01-01T00:00:00Z"
                               :checked-at "2020-01-01T00:00:00Z" :refs []
                               :verdict :unable-to-determine :unable-reason :guess}}]
    (is (= :invalid-unable-reason
           (try (outcome/validate-check! entry) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest pre-format-three-history-is-outside-the-state-projection
  (let [source "source:legacy"
        legacy {:evidence/id source :evidence/type :promise/park-made
                :evidence/at "2020-01-01T00:00:00Z"
                :evidence/tags [:promise-history]
                :evidence/body {:history/format 2 :id "legacy-promise" :agent "a"
                                :fulfilment-criterion {:kind :prose :text "inspect"}}}]
    (reset! *store* {:entries {source legacy} :order [source]})
    (outcome/sweep! *store* 1577836800000)
    (is (empty? (checks)))
    (is (every? #(not (str/includes? % "tags=promise-history")) @*request-paths*))))

(deftest format-three-due-sweep-is-idempotent-without-evidence-tag-reads
  (let [id (:id (park/park! request {}))]
    (is (history/await-writes! 5000))
    (swap! *jobs* assoc "criterion-job" {:state "failed" :finished-at "2020-01-02T00:00:00Z"})
    (let [before (:existing (outcome/stats))]
      (outcome/sweep! *store* 1578009600000)
      (outcome/sweep! *store* 1578009600000)
      (is (= 1 (count (checks))))
      (is (= id (get-in (first (checks)) [:evidence/body :promise-id])))
      (is (< before (:existing (outcome/stats)))))
    (is (every? #(not (str/includes? % "tags=promise-history")) @*request-paths*))))

(deftest format-three-not-yet-due-writes-no-check
  (park/park! (assoc request :deadline "2099-01-01T00:00:00Z") {})
  (is (history/await-writes! 5000))
  (outcome/sweep! *store* 1578009600000)
  (is (empty? (checks))))

(deftest full-cursorless-checkable-state-window-refuses-before-writing
  (let [paths (atom [])
        request! (fn [_ _ path _]
                   (swap! paths conj path)
                   {:hyperedges
                    (mapv (fn [n] {:hx/id (str "state:" n)
                                   :hx/type :promise/state
                                   :hx/props {:promise-id (str "promise:" n)}})
                          (range 1000))})
        error (try
                (binding [outcome/*request!* request!]
                  (outcome/sweep! *store* 1578009600000))
                nil
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :truncated-input (:reason (ex-data error))))
    (is (= :promise-states-checkable (:source (ex-data error))))
    (is (= 1 (count @paths)))
    (is (empty? (checks)))))
