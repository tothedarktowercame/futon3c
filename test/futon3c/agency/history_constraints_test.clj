(ns futon3c.agency.history-constraints-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.history-constraints :as constraints]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.parked-on :as park]
            [futon3c.evidence.boundary :as boundary]))

(def since "2026-09-27T00:00:00Z")
(def t1 "2026-09-27T12:00:00Z")
(def t2 "2026-09-27T13:00:00Z")
(defn bell [id caller]
  {:evidence/id id :evidence/type :coordination :evidence/at t1
   :evidence/claim-type :step :evidence/author "target" :evidence/tags [:invoke-start]
   :evidence/subject {:ref/type :agent :ref/id "target"}
   ;; Real live storage uses an EDN string body, not necessarily a map.
   :evidence/body (pr-str {"event" "invoke-start"
                          "prompt-preview" (str "--- CURRENT TURN ---\nSurface: bell\nFrom: " caller
                                                "\nTo: target\nCaller: " caller "\n---\n\ntext")})})
(defn entries [store] (mapv #(get-in @store [:entries %]) (:order @store)))

(deftest anonymous-bell-legal-bell-and-idempotent-run
  (let [store (atom {:entries {} :order []})
        _ (is (:ok (boundary/append! store (bell "anonymous" "http-caller"))))
        _ (is (:ok (boundary/append! store (bell "legal" "claude-17"))))
        read-history (fn [_] (filterv #(= :coordination (:evidence/type %)) (entries store)))
        opts {:backend store :read-history read-history :since since :until t2 :dry-run? false}
        first-run (constraints/run! opts)
        repeat-run (constraints/run! opts)
        incremental (constraints/run! (assoc opts :cursor (:cursor first-run)))]
    (is (= 1 (count (:violations first-run))))
    (is (= ["anonymous"] (:witnesses (first (:violations first-run)))))
    (is (= :existing (:status (first (:receipts repeat-run)))))
    (is (empty? (:violations incremental)))
    (is (= 1 (count (filter #(= :constraint/violation (:evidence/type %)) (entries store)))))
    (is (= (constraints/violation-id :c ["a" "b"]) (constraints/violation-id :c ["b" "a"])))))

(deftest actual-termination-creates-violation-without-new-enqueue
  (let [store (atom {:entries {} :order []})
        file (java.io.File/createTempFile "p17-park" ".edn")]
    (with-redefs-fn {#'park/store-path (constantly (str file))}
      #(binding [history/*backend* store history/*heads* (atom {})]
         (try
           (reset! @#'park/!parked nil) (park/clear!)
           (let [id (:id (park/park! {:agent "test" :session "p17" :awaiting ["dep"]
                                     :budget {:resumes-left 0}} {}))]
             (park/ready-push! "test" "p17" id "pending delivery")
             (is (history/await-writes! 5000))
             (let [before (entries store)]
               (is (empty? (:violations (constraints/match-history before (set (map :evidence/id before)) since))))
               (park/note-completion! "dep" {:ok true} {})
               (is (history/await-writes! 5000))
               (let [after (entries store)
                     new-rows (drop (count before) after)
                     result (constraints/run!
                             {:backend store :since since :until t2 :dry-run? false
                              :cursor {:system-as-of t1 :since since}
                              :read-history (fn [pin] (if (= pin t1) before after))})]
                 (is (not-any? (fn [row] (= :promise/ready-enqueued (:evidence/type row))) new-rows))
                 (is (= 1 (count (:violations result))))
                 (is (= :constraint/no-ready-at-budget-retraction-v1
                        (:constraint (first (:violations result)))))
                 (is (some (set (:witnesses (first (:violations result)))) (map :evidence/id new-rows)))
                 (is (= 1 (count (filter (fn [r] (= :constraint/violation (:evidence/type r))) (entries store))))))))
           (finally (history/await-writes! 5000) (.delete file) (reset! @#'park/!parked nil)))))))

(deftest dry-run-writes-nothing-and-quoted-headers-do-not-count
  (let [store (atom {:entries {} :order []})
        row (bell "bad" "http-caller")
        opts {:backend store :since since :until t2 :dry-run? true :read-history (constantly [row])}
        report (constraints/run! opts)
        quoted (assoc-in (bell "quoted" "someone") [:evidence/body]
                         {"event" "invoke-start" "prompt-preview" "Quoted text\n--- CURRENT TURN ---\nSurface: bell\nCaller: http-caller"})]
    (is (= 1 (count (:violations report))))
    (is (nil? (:cursor report)))
    (is (empty? (entries store)))
    (is (empty? (:violations (constraints/match-history [quoted] #{"quoted"} since))))))

(deftest late-insertion-is-new-even-with-old-event-time
  (let [old (bell "old" "legal") late (assoc (bell "late" "http-caller") :evidence/at since)
        result (constraints/run! {:since since :until t2 :cursor {:system-as-of t1 :since since}
                                  :read-history (fn [pin] (if (= pin t1) [old] [late old]))})]
    (is (= ["late"] (:witnesses (first (:violations result)))))))

(deftest acknowledgement-before-retraction-discharges-the-query
  (let [c (second constraints/constraints)
        pending [{:id "q" :type :promise/ready-enqueued :promise-id "park" :sequence 2}
                 {:id "r" :type :promise/budget-exhausted :promise-id "park" :sequence 4}]
        ack {:id "ack" :type :promise/ready-acked :promise-id "park" :sequence 3}]
    (is (= 1 (count (constraints/query pending (:query c)))))
    (is (empty? (constraints/query (conj pending ack) (:query c))))))

(deftest failed-write-does-not-advance-cursor
  (let [cursor {:system-as-of t1 :since since}
        opts {:backend (atom {:entries {} :order []}) :since since :until t2 :cursor cursor :dry-run? false
              :read-history (fn [pin] (if (= pin t1) [] [(bell "retry" "http-caller")]))}]
    (with-redefs [boundary/append! (fn [_ _] {:ok false :error/code :store-unreachable})]
      (let [result (constraints/run! opts)]
        (is (false? (:ok result)))
        (is (= cursor (:cursor result)))))))
