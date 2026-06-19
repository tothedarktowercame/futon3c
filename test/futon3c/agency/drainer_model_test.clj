(ns futon3c.agency.drainer-model-test
  "Logic-model BEFORE code for the drainer fix (M-agency-hardening 'drainer car').

   Two abstract reference models of the invoke->drain system:
   - OLD: accept-and-drain! parks a SHARED invoke-executor lane on @waiter whenever
     the target agent is already draining (the live bug: turns queued behind a slow
     agent each burn one of the 4 lanes).
   - NEW: a dedicated PER-AGENT drainer owns draining+finalization; a turn enqueues
     and the caller does not hold a shared lane while it waits.

   Invariants over adversarial interleavings (arrivals + drain ticks, freely ordered):
     I-no-drop       every accepted turn completes exactly once
     I-fifo          per agent, completion order == arrival (seq) order
     I-once          every turn finalized exactly once
     I-no-lane-burn  no QUEUED turn holds a shared invoke lane (OLD violates, NEW holds)
     I-one-drainer   per agent, drains are serial (encoded: per-agent completed seqs
                     are strictly increasing -- a concurrent double-drain would dup/reorder)

   Gate: NEW satisfies every invariant for every generated schedule (conforming
   witness => 0 violations); OLD reproduces the lane-burn violation on an adversarial
   trace (adversarial => caught). Implement the production drainer ONLY against NEW."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def ^:private init
  {:counter 0 :seqs {} :queues {} :draining #{}
   :parked #{} :max-parked 0 :completed [] :finalized {}})

;; ---- OLD design: arrival rides an invoke lane; parks if agent already draining ----
(defn- arrive-old [s a]
  (let [id (:counter s)
        sq (inc (get-in s [:seqs a] 0))
        already? (contains? (:draining s) a)            ;; someone is already draining a
        parked (cond-> (:parked s) already? (conj id))  ;; -> this lane PARKS on @waiter
        draining (cond-> (:draining s) (not already?) (conj a))] ;; else it becomes drainer
    (-> s
        (update :counter inc)
        (assoc-in [:seqs a] sq)
        (update-in [:queues a] (fnil conj []) {:id id :agent a :seq sq})
        (assoc :parked parked :draining draining)
        (update :max-parked max (count parked)))))

(defn- tick-old [s a]
  (if-let [t (first (get-in s [:queues a]))]
    (let [q' (vec (rest (get-in s [:queues a])))]
      (-> s
          (assoc-in [:queues a] q')
          (update :parked disj (:id t))          ;; completing t releases its parked lane
          (update :completed conj t)
          (update-in [:finalized (:id t)] (fnil inc 0))
          (cond-> (empty? q') (update :draining disj a))))
    s))

;; ---- NEW design: arrival only enqueues; a per-agent drainer thread does the work ----
(defn- arrive-new [s a]
  (let [id (:counter s)
        sq (inc (get-in s [:seqs a] 0))]
    (-> s
        (update :counter inc)
        (assoc-in [:seqs a] sq)
        (update-in [:queues a] (fnil conj []) {:id id :agent a :seq sq}))))
        ;; NB: never touches :parked / :max-parked -> no shared lane held while queued

(defn- tick-new [s a]
  (if-let [t (first (get-in s [:queues a]))]
    (-> s
        (update-in [:queues a] (comp vec rest))
        (update :completed conj t)
        (update-in [:finalized (:id t)] (fnil inc 0)))
    s))

(defn- run [arrive-fn tick-fn events]
  (reduce (fn [s [op a]] (case op :arrive (arrive-fn s a) :tick (tick-fn s a)))
          init events))

(defn- drain-all [tick-fn s agents]
  (loop [s s, guard 0]
    (if (and (some seq (vals (:queues s))) (< guard 100000))
      (recur (reduce tick-fn s agents) (inc guard))
      s)))

(defn- invariants [final]
  (let [completed (:completed final)
        ids (map :id completed)]
    {:no-drop (= (sort ids) (range (:counter final)))           ;; every id, exactly once
     :once    (every? #(= 1 %) (vals (:finalized final)))
     :fifo    (every? (fn [[_ ts]] (or (<= (count ts) 1)
                                       (apply < (map :seq ts)))) ;; strictly increasing
                      (group-by :agent completed))
     :max-parked (:max-parked final)}))

(def ^:private agents [:a :b :c])
(def ^:private gen-schedule
  (gen/vector (gen/tuple (gen/elements [:arrive :tick]) (gen/elements agents)) 0 60))

(defspec new-design-satisfies-all-invariants 400
  (prop/for-all [events gen-schedule]
    (let [final (drain-all tick-new (run arrive-new tick-new events) agents)
          inv (invariants final)]
      (and (:no-drop inv) (:once inv) (:fifo inv)
           (zero? (:max-parked inv))))))                        ;; I-no-lane-burn for NEW

(deftest old-design-reproduces-lane-burn
  (testing "4 arrivals for one busy agent before any drain -> 3 shared lanes parked (THE BUG)"
    (let [s (run arrive-old tick-old [[:arrive :a] [:arrive :a] [:arrive :a] [:arrive :a]])]
      (is (= 3 (:max-parked s)))
      (is (pos? (:max-parked s)) "I-no-lane-burn VIOLATED by old design"))))

(deftest new-design-no-lane-burn-and-correct-on-same-trace
  (let [events [[:arrive :a] [:arrive :a] [:arrive :a] [:arrive :a]]
        final (drain-all tick-new (run arrive-new tick-new events) [:a])
        inv (invariants final)]
    (is (zero? (:max-parked inv)) "NEW never parks a shared lane")
    (is (= [1 2 3 4] (map :seq (:completed final))) "FIFO + no-drop")
    (is (:once inv) "exactly-once finalize")))

(deftest old-design-still-correct-on-ordering
  (testing "old design is correct on no-drop/fifo/once -- only the lane-burn is wrong"
    (let [final (drain-all tick-old
                           (run arrive-old tick-old
                                [[:arrive :a] [:arrive :b] [:arrive :a] [:tick :a]
                                 [:arrive :a] [:tick :b] [:arrive :b]])
                           agents)
          inv (invariants final)]
      (is (:no-drop inv))
      (is (:once inv))
      (is (:fifo inv)))))
