(ns futon3c.agency.turn-queue-hold-test
  "Operator hold on a per-agent turn queue: pause the queue BETWEEN turns
   without disturbing the turn already in flight, keep accepting bells behind
   the hold, and resume them in order on release."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.turn-queue :as turn-queue])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-temp-queue [f]
  (let [dir (doto (java.io.File. "target/test-queues")
              (.mkdirs))
        file (java.io.File/createTempFile "futon3c-hold-" ".edn" dir)
        path (.getPath file)]
    (with-redefs [turn-queue/queue-store-path (constantly path)]
      (turn-queue/clear!)
      (turn-queue/stop-all-drainers!)
      (try
        (f)
        (finally
          (turn-queue/stop-all-drainers!)
          (turn-queue/clear!)
          (.delete file))))))

(deftest hold-lets-the-in-flight-turn-finish-and-stops-the-next
  ;; The load-bearing property: Joe holds the queue WHILE a turn is running.
  ;; That turn must complete and finalize normally; only the turns behind it
  ;; wait.
  (with-temp-queue
    (fn []
      (let [aid "held-agent"
            started (promise)
            gate (promise)
            processed (atom [])
            first-done (CountDownLatch. 1)
            mk (fn [n latch]
                 {:to aid :from "joe" :surface "bell" :msg-id (str "h" n)
                  :process-fn (fn [_]
                                (when (= n 1)
                                  (deliver started true)
                                  @gate)
                                (swap! processed conj n)
                                {:result (str "r" n)})
                  :finalize-fn (fn [_] (when latch (.countDown latch)))})]
        (turn-queue/accept-async! (mk 1 first-done))
        (is (true? (deref started 3000 false)) "turn 1 is in flight")

        (turn-queue/hold! aid {:reason "having a word" :by "joe"})
        (is (true? (turn-queue/held? aid)))

        ;; Turns queued behind the hold are accepted, not rejected.
        (turn-queue/accept-async! (mk 2 nil))
        (turn-queue/accept-async! (mk 3 nil))

        (deliver gate true)
        (is (.await first-done 5 TimeUnit/SECONDS)
            "the turn already in flight finishes and finalizes despite the hold")
        (Thread/sleep 1200)
        (is (= [1] @processed) "nothing new is popped while held")
        (is (= 2 (count (get-in (turn-queue/snapshot) [:queues aid])))
            "both later bells are still queued, in order, none dropped")

        (turn-queue/release! aid)
        (is (false? (turn-queue/held? aid)))
        (let [deadline (+ (System/currentTimeMillis) 5000)]
          (while (and (< (count @processed) 3)
                      (< (System/currentTimeMillis) deadline))
            (Thread/sleep 50)))
        (is (= [1 2 3] @processed)
            "release resumes the backlog immediately and in FIFO order")))))

(deftest hold-expires-so-a-forgotten-hold-cannot-strand-a-queue
  (with-temp-queue
    (fn []
      (let [aid "expiring-agent"]
        (turn-queue/hold! aid {:ttl-ms 150})
        (is (true? (turn-queue/held? aid)))
        (Thread/sleep 250)
        (is (false? (turn-queue/held? aid)) "the hold auto-released after its TTL")
        (is (empty? (turn-queue/holds)) "expired holds are cleared from the view")))))

(deftest hold-is-per-agent
  (with-temp-queue
    (fn []
      (let [done (CountDownLatch. 1)]
        (turn-queue/hold! "quiet-agent" nil)
        (turn-queue/accept-async!
         {:to "busy-agent" :from "joe" :surface "bell" :msg-id "p1"
          :process-fn (constantly {:result "ok"})
          :finalize-fn (fn [_] (.countDown done))})
        (is (.await done 5 TimeUnit/SECONDS)
            "holding one agent does not stall any other agent's queue")))))

(deftest queue-view-shows-the-backlog-and-the-hold
  (with-temp-queue
    (fn []
      (let [aid "viewed-agent"]
        (turn-queue/hold! aid {:reason "word" :by "joe"})
        (doseq [n [1 2]]
          (turn-queue/accept-async!
           {:to aid :from "claude-3" :surface "bell" :msg-id (str "v" n)
            :prompt (str "bell body " n)
            :process-fn (constantly {:result "ok"})}))
        (let [view (turn-queue/queue-view)
              row (first (filter #(= aid (:agent-id %)) (:agents view)))]
          (is (= 2 (:pending row)) "pending depth is visible before the turns become jobs")
          (is (= "word" (get-in row [:held :reason])))
          (is (= [1 2] (mapv :seq (:queued row))) "queued turns are listed in FIFO order")
          (is (= "claude-3" (:from (first (:queued row)))) "caller is visible")
          (is (= "bell body 1" (:preview (first (:queued row))))
              "a preview of each waiting bell is visible")
          (testing "held agents are reported even when their queue is empty"
            (is (contains? (:held view) aid))))
        ;; Leave nothing draining behind for the next test.
        (turn-queue/release! aid)))))
