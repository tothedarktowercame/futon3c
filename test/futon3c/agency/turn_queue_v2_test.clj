(ns futon3c.agency.turn-queue-v2-test
  "Drainer v2: dedicated per-agent drainer thread + accept-async!/accept-block!.
   Companion to drainer-model-test (which validated the design); these exercise the
   real implementation against the same invariants: no-drop, FIFO, finalize-once,
   non-blocking accept, and load-dark defaults."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.turn-queue :as turn-queue])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-temp-v2 [f]
  (let [dir (doto (java.io.File. "target/test-queues")
              (.mkdirs))
        file (java.io.File/createTempFile "futon3c-drainer-v2-" ".edn" dir)
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

(deftest v2-defaults-on
  (testing "drainer v2 defaults on unless explicitly disabled"
    (is (true? (turn-queue/drainer-v2-enabled?)))
    (System/setProperty "FUTON3C_DRAINER_V2" "false")
    (try
      (is (false? (turn-queue/drainer-v2-enabled?)))
      (finally
        (System/clearProperty "FUTON3C_DRAINER_V2"))))
  (testing "dynamic bypass defaults off"
    (is (false? turn-queue/*drained-by-outer*))))

(deftest dedicated-drainer-fifo-no-drop-and-finalizes
  (with-temp-v2
    (fn []
      (let [seen (atom [])
            results (atom [])
            done (CountDownLatch. 3)
            mk (fn [n]
                 {:to "agent-x" :from "joe" :surface "bell" :msg-id (str "m" n)
                  :process-fn (fn [_] (swap! seen conj n) {:result (str "r" n)})
                  :finalize-fn (fn [res] (swap! results conj (:result res)) (.countDown done))})]
        (doseq [n [1 2 3]] (turn-queue/accept-async! (mk n)))
        (is (.await done 5 TimeUnit/SECONDS) "all three drained within timeout")
        (is (= [1 2 3] @seen) "single dedicated drainer preserves FIFO")
        (is (= #{"r1" "r2" "r3"} (set @results)) "every turn finalized exactly once with its result")))))

(deftest per-agent-drainer-serializes-concurrent-turns-no-overlap
  ;; I-1 — "single identity is sequential execution" (incident 2026-06-26). Even when
  ;; several turns for ONE agent are accepted back-to-back, at most one is ever
  ;; in-flight: the dedicated drainer runs them one at a time, never overlapping.
  (with-temp-v2
    (fn []
      (let [in-flight (atom 0)
            max-in-flight (atom 0)
            done (CountDownLatch. 4)
            mk (fn [n]
                 {:to "solo-agent" :from "joe" :surface "bell" :msg-id (str "s" n)
                  :process-fn (fn [_]
                                (let [c (swap! in-flight inc)]
                                  (swap! max-in-flight max c)
                                  (Thread/sleep 40)
                                  (swap! in-flight dec)
                                  {:result n}))
                  :finalize-fn (fn [_] (.countDown done))})]
        (doseq [n [1 2 3 4]] (turn-queue/accept-async! (mk n)))
        (is (.await done 5 TimeUnit/SECONDS) "all four turns drained")
        (is (= 1 @max-in-flight)
            "one identity ⇒ at most one in-flight turn at a time (sequential execution)")))))

(deftest accept-async-returns-without-blocking
  (with-temp-v2
    (fn []
      (let [gate (promise)
            started (promise)
            r (turn-queue/accept-async!
               {:to "agent-y" :from "joe" :surface "bell" :msg-id "g1"
                :process-fn (fn [_] (deliver started true) @gate {:result "ok"})})]
        (is (= :queued (:status r)) "accept-async! returns the accept result immediately")
        (is (true? (deref started 3000 false)) "processing began on the drainer thread, not the caller")
        (deliver gate true)))))                 ;; release so the drainer thread can finish

(deftest finalizer-is-installed-before-queue-publication
  ;; Regression for the intermittent complete-content-then-[interrupted]
  ;; incident. Force the exact load-bearing branch: accept! observes the
  ;; finalizer map synchronously, before accept-async! can return.
  (with-temp-v2
    (fn []
      (let [installed? (atom false)
            finalizers (var-get (ns-resolve 'futon3c.agency.turn-queue
                                            '!finalizers))]
        (with-redefs [turn-queue/accept!
                      (fn [entry]
                        (reset! installed? (contains? @finalizers (:id entry)))
                        {:status :deduped :entry entry})]
          (turn-queue/accept-async!
           {:id "race-turn"
            :to "agent-race"
            :from "joe"
            :surface "repl"
            :process-fn (fn [_] {:result "fast"})
            :finalize-fn identity}))
        (is (true? @installed?)
            "a drainer can never observe a published entry without its finalizer")))))

(deftest exact-turn-id-dedup-does-not-steal-original-finalizer
  (with-temp-v2
    (fn []
      (let [first-finalizer (fn [_] :first)
            finalizers (var-get (ns-resolve 'futon3c.agency.turn-queue
                                            '!finalizers))
            observed (atom nil)]
        (swap! finalizers assoc "same-turn" first-finalizer)
        (with-redefs [turn-queue/accept!
                      (fn [entry]
                        (reset! observed (get @finalizers (:id entry)))
                        {:status :deduped :entry entry})]
          (turn-queue/accept-async!
           {:id "same-turn" :to "agent-race" :process-fn identity
            :finalize-fn (fn [_] :retry)}))
        (is (identical? first-finalizer @observed))
        (is (identical? first-finalizer (get @finalizers "same-turn"))
            "an exact-id retry cannot replace or remove the original channel owner")))))

(deftest exact-turn-id-dedup-does-not-steal-waiter-or-processor
  (with-temp-v2
    (fn []
      (let [original-process (fn [_] {:result "original"})
            accepted (turn-queue/accept!
                      {:id "same-queue-id" :msg-id "same-msg"
                       :to "agent-id-owner" :process-fn original-process})
            waiters (var-get (ns-resolve 'futon3c.agency.turn-queue '!waiters))
            processors (var-get (ns-resolve 'futon3c.agency.turn-queue '!processors))
            original-waiter (get @waiters "same-queue-id")
            duplicate (turn-queue/accept!
                       {:id "same-queue-id" :msg-id "same-msg"
                        :to "agent-id-owner" :process-fn (constantly {:result "retry"})})]
        (is (= :queued (:status accepted)))
        (is (= :deduped (:status duplicate)))
        (is (identical? original-waiter (get @waiters "same-queue-id")))
        (is (identical? original-process (get @processors "same-queue-id"))
            "the original accepted turn remains the sole owner of its execution")))))

(deftest accept-block-returns-terminal-result
  (with-temp-v2
    (fn []
      (let [res (turn-queue/accept-block!
                 {:to "agent-z" :from "joe" :surface "bell" :msg-id "b1"
                  :process-fn (fn [_] {:result "blocked-ok" :session-id "s"})})]
        (is (= "blocked-ok" (:result res)) "accept-block! returns the terminal invoke result")))))

(deftest msg-id-dedup-still-holds-under-v2
  (with-temp-v2
    (fn []
      (let [calls (atom 0)
            mk (fn [] {:to "agent-d" :from "joe" :surface "bell" :msg-id "dup"
                       :process-fn (fn [_] (swap! calls inc) {:result "ok"})})
            r1 (turn-queue/accept-async! (mk))
            r2 (turn-queue/accept-async! (mk))]
        (Thread/sleep 300)
        (is (= 1 @calls) "duplicate msg-id is not processed twice")
        (is (= :queued (:status r1)))
        (is (= :deduped (:status r2)))))))

(deftest idle-drainer-does-not-enter-durable-drain-loop
  (let [drain-calls (atom 0)
        {:keys [running lock thread]}
        (with-redefs [turn-queue/drain! (fn [& _] (swap! drain-calls inc))]
          (#'turn-queue/spawn-drainer! "idle-test"))]
    (try
      (Thread/sleep 1200)
      (is (zero? @drain-calls)
          "an empty queue must not wake into durable acquire/release writes")
      (finally
        (reset! running false)
        (locking lock (.notifyAll lock))
        (.interrupt ^Thread thread)
        (.join ^Thread thread 2000)))))
