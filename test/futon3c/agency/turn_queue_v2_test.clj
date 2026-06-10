(ns futon3c.agency.turn-queue-v2-test
  "Drainer v2: dedicated per-agent drainer thread + accept-async!/accept-block!.
   Companion to drainer-model-test (which validated the design); these exercise the
   real implementation against the same invariants: no-drop, FIFO, finalize-once,
   non-blocking accept, and load-dark defaults."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.turn-queue :as turn-queue])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-temp-v2 [f]
  (let [path (.getPath (java.io.File/createTempFile "futon3c-drainer-v2-" ".edn"))]
    (with-redefs [turn-queue/queue-store-path (constantly path)]
      (turn-queue/clear!)
      (turn-queue/stop-all-drainers!)
      (try
        (f)
        (finally
          (turn-queue/stop-all-drainers!)
          (turn-queue/clear!)
          (.delete (java.io.File. path)))))))

(deftest v2-defaults-off
  (testing "load-dark: flag and dynamic bypass default off"
    (is (false? (turn-queue/drainer-v2-enabled?)))
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
