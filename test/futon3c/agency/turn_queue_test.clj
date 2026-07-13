(ns futon3c.agency.turn-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.turn-queue :as turn-queue]))

(defn- with-temp-queue [f]
  (let [dir (doto (java.io.File. "target/test-queues")
              (.mkdirs))
        file (java.io.File/createTempFile "futon3c-durable-turn-queue-" ".edn" dir)
        path (.getPath file)]
    (with-redefs [turn-queue/queue-store-path (constantly path)]
      (turn-queue/stop-all-drainers!)
      (turn-queue/clear!)
      (try
        (f)
        (finally
          (turn-queue/stop-all-drainers!)
          (turn-queue/clear!)
          (.delete file))))))

(defn- await! [p label]
  (let [value (deref p 5000 ::timeout)]
    (is (not= ::timeout value) (str label " timed out"))
    (when (instance? Throwable value)
      (throw value))
    value))

(defn- daemon-call [name f]
  (let [p (promise)
        thread (Thread.
                ^Runnable
                (fn []
                  (deliver p
                           (try
                             (f)
                             (catch Throwable t
                               t))))
                name)]
    (.setDaemon thread true)
    (.start thread)
    p))

(deftest ordered-drain-is-fifo-and-no-drop
  (with-temp-queue
    (fn []
      (let [started (promise)
            release (promise)
            seen (atom [])
            f1 (daemon-call
                "turn-queue-test-f1"
                #(turn-queue/accept-and-drain!
                  {:id "t1" :from "joe" :to "claude-4" :surface "emacs-repl" :msg-id "m1"}
                  (fn [entry]
                    (swap! seen conj (:id entry))
                    (deliver started true)
                    @release
                    {:result "one" :session-id "s1"})))
            _ (await! started "first queued turn start")
            f2 (daemon-call
                "turn-queue-test-f2"
                #(turn-queue/accept-and-drain!
                  {:id "t2" :from "claude-3" :to "claude-4" :surface "bell" :msg-id "m2"}
                  (fn [entry]
                    (swap! seen conj (:id entry))
                    {:result "two" :session-id "s2"})))]
        (is (= ["t1"] @seen))
        (deliver release true)
        (is (= "one" (:result (await! f1 "first queued turn result"))))
        (is (= "two" (:result (await! f2 "second queued turn result"))))
        (is (= ["t1" "t2"] @seen))
        (is (= 2 (get-in (turn-queue/snapshot) [:drained-frontier "claude-4"])))))))

(deftest reply-route-comes-from-entry
  (with-temp-queue
    (fn []
      (let [result (turn-queue/accept-and-drain!
                    {:id "route-1" :from "claude-6" :to "claude-4"
                     :surface "bell" :msg-id "route-msg"}
                    (fn [_] {:result "ok"}))]
        (is (= {:from "claude-6"
                :surface "bell"
                :msg-id "route-msg"
                :seq 1}
               (:turn-queue/reply-route result)))))))

(deftest msg-id-dedup-does-not-process-twice
  (with-temp-queue
    (fn []
      (let [calls (atom 0)
            process (fn [_] (swap! calls inc) {:result "ok"})
            first-result (turn-queue/accept-and-drain!
                          {:id "a" :from "joe" :to "claude-4" :surface "bell" :msg-id "same"}
                          process)
            second-result (turn-queue/accept-and-drain!
                           {:id "b" :from "joe" :to "claude-4" :surface "bell" :msg-id "same"}
                           process)]
        (is (= 1 @calls))
        (is (= :processed (:turn-queue/status first-result)))
        (is (= :deduped (:turn-queue/status second-result)))
        (is (= :deduped (get-in (turn-queue/snapshot) [:entries "b" :status])))))))

(deftest stale-frontier-is-not-delivered-as-processed
  (with-temp-queue
    (fn []
      (let [calls (atom 0)]
        (turn-queue/accept-and-drain!
         {:id "new" :from "c4" :to "agent" :surface "bell" :msg-id "m2"}
         (fn [_] (swap! calls inc) {:result "new"}))
        ;; Simulate a crossed late turn with seq below the already-drained frontier.
        (swap! @#'turn-queue/!queue
               (fn [state]
                 (-> state
                     (assoc-in [:entries "old"] {:id "old" :from "c3" :to "agent"
                                                 :surface "bell" :msg-id "m1"
                                                 :seq 0 :accepted-at "late"
                                                 :status :queued})
                     (assoc-in [:queues "agent"] ["old"]))))
        (turn-queue/drain! "agent" (fn [_] (swap! calls inc) {:result "old"}))
        (is (= 1 @calls))
        (is (= :stale (get-in (turn-queue/snapshot) [:entries "old" :status])))))))

(deftest enabled-defaults-on
  (testing "durable queue defaults on unless explicitly disabled"
    (is (true? (turn-queue/enabled?)))
    (System/setProperty "FUTON3C_DURABLE_QUEUE" "false")
    (try
      (is (false? (turn-queue/enabled?)))
      (finally
        (System/clearProperty "FUTON3C_DURABLE_QUEUE")))))
