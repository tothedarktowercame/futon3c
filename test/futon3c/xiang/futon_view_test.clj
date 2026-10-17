(ns futon3c.xiang.futon-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.futon-view :as fv]
            [futon3c.xiang.turn-service]))

(defn- with-stubs [f]
  (let [runs (atom []) invalidated (atom []) gate (promise)
        run0 @fv/!run inv0 @fv/!invalidate]
    (reset! fv/!run (fn [s] @gate (swap! runs conj s) {:exit 0}))
    (reset! fv/!invalidate (fn [s] (swap! invalidated conj s)))
    (try (f runs invalidated gate)
         (finally (reset! fv/!run run0) (reset! fv/!invalidate inv0)))))

(defn- wait-for [pred]
  (loop [n 0] (when (and (not (pred)) (< n 200)) (Thread/sleep 10) (recur (inc n)))))

(deftest a-burst-for-one-session-runs-once-then-rebuilds-its-frames
  (with-stubs
    (fn [runs invalidated gate]
      ;; the bad case: :reading and :settled for the same turn back to back
      ;; must not start two python processes
      (let [blocker (fv/refresh! "s-busy")]          ; occupies the single thread
        (is blocker)
        (is (true? (fv/refresh! "s1")))
        (is (nil? (fv/refresh! "s1")) "a second event while queued is coalesced")
        (deliver gate true)
        (wait-for #(= 2 (count @invalidated)))
        (is (= ["s-busy" "s1"] @runs))
        (is (= ["s-busy" "s1"] @invalidated))))))

(deftest only-reading-and-settled-trigger
  (with-stubs
    (fn [runs _ gate]
      (deliver gate true)
      (let [listener (get @@#'futon3c.xiang.turn-service/!turn-listeners :futon3c.xiang.futon-view/futon-view)]
        (testing "the listener is registered at load"
          (is (fn? listener)))
        (listener {:event :recorded :session-id "s-rec"})
        (listener {:event :reading :session-id "s-read"})
        (wait-for #(= ["s-read"] @runs))
        (is (= ["s-read"] @runs) ":recorded alone does not update the view")))))

(deftest nil-session-is-ignored
  (is (nil? (fv/refresh! nil))))
