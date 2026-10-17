(ns futon3c.xiang.ma-observer-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.turn-events :as te]
            [futon3c.xiang.ma-observer :as ma]))

(deftest final-turn-of-an-enabled-agent-runs-ma-once
  (let [runs (atom []) done (promise) run0 @ma/!run]
    (reset! ma/!run (fn [s a] (swap! runs conj [s a]) (deliver done true) {:exit 0}))
    (try
      (with-redefs [ma/enabled-agents (constantly #{"zai-1"})]
        (let [listener (fn [ev] (when (contains? (ma/enabled-agents) (:author ev))
                                  (ma/observe! (:session-id ev) (:author ev))))]
          ;; the real listener's body, run directly: author not enabled -> nothing
          (listener {:session-id "s0" :author "codex-9"})
          (listener {:session-id "s1" :author "zai-1"})
          (deref done 2000 nil)
          (is (= [["s1" "zai-1"]] @runs))))
      (finally (reset! ma/!run run0)))))

(deftest registered-on-the-post-phase
  (let [ls @@#'te/!listeners]
    (is (= :post (get-in ls [:futon3c.xiang.ma-observer/ma :phase])))))
