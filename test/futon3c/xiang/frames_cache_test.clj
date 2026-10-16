(ns futon3c.xiang.frames-cache-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.xiang.frames-cache :as cache]))

(use-fixtures :each (fn [f] (cache/reset!) (try (f) (finally (cache/reset!)))))

(defn- harness [runner]
  (let [scheduled (atom [])]
    {:scheduled scheduled
     :opts {:runner runner
            :schedule! (fn [delay thunk] (swap! scheduled conj [delay thunk]))}}))

(defn- run-next! [h]
  (let [[_ thunk] (first @(:scheduled h))]
    (swap! (:scheduled h) subvec 1)
    (thunk)))

(deftest reads-once-and-rebuilds-after-invalidation
  (let [calls (atom 0)
        h (harness (fn [_] {:ok true :frames [{:n (swap! calls inc)}]}))
        a (cache/frames! "s" (:opts h))
        b (cache/frames! "s" (:opts h))]
    (is (= 1 @calls))
    (is (= (:version a) (:version b) 1))
    (is (false? (:cached a)))
    (is (true? (:cached b)))
    (cache/invalidate! "s" (:opts h))
    (run-next! h)
    (let [c (cache/frames! "s" (:opts h))]
      (is (= 2 @calls))
      (is (= 2 (:version c))))))

(deftest invalidations-coalesce-and-one-during-build-runs-once-more
  (let [calls (atom 0) h* (atom nil)
        runner (fn [_]
                 (let [n (swap! calls inc)]
                   (when (= n 2) (cache/invalidate! "s" (:opts @h*)))
                   {:ok true :frames [{:n n}]}))
        h (harness runner)]
    (reset! h* h)
    (cache/frames! "s" (:opts h))
    (dotimes [_ 3] (cache/invalidate! "s" (:opts h)))
    (is (= 1 (count @(:scheduled h))))
    (run-next! h)
    (is (= 2 @calls))
    (is (= 1 (count @(:scheduled h))) "an event during the build queues one follow-up")
    (run-next! h)
    (is (= 3 @calls))
    (is (empty? @(:scheduled h)))))

(deftest unseen-session-is-not-built
  (let [calls (atom 0) h (harness (fn [_] (swap! calls inc) {:ok true :frames []}))]
    (is (false? (cache/invalidate! "unseen" (:opts h))))
    (is (zero? @calls))
    (is (empty? @(:scheduled h)))))

(deftest failed-rebuild-keeps-last-good-entry
  (let [fail? (atom false) calls (atom 0)
        h (harness (fn [_] (swap! calls inc)
                     (if @fail? {:ok false :reason :broken :err "nope"}
                         {:ok true :frames [{:good true}]})))
        first (cache/frames! "s" (:opts h))]
    (reset! fail? true)
    (cache/invalidate! "s" (:opts h))
    (run-next! h)
    (let [after (cache/frames! "s" (:opts h))]
      (is (= (:frames first) (:frames after)))
      (is (= (:version first) (:version after)))
      (is (= :broken (get-in after [:last-error :reason]))))))

(deftest idle-eviction-keeps-only-fresh-entry
  (let [h (harness (fn [s] {:ok true :frames [{:session s}]}))]
    (cache/frames! "old" (:opts h))
    (cache/frames! "fresh" (:opts h))
    (swap! cache/!cache assoc-in ["old" :last-read-at] 0)
    (cache/evict-idle! 10000)
    (is (nil? (get @cache/!cache "old")))
    (is (some? (get @cache/!cache "fresh")))))

(deftest concurrent-first-read-runs-once
  (let [calls (atom 0) entered (promise) release (promise)
        opts {:runner (fn [_] (swap! calls inc) (deliver entered true) @release
                        {:ok true :frames [{:once true}]})}
        a (future (cache/frames! "s" opts))]
    @entered
    (let [b (future (cache/frames! "s" opts))]
      (deliver release true)
      (is (= 1 @calls))
      (is (= (:version @a) (:version @b) 1)))))
