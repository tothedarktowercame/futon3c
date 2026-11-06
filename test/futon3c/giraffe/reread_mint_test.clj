(ns futon3c.giraffe.reread-mint-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.giraffe.reread-mint :as mint]))

(deftest concurrent-requests-coalesce
  (let [entered (promise) release (promise) calls (atom 0)
        coordinator (mint/coordinator
                     (fn []
                       (swap! calls inc)
                       (deliver entered true)
                       @release
                       {:status :minted :run-id "run-1"}))]
    (is (= :started (:status (mint/request! coordinator))))
    @entered
    (is (= :coalesced (:status (mint/request! coordinator))))
    (is (= :coalesced (:status (mint/request! coordinator))))
    (deliver release true)
    (let [state (mint/await-idle! coordinator 2000)]
      (is (= 2 @calls) "all mid-mint requests become one follow-up mint")
      (is (= (:requested state) (:completed state))))))

(deftest landing-during-mint-runs-one-follow-up
  (let [first-entered (promise) first-release (promise) calls (atom 0)
        coordinator (mint/coordinator
                     (fn []
                       (let [n (swap! calls inc)]
                         (when (= 1 n)
                           (deliver first-entered true)
                           @first-release)
                         {:status :minted :run-id (str "run-" n)})))]
    (mint/request! coordinator)
    @first-entered
    (mint/request! coordinator)
    (deliver first-release true)
    (let [state (mint/await-idle! coordinator 2000)]
      (is (= 2 @calls))
      (is (= "run-2" (:last-run-id state))))))
