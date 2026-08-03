(ns futon3c.diagramprover.causal.cohort-guard-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.cohort-guard :as guard]))

(def config
  {:axes [:memory-channel :recall-query-mode :graph-population]
   :join-key :dispatch-id
   :arm-field :memory-channel
   :denominator-requirements
   {:push+pull {:pull-offer-rate :pull-surfaced-ids}
    :pull-only {:pull-offer-rate :pull-surfaced-ids}}})

(def good-record
  {:dispatch-id "d-001"
   :memory-channel :push+pull
   :recall-query-mode :frequency-4
   :graph-population :star-forest
   :pull-surfaced-ids ["e-1" "e-2"]})

(deftest licensed-when-complete
  (let [verdict (guard/guard! good-record config)]
    (is (true? (:licensed? verdict)))
    (is (= verdict (guard/dispatch-verdict good-record config)))))

(deftest push-pull-without-pull-offers-refused
  ;; the exact axis-1 failure: uses countable, offers unrecorded
  (let [record (dissoc good-record :pull-surfaced-ids)
        verdict (guard/dispatch-verdict record config)]
    (is (false? (:licensed? verdict)))
    (is (= {:pull-offer-rate :pull-surfaced-ids}
           (-> verdict :checks (nth 2) :unrecorded-denominators)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"refused"
                          (guard/guard! record config)))
    (is (= [:denominator-recorded]
           (:failed (try (guard/guard! record config)
                         (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest push-arm-needs-no-pull-denominator
  (let [record (-> good-record
                   (assoc :memory-channel :push)
                   (dissoc :pull-surfaced-ids))]
    (is (true? (:licensed? (guard/dispatch-verdict record config))))))

(deftest unrecoverable-arm-refused
  (let [record (dissoc good-record :recall-query-mode)
        verdict (guard/dispatch-verdict record config)]
    (is (false? (:licensed? verdict)))
    (is (= [:recall-query-mode]
           (-> verdict :checks first :missing-fields)))))

(deftest join-key-collision-refused
  ;; the session-id-is-per-seat failure: same key value across dispatches
  (let [verdict (guard/dispatch-verdict
                 good-record (assoc config :prior-key-values ["d-001"]))]
    (is (false? (:licensed? verdict)))
    (is (true? (-> verdict :checks second :collision?))))
  (is (true? (:licensed? (guard/dispatch-verdict
                          good-record
                          (assoc config :prior-key-values ["d-000"]))))))

(deftest missing-join-key-refused
  (let [verdict (guard/dispatch-verdict (dissoc good-record :dispatch-id)
                                        config)]
    (is (false? (:licensed? verdict)))
    (is (true? (-> verdict :checks second :absent?)))))
