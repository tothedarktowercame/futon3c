(ns futon3c.apm.live-supervisor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.live-supervisor :as sut]))

(def inspection
  {:ok true :stepper/status :ready
   :obligation {:obligation/action
                {:kind :preflight :phase :preflight :frame-id "f19"}}})

(defn base [calls driven]
  {:launch-audit-fn (fn [] (swap! calls conj :audit) {:ok true})
   :inspect-fn (fn [] (swap! calls conj :inspect) inspection)
   :drive-phase-fn (fn [_] (swap! calls conj :drive) driven)
   :advance-fn (fn [_ _] (swap! calls conj :advance) {:ok true})
   :project-fn (fn [] (swap! calls conj :project) {:ok true})
   :park-fn (fn [request] (swap! calls conj [:park request]) {:ok true})
   :continuation-payload "continue-f19"})

(deftest awaiting-phase-parks-on-exact-job-without-advance
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :awaiting-terminal
                                      :state {:ticket {:job-id "job-19"}}}))]
    (is (= :parked (:status result)))
    (is (= "job-19" (:job-id result)))
    (is (= [:audit :inspect :drive] (take 3 @calls)))
    (is (not-any? #{:advance :project} @calls))
    (is (= ["job-19"] (get-in (last @calls) [1 :awaiting])))))

(deftest nested-solver-round-parks-on-its-active-ticket
  (let [calls (atom [])
        result (sut/tick!
                (base calls {:ok true :status :awaiting-terminal
                             :state {:state/type :solver-rounds
                                     :active {:ticket {:job-id "solve-round-2"}}}}))]
    (is (= :parked (:status result)))
    (is (= "solve-round-2" (:job-id result)))
    (is (= ["solve-round-2"] (get-in (last @calls) [1 :awaiting])))))

(deftest awaiting-phase-without-job-id-fails-before-park
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :awaiting-terminal
                                      :state {:state/type :solver-rounds}}))]
    (is (= :live-supervisor-job-id-missing (:error/code result)))
    (is (not-any? #(and (vector? %) (= :park (first %))) @calls))))

(deftest certified-phase-advances-projects-then-parks-for-next-tick
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :certified
                                      :certificate {:receipt/id "r1"}}))]
    (is (= :phase-advanced (:status result)))
    (is (= [:audit :inspect :drive :advance :project]
           (take 5 @calls)))
    (is (= [] (get-in (last @calls) [1 :awaiting])))))

(deftest audit-and-projection-failures-stop-without-routing-around
  (testing "audit"
    (let [calls (atom [])
          result (sut/tick! (assoc (base calls {})
                                   :launch-audit-fn #(do (swap! calls conj :audit)
                                                         {:ok false})))]
      (is (= :live-supervisor-launch-audit-failed (:error/code result)))
      (is (= [:audit] @calls))))
  (testing "projection"
    (let [calls (atom [])
          result (sut/tick! (assoc (base calls {:ok true :status :certified
                                                :certificate {}})
                                   :project-fn #(do (swap! calls conj :project)
                                                    {:ok false})))]
      (is (= :live-supervisor-projection-failed (:error/code result)))
      (is (not-any? #(and (vector? %) (= :park (first %))) @calls)))))
