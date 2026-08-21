(ns futon3c.apm.countdown-control-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.countdown-control :as sut]))

(deftest replacement-registration-starts-at-f19-with-complete-cycle
  (let [body (sut/registration-body)
        units (get-in body [:block-plan 0 :units])]
    (is (= 9 (count units)))
    (is (= "f19" (:frame-id (first units))))
    (is (= "f27" (:frame-id (last units))))
    (is (= 10 (count (:phase-order body))))
    (is (= :preflight (first (:phase-order body))))
    (is (= :close-frame (last (:phase-order body))))
    (is (not-any? #(contains? % :required-receipt-kinds) units)
        "eventual close receipts must not be required at open-frame runtime")))

(deftest set-alight-drives-live-supervisor-with-exact-continuation
  (let [calls (atom [])
        result
        (sut/set-alight!
         {:agent "codex-10" :session "session-10" :surface "emacs-repl"}
         {:launch-audit-fn #(do (swap! calls conj :audit) {:ok true})
          :inspect-fn #(do (swap! calls conj :inspect)
                           {:ok true :stepper/status :ready
                            :obligation {:obligation/action
                                         {:kind :solve :phase :solve
                                          :frame-id "f19"}}})
          :drive-phase-fn #(do (swap! calls conj [:drive %])
                               {:ok true :status :awaiting-terminal
                                :state {:ticket {:job-id "job-f19-solve"}}})
          :advance-fn (fn [& _] (swap! calls conj :advance) {:ok true})
          :project-fn #(do (swap! calls conj :project) {:ok true})
          :park-fn #(do (swap! calls conj [:park %]) {:ok true})})]
    (is (= :parked (:status result)))
    (is (= "job-f19-solve" (:job-id result)))
    (is (= [:audit :inspect] (take 2 @calls)))
    (is (= ["job-f19-solve"] (get-in (last @calls) [1 :awaiting])))
    (is (re-find #"set-alight!" (get-in (last @calls) [1 :payload])))
    (is (not-any? #{:advance :project} @calls))))

(deftest set-alight-never-parks-when-launch-audit-fails
  (let [parked? (atom false)
        result (sut/set-alight!
                {:agent "codex-10" :session "wrong" :surface "emacs-repl"}
                {:launch-audit-fn (constantly {:ok false :finding :identity})
                 :inspect-fn (constantly {:ok true})
                 :drive-phase-fn (constantly {:ok true})
                 :advance-fn (constantly {:ok true})
                 :project-fn (constantly {:ok true})
                 :park-fn #(do (reset! parked? true) {:ok true})})]
    (is (= :live-supervisor-launch-audit-failed (:error/code result)))
    (is (false? @parked?))))
