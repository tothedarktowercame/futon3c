(ns futon3c.apm.live-solver-rounds-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.live-solver-rounds :as sut]))

(def base-request
  {:dispatch/id "opening" :agent-id "f19-solver" :frame-id "f19"
   :problem-id "a01J05" :phase :solve :branch "solver-branch"})

(def legacy-state
  {:state/type :live-job-dispatched :request base-request
   :ticket {:job-id "job-1" :agent-id "f19-solver"}
   :activation/accepted? true})

(defn effects [persisted]
  {:request base-request :state legacy-state
   :announce-fn (fn [request]
                  {:ok true :job-id (str "job-" (:solver/round request))})
   :activate-fn (fn [_ _] {:ok true})
   :job-fn (fn [_]
             {:job-id "job-1" :agent-id "f19-solver" :session-id "solver-session"
              :state :done :report {:committed? false :residual "unfinished"}})
   :persist-fn (fn [state] (reset! persisted state) {:ok true})
   :validate-solved (fn [_ _ _]
                      {:ok false :error/code :live-proof-terminal-invalid
                       :findings [:final-head-not-committed]})
   :provide-receipt (fn [& _] (throw (ex-info "must not receipt" {})))})

(deftest legacy-failed-ticket-becomes-round-one-and-continues-same-session
  (let [persisted (atom nil)
        result (sut/drive! (effects persisted))]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= 1 (count (get-in result [:state :rounds]))))
    (is (= "solver-session"
           (get-in result [:state :rounds 0 :session-id])))
    (is (= 2 (get-in result [:state :active :request :solver/round])))
    (is (= "solver-session"
           (get-in result [:state :active :request :solver/prior-session-id])))
    (is (= 48 (get-in result [:state :active :request
                              :solver/remaining-rounds])))))

(deftest round-cap-requires-human-without-classifying-the-problem
  (let [persisted (atom nil)
        prior (mapv (fn [ordinal] {:ordinal ordinal :job-id (str "j" ordinal)})
                    (range 1 50))
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request :rounds prior :active legacy-state}
        result (sut/drive! (assoc (effects persisted) :state state
                                  :max-rounds 50))]
    (is (= :solver-human-intervention-required (:error/code result)))
    (is (= 50 (count (get-in result [:state :rounds]))))
    (is (nil? (get-in result [:state :problem/classification])))))

(deftest strict-success-is-the-only-path-to-a-solve-receipt
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :validate-solved (fn [_ _ _] {:ok true})
                       :provide-receipt
                       (fn [& _] {:ok true :certificate {:receipt/id "solved"}})))]
    (is (= :certified (:status result)))
    (is (= "solved" (get-in result [:certificate :receipt/id])))
    (is (= :live-job-certified (:state/type @persisted)))))

(deftest claimed-defect-stops-for-review-without-classifying-problem
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f19-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:solver/outcome :claimed-defect
                                           :residual "Hypotheses contradict conclusion at z=0."}})))]
    (is (= :solver-defect-review-required (:error/code result)))
    (is (= :claimed-defect (get-in result [:state :rounds 0 :outcome])))
    (is (nil? (get-in result [:state :problem/classification])))))

(deftest legacy-agent-nested-progress-fields-are-lifted-into-round-record
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f19-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:lean {:solver/outcome :progress
                                                  :residual "exact remaining goal"
                                                  :artifact-commits ["abc"]}}})))]
    (is (= :progress (get-in result [:state :rounds 0 :outcome])))
    (is (= "exact remaining goal"
           (get-in result [:state :rounds 0 :report :residual])))
    (is (= ["abc"]
           (get-in result [:state :rounds 0 :report :artifact-commits])))))

(deftest later-round-session-drift-fails-closed
  (let [persisted (atom nil)
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request
               :rounds [{:ordinal 1 :session-id "original-session"}]
               :active (assoc legacy-state
                              :request (sut/round-request base-request 2 nil))}
        result (sut/drive!
                (assoc (effects persisted) :state state
                       :job-fn (fn [_]
                                 {:job-id "job-2" :agent-id "f19-solver"
                                  :session-id "different-session" :state :done
                                  :report {}})))]
    (is (= :solver-session-mismatch (:error/code result)))
    (is (nil? @persisted))))
