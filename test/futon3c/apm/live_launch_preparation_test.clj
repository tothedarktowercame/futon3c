(ns futon3c.apm.live-launch-preparation-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-launch-preparation :as sut]))

(defn- observation []
  {:frame-id "f19" :problem-id "a00J01"
   :ledger {:version 5
            :digest "ed49e674ccabb666f32faac12bb2eb0a69daaa091e551692be550267f1ca98b7"
            :phase :preflight :claim nil}
   :workspaces
   (into {} (map (fn [role]
                   [role {:lease {:workspace/id (str "workspace-" (name role))
                                  :frame/id "f19" :problem/id "a00J01" :role role}
                          :validation {:valid? true :findings []}}])
                 sut/required-workspace-roles))
   :seats
   (into {} (map (fn [[role type]]
                   [role {:agent-id (str "f19-" (name role)) :type type
                          :frame-id "f19" :invoke-ready? true
                          :effective-timeouts
                          {:request-timeout-ms (if (= type :zai) 300000 :not-applicable)
                           :turn-timeout-ms 3600000}}])
                 sut/required-seat-types))
   :role-cards
   (into {} (map (fn [role] [role {:path (str (name role) ".md")
                                   :blob (str "blob-" (name role))}])
                 (keys sut/required-seat-types)))})

(deftest complete-preparation-is-content-addressed
  (let [result (sut/validate (observation))
        receipt (:receipt result)]
    (is (:ok result))
    (is (= (:receipt/id receipt)
           (machine/ledger-digest [(dissoc receipt :receipt/id)])))
    (is (= #{:solver :student} (set (keys (:workspace/ids receipt)))))
    (is (= 5 (count (:seat/ids receipt))))))

(deftest preparation-fails-closed
  (testing "no ledger drift, unvalidated workspace, bad seat, or unpinned card"
    (let [bad (-> (observation)
                  (assoc-in [:ledger :version] 6)
                  (assoc-in [:workspaces :solver :validation :valid?] false)
                  (assoc-in [:seats :student :effective-timeouts :turn-timeout-ms] 300000)
                  (assoc-in [:role-cards :guide] {}))
          findings (set (map :finding (:findings (sut/validate bad))))]
      (is (= #{:ledger-version-mismatch :workspace-validation-failed
               :seat-turn-timeout-mismatch :role-card-pin-missing}
             findings)))))
