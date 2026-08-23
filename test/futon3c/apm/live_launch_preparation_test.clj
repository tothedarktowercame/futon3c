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
    (is (= 7 (count (:seat/ids receipt))))))

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

(deftest prepare-is-idempotent-and-revalidates-every-effect
  (let [base (observation)
        calls (atom [])
        leases (into {} (map (fn [role]
                               [role (get-in base [:workspaces role :lease])])
                             sut/required-workspace-roles))
        result (sut/prepare!
                {:unit {:frame/id "f19" :problem/id "a00J01"}
                 :ledger (:ledger base) :role-cards (:role-cards base)
                 :leases leases
                 :workspace-exists? (fn [_ role] (swap! calls conj [:exists role]) true)
                 :provision-fn (fn [_ role] (swap! calls conj [:provision role]) {:ok false})
                 :validate-workspace-fn
                 (fn [lease] (swap! calls conj [:validate (:role lease)])
                   {:valid? true :findings []})
                 :mint-fn (fn [frame cast timeouts]
                            (swap! calls conj [:mint frame cast timeouts]) {:ok true})
                 :roster-fn (fn [frame]
                              (swap! calls conj [:roster frame]) (:seats base))})]
    (is (:ok result))
    (is (empty? (filter #(= :provision (first %)) @calls)))
    (is (= 2 (count (filter #(= :validate (first %)) @calls))))
    (is (= 1 (count (filter #(= :mint (first %)) @calls))))))

(deftest prepare-refuses-unattributed-existing-workspace-before-mint
  (let [calls (atom [])
        result (sut/prepare!
                {:unit {:frame/id "f19" :problem/id "a00J01"}
                 :ledger (:ledger (observation))
                 :role-cards (:role-cards (observation)) :leases {}
                 :workspace-exists? (fn [_ _] true)
                 :provision-fn (fn [& _] (swap! calls conj :provision))
                 :validate-workspace-fn (fn [& _] (swap! calls conj :validate))
                 :mint-fn (fn [& _] (swap! calls conj :mint))
                 :roster-fn (fn [& _] (swap! calls conj :roster))})]
    (is (= :existing-workspace-without-lease (:error/code result)))
    (is (empty? @calls))))

(deftest preparation-is-frame-generic-without-weakening-attribution
  (let [base (-> (observation)
                 (assoc :frame-id "f20" :problem-id "a01J06")
                 (update :workspaces
                         (fn [workspaces]
                           (into {}
                                 (map (fn [[role entry]]
                                        [role (-> entry
                                                  (assoc-in [:lease :frame/id] "f20")
                                                  (assoc-in [:lease :problem/id] "a01J06"))]))
                                 workspaces)))
                 (update :seats
                         (fn [seats]
                           (into {}
                                 (map (fn [[role seat]]
                                        [role (assoc seat :agent-id (str "f20-" (name role))
                                                   :frame-id "f20")]))
                                 seats))))]
    (is (:ok (sut/validate base)))
    (is (= :live-launch-preparation-invalid
           (:error/code (sut/validate (assoc-in base [:seats :solver :frame-id]
                                                "f19")))))))
