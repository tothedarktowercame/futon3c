(ns futon3c.apm.campaign-qualification-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-qualification :as qualification]))

(def role-configs
  (into {} (map (fn [role]
                  [role {:request-timeout-ms (if (= role :student)
                                               300000 :not-applicable)
                         :turn-timeout-ms 3600000
                         :request/source (if (= role :student)
                                           :zai-api/default-request-timeout-ms
                                           :not-applicable)
                         :turn/source :frame-seat/code-default}])
                qualification/required-roles)))

(def observations
  {:seat-configs role-configs
   :problem-check {:topology? false
                   :classification-source :operator-manifest}
   :registration-check {:frame-timeout-ms 14400000
                        :complete? true :coherent? true
                        :branch "frame/18" :commit "0123456789abcdef"
                        :worktree "/srv/apm-frames/frame-18"
                        :worktree-clean? true :head-matches? true
                        :dedicated-worktree? true}
   :cast-check {:ready? true :attributed? true}
   :continuation-check {:durable? true :wake-tested? true}
   :projection-check {:ledger-derived? true :frame-matches? true}
   :trace-check {:completed-roles qualification/required-roles
                 :recall-invoked? true :terrain-measured? true
                 :dispositions-complete? true :promotion-reviewed? true}
   :separation-check {:author-reviewer-distinct? true :arms-isolated? true}
   :receipt-check {:durable? true :replayable? true}
   :apparatus-check {:unchanged-since-open? true}})

(deftest explicit-effective-timeouts-are-derived-not-defaulted
  (let [facts (qualification/derive-facts observations)]
    (is (= {:topology? false :classification-source :operator-manifest}
           (:problem facts)))
    (is (= {:explicit? true :request-minutes 5 :turn-minutes 60
            :solver-minutes 60 :student-minutes 60 :frame-minutes 240}
           (:timeouts facts)))))

(deftest frame-17-five-minute-turn-baseline-is-visible
  (let [facts (qualification/derive-facts
               (assoc-in observations [:seat-configs :guide :turn-timeout-ms]
                         300000))]
    (is (= 5 (get-in facts [:timeouts :turn-minutes])))
    (is (= 60 (get-in facts [:timeouts :solver-minutes])))))

(deftest missing-role-or-route-fails-closed
  (let [facts (qualification/derive-facts
               (-> observations
                   (update :seat-configs dissoc :scribe)
                   (dissoc :continuation-check)))]
    (is (false? (get-in facts [:timeouts :explicit?])))
    (is (false? (get-in facts [:cast :ready?])))
    (is (nil? (get-in facts [:continuations :durable?])))))

(deftest serving-roster-metadata-is-the-timeout-source
  (let [seat-ids (into {} (map (fn [role] [role (str "f18-" (name role))])
                                qualification/required-roles))
        agents (into {} (map (fn [[role agent-id]]
                               [agent-id {:metadata
                                          {:effective-timeouts
                                           (get role-configs role)}}])
                             seat-ids))]
    (is (= role-configs
           (qualification/seat-configs-from-roster
            {:ok true :agents agents} seat-ids)))))
