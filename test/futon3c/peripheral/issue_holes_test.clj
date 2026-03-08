(ns futon3c.peripheral.issue-holes-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.issue-holes :as ih])
  (:import [java.time Instant]))

(deftest extract-mission-and-claim-refs
  (testing "extractors pull explicit mission tags + known claim refs"
    (let [text "Relates to M-codex-irc-execution and :war-room/tri-agent-loop."
          missions (ih/extract-mission-refs text #{"codex-irc-execution" "mission-control"})
          claims (ih/extract-claim-refs text #{"war-room/tri-agent-loop"})]
      (is (= ["codex-irc-execution"] missions))
      (is (= ["war-room/tri-agent-loop"] claims))))
  (testing "generic short mission ids are ignored to avoid noisy anchors"
    (is (= []
           (ih/extract-mission-refs "Tickle smoke test and docs cleanup."
                                    #{"test" "mission-control"})))
    (is (= []
           (ih/extract-mission-refs "Escalate to M-test immediately."
                                    #{"test" "mission-control"})))
    (is (= ["cyder"]
           (ih/extract-mission-refs "Escalate to M-cyder now."
                                    #{"cyder" "mission-control"})))))

(deftest issue->hole-classifies-spam-candidate
  (testing "unanchored old unowned issue is marked spam-candidate"
    (let [now (Instant/parse "2026-03-08T00:00:00Z")
          issue {:number 99
                 :title "Untitled"
                 :body ""
                 :url "https://example.test/issues/99"
                 :createdAt "2026-01-01T00:00:00Z"
                 :updatedAt "2026-01-01T00:00:00Z"
                 :labels []
                 :assignees []
                 :comments []
                 :author {:login "nobody"}}
          hole (ih/issue->hole issue {:repo "owner/repo"
                                      :now now
                                      :known-mission-ids #{"mission-control"}
                                      :known-claim-ids #{"war-room/tri-agent-loop"}
                                      :stale-days 14
                                      :old-days 30})]
      (is (= :spam-candidate (:hole/status hole)))
      (is (some #{:unanchored} (:hole/flags hole)))
      (is (some #{:close-or-reframe} (:hole/suggested-actions hole))))))

(deftest build-export-from-issues-aggregates-summary
  (testing "summary tracks status/anchor counts"
    (let [now (Instant/parse "2026-03-08T00:00:00Z")
          issues [{:number 1
                   :title "Implement M-mission-control wiring for :war-room/tri-agent-loop"
                   :body "Followup"
                   :url "https://example.test/issues/1"
                   :createdAt "2026-03-07T00:00:00Z"
                   :updatedAt "2026-03-07T00:00:00Z"
                   :labels [{:name "enhancement"}]
                   :assignees [{:login "joe"}]
                   :comments [{:id "c1"}]
                   :author {:login "joe"}}
                  {:number 2
                   :title "Unowned task"
                   :body ""
                   :url "https://example.test/issues/2"
                   :createdAt "2026-02-01T00:00:00Z"
                   :updatedAt "2026-02-10T00:00:00Z"
                   :labels []
                   :assignees []
                   :comments []
                   :author {:login "ghost"}}]
          export (ih/build-issue-hole-export-from-issues
                  issues
                  {:repo "owner/repo"
                   :now now
                   :known-mission-ids #{"mission-control"}
                   :known-claim-ids #{"war-room/tri-agent-loop"}
                   :stale-days 14
                   :old-days 30})]
      (is (= 2 (get-in export [:summary :open-issues])))
      (is (= 1 (get-in export [:summary :anchored])))
      (is (= 1 (get-in export [:summary :unanchored])))
      (is (= 1 (get-in export [:summary :by-status :active])))
      (is (= 1 (get-in export [:summary :by-status :spam-candidate])))
      (is (map? (:strategic/intelligence export)))
      (is (seq (get-in export [:strategic/intelligence :why-these-now])))
      (is (seq (get-in export [:strategic/intelligence :blockers-risks])))
      (is (some #(clojure.string/includes? % "#2")
                (get-in export [:strategic/intelligence :blockers-risks]))))))
