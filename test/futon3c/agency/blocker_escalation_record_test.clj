(ns futon3c.agency.blocker-escalation-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.blocker-escalation-record :as sut]
            [futon3c.agency.act-harness :as harness]))

(def record
  {:id "act:blocker" :kind :escalation/blocker :schema 1
   :source-job "invoke-job-1" :author "codex-5" :orchestrator "claude-17"
   :blocker "The store schema remains ambiguous."
   :at "2026-09-29T01:00:00Z" :psr-id "psr:1" :pur-id "pur:1"
   :pattern-id "orchestration/recorded-handoff"
   :act/stamp {:executor "codex-5" :signer "codex-5"
               :authority {:dispatch-edge "edge:1"} :executor-basis :declared}
   :act/harness (harness/plain "test:blocker")})

(deftest blocker-round-trips
  (is (= record (-> record sut/->hyperedge sut/hyperedge->record))))

(deftest blocker-refuses-work-request-field
  (testing "an escalation reports a block; it cannot smuggle a work request"
    (let [error (try (sut/validate! (assoc record :work-request "do this")) nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (= :unexpected-key (:reason (ex-data error)))))))
