(ns futon3c.social.pipeline-test
  "Integration test for the full social pipeline.

   Will run: S-presence -> S-authenticate -> S-mode -> S-dispatch ->
             S-validate -> S-persist

   Currently a stub — will be filled in as components are implemented
   (M-agency-refactor Parts II-VI)."
  (:require [clojure.test :refer [deftest is testing]]))

;; TODO: Wire pipeline integration test once all components exist.
;;
;; The pipeline test will:
;; 1. Create an AgentConnection (make-connection)
;; 2. Run through S-presence -> PresenceRecord
;; 3. Run through S-authenticate -> AgentIdentity
;; 4. Classify a message through S-mode -> ClassifiedMessage
;; 5. Route through S-dispatch -> DispatchReceipt
;; 6. Validate through S-validate -> CoordinationOutcome
;; 7. Persist through S-persist -> SessionRecord
;; 8. Verify all shapes validate at each boundary
;;
;; Exit condition: connection -> presence -> identity -> dispatch -> persist
;; At least one proof-path from gate pipeline submission.

(deftest pipeline-integration-placeholder
  (testing "placeholder — pipeline integration test will be wired in Part VI"
    (is true "placeholder passes — replace when components exist")))
