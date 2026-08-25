(ns futon3c.apm.typed-role-submission-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.typed-role-submission :as sut]))

(defn authority [phase]
  {:job-id (str "job-" (name phase))
   :dispatch/id (str "dispatch-" (name phase))
   :agent-id "f30-role" :frame-id "f30" :problem-id "m00A00"
   :phase phase :role :controller :submission/token "secret"})

(defn payload [phase]
  {:command-own-exit 0 :outcome "complete" :failure-account []
   :evidence (zipmap (sut/evidence-required-by-phase phase) (repeat true))})

(deftest every-modelled-live-phase-has-an-executable-schema
  (doseq [phase (keys sut/evidence-required-by-phase)]
    (is (:ok (sut/validate-payload (authority phase) (payload phase)))
        (name phase))))

(deftest solver-checkpoint-schema-requires-structured-strategy-evidence
  (let [ordinary (authority :solve)
        checkpoint (assoc ordinary :solver/round 10
                           :solver/strategy-checkpoint? true)]
    (is (not (contains? (sut/evidence-required ordinary) :solver/strategy)))
    (is (contains? (sut/evidence-required checkpoint) :solver/strategy))
    (is (= #{:solver/strategy}
           (:evidence/missing
            (sut/validate-payload checkpoint (payload :solve)))))))

(deftest authority-is-controller-owned-and-submission-is-content-addressed
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "apm-submissions" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [sut/*submission-root* root]
      (let [request (-> (authority :student-attempt-1)
                        (dissoc :job-id)
                        (assoc :memory-snapshot {:snapshot-id "snap-1"}))
            ticket {:job-id "job-student-attempt-1"}
            registered (sut/register! request ticket)
            submitted (sut/submit! (:job-id ticket) "secret"
                                   (payload :student-attempt-1))]
        (is (:ok registered))
        (is (= [:memory-use]
               (:evidence-required
                (sut/schema (:job-id ticket) "secret"))))
        (is (:ok submitted))
        (is (= "f30" (get-in submitted [:submission :authority :frame-id])))
        (is (= "snap-1" (get-in submitted
                                [:submission :authority :memory-snapshot
                                 :snapshot-id])))
        (is (string? (get-in submitted [:submission :submission/id])))
        (is (= :already-submitted
               (:status (sut/submit! (:job-id ticket) "secret"
                                     (payload :student-attempt-1)))))
        (is (= :role-submission-conflict
               (:error/code
                (sut/submit! (:job-id ticket) "secret"
                             (assoc-in (payload :student-attempt-1)
                                       [:evidence :memory-use] false)))))))))

(deftest malformed-and-forged-payloads-fail-before-persistence
  (testing "field-level feedback names missing evidence"
    (let [result (sut/validate-payload (authority :solve)
                                       {:command-own-exit 0 :outcome "partial"
                                        :failure-account [] :evidence {}})]
      (is (= :role-submission-payload-invalid (:error/code result)))
      (is (contains? (:evidence/missing result) :final-head))))
  (testing "an agent cannot supply authority fields"
    (let [result (sut/validate-payload
                  (authority :guide-intervention-1)
                  (assoc (payload :guide-intervention-1) :frame-id "f99"))]
      (is (= [:authority-field-supplied-by-agent] (:findings result))))))

(deftest f29-narrated-search-is-not-execution-evidence
  (let [{:keys [authority payload expected-finding]}
        (edn/read-string
         (slurp "test/fixtures/apm/f29-narrated-search-without-receipt.edn"))
        result (sut/validate-payload authority payload)]
    (is (= :role-submission-payload-invalid (:error/code result)))
    (is (some #{expected-finding} (:findings result)))
    (is (= #{:memory-search-receipt-ids :receipt}
           (:evidence/missing result)))))

(deftest capable-role-may-honestly-report-no-search
  (let [auth (assoc (authority :student-attempt-1) :role :student)
        value (payload :student-attempt-1)]
    (is (not (contains? (sut/evidence-required auth)
                        :memory-search-receipt-ids)))
    (is (:ok (sut/validate-payload auth value)))))

(deftest canonical-pattern-search-is-mandatory-for-scribe-and-promotion-proctor
  (doseq [role [:scribe :promotion-proctor]]
    (let [auth (assoc (authority :promote-solver) :role role)
          value (assoc-in (payload :promote-solver)
                          [:evidence :memory-search-receipt-ids] [])
          result (sut/validate-payload auth value)]
      (is (= :role-submission-payload-invalid (:error/code result)) (name role))
      (is (some #{:canonical-pattern-search-required}
                (get-in result [:memory-search/check :findings])) (name role)))))

(deftest only-zai-scribe-end-reduction-has-candidate-authority
  (let [codex-auth (assoc (authority :scribe-reduce) :role :scribe)
        zai-auth (assoc (authority :scribe-reduce) :role :zai-scribe)]
    (is (not (contains? (sut/evidence-required codex-auth)
                        :memory-candidates)))
    (is (contains? (sut/evidence-required zai-auth) :memory-candidates))))
