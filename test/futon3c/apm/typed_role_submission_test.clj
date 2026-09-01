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
   :evidence (cond-> (zipmap (sut/evidence-required-by-phase phase)
                             (repeat true))
               (contains? #{:student-attempt-1 :student-attempt-2
                            :student-attempt-3} phase)
               (assoc :memory-use {:used-ids []}))})

(deftest every-modelled-live-phase-has-an-executable-schema
  (doseq [phase (keys sut/evidence-required-by-phase)]
    (is (:ok (sut/validate-payload (authority phase) (payload phase)))
        (name phase))))

(deftest schema-materializes-nested-null-leaf-evidence-from-validation-authority
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "apm-schema-shapes"
                         (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [sut/*submission-root* root]
      (doseq [[phase expected]
              [[:guide-intervention-1
                {:channel-audit {:direct-student-contact? nil}}]
               [:student-attempt-1 {:memory-use {:used-ids nil}}]
               [:close-frame {:trace-id nil :result nil
                              :memory-use-audit nil}]]]
        (let [auth (authority phase)
              _ (sut/register! auth {:job-id (:job-id auth)})
              schema (sut/schema (:job-id auth) "secret")]
          (is (= expected (:evidence-shape schema)))
          (is (= (set (keys expected))
                 (set (:evidence-required schema)))))))))

(deftest validator-inspected-structures-are-declared-required-or-optional
  (doseq [phase (keys sut/validator-evidence-fields-by-phase)]
    (is (empty? (sut/validator-schema-findings (authority phase)))
        (name phase)))
  (testing "removing an inspected optional field fails the completeness check"
    (with-redefs [sut/evidence-optional-shape-by-phase
                  (dissoc sut/evidence-optional-shape-by-phase
                          :guide-intervention-1)]
      (is (= [:candidates]
             (sut/validator-schema-findings
              (authority :guide-intervention-1))))))
  (testing "removing an inspected candidate leaf fails the completeness check"
    (with-redefs [sut/evidence-optional-shape-by-phase
                  (update-in sut/evidence-optional-shape-by-phase
                             [:guide-intervention-1 :candidates 0]
                             dissoc :body)]
      (is (= [[:candidates 0 :body]]
             (sut/validator-schema-findings
              (authority :guide-intervention-1)))))))

(deftest guide-schema-declares-optional-candidate-leaf-shape
  (let [shape (sut/evidence-optional-shape
               (authority :guide-intervention-1))]
    (is (= [{:name nil :hook nil :body nil :pattern-ids nil}]
           (:candidates shape)))
    (is (not (contains? (sut/evidence-required
                         (authority :guide-intervention-1))
                        :candidates)))))

(deftest solver-checkpoint-schema-requires-structured-strategy-evidence
  (let [ordinary (authority :solve)
        checkpoint (assoc ordinary :solver/round 10
                           :solver/strategy-checkpoint? true)]
    (is (not (contains? (sut/evidence-required ordinary) :solver/strategy)))
    (is (contains? (sut/evidence-required checkpoint) :solver/strategy))
    (is (= #{:solver/strategy}
           (:evidence/missing
            (sut/validate-payload checkpoint (payload :solve)))))))

(deftest solver-shelf-is-controller-authority-and-requires-an-observation
  (let [shelf {:canary/id "c1" :assignment :control :shelf/entries []}
        auth (assoc (authority :solve) :solver-shelf-canary shelf)
        required (sut/evidence-required auth)]
    (is (= shelf (:solver-shelf-canary
                  (sut/authority auth {:job-id (:job-id auth)}))))
    (is (contains? required :solver/shelf-observation))
    (is (= #{:solver/shelf-observation}
           (:evidence/missing (sut/validate-payload auth (payload :solve)))))))

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
    (is (= #{:receipt}
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
          value (payload :promote-solver)
          result (sut/validate-payload auth value)]
      (is (= :role-submission-payload-invalid (:error/code result)) (name role))
      (is (= :canonical-pattern-search-required
             (get-in result [:memory-search/check :error/code])) (name role)))))

(deftest student-cannot-replay-controller-owned-memory-accounting
  (let [auth (assoc (authority :student-attempt-2) :role :student)
        valid (payload :student-attempt-2)
        replayed (assoc-in valid [:evidence :memory-use :surfaced-ids]
                           ["controller-memory-id"])
        result (sut/validate-payload auth replayed)]
    (is (:ok (sut/validate-payload auth valid)))
    (is (= [:controller-derived-memory-field-supplied-by-agent]
           (:findings result)))))

(deftest only-zai-scribe-end-reduction-has-candidate-authority
  (let [codex-auth (assoc (authority :scribe-reduce) :role :scribe)
        zai-auth (assoc (authority :scribe-reduce) :role :zai-scribe)]
    (is (not (contains? (sut/evidence-required codex-auth)
                        :memory-candidates)))
    (is (contains? (sut/evidence-required zai-auth) :memory-candidates))))

(deftest registered-job-membership-is-readable-by-frame
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "typed-authorities"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [sut/*submission-root* root]
      (is (:ok (sut/register! (authority :solve) {:job-id "job-solve"})))
      (is (:ok (sut/register! (assoc (authority :verify) :frame-id "f31")
                              {:job-id "job-verify"})))
      (is (= #{"job-solve"} (sut/registered-job-ids-for-frame "f30")))
      (is (= #{"job-verify"} (sut/registered-job-ids-for-frame "f31"))))))

(deftest predicate-key-normalization-is-exact-and-conflict-sensitive
  (is (= {:ok true :value {:direct-student-contact? false}}
         (sut/normalize-predicate-keys
          {:direct-student-contact false} [:direct-student-contact?])))
  (is (= {:ok true :value {}}
         (sut/normalize-predicate-keys {} [:direct-student-contact?])))
  (let [result (sut/normalize-predicate-keys
                {:direct-student-contact? false
                 :direct-student-contact true}
                [:direct-student-contact?])]
    (is (= :wire-predicate-key-conflict (:error/code result)))
    (is (= [:wire-predicate-key-conflict] (:findings result)))))
