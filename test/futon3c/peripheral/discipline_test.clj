(ns futon3c.peripheral.discipline-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.evidence.threads :as threads]
            [futon3c.peripheral.discipline :as discipline]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest discipline-start-produces-goal-evidence
  (let [p (discipline/make-discipline (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-discipline-1" :agent-id "codex-1"})]
    (is (:ok start))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (is (= :coordination (get-in start [:evidence :evidence/type])))))

(deftest discipline-step-selects-pattern
  (let [backend (tools/make-mock-backend {:psr-select {:pattern-id "patterns/mesh-001"}})
        p (discipline/make-discipline backend)
        start (runner/start p {:session-id "sess-discipline-2"})
        step (runner/step p (:state start) {:tool :psr-select :args ["patterns/mesh-001"]})]
    (is (:ok step))
    (is (= :patterns/mesh-001 (get-in step [:state :selected-pattern])))
    (is (= :pattern-selection (get-in step [:evidence :evidence/type])))
    (is (= :goal (get-in step [:evidence :evidence/claim-type])))
    (is (= :patterns/mesh-001 (get-in step [:evidence :evidence/pattern-id])))))

(deftest discipline-rejects-non-discipline-tools
  (let [p (discipline/make-discipline (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-discipline-3"})
        err (runner/step p (:state start) {:tool :edit :args ["src/a.clj"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :tool-not-allowed (:error/code err)))))

(deftest discipline-stop-returns-pattern-context
  (let [backend (tools/make-mock-backend {:psr-select {:pattern-id "patterns/mesh-002"}
                                          :pur-update {:status :ok}})
        p (discipline/make-discipline backend)
        start (runner/start p {:session-id "sess-discipline-4"})
        step1 (runner/step p (:state start) {:tool :psr-select :args ["patterns/mesh-002"]})
        step2 (runner/step p (:state step1) {:tool :pur-update :args ["patterns/mesh-002" :ok]})
        stop (runner/stop p (:state step2) "par generated")]
    (is (:ok stop))
    (is (= :patterns/mesh-002 (get-in stop [:context :pattern-id])))
    (is (= :conclusion (get-in stop [:evidence :evidence/claim-type])))
    (is (= 2 (count (get-in stop [:fruit :records]))))))

(deftest discipline-psr-pur-evidence-projects-pattern-thread
  (let [evidence-store (atom {:entries {} :order []})
        backend (tools/make-mock-backend {:psr-search {:patterns ["patterns/mesh-003"]}
                                          :psr-select {:pattern-id "patterns/mesh-003"}
                                          :pur-update {:status :ok}})
        p (discipline/make-discipline backend)
        start (runner/start p {:session-id "sess-discipline-5"
                               :agent-id "codex-1"
                               :evidence-store evidence-store})
        step1 (runner/step p (:state start) {:tool :psr-search :args ["mesh"]})
        step2 (runner/step p (:state step1) {:tool :psr-select :args ["patterns/mesh-003"]})
        step3 (runner/step p (:state step2) {:tool :pur-update :args ["patterns/mesh-003" :ok]})
        stop (runner/stop p (:state step3) "par generated")
        entries (vals (:entries @evidence-store))
        pattern-ref {:ref/type :pattern :ref/id "patterns/mesh-003"}
        thread (threads/project-thread evidence-store pattern-ref)]
    (is (:ok stop))
    (is (some #(and (= :pattern-selection (:evidence/type %))
                    (= :goal (:evidence/claim-type %))
                    (= pattern-ref (:evidence/subject %))
                    (= :patterns/mesh-003 (:evidence/pattern-id %)))
              entries))
    (is (some #(and (= :pattern-outcome (:evidence/type %))
                    (= :evidence (:evidence/claim-type %))
                    (= pattern-ref (:evidence/subject %))
                    (= :patterns/mesh-003 (:evidence/pattern-id %)))
              entries))
    (is (map? thread))
    (is (= pattern-ref (:thread/subject thread)))
    (is (>= (:thread/entry-count thread) 2))))
