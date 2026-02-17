(ns futon3c.peripheral.proof-backend-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.proof-dag :as dag]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.test-fixtures :as fix]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- make-test-backend
  "Create a ProofBackend with a temp directory for isolation."
  []
  (let [dir (fix/temp-dir!)]
    {:backend (pb/make-proof-backend {:cwd dir} (tools/make-mock-backend))
     :cwd dir}))

(defn- init-test-problem!
  "Initialize a test problem and load it into the backend."
  [{:keys [backend cwd]}]
  (pb/init-problem! cwd "P-test" "Prove X > 0 for all X in S" "All cases verified")
  (tools/execute-tool backend :proof-load ["P-test"]))

;; =============================================================================
;; Load/Save round-trip
;; =============================================================================

(deftest load-save-round-trip
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        load-result (tools/execute-tool backend :proof-load ["P-test"])]
    (is (:ok load-result))
    (is (= "P-test" (get-in load-result [:result :proof/problem-id])))
    ;; Save bumps version
    (let [save-result (tools/execute-tool backend :proof-save ["P-test"])]
      (is (:ok save-result))
      (is (= 2 (get-in save-result [:result :proof/version])))
      ;; Reload confirms persistence
      (let [reload (tools/execute-tool backend :proof-load ["P-test"])]
        (is (:ok reload))
        (is (= 2 (get-in reload [:result :proof/version])))))))

(deftest load-nonexistent-problem
  (let [{:keys [backend]} (make-test-backend)
        result (tools/execute-tool backend :proof-load ["P-nonexistent"])]
    (is (not (:ok result)))))

;; =============================================================================
;; Status policy enforcement (SR-2)
;; =============================================================================

(deftest status-policy-rejects-invalid-status
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        ;; Add a ledger item
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-1" {:item/label "test item" :item/status :open}])
        ;; Try to set an invalid status
        result (tools/execute-tool backend :ledger-upsert
                 ["P-test" "L-1" {:item/status :basically-solved}])]
    (is (not (:ok result)))
    (is (= :invalid-status (get-in result [:error :code])))))

(deftest status-policy-rejects-proved-with-numerical-evidence
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-1" {:item/label "test" :item/status :open
                             :item/evidence-type :numerical}])
        result (tools/execute-tool backend :ledger-upsert
                 ["P-test" "L-1" {:item/status :proved}])]
    (is (not (:ok result)))
    (is (= :invalid-transition (get-in result [:error :code])))))

(deftest status-validate-tool
  (let [{:keys [backend]} (make-test-backend)]
    (testing "numerical cannot yield proved"
      (let [r (tools/execute-tool backend :status-validate [:open :proved :numerical])]
        (is (:ok r))
        (is (not (get-in r [:result :valid?])))))
    (testing "analytical can yield proved"
      (let [r (tools/execute-tool backend :status-validate [:open :proved :analytical])]
        (is (:ok r))
        (is (get-in r [:result :valid?]))))
    (testing "cannot leave :false"
      (let [r (tools/execute-tool backend :status-validate [:false :open :analytical])]
        (is (:ok r))
        (is (not (get-in r [:result :valid?])))))))

;; =============================================================================
;; Ledger upsert
;; =============================================================================

(deftest ledger-upsert-creates-and-updates
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        ;; Create new item
        create (tools/execute-tool backend :ledger-upsert
                 ["P-test" "L-new" {:item/label "New obligation"
                                    :item/status :open}])
        ;; Query it back
        query (tools/execute-tool backend :ledger-query ["P-test"])]
    (is (:ok create))
    (is (= "L-new" (get-in create [:result :item/id])))
    (is (:ok query))
    (is (contains? (:result query) "L-new"))))

(deftest ledger-query-by-status
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-open" {:item/label "Open item" :item/status :open}])
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-partial" {:item/label "Partial item" :item/status :partial}])
        query-open (tools/execute-tool backend :ledger-query
                     ["P-test" {:status :open}])]
    (is (:ok query-open))
    (is (= 1 (count (:result query-open))))
    (is (contains? (:result query-open) "L-open"))))

;; =============================================================================
;; DAG acyclicity (SR-3)
;; =============================================================================

(deftest dag-check-detects-acyclic
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-A" {:item/label "A" :item/status :open
                             :item/depends-on #{} :item/unlocks #{"L-B"}}])
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-B" {:item/label "B" :item/status :open
                             :item/depends-on #{"L-A"} :item/unlocks #{}}])
        result (tools/execute-tool backend :dag-check ["P-test"])]
    (is (:ok result))
    (is (:acyclic? (:result result)))))

(deftest dag-check-detects-cycle
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-A" {:item/label "A" :item/status :open
                             :item/depends-on #{"L-B"} :item/unlocks #{"L-B"}}])
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-B" {:item/label "B" :item/status :open
                             :item/depends-on #{"L-A"} :item/unlocks #{"L-A"}}])
        result (tools/execute-tool backend :dag-check ["P-test"])]
    (is (:ok result))
    (is (not (:acyclic? (:result result))))
    (is (seq (:cycle-nodes (:result result))))))

;; =============================================================================
;; Impact scoring (SR-6)
;; =============================================================================

(deftest dag-impact-ranks-by-transitive-unlock
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        ;; A → B → C (A has highest impact)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-A" {:item/label "A" :item/status :open
                             :item/depends-on #{} :item/unlocks #{"L-B"}}])
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-B" {:item/label "B" :item/status :open
                             :item/depends-on #{"L-A"} :item/unlocks #{"L-C"}}])
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-C" {:item/label "C" :item/status :open
                             :item/depends-on #{"L-B"} :item/unlocks #{}}])
        result (tools/execute-tool backend :dag-impact ["P-test"])]
    (is (:ok result))
    (let [scores (:result result)]
      ;; L-A should have highest score (unlocks B and C)
      (is (= "L-A" (:item-id (first scores))))
      (is (= 2 (:score (first scores)))))))

;; =============================================================================
;; Canonical statement (SR-1)
;; =============================================================================

(deftest canonical-get-and-update
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        get-result (tools/execute-tool backend :canonical-get ["P-test"])]
    (is (:ok get-result))
    (is (= "Prove X > 0 for all X in S" (get-in get-result [:result :statement])))
    ;; Update with version tracking
    (let [update-result (tools/execute-tool backend :canonical-update
                          ["P-test" "Prove X >= 0 for all X in S" "Relaxed bound"])]
      (is (:ok update-result))
      (is (= "Prove X >= 0 for all X in S" (get-in update-result [:result :statement])))
      (is (= 2 (count (get-in update-result [:result :version-history])))))))

;; =============================================================================
;; Cycle lifecycle
;; =============================================================================

(deftest cycle-begin-and-advance
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-blocker" {:item/label "Main blocker" :item/status :open}])
        ;; Begin cycle
        begin (tools/execute-tool backend :cycle-begin ["P-test" "L-blocker"])
        cycle-id (get-in begin [:result :cycle/id])
        ;; Advance through observe
        adv (tools/execute-tool backend :cycle-advance
                                ["P-test" cycle-id {:blocker-id "L-blocker"}])]
    (is (:ok begin))
    (is (= :observe (get-in begin [:result :cycle/phase])))
    (is (:ok adv))
    (is (= :propose (get-in adv [:result :cycle/phase])))))

(deftest cycle-begin-rejects-nonexistent-blocker
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        result (tools/execute-tool backend :cycle-begin ["P-test" "L-nonexistent"])]
    (is (not (:ok result)))
    (is (= :invalid-blocker (get-in result [:error :code])))))

(deftest cycle-advance-requires-phase-outputs
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-b" {:item/label "Blocker" :item/status :open}])
        begin (tools/execute-tool backend :cycle-begin ["P-test" "L-b"])
        cycle-id (get-in begin [:result :cycle/id])
        ;; Try to advance without required output
        adv (tools/execute-tool backend :cycle-advance
              ["P-test" cycle-id {}])]
    (is (not (:ok adv)))
    (is (= :missing-phase-outputs (get-in adv [:error :code])))))

(deftest cycle-list-and-get
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-b" {:item/label "B" :item/status :open}])
        begin (tools/execute-tool backend :cycle-begin ["P-test" "L-b"])
        cycle-id (get-in begin [:result :cycle/id])
        list-result (tools/execute-tool backend :cycle-list ["P-test"])
        get-result (tools/execute-tool backend :cycle-get ["P-test" cycle-id])]
    (is (:ok list-result))
    (is (= 1 (count (:result list-result))))
    (is (:ok get-result))
    (is (= cycle-id (get-in get-result [:result :cycle/id])))))

;; =============================================================================
;; Failed routes (SR-8: honesty)
;; =============================================================================

(deftest failed-route-append-only
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-b" {:item/label "B" :item/status :open}])
        add-result (tools/execute-tool backend :failed-route-add
                     ["P-test" {:route/blocker-id "L-b"
                                :route/approach "Direct proof by induction"
                                :route/structural-obstruction "Induction schema cannot close under dependency cycle"
                                :route/failure-reason "Induction hypothesis too weak"
                                :route/evidence-refs ["artifact-001"]}])]
    (is (:ok add-result))
    (is (string? (get-in add-result [:result :route/id])))))

(deftest failed-route-requires-structural-obstruction
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-b" {:item/label "B" :item/status :open}])
        add-result (tools/execute-tool backend :failed-route-add
                     ["P-test" {:route/blocker-id "L-b"
                                :route/approach "Direct proof by induction"
                                :route/failure-reason "Induction hypothesis too weak"
                                :route/evidence-refs ["artifact-001"]}])]
    (is (not (:ok add-result)))
    (is (= :missing-structural-obstruction (get-in add-result [:error :code])))))

(deftest honesty-cannot-erase-failure-point
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        ;; Create item with failure point
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-fail" {:item/label "Failed item"
                                :item/status :open
                                :item/failure-point "Diverges at case 3c"}])
        ;; Try to erase the failure point
        result (tools/execute-tool backend :ledger-upsert
                 ["P-test" "L-fail" {:item/failure-point nil}])]
    (is (not (:ok result)))
    (is (= :honesty-violation (get-in result [:error :code])))))

;; =============================================================================
;; Gate check (CR-8)
;; =============================================================================

(deftest gate-check-reports-missing-gates
  (let [{:keys [backend] :as ctx} (make-test-backend)
        _ (init-test-problem! ctx)
        _ (tools/execute-tool backend :ledger-upsert
            ["P-test" "L-b" {:item/label "B" :item/status :open}])
        begin (tools/execute-tool backend :cycle-begin ["P-test" "L-b"])
        cycle-id (get-in begin [:result :cycle/id])
        ;; Gate check on fresh cycle — most gates should fail
        result (tools/execute-tool backend :gate-check ["P-test" cycle-id])]
    (is (:ok result))
    (is (not (get-in result [:result :all-passed?])))
    ;; G5 should pass (blocker exists)
    (is (:passed? (first (filter #(= :G5-scope (:gate %))
                                  (get-in result [:result :gates])))))))

;; =============================================================================
;; Shape validation
;; =============================================================================

(deftest proof-state-validates-against-shape
  (let [state (pb/make-initial-state "P-val" "Prove foo" "All cases")]
    (is (nil? (ps/validate ps/ProofState state)))))

;; =============================================================================
;; DAG pure functions
;; =============================================================================

(deftest dag-acyclicity-pure
  (testing "empty ledger is acyclic"
    (is (:acyclic? (dag/acyclic? {}))))

  (testing "linear chain is acyclic"
    (let [ledger {"A" {:item/id "A" :item/depends-on #{} :item/unlocks #{"B"}}
                  "B" {:item/id "B" :item/depends-on #{"A"} :item/unlocks #{"C"}}
                  "C" {:item/id "C" :item/depends-on #{"B"} :item/unlocks #{}}}]
      (is (:acyclic? (dag/acyclic? ledger)))))

  (testing "cycle detected"
    (let [ledger {"A" {:item/id "A" :item/depends-on #{"C"} :item/unlocks #{"B"}}
                  "B" {:item/id "B" :item/depends-on #{"A"} :item/unlocks #{"C"}}
                  "C" {:item/id "C" :item/depends-on #{"B"} :item/unlocks #{"A"}}}]
      (is (not (:acyclic? (dag/acyclic? ledger)))))))

(deftest dag-impact-scoring-pure
  (let [ledger {"A" {:item/id "A" :item/depends-on #{} :item/unlocks #{"B" "C"}}
                "B" {:item/id "B" :item/depends-on #{"A"} :item/unlocks #{"D"}}
                "C" {:item/id "C" :item/depends-on #{"A"} :item/unlocks #{}}
                "D" {:item/id "D" :item/depends-on #{"B"} :item/unlocks #{}}}
        scores (dag/impact-scores ledger)]
    ;; A transitively unlocks B, C, D (score 3)
    (is (= 3 (:score (first scores))))
    (is (= "A" (:item-id (first scores))))))

(deftest dag-dangling-refs
  (let [ledger {"A" {:item/id "A" :item/depends-on #{"MISSING"} :item/unlocks #{}}}]
    (is (= #{"MISSING"} (dag/dangling-refs ledger)))))

;; =============================================================================
;; Corpus check
;; =============================================================================

(deftest corpus-check-rejects-empty-query
  (let [{:keys [backend]} (make-test-backend)
        result (tools/execute-tool backend :corpus-check [""])]
    (is (not (:ok result)))
    (is (= :invalid-query (get-in result [:error :code])))))

(deftest corpus-check-rejects-nil-query
  (let [{:keys [backend]} (make-test-backend)
        result (tools/execute-tool backend :corpus-check [nil])]
    (is (not (:ok result)))
    (is (= :invalid-query (get-in result [:error :code])))))

(deftest corpus-check-reports-missing-futon3a
  (let [dir (fix/temp-dir!)
        backend (pb/make-proof-backend {:cwd dir
                                        :futon3a-root "/nonexistent/futon3a"}
                                       (tools/make-mock-backend))
        result (tools/execute-tool backend :corpus-check
                 ["Cameron-Martin quasi-invariance for Phi^4_3 measure"])]
    (is (not (:ok result)))
    (is (= :corpus-unavailable (get-in result [:error :code])))))
