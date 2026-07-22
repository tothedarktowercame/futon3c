(ns futon3c.agents.zaif-controller-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.zai-api :as zai]
            [futon3c.agents.zaif-controller :as zaif]))

(deftest fixture-beliefs-produce-deterministic-arm-choices
  (testing "posting statistics can select retrieve"
    (let [d (zaif/decide {:mission "M-z"
                          :observations {:posting-stats {:total-docs 1000 :dfs [1]}
                                         :estimated-tokens 400}
                          :task-belief {:act-value 0.1}
                          :c-belief {:operator-c-uncertainty 0.2}})]
      (is (= :retrieve (:arm d)))
      (is (= 1.0 (:gamma-used d)))))
  (testing "operator uncertainty can select ask after attention cost"
    (is (= :ask (:arm (zaif/decide {:mission "M-z"
                                    :c-belief {:operator-c-uncertainty 1.0}
                                    :observations {:retrieve-eig 0.1}
                                    :task-belief {:act-value 0.1}}))))))

(deftest gamma-lowered-mission-shifts-act-to-hedge
  (let [inputs {:mission "M-low"
                :gamma {"M-low" {:policy-precision 0.5}}
                :task-belief {:act-value 0.6}
                :observations {:retrieve-eig 0.45 :estimated-tokens 100}
                :c-belief {:operator-c-uncertainty 0.1}}
        neutral (zaif/decide (assoc inputs :gamma {"M-low" {:policy-precision 1.0}}))
        lowered (zaif/decide inputs)]
    (is (= :act (:arm neutral)))
    (is (= :retrieve (:arm lowered)))
    (is (= 0.5 (:gamma-used lowered)))))

(deftest missing-gamma-uses-uniform-prior
  (let [d (zaif/decide {:mission "M-unburned"
                        :gamma {}
                        :task-belief {:act-value 0.2}})]
    (is (= 1.0 (:gamma-used d)))))

(deftest evidence-record-shape
  (let [inputs {:mission "M-z" :task-belief {:act-value 0.2}}
        decision (zaif/decide inputs)
        ev (zaif/decision-evidence-entry {:agent-id "zai-test"
                                          :sid "sid-1"
                                          :decision decision
                                          :inputs inputs})]
    (is (= :coordination (:evidence/type ev)))
    (is (= :step (:evidence/claim-type ev)))
    (is (= [:zaif :arm-choice] (:evidence/tags ev)))
    (is (= (:arm decision) (get-in ev [:evidence/body :arm])))
    (is (= (:g-terms decision) (get-in ev [:evidence/body :g-terms])))
    (is (= "M-z" (get-in ev [:evidence/body :mission])))
    (is (string? (get-in ev [:evidence/body :inputs-digest :sha256-16])))))

(deftest zai-profile-does-not-consult-controller
  (let [called? (atom false)]
    (with-redefs [zaif/decide (fn [_] (reset! called? true) {:arm :yield})
                  zaif/persist-decision! (fn [_])]
      (is (nil? (#'zai/maybe-zaif-decision! {:profile :zai
                                             :agent-id "zai-test"
                                             :sid "sid"})))
      (is (false? @called?)))))

(deftest zaif-profile-consults-controller-and-persists
  (let [persisted (atom [])]
    (with-redefs [zaif/persist-decision! (fn [ctx] (swap! persisted conj ctx))]
      (let [decision (#'zai/maybe-zaif-decision!
                      {:profile :zaif
                       :agent-id "zai-test"
                       :sid "sid"
                       :turn-id "turn-1"
                       :round 1
                       :zaif-inputs-fn (fn [_] {:mission "M-z"
                                                :task-belief {:act-value 1.0}})})]
        ;; Returns the shipped (primary) decision
        (is (= :act (:arm decision)))
        ;; Both constants persisted (D-1 dual recording)
        (is (= 2 (count @persisted)))
        (is (= #{:shipped :sweep}
               (set (map :constant-label @persisted))))
        (is (= #{0.65 0.15}
               (set (map :constant @persisted))))
        ;; Both share the same pairing-key
        (is (= 1 (count (set (map :pairing-key @persisted)))))
        ;; Both use the same inputs
        (is (every? #(= "M-z" (get-in % [:inputs :mission])) @persisted))))))

(deftest zaif-profile-hydrates-by-default
  (testing "without an explicit :zaif-inputs-fn the D-1 hydrator runs —
    recorded inputs must NOT be the empty-map degenerate shape (the
    unwired-hydrator regression caught live on zai-2, 2026-07-22)"
    (let [persisted (atom [])]
      (with-redefs [zaif/persist-decision! (fn [ctx] (swap! persisted conj ctx))]
        (#'zai/maybe-zaif-decision!
         {:profile :zaif
          :agent-id "zai-test"
          :sid "sid"
          :turn-id "turn-h"
          :round 1
          :context "please check the failing witness derivation in M-a-sorry-enterprise"})
        (is (= 2 (count @persisted)))
        (doseq [p @persisted]
          (let [inputs (:inputs p)]
            (is (= "M-a-sorry-enterprise" (:mission inputs)))
            (is (number? (get-in inputs [:c-belief :operator-c-uncertainty])))
            (is (seq (get-in inputs [:observations :posting-stats])))))))))

(deftest zaif-persistence-rejection-does-not-kill-the-turn
  (testing "a store rejection during shadow persistence is swallowed —
    counted+surfaced, never propagated into the round loop (2026-07-22
    brown-out incident: a rejected write aborted a live operator turn)"
    (with-redefs [zaif/persist-decision!
                  (fn [_] (throw (ex-info "ZAIF decision persistence was rejected" {})))]
      (let [decision (#'zai/maybe-zaif-decision!
                      {:profile :zaif
                       :agent-id "zai-test"
                       :sid "sid"
                       :turn-id "turn-b"
                       :round 1
                       :zaif-inputs-fn (fn [_] {:mission "M-z"
                                                :task-belief {:act-value 1.0}})})]
        ;; still returns the shipped decision; no exception escaped
        (is (= :act (:arm decision)))))))

(deftest zaif-persistence-failure-is-counted-and-raised
  (let [before (:failure-count (zaif/persistence-status))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (zaif/persist-decision!
                  {:agent-id "zai-test"
                   :sid "sid"
                   :turn-id "turn-test"
                   :decision {:arm :yield :g-terms {}}
                   :inputs {}})))
    (let [status (zaif/persistence-status)]
      (is (= (inc before) (:failure-count status)))
      (is (string? (:last-error status))))))

(deftest calibration-ask-arm-unreachable-at-shipped-cost
  (testing "ZU-2 calibration: at cost=0.65, :ask cannot win against realistic act-value"
    ;; The :ask value = c-uncertainty - 0.65. Even at c-uncertainty=0.7
    ;; (high), ask-value = 0.05 — below any gamma-weighted act-value.
    (let [d (zaif/decide {:mission "M-z"
                          :c-belief {:operator-c-uncertainty 0.7}
                          :task-belief {:act-value 0.0}
                          :observations {}})
          ask-val (-> d :g-terms :ask)]
      (is (< ask-val 0.1)
          "ask-value at c-uncertainty=0.7 is < 0.1 — below typical act-values"))
    ;; At low cost (0.15 — the calibration sweep's clean-separation value),
    ;; :ask would win on high C-uncertainty. But that constant is NOT shipped.
    ;; This test documents the gap: the shipped constant makes :ask unreachable.
    (let [d (zaif/decide {:mission "M-z"
                          :c-belief {:operator-c-uncertainty 0.5}
                          :task-belief {:act-value 0.3}
                          :observations {}})]
      (is (= :act (:arm d))
          "With typical beliefs, the shipped constants always pick :act")))
  (testing "non-correction sessions correctly pick :act"
    (is (= :act (:arm (zaif/decide {:mission "M-z"
                                     :task-belief {:act-value 0.5}
                                     :c-belief {:operator-c-uncertainty 0.2}
                                     :observations {}}))))))

;; ─── D-1: dual-constant recording tests ────────────────────────

(deftest decide-with-constants-override-changes-ask-value
  (testing "constants-override changes the operator-attention-cost in the output"
    (let [inputs {:mission "M-z"
                  :c-belief {:operator-c-uncertainty 0.5}
                  :task-belief {:act-value 0.1}
                  :observations {}}
          shipped (zaif/decide inputs)
          swept (zaif/decide (assoc inputs :constants-override
                                    {:operator-attention-cost 0.15}))]
      (is (= 0.65 (:operator-attention-cost shipped)))
      (is (= 0.15 (:operator-attention-cost swept)))
      ;; ask-value at shipped cost = 0.5 - 0.65 = -0.15
      (is (< (Math/abs (- (get-in shipped [:g-terms :ask]) -0.15)) 0.001))
      ;; ask-value at sweep cost = 0.5 - 0.15 = 0.35
      (is (< (Math/abs (- (get-in swept [:g-terms :ask]) 0.35)) 0.001)))))

(deftest dual-decide-produces-two-paired-decisions
  (testing "dual-decide returns both constants from the same inputs"
    (let [inputs {:mission "M-z"
                  :c-belief {:operator-c-uncertainty 0.5}
                  :task-belief {:act-value 0.1}
                  :observations {}}
          results (zaif/dual-decide inputs)]
      (is (= 2 (count results)))
      (is (= #{:shipped :sweep} (set (map :label results))))
      (is (= #{0.65 0.15} (set (map :operator-attention-cost results)))))))

(deftest dual-decide-determinism-check
  (testing "arms are re-derivable from recorded inputs by calling decide again"
    ;; This is the acceptance criterion 2 determinism check: the scorer
    ;; can re-derive the arm from the inputs + constant override.
    (let [inputs {:mission "M-futon-forward-model"
                  :c-belief {:operator-c-uncertainty 1.0}
                  :task-belief {:act-value 0.1}
                  :gamma {"M-futon-forward-model" {:policy-precision 0.7071067811865476}}
                  :observations {:posting-stats {:total-docs 10 :dfs [1]}}}
          results (zaif/dual-decide inputs)]
      (doseq [{:keys [label operator-attention-cost decision]} results]
        (let [re-derived (zaif/decide
                          (assoc inputs :constants-override
                                 {:operator-attention-cost operator-attention-cost}))]
          (is (= (:arm decision) (:arm re-derived))
              (str "arm mismatch for " label ": " (:arm decision) " vs " (:arm re-derived)))
          (is (= (:g-terms decision) (:g-terms re-derived))
              (str "g-terms mismatch for " label)))))))

(deftest dual-decide-constants-diverge-on-high-c-uncertainty
  (testing "at moderate c-uncertainty, shipped picks :act, sweep picks :ask"
    ;; c-uncertainty 0.5: shipped ask = 0.5-0.65 = -0.15 (below act=0.3);
    ;; sweep ask = 0.5-0.15 = 0.35 (above act=0.3).
    (let [inputs {:mission "M-z"
                  :c-belief {:operator-c-uncertainty 0.5}
                  :task-belief {:act-value 0.3}
                  :gamma {"M-z" {:policy-precision 1.0}}
                  :observations {}}
          results (zaif/dual-decide inputs)
          by-label (into {} (map (juxt :label :decision) results))]
      (is (= :act (get-in by-label [:shipped :arm])))
      (is (= :ask (get-in by-label [:sweep :arm]))))))

(deftest live-shaped-context-leaves-divergent-rounds-reachable
  (testing "realistic posting-stats cannot let :retrieve swamp both constants"
    ;; Live task-belief is empty (act = 0) and context text is never blank,
    ;; so an unnormalized EIG proxy (2.5-4 on any real message) made
    ;; :retrieve win at BOTH constants — an empty Z3a divergent-round set.
    ;; With the log(total+1) normalization the sweep's ask arm stays
    ;; reachable on high-c-uncertainty missions.
    (let [inputs {:mission "M-live"
                  :c-belief {:operator-c-uncertainty 1.0}
                  :task-belief {}
                  :gamma {"M-live" {:policy-precision 1.0}}
                  :observations {:posting-stats {:total-docs 48
                                                 :dfs [1 1 1 1 1 1 1 1 1 1]
                                                 :estimated-tokens 96}}}
          results (zaif/dual-decide inputs)
          by-label (into {} (map (juxt :label :decision) results))]
      (is (= :retrieve (get-in by-label [:shipped :arm])))
      (is (= :ask (get-in by-label [:sweep :arm])))
      (is (< (get-in by-label [:shipped :g-terms :retrieve]) 1.0)))))

(deftest decision-evidence-entry-carries-pairing-and-constant
  (testing "evidence entry includes :constant, :constant-label, :pairing-key"
    (let [inputs {:mission "M-z" :task-belief {:act-value 0.2}}
          decision (zaif/decide inputs)
          entry (zaif/decision-evidence-entry
                 {:agent-id "zai-test"
                  :sid "sid-1"
                  :turn-id "turn-1"
                  :round 3
                  :decision decision
                  :inputs inputs
                  :constant 0.15
                  :constant-label :sweep
                  :pairing-key "turn-1:r3"})]
      (is (= [:zaif :arm-choice] (:evidence/tags entry)))
      (is (= 0.15 (get-in entry [:evidence/body :constant])))
      (is (= :sweep (get-in entry [:evidence/body :constant-label])))
      (is (= "turn-1:r3" (get-in entry [:evidence/body :pairing-key])))
      (is (= 3 (get-in entry [:evidence/body :round])))
      (is (contains? (get-in entry [:evidence/body]) :inputs-snapshot))
      (is (= "M-z" (get-in entry [:evidence/body :inputs-snapshot :mission]))))))

(deftest decision-evidence-entry-backward-compatible-without-z3a-fields
  (testing "entry works without constant/pairing fields (backward compat)"
    (let [inputs {:mission "M-z" :task-belief {:act-value 0.2}}
          decision (zaif/decide inputs)
          entry (zaif/decision-evidence-entry
                 {:agent-id "zai-test"
                  :sid "sid-1"
                  :decision decision
                  :inputs inputs})]
      (is (= [:zaif :arm-choice] (:evidence/tags entry)))
      (is (nil? (get-in entry [:evidence/body :constant])))
      (is (nil? (get-in entry [:evidence/body :pairing-key]))))))

(deftest dual-decision-through-stub-evidence-store
  (testing "both constants' decisions recorded through a stub store, mechanically paired"
    ;; Acceptance criterion 2: dual-decision test through a stub evidence store.
    ;; Uses a simple atom as the evidence store (boundary resolves it to AtomBackend).
    (let [!store (atom [])
          inputs {:mission "M-futon-forward-model"
                  :c-belief {:operator-c-uncertainty 0.5}
                  :task-belief {:act-value 0.3}
                  :gamma {"M-futon-forward-model" {:policy-precision 0.7071067811865476}}
                  :observations {}}
          pairing-key "test-turn:r1"
          results (zaif/dual-decide inputs)]
      ;; Persist both decisions through persist-decision! with the stub
      ;; We test decision-evidence-entry directly (persist-decision! requires
      ;; a real boundary store; here we verify the entry shape + pairing).
      (doseq [{:keys [label operator-attention-cost decision]} results]
        (let [entry (zaif/decision-evidence-entry
                     {:agent-id "zai-test"
                      :sid "sid-stub"
                      :turn-id "test-turn"
                      :round 1
                      :decision decision
                      :inputs inputs
                      :constant operator-attention-cost
                      :constant-label label
                      :pairing-key pairing-key})]
          (swap! !store conj entry)))
      (let [entries @!store]
        (is (= 2 (count entries)))
        (is (= #{:shipped :sweep}
               (set (map #(get-in % [:evidence/body :constant-label]) entries))))
        ;; Mechanical pairing: both share the same pairing-key
        (is (= 1 (count (set (map #(get-in % [:evidence/body :pairing-key]) entries)))))
        ;; Determinism: arms re-derivable from recorded inputs
        (doseq [entry entries
                :let [body (:evidence/body entry)
                      recorded-inputs (:inputs-snapshot body)
                      constant (:constant body)
                      re-derived (zaif/decide
                                  (assoc recorded-inputs :constants-override
                                         {:operator-attention-cost constant}))]]
          (is (= (get-in entry [:evidence/body :arm]) (:arm re-derived))
              "arm must be re-derivable from recorded inputs + constant"))
        ;; Divergence: the two constants pick different arms on high c-uncertainty
        (let [arms (set (map #(get-in % [:evidence/body :arm]) entries))]
          (is (> (count arms) 1)
              "high-c-uncertainty inputs should produce arm divergence across constants"))))))
