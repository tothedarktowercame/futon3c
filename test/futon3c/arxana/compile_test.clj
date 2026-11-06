(ns futon3c.arxana.compile-test
  "INSTANTIATE increment 7 (Compile) tests: deterministic candidate identity,
  trusted vs conditional intent, ambiguity/stale propagation, extension
  refusal, separate admission, non-executability, immutable prediction, and
  negative-fixture refusal (D6 result algebra and tripwires)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.compile.admission :as admission]
            [futon3c.arxana.compile.compiler :as compiler]
            [futon3c.arxana.compile.intent :as intent]))

;;; Fixtures -------------------------------------------------------------------

(def interpreter
  {:interpreter/id "interp/test-human-1"
   :interpreter/type :human
   :interpreter/method "manual-reading"})

(def trusted-profile
  (intent/make-trust-profile
   {:trust-profile/id "trust/test" :trust-profile/version "v1"
    :trusted-interpreters {"interp/test-human-1" true}}))

(def untrusted-profile
  (intent/make-trust-profile
   {:trust-profile/id "trust/test" :trust-profile/version "v1"
    :trusted-interpreters {}}))

(defn test-intent
  ([] (test-intent {}))
  ([overrides]
   (merge (intent/make-intent
           {:intent/type :edit/replace
            :intent/source-act-id "evidence/act-1"
            :intent/target-ref {:manifestation/id "mft/tex-section-1"}
            :intent/proposed-change {:replacement "Objects and Rules"}
            :interpreter interpreter
            :interpreted-at "2026-11-06T20:00:00Z"})
          overrides)))

(def resolution-assertion
  {:resolution-assertion/id "res-assert-1"
   :evidence/outcome :unique
   :artifact/revision-id "rev-tex-1"})

(def correspondence-assertion
  {:correspondence-assertion/id "corr-assert-1"
   :verification :verified})

(defn grounding
  ([] (grounding {}))
  ([{:keys [resolution correspondences]}]
   {:artifact/revision-id "rev-tex-1"
    :resolution-assertion (or resolution resolution-assertion)
    :correspondence-assertions (or correspondences [correspondence-assertion])
    :grounding/predicted-effects [{:type :revision/creates :artifact/id "art-1"}
                                  {:type :correspondence/stales :ids ["corr-assert-1"]}]
    :grounding/risks [{:type :derived-artifact/rebuild-required}]}))

(defn compile-input
  ([] (compile-input {}))
  ([{:keys [intent profile ground extensions]}]
   {:source-act/id "evidence/act-1"
    :intent (or intent (test-intent))
    :trust-profile (or profile trusted-profile)
    :grounding (or ground (grounding))
    :extensions extensions}))

;;; Deterministic candidate identity -------------------------------------------

(deftest deterministic-candidate-test
  (let [r1 (compiler/compile-action (compile-input))
        r2 (compiler/compile-action (compile-input))]
    (is (= :compiled (:status r1)))
    (is (= (get-in r1 [:candidate-action :candidate-action/id])
           (get-in r2 [:candidate-action :candidate-action/id]))
        "same semantic input → same candidate ID (tripwire 1)")
    (is (= r1 r2) "compilation is deterministic and idempotent")))

(deftest candidate-id-excludes-presentation-test
  (let [base (compiler/compile-action (compile-input))
        ;; a reinterpretation timestamp / request identity is not semantic
        r2 (compiler/compile-action
            (compile-input {:intent (test-intent {"extra/request-id" "req-999"})}))]
    (is (= (get-in base [:candidate-action :candidate-action/id])
           (get-in r2 [:candidate-action :candidate-action/id]))
        "non-semantic intent metadata does not change candidate ID")))

(deftest removing-grounding-changes-compilation-test
  (let [base (compiler/compile-action (compile-input))
        no-corr (compiler/compile-action
                 (compile-input {:ground (grounding {:correspondences []})}))]
    (is (not= (get-in base [:candidate-action :candidate-action/id])
              (get-in no-corr [:candidate-action :candidate-action/id]))
        "removing a warrant changes the candidate (tripwire 2)")))

;;; Trust ------------------------------------------------------------------------

(deftest trusted-vs-conditional-intent-test
  (testing "trusted interpreter + grounded evidence → :compiled"
    (is (= :compiled (:status (compiler/compile-action (compile-input))))))
  (testing "provenanced but untrusted interpreter → only :conditional"
    (let [r (compiler/compile-action (compile-input {:profile untrusted-profile}))]
      (is (= :conditional (:status r)))
      (is (= :conditional (:trust r)))
      (is (some? (:candidate-action r)) "conditional still carries a candidate")))
  (testing "client-attested intent under a permissive profile → :conditional"
    (let [client-intent {:intent/type :edit/replace
                         :intent/source-act-id "evidence/act-1"
                         :intent/target-ref {:manifestation/id "mft/tex-section-1"}
                         :intent/proposed-change {:replacement "x"}
                         :intent/interpreter nil}
          profile (intent/make-trust-profile
                   {:trust-profile/id "t" :trust-profile/version "v1"
                    :allow-client-attested? true})
          r (compiler/compile-action
             (compile-input {:intent client-intent :profile profile}))]
      (is (= :conditional (:status r)) "never :compiled without provenance")))
  (testing "unprovenanced intent under strict profile → :invalid, never compiled"
    (let [bad {:intent/type :edit/replace
               :intent/source-act-id "evidence/act-1"
               :intent/target-ref {:manifestation/id "mft/tex-section-1"}
               :intent/proposed-change {:replacement "x"}
               :intent/interpreter nil}
          r (compiler/compile-action (compile-input {:intent bad}))]
      (is (= :invalid (:status r)))
      (is (nil? (:candidate-action r))))))

;;; Ambiguity and staleness --------------------------------------------------------

(deftest ambiguity-preserved-test
  (let [ambig-res {:resolution-assertion/id "res-assert-ambig"
                   :evidence/outcome :ambiguous
                   :evidence/alternatives [{:candidate/id "cand-a"}
                                           {:candidate/id "cand-b"}]
                   :artifact/revision-id "rev-tex-1"}
        r (compiler/compile-action
           (compile-input {:ground (grounding {:resolution ambig-res})}))]
    (is (#{:ambiguous :conditional} (:status r))
        "ambiguous grounding is never silently compiled")
    (is (not= :compiled (:status r)))
    (is (seq (:alternatives r))
        "alternatives are retained, never first-candidate selection (tripwire 3)")))

(deftest accepted-ambiguity-still-not-silent-test
  (let [accepted {:resolution-assertion/id "res-assert-ambig"
                  :evidence/outcome :ambiguous
                  :resolution-acceptance/id "accept-1"
                  :evidence/alternatives [{:candidate/id "cand-a"}
                                          {:candidate/id "cand-b"}]
                  :artifact/revision-id "rev-tex-1"}
        r (compiler/compile-action
           (compile-input {:ground (grounding {:resolution accepted})}))]
    (is (= :ambiguous (:status r))
        "acceptance lets ambiguity be cited; alternatives remain (invariant 5)")
    (is (= 2 (count (:alternatives r))))))

(deftest stale-grounding-test
  (let [stale-corr (assoc correspondence-assertion :verification :stale)
        r (compiler/compile-action
           (compile-input {:ground (grounding {:correspondences [stale-corr]})}))]
    (is (= :stale-input (:status r)))
    (is (nil? (:candidate-action r))
        "stale input refuses compilation, it does not emit a candidate")))

(deftest weak-correspondence-conditional-test
  (let [weak (assoc correspondence-assertion :verification :unverified)
        r (compiler/compile-action
           (compile-input {:ground (grounding {:correspondences [weak]})}))]
    (is (= :conditional (:status r)))
    (is (some #(= :correspondence/verification-obligation (:type %))
              (:obligations r)))))

(deftest negative-fixture-never-compiled-test
  (testing "contradictory draft8-style fixture: ambiguous + stale, no acceptance"
    (let [bad-res {:resolution-assertion/id "res-bad"
                   :evidence/outcome :ambiguous
                   :artifact/revision-id "rev-tex-1"}
          stale-corr (assoc correspondence-assertion :verification :stale)
          r (compiler/compile-action
             (compile-input {:ground (grounding {:resolution bad-res
                                                 :correspondences [stale-corr]})}))]
      (is (#{:stale-input :conditional :ambiguous :invalid} (:status r)))
      (is (not= :compiled (:status r))
          "contradictory grounding can never produce :compiled (tripwire 8)"))))

;;; Extensions ----------------------------------------------------------------------

(deftest extension-refusal-test
  (testing "extension redefining a core field is refused"
    (let [r (compiler/compile-action
             (compile-input {:extensions {:operation {:type :artifact/delete}}}))]
      (is (= :unsupported (:status r)))
      (is (nil? (:candidate-action r)))))
  (testing "bare (unqualified) extension key is refused"
    (let [r (compiler/compile-action
             (compile-input {:extensions {:score 42}}))]
      (is (= :unsupported (:status r)))))
  (testing "unknown namespaced extension is preserved opaque and changes nothing"
    (let [base (compiler/compile-action (compile-input))
          ext (compiler/compile-action
               (compile-input {:extensions {:circus/recall-score 0.87}}))]
      (is (= :compiled (:status ext)))
      (is (= 0.87 (get-in ext [:candidate-action :extensions :circus/recall-score])))
      (is (= (get-in base [:candidate-action :candidate-action/id])
             (get-in ext [:candidate-action :candidate-action/id]))
          "consumer scores cannot alter core candidate ID (tripwire 5)"))))

;;; Admission, execution boundary, receipts --------------------------------------------

(deftest admission-is-separate-record-test
  (let [r (compiler/compile-action (compile-input))
        cand (:candidate-action r)
        decision (admission/make-admission-decision
                  {:decision/status :admitted
                   :policy/id "policy/test" :policy/revision "p1"
                   :actor "operator-1"
                   :candidate-action/id (:candidate-action/id cand)})]
    (is (compiler/non-executable? cand)
        "candidate carries no credential, token, consent or executor (tripwire 4)")
    (is (not (contains? cand :admission/id))
        "candidate contains no admission reference")
    (is (admission/valid-decision? decision))
    (is (= (:candidate-action/id cand) (:candidate-action/id decision))
        "decision names the candidate; the records are distinct")
    (is (not= (:candidate-action/id cand) (:admission/id decision)))))

(deftest receipt-cannot-rewrite-prediction-test
  (let [r (compiler/compile-action (compile-input))
        cand (:candidate-action r)
        decision (admission/make-admission-decision
                  {:decision/status :admitted
                   :policy/id "policy/test" :policy/revision "p1"
                   :actor "operator-1"
                   :candidate-action/id (:candidate-action/id cand)})
        before-predicted (:predicted-effects cand)
        receipt (admission/make-receipt
                 {:request/id "req-1" :operation/id "op-1"
                  :candidate-action/id (:candidate-action/id cand)
                  :admission/id (:admission/id decision)
                  :revision/before "rev-tex-1" :revision/after "rev-tex-2"
                  :actual-effects [{:type :revision/created :artifact/id "art-1"}]
                  :deviations [{:type :renderer/not-run}]
                  :outcome :applied
                  :evidence ["evidence/exec-1"]})]
    (is (admission/valid-receipt? receipt))
    (is (= before-predicted (:predicted-effects cand))
        "candidate prediction is untouched by the receipt (tripwire 7)")
    (is (= [{:type :renderer/not-run}] (:receipt/deviations receipt))
        "deviations are recorded as deviations, not merged into prediction")
    (is (thrown? clojure.lang.ExceptionInfo
                 (admission/make-receipt
                  {:candidate-action/id "c" :admission/id "a"
                   :outcome :applied
                   :predicted-effects [{:type :revision/creates}]}))
        "a receipt carrying predicted-effects is refused")))
