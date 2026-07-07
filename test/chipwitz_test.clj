;; chipwitz_test.clj — M-chipwitz-corps warrant-check layer tests.
;;
;; Tests the PURE functions: find-warrant, determined-fork?, make-psr,
;; make-nag, autopen-rate, threshold-adjustment, chipwitz-gate.
;; The cascade-policy-fn is INJECTED (mock) so no constructor dependency.

(require '[futon3c.aif.chipwitz :as cw])

(def fails (atom 0))
(defn t [nm p]
  (if p
    (println "  PASS" nm)
    (do (swap! fails inc) (println "  FAIL" nm))))

;; Mock cascade-policy-fn: simulates the warrant-finder returning scored patterns
(defn mock-cascade-strong
  "Mock: returns a cascade with a strong top pattern (rel 0.55 — above threshold)."
  [psi budget]
  {:shown [{:pattern "aif/expected-free-energy-scorecard" :rel 0.55}
           {:pattern "aif/predictive-entropy-as-ambiguity" :rel 0.44}]
   :size 2})

(defn mock-cascade-weak
  "Mock: returns a cascade with weak patterns (max rel 0.30 — below threshold)."
  [psi budget]
  {:shown [{:pattern "some/weak-pattern" :rel 0.30}]
   :size 1})

(defn mock-cascade-empty
  "Mock: returns an empty cascade (no patterns found)."
  [psi budget]
  {:shown [] :size 0})

;; ===== TEST 1: determined-fork? finds a warrant when rel > threshold =====
(println "\n=== TEST 1: determined-fork? with strong cascade ===")
(let [warrant (cw/determined-fork? mock-cascade-strong
                                   {:psi "test" :description "A/B fork" :options ["A" "B"]}
                                   6)]
  (t "warrant found" (some? warrant))
  (t "warrant pattern is the strong one" (= "aif/expected-free-energy-scorecard" (:pattern-id warrant)))
  (t "warrant rel is 0.55" (= 0.55 (:rel warrant)))
  (t "warrant determines" (:determines? warrant)))

;; ===== TEST 2: determined-fork? returns nil when rel < threshold =====
(println "\n=== TEST 2: determined-fork? with weak cascade ===")
(let [warrant (cw/determined-fork? mock-cascade-weak
                                   {:psi "test" :description "undetermined fork" :options ["X" "Y"]}
                                   6)]
  (t "no warrant found (nil)" (nil? warrant)))

;; ===== TEST 3: determined-fork? returns nil on empty cascade =====
(println "\n=== TEST 3: determined-fork? with empty cascade ===")
(let [warrant (cw/determined-fork? mock-cascade-empty
                                   {:psi "test" :description "no patterns" :options ["A" "B"]}
                                   6)]
  (t "no warrant found (nil)" (nil? warrant)))

;; ===== TEST 4: make-psr produces a valid proto-PSR =====
(println "\n=== TEST 4: make-psr produces valid proto-PSR ===")
(let [warrant {:pattern-id "aif/expected-free-energy-scorecard" :rel 0.55 :determines? true}
      choice-point {:description "A/B fork: full vs partial discharge" :options ["A" "B"]}
      psr (cw/make-psr {:choice-point choice-point :warrant warrant :chosen-option "B"})]
  (t "psr has pattern-id" (= "aif/expected-free-energy-scorecard" (:psr/pattern-id psr)))
  (t "psr has chosen-option B" (= "B" (:psr/chosen-option psr)))
  (t "psr has rationale containing pattern name" (clojure.string/includes? (:psr/rationale psr) "expected-free-energy"))
  (t "psr has timestamp" (some? (:psr/at psr))))

;; ===== TEST 5: make-nag produces a legible gap =====
(println "\n=== TEST 5: make-nag produces legible gap ===")
(let [cascade-result {:shown [{:pattern "weak/best-effort" :rel 0.30}] :size 1}
      nag (cw/make-nag {:choice-point {:description "genuinely undetermined" :options ["X" "Y"]}
                        :cascade-result cascade-result
                        :threshold 0.45})]
  (t "nag is not warranted" (false? (:nag/warranted nag)))
  (t "nag has gap text" (clojure.string/includes? (:nag/gap nag) "No pattern above threshold"))
  (t "nag has unblock text" (some? (:nag/unblock nag)))
  (t "nag has best-effort pattern" (= "weak/best-effort" (:nag/best-effort-pattern nag))))

;; ===== TEST 6: autopen-rate on clean PXR records (0% autopen) =====
(println "\n=== TEST 6: autopen-rate on clean records ===")
(let [pxrs (for [i (range 10)]
             {:psr {:pattern-id "test"}
              :pur {:outcome :success :prediction-error 0.1}})
      rate (cw/autopen-rate pxrs)]
  (t "rate is 0.0" (= 0.0 (:rate rate)))
  (t "n-total is 10" (= 10 (:n-total rate)))
  (t "n-autopen is 0" (= 0 (:n-autopen rate))))

;; ===== TEST 7: autopen-rate on rubber-stamp records (high autopen) =====
(println "\n=== TEST 7: autopen-rate on rubber-stamp records ===")
(let [pxrs (for [i (range 10)]
             {:psr {:pattern-id "test"}
              :pur {:outcome nil :prediction-error nil}})
      rate (cw/autopen-rate pxrs)]
  (t "rate is 1.0 (all autopen)" (= 1.0 (:rate rate)))
  (t "n-autopen is 10" (= 10 (:n-autopen rate))))

;; ===== TEST 8: autopen-rate on mixed records =====
(println "\n=== TEST 8: autopen-rate on mixed records ===")
(let [pxrs (concat (for [i (range 7)]   ; 7 good
                     {:pur {:outcome :success :prediction-error 0.1}})
                   (for [i (range 3)]   ; 3 autopen
                     {:pur {:outcome nil}}))
      rate (cw/autopen-rate pxrs)]
  (t "rate is 0.3" (= 0.3 (:rate rate)))
  (t "n-autopen is 3" (= 3 (:n-autopen rate))))

;; ===== TEST 9: threshold-adjustment raises on high autopen =====
(println "\n=== TEST 9: threshold-adjustment raises on high autopen ===")
(let [adj (cw/threshold-adjustment 0.45 {:rate 0.5 :n-total 10 :n-autopen 5})]
  (t "direction is :raise" (= :raise (:direction adj)))
  (t "new-threshold > current" (> (:new-threshold adj) 0.45)))

;; ===== TEST 10: threshold-adjustment holds on insufficient data =====
(println "\n=== TEST 10: threshold-adjustment holds on insufficient data ===")
(let [adj (cw/threshold-adjustment 0.45 {:rate 0.5 :n-total 3 :n-autopen 2})]
  (t "direction is :hold" (= :hold (:direction adj)))
  (t "reason mentions insufficient" (clojure.string/includes? (:reason adj) "insufficient")))

;; ===== TEST 11: chipwitz-gate returns warranted-proceed on strong cascade =====
(println "\n=== TEST 11: chipwitz-gate warranted-proceed ===")
(let [result (cw/chipwitz-gate mock-cascade-strong
                                {:psi "test" :description "fork" :options ["A" "B"]}
                                6)]
  (t "decision is :warranted-proceed" (= :warranted-proceed (:decision result)))
  (t "has :psr" (some? (:psr result)))
  (t "psr has pattern-id" (some? (:psr/pattern-id (:psr result)))))

;; ===== TEST 12: chipwitz-gate returns NAG on weak cascade =====
(println "\n=== TEST 12: chipwitz-gate returns NAG ===")
(let [result (cw/chipwitz-gate mock-cascade-weak
                                {:psi "test" :description "undetermined" :options ["X" "Y"]}
                                6)]
  (t "decision is :nag" (= :nag (:decision result)))
  (t "has :nag" (some? (:nag result)))
  (t "nag has gap" (some? (:nag/gap (:nag result)))))

;; ===== SUMMARY =====
(println "\n" (if (zero? @fails) "ALL TESTS PASS" (str @fails " TESTS FAILED")))
