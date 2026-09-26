(ns futon3c.aif.flight-record-test
  "M-first-flights build-order step 2: the close-time composer. The rule
   under test is the composition rule itself — apparatus facts become terms
   with grounds, pilot judgments pass through, absences become typed
   sorries, and NOTHING is fabricated (no guessed class, no invented
   warrant, no asserted verification)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.aif.flight-record :as fr]))

(def begin-fixture
  {:run-id "live-test-0001" :agent "claude-test"
   :v-attribution :operator-directed
   :begin-at "2026-06-12T12:00:00Z"
   :scan-as-of "2026-06-12T11:59:30Z"
   :cg-id "cg-test"
   :pre {:dT-snapshot [{:action {:type :advance-mission :target "M-x"} :g-total -4.1 :rank 1}
                       {:action {:type :address-sorry :target "sorry/y"} :g-total -4.3 :rank 2}]
         :v {:type :advance-mission :target "M-x"}
         :predicted-discharge -4.1
         :predicted-constant -4.05}})

(def window-fixture
  {:begin "2026-06-12T12:00:00Z" :commit "2026-06-12T12:00:18Z"
   :threshold "2026-06-12T12:01:43Z"
   :scans [{:as-of "2026-06-12T12:03:00Z" :g {:g -4.06 :g-grain :one-step-action}}
           {:as-of "2026-06-12T12:05:00Z" :g {:g -4.0601 :g-grain :one-step-action}}]
   :epsilon 0.005 :agreement 0.0001})

(defn- compose [overrides]
  (fr/compose-flight-record
   (merge {:run-id "live-test-0001" :begin begin-fixture :agent "claude-test"
           :predicted -4.1 :predicted-constant -4.05 :realised -4.0601
           :realised-source :measured :executed? false :evidence-ref nil
           :merge-event nil :frame-path "data/repl-traces/live-test-0001.edn"
           :logged-turn 99 :flight {}}
          overrides)))

(defn- cell-ok? [c]
  (or (and (map? c) (contains? c :judgment) (contains? c :ground))
      (and (map? c) (map? (:sorry c)) (keyword? (get-in c [:sorry :kind])))))

(deftest every-organ-is-term-or-sorry
  (let [r (compose {})]
    (testing "the cell calculus holds for every organ the composer emits"
      (doseq [[k c] (:organs r)]
        (is (cell-ok? c) (str k " is neither term nor typed sorry: " (pr-str c)))))
    (is (= "live-test-0001" (:flight/id r)))
    (is (= :full (:flight/derivation r)))))

(deftest producer-derived-grounds-are-typed
  (let [r (compose {})]
    (is (= :begin-scan (get-in r [:organs :field-read :ground :ground/kind])))
    (is (= :organ-ref (get-in r [:organs :velocity :ground :ground/kind])))
    (is (= :warrant (get-in r [:organs :velocity :ground :organ])))
    (is (= :model-output (get-in r [:organs :prediction :ground :ground/kind])))
    (is (= :missing-witness (get-in r [:organs :measurement :ground :ground/kind])))
    (is (= :derived (get-in r [:organs :counterfactual :ground :ground/kind])))))

(deftest nothing-is-fabricated
  (let [r (compose {})]
    (testing "no pilot judgment supplied -> typed sorries, not guesses"
      (is (= :not-yet (get-in r [:organs :warrant :sorry :kind])))
      (is (= :not-yet (get-in r [:organs :verification :sorry :kind])))
      (is (nil? (get-in r [:organs :measurement :judgment :class]))
          "class is the pilot's judgment; absent stays absent")
      (is (nil? (get-in r [:organs :window]))
          "no window supplied -> no window organ invented")
      (is (= :not-yet (get-in r [:organs :field-read :judgment :neighbourhood :sorry :kind]))
          "neighbourhood not externalised -> inner typed sorry"))))

(deftest proposal-mode-act-is-typed-absence
  (let [r (compose {:executed? false})]
    (is (= :proposal-mode (get-in r [:organs :act :sorry :kind])))))

(deftest executed-act-carries-witness-without-asserted-verification
  (let [r (compose {:executed? true :evidence-ref "futon7 abc1234"
                    :merge-event {:type :operator-merge :at "2026-06-12T12:06:00Z" :note "n"}})]
    (is (= :executed (get-in r [:organs :act :judgment :state])))
    (is (= "futon7 abc1234" (get-in r [:organs :act :judgment :witness :ref])))
    (is (= "claude-test" (get-in r [:organs :act :judgment :witness :verified-by])))
    (is (not (contains? (get-in r [:organs :act :judgment :witness]) :verification))
        "verification is never asserted on the pilot's behalf — F4 catches the gap honestly")
    (is (= :operator-merge (get-in r [:organs :out-of-band :judgment 0 :type]))
        "the merge event lands in the oob organ with the shared instant")))

(deftest pilot-supplied-grounds-pass-through
  (let [r (compose {:flight {:window window-fixture
                             :window-ground "two scans via test poll"
                             :class :clean
                             :warrant {:judgment {:determined? true
                                                  :determined-by {:kind :operator-direction
                                                                  :ref "queue#367"}}
                                       :ground "the queue entry"}
                             :plan-sketch ["edit" "commit" "settle" "close"]
                             :steers [{:type :operator-steer
                                       :at "2026-06-12T12:01:00Z"
                                       :content "fly tight"}]
                             :links [{:type :re-measures :to "live-prior"}]}})]
    (is (= window-fixture (get-in r [:organs :window :judgment])))
    (is (= :window (get-in r [:organs :measurement :judgment :window]))
        "measurement points at the thirteenth organ")
    (is (= :clean (get-in r [:organs :measurement :judgment :class])))
    (is (= :operator-direction (get-in r [:organs :warrant :judgment :determined-by :kind])))
    (is (= ["edit" "commit" "settle" "close"] (get-in r [:organs :velocity :judgment :plan-sketch])))
    (is (= :operator-steer (get-in r [:organs :out-of-band :judgment 0 :type])))
    (is (= [{:type :re-measures :to "live-prior"}] (:flight/links r)))))

(deftest flight-opt-contract-is-symmetric
  ;; the live-df706c45 step-read finding: warrant/verification needed
  ;; pre-wrapping while neighbourhood/window arrived bare — the first
  ;; stepping pilot tripped over the split. Both shapes must now work.
  (testing "bare judgments get wrapped, grounds derived from their own content"
    (let [r (compose {:flight {:warrant {:determined? true
                                         :determined-by {:kind :standing-contract
                                                         :ref "QUEUE.md#x"}}
                               :verification {:holes [{:hole "l:1" :class :bounded-doable
                                                       :evidence "- [ ] x"}]
                                              :chosen-hole "l:1"}
                               :window window-fixture}})]
      (is (= :standing-contract (get-in r [:organs :warrant :judgment :determined-by :kind])))
      (is (= "QUEUE.md#x" (get-in r [:organs :warrant :ground :ref]))
          "warrant ground derived from the ref inside the judgment")
      (is (= "l:1" (get-in r [:organs :verification :judgment :chosen-hole])))
      (is (= :inline-evidence (get-in r [:organs :verification :ground :ground/kind])))
      (is (= window-fixture (get-in r [:organs :window :judgment])))))
  (testing "pre-wrapped cells pass through untouched (no double-wrap)"
    (let [warrant-cell {:judgment {:determined? false :queued {:queue-ref "q#1"}}
                        :ground "the queue entry itself"}
          window-cell {:judgment window-fixture :ground "scans via poll p-1"}
          r (compose {:flight {:warrant warrant-cell :window window-cell}})]
      (is (= warrant-cell (get-in r [:organs :warrant])))
      (is (= window-cell (get-in r [:organs :window])))))
  (testing "pilot-synthesis warrant ground derives from the reasoning"
    (let [r (compose {:flight {:warrant {:determined? true
                                         :determined-by {:kind :pilot-synthesis
                                                         :reasoning "wall blocks pairs AND substrate is the safe default"}}}})]
      (is (= "wall blocks pairs AND substrate is the safe default"
             (get-in r [:organs :warrant :ground :reasoning]))))))

(deftest fallback-class-derives-mechanically
  (let [r (compose {:realised-source :target-absent-fallback :realised -4.1})]
    (is (= :fallback (get-in r [:organs :measurement :judgment :class]))
        "the censored fallback is the ONE mechanically derivable class")))

(deftest g-values-wear-grains-and-counterfactual-derives
  (let [r (compose {})]
    (doseq [path [[:organs :prediction :judgment :scaled]
                  [:organs :prediction :judgment :constant]
                  [:organs :begin-state :judgment :target-g]
                  [:organs :measurement :judgment :predicted]
                  [:organs :measurement :judgment :realised]]]
      (is (= :one-step-action (:g-grain (get-in r path))) (str path)))
    (is (= :not-yet (get-in r [:organs :prediction :judgment :policy :sorry :kind])))
    (is (= :rollout-engine (get-in r [:organs :prediction :judgment :policy :sorry :blocked-by])))
    (is (< (Math/abs (- 0.0101 (get-in r [:organs :counterfactual :judgment :constant-error]))) 1e-9)
        "constant-error = |realised - predicted-constant|, derived")))

(def frame-fixture
  {:run-id "live-old-0001" :agent "claude-3"
   :trace [{:v {:type :advance-mission :target "M-x"}
            :v-attribution :operator-directed
            :dT-snapshot [{:action {:type :advance-mission :target "M-x"} :g-total -4.1 :rank 1}]
            :predicted-discharge -4.12 :predicted-constant -4.08
            :realised-discharge -4.084
            :independent? true :evidence-ref "futon7 88b3cb1"
            :realised-source :measured :realised-read :settled}]})

(deftest backfill-is-thin-and-honest
  (let [r (fr/backfill-record frame-fixture)]
    (is (= :thin (:flight/derivation r)))
    (testing "every organ is still a cell"
      (doseq [[k c] (:organs r)]
        (is (cell-ok? c) (str k ": " (pr-str c)))))
    (testing "tags become terms; prose judgments become :derivation-thin sorries"
      (is (= :derivation-thin (get-in r [:organs :warrant :sorry :kind])))
      (is (= :derivation-thin (get-in r [:organs :verification :sorry :kind])))
      (is (= :derivation-thin (get-in r [:organs :begin-state :sorry :kind])))
      (is (= :executed (get-in r [:organs :act :judgment :state])))
      (is (= "futon7 88b3cb1" (get-in r [:organs :act :judgment :witness :ref])))
      (is (= :one-step-action (get-in r [:organs :measurement :judgment :realised :g-grain]))))
    (testing "clean-vs-null lived in prose — class honestly absent on settled measured pairs"
      (is (nil? (get-in r [:organs :measurement :judgment :class])))
      (is (= :settled (get-in r [:organs :measurement :judgment :realised-read]))))))

(deftest backfill-derives-only-mechanical-classes
  (let [fallback (assoc-in frame-fixture [:trace 0 :realised-source] :target-absent-fallback)
        transient* (assoc-in frame-fixture [:trace 0 :realised-read] :transient)]
    (is (= :fallback (get-in (fr/backfill-record fallback)
                             [:organs :measurement :judgment :class])))
    (is (= :transient (get-in (fr/backfill-record transient*)
                              [:organs :measurement :judgment :class])))))

(deftest backfill-proposal-mode-stays-typed-absence
  (let [proposal (update-in frame-fixture [:trace 0] dissoc :independent? :evidence-ref)
        r (fr/backfill-record proposal)]
    (is (= :proposal-mode (get-in r [:organs :act :sorry :kind]))
        "proposal-mode was a typed absence by design, not thinness")))

(deftest validity-mask-derives-never-authored
  (testing "the canonical producer-side mask agrees with the verifier's rule on the fixtures"
    (let [full-clean (compose {:flight {:window window-fixture :class :clean}})
          full-no-window (compose {:flight {:class :clean}})
          thin (fr/backfill-record frame-fixture)]
      (is (= {:mask :in :reason :clean-or-null-settled} (fr/validity-mask full-clean)))
      (is (= {:mask :out :reason :window-unsettled} (fr/validity-mask full-no-window)))
      (is (= {:mask :out :reason :derivation-thin} (fr/validity-mask thin)))
      (is (= {:mask :out :reason :fallback}
             (fr/validity-mask (assoc-in full-clean [:organs :measurement :judgment :class] :fallback)))
          "the censored fallback masks out by name")
      (is (= {:mask :out :reason :class-absent}
             (fr/validity-mask (assoc-in full-clean [:organs :measurement :judgment :class] nil)))))))

(deftest write-round-trips
  (let [dir (str (System/getProperty "java.io.tmpdir") "/flight-record-test-" (System/nanoTime))
        r (compose {:flight {:window window-fixture :class :clean}})
        path (fr/write-flight-record! r dir)
        back (edn/read-string (slurp path))]
    (is (= r back) "the persisted record reads back identical")
    (is (.endsWith ^String path "live-test-0001.flight.edn"))))
