(ns futon3c.diagramprover.wm-flight-replay-test
  "WM-MAP-REPLAY-I: the wiring map replayed at a defect's pre-fix sha must
  report the defect; at the map's pin it must not. Each variant declares what
  its sites read and write AT ITS PIN (the reader of a flow may sit at a
  different var before and after the fix), materialised with `git show` like
  the map test (no checkout, no worktree).

  G (the fourth flight, flight-e70b4baf): the joint decision throws its
  typed refusal under :kind; the runner's classifier, explicit-failure-kind,
  reads :failure-kind/:outcome, unchanged by the fix. Before 321d82c8 the two
  did not meet, so the refusal closed :untyped-failure. The fix put a
  translator between them: judge-refusal reads :kind, and the selection
  catch throws judge-refusal-abstention's {:outcome :abstained}, which the
  classifier reads. Pre-fix pin: 321d82c8^ = cdc492ec (c7367eaa^ is
  321d82c8, which already carries the fix). The classifier's :failure-kind
  is written by the runner's own typed throws, outside this variant, so it
  is read-never-written at both pins; G is the :kind/:outcome pair. A and D need `select-keys` read
  as a read (PROVER-READS-I); their shas are kept here :pending."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-flight-wiring-test :as map-test]))

(def judge-box
  {:box/id :g-judge :box/kind :component
   :site {:file "futon2/scripts/futon2/report/war_machine.clj" :var "cascade-decision-admitted"}
   :writes [:kind]})

(def classifier-box
  {:box/id :g-classifier :box/kind :component
   :site {:file "futon2/src/futon2/aif/full_loop_runner.clj" :var "explicit-failure-kind"}
   :reads [:failure-kind :outcome]})

(def replays
  {:G {:pre-fix {:sha "cdc492ec"
                 :boxes [judge-box classifier-box]}
       :pinned {:boxes [judge-box
                        {:box/id :g-translator-read :box/kind :component
                         :site {:file "futon2/src/futon2/aif/full_loop_runner.clj" :var "judge-refusal"}
                         :reads [:kind]}
                        {:box/id :g-translator-write :box/kind :component
                         :site {:file "futon2/src/futon2/aif/full_loop_runner.clj" :var "judge-refusal-abstention"}
                         :writes [:outcome]}
                        classifier-box]}}
   ;; waiting on PROVER-READS-I: at their fixes the read sits in a
   ;; select-keys vector, which the prover reports as a read not found
   :A {:pending :prover-reads-i :pre-fix {:sha "8588dba0^"}
       :boxes-sketch {:writer "futon2/src/futon2/aif/flight_runner.clj http-click-fn writes [:detail :status]"
                      :reader "futon2/src/futon2/aif/flight.clj record-click reads [:detail :status]"}}
   :D {:pending :prover-reads-i :pre-fix {:sha "367be490^"}
       :boxes-sketch {:writer+reader "futon2/src/futon2/aif/observation_checks.clj the C8 check, [:message :class :timeout-ms]"}}})

(defn- run [futon2-sha boxes]
  (let [s {:repos {"futon2" futon2-sha "futon3c" (get map-test/repos "futon3c")} :boxes boxes}
        root (#'map-test/materialise s)]
    (map-test/report root s)))

(defn- pair [findings]
  (set (for [f findings :when (#{:read-never-written :written-never-read} (:finding f))]
         [(:finding f) (:field f)])))

(defn- conformance-misses
  "Findings saying a declaration does not match the pinned source: the pair
  above is computed from the declarations, so the source must bear them out."
  [findings]
  (filterv #(#{:declaration-without-occurrence :declared-read-not-found
               :declared-write-not-found :var-not-found :site-unreadable} (:finding %))
           findings))

(def g #{[:written-never-read :kind] [:read-never-written :outcome]})

(deftest g-is-a-finding-before-its-fix
  (let [{:keys [sha boxes]} (get-in replays [:G :pre-fix])]
    (is (= (conj g [:read-never-written :failure-kind]) (pair (run sha boxes)))
        "the judge writes :kind, the classifier reads :failure-kind/:outcome: they do not meet")
    (is (empty? (conformance-misses (run sha boxes))) "each declaration is borne out by cdc492ec's source")))

(deftest g-is-not-a-finding-at-the-pin
  (let [findings (run (get map-test/repos "futon2") (get-in replays [:G :pinned :boxes]))]
    (is (= #{[:read-never-written :failure-kind]} (pair findings))
        "the judge's :kind is read by judge-refusal and the classifier's :outcome written by judge-refusal-abstention: G is gone; :failure-kind's writers are outside the variant")
    (is (empty? (conformance-misses findings)) "each declaration is borne out by the pinned source")))

(deftest the-pinned-reader-did-not-exist-before-the-fix
  ;; why the declaration is per pin: at cdc492ec there is no judge-refusal
  (is (some #(= :var-not-found (:finding %))
            (run (get-in replays [:G :pre-fix :sha]) (get-in replays [:G :pinned :boxes])))))

(deftest a-and-d-wait-on-prover-reads-i
  (is (= #{:A :D} (set (keep (fn [[k v]] (when (= :prover-reads-i (:pending v)) k)) replays)))))
