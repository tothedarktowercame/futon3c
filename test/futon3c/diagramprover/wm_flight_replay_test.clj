(ns futon3c.diagramprover.wm-flight-replay-test
  "WM-MAP-REPLAY-I: the wiring map replayed at a defect's pre-fix sha must
  report the defect; at the map's pin it must not. Each variant declares what
  its sites read and write AT ITS PIN (the reader of a flow may sit at a
  different var before and after the fix), materialised with `git show` like
  the map test (no checkout, no worktree).

  G (the fourth flight, flight-e70b4baf): the joint decision threw its typed
  refusal under :kind; before 321d82c8 the runner's reader of that throw was
  explicit-failure-kind, reading :failure-kind/:outcome, so the refusal
  closed :untyped-failure. Pre-fix pin: 321d82c8^ = cdc492ec (c7367eaa^ is
  321d82c8, which already carries the fix). A and D need `select-keys` read
  as a read (PROVER-READS-I); their shas are kept here :pending."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-flight-wiring-test :as map-test]))

(def judge-box
  {:box/id :g-judge :box/kind :component
   :site {:file "futon2/scripts/futon2/report/war_machine.clj" :var "cascade-decision-admitted"}
   :writes [:kind]})

(def replays
  {:G {:pre-fix {:sha "cdc492ec"
                 :boxes [judge-box
                         {:box/id :g-reader :box/kind :component
                          :site {:file "futon2/src/futon2/aif/full_loop_runner.clj" :var "explicit-failure-kind"}
                          :reads [:failure-kind :outcome]}]}
       :pinned {:boxes [judge-box
                        {:box/id :g-reader :box/kind :component
                         :site {:file "futon2/src/futon2/aif/full_loop_runner.clj" :var "judge-refusal"}
                         :reads [:kind]}]}}
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

(deftest g-is-a-finding-before-its-fix
  (let [{:keys [sha boxes]} (get-in replays [:G :pre-fix])]
    (is (= #{[:read-never-written :failure-kind] [:read-never-written :outcome] [:written-never-read :kind]}
           (pair (run sha boxes)))
        "the judge writes :kind, its reader reads :failure-kind/:outcome: they do not meet")
    (is (empty? (conformance-misses (run sha boxes))) "each declaration is borne out by cdc492ec's source")))

(deftest g-is-not-a-finding-at-the-pin
  (let [findings (run (get map-test/repos "futon2") (get-in replays [:G :pinned :boxes]))]
    (is (empty? (pair findings)) "the judge writes :kind and judge-refusal reads it")
    (is (empty? (conformance-misses findings)) "each declaration is borne out by the pinned source")))

(deftest the-pinned-reader-did-not-exist-before-the-fix
  ;; why the declaration is per pin: at cdc492ec there is no judge-refusal
  (is (some #(= :var-not-found (:finding %))
            (run (get-in replays [:G :pre-fix :sha]) (get-in replays [:G :pinned :boxes])))))

(deftest a-and-d-wait-on-prover-reads-i
  (is (= #{:A :D} (set (keep (fn [[k v]] (when (= :prover-reads-i (:pending v)) k)) replays)))))
