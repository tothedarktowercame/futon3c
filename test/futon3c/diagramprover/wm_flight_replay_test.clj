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
  is read-never-written at both pins; G is the :kind/:outcome pair.

  A (the second flight): http-click-fn wrote the server's :status/:detail
  onto the abstention; record-click's select-keys kept only
  :kind/:missing/:declines, so the reason never reached the flight record.
  Per pin: at 8588dba0^ the writer's :status/:detail meet no reader; at the
  pin record-click's select-keys reads them (a read since PROVER-READS-I,
  futon3c 8f716c8e).

  D (the second flight's C8 refusals): at 367be490^ the registry read's catch
  wrote {:status :unreachable :body msg} and the same var destructured :body
  and left it out of the refusal: one var, no key crossing between two
  sites, so no per-pin pair can show it. It shows the other way: the pinned
  declarations (registry-get writes :message/:timeout-ms, the two C8 readers
  read them through select-keys) replayed at 367be490^ are not borne out by
  that source (registry-get absent, the readers without the keys)."
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
   :A (let [writer {:box/id :a-click :box/kind :component
                    :site {:file "futon2/src/futon2/aif/flight_runner.clj" :var "http-click-fn"}
                    :writes [:kind :missing :status :detail]}
            reader (fn [reads] {:box/id :a-record :box/kind :component
                                :site {:file "futon2/src/futon2/aif/flight.clj" :var "record-click"}
                                :reads reads})]
        {:pre-fix {:sha "8588dba0^" :boxes [writer (reader [:kind :missing :declines])]}
         :pinned {:boxes [writer (reader [:kind :missing :declines :status :detail])]}})
   :D {:pre-fix {:sha "367be490^"}
       :pinned {:boxes [{:box/id :d-get :box/kind :component
                         :site {:file "futon2/src/futon2/aif/observation_checks.clj" :var "registry-get"}
                         :writes [:message :timeout-ms]}
                        {:box/id :d-entry :box/kind :component
                         :site {:file "futon2/src/futon2/aif/observation_checks.clj" :var "fetch-registry-entry"}
                         :reads [:message :timeout-ms]}
                        {:box/id :d-latest :box/kind :component
                         :site {:file "futon2/src/futon2/aif/observation_checks.clj" :var "fetch-latest-for"}
                         :reads [:message :timeout-ms]}]}}})

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

(def a #{[:written-never-read :status] [:written-never-read :detail]})

(deftest a-is-a-finding-before-its-fix
  (let [{:keys [sha boxes]} (get-in replays [:A :pre-fix])
        findings (run sha boxes)]
    (is (= (conj a [:read-never-written :declines]) (pair findings))
        "the click writes :status/:detail, record-click reads :kind/:missing/:declines: the reason meets no reader")
    (is (empty? (conformance-misses findings)) "each declaration is borne out by 8588dba0^'s source")))

(deftest a-is-not-a-finding-at-the-pin
  (let [findings (run (get map-test/repos "futon2") (get-in replays [:A :pinned :boxes]))]
    (is (= #{[:read-never-written :declines]} (pair findings))
        "record-click's select-keys reads :status/:detail; :declines's writer (the abstention carrier) is outside the variant")
    (is (empty? (conformance-misses findings)) "no false declared-read-not-found: select-keys is a read")))

(deftest d-the-pinned-declarations-are-not-borne-out-before-the-fix
  (let [findings (run (get-in replays [:D :pre-fix :sha]) (get-in replays [:D :pinned :boxes]))]
    (is (= #{[:var-not-found :d-get]
             [:declaration-without-occurrence :d-entry :message] [:declaration-without-occurrence :d-entry :timeout-ms]
             [:declaration-without-occurrence :d-latest :message] [:declaration-without-occurrence :d-latest :timeout-ms]}
           (set (for [f (conformance-misses findings)]
                  (if (= :var-not-found (:finding f))
                    [:var-not-found (some #(when (= (:site %) (:site f)) (:box/id %)) (get-in replays [:D :pinned :boxes]))]
                    [(:finding f) (:box/id f) (:field f)])))))))

(deftest d-is-not-a-finding-at-the-pin
  (let [findings (run (get map-test/repos "futon2") (get-in replays [:D :pinned :boxes]))]
    (is (empty? (pair findings)))
    (is (empty? (conformance-misses findings)) "the readers read :message/:timeout-ms through select-keys")))
