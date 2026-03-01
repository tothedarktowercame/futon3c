(ns futon3c.peripheral.mission-backend-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.mission-backend :as mb]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.test-fixtures :as fix]))

(defn- make-test-backend
  []
  (let [dir (fix/temp-dir!)]
    {:backend (mb/make-mission-backend {:cwd dir} (tools/make-mock-backend))
     :cwd dir}))

(defn- write-file!
  [cwd rel-path content]
  (let [f (io/file cwd rel-path)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (.getPath f)))

(defn- init-loaded-mission!
  [{:keys [backend cwd]}]
  (mb/init-mission! cwd "M-test" "Mission test" ["All checks pass"] ["src/"] ["vendor/"])
  (tools/execute-tool backend :mission-load ["M-test"]))

(defn- build-complete-cycle!
  ([backend]
   (build-complete-cycle! backend {:doc-artifacts ["docs/M-test.md"]
                                   :hypergraph-plan {:status :wired
                                                     :refs ["hx:mission/M-test#2.11"]}}))
  ([backend integrate-extra]
  (let [_ (tools/execute-tool backend :obligation-upsert
                              ["M-test" "O-main" {:item/label "Main obligation"
                                                  :item/status :open
                                                  :item/depends-on #{}
                                                  :item/unlocks #{}
                                                  :item/artifact-paths []}])
        begin (tools/execute-tool backend :cycle-begin ["M-test" "O-main"])
        cycle-id (get-in begin [:result :cycle/id])]
    (doseq [phase-data [{:blocker-id "O-main"}
                        {:approach "Implement the missing pieces"}
                        {:artifacts ["src/futon3c/peripheral/mission_backend.clj"]}
                        {:validation-artifacts ["test/futon3c/peripheral/mission_backend_test.clj"]}
                        {:classification :partial}
                        (merge {:rationale "Integrate docs/code checks"
                                :obligation-changes {}}
                               integrate-extra)
                        {:saved? true}
                        {:gates-passed true}]]
      (tools/execute-tool backend :cycle-advance ["M-test" cycle-id phase-data]))
    cycle-id)))

(def ^:private guide-with-gf
  "G5  Task Specification
G4  Agent Authorization
GF  Fidelity Contract
G3  Pattern Reference")

(def ^:private mission-doc-with-open-section
  "# Mission: M-test

## Fidelity Contract (GF)
### Baseline Capability Inventory
### Capability Preservation Matrix (CPM)
### Tripwire Matrix
### Latent Dependency/Omission Probe
### Drop/Defer Decision Records (DR)

### 2.11 Model Descriptor System
This section exists but remains open.

## Success Criteria
- [ ] Model descriptor system rebuilt (Section 2.11)
")

(def ^:private mission-doc-with-closed-section
  "# Mission: M-test

## Fidelity Contract (GF)
### Baseline Capability Inventory
### Capability Preservation Matrix (CPM)
### Tripwire Matrix
### Latent Dependency/Omission Probe
### Drop/Defer Decision Records (DR)

### 2.11 Model Descriptor System
Closed and shipped.

## Success Criteria
- [x] Model descriptor system rebuilt (Section 2.11)
")

(deftest mission-doc-audit-reports-open-honest-interval
  (let [{:keys [backend cwd] :as ctx} (make-test-backend)
        _ (init-loaded-mission! ctx)
        _ (write-file! cwd "holes/missions/M-test.md" mission-doc-with-open-section)
        _ (write-file! cwd "docs/futonic-missions.md" guide-with-gf)
        result (tools/execute-tool backend :mission-doc-audit ["M-test"])]
    (is (:ok result))
    (is (= :drift (get-in result [:result :status])))
    (is (= 1 (get-in result [:result :open-section-count])))
    (is (= "2.11" (get-in result [:result :honest-intervals 0 :section])))))

(deftest gate-check-includes-gf-fidelity-gate
  (let [{:keys [backend cwd] :as ctx} (make-test-backend)
        _ (init-loaded-mission! ctx)
        _ (write-file! cwd "holes/missions/M-test.md" mission-doc-with-open-section)
        _ (write-file! cwd "docs/futonic-missions.md" guide-with-gf)
        cycle-id (build-complete-cycle! backend)
        result (tools/execute-tool backend :gate-check ["M-test" cycle-id])
        gf (first (filter #(= :GF-fidelity (:gate %))
                          (get-in result [:result :gates])))]
    (is (:ok result))
    (is (some? gf))
    (is (false? (:passed? gf)))
    (is (str/includes? (:detail gf) "Open honest intervals"))
    (is (false? (get-in result [:result :all-passed?])))))

(deftest gate-check-gf-passes-when-no-open-intervals
  (let [{:keys [backend cwd] :as ctx} (make-test-backend)
        _ (init-loaded-mission! ctx)
        _ (write-file! cwd "holes/missions/M-test.md" mission-doc-with-closed-section)
        _ (write-file! cwd "docs/futonic-missions.md" guide-with-gf)
        cycle-id (build-complete-cycle! backend)
        result (tools/execute-tool backend :gate-check ["M-test" cycle-id])
        gf (first (filter #(= :GF-fidelity (:gate %))
                          (get-in result [:result :gates])))]
    (is (:ok result))
    (is (true? (:passed? gf)))
    (is (true? (get-in result [:result :all-passed?])))))

(deftest gate-check-gd-document-fails-when-hypergraph-plan-empty
  (let [{:keys [backend cwd] :as ctx} (make-test-backend)
        _ (init-loaded-mission! ctx)
        _ (write-file! cwd "holes/missions/M-test.md" mission-doc-with-closed-section)
        _ (write-file! cwd "docs/futonic-missions.md" guide-with-gf)
        cycle-id (build-complete-cycle! backend {:doc-artifacts ["docs/M-test.md"]
                                                 :hypergraph-plan {:status :wired
                                                                   :refs []}})
        result (tools/execute-tool backend :gate-check ["M-test" cycle-id])
        gd (first (filter #(= :GD-document (:gate %))
                          (get-in result [:result :gates])))]
    (is (:ok result))
    (is (some? gd))
    (is (false? (:passed? gd)))
    (is (str/includes? (:detail gd) "Hypergraph plan missing refs"))
    (is (false? (get-in result [:result :all-passed?])))))
