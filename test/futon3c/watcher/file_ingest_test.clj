(ns futon3c.watcher.file-ingest-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.file-ingest :as sut]
            [futon3c.watcher.multi :as multi]))

(deftest mission-doc-path-stays-inside-stack-roots
  (testing "mission-doc ingest is scoped to holes/missions under /home/joe/code"
    (is (true? (sut/mission-doc-path? "/home/joe/code/futon3/holes/missions/M-live-geometric-stack.md")))
    (is (false? (sut/mission-doc-path? "/home/joe/npt/missions/M-ukrns-wp.md")))
    (is (false? (sut/mission-doc-path? "/home/joe/code/futon3/holes/missions/notes.md")))))

(deftest excursion-doc-path-stays-inside-stack-roots
  (testing "excursion ingest is scoped to E-prefix docs under holes/missions"
    (is (true? (sut/excursion-doc-path? "/home/joe/code/futon3c/holes/missions/E-night-shift.md")))
    (is (false? (sut/excursion-doc-path? "/home/joe/code/futon3c/holes/missions/M-night-shift.md")))
    (is (false? (multi/watched? "/home/joe/code/futon3c/.state/frame/holes/missions/E-night-shift.md")))))

(deftest sorry-registry-path-detection-stays-narrow
  (testing "only the canonical futon2 sorry registry gets the special watcher path"
    (is (true? (sut/sorry-registry-path? "/home/joe/code/futon2/resources/sorrys.edn"))) ; R-A.1 canonical
    (is (true? (sut/sorry-registry-path? "/home/joe/code/futon2/data/sorrys.edn")))      ; transition-safe
    (is (false? (sut/sorry-registry-path? "/home/joe/code/futon2/data/other.edn")))
    (is (false? (sut/sorry-registry-path? "/home/joe/code/futon2/resources/other.edn")))
    (is (false? (sut/sorry-registry-path? "/home/joe/code/futon3c/data/sorrys.edn")))))

(deftest sorry-endpoint-normalization-keeps-local-name-and-label
  (is (= "futon2/sorry/r3a-likelihood-coupling-density"
         (sut/normalize-sorry-endpoint "futon2"
                                       :sorry/r3a-likelihood-coupling-density))))

(deftest file-endpoint-normalization-keeps-repo-label-and-relative-path
  (is (= "futon3c-d/file/src/futon3c/aif/stack_generator.clj"
         (sut/normalize-file-endpoint
          "/home/joe/code/futon3c/src/futon3c/aif/stack_generator.clj")))
  (is (= "vsat-d/file/docs/spec.md"
         (sut/normalize-file-endpoint
          "/home/joe/vsat/docs/spec.md"))))

(deftest mission-code-paths-project-to-file-to-mission-edges
  (let [plan (sut/build-file-to-mission-edge-docs
              {:path "/home/joe/code/futon3c/holes/missions/M-mission-wiring.md"
               :label "futon3c-d"
               :mission-id "mission-wiring"
               :mission-endpoint "futon3c-d/mission/mission-wiring"
               :mission-code-paths ["/home/joe/code/futon3c/src/futon3c/aif/stack_generator.clj"
                                    "/home/joe/code/futon3/scripts/ingest_one_file.clj"]})]
    (is (= 2 (count (:edge-docs plan))))
    (is (empty? (:unresolved-code-paths plan)))
    (is (= ["futon3c-d/file/src/futon3c/aif/stack_generator.clj"
            "futon3c-d/mission/mission-wiring"]
           (:endpoints (first (:edge-docs plan)))))
    (is (= "mission/code-paths"
           (get-in (first (:edge-docs plan))
                   [:props "relation/source-field"])))
    (is (= "mission/mentions-file"
           (get-in (first (:edge-docs plan))
                   [:props "relation/semantics"])))
    (is (= "mentions/stated"
           (get-in (first (:edge-docs plan))
                   [:props "relation/subtype"])))
    (is (true? (get-in (first (:edge-docs plan))
                       [:props "relation/feeds-mu?"])))
    (is (false? (get-in (first (:edge-docs plan))
                        [:props "relation/feeds-A?"])))
    (is (= "futon3c-d/mission/mission-wiring"
           (get-in (first (:edge-docs plan))
                   [:props "relation/logical-source-endpoint"])))
    (is (= "futon3c-d/file/src/futon3c/aif/stack_generator.clj"
           (get-in (first (:edge-docs plan))
                   [:props "relation/logical-target-endpoint"])))))

(deftest ingest-mission-doc-emits-one-file-to-mission-edge-per-code-path
  (let [posted (atom [])]
    (with-redefs [sut/sync-mission!
                  (fn [_path _root]
                    {:ok? true
                     :status 200
                     :body {:mission {:mission/id "mission-wiring"
                                      :mission/title "Mission Wiring"
                                      :mission/status "in-progress"
                                      :mission/repo "futon3c"
                                      :mission/date "2026-05-27"
                                      :mission/owner "Joe"
                                      :mission/summary "fixture"
                                      :mission/cross-refs []
                                      :mission/code-paths ["/home/joe/code/futon3c/src/futon3c/aif/stack_generator.clj"
                                                           "/home/joe/code/futon3/scripts/ingest_one_file.clj"]
                                      :mission/phase "identify"
                                      :mission/mtime "2026-05-27"
                                      :mission/psrs []
                                      :mission/purs []}}})
                  sut/post-hyperedge! (fn [& _] {:ok? true})
                  sut/post-entity! (fn [& _] {:ok? true})
                  sut/emit-cross-ref-edges! (fn [& _] {:emitted 0 :unresolved 0 :failed 0})
                  sut/post-hyperedge-doc! (fn [doc]
                                            (swap! posted conj doc)
                                            {:ok? true})]
      (let [result (sut/ingest-mission-doc! {:path "/home/joe/code/futon3c/holes/missions/M-mission-wiring.md"
                                             :label "futon3c-d"
                                             :root "/home/joe/code/futon3c"})]
        (is (= 2 (:edges result)))
        (is (= 2 (count @posted)))
        (is (every? #(= "code/v05/file→mission" (:hx-type %)) @posted))
        (is (every? #(some #{"mission/mentions-file"} (:labels %)) @posted))
        (is (every? #(= "mentions/stated"
                        (get-in % [:props "relation/subtype"]))
                    @posted))
        (is (= ["futon3c-d/file/src/futon3c/aif/stack_generator.clj"
                "futon3c-d/mission/mission-wiring"]
               (:endpoints (first @posted))))))))

(deftest ingest-excursion-doc-emits-vertex-and-parent-edge
  (let [posted (atom [])]
    (with-redefs [sut/post-hyperedge! (fn [& _] {:ok? true})
                  sut/post-entity! (fn [& _] {:ok? true})
                  sut/post-hyperedge-doc! (fn [doc]
                                            (swap! posted conj doc)
                                            {:ok? true})]
      (let [result (sut/ingest-excursion-doc! {:path "/home/joe/code/futon3c/holes/missions/E-support-coverage.md"
                                               :label "futon3c-d"})]
        (is (= 1 (:vertices result)))
        (is (= 1 (:edges result)))
        (is (= "support-coverage" (get-in result [:excursion :excursion/id])))
        (is (= "futon3c-d/mission/war-machine-pilot"
               (get-in result [:excursion :excursion/parent-mission])))
        (is (= "code/v05/excursion→parent-mission"
               (:hx-type (first @posted))))
        (is (= ["futon3c-d/excursion/support-coverage"
                "futon3c-d/mission/war-machine-pilot"]
               (:endpoints (first @posted))))))))

(deftest fixture-roundtrip-stays-lossless-on-three-cases
  (let [report (sut/fixture-sorry-roundtrip {:path "/home/joe/code/futon2/resources/sorrys.edn"
                                             :label "futon2"})]
    (is (true? (:pass? report)))
    (is (= 3 (:case-count report)))
    (is (true? (:lossless-roundtrip? report)))
    (is (every? :passed? (:cases report)))))
