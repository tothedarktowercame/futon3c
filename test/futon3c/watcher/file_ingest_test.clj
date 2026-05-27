(ns futon3c.watcher.file-ingest-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.file-ingest :as sut]))

(deftest mission-doc-path-stays-inside-stack-roots
  (testing "mission-doc ingest is scoped to holes/missions under /home/joe/code"
    (is (true? (sut/mission-doc-path? "/home/joe/code/futon3/holes/missions/M-live-geometric-stack.md")))
    (is (false? (sut/mission-doc-path? "/home/joe/npt/missions/M-ukrns-wp.md")))
    (is (false? (sut/mission-doc-path? "/home/joe/code/futon3/holes/missions/notes.md")))))

(deftest sorry-registry-path-detection-stays-narrow
  (testing "only the canonical futon2 sorry registry gets the special watcher path"
    (is (true? (sut/sorry-registry-path? "/home/joe/code/futon2/data/sorrys.edn")))
    (is (false? (sut/sorry-registry-path? "/home/joe/code/futon2/data/other.edn")))
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
                   [:props "relation/source-field"])))))

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
        (is (= ["futon3c-d/file/src/futon3c/aif/stack_generator.clj"
                "futon3c-d/mission/mission-wiring"]
               (:endpoints (first @posted))))))))

(deftest fixture-roundtrip-stays-lossless-on-three-cases
  (let [report (sut/fixture-sorry-roundtrip {:path "/home/joe/code/futon2/data/sorrys.edn"
                                             :label "futon2"})]
    (is (true? (:pass? report)))
    (is (= 3 (:case-count report)))
    (is (true? (:lossless-roundtrip? report)))
    (is (every? :passed? (:cases report)))))
