(ns futon3c.giraffe.excursions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn] [clojure.set :as set]
            [futon3c.giraffe.excursions :as e]
            [futon3c.giraffe.run :as writer]
            [futon2.aif.outer-score :as score]
            [futon2.aif.outer-observation :as observation]
            [futon2.aif.mission-registry :as registry]))

(deftest frozen-census-retains-real-completion-classes-and-unknown-graph
  (let [adapter (e/missions-and-excursions->input
                 {:edges-file "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json"})
        result (e/combined-pass (:input adapter))
        rows (into {} (map (juxt :id identity)) (:order result))
        record (edn/read-string (slurp "/home/joe/code/futon2/data/wm-runs/tick-run-record-2026-10-29-8bb5e6dd-087a-46b1-beb9-bda595f8cc07.edn"))
        open (set (mapcat :ids (vals (get-in record [:world-at-selection :open-tasks]))))
        ids (set (map (comp name :id) (:order result)))
        missing (set/difference open ids)
        output (writer/write-run! {:root (str (java.nio.file.Files/createTempDirectory "q1-E-test-" (make-array java.nio.file.attribute.FileAttribute 0)))
                                   :adapter-result adapter :pass-output result})]
    (let [excursions (registry/load-excursions "/home/joe/code")
          observed (observation/giraffe-field
                    (:dir output) {:default-reach 1/2}
                    {:loaded-missions (registry/load-missions-from-files "/home/joe/code")
                     :loaded-excursions excursions :loaded-tickets {:tickets []}
                     :excursion-targets (mapv :id (:excursions excursions))})]
      (is (= 636 (+ (count (:field observed)) (count (:outer-field/excluded observed)))))
      (is (= :unknown (get-in observed [:facts "E-starmap-branch-merge" :graph-terms :status])))
      (is (= :selected-task-revision-baseline (get-in observed [:facts "E-starmap-branch-merge" :completion :producer]))))
    (is (= 636 (count ids)))
    (is (= 189 (count missing)))
    (is (every? #(.startsWith % "T-") missing))
    (is (empty? (set/difference ids open)))
    (doseq [[id class] [[:E-starmap-branch-merge :checkbox-locators-with-retained-prose]
                       [:E-kimi-task-124 :completion-prose-needs-locators]
                       [:E-2026-08-10-learnings :completion-definition-not-found]]]
      (is (= class (get-in rows [id :completion :completion-class]))))
    (is (= :task-completion-author (get-in rows [:E-2026-08-10-learnings :completion :producer])))
    (is (every? #(and (nil? (:cone %)) (nil? (:unblocks %))) (vals rows)))
    (is (= :undetermined (get-in rows [:E-starmap-branch-merge :status])))
    (is (:ok? (writer/verify-run (:dir output))))
    (doseq [step [:by-cone :by-unblocks]]
      (is (= :outer-score/graph-term-unavailable
             (try (score/apply-selection-step {"E-starmap-branch-merge" (rows :E-starmap-branch-merge)}
                                              [{:target "E-starmap-branch-merge"}] step)
                  nil (catch clojure.lang.ExceptionInfo ex (:kind (ex-data ex)))))))))
