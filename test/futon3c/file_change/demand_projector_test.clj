(ns futon3c.file-change.demand-projector-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.file-change.demand-projector :as projector]))

(defn entry [path seq]
  {:repo "futon3c-d" :worktree "/code/futon3c" :path path
   :content-id (str "h" seq) :operation :write :seq seq})

(deftest warrant-currency-is-demand-bound-not-a-sweep
  (let [state (projector/empty-state 10 30)
        entries [(entry "src/a.clj" 1)]
        [_ absent] (projector/project state entries [] 0)
        [_ named] (projector/project
                   state entries
                   [{:demand/id :handoff-1 :entry-id "warrant-1"
                     :repo-root "/code/futon3c" :paths ["src/a.clj" "test/a_test.clj"]}
                    {:demand/id :unrelated :entry-id "warrant-2"
                     :repo-root "/code/futon3c" :paths ["src/b.clj"]}]
                   0)]
    (is (empty? absent))
    (is (= [{:demand/type :test-registry/currency
             :entry-id "warrant-1" :repo-root "/code/futon3c"
             :changed-paths ["src/a.clj"]
             :basis :named-path-intersection :demand/id :handoff-1}]
           named))))

(deftest task-changes-debounce-but-cannot-starve
  (let [s0 (projector/empty-state 10 25)
        [s1 _] (projector/project s0 [(entry "holes/missions/M-one.md" 1)] [] 0)
        [s2 early] (projector/due-demands s1 9)
        [s3 _] (projector/project s2 [(entry "holes/tickets/T-two.md" 2)] [] 9)
        [s4 still-early] (projector/due-demands s3 18)
        [s5 _] (projector/project s4 [(entry "holes/excursions/E-three.md" 3)] [] 18)
        [done due] (projector/due-demands s5 25)]
    (is (empty? early))
    (is (empty? still-early))
    (is (= 1 (count due)))
    (is (= :giraffe/full-input-recompute (:demand/type (first due))))
    (is (= 25 (:due-at-ms (first due))))
    (is (= ["holes/excursions/E-three.md" "holes/missions/M-one.md"
            "holes/tickets/T-two.md"]
           (:changed-paths (first due))))
    (is (nil? (:giraffe-pending done)))))

(deftest level-jump-rebuilds-authorities-without-enumerating-warrants
  (let [jump {:repo "futon3c-d" :worktree "/code/futon3c" :path "."
              :content-id "jump" :operation :level-jump :seq 1}
        demands [{:demand/id :active :entry-id "w1"
                  :repo-root "/code/futon3c" :paths ["src/a.clj"]}]
        [state currency] (projector/project (projector/empty-state 10 20)
                                            [jump] demands 5)
        [_ giraffe] (projector/due-demands state 15)]
    (is (= ["w1"] (mapv :entry-id currency)))
    (is (= [] (:changed-paths (first currency))))
    (is (= :walk-level-jump (:basis (first currency))))
    (is (true? (:level-jump? (first giraffe))))))

(deftest ordinary-code-change-does-not-demand-giraffe
  (let [[state _] (projector/project (projector/empty-state 10 20)
                                     [(entry "src/a.clj" 1)] [] 0)
        [_ due] (projector/due-demands state 100)]
    (is (empty? due))))
