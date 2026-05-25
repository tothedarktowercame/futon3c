(ns futon3c.watcher.file-ingest-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.file-ingest :as sut]))

(deftest mission-doc-path-stays-inside-stack-roots
  (testing "mission-doc ingest is scoped to holes/missions under /home/joe/code"
    (is (true? (sut/mission-doc-path? "/home/joe/code/futon3/holes/missions/M-live-geometric-stack.md")))
    (is (false? (sut/mission-doc-path? "/home/joe/npt/missions/M-ukrns-wp.md")))
    (is (false? (sut/mission-doc-path? "/home/joe/code/futon3/holes/missions/notes.md")))))
