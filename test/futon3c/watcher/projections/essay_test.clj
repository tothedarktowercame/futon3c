(ns futon3c.watcher.projections.essay-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.file-ingest :as file-ingest]
            [futon3c.watcher.projections.essay :as sut]))

(def operator-foreword-path
  "/home/joe/code/futon7a/essays/operator-foreword/operator-foreword.md")

(def mission-doc-path
  "/home/joe/code/futon4/holes/missions/M-essay-corpus-substrate.md")

(def synthetic-essay-path
  (.getAbsolutePath
   (io/file "test/fixtures/futon9a/essays/synthetic-slate/synthetic-slate.md")))

(deftest essay-home-path-predicate-is-strict
  (is (true? (sut/essay-home-md? operator-foreword-path)))
  (is (false? (sut/essay-home-md? mission-doc-path)))
  (is (false? (sut/essay-home-md?
               "/home/joe/code/futon7a/essays/some-readme.md"))))

(deftest collect-file-projects-operator-foreword
  (testing "the real first essay fixture keeps its full section/annotation shape"
    (let [{:keys [essay sections annotations]} (sut/collect-file operator-foreword-path)
          scope-mush (some #(when (= "hx:of:v1:scope-mush-naming" (:id %)) %) annotations)]
      (is (= "arxana/essay/operator-foreword-v1" (:id essay)))
      (is (= "arxana/essay" (:type essay)))
      (is (= 11 (count sections)))
      (is (= 16 (count annotations)))
      (is (= 3 (count (filter #(= :annotated (:role %)) (:endpoints scope-mush)))))
      (is (= "writing-coherence/scope-mush"
             (:pattern-name
              (some #(when (= :source (:role %)) %) (:endpoints scope-mush))))))))

(deftest collect-file-projects-a-second-essay-fixture
  (testing "the projector generalizes beyond the foreword"
    (let [{:keys [essay sections annotations]} (sut/collect-file synthetic-essay-path)]
      (is (= "arxana/essay/synthetic-slate-v1" (:id essay)))
      (is (= "Synthetic Slate (2026-05-21)" (:name essay)))
      (is (= 2 (count sections)))
      (is (= 2 (count annotations)))
      (is (= "§1. First Move" (:name (first sections))))
      (is (= #{:annotation/comment :aif/invariant-witness}
             (set (map :hx-type annotations)))))))

(deftest dispatch-ingests-essays-with-stable-ids
  (testing "essay dispatch bypasses collect-repo and posts stable entity/hyperedge ids"
    (let [entity-calls (atom [])
          hyperedge-calls (atom [])
          run! (fn []
                 (with-redefs [file-ingest/collect-repo (fn [_]
                                                          (throw (ex-info "essay dispatch should not rebuild root-ctx" {})))
                               file-ingest/post-entity! (fn [payload]
                                                         (swap! entity-calls conj payload)
                                                         {:ok? true})
                               file-ingest/post-hyperedge-doc! (fn [payload]
                                                                (swap! hyperedge-calls conj payload)
                                                                {:ok? true})]
                   (file-ingest/dispatch! {:path synthetic-essay-path
                                           :root "/home/joe/code/futon3c"
                                           :label "futon9a"})))]
      (let [first-result (run!)
            first-entities @entity-calls
            first-hyperedges @hyperedge-calls
            first-entity-ids (map :id first-entities)
            first-hx-ids (map :id first-hyperedges)]
        (is (= :essay (:status first-result)))
        (is (= 3 (:vertices first-result)))
        (is (= 2 (:edges first-result)))
        (is (= 0 (:failed first-result)))
        (is (= (count first-entity-ids) (count (distinct first-entity-ids))))
        (is (= (count first-hx-ids) (count (distinct first-hx-ids))))
        (reset! entity-calls [])
        (reset! hyperedge-calls [])
        (let [second-result (run!)]
          (is (= :essay (:status second-result)))
          (is (= first-entity-ids (map :id @entity-calls)))
          (is (= first-hx-ids (map :id @hyperedge-calls))))))))
