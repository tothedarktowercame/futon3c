(ns futon3c.arxana.workplan-demo-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.workplan-backlinks :as backlinks])
  (:import [java.io File]
           [java.net ServerSocket]))

(defn- free-port [] (with-open [s (ServerSocket. 0)] (.getLocalPort s)))
(defn- call [cfg id op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id
                         :principal/id "workplan-durable-test" :operation op :body body}))
(defn- delete-tree! [^File root]
  (when (.exists root) (doseq [f (reverse (file-seq root))] (.delete ^File f))))

(deftest durable-store-survives-restart-and-rebuilds-a-missing-index
  (let [root (File/createTempFile "workplan-3-" ".store"
                                  (File. "/home/joe/worktrees/futon3c"))
        _ (.delete root) _ (.mkdirs root)
        raw (json/read-value (slurp "test/resources/arxana/sessionista/joe-introduces.json")
                             json/keyword-keys-object-mapper)
        entry (:entry raw) text (get-in entry [:evidence/body :text]) eid (:evidence/id entry)
        intro! (fn [cfg]
                 (backlinks/materialise-rules! #(service/dispatch cfg %) (backlinks/read-rules))
                 (call cfg "seed-inbox" :clusion/assert
                       {:record (records/make-clusion {:type-revision [:lexicon/entity 1]
                                                       :endpoints []
                                                       :asserted-content {:entity/slug "inbox-zero"}})})
                 (:ok (call cfg "durable-introduction" :workplan/introduce
                            {:evidence/id eid :text text :actor "joe" :time (:evidence/at entry)
                             :candidate/slugs ["inbox-zero"]})))]
    (try
      (let [h (base/start! (free-port) {:data-dir (.getPath root) :prepare? false})
            introduced (intro! (:cfg h))
            candidate (get-in (:ok (call (:cfg h) "first-view" :workplan/entity-backlinks
                                          {:entity/slug "sessionista-mode"}))
                              [:referents 0 :record :id])]
        (is (some #{"sessionista-mode"} (map :entity/slug (:introductions introduced))))
        (is (:ok (call (:cfg h) "durable-assessment" :workplan/assess-referent
                       {:subject/id candidate :verdict :accepted :actor "joe"
                        :reason "durability fixture" :time "2026-11-09T20:00:00.000Z"})))
        (base/stop! h))
      (let [h (base/start! (free-port) {:data-dir (.getPath root) :prepare? false})
            view (:ok (call (:cfg h) "after-restart" :workplan/entity-backlinks
                            {:entity/slug "sessionista-mode"}))]
        (is (= eid (get-in view [:introductions 0 :attestations 0 :content :evidence/id])))
        (is (= :accepted (get-in view [:referents 0 :assessments 0 :verdict])))
        (is (true? (get-in view [:forms/effective 0 :enabled?])))
        (base/stop! h))
      ;; The sidecar is derived. Removing only it must trigger the server's
      ;; deterministic startup rebuild from durable XTDB records.
      (let [idx (File. root "arxana-index.sqlite")]
        (is (.delete idx))
        (let [h (base/start! (free-port) {:data-dir (.getPath root) :prepare? false})
              view (:ok (call (:cfg h) "after-index-rebuild" :workplan/entity-backlinks
                              {:entity/slug "sessionista-mode"}))]
          (is (= eid (get-in view [:introductions 0 :attestations 0 :content :evidence/id])))
          (is (= :accepted (get-in view [:referents 0 :assessments 0 :verdict])))
          (base/stop! h)))
      (finally (delete-tree! root)))))
