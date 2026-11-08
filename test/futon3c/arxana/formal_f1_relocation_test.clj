(ns futon3c.arxana.formal-f1-relocation-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.formal-f1-relocation :as relocation]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f1b2-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})
              provider (artifact/in-memory-provider)
              cfg {:graph-store (store-http/http-store
                                 (str "http://127.0.0.1:"
                                      (.getPort (.getAddress server)))
                                 f1/service-principal)
                   :artifact-provider provider}]
          (try (f node cfg provider)
               (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(deftest edited-export-relocates-without-rewriting-golden-interpretations
  (with-stack
    (fn [node cfg provider]
      (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
            _ (f1a/materialize! cfg plans)
            exported (export/export plans)
            first-ingest (export/ingest! cfg exported)
            entries (vec (mapcat :interpretations (get-in exported [:manifest :turns])))
            [unique-entry ambiguous-entry missing-entry] entries
            unique-id (:clusion/id unique-entry)
            ambiguous-id (:clusion/id ambiguous-entry)
            missing-id (:clusion/id missing-entry)
            originals (into {} (for [id [unique-id ambiguous-id missing-id]]
                                 [id (f1/clusion-by-id node id)]))
            registry (relocation/register-strategies exported)
            edited (relocation/edit-successor exported unique-id ambiguous-id missing-id)
            successor (relocation/ingest-successor! cfg exported edited)
            successor-rid (get-in successor [:ok :artifact/revision-id])
            run (relocation/relocate provider registry
                                     (:markdown/revision-id exported) successor-rid
                                     successor-rid successor-rid)
            by-id (into {} (map (juxt :subject-clusion-id identity) (:results run)))
            responses (relocation/append-relocations! cfg run)]
        (is (:ok (:markdown first-ingest)))
        (is (:ok successor) "successor ingest uses the first revision as expected head")
        (is (= successor-rid (f1/current-head node "formal-arxana:f1-conversation")))
        (is (= (count entries) (count registry))
            "every interpretation range has a content-addressed F0 strategy")
        (testing "offset shift remains unique and records the moved range"
          (let [result (get-in by-id [unique-id :resolution])
                old-start (first (:range unique-entry))
                new-start (get-in result [:result/candidates 0 :location :range :start])]
            (is (= :resolved (:result/status result)))
            (is (> new-start old-start))))
        (testing "duplicate is an ambiguity, never first-candidate selection"
          (let [result (get-in by-id [ambiguous-id :resolution])]
            (is (= :ambiguous (:result/status result)))
            (is (= 2 (count (:result/candidates result))))))
        (testing "deleted text remains missing without fuzzy nearby invention"
          (let [result (get-in by-id [missing-id :resolution])]
            (is (= :missing (:result/status result)))
            (is (empty? (:result/candidates result)))))
        (testing "all outcomes are appended as evidence and originals stay byte-equal"
          (is (every? :ok responses))
          (doseq [[id original] originals]
            (is (= original (f1/clusion-by-id node id))))
          (let [standings (set (mapcat (fn [id]
                                         (map :standing (f1/evidence-for node id)))
                                       [unique-id ambiguous-id missing-id]))]
            (is (every? standings #{:verified :disputed :contradicted})
                "new relocation standings coexist with original model attestations")))
        (testing "a read whose expected head is historical is stale before resolution"
          (let [stale (relocation/relocate provider registry
                                           (:markdown/revision-id exported) successor-rid
                                           (:markdown/revision-id exported) successor-rid)]
            (is (= :stale (:result/status stale)))
            (is (empty? (:results stale)))))
        (testing "reusing the historical expected head for another write is refused"
          (let [again (relocation/ingest-successor! cfg exported
                                                    (assoc edited :markdown
                                                           (str (:markdown edited) "x")))]
            (is (= :stale (get-in again [:error :error/category])))))))))
