(ns futon3c.arxana.formal-f1-projection-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.formal-f1-projection :as projection]
            [futon3c.arxana.formal-f1-relocation :as relocation]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f1c1-" ".db") _ (.delete db)]
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

(defn request [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "formal-f1-projection-test" :body body})

(defn interp-ids [manifest]
  (set (for [turn (:turns manifest) i (:interpretations turn)] (:clusion/id i))))

(defn projected-ids [projection]
  (set (map :interpretation/id (:projection/fragments projection))))

(deftest registered-f1-projections-use-public-read-ports
  (with-stack
    (fn [_node cfg provider]
      (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
            materialized (f1a/materialize! cfg plans)
            exported (export/export plans)
            ingested (export/ingest! cfg exported)
            entries (vec (mapcat :interpretations (get-in exported [:manifest :turns])))
            [unique ambiguous missing] (map :clusion/id (take 3 entries))
            registry (relocation/register-strategies exported)
            edited (relocation/edit-successor exported unique ambiguous missing)
            successor (relocation/ingest-successor! cfg exported edited)
            rid (get-in successor [:ok :artifact/revision-id])
            relocated (relocation/relocate provider registry
                                            (:markdown/revision-id exported) rid rid rid)
            relocation-writes (relocation/append-relocations! cfg relocated)
            manifest-rid (:manifest/revision-id exported)
            project! (fn [op body]
                       (let [response (service/dispatch cfg (request (str "project:" (name op))
                                                                    op body))]
                         (or (:ok response)
                             (throw (ex-info "projection failed" {:response response})))))
            linear (project! :formal-f1/linear {:manifest/revision-id manifest-rid})
            outline (project! :formal-f1/outline {:manifest/revision-id manifest-rid})
            gcc (project! :formal-f1/gcc {:manifest/revision-id manifest-rid
                                          :query/limit 200})
            expected (interp-ids (:manifest exported))]
        (is (every? #(every? :ok (:writes %)) materialized))
        (is (:ok (:manifest ingested)))
        (is (every? :ok relocation-writes))
        (is (= expected (set (mapcat :interpretation/ids
                                     (:projection/fragments linear)))))
        (is (every? #(seq (:text %)) (:projection/fragments linear))
            "linear fragments carry the exported prose with marks inline")
        (is (= expected (projected-ids outline)))
        (is (= expected (projected-ids gcc))
            "the same interpretation identities cross all three projections")
        (testing "every emitted record id resolves through public entity/get"
          (doseq [view [linear outline gcc]
                  fragment (:projection/fragments view)
                  id (:record/ids fragment)]
            (is (= id (get-in (service/dispatch cfg (request (str "get:" id)
                                                              :entity/get {:entity/id id}))
                              [:ok :record :id])))))
        (testing "the outline preserves competing readings and all standings"
          (let [ask-id (get-in (last plans) [:claims 0 :id])
                successor-id (get-in (last plans) [:claims 3 :id])
                by-id (into {} (map (juxt :interpretation/id identity)
                                    (:projection/fragments outline)))
                ask (by-id ask-id)]
            (is (contains? by-id successor-id))
            (is (= [:rejected] (mapv :verdict (get-in ask [:standing :assessment]))))
            (is (seq (get-in ask [:standing :resolution])))
            (is (seq (get-in ask [:standing :evidence-qualification])))))
        (testing "model evidence never launders a reading into acceptance"
          (let [model-only (some #(when (and (some (comp #{:model} :basis)
                                                   (get-in % [:standing :evidence-qualification]))
                                              (empty? (get-in % [:standing :assessment]))) %)
                                 (:projection/fragments outline))]
            (is model-only)
            (is (not-any? #(= :accepted (:verdict %))
                          (get-in model-only [:standing :assessment])))))
        (testing "assessment removal changes standing, not semantic ids"
          (let [base-read (fn [op body]
                            (service/dispatch cfg (request (str "read:" (name op) ":" (hash body))
                                                           op body)))
                without-assessment
                (fn [op body]
                  (let [r (base-read op body)]
                    (if (and (= op :graph/query) (= :assessments (:query/type body)))
                      (assoc-in r [:ok :result/assessments] []) r)))
                changed (projection/outline without-assessment (:manifest exported))]
            (is (= (projected-ids outline) (projected-ids changed)))
            (is (not= (mapv #(get-in % [:standing :assessment])
                            (:projection/fragments outline))
                      (mapv #(get-in % [:standing :assessment])
                            (:projection/fragments changed))))))
        (testing "GCC is bounded, typed and explicitly non-executable"
          (let [limited (project! :formal-f1/gcc {:manifest/revision-id manifest-rid
                                                  :query/limit 2})]
            (is (= :truncated (:result/status limited)))
            (is (= :limit (:result/truncation limited)))
            (is (= (count expected) (:result/total limited)))
            (is (every? :candidate/non-executable? (:projection/fragments limited)))))
        (doseq [view [linear outline gcc]]
          (is (seq (:drops view)) "each projection names what it omits"))))))
