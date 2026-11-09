(ns futon3c.arxana.lexicon-loader-test
  (:require [clojure.test :refer [deftest is]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.lexicon-loader :as sut]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [idx (File/createTempFile "lexicon-loader-" ".sqlite") _ (.delete idx)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath idx)})
              cfg {:graph-store (store-http/http-store
                                 (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                                 f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)}]
          (try (f cfg) (finally (f1-server/stop-server! server)))))
      (finally (.delete idx)))))

(defn- call [cfg id op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id
                         :principal/id "lexicon-loader-test" :operation op :body body}))
(defn- get-record [cfg id] (get-in (call cfg (str "get:" id) :entity/get {:entity/id id}) [:ok :record]))
(defn- entity [slug]
  (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                         :asserted-content {:entity/slug slug}}))
(defn- ref-record [source verb target]
  (records/make-clusion {:type-revision [:workplan/referent 1]
                         :endpoints [{:role :entity :kind :clusion :target {:clusion/id (:id (entity source))}}
                                     {:role :target :kind :clusion :target {:clusion/id (:id (entity target))}}]
                         :asserted-content {:referent/role verb}}))

(deftest real-input-plan-has-no-correctable-identity
  (let [p (sut/plan) entity-record (some #(when (= (:id (entity "agency")) (get-in % [:record :id])) (:record %)) (:records p))]
    (is (= 254 (count (:rows p))))
    (is (= 158 (count (filter (comp :live val) (:live-tier p)))))
    (is (= 158 (count (filter (comp :tier val) (:live-tier p)))))
    ;; The current file parses to 742 targets (claude-6's earlier survey said
    ;; 744); keep this assertion tied to the bytes actually loaded.
    (is (= 742 (+ (count (:resolved p)) (count (:unresolved p)))))
    (is (= 153 (count (:unresolved p))))
    (is (= 9 (count (get-in p [:survey :new-rows]))))
    (is (= {:entity/slug "agency"} (:asserted-content entity-record)))))

(deftest idempotent-real-input-load-keeps-competing-claims
  (with-stack
    (fn [cfg]
      ;; An already-existing Joe introduction is byte-identical after loading.
      (doseq [slug ["arxana" "agency"]]
        (is (:ok (call cfg (str "seed:" slug) :clusion/assert {:record (entity slug)}))))
      (let [intro (records/make-clusion
                   {:type-revision [:workplan/introduction 1]
                    :endpoints [{:role :entity :kind :clusion :target {:clusion/id (:id (entity "arxana"))}}
                                {:role :source :kind :external-ref
                                 :target {:ref/store :evidence :ref/id "emacs-existing-arxana"}}]
                    :asserted-content {}})]
        (is (:ok (call cfg "seed:introduction" :clusion/assert {:record intro})))
        (let [before (get-record cfg (:id intro))
              first-run (sut/materialise! #(service/dispatch cfg %))
              second-run (sut/materialise! #(service/dispatch cfg %))]
          (is (empty? (:failures first-run)))
          (is (pos? (get-in first-run [:counts :records/created])))
          (is (zero? (get-in second-run [:counts :records/created])))
          (is (= before (get-record cfg (:id intro))))
          (doseq [[source verb target] [["kangaroo-pouch" "part-of" "agency"]
                                        ["kangaroo-pouch" "in-namespace" "agency"]
                                        ["kangaroo-pouch" "extends" "agency"]
                                        ["arxana" "uses" "arxana-store"]
                                        ;; futon4 is an alias of the Arxana entity in claude-6's data.
                                        ["arxana" "part-of" "arxana"]]]
            (let [r (ref-record source verb target)]
              (is (= r (get-record cfg (:id r))))))
          (let [{:keys [source verb target]} (first (:unresolved (sut/plan)))
                absent (ref-record source verb target)]
            (is (:error (call cfg "unresolved-missing" :entity/get {:entity/id (:id absent)})))))))))
