(ns futon3c.arxana.workplan-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http] [futon3c.arxana.workplan :as workplan]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "workplan-" ".db") _ (.delete db)]
    (try (with-open [node (xtn/start-node)]
           (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1" :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store (str "http://127.0.0.1:" (.getPort (.getAddress server))) f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider)}]
             (try (f cfg) (finally (f1-server/stop-server! server)))))
         (finally (.delete db)))))
(defn- call [cfg id op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id :operation op :principal/id "workplan-test" :body body}))

(deftest one-workplan-item-end-to-end
  (with-stack
    (fn [cfg]
      (let [slice (workplan/read-slice) first-run (workplan/materialise! #(service/dispatch cfg %) slice)
            second-run (workplan/materialise! #(service/dispatch cfg %) slice)
            view (get-in (call cfg "view" :workplan/item-view {:item/id (:slice slice)}) [:ok])]
        (is (= 29 (count (:records first-run))))
        (is (= 29 (:created first-run)))
        (is (zero? (:created second-run)))
        (doseq [record (:records first-run)]
          (is (= record (get-in (call cfg (str "get:" (:id record)) :entity/get {:entity/id (:id record)}) [:ok :record]))))
        (is (= 5 (count (:mentions view))))
        (is (= (sort (map :time (:mentions view))) (map :time (:mentions view))))
        (is (= 4 (count (filter :quote (:mentions view)))))
        (is (= 3 (count (:names view))))
        (is (= 1 (count (:serves view))))
        (is (= 6 (some #(get-in % [:content :pending/level]) (get-in view [:attestations :drafts]))))
        (is (= "no ruling by Joe" (:standing view)))
        (testing "dangling local id is a typed refusal"
          (let [bad (update slice :records conj {:record :clusion :local-id :bad :type-revision [:workplan/names 1]
                                                  :identity {} :endpoints [{:role :item :kind :clusion :target {:local-id :missing}}
                                                                           {:role :entity :kind :clusion :target {:local-id :item}}]})]
            (is (= :arxana.workplan/dangling-local-id
                   (:error (ex-data (try (workplan/plan bad) (catch clojure.lang.ExceptionInfo e e))))))))))))
