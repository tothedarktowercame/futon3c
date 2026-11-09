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
      (let [slice (workplan/read-slice "/home/joe/notes/cascade-live-outline/arxana/t-pattern-search-slice.edn") first-run (workplan/materialise! #(service/dispatch cfg %) slice)
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

(deftest all-workplan-items-load-and-list
  (with-stack
    (fn [cfg]
      (let [slice (workplan/read-slice) started (System/nanoTime)
            loaded (workplan/materialise! #(service/dispatch cfg %) slice)
            elapsed-ms (/ (- (System/nanoTime) started) 1e6)
            replay (workplan/materialise! #(service/dispatch cfg %) slice)
            list-started (System/nanoTime)
            page1 (get-in (call cfg "list:1" :workplan/items {:query/limit 200}) [:ok])
            page2 (get-in (call cfg "list:2" :workplan/items {:query/limit 200 :query/offset 200}) [:ok])
            items (vec (concat (:items page1) (:items page2)))
            closed (first (filter #(= "E-R14-red-ring-fill" (:item/id %)) items))
            triple (first (filter #(>= (count (:benefits %)) 3) items))
            ticket (get-in (call cfg "ticket" :workplan/item-view {:item/id "T-pattern-search-reads-stale-index"}) [:ok])
            closed-view (get-in (call cfg "closed" :workplan/item-view {:item/id "E-R14-red-ring-fill"}) [:ok])]
        (println "WORKPLAN2_TIMING" {:load-ms elapsed-ms :list-ms (/ (- (System/nanoTime) list-started) 1e6)})
        (is (= {:records 3881 :clusions 1720 :attestations 2160 :assessments 1 :items 323} (:counts loaded)))
        (is (empty? (:failures loaded)))
        (is (= 3881 (:created loaded)))
        (is (zero? (:created replay)))
        (is (= 323 (count items)))
        (is (= :truncated (:result/status page1)))
        (is (= :complete (:result/status page2)))
        (is closed)
        (is (= "joe" (get-in closed [:rulings 0 :actor])))
        (is (= "joe" (get-in closed-view [:assessments 0 :actor])))
        (is (= "~/notes/cascade-live-outline/pending/closed-by-joe.tsv"
               (get-in closed-view [:assessments 0 :source-refs 0 :path])))
        (is (= 5 (count (:mentions ticket))))
        (is triple)
        (is (= 3 (count (take 3 (:benefits triple)))))
        (is (= 323 (get-in page1 [:result/total])))
        (doseq [record (:records loaded)]
          (is (= record (get-in (call cfg (str "all:get:" (:id record)) :entity/get {:entity/id (:id record)}) [:ok :record]))))))))
