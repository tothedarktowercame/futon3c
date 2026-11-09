(ns futon3c.arxana.workplan-backlinks-test
  (:require [clojure.test :refer [deftest is]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.workplan-backlinks :as sut]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(def fixtures "test/resources/arxana/backlinks")
(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "backlinks-" ".db") idx (File/createTempFile "backlinks-derived-" ".edn") _ (.delete db)]
    (spit idx (slurp sut/default-index))
    (try (with-open [node (xtn/start-node)]
           (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1" :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store (str "http://127.0.0.1:" (.getPort (.getAddress server))) f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider) :workplan/backlinks-index (.getPath idx)}]
             (try (f cfg) (finally (f1-server/stop-server! server)))))
         (finally (.delete db) (.delete idx)))))
(defn- call [cfg id op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id :operation op :principal/id "backlinks-test" :body body}))

(deftest exact-forms-build-a-deterministic-derived-index
  (let [rules (sut/read-rules) responses (sut/recorded-responses fixtures)
        a (sut/build-index rules responses) b (sut/build-index rules responses)
        switched (update-in rules [:rules 2 :forms 0] assoc :enabled? false)
        c (sut/build-index switched responses)]
    (is (= a b))
    (is (= {:hits 50 :kept 4 :dropped/metadata-only 46 :occurrences 4}
           (dissoc (get-in a [:stats "Agency"]) :entity/slug)))
    (is (= 24 (count (:backlinks a))))
    (is (= 19 (count (:backlinks c))))
    (is (= (remove #(and (= "kangaroo-pouch" (:entity/slug %)) (= "kangaroo" (:form %))) (:backlinks a))
           (:backlinks c)))
    (is (= #{"Agency"} (set (map :form (filter #(= "agency" (:entity/slug %)) (:backlinks a))))))))

(deftest stored-rules-and-public-view
  (with-stack
    (fn [cfg]
      (let [rows (sut/rule-records (sut/read-rules))
            _ (doseq [{:keys [entity]} rows]
                (is (get-in (call cfg (str "entity:" (:id entity)) :clusion/assert {:record entity}) [:ok :created?])))
            first-run (sut/materialise-rules! #(service/dispatch cfg %) (sut/read-rules))
            second-run (sut/materialise-rules! #(service/dispatch cfg %) (sut/read-rules))]
        (is (every? #(true? (get-in % [0 :ok :created?])) first-run))
        ;; C2 message replay returns its first outcome; content ids prove the
        ;; replay addressed exactly the same six immutable records.
        (is (= (mapv #(mapv (fn [r] (get-in r [:ok :record/id])) %) first-run)
               (mapv #(mapv (fn [r] (get-in r [:ok :record/id])) %) second-run)))
        (doseq [slug ["agency" "giraffe" "kangaroo-pouch"]]
          (let [view (:ok (call cfg (str "view:" slug) :workplan/entity-backlinks {:entity/slug slug :query/limit 100}))]
            (is (= slug (get-in view [:entity :asserted-content :entity/slug])))
            (is (= :complete (:result/status view)))
            (doseq [id (:record/ids view)]
              (is (= id (get-in (call cfg (str "get:" id) :entity/get {:entity/id id}) [:ok :record :id]))))))
        (is (= ["agency" "giraffe" "kangaroo-pouch"]
               (get-in (call cfg "list" :workplan/entity-backlinks {}) [:ok :entities])))))))
