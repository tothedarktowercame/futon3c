(ns futon3c.arxana.formal-f6-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f5 :as f5]
            [futon3c.arxana.formal-f5-projector :as f5-projector]
            [futon3c.arxana.formal-f6 :as f6]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f6a-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server!
                      {:node node :port 0 :bind-host "127.0.0.1"
                       :arxana-index-path (.getPath db)})
              cfg {:graph-store
                   (store-http/http-store
                    (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                    f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)}]
          (try (f cfg) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn- request [mid op principal body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id principal :body body})
(defn- call [cfg mid op principal body]
  (service/dispatch cfg (request mid op principal body)))
(defn- get-record [cfg id]
  (get-in (call cfg (str "get:" id ":" (random-uuid)) :entity/get "f6-test"
                {:entity/id id}) [:ok :record]))
(defn- assert-record! [cfg record]
  (call cfg (str "seed:" (:id record)) :clusion/assert "f6-test" {:record record}))

(defn- fixture! [cfg]
  (let [geometry (f5/plan "kernel:f5-fixture")
        projector (f5-projector/plan)
        _ (doseq [record (concat (:records geometry) (:records projector))]
            (is (:ok (assert-record! cfg record)) (str (:id record))))
        plan (f6/plan geometry (:panels projector) "kernel:f5-fixture")
        writes (f6/materialise! #(service/dispatch cfg %) plan)]
    (is (every? :ok writes))
    plan))

(defn- view [cfg plan suffix]
  (call cfg (str "view:" suffix) :formal-f6/argument "f6-test"
        {:argument/id (get-in plan [:argument :id])}))

(defn- item-row [response id]
  (first (filter #(= id (:record/id %)) (get-in response [:ok :items]))))

(defn- support-records [response supporting-id]
  (->> (get-in response [:ok :supports])
       (map :record)
       (filter (fn [record]
                 (= supporting-id
                    (get-in (first (filter #(= :supporting-content (:role %))
                                           (:endpoints record)))
                            [:target :clusion/id]))))
       vec))

(deftest argument-records-moves-and-resupports-are-orthogonal
  (with-stack
    (fn [cfg]
      (let [plan (fixture! cfg)
            argument-id (get-in plan [:argument :id])
            diagram (get-in plan [:items :diagram])
            square (get-in plan [:items :square-claim])
            cube (get-in plan [:items :cube-claim])
            before (view cfg plan "before")
            supports-before (mapv :record (get-in before [:ok :supports]))
            diagram-before (get-record cfg (:id diagram))
            move-margin (call cfg "move:margin" :formal-f6/move "joe"
                              {:argument/id argument-id :item/id (:id diagram)
                               :zone :margin :order 1 :offset/x 12 :offset/y 0
                               :placed-at "2026-11-08T00:00:02.000Z"})
            current-after (fn [suffix]
                            (get-in (item-row (view cfg plan suffix) (:id diagram))
                                    [:placement :id]))
            at-margin (current-after "at-margin")
            move-main (call cfg "move:main" :formal-f6/move "joe"
                            {:argument/id argument-id :item/id (:id diagram)
                             :zone :main :order 3 :offset/x 0 :offset/y 0
                             :placed-at "2026-11-08T00:00:03.000Z"})
            at-main (current-after "at-main")
            ;; written last but placed earlier: must not become current
            backdated (call cfg "move:backdated" :formal-f6/move "joe"
                            {:argument/id argument-id :item/id (:id diagram)
                             :zone :margin :order 9 :offset/x 0 :offset/y 0
                             :placed-at "2026-11-08T00:00:02.500Z"})
            after-backdated (current-after "after-backdated")
            moved (view cfg plan "moved")
            diagram-row (item-row moved (:id diagram))
            supports-moved (mapv :record (get-in moved [:ok :supports]))]
        (testing "the public read returns exact stored records"
          (doseq [row (get-in before [:ok :items])]
            (is (= (:record row) (get-record cfg (:record/id row)))))
          (doseq [row (get-in before [:ok :supports])]
            (is (= (:record row) (get-record cfg (:record/id row))))))
        (testing "moving changes only placement history and latest is event-time ordered"
          (is (:ok move-margin))
          (is (:ok move-main))
          (is (= diagram-before (get-record cfg (:id diagram))))
          (is (= supports-before supports-moved))
          (is (= (get-in move-margin [:ok :placement/id]) at-margin))
          (is (= (get-in move-main [:ok :placement/id]) at-main))
          (is (:ok backdated))
          (is (= (get-in move-main [:ok :placement/id]) after-backdated))
          (is (= 4 (count (:placement/history-ids diagram-row))))
          (is (= :main (get-in diagram-row [:placement :asserted-content :zone])))
          (is (= 3 (get-in diagram-row [:placement :asserted-content :order]))))
        (testing "re-support appends an attributable edge and keeps the old one"
          (let [old-support (first (support-records moved (:id diagram)))
                changed (call cfg "support:cube" :formal-f6/re-support "joe"
                              {:argument/id argument-id
                               :supporting-content/id (:id diagram)
                               :supported-claim/id (:id cube)
                               :reason "the diagram now explains the cube fibre"
                               :changed-at "2026-11-08T00:00:04.000Z"
                               :evidence ["kernel:f5-fixture"]})
                after (view cfg plan "resupported")
                diagram-supports (support-records after (:id diagram))
                new-id (get-in changed [:ok :support/id])
                repeated (call cfg "support:cube:again" :formal-f6/re-support "joe"
                               {:argument/id argument-id
                                :supporting-content/id (:id diagram)
                                :supported-claim/id (:id cube)
                                :reason "same claim"
                                :changed-at "2026-11-08T00:00:05.000Z"})
                final (view cfg plan "final")]
            (is (true? (get-in changed [:ok :created?])))
            (is (= old-support (get-record cfg (:id old-support))))
            (is (= "joe" (get-in (get-record cfg new-id) [:asserted-content :actor])))
            (is (= 2 (count diagram-supports)))
            (is (contains? (set (get-in after [:ok :current/support-ids])) new-id))
            (is (not (contains? (set (get-in after [:ok :current/support-ids]))
                                (:id old-support))))
            (is (= {(:id old-support) false new-id true}
                   (into {} (for [row (get-in after [:ok :supports])
                                  :when (#{(:id old-support) new-id} (:record/id row))]
                              [(:record/id row) (:current? row)]))))
            (let [supersession (get-record cfg (get-in changed [:ok :supersession/id]))
                  role-id (fn [role] (get-in (first (filter #(= role (:role %))
                                                            (:endpoints supersession)))
                                             [:target :clusion/id]))]
              (is (= [:f6/re-supports 1] (:type-revision supersession)))
              (is (= new-id (role-id :successor)))
              (is (= (:id old-support) (role-id :predecessor)))
              (is (= "joe" (get-in supersession [:asserted-content :actor])))
              (is (= "the diagram now explains the cube fibre"
                     (get-in supersession [:asserted-content :reason]))))
            (is (false? (get-in repeated [:ok :created?])))
            (is (= new-id (get-in repeated [:ok :support/id])))
            (is (= 2 (count (support-records final (:id diagram)))))
            (is (not= (:id square) (:id cube)))))))))

(deftest content-constructor-refuses-layout-leakage
  (is (= :arxana.f6/layout-in-content
         (:error (ex-data
                  (try (f6/item :bad :diagram {:text "x" :layout {:x 3}} [])
                       (catch clojure.lang.ExceptionInfo e e)))))))
