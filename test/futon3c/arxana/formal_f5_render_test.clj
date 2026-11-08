(ns futon3c.arxana.formal-f5-render-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f5 :as f5]
            [futon3c.arxana.formal-f5-projector :as p]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http] [xtdb.node :as xtn])
  (:import [java.io File] [java.nio.file Files]))
(def lean-root "/home/joe/code/futon5/holes/labs/M-formal-arxana/f5-lean")
(defn req [id op body] {:protocol/version "arxana.v1" :message/id id :operation op
                        :principal/id "formal-f5b-test" :body body})
(defn get-record [cfg id]
  (get-in (service/dispatch cfg (req (str "get:" id) :entity/get {:entity/id id})) [:ok :record]))
(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f5b-" ".db") _ (.delete db)]
    (try (with-open [node (xtn/start-node)]
           (let [s (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                             :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store
                                    (str "http://127.0.0.1:" (.getPort (.getAddress s)))
                                    f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider)}]
             (try (f cfg) (finally (f1-server/stop-server! s)))))
         (finally (.delete db)))))
(defn render [cfg panels id]
  (get-in (service/dispatch cfg (req id :geometry/render
                                     {:map/id :square :value/id :neg-one
                                      :cartesian/panel-id (get-in panels [:cartesian :id])
                                      :angular/panel-id (get-in panels [:angular :id])})) [:ok]))
(defn rendered-record-ids [panel]
  (->> (re-seq #"arxana:clusion:sha256:[0-9a-f]{64}" panel) distinct sort vec))
(deftest f5b-record-driven-two-panel-projection
  (with-stack
    (fn [cfg]
      (let [bytes (Files/readAllBytes (.toPath (io/file lean-root f5/lean-source)))
            rid (:artifact/revision-id (first (f3/ingest-manifestations!
                                               cfg [{:artifact/id "formal-f5b:test-lean"
                                                     :media-type "text/x-lean" :bytes bytes}])))
            run (edn/read-string (slurp "test/resources/arxana/formal_arxana/F5a-kernel-run.edn"))
            geometry (f5/materialise! cfg rid run) presentation (p/materialise! cfg)
            panels (:panels presentation) before (render cfg panels "f5b:before")
            geometry-ids (mapv :id (:records geometry))]
        (is (every? :ok (:writes geometry))) (is (every? :ok (:writes presentation)))
        (doseq [id (concat geometry-ids (map :id (:records presentation)))]
          (is (= id (:id (get-record cfg id)))))
        (testing "deterministic panels carry the same record ids"
          (is (= (:html before) (:html (render cfg panels "f5b:again"))))
          (is (= (:geometry/record-ids before)
                 (rendered-record-ids (get-in before [:panels :cartesian]))))
          (is (= (:geometry/record-ids before)
                 (rendered-record-ids (get-in before [:panels :angular])))))
        (testing "panel-local placement changes Cartesian only"
          (let [point (get-in geometry [:entities :i])
                move (p/placement (:cartesian panels) point 17 -9 "2026-11-08T18:00:00Z")
                _ (is (:ok (p/write! cfg move))) _ (is (= move (get-record cfg (:id move))))
                after (render cfg panels "f5b:moved")]
            (is (not= (get-in before [:panels :cartesian]) (get-in after [:panels :cartesian])))
            (is (= (get-in before [:panels :angular]) (get-in after [:panels :angular])))
            (is (= (:geometry/record-ids before) (:geometry/record-ids after))
                "a drag changes no geometry record id")
            (is (= (:id point) (:id (get-record cfg (:id point)))))
            (testing "a second drag of the same point is the one drawn"
              (let [move2 (p/placement (:cartesian panels) point -30 40 "2026-11-08T18:00:05Z")
                    _ (is (:ok (p/write! cfg move2)))
                    again (render cfg panels "f5b:moved-again")
                    [cx cy] (map #(Double/parseDouble %)
                                 (rest (re-find #"cx='([0-9.]+)' cy='([0-9.]+)'"
                                                (subs (get-in again [:panels :cartesian])
                                                      (.indexOf ^String (get-in again [:panels :cartesian])
                                                                (str "data-record-id='" (:id point) "'"))))))]
                (is (< (Math/abs (- cx 120.0)) 0.01) "i at a quarter turn: x = 150 + 0 - 30")
                (is (< (Math/abs (- cy 80.0)) 0.01) "y = 150 - 110 + 40")))))
        (testing "an undeclared extra member is refused by the F5c cardinality contract"
          (let [point (f5/entity :eighth-turn :point)
                value (p/relation :geometry/value [(p/endpoint :point point)]
                                  {:turn/numerator 1 :turn/denominator 8})
                member (p/relation :geometry/member
                                   [(p/endpoint :fibre (get-in geometry [:entities :fibre]))
                                    (p/endpoint :member point)] {})]
            (doseq [r [point value member]]
              (is (:ok (p/write! cfg r))) (is (= r (get-record cfg (:id r)))))
            (let [answer (:error (service/dispatch cfg
                                                   (req "f5b:member" :geometry/render
                                                        {:map/id :square :value/id :neg-one
                                                         :cartesian/panel-id (get-in panels [:cartesian :id])
                                                         :angular/panel-id (get-in panels [:angular :id])})))]
              (is (= :conflict (:error/category answer)))
              (is (= :arxana.geometry/cardinality-mismatch (:error/code answer))))))))))

(defn- render-after-writing [record-order]
  (with-stack
    (fn [cfg]
      (let [bytes (Files/readAllBytes (.toPath (io/file lean-root f5/lean-source)))
            rid (:artifact/revision-id (first (f3/ingest-manifestations!
                                               cfg [{:artifact/id "formal-f5b:test-lean"
                                                     :media-type "text/x-lean" :bytes bytes}])))
            run (edn/read-string (slurp "test/resources/arxana/formal_arxana/F5a-kernel-run.edn"))
            kernel (f3/append-kernel-run! cfg rid run)
            geometry (f5/plan (get-in kernel [:attestation :id]))
            presentation (p/plan)
            records (record-order (concat (:records geometry) (:records presentation)))]
        (doseq [r records] (is (:ok (p/write! cfg r))))
        (:html (render cfg (:panels presentation) "f5b:order"))))))

(deftest f5b-bytes-do-not-depend-on-write-order
  ;; Same records, written in opposite orders to two fresh stores: same page.
  (let [forward (render-after-writing identity)
        backward (render-after-writing reverse)]
    (is (string? forward))
    (is (= forward backward))))
