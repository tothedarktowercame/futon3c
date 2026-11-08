(ns futon3c.arxana.formal-f5-change-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.digest :as digest]
            [futon3c.arxana.formal-f3 :as f3] [futon3c.arxana.formal-f5 :as f5]
            [futon3c.arxana.formal-f5-projector :as p] [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http] [xtdb.node :as xtn])
  (:import [java.io File] [java.nio.file Files]))
(def lean-root "/home/joe/code/futon5/holes/labs/M-formal-arxana/f5-lean")
(defn req [id op body] {:protocol/version "arxana.v1" :message/id id :operation op
                        :principal/id "formal-f5c-test" :body body})
(defn call [cfg id op body] (service/dispatch cfg (req id op body)))
(defn get-record [cfg id] (get-in (call cfg (str "get:" id) :entity/get {:entity/id id}) [:ok :record]))
(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f5c-" ".db") _ (.delete db)]
    (try (with-open [node (xtn/start-node)]
           (let [s (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                             :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store
                                    (str "http://127.0.0.1:" (.getPort (.getAddress s)))
                                    f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider)}]
             (try (f cfg) (finally (f1-server/stop-server! s)))))
         (finally (.delete db)))))
(defn render [cfg id] (:ok (call cfg id :geometry/render {:value/id :neg-one})))
(deftest f5c-square-to-cube-is-an-append-only-semantic-change
  (with-stack
    (fn [cfg]
      (let [source (Files/readAllBytes (.toPath (io/file lean-root f5/lean-source)))
            run (f5/run-lean-gate lean-root)
            rid (:artifact/revision-id (first (f3/ingest-manifestations!
                                               cfg [{:artifact/id "formal-f5c:test-lean"
                                                     :media-type "text/x-lean" :bytes source}])))
            geometry (f5/materialise! cfg rid run) presentation (p/materialise! cfg)
            cube (p/cube-plan geometry (get-in geometry [:kernel :attestation :id]))
            cube-writes (mapv #(p/write! cfg %) (:records cube))
            before (render cfg "f5c:before")
            square-selection (:selection presentation)
            circle-id (get-in geometry [:entities :circle :id])
            neg-one-id (get-in geometry [:entities :neg-one :id])]
        (is (= :passed (:outcome run))) (is (:sorry-free? run)) (is (:mathlib/unchanged? run))
        (is (= (digest/sha256-hex source)
               (:sha256 (first (filter #(= f5/lean-source (:path %)) (:inputs run))))))
        (is (re-find #"cube_fibre_neg_one_card" (String. source "UTF-8")))
        (is (every? :ok (:writes geometry))) (is (every? :ok (:writes presentation)))
        (is (every? :ok cube-writes))
        (is (= :arxana.geometry/unknown-map
               (get-in (call cfg "f5c:unknown-map" :geometry/fibre
                             {:map/id :unknown :value/id :neg-one}) [:error :error/code])))
        (is (= :arxana.geometry/unknown-value
               (get-in (call cfg "f5c:unknown-value" :geometry/fibre
                             {:map/id :square :value/id :unknown}) [:error :error/code])))
        (doseq [r (concat (:records geometry) (:records presentation) (:records cube))]
          (is (= r (get-record cfg (:id r)))))
        (testing "square is selected first and both panels show its two stored members"
          (is (= :square (:map/id before)))
          (is (= #{:i :neg-i}
                 (set (get-in (call cfg "f5c:square-fibre" :geometry/fibre
                                    {:map/id :square :value/id :neg-one})
                              [:ok :result/member-ids]))))
          (is (every? #(re-find (re-pattern (name %)) (get-in before [:panels :cartesian]))
                      [:i :neg-i]))
          (is (every? #(re-find (re-pattern (name %)) (get-in before [:panels :angular]))
                      [:i :neg-i])))
        (testing "switch appends a selection and propagates cube members to both panels"
          (let [switched (call cfg "f5c:select-cube" :geometry/select
                               {:map/id :cube :selected-at "2026-11-08T18:30:00.000Z"})
                after (render cfg "f5c:after")
                fibre (:ok (call cfg "f5c:cube-fibre" :geometry/fibre
                                 {:map/id :cube :value/id :neg-one}))]
            (is (:ok switched)) (is (= :cube (:map/id after)))
            (is (= #{:cube-root-1 :neg-one :cube-root-3} (set (:result/member-ids fibre))))
            (is (= 3 (:fibre/cardinality fibre)))
            (is (= "cube_fibre_neg_one_card" (:lean/declaration fibre)))
            (is (= (get-in geometry [:kernel :attestation :id]) (:kernel/attestation-id fibre)))
            (is (= (get-in geometry [:kernel :attestation])
                   (get-record cfg (:kernel/attestation-id fibre))))
            (doseq [label ["cube-root-1" "neg-one" "cube-root-3"]]
              (is (re-find (re-pattern label) (get-in after [:panels :cartesian])))
              (is (re-find (re-pattern label) (get-in after [:panels :angular]))))
            (is (= square-selection (get-record cfg (:id square-selection))))
            (testing "the angular panel's exponent comes from the stored map"
              (is (re-find #"2θ=" (get-in before [:panels :angular])))
              (is (re-find #"3θ=" (get-in after [:panels :angular])))
              (is (not (re-find #"2θ=" (get-in after [:panels :angular])))))
            (testing "nothing stored before the switch was rewritten or removed"
              (doseq [r (concat (:records geometry) (:records presentation))]
                (is (= r (get-record cfg (:id r)))))
              (is (= #{:i :neg-i}
                     (set (get-in (call cfg "f5c:square-after" :geometry/fibre
                                        {:map/id :square :value/id :neg-one})
                                  [:ok :result/member-ids])))))
            (testing "selection order is event time, not write order or id"
              (is (= :arxana.geometry/selected-at-not-iso-instant
                     (get-in (call cfg "f5c:epoch" :geometry/select
                                   {:map/id :square :selected-at 1794161751903})
                             [:error :error/code])))
              (is (:ok (call cfg "f5c:older-square" :geometry/select
                             {:map/id :square :selected-at "2026-11-08T18:29:59.999Z"})))
              (is (= :cube (:map/id (render cfg "f5c:still-cube")))
                  "a selection written later but timed earlier does not win")
              (is (:ok (call cfg "f5c:back-to-square" :geometry/select
                             {:map/id :square :selected-at "2026-11-08T18:31:00.000Z"})))
              (is (= :square (:map/id (render cfg "f5c:square-again")))))
            (is (= circle-id (get-in (get-record cfg circle-id) [:id])))
            (is (= neg-one-id (get-in (get-record cfg neg-one-id) [:id])))))
        (testing "cardinality mismatch is a typed refusal, never a fixed answer"
          (let [fourth (f5/entity :planted-fourth-root :point)
                member (p/relation :geometry/member
                                   [(p/endpoint :fibre (:fibre cube)) (p/endpoint :member fourth)] {})]
            (doseq [r [fourth member]] (is (:ok (p/write! cfg r))) (is (= r (get-record cfg (:id r)))))
            (let [answer (:error (call cfg "f5c:mismatch" :geometry/fibre
                                       {:map/id :cube :value/id :neg-one}))]
              (is (= :conflict (:error/category answer)))
              (is (= :arxana.geometry/cardinality-mismatch (:error/code answer))))))))))

(deftest f5c-cube-answer-is-read-from-whatever-is-stored
  ;; A fixed {cube-root-1 neg-one cube-root-3} would pass the main test, whose
  ;; store holds exactly those. Here the cube fibre's members are other points.
  (with-stack
    (fn [cfg]
      (let [source (Files/readAllBytes (.toPath (io/file lean-root f5/lean-source)))
            rid (:artifact/revision-id (first (f3/ingest-manifestations!
                                               cfg [{:artifact/id "formal-f5c:alt-lean"
                                                     :media-type "text/x-lean" :bytes source}])))
            run (edn/read-string
                 (slurp "test/resources/arxana/formal_arxana/F5a-kernel-run.edn"))
            geometry (f5/materialise! cfg rid run)
            att (get-in geometry [:kernel :attestation :id])
            fibre (f5/entity :alt-cube-fibre :fibre)
            points (mapv #(f5/entity % :point) [:alt-a :alt-b :alt-c])
            records (concat [fibre]
                            points
                            [(f5/relation-rev :geometry/fibre-of 2
                                              [(p/endpoint :fibre fibre)
                                               (p/endpoint :map (get-in geometry [:maps :cube]))
                                               (p/endpoint :value (get-in geometry [:entities :neg-one]))]
                                              {:kernel/attestation-id att
                                               :lean/declaration "cube_fibre_neg_one_card"
                                               :fibre/cardinality 3})]
                            (map #(p/relation :geometry/member
                                              [(p/endpoint :fibre fibre) (p/endpoint :member %)] {})
                                 points))]
        (doseq [r records] (is (:ok (p/write! cfg r))) (is (= r (get-record cfg (:id r)))))
        (is (= #{:alt-a :alt-b :alt-c}
               (set (get-in (call cfg "f5c:alt" :geometry/fibre {:map/id :cube :value/id :neg-one})
                            [:ok :result/member-ids]))))))))
