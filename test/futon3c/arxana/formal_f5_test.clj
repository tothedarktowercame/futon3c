(ns futon3c.arxana.formal-f5-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.digest :as digest] [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f5 :as f5] [futon3c.arxana.service :as service]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store-http :as store-http] [xtdb.node :as xtn])
  (:import [java.io File] [java.nio.file Files]))
(def lean-root "/home/joe/code/futon5/holes/labs/M-formal-arxana/f5-lean")
(defn req [id op body] {:protocol/version "arxana.v1" :message/id id :operation op
                        :principal/id "formal-f5a-test" :body body})
(defn get-record [cfg id] (get-in (service/dispatch cfg (req (str "get:" id) :entity/get
                                                             {:entity/id id})) [:ok :record]))
(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f5a-" ".db") _ (.delete db)]
    (try (with-open [node (xtn/start-node)]
           (let [s (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                             :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store
                                    (str "http://127.0.0.1:" (.getPort (.getAddress s)))
                                    f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider)}]
             (try (f cfg) (finally (f1-server/stop-server! s)))))
         (finally (.delete db)))))
(deftest f5a-square-fibre-is-record-derived-and-kernel-cited
  (with-stack
    (fn [cfg]
      (let [bytes (Files/readAllBytes (.toPath (io/file lean-root f5/lean-source)))
            ingest (f3/ingest-manifestations! cfg [{:artifact/id "formal-f5:lean-geometry"
                                                    :media-type "text/x-lean" :bytes bytes}])
            rid (:artifact/revision-id (first ingest))
            run (edn/read-string (slurp "test/resources/arxana/formal_arxana/F5a-kernel-run.edn"))
            m (f5/materialise! cfg rid run)
            answer (get-in (service/dispatch cfg (req "f5:fibre" :geometry/fibre
                                                       {:map/id :square :value/id :neg-one})) [:ok])]
        (is (every? :ok (:writes m)))
        (testing "every claim and kernel record is read back through public ports"
          (doseq [r (:records m)] (is (= r (get-record cfg (:id r)))))
          (is (= (get-in m [:kernel :attestation])
                 (get-record cfg (get-in m [:kernel :attestation :id])))))
        (testing "fibre answer comes from indexed member records"
          (is (= :complete (:result/status answer)))
          (is (= #{:i :neg-i} (set (:result/member-ids answer))))
          (is (= (get-in m [:kernel :attestation :id]) (:kernel/attestation-id answer)))
          (is (= (get-in m [:kernel :attestation :id])
                 (:id (get-record cfg (:kernel/attestation-id answer)))))
          (doseq [id (:record/ids answer)] (is (= id (:id (get-record cfg id))))))
        (testing "coordinates cannot enter mathematical identity"
          (let [point (get-in m [:entities :i])
                drawn (assoc-in point [:asserted-content :presentation/coordinate] [100 200])]
            (is (= (:id point) (records/clusion-id (dissoc drawn :id))))))
        (testing "inversion/conjugation cites the checked Lean run"
          (let [stored (get-record cfg (get-in m [:symmetry :id]))]
            (is (= "inversion_is_conjugation"
                   (get-in stored [:asserted-content :lean/declaration])))
            (is (= (get-in m [:kernel :attestation :id])
                   (get-in stored [:asserted-content :kernel/attestation-id])))))
        (testing "the fibre detects a stored member count that contradicts its Lean-backed cardinality"
          (let [extra (f5/entity :planted-extra :point)
                link (f5/relation :geometry/member
                                  [(f5/endpoint :fibre (get-in m [:entities :fibre]))
                                   (f5/endpoint :member extra)] {})]
            (doseq [r [extra link]]
              (is (:ok (service/dispatch cfg (assoc (req (str "w:" (:id r)) :clusion/assert
                                                          {:record r})
                                                    :principal/id "formal-f5a/materialiser")))))
            (let [err (:error (service/dispatch cfg (req "f5:fibre:2" :geometry/fibre
                                                       {:map/id :square :value/id :neg-one})))]
              (is (= :conflict (:error/category err)))
              (is (= :arxana.geometry/cardinality-mismatch (:error/code err))))))
        (testing "the cited kernel run checked these exact Lean bytes, sorry-free"
          (is (= (digest/sha256-hex bytes)
                 (:sha256 (first (filter #(= f5/lean-source (:path %)) (:inputs run))))))
          (is (= [:passed true true ""]
                 ((juxt :outcome :sorry-free? :mathlib/unchanged? :output) run)))
          (let [source (String. ^bytes bytes "UTF-8")]
            (doseq [decl ["square_fibre_neg_one" "cube_fibre_neg_one_card"
                          "inversion_is_conjugation"]]
              (is (re-find (re-pattern (str "theorem " decl " ")) source) decl))))))))

(deftest lean-gate-refuses-sorry-even-on-exit-zero
  (is (f5/sorry-free? "theorem t : True := trivial" ""))
  (is (not (f5/sorry-free? "theorem t : 1 = 2 := by sorry" "")))
  (is (not (f5/sorry-free? "theorem t : 1 = 2 := by admit" "")))
  (is (not (f5/sorry-free? "theorem t : True := trivial"
                           "t.lean:1:8: warning: declaration uses 'sorry'"))))
