(ns futon3c.circus.qa-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.circus.hou :as hou]
            [futon3c.circus.qa :as qa]))

(defn- run-dir []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "circus-qa" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (spit (io/file dir "manifest.edn")
          (pr-str {:run-id "fixture-run"
                   :focus-relation-input {:evidence-pin {:evidence-pin [["rel-1" "sha"]]}}}))
    (spit (io/file dir "input.edn")
          (pr-str {:items [{:id :M-ranked} {:id :M-excluded} {:id :M-no-focus}]}))
    (spit (io/file dir "excluded.edn")
          (pr-str [{:id "M-excluded" :reason :operator-gated :status-line "fixture"}]))
    (spit (io/file dir "rejected.edn") "[]")
    (spit (io/file dir "pass.edn")
          (pr-str {:ready-set [:M-ranked :M-no-focus]
                   :order [{:id :M-ranked :rank 4} {:id :M-no-focus :rank 5}]}))
    (spit (io/file dir "priority.edn")
          (pr-str [{:target :M-ranked :pass-rank 4 :priority-band :focus
                    :cutoff-status :within-cutoff}
                   {:target :M-no-focus :pass-rank 5 :priority-band :ready
                    :cutoff-status :within-cutoff}]))
    (str dir)))

(def entities
  {"rel-1" {:id "rel-1" :type-revision [:hou/focus-relation 1]
             :endpoints [{:role :focus :target {:clusion/id "focus-1"}}
                         {:role :item :target {:clusion/id "item-1"}}]}
   "focus-1" {:id "focus-1" :asserted-content {:hou/turn "turn-1"}}
   "item-1" {:id "item-1" :asserted-content {:item/id "M-ranked"}}})

(defn- opts [dir]
  {:run-dir dir
   :get-entity-fn (fn [_ id] {:ok (get entities id)})
   :query-fn (fn [_ query]
               (is (= [:hou/focus-relation 1] (:edge-type query)))
               {:ok {:status :ok :truncated? false
                     :edge-ids (if (= (:endpoint-id query)
                                      (:id (hou/item-record "M-ranked")))
                                 ["rel-1"] [])}})
   :chain-fn (fn [{:keys [turn-id]}]
               {:turn {:id turn-id :created_at "2026-11-17T00:00:00Z"}
                :gcc {:驴 {:clusion-ids ["lu"]} :馬 {:clusion-ids []}
                      :狗 {:clusion-ids ["dog"]} :象 {:clusion-ids ["xiang"]}
                      :mentions {:clusion-ids ["mention"]} :猴 {:clusion-ids ["focus-1" "rel-1"]}}})})

(deftest top-items-explains-pinned-focus-and-turn-gcc-counts
  (let [result (qa/top-items (assoc (opts (run-dir)) :n 1))
        row (first (:items result))]
    (is (= "M-ranked" (:item row)))
    (is (= ["rel-1"] (mapv :clusion-id (:focus-relations row))))
    (is (= {:id "turn-1" :created_at "2026-11-17T00:00:00Z"}
           (get-in row [:focus-relations 0 :turn])))
    (is (= 1 (get-in row [:focus-relations 0 :gcc-counts :驴])))
    (is (true? (:truncated? result)))))

(deftest why-not-stops-at-excluded
  (let [result (qa/why-not (assoc (opts (run-dir)) :item "M-excluded"))]
    (is (= :excluded (:stage result)))
    (is (= :operator-gated (:reason result)))))

(deftest why-not-ranked-item
  (let [result (qa/why-not (assoc (opts (run-dir)) :item "M-ranked"))]
    (is (= :ranked (:stage result)))
    (is (= 1 (get-in result [:evidence :rank])))
    (is (= :present (:focus-status result)))))

(deftest rejected-third-party-edge-does-not-reject-its-from-item
  (let [dir (run-dir)]
    (spit (io/file dir "rejected.edn")
          (pr-str [{:from "M-ranked" :to "M-other" :reason :edge-rejected}]))
    (is (= :ranked (:stage (qa/why-not (assoc (opts dir) :item "M-ranked")))))))

(deftest ranked-item-without-focus-relation-is-typed
  (let [result (qa/why-not (assoc (opts (run-dir)) :item "M-no-focus"))]
    (is (= :ranked (:stage result)))
    (is (= :absent (:focus-status result)))
    (is (= :no-pinned-focus-relation (:focus-reason result)))))
