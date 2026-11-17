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
                   :focus-relation-input {:evidence-pin {:evidence-pin [["focus-1" "sha"]]}}}))
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
    (is (= :no-pinned-focus-evidence (:focus-reason result)))))

(deftest relation-whose-focus-header-is-not-pinned-is-left-out
  ;; The run pins 猴 focus headers; rel-1's header focus-1 is pinned in
  ;; run-dir. Re-pin a different header and the same relation must drop out.
  (let [dir (run-dir)]
    (spit (io/file dir "manifest.edn")
          (pr-str {:run-id "fixture-run"
                   :focus-relation-input {:evidence-pin {:evidence-pin [["focus-other" "sha"]]}}}))
    (let [row (first (:items (qa/top-items (assoc (opts dir) :n 1))))]
      (is (= :absent (:focus-status row)))
      (is (empty? (:focus-relations row))))))

(deftest truncated-focus-relation-query-is-reported
  (let [dir (run-dir)
        o (assoc (opts dir) :query-fn (fn [_ _] {:ok {:status :ok :truncated? true :edge-ids ["rel-1"]}}))
        row (first (:items (qa/top-items (assoc o :n 1))))]
    (is (true? (get-in row [:focus-pin :truncated?])))))

(defn- tag-opts [dir named pinned? relation?]
  (spit (io/file dir "manifest.edn")
        (pr-str {:run-id "fixture-run"
                 :focus-relation-input
                 {:evidence-pin {:evidence-pin
                                 (cond-> [["focus-1" "sha"]]
                                   pinned? (conj ["turn/turn-tag" "turn-sha"]))}}}))
  (merge (opts dir)
         {:items [{:id "M-old"} {:id "M-ranked"}]
          :query-fn (fn [_ _]
                      {:ok {:status :ok :truncated? false
                            :edge-ids (if relation? ["rel-1"] [])}})
          :read-turn-fn (fn [_ id]
                          (when (= id "turn-tag")
                            {:created_at "2026-11-17T01:00:00Z"
                             :agent_id "seat-a" :mission_id "M-old"}))
          :read-turn-focus-fn
          (fn [_ {:keys [id record]}]
            {:turn {:id id :created-at (:created_at record)
                    :seat (:agent_id record) :mission-id (:mission_id record)
                    :named named}})
          :with-successors-fn
          (fn [_ items _]
            (mapv #(assoc % :successors (if (= "M-old" (:id %))
                                         ["M-ranked"] [])) items))}))

(deftest mission-tag-focus-follows-one-supersession-step
  (let [row (first (:items (qa/top-items (assoc (tag-opts (run-dir) [] true false)
                                                :n 1))))]
    (is (= :present (:focus-status row)))
    (is (= "mission-tag" (:focus-route row)))
    (is (= [{:turn-id "turn-tag" :created_at "2026-11-17T01:00:00Z"
             :seat "seat-a" :tag "M-old"
             :supersession-path ["M-old" "M-ranked"]}]
           (:focus-tags row)))))

(deftest relation-and-mission-tag-report-both
  (let [row (first (:items (qa/top-items (assoc (tag-opts (run-dir) [] true true)
                                                :n 1))))]
    (is (= "both" (:focus-route row)))
    (is (seq (:focus-relations row)))
    (is (seq (:focus-tags row)))))

(deftest named-header-turn-is-not-mission-tag-focus
  (let [row (first (:items (qa/top-items
                            (assoc (tag-opts (run-dir) ["M-ranked"] true false)
                                   :n 1))))]
    (is (= :absent (:focus-status row)))
    (is (empty? (:focus-tags row)))))

(deftest mission-tag-turn-outside-pin-is-ignored
  (let [row (first (:items (qa/top-items (assoc (tag-opts (run-dir) [] false false)
                                                :n 1))))]
    (is (= :absent (:focus-status row)))
    (is (empty? (:focus-tags row)))))
