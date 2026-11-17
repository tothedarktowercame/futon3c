(ns futon3c.circus.chain-record-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.circus.chain-record :as chain]))

(defn- fixture-run [pins]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "chain-record" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (spit (io/file dir "input.edn") "{:fixture true}\n")
    (spit (io/file dir "priority.edn")
          (pr-str [{:target :M-one :pass-rank 12 :priority-band :focus :cutoff-status :within-cutoff}
                   {:target :M-two :pass-rank 30 :priority-band :associated :cutoff-status :outside-cutoff}]))
    (spit (io/file dir "manifest.edn")
          (pr-str {:run-id "run-fixture" :git-sha "git-fixture"
                   :readings-generation {:digest "generation"}
                   :focus/version {:schema [:hou/focus 1] :digest "focus-digest"}
                   :focus-relation-input {:evidence-pin {:evidence-pin pins}}}))
    (str dir)))

(defn- fixture-config [run-dir absent-animal]
  (let [ids {:驴 ["lu-1"] :馬 ["ma-1"] :狗 ["dog-1"] :象 ["xiang-1"]
             :mentions ["mention-1"] :猴 ["hou-1"]}
        by-type {[:pattern/proposal 1] :驴 [:pattern/occurrence 1] :馬
                 [:memory/recall 1] :狗 [:interpretation 1] :象
                 [:workplan/mention 2] :mentions [:hou/focus 1] :猴}
        entities {"mention-1" {:id "mention-1"
                                :endpoints [{:role :item :target {:clusion/id "item-1"}}]}
                  "hou-link-1" {:id "hou-link-1"
                                 :endpoints [{:role :focus :target {:clusion/id "hou-1"}}
                                             {:role :item :target {:clusion/id "item-1"}}]}
                  "item-1" {:id "item-1" :asserted-content {:item/id "M-one"}}}]
    {:turn-id "turn-fixture" :run-dir run-dir :turn-reference-id "turn-ref"
     :read-turn-fn (fn [_ _] {:created_at "2026-11-17T00:00:00Z"
                               :origin "operator" :reply_text "not exposed"})
     :verify-run-fn (constantly {:ok? true :mismatches []})
     :query-fn (fn [_ q]
                 (let [animal (get by-type (:edge-type q))
                       rows (cond
                              (= [:hou/focus-relation 1] (:edge-type q)) ["hou-link-1"]
                              (= animal absent-animal) []
                              :else (get ids animal []))]
                   {:ok {:status :ok :truncated? false :edge-ids rows}}))
     :get-entity-fn (fn [_ id] {:ok (get entities id)})}))

(deftest collects-all-animal-records-items-priority-and-focus-link
  (let [run-dir (fixture-run [["lu-1" "sha"] ["hou-1" "sha"]
                              ["not-this-turn" "sha"]])
        record (chain/chain-record (fixture-config run-dir nil))]
    (is (every? #(= :present (get-in record [:gcc % :status]))
                [:驴 :馬 :狗 :象 :mentions :猴]))
    (is (= ["M-one"] (:items record)))
    (is (= ["hou-1" "lu-1"] (:focus-link record)))
    (is (= {:item-id "M-one" :status :ranked :rank 1 :pass-rank 12
            :priority-band :focus :cutoff-status :within-cutoff}
           (first (get-in record [:priority :items]))))
    (is (true? (get-in record [:turn :reply_text_present?])))
    (is (not-any? #(= "not exposed" %) (tree-seq coll? seq record)))))

(deftest absent-animal-is-typed
  (let [record (chain/chain-record
                (fixture-config (fixture-run []) :狗))]
    (is (= {:status :absent :reason :no-clusions}
           (get-in record [:gcc :狗])))))

(deftest focus-link-excludes-unpinned-clusion
  (let [record (chain/chain-record
                (fixture-config (fixture-run [["ma-1" "sha"]]) nil))]
    (is (= ["ma-1"] (:focus-link record)))
    (is (not (some #{"lu-1"} (:focus-link record))))))

(deftest canonical-record-sha-is-stable-under-map-order
  (let [a {:turn {:id "t" :origin "operator"} :items ["M-one"]}
        b (array-map :items ["M-one"] :turn (array-map :origin "operator" :id "t"))]
    (is (= (chain/canonical-edn a) (chain/canonical-edn b)))))

(deftest turn-item-missing-from-priority-is-typed-not-dropped
  (let [cfg (fixture-config (fixture-run []) nil)
        cfg (update cfg :get-entity-fn
                    (fn [f] (fn [g id] (if (= id "item-1")
                                         {:ok {:id "item-1" :asserted-content {:item/id "M-unranked"}}}
                                         (f g id)))))
        record (chain/chain-record cfg)]
    (is (= [{:item-id "M-unranked" :status :not-ranked}]
           (get-in record [:priority :items])))))
