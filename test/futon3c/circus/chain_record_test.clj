(ns futon3c.circus.chain-record-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.circus.chain-record :as chain]
            [futon3c.giraffe.run :as giraffe]))

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

(defn- mapped-parts []
  (into {} (map (fn [part]
                  [part {:element "fixture element" :turn "turn-fixture"
                         :quote "five exact fixture words are here"
                         :who "operator"}]))
        [:I1 :I2 :G :B]))

(defn- ma-turn [failure-kind]
  {:origin "operator" :reply_text "fixture reply"
   :proposed_patterns [{:patterns ["p-one" "p-two"]}]
   :pattern_verdicts
   {:connection {:status :linked :turn/id "turn-before" :sha256 "fixture-sha"}
    :model_outcome
    {:batches [{:batch 0 :patterns ["p-one" "p-two"]
                :failure_kind failure-kind}]}
    :patterns (mapv (fn [pattern]
                      {:pattern pattern :turn (mapped-parts) :chain (mapped-parts)})
                    ["p-one" "p-two"])}})

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

(deftest light-turn-gcc-never-reads-or-verifies-a-giraffe-run
  (let [cfg (fixture-config "/path/that/must/not/be-read" :狗)
        light (chain/turn-gcc (dissoc cfg :run-dir :verify-run-fn))]
    (is (= "turn-fixture" (get-in light [:turn :id])))
    (is (= {:status :absent :reason :no-clusions} (get-in light [:gcc :狗])))
    (is (= ["M-one"] (:items light)))
    (is (= #{:mention :focus} (set (map :kind (:item-links light)))))))

(deftest focus-link-excludes-unpinned-clusion
  (let [record (chain/chain-record
                (fixture-config (fixture-run [["ma-1" "sha"]]) nil))]
    (is (= ["ma-1"] (:focus-link record)))
    (is (not (some #{"lu-1"} (:focus-link record))))))

(deftest canonical-record-sha-is-stable-under-map-order
  (let [a {:turn {:id "t" :origin "operator"} :items ["M-one"]}
        b (array-map :items ["M-one"] :turn (array-map :origin "operator" :id "t"))]
    (is (= (chain/canonical-edn a) (chain/canonical-edn b)))))

(deftest priority-pin-migration-does-not-change-chain-record
  (let [dir (fixture-run [["hou-1" "sha"]])
        manifest (read-string (slurp (io/file dir "manifest.edn")))
        pin (get-in manifest [:focus-relation-input :evidence-pin])
        priority-file (io/file dir "priority.edn")
        rows (mapv #(assoc % :evidence-pin pin) (read-string (slurp priority-file)))
        _ (spit priority-file (pr-str rows))
        before (chain/chain-record (fixture-config dir nil))]
    (is (= :migrated (:status (giraffe/migrate-priority-run!
                               dir {:write? true :at "2026-11-18T00:00:00Z"}))))
    (is (= before (chain/chain-record (fixture-config dir nil))))))

(deftest turn-item-missing-from-priority-is-typed-not-dropped
  (let [cfg (fixture-config (fixture-run []) nil)
        cfg (update cfg :get-entity-fn
                    (fn [f] (fn [g id] (if (= id "item-1")
                                         {:ok {:id "item-1" :asserted-content {:item/id "M-unranked"}}}
                                         (f g id)))))
        record (chain/chain-record cfg)]
    (is (= [{:item-id "M-unranked" :status :not-ranked}]
           (get-in record [:priority :items])))))

(deftest ma-reading-completes-every-pattern-only-when-its-batch-and-maps-complete
  (let [reading (chain/ma-reading (ma-turn nil))]
    (is (= ["p-one" "p-two"] (:proposed-pattern-ids reading)))
    (is (every? #(= :completed (:status %)) (:patterns reading)))
    (is (= {:status :linked :turn/id "turn-before" :sha256 "fixture-sha"}
           (:connection reading)))
    (is (true? (:obligation-met? reading)))))

(deftest ma-reading-failed-batch-does-not-complete-its-patterns
  (let [reading (chain/ma-reading (ma-turn "model/truncated-reply"))]
    (is (= ["model/truncated-reply"] (:batch-failure-kinds reading)))
    (is (nil? (:legitimate-absence-reason reading)))
    (is (every? #(= :not-completed (:status %)) (:patterns reading)))
    (is (false? (:obligation-met? reading)))))

(deftest ma-reading-no-reply-is-a-legitimate-absence
  (let [reading (chain/ma-reading (dissoc (ma-turn nil) :reply_text))]
    (is (= :no-reply-text (:legitimate-absence-reason reading)))
    (is (true? (:obligation-met? reading)))))

(deftest ma-reading-legacy-outcome-is-a-failure-not-an-absence
  (let [turn (-> (ma-turn nil) (assoc :pattern_verdicts
                                      {:patterns (get-in (ma-turn nil)
                                                         [:pattern_verdicts :patterns])}))
        reading (chain/ma-reading turn)]
    (is (nil? (:legitimate-absence-reason reading)))
    (is (= ["legacy-unknown" "legacy-unknown"]
           (mapv :batch/failure-kind (:patterns reading))))
    (is (false? (:obligation-met? reading)))))

(deftest item-absence-is-typed-only-without-item-evidence
  ;; CL-62: turn-Y9sLUY's 猴 header had :hou/relations [] and no mention edges.
  (is (= :no-item-evidence (chain/item-absence [] [] [])))
  (testing "mention or focus-relation edges that resolve to no item are not absence"
    (is (nil? (chain/item-absence [] ["mention-1"] [])))
    (is (nil? (chain/item-absence [] [] ["focus-relation-1"]))))
  (is (nil? (chain/item-absence ["M-one"] ["mention-1"] []))))

(deftest light-turn-carries-common-ground-source-fields-from-one-read
  (let [reads (atom 0)
        cfg (assoc (fixture-config "/not-read" nil)
                   :read-turn-fn (fn [_ _]
                                   (swap! reads inc)
                                   {:mission_id "M-invented"
                                    :lu_ranked {:ranked [{:id "p-invented" :rank 1}]}
                                    :pattern_verdicts {:patterns [{:pattern "p-invented"
                                                                  :status :occurrence}]}}))
        light (chain/turn-gcc cfg)]
    (is (= 1 @reads))
    (is (= "M-invented" (get-in light [:turn :mission_id])))
    (is (= [{:id "p-invented" :rank 1}] (get-in light [:lu-ranked :ranked])))
    (is (= :occurrence (get-in light [:pattern-verdicts :patterns 0 :status])))))
