(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-cascade-spec-test
  "assemble/assemble-one's spec through the real ranker, following cascade-lane's
  :cascade-spec option. The reader records the input in its own metadata beside
  its derived scoring spec; no projection is used to manufacture equality."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.cascade-model-manifest :as manifest]
            [futon2.aif.cascade-policy :as policy]
            [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))

(def live-records-read
  (mapv #(assoc % :why "No :cascade-spec in the spike record: neither the assembled spec nor the ranker's received-spec receipt is recorded.")
        (filter #(re-find #"tick-run-record.*(278b6988|7f89646a|e70b4baf)" (:path %))
                support/live-records-read)))

(defn observe [mutation]
  (let [p (get-in @support/assembled [:problems 0 :cascade-problem])
        pair (get-in @support/assembled [:problems 0 :constructed-candidates 0])
        candidate {:kind :cascade-candidate :id (:candidate-id pair)
                   :precedence (mapv #(policy/token-interpretation % (get-in p [:interpretations %]))
                                     (:precedence pair))
                   :construction-receipt (:construction-receipt pair)}
        writer (:cascade-spec p)
        carrier (case mutation
                  :none writer
                  :absent {:absent :no-cascade-spec}
                  :different (assoc writer :want #{:test-covers-missing-total-repos})
                  :missing-want (dissoc writer :want))
        ranked (efe/rank-cascade-actions
                {:cascade-belief (manifest/observed-belief
                                  (set (for [[k v] (:facts p) :when (true? v)] k)))}
                [candidate] {:cascade-spec carrier :horizon-steps (:horizon-steps p)
                             :f-prefix-production? true})]
    {:writer writer :reader (get-in (meta ranked) [:cascade-scoring :spec-in])
     :ranked ranked :derived (get-in (meta ranked) [:cascade-scoring :spec])}))

(defn check [] (observe :none))

(def wire
  {:wire [:construction-assemble-one :r4-kernel :cascade-spec]
   :kind :witnessed-hermetically :test `the-observed-handoff :check check
   :live-records-read live-records-read
   :note "Real assemble -> assemble-one over cascade-decision-test's tick-1 sources (construction-support/assembled), then rank-cascade-actions with the same :cascade-spec option cascade-lane forwards. Reader end: output metadata [:cascade-scoring :spec-in], retained before transformation; :spec is a different derived record."})

(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (= (pr-str (:writer o)) (pr-str (:reader o))))
    (is (seq (:ranked o)))
    (is (not= (:reader o) (:derived o)))))

(deftest bad-carriers-before-the-real-reader
  (doseq [mutation [:absent :different :missing-want]]
    (let [o (observe mutation)]
      (is (not (w/received? o)) (name mutation))
      (case mutation
        :absent (do (is (= {:absent :no-cascade-spec} (:reader o)))
                    (is (= :missing-cascade-want (get-in o [:ranked :kind]))))
        :missing-want (do (is (not (contains? (:reader o) :want)))
                          (is (= :missing-cascade-want (get-in o [:ranked :kind]))))
        :different (do (is (= #{:test-covers-missing-total-repos} (get-in o [:reader :want])))
                       (is (seq (:ranked o))))))))

(deftest live-records-do-not-carry-this-wire
  (is (= 3 (count live-records-read)))
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)))
    (when (= sha256 (w/sha256-file path))
      (is (not-any? #(and (map? %) (contains? % :cascade-spec))
                    (tree-seq coll? seq (w/read-record path)))))))
