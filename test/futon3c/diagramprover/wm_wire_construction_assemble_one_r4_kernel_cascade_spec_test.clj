(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-cascade-spec-test
  "assemble/assemble-one's spec through the real ranker, following cascade-lane's
  :cascade-spec option. The reader records the input in its own metadata beside
  its derived scoring spec; no projection is used to manufacture equality.

  assemble/assemble-one's spec through the real ranker, following cascade-lane's
  :cascade-spec option. Values come from the content-addressed producer record;
  no product code is loaded here."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def stem "wm-wire-construction-assemble-one-r4-kernel-cascade-spec-tes")
(def wire-id [:construction-assemble-one :r4-kernel :cascade-spec])
(def producer (delay (producer-record/record stem)))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation]
  (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))

(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-cascade-spec-test/changed-carrier-changes-the-derived-product :kind :value-varying
                  :product [:scores] :intervention :before-reader}
   :wire [:construction-assemble-one :r4-kernel :cascade-spec]
   :kind :witnessed-hermetically :test `the-observed-handoff :check check
   :live-records-read []
   :note "Real assemble -> assemble-one over cascade-decision-test's tick-1 sources (construction-support/assembled), then rank-cascade-actions with the same :cascade-spec option cascade-lane forwards. Reader end: output metadata [:cascade-scoring :spec-in], retained before transformation; :spec is a different derived record. The values are read from the producer record."})

(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (str "writer-reader " (pr-str o)))
    (is (:ranked-present? o))
    (is (:reader-differs-from-derived? o))))

(deftest bad-carriers-before-the-real-reader
  (doseq [mutation [:absent :different :missing-want]]
    (let [o (observe mutation)]
      (is (not (w/received? o)) (name mutation))
      (case mutation
        :absent (do (is (= {:absent :no-cascade-spec} (:reader o)))
                    (is (= :missing-cascade-want (:ranked-kind o))))
        :missing-want (do (is (not (contains? (:reader o) :want)))
                          (is (= :missing-cascade-want (:ranked-kind o))))
        :different (do (is (= #{:test-covers-missing-total-repos} (get-in o [:reader :want])))
                       (is (:ranked-present? o)))))))

(deftest live-records-do-not-carry-this-wire
  (let [live (:live-records @producer)]
    (is (= 3 (:count live)))
    (doseq [{:keys [sha256 sha-matches? no-cascade-spec?]} (:pins live)]
      (is sha-matches? (str "sha256 " sha256))
      (is no-cascade-spec? (str "no :cascade-spec in " sha256)))))

(deftest changed-carrier-changes-the-derived-product
  (let [r (:second-layer (fields))]
    (doseq [k [:competing-before? :scores-numeric? :scores-differ?]]
      (testing (name k) (is (true? (get r k)))))
    (is (= [3 3] (:candidate-counts r)))
    (is (not= (get-in r [:before :scores]) (get-in r [:after :scores])))))
