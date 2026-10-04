(ns futon3c.diagramprover.causal.open-theory-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.open-theory :as ot]
            [futon3c.diagramprover.causal.scm :as scm]))

(def upstream (ot/theory :up {"b" "a"} :inputs ["a"] :interface [:b]))
(def downstream (ot/theory :down {"c" "b"} :inputs ["b"] :interface [:b]))

(deftest theory-reads-arrows-off-mechanisms
  (is (= [{:from :a :to :b}] (:arrows upstream)))
  (is (= #{:a :b} (:variables upstream))))

(deftest gluing-along-an-interface-composes
  (let [{:keys [glued? dag owners inputs]} (ot/glue [upstream downstream])]
    (is glued?)
    (is (= {:b :up :c :down} owners))
    (is (= #{:a} inputs))
    (testing "the composite is an ordinary closed theory: D1 receipts apply"
      (is (dsep/d-connected? dag :a :c #{}))
      (is (dsep/d-separated? dag :a :c #{:b}))
      (is (= false (:answer (scm/counterfactual
                             dag {:evidence {:c true} :intervention {:b false}
                                  :outcome :c})))))))

(deftest sharing-outside-an-interface-is-refused
  (let [private (ot/theory :down {"c" "b"} :inputs ["b"])]
    (is (= {:glued? false :reason :undeclared-sharing :details {:b [:down]}}
           (ot/glue [upstream private])))))

(deftest two-owners-of-one-mechanism-are-refused
  (let [rival (ot/theory :rival {"b" "z"} :inputs ["z"] :interface [:b])]
    (is (= {:glued? false :reason :double-mechanism :details {:b [:up :rival]}}
           (ot/glue [upstream rival])))))

(deftest gluing-that-closes-a-loop-is-refused
  (let [back (ot/theory :back {"a" "b"} :inputs ["b"] :interface [:a :b])
        up (assoc upstream :interface #{:a :b})]
    (is (= :cycle (:reason (ot/glue [up back]))))))
