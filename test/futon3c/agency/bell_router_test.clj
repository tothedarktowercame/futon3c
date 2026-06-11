(ns futon3c.agency.bell-router-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.bell-router :as br]))

(defn- bell [id from to & [state]]
  {:job-id id :caller from :agent-id to :surface "bell" :state (or state "running")})
(defn- typed-bell [id from to type & [ref]]
  (cond-> (bell id from to)
    type (assoc :bell-type type)
    ref (assoc :ref ref)))
(defn- reply [id to bellback-of]
  {:job-id id :caller "auto-bellback" :agent-id to :surface "auto-bellback"
   :bellback-of bellback-of :state "done"})

(deftest open-and-answered
  (let [g (br/graph [(bell "J1" "A" "B")
                     (bell "J2" "A" "C")
                     (reply "R2" "A" "J2")])]            ;; J2 has a reply
    (is (= #{"J1"} (set (map :id (:open g)))) "J1 open, J2 answered")
    (is (= #{"J2"} (set (map :id (:answered g)))))
    (is (= "R2" (:reply-id (first (:answered g)))))))

(deftest crossing-detected
  (let [g (br/graph [(bell "J1" "A" "B")                 ;; A asks B
                     (bell "J2" "B" "A")])]              ;; B asks A — both open
    (is (= [{:a "A" :b "B"}] (:crossings g)) "A<->B mutual open bells = one crossing")))

(deftest no-crossing-once-one-answered
  (let [g (br/graph [(bell "J1" "A" "B")
                     (bell "J2" "B" "A")
                     (reply "R1" "B" "J1")])]            ;; A->B answered
    (is (empty? (:crossings g)) "no crossing once one direction is answered")))

(deftest by-agent-awaiting-and-owes
  (let [g (br/graph [(bell "J1" "A" "B")])
        a (get (:by-agent g) "A")
        b (get (:by-agent g) "B")]
    (is (= [{:id "J1" :to "B"}] (:awaiting a)) "A awaits a reply from B")
    (is (empty? (:owes a)))
    (is (= [{:id "J1" :from "A"}] (:owes b)) "B owes A a reply")
    (is (empty? (:awaiting b)))))

(deftest ignores-non-bell-and-malformed
  (let [g (br/graph [{:job-id "W1" :caller "A" :agent-id "B" :surface "whistle"}
                     {:job-id "X" :surface "bell"}       ;; no from/to
                     (bell "J1" "A" "B")])]
    (is (= #{"J1"} (set (map :id (:open g)))) "whistles + malformed bells excluded")))

(deftest typed-answer-ref-resolves-instead-of-crossing
  (let [g (br/graph [(typed-bell "J1" "A" "B" :query)
                     (typed-bell "J2" "B" "A" :answer "J1")])]
    (is (empty? (:crossings g)) "typed answer is not a second open direction")
    (is (= #{"J1"} (set (map :id (:answered g)))))
    (is (= "J2" (:reply-id (first (:answered g)))))))

(deftest mutual-typed-queries-still-cross
  (let [g (br/graph [(typed-bell "J1" "A" "B" :query)
                     (typed-bell "J2" "B" "A" :query)])]
    (is (= [{:a "A" :b "B"}] (:crossings g)))))
