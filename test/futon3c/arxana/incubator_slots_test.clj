(ns futon3c.arxana.incubator-slots-test
  (:require [clojure.test :refer [deftest is]] [clojure.string :as str]
            [futon3c.arxana.incubator-slots :as s] [futon3c.arxana.draft-edition :as d]
            [futon3c.arxana.service :as service]))
(def form (str/join "\n" (map #(str "Invented prompt " %) (range 1 97))))
(def segments (mapv #(hash-map :index % :text "😀 same passage" :span {:start (* % 20) :end (+ 14 (* % 20))}
                             :sources (if (= % 1) [{:id "candidate-a"} {:id "candidate-b"}] [])) (range 1 52)))
(defn base [] (s/session (s/slots form (d/sha form)) segments))
(defn action [state kind & kvs]
  (s/act state (merge {:kind kind :subject "placement/1/team" :actor "test-agent"
                       :time "2026-11-18T00:00:00.000Z" :scope :placement} (apply hash-map kvs))))
(deftest captured-shape-and-pins
  (let [slots (s/slots form (d/sha form)) state (base)]
    (is (= 28 (count slots)))
    (is (= "Invented prompt 2" (:prompt (first slots))))
    (is (= ["Invented prompt 6" "Invented prompt 7" "Invented prompt 8"] (:options (nth slots 2))))
    ;; Joe 2026-11-18: free-text fields are 150 words; other kinds keep no limit.
    (is (every? #(= 150 (get-in % [:limit :words])) (filter #(= :prose (:kind %)) slots)))
    (is (every? #(= :unknown (get-in % [:limit :status])) (remove #(= :prose (:kind %)) slots)))
    (is (thrown? clojure.lang.ExceptionInfo (s/slots (str form "changed") (d/sha form))))
    (is (= (set (range 1 52)) (set (map :segment (:placements state)))))
    (let [[a b] (:placements state)] (is (= (:span a) (:span b))) (is (not= (:id a) (:id b))))
    (is (not= (:span (first segments)) (:span (second segments))))
    (is (= 2 (count (:sources (first (:placements state))))))))
(deftest scoped-acts-never-accept-evidence
  (let [state (base) accepted (action state :accept) moved (action accepted :move :destination "blockers")
        undone (action moved :undecide)]
    (is (= "test-agent" (:actor (first (s/current-placements accepted)))))
    (is (= :proposed (:evidence (s/reading accepted (first (filter #(= "team" (:id %)) (:slots state)))))))
    (is (= :projected (:benefit (s/reading (action state :accept :actor "joe") (first (:slots state))))))
    (is (= "blockers" (:slot (first (s/current-placements moved)))))
    (is (= "traction" (:slot (second (s/current-placements moved)))))
    (is (nil? (:decision (first (s/current-placements undone)))))
    (is (= (:segments state) (:segments moved)))
    (is (= 3 (count (:acts undone))))
    (is (thrown? clojure.lang.ExceptionInfo (action state :move :destination "missing")))
    (is (thrown? clojure.lang.ExceptionInfo (action state :challenge :reason "")))
    (is (thrown? clojure.lang.ExceptionInfo (action state :accept :scope :source)))
    (is (thrown? clojure.lang.ExceptionInfo (action state :accept :actor nil)))))
(deftest separate-requirements-and-argument
  (let [state (base) slot {:id "team" :limit {:status :known :words 1}}
        support {:id "edge/s" :kind :supports :from "claim/2" :to "claim/1" :warrant {:actor "joe" :reason "fixture"}}
        pending {:id "edge/a" :kind :attacks :from "claim/3" :to "claim/1"}
        supported (s/relation state support)
        cut (s/relation supported {:id "edge/c" :kind :undercuts :from "claim/3" :to "edge/s" :warrant {:actor "joe"}})]
    (is (= :over-limit (:limit-status (s/reading state slot))))
    (is (= :unknown (:limit-status (s/reading state (assoc slot :limit {:status :unknown})))) )
    (is (= :in (:label (s/argument-reading state "claim/1"))))
    (is (empty? (:supports (s/argument-reading state "claim/1"))))
    (is (= ["edge/s"] (:supports (s/argument-reading supported "claim/1"))))
    (is (= :in (:label (s/argument-reading (s/relation state pending) "claim/1"))))
    (is (= :out (:label (s/argument-reading (s/relation state (assoc pending :warrant {:actor "joe"})) "claim/1"))))
    (is (= ["edge/s"] (:undercut (s/argument-reading cut "claim/1"))))
    (is (empty? (:supports (s/argument-reading cut "claim/1"))))
    (is (= [[300000 300000 300000] [300000 600000 900000]] (mapv :totals (:readings s/budget-challenge))))
    (is (= :unresolved (:status s/budget-challenge)))
    (is (some #(str/includes? % "12-month") (:reasons (s/reading state {:id "success" :limit {:status :unknown}}))))))
(deftest read-operation-has-no-store
  (with-redefs [s/read-proposals (fn [_] (s/view (base)))]
    (let [request {:protocol/version "arxana.v1" :message/id "invented" :principal/id "test-agent"
                   :operation :incubator/proposals :body {}}
          result (service/dispatch {} request)]
      (is (= :ok (get-in result [:ok :status])))
      (is (= 28 (count (get-in result [:ok :slots]))))
      (is (:error (service/dispatch {} (assoc request :body {:unknown true}))))))
  (with-redefs [d/load-inputs (fn [_] (d/fail :truncated {}))]
    (is (= {:status :inconclusive :reason :truncated} (s/read-proposals {})))))
