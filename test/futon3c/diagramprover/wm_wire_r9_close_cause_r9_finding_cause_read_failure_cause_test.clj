(ns futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-cause-read-failure-cause-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-close-cause :r9-finding-cause-read :failure-cause])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:second-layer {:test `changed-cause-is-preserved :kind :record :product [:read] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-close-cause-reaches-finding-failure-cause
           :check check :live-records-read []})
(deftest the-close-cause-reaches-finding-failure-cause
  (let [o (check)] (is (= {:cause [{:class "java.lang.RuntimeException" :message "beneath"}]} (:writer o)))
       (is (w/received? o) (str "writer-reader " (pr-str o)))))
(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (:typed-absence (fields))] (is (= {:absent :no-cause} (:reader o))) (is (not (w/received? o)))))
(deftest a-different-cause-fails-the-wire
  (let [o (:different (fields))] (is (some? (:reader o))) (is (not= (:writer o) (:reader o)))
       (is (not (w/received? o)))))
(deftest the-live-finding-reads-typed-absent
  (let [x (get-in @producer [:live-controls :finding])]
    (is (:sha-ok? x)) (is (:cause-key-absent? x)) (is (= {:absent :cause-not-on-record} (:cause-read x)))))
(deftest changed-cause-is-preserved
  (doseq [[k v] (:second-layer (fields))]
    (testing (name k) (if (= k :missing) (is (= {:absent :cause-not-on-record} v))
                          (is (true? v) (str k " relation failed"))))))
