(ns futon3c.peripheral.war-machine-pilot-guardrails-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.war-machine-pilot :as pilot])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-file []
  (let [dir (Files/createTempDirectory "wm-pilot-guardrails-test"
                                       (into-array FileAttribute []))]
    (.getAbsolutePath (io/file (str dir) "needs-you.edn"))))

(defn- delete-tree! [path]
  (let [root (io/file path)]
    (when (.exists root)
      (doseq [^File f (reverse (file-seq root))]
        (.delete f)))))

(defn- judgement [ranked-actions]
  {:mode :base-case
   :ranked-actions ranked-actions})

(defn- ranked [rank type target g]
  {:rank rank
   :G-total g
   :action {:type type :target target}})

(defn- scheduler-resolve [sym]
  (case sym
    futon3c.wm.scheduler/status (fn [] {:tick-count 17})
    futon3c.wm.scheduler/request-tick! (fn [] {:queued? true})
    (throw (ex-info "unexpected requiring-resolve" {:sym sym}))))

(deftest default-begin-live-cycle-keeps-top-action-test
  (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                (fn []
                  (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                              (ranked 2 :address-sorry "sorry/foo" -1.0)]))
                clojure.core/requiring-resolve scheduler-resolve]
    (let [result (pilot/begin-live-cycle! {:tick? false})]
      (is (= true (:ok result)))
      (is (= {:type :learn-action-class :target :open-mission} (:v result)))
      (is (not (contains? result :needs-you-emitted)))
      (is (not (contains? result :needs-you-path))))))

(deftest guardrails-mode-steps-to-first-autonomous-action-test
  (let [path (temp-file)]
    (try
      (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                    (fn []
                      (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                                  (ranked 2 :address-sorry "sorry/foo" -1.0)]))
                    clojure.core/requiring-resolve scheduler-resolve]
        (let [result (pilot/begin-live-cycle! {:guardrails? true
                                               :tick? false
                                               :needs-you-path path})]
          (is (= true (:ok result)))
          (is (= {:type :address-sorry :target "sorry/foo"} (:v result)))
          (is (= 1 (:needs-you-emitted result)))
          (let [items (edn/read-string (slurp path))]
            (is (= 1 (count items)))
            (is (= :learn-action-class (:wm-action-class (first items))))
            (is (:unblock-action (first items))))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))

(deftest guardrails-mode-soft-stops-when-no-autonomous-action-test
  (let [path (temp-file)]
    (try
      (with-redefs [futon3c.peripheral.war-machine-pilot/live-judgement
                    (fn []
                      (judgement [(ranked 1 :learn-action-class :open-mission -9.0)
                                  (ranked 2 :open-mission "M-net-new" -8.0)]))]
        (let [result (pilot/begin-live-cycle! {:guardrails? true
                                               :tick? false
                                               :needs-you-path path
                                               :guardrails-ctx
                                               {:mission-status-fn
                                                (fn [_] {:open? false :open-hole-count 1})}})]
          (is (= false (:ok result)))
          (is (= :no-autonomous-action (:reason result)))
          (is (= 2 (:needs-you-emitted result)))
          (let [items (edn/read-string (slurp path))]
            (is (= 2 (count items)))
            (is (every? :unblock-action items)))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))
