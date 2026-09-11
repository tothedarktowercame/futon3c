(ns futon3c.apm.store-read-hold-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [futon3c.apm.store-read-hold :as sut]
            [futon3c.substrate.read-health :as health])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest warning-survives-a-new-reader-and-corruption-is-not-empty
  (let [directory (str (Files/createTempDirectory "store-warning-test" (make-array FileAttribute 0)))
        frame {:frame/id "f1" :problem/id "p1"}
        warning (merge frame {:trace-id "request1" :elapsed-ms 5600})]
    (try
      (sut/record-warning! directory warning)
      (is (= warning (dissoc (first (sut/warnings directory frame)) :warning/id)))
      (is (thrown? Exception (sut/warnings directory (assoc frame :frame/id "f2"))))
      (spit (first (.listFiles (sut/warnings-directory directory))) "{:broken")
      (is (thrown? Exception (sut/warnings directory frame)))
      (finally (doseq [file (reverse (file-seq (io/file directory)))] (.delete file))))))

(deftest context-is-scoped-and-propagates-to-frame-futures
  (sut/with-frame! {:policy/version 1 :repair-agent-id "repair"} {:frame/id "f1"} "/unused"
    #(is (= "f1" @(future (:frame/id health/*context*)))))
  (is (nil? health/*context*)))

(deftest dispatch-is-activated-but-terminal-replay-is-not
  (doseq [state ["queued" "running" "done" "failed"]]
    (let [calls (atom []) hold {:hold/id "stable"}
          http (fn [method url body]
                 (swap! calls conj [method url body])
                 (cond
                   (.endsWith url "/announce")
                   {:http/status 202 :ok true :accepted true :job-id "store-repair-stable"}
                   (= method "GET")
                   {:http/status 200 :job {:job-id "store-repair-stable"
                                          :agent-id "repair" :state state}}
                   (.endsWith url "/activate")
                   {:http/status 202 :ok true :accepted true}))
          result (sut/dispatch! http "http://agency" {:repair-agent-id "repair"} hold)]
      (is (:ok result))
      (is (= "store-repair-stable" (:dispatch/id result)))
      (is (= (if (= state "queued") 1 0)
             (count (filter #(.endsWith (second %) "/activate") @calls)))))))
