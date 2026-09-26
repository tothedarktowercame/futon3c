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

(deftest repair-is-queued-to-exact-session-without-an-invoke
  (let [calls (atom [])
        http (fn [method url body]
               (swap! calls conj [method url body])
               (if (= method "GET")
                 {:http/status 200 :ok true :agent-id "repair" :agent {:session-id "busy-session"}}
                 {:http/status 200 :ok true :id "followup-1" :status "queued"}))
        policy {:repair-agent-id "repair" :queue-state-path "/fixture/queue.edn"}
        result (sut/dispatch! http "http://agency" policy {:hold/id "stable"})]
    (is (:ok result))
    (is (= "followup-1" (:dispatch/id result)))
    (is (= :busy-safe-followup (:delivery/type result)))
    (is (= ["GET" "POST"] (mapv first @calls)))
    (is (.endsWith (second (second @calls)) "/followups"))
    (is (= "busy-session" (get-in @calls [1 2 :session])))
    (is (= "apm-store-repair" (get-in @calls [1 2 :type])))
    (is (= ["store-repair-stable" "busy-session"] (get-in @calls [1 2 :dedupe-key])))))

(deftest no-session-or-failed-enqueue-is-not-a-delivered-repair
  (doseq [[response expected] [[{:http/status 200 :ok true :agent-id "repair" :agent {}}
                                :store-repair-session-unavailable]
                               [{:http/status 200 :ok true :agent-id "repair" :agent {:session-id "s"}}
                                :store-repair-followup-enqueue-failed]]]
    (is (= expected
           (:error/code (sut/dispatch! (fn [method _ _] (if (= "GET" method) response {:http/status 503}))
                                      "http://agency" {:repair-agent-id "repair" :queue-state-path "/queue"}
                                      {:hold/id "h"}))))))
