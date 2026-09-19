(ns futon3c.test-registry.validation-adapters-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry.validation-adapters :as adapters])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(def live-trip-root "/home/joe/code/futon2/data/wm-tripwires/trips")

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "validation-adapters-" (make-array FileAttribute 0))))

(defn- live-agency-response []
  (let [request (-> (HttpRequest/newBuilder (URI/create "http://127.0.0.1:7070/api/alpha/invoke/jobs?limit=200"))
                    (.GET) (.build))
        response (.send (HttpClient/newHttpClient) request (HttpResponse$BodyHandlers/ofString))]
    (is (= 200 (.statusCode response)) "read-only live Agency endpoint is available")
    (json/parse-string (.body response) true)))

(deftest live-pinned-readers-and-idempotent-sweeps
  (let [dir (temp-dir) trip-dir (io/file dir "trips") _ (.mkdirs trip-dir)
        live-trip (apply min-key #(.length ^java.io.File %)
                         (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn"))
                                 (file-seq (io/file live-trip-root))))
        copied-trip (io/file trip-dir (.getName live-trip))
        _ (Files/copy (.toPath live-trip) (.toPath copied-trip)
                      (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
        queue (str (io/file dir "queue.ednlog"))
        cursor (str (io/file dir "cursor.ednlog"))
        response (live-agency-response)
        failed (adapters/agency-incidents response)
        trips (adapters/wm-trip-incidents (.getCanonicalPath trip-dir))
        opts {:queue-file queue :cursor-file cursor :backend (atom {:entries {} :order []})}]
    (try
      (is (seq failed) "live API includes terminal failure evidence")
      (is (every? #(re-find #"^agency/" (:subject-id %)) failed))
      (is (seq trips) "live trip ledger includes pinned reports")
      (is (every? #(re-find #"^wm/tripwire/" (:subject-id %)) trips))
      (let [first-agency (adapters/sweep-incidents! opts :agency failed)
            second-agency (adapters/sweep-incidents! opts :agency failed)
            one-trip [(first trips)]
            first-trip (adapters/sweep-incidents! opts :wm-trips one-trip)
            second-trip (adapters/sweep-incidents! opts :wm-trips one-trip)]
        (is (pos? (:enqueued first-agency)))
        (is (= {:enqueued 0 :skipped (count failed)} second-agency))
        (is (= {:enqueued 1 :skipped 0} first-trip))
        (is (= {:enqueued 0 :skipped 1} second-trip))
        (is (= (+ (count failed) 1)
               (count (remove str/blank? (str/split-lines (slurp queue)))))))
      (finally (doseq [f (reverse (file-seq dir))] (io/delete-file f))))))

(deftest explicit-subject-overrides-are-honored
  (let [response {:ok true :jobs [{:job-id "job-x" :agent-id "agent-x" :state "failed"
                                   :finished-at "2026-09-19T00:00:00Z"
                                   :terminal-message "failed"}]}
        incident (first (adapters/agency-incidents response {"job-x" "component/x"}))]
    (is (= "component/x" (:subject-id incident)))))
