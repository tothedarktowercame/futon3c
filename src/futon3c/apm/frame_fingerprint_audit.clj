(ns futon3c.apm.frame-fingerprint-audit
  "Run the artifact-fingerprint audit at the durable frame-close boundary."
  (:require [cheshire.core :as json]
            [clojure.java.shell :as shell])
  (:import [java.nio.file AtomicMoveNotSupportedException CopyOption Files Path
            StandardCopyOption]
           [java.time Instant]))

(def audit-script
  "holes/labs/M-apm-demonstration/analysis/fingerprint_audit.py")

(defn- atomic-write!
  [^Path path content]
  (Files/createDirectories (.getParent path)
                           (make-array java.nio.file.attribute.FileAttribute 0))
  (let [tmp (Files/createTempFile (.getParent path) ".fingerprint-audit-" ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (str tmp) content)
      (try
        (Files/move tmp path
                    (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                            StandardCopyOption/REPLACE_EXISTING]))
        (catch AtomicMoveNotSupportedException _
          (Files/move tmp path
                      (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
      (finally
        (Files/deleteIfExists tmp)))))

(defn- default-run-command
  [campaigns-root campaign]
  (apply shell/sh
         ["python3" audit-script "--campaign" campaign "--json"
          :env (assoc (into {} (System/getenv))
                      "APM_CAMPAIGNS" campaigns-root)]))

(defn- valid-row?
  [row]
  (and (map? row)
       (string? (:frame row))
       (integer? (:attempt row))
       (string? (:memory row))
       (string? (:verdict row))))

(defn- valid-audit?
  [audit campaign expected-frame-id expected-minimum-rows]
  (and (map? audit)
       (= campaign (:campaign audit))
       (map? (:summary audit))
       (vector? (:rows audit))
       (<= expected-minimum-rows
           (count (filter #(= expected-frame-id (:frame %)) (:rows audit))))
       (every? valid-row? (:rows audit))))

(defn audit!
  "Run the existing campaign fingerprint analyzer and atomically publish its
  JSON. A failed analyzer is made visible in a status EDN file but does not
  retract the already-durable frame close. `campaign-directory` is the parent
  containing the campaign's frame directories, never an individual frame
  directory. `expected-frame-id` and `expected-minimum-rows` bind successful
  output to the durable close receipt's recorded memory uses. `run-command-fn`
  is injectable for tests and receives `[campaigns-root campaign]`."
  [{:keys [campaign-directory expected-frame-id expected-minimum-rows
           run-command-fn now-fn]
    :or {run-command-fn default-run-command
         now-fn #(str (Instant/now))
         expected-minimum-rows 0}}]
  (let [campaign-path (Path/of (str campaign-directory) (make-array String 0))
        campaign (.toString (.getFileName campaign-path))
        campaigns-root (str (.getParent campaign-path))
        analysis-dir (.resolve campaign-path "analysis")
        artifact (.resolve analysis-dir "fingerprint-audit.json")
        status-path (.resolve analysis-dir "fingerprint-audit-status.edn")
        result (try
                 (run-command-fn campaigns-root campaign)
                 (catch Throwable t
                   {:exit -1 :out "" :err (str (.getName (class t)) ": "
                                                (.getMessage t))}))
        parsed (when (zero? (long (or (:exit result) -1)))
                 (try
                   (json/parse-string (:out result) true)
                   (catch Throwable _ nil)))
        ok? (valid-audit? parsed campaign expected-frame-id
                          expected-minimum-rows)
        status (cond-> {:ok ok?
                        :audit/at (now-fn)
                        :audit/campaign campaign
                        :audit/artifact-path (str artifact)}
                 ok?
                 (assoc :audit/use-events (get-in parsed [:summary :use-events])
                        :audit/rows (count (:rows parsed)))
                 (not ok?)
                 (assoc :error/code (if (zero? (long (or (:exit result) -1)))
                                      :fingerprint-audit-invalid-output
                                      :fingerprint-audit-command-failed)
                        :audit/expected-minimum-rows expected-minimum-rows
                        :audit/expected-frame-id expected-frame-id
                        :command/exit (:exit result)
                        :command/stderr (subs (str (:err result))
                                             0 (min 1000 (count (str (:err result)))))))]
    (when ok?
      (atomic-write! artifact (str (json/generate-string parsed {:pretty true}) "\n")))
    (atomic-write! status-path (str (pr-str status) "\n"))
    status))
