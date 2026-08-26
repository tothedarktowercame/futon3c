(ns futon3c.agency.inbox
  "Atomic filesystem delivery for pull-only Agency seats."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.time Instant]))

(defn inbox-root
  []
  (io/file (or (System/getenv "FUTON3C_AGENCY_INBOX_DIR")
               (str (System/getProperty "user.home") "/.claude/agency-inbox"))))

(defn- safe-path-segment
  [label value]
  (let [value (str value)]
    (when (or (str/blank? value)
              (str/includes? value "/")
              (str/includes? value ".."))
      (throw (ex-info (str "Unsafe inbox " label) {label value})))
    value))

(defn deliver-to-inbox!
  [{:keys [job-id agent-id caller prompt bell-type ref bellback-of created-at]}]
  (let [agent-id (safe-path-segment :agent-id agent-id)
        job-id (safe-path-segment :job-id job-id)
        directory (io/file (inbox-root) agent-id)
        final-file (io/file directory (str job-id ".json"))
        temp-file (io/file directory (str job-id ".json.tmp"))
        payload {:job-id job-id
                 :agent-id agent-id
                 :from (str caller)
                 :surface "bell"
                 :bell-type (some-> bell-type name)
                 :ref (some-> ref str)
                 :in-reply-to (some-> bellback-of str)
                 :prompt (str prompt)
                 :created-at (str (or created-at (Instant/now)))
                 :ack-url (str "/api/alpha/invoke/jobs/" job-id "/ack")}]
    (Files/createDirectories (.toPath directory)
                             (make-array java.nio.file.attribute.FileAttribute 0))
    (Files/write (.toPath temp-file)
                 (.getBytes (json/generate-string payload) StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))
    (Files/move (.toPath temp-file)
                (.toPath final-file)
                (into-array java.nio.file.CopyOption
                            [StandardCopyOption/ATOMIC_MOVE
                             StandardCopyOption/REPLACE_EXISTING]))
    (.getAbsolutePath final-file)))
