(ns futon3c.agency.atomic-file
  "Forced same-directory replacement and visible quarantine for Agency state."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path StandardCopyOption StandardOpenOption CopyOption OpenOption]))

(def ^:dynamic *before-move*
  "Test hook called after the temporary file is forced and before replacement."
  nil)

(defonce ^:private !corruptions
  (atom {:total 0 :by-store {} :last nil}))

(defn stats [] @!corruptions)

(defn write!
  "Force CONTENT to a sibling temporary file, then atomically replace PATH."
  [path content]
  (let [target (.toAbsolutePath (.toPath (io/file path)))
        parent (.getParent target)
        _ (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))
        tmp (Files/createTempFile parent (str "." (.getFileName target) "-") ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))
        bytes (.getBytes (str content) StandardCharsets/UTF_8)]
    (try
      (with-open [channel (FileChannel/open tmp
                                            (into-array OpenOption
                                                        [StandardOpenOption/WRITE
                                                         StandardOpenOption/TRUNCATE_EXISTING]))]
        (let [buffer (ByteBuffer/wrap bytes)]
          (while (.hasRemaining buffer) (.write channel buffer)))
        (.force channel true))
      (when *before-move* (*before-move* {:target target :temp tmp}))
      (Files/move tmp target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      path
      (finally (Files/deleteIfExists tmp)))))

(defn- quarantine! [store ^java.io.File file error]
  (let [at-ms (System/currentTimeMillis)
        source (.toAbsolutePath (.toPath file))
        target (Path/of (str source ".corrupt-" at-ms) (make-array String 0))
        event {:store store :path (str source) :preserved-as (str target)
               :at-ms at-ms :message (.getMessage error)}]
    (Files/move source target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (swap! !corruptions
           (fn [state]
             (-> state
                 (update :total inc)
                 (update-in [:by-store store] (fnil inc 0))
                 (assoc :last event))))
    (binding [*out* *err*]
      (println (str "[agency-state] CORRUPT " store " file preserved: "
                    source " -> " target ": " (.getMessage error))))
    event))

(defn load-edn-map!
  "Read PATH as one EDN map. Missing is EMPTY; malformed is quarantined and EMPTY."
  [store path empty]
  (let [file (io/file path)]
    (if-not (.exists file)
      empty
      (try
        (let [value (edn/read-string (slurp file))]
          (when-not (map? value)
            (throw (ex-info "Agency state root is not a map"
                            {:store store :path (.getAbsolutePath file)})))
          value)
        (catch Throwable error
          (quarantine! store file error)
          empty)))))
