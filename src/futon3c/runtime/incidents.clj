(ns futon3c.runtime.incidents
  "Durable, append-only records for uncaught JVM failures."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.lang ProcessHandle Thread$UncaughtExceptionHandler]
           [java.lang.management ManagementFactory]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardOpenOption]
           [java.time Instant]
           [java.util UUID]))

(def default-root "/home/joe/code/futon3c/data/jvm-incidents")

(def ^:private rate-window-ms 60000)
(def ^:private max-rate-keys 256)
(defonce ^:private !last-written (atom {}))
(defonce ^:private !installed-handler (atom nil))

(defn- truncate
  [s max-chars]
  (when (some? s)
    (let [s (str s)]
      (if (> (.length s) max-chars)
        (subs s 0 max-chars)
        s))))

(defn- bounded-pr-str
  [value]
  (when (some? value)
    (truncate
     (binding [*print-length* 100
               *print-level* 8]
       (pr-str value))
     2000)))

(defn- heap-snapshot
  []
  (let [runtime (Runtime/getRuntime)
        mb (* 1024 1024)]
    {:used-mb (quot (- (.totalMemory runtime) (.freeMemory runtime)) mb)
     :max-mb (quot (.maxMemory runtime) mb)}))

(defn- runtime-snapshot
  []
  {:pid (.pid (ProcessHandle/current))
   :uptime-ms (.getUptime (ManagementFactory/getRuntimeMXBean))})

(defn health
  "Return an allocation-light JVM health snapshot for the HTTP surface."
  []
  {:heap (heap-snapshot)
   :threads (.getThreadCount (ManagementFactory/getThreadMXBean))
   :uptime-ms (.getUptime (ManagementFactory/getRuntimeMXBean))
   :incident-count (count (or (.listFiles (io/file default-root)
                                          (reify java.io.FilenameFilter
                                            (accept [_ _ name]
                                              (str/ends-with? name ".edn"))))
                             (make-array java.io.File 0)))})

(defn- write-new!
  [path value]
  (let [file (io/file path)
        bytes (.getBytes (str (pr-str value) "\n") StandardCharsets/UTF_8)]
    (io/make-parents file)
    (Files/write (.toPath file)
                 bytes
                 (into-array StandardOpenOption
                             [StandardOpenOption/CREATE_NEW
                              StandardOpenOption/WRITE]))
    value))

(defn- reserve-rate-slot!
  [rate-key now-ms]
  (let [allowed? (volatile! false)]
    (swap! !last-written
           (fn [timestamps]
             (vreset! allowed? false)
             (let [fresh (into {}
                               (filter (fn [[_ written-ms]]
                                         (< (- now-ms written-ms)
                                            rate-window-ms)))
                               timestamps)]
               (if (contains? fresh rate-key)
                 fresh
                 (do
                   (vreset! allowed? true)
                   (let [updated (assoc fresh rate-key now-ms)]
                     (if (<= (count updated) max-rate-keys)
                       updated
                       (into {}
                             (take max-rate-keys)
                             (sort-by val > updated)))))))))
    @allowed?))

(defn- release-rate-slot!
  [rate-key reserved-ms]
  (swap! !last-written
         (fn [timestamps]
           (if (= reserved-ms (get timestamps rate-key))
             (dissoc timestamps rate-key)
             timestamps))))

(defn- incident-record
  [^Throwable throwable thread-name]
  (let [runtime (runtime-snapshot)]
    {:jvm-incident/id (str "jvi-" (UUID/randomUUID))
     :at (str (Instant/now))
     :thread (or thread-name "unknown")
     :ex-class (.getName (class throwable))
     :message (.getMessage throwable)
     :ex-data (bounded-pr-str (ex-data throwable))
     :stack (->> (.getStackTrace throwable)
                 (take 30)
                 (mapv str))
     :heap (heap-snapshot)
     :runtime runtime
     :jvm-incident/schema-version 1}))

(defn record-incident!
  "Record an uncaught throwable without ever propagating an internal failure.
   Identical exception class/message pairs are recorded at most once per minute."
  [throwable thread-name]
  (try
    (let [ex-class (.getName (class throwable))
          message (.getMessage ^Throwable throwable)
          rate-key [ex-class message]
          now-ms (System/currentTimeMillis)]
      (when (reserve-rate-slot! rate-key now-ms)
        (try
          (let [record (incident-record throwable thread-name)]
            (write-new! (io/file default-root
                                 (str (:jvm-incident/id record) ".edn"))
                        record))
          (catch Throwable t
            (release-rate-slot! rate-key now-ms)
            (throw t)))))
    (catch Throwable _
      (try
        (.println System/err "[jvm-incidents] failed to record uncaught exception")
        (catch Throwable _ nil))
      nil)))

(defn- report-uncaught!
  [^Thread thread ^Throwable throwable previous]
  (if previous
    (.uncaughtException ^Thread$UncaughtExceptionHandler previous
                        thread throwable)
    (do
      (.print System/err
              (str "Exception in thread \"" (.getName thread) "\" "))
      (.printStackTrace throwable System/err))))

(defn install-default-handler!
  "Install the record-and-report default uncaught exception handler.
   Safe to invoke repeatedly, including after a Drawbridge namespace load."
  []
  (let [handler
        (or @!installed-handler
            (let [previous (Thread/getDefaultUncaughtExceptionHandler)
                  candidate
                  (reify Thread$UncaughtExceptionHandler
                    (uncaughtException [_ thread throwable]
                      (record-incident! throwable (.getName ^Thread thread))
                      (report-uncaught! thread throwable previous)))]
              (if (compare-and-set! !installed-handler nil candidate)
                candidate
                @!installed-handler)))]
    (Thread/setDefaultUncaughtExceptionHandler handler)
    handler))

(defn- edn-files
  []
  (->> (or (.listFiles (io/file default-root)) [])
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".edn"))))

(defn incidents
  "Read incident records newest-first, optionally capped to LIMIT."
  ([]
   (->> (edn-files)
        (map #(edn/read-string (slurp %)))
        (sort-by :at #(compare %2 %1))
        vec))
  ([limit]
   (->> (incidents)
        (take (max 0 (long limit)))
        vec)))
