(ns futon3c.apm.campaign-ledger
  "Durable compare-and-append boundary for campaign-machine events.

  One EDN form is appended per line.  Writers take an OS file lock, validate
  the complete existing history, compare its version and digest, validate the
  proposed successor in memory, append exactly one record, and force it to
  stable storage before returning a receipt.  Malformed or invalid history is
  a stop condition; this namespace never truncates or repairs a ledger."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-machine :as machine])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io StringReader]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel OverlappingFileLockException]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption OpenOption Path StandardOpenOption]
           [java.nio.file.attribute FileAttribute]))

(defonce ^:private local-path-locks (atom {}))

(defn- local-path-lock [^Path path]
  (let [key (str (.normalize (.toAbsolutePath path)))]
    (or (get @local-path-locks key)
        (get (swap! local-path-locks
                    #(if (contains? % key) % (assoc % key (Object.))))
             key))))

(def ^:private eof (Object.))

(defn- ledger-path [path]
  (cond
    (instance? Path path) path
    (string? path) (Path/of path (make-array String 0))
    :else nil))

(defn- read-form [^LineNumberingPushbackReader reader]
  (try
    {:ok true :form (edn/read {:eof eof} reader)}
    (catch Throwable t
      {:ok false :error/code :campaign-ledger-corrupt
       :finding {:line (.getLineNumber reader)
                 :message (.getMessage t)}})))

(defn- parse-events [text]
  (with-open [reader (LineNumberingPushbackReader. (StringReader. text))]
    (loop [events []]
      (let [result (read-form reader)]
        (cond
          (not (:ok result)) result
          (identical? eof (:form result)) {:ok true :events events}
          :else (recur (conj events (:form result))))))))

(defn read-ledger
  "Read and validate PATH. A missing file is the valid empty ledger."
  [path]
  (if-let [path (ledger-path path)]
    (try
      (let [text (if (Files/exists path (make-array LinkOption 0))
                   (Files/readString path StandardCharsets/UTF_8)
                   "")
            parsed (parse-events text)]
        (if-not (:ok parsed)
          parsed
          (let [events (:events parsed)
                projection (machine/projection events)]
            (if (= :valid (:projection/status projection))
              {:ok true :path (str path) :events events
               :projection projection}
              {:ok false :error/code :campaign-ledger-invalid
               :path (str path) :projection projection}))))
      (catch Throwable t
        {:ok false :error/code :campaign-ledger-read-failed
         :path (str path) :finding {:message (.getMessage t)}}))
    {:ok false :error/code :campaign-ledger-path-invalid}))

(defn- ensure-parent! [^Path path]
  (when-let [parent (.getParent (.toAbsolutePath path))]
    (Files/createDirectories parent (make-array FileAttribute 0))))

(defn- read-locked-channel [^FileChannel channel]
  (.position channel 0)
  (let [size (.size channel)]
    (when (> size Integer/MAX_VALUE)
      (throw (ex-info "Campaign ledger exceeds supported size"
                      {:error/code :campaign-ledger-too-large :bytes size})))
    (let [buffer (ByteBuffer/allocate (int size))]
      (while (.hasRemaining buffer)
        (when (= -1 (.read channel buffer))
          (throw (ex-info "Unexpected EOF while reading campaign ledger"
                          {:error/code :campaign-ledger-short-read}))))
      (.flip buffer)
      (str (.decode StandardCharsets/UTF_8 buffer)))))

(defn- append-bytes! [^FileChannel channel event]
  (let [bytes (.getBytes (str (pr-str event) "\n") StandardCharsets/UTF_8)
        buffer (ByteBuffer/wrap bytes)
        offset (.size channel)]
    (.position channel offset)
    (while (.hasRemaining buffer)
      (.write channel buffer))
    (.force channel true)
    {:byte-offset offset :byte-count (alength bytes)}))

(defn- mismatch [code expected actual]
  {:ok false :error/code code :finding {:expected expected :actual actual}})

(defn compare-and-append!
  "Append EVENT only when PATH's current projection has EXPECTED-VERSION and
  EXPECTED-DIGEST. Returns a durable before/after receipt or an explicit
  refusal. The event's own :event/expected-version is independently checked by
  campaign-machine."
  [path expected-version expected-digest event]
  (if-let [path (ledger-path path)]
    (let [path-lock (local-path-lock path)]
      (locking path-lock
        (try
        (ensure-parent! path)
        (with-open [channel (FileChannel/open
                             path
                             (into-array OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/READ
                                          StandardOpenOption/WRITE]))
                    _lock (.lock channel)]
          (let [parsed (parse-events (read-locked-channel channel))]
            (if-not (:ok parsed)
              parsed
              (let [events (:events parsed)
                    before (machine/projection events)
                    before-version (:campaign/version before)
                    before-digest (:ledger/digest before)]
                (cond
                  (not= :valid (:projection/status before))
                  {:ok false :error/code :campaign-ledger-invalid
                   :projection before}

                  (not= expected-version before-version)
                  (mismatch :campaign-ledger-version-mismatch
                            expected-version before-version)

                  (not= expected-digest before-digest)
                  (mismatch :campaign-ledger-digest-mismatch
                            expected-digest before-digest)

                  :else
                  (let [successor-events (conj events event)
                        after (machine/projection successor-events)]
                    (if-not (= :valid (:projection/status after))
                      {:ok false :error/code :campaign-event-refused
                       :projection after}
                      (let [write (append-bytes! channel event)]
                        {:ok true :durable? true :path (str path)
                         :event/id (:event/id event)
                         :before {:version before-version
                                  :digest before-digest
                                  :event-count (:ledger/event-count before)}
                         :after {:version (:campaign/version after)
                                 :digest (:ledger/digest after)
                                 :event-count (:ledger/event-count after)}
                         :write write}))))))))
        (catch OverlappingFileLockException _
          {:ok false :error/code :campaign-ledger-lock-busy :path (str path)})
          (catch Throwable t
            {:ok false :error/code (or (:error/code (ex-data t))
                                      :campaign-ledger-append-failed)
             :path (str path) :finding {:message (.getMessage t)}}))))
    {:ok false :error/code :campaign-ledger-path-invalid}))
