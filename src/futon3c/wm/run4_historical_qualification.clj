(ns futon3c.wm.run4-historical-qualification
  "Server-only producer of qualification output. A reviewed manifest supplies
  commands; callers cannot submit result rows. No repair admission or dispatch.
  Passing commands are qualification evidence, never independent review."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.wm.run4-realized-recording :as recording])
  (:import [java.nio.file Files LinkOption StandardOpenOption]
           [java.nio.channels FileChannel]
           [java.security MessageDigest]
           [java.util.concurrent TimeUnit]))

(defn- refuse! [reason]
  (throw (ex-info "Historical qualification refused" {:reason reason})))

(defn- sha [bytes]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (MessageDigest/getInstance "SHA-256") bytes))))

(defn- safe-id? [x]
  (and (string? x) (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" x))))

(defn- pin? [x]
  (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))

(defn- captured! [root path]
  (when-not (string? path) (refuse! :invalid-path))
  (let [base (.getCanonicalFile (io/file root))
        f (io/file path)
        real (.getCanonicalFile f)]
    (when-not (and (.isDirectory base)
                   (Files/isRegularFile (.toPath f) (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                   (.startsWith (.toPath real) (.toPath base))
                   (not= real base))
      (refuse! :source-outside-authority))
    {:path (.getPath real) :bytes (Files/readAllBytes (.toPath real))}))

(defn- one-form! [bytes]
  (try
    (with-open [r (java.io.PushbackReader.
                   (java.io.StringReader. (String. ^bytes bytes "UTF-8")))]
      (let [v (edn/read {:eof ::empty} r)]
        (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
          (refuse! :invalid-manifest))
        v))
    (catch Exception _ (refuse! :invalid-manifest))))

(defn- validate! [m]
  (when-not
   (and (map? m)
        (= #{:schema :verification-id :repair-id :sources :checks} (set (keys m)))
        (= :wm/historical-qualification-plan-v1 (:schema m))
        (every? safe-id? [(:verification-id m) (:repair-id m)])
        (vector? (:sources m)) (seq (:sources m))
        (= (count (:sources m)) (count (distinct (map :path (:sources m)))))
        (every? #(and (= #{:path :sha256} (set (keys %)))
                       (string? (:path %)) (pin? (:sha256 %))) (:sources m))
        (vector? (:checks m)) (seq (:checks m))
        (= (count (:checks m)) (count (distinct (map :id (:checks m)))))
        (every? #(and (= #{:id :argv :timeout-ms} (set (keys %)))
                       (keyword? (:id %)) (vector? (:argv %)) (seq (:argv %))
                       (every? (fn [a] (and (string? a) (not (empty? a)))) (:argv %))
                       (integer? (:timeout-ms %)) (<= 1 (:timeout-ms %) 600000))
                (:checks m)))
    (refuse! :invalid-plan))
  m)

(defn- sources! [root sources]
  (mapv (fn [{:keys [path sha256]}]
          (let [c (captured! root path) h (sha (:bytes c))]
            (when-not (= h sha256) (refuse! :source-drift))
            {:path (:path c) :sha256 h})) sources))

(defn- output! [path]
  (when (> (Files/size path) 1048576) (refuse! :check-output-too-large))
  (let [bytes (Files/readAllBytes path)]
    {:sha256 (sha bytes) :utf8 (String. ^bytes bytes "UTF-8")}))

(defn- execute! [repo {:keys [id argv timeout-ms]}]
  ;; Output is redirected to disposable files to avoid pipe deadlock. The
  ;; exact command is preregistered. This is not a sandbox or a purity claim.
  (let [dir (Files/createTempDirectory "wm-qualification-output-"
                                       (make-array java.nio.file.attribute.FileAttribute 0))
        out (.resolve dir "stdout") err (.resolve dir "stderr")]
    (try
      (let [p (.start (doto (ProcessBuilder. ^java.util.List argv)
                       (.directory (io/file repo))
                       (.redirectOutput (.toFile out))
                       (.redirectError (.toFile err))))
            finished? (.waitFor p (long timeout-ms) TimeUnit/MILLISECONDS)]
        (when-not finished?
          (.destroyForcibly p)
          (.waitFor p))
        {:id id :argv argv :timeout-ms timeout-ms :timed-out? (not finished?)
         :exit (.exitValue p)
         :stdout (output! out) :stderr (output! err)})
      (finally
        (Files/deleteIfExists out) (Files/deleteIfExists err) (Files/deleteIfExists dir)))))

(defn- produce-locked!
  "Execute the exact nonempty command population in a digest-pinned, reviewed
  server manifest. Recheck pins after execution. Writes only a fresh evidence
  receipt under OUTPUT-ROOT. Has no callback for caller-supplied results.
  Commands themselves must be independently reviewed for allowed effects."
  [{:keys [source-root output-root manifest-path manifest-sha256]}]
  (when-not (pin? manifest-sha256) (refuse! :invalid-manifest-pin))
  (let [c (captured! source-root manifest-path)
        _ (when-not (= manifest-sha256 (sha (:bytes c))) (refuse! :manifest-drift))
        m (validate! (one-form! (:bytes c)))
        before (sources! source-root (:sources m))
        base (.getCanonicalFile (io/file output-root))
        target (io/file base (str (:verification-id m) ".qualification.edn"))]
    (when-not (and (.isDirectory base)
                   (= base (.getCanonicalFile (.getParentFile target)))
                   (not (Files/exists (.toPath target)
                                      (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))))
      (refuse! :output-not-fresh))
    (let [rows (mapv #(execute! source-root %) (:checks m))
          after (sources! source-root (:sources m))
          current (captured! source-root manifest-path)]
      (when-not (and (= before after) (= manifest-sha256 (sha (:bytes current))))
        (refuse! :post-execution-drift))
      (let [record {:schema :wm/historical-qualification-output-v1
                    :verification-id (:verification-id m) :repair-id (:repair-id m)
                    :manifest {:path (:path c) :sha256 manifest-sha256}
                    :sources before :checks rows
                    :qualification-passed? (every? #(and (zero? (:exit %))
                                                        (false? (:timed-out? %))) rows)
                    :independent-review :not-performed :repair-admitted? false}]
        (recording/*append-immutable!* target record)
        record))))

(defonce ^:private publication-locks (atom {}))

(defn produce!
  "Server-only invocation. Serializes qualification executions sharing an
  output root across JVM threads and processes; never reruns an existing ID."
  [{:keys [output-root] :as opts}]
  (when-not (= #{:source-root :output-root :manifest-path :manifest-sha256}
                (set (keys opts)))
    (refuse! :invalid-producer-options))
  (let [base (.getCanonicalFile (io/file output-root))
        _ (when-not (.isDirectory base) (refuse! :invalid-output-root))
        key (.getPath base)
        mutex (get (swap! publication-locks
                          #(if (contains? % key) % (assoc % key (Object.)))) key)
        lock-file (io/file base ".historical-qualification.lock")]
    (when (Files/isSymbolicLink (.toPath lock-file)) (refuse! :invalid-lock-file))
    (locking mutex
      (with-open [channel (FileChannel/open (.toPath lock-file)
                                           (into-array java.nio.file.OpenOption
                                                       [StandardOpenOption/CREATE
                                                        StandardOpenOption/WRITE
                                                        LinkOption/NOFOLLOW_LINKS]))
                  _lock (.lock channel)]
        (produce-locked! opts)))))
