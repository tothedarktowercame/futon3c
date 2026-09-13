(ns futon3c.agency.invoke-ingress-controller
  "Prospective invoke intake/drain state machine. This namespace is not wired
  into the serving HTTP process. One controller owns its generation and token;
  request candidates cannot supply either."
  (:require [clojure.edn :as edn])
  (:import (java.io PushbackReader StringReader)
           (java.net InetAddress)
           (java.nio ByteBuffer)
           (java.nio.charset CodingErrorAction StandardCharsets)
           (java.nio.channels FileChannel)
           (java.nio.file Files Path StandardCopyOption StandardOpenOption)
           (java.security MessageDigest)
           (java.util.concurrent.locks ReentrantLock)))

(defn- refuse! [code data]
  (throw (ex-info (name code) (assoc data :refusal code))))

(def deferred-schema :agency/deferred-resumes-v1)
(declare one-edn)

(defn- sha256 [^bytes bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))

(defn- deferred-projection [state]
  {:schema deferred-schema
   :order (:deferred-order state)
   :records (:deferred state)})

(defn- validate-projection! [p]
  (let [order (:order p) records (:records p)]
    (when-not (= deferred-schema (:schema p))
      (refuse! :ingress/deferred-schema-invalid {:schema (:schema p)}))
    (when-not (and (vector? order) (= (count order) (count (distinct order)))
                   (map? records) (= (set order) (set (keys records)))
                   (every? (fn [[id rec]]
                             (and (string? id) (= :pending (:status rec))
                                  (contains? rec :payload))) records))
      (refuse! :ingress/deferred-projection-invalid {}))
    (try
      (when-not (= p (one-edn (pr-str p)))
        (refuse! :ingress/deferred-edn-roundtrip-failed {}))
      (catch clojure.lang.ExceptionInfo e
        (if (:refusal (ex-data e))
          (throw (ex-info "unsupported deferred payload"
                          {:refusal :ingress/deferred-payload-unsupported} e))
          (throw e))))
    p))

(defn- decode-utf8 [^bytes bs]
  (try
    (str (-> (.newDecoder StandardCharsets/UTF_8)
             (.onMalformedInput CodingErrorAction/REPORT)
             (.onUnmappableCharacter CodingErrorAction/REPORT)
             (.decode (ByteBuffer/wrap bs))))
    (catch Throwable e
      (throw (ex-info "invalid deferred UTF-8" {:refusal :ingress/deferred-invalid-utf8} e)))))

(defn- one-edn [s]
  (try
    (with-open [r (PushbackReader. (StringReader. s))]
      (let [x (edn/read {:eof ::eof} r) trailing (edn/read {:eof ::eof} r)]
        (when (or (= ::eof x) (not= ::eof trailing))
          (refuse! :ingress/deferred-edn-cardinality {}))
        x))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable e
      (throw (ex-info "invalid deferred EDN" {:refusal :ingress/deferred-edn-invalid} e)))))

(defn file-deferred-store
  "Isolated durable adapter. The on-disk envelope authenticates the canonical
  projection bytes; recovery reads the file once, then parses those same bytes."
  [path]
  (let [^Path target (Path/of (str path) (make-array String 0))
        ^Path lock-path (.resolveSibling target (str (.getFileName target) ".lock"))]
    {:path (str target)
     :acquire!
     (fn []
       (let [ch (FileChannel/open lock-path (into-array StandardOpenOption
                                                        [StandardOpenOption/CREATE
                                                         StandardOpenOption/WRITE]))]
         (try
           (if-let [lock (try (.tryLock ch) (catch java.nio.channels.OverlappingFileLockException _ nil))]
             {:channel ch :lock lock}
             (do (.close ch) (refuse! :ingress/deferred-store-owned {:path (str target)})))
           (catch Throwable e
             (when (.isOpen ch) (.close ch))
             (if (instance? clojure.lang.ExceptionInfo e) (throw e)
                 (throw (ex-info "store ownership failed"
                                 {:refusal :ingress/deferred-store-ownership-failed} e)))))))
     :release!
     (fn [{:keys [^java.nio.channels.FileLock lock ^FileChannel channel]}]
       (when (and lock (.isValid lock)) (.release lock))
       (when (and channel (.isOpen channel)) (.close channel))
       true)
     :persist!
     (fn [_lease projection]
       (let [projection (validate-projection! projection)
             projection-bytes (.getBytes (pr-str projection) StandardCharsets/UTF_8)
             envelope {:schema :agency/deferred-resume-envelope-v1
                       :projection projection :projection-sha256 (sha256 projection-bytes)}
             bytes (.getBytes (pr-str envelope) StandardCharsets/UTF_8)
             parent (.getParent target)
             tmp (Files/createTempFile parent ".deferred-" ".tmp" (make-array java.nio.file.attribute.FileAttribute 0))]
         (try
           (Files/write tmp bytes (into-array StandardOpenOption [StandardOpenOption/WRITE
                                                                  StandardOpenOption/TRUNCATE_EXISTING]))
           (with-open [ch (FileChannel/open tmp (into-array StandardOpenOption [StandardOpenOption/WRITE]))]
             (.force ch true))
           (Files/move tmp target (into-array StandardCopyOption
                                              [StandardCopyOption/ATOMIC_MOVE
                                               StandardCopyOption/REPLACE_EXISTING]))
           (with-open [ch (FileChannel/open parent (into-array StandardOpenOption [StandardOpenOption/READ]))]
             (.force ch true))
           true
           (finally (Files/deleteIfExists tmp)))))
     :read!
     (fn [_lease]
       (when-not (Files/isRegularFile target (make-array java.nio.file.LinkOption 0))
         (refuse! :ingress/deferred-store-missing {:path (str target)}))
       (let [bytes (Files/readAllBytes target)
             envelope (one-edn (decode-utf8 bytes))
             projection (:projection envelope)
             projection-bytes (.getBytes (pr-str projection) StandardCharsets/UTF_8)]
         (when-not (= :agency/deferred-resume-envelope-v1 (:schema envelope))
           (refuse! :ingress/deferred-envelope-schema-invalid {}))
         (when-not (= (:projection-sha256 envelope) (sha256 projection-bytes))
           (refuse! :ingress/deferred-digest-mismatch {}))
         (validate-projection! projection)))}))

(defn initialize-file-store!
  "Explicitly create a fresh empty store; refuses to overwrite any record."
  [store]
  (let [path (Path/of (:path store) (make-array String 0))
        lease ((:acquire! store))
        projection {:schema deferred-schema :order [] :records {}}
        projection-bytes (.getBytes (pr-str projection) StandardCharsets/UTF_8)
        envelope {:schema :agency/deferred-resume-envelope-v1
                  :projection projection :projection-sha256 (sha256 projection-bytes)}
        bytes (.getBytes (pr-str envelope) StandardCharsets/UTF_8)]
    (try
      (try
        (Files/write path bytes (into-array StandardOpenOption
                                            [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
        (catch java.nio.file.FileAlreadyExistsException _
          (refuse! :ingress/deferred-store-already-exists {:path (str path)})))
      (with-open [ch (FileChannel/open path (into-array StandardOpenOption [StandardOpenOption/WRITE]))]
        (.force ch true))
      (with-open [ch (FileChannel/open (.getParent path) (into-array StandardOpenOption [StandardOpenOption/READ]))]
        (.force ch true))
      true
      (finally ((:release! store) lease)))))

(defmacro ^:private with-controller-lock [c & body]
  `(let [^ReentrantLock lock# (:lock ~c)]
     (.lock lock#)
     (try
       (when @(:released? ~c)
         (refuse! :ingress/controller-released {}))
       ~@body
       (finally (.unlock lock#)))))

(defn controller
  "Construct an isolated controller. PERSIST! must durably replace the deferred
  resume state before a transition becomes visible. AUTH-TOKEN is provisioned
  by the local operator boundary, never taken from an invoke request."
  [{:keys [auth-token deferred-store test-only?]}]
  (when-not (and (string? auth-token) (not-empty auth-token))
    (refuse! :ingress/auth-token-missing {}))
  (when-not (or deferred-store test-only?)
    (refuse! :ingress/deferred-store-required {}))
  (let [lease (when deferred-store ((:acquire! deferred-store)))
        projection (try
                     (if deferred-store
                       ((:read! deferred-store) lease)
                       {:schema deferred-schema :order [] :records {}})
                     (catch Throwable e
                       (when lease ((:release! deferred-store) lease))
                       (throw e)))]
   {:lock (ReentrantLock.)
    :released? (atom false)
    :auth-token auth-token
    :deferred-store deferred-store
    :store-lease lease
    :test-only? test-only?
    :state (atom {:mode :open :generation 0 :waiting-writer 0 :entrant-tokens #{}
                 :accepted-queued #{} :executing #{} :final-delivery #{}
                 :deferred-order (:order projection) :deferred (:records projection)})}))

(defn- persist-state! [c next-state]
  (when-let [store (:deferred-store c)]
    (try
      (when-not (true? ((:persist! store) (:store-lease c)
                        (deferred-projection next-state)))
        (refuse! :ingress/deferred-persistence-failed {}))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (throw (ex-info "deferred persistence failed"
                        {:refusal :ingress/deferred-persistence-failed} e)))))
  (reset! (:state c) next-state))

(defn release-controller!
  "Release exclusive deferred-store ownership. The controller must not be used
  afterward; integration owns lifecycle enforcement around this boundary."
  [c]
  (with-controller-lock c
    (when-let [store (:deferred-store c)]
      ((:release! store) (:store-lease c)))
    (reset! (:released? c) true)
    true))

(defn begin-creation!
  "Register an entrant before it waits for the invoke-jobs writer lock. Returns
  an opaque generation ticket, or refuses after intake closes."
  [c]
  (with-controller-lock c
    (let [s @(:state c)]
      (when-not (= :open (:mode s))
        (refuse! :ingress/intake-closed {:generation (:generation s)}))
      (let [token (Object.)]
        (swap! (:state c) #(-> % (update :waiting-writer inc)
                                  (update :entrant-tokens conj token)))
        {:generation (:generation s) :token token}))))

(defn creation-finished!
  "Move a waiting entrant to the accepted queue, or merely release it when its
  ledger operation failed. Must run in finally around the writer-lock section."
  [c ticket accepted-job-id]
  (with-controller-lock c
    (let [s @(:state c)]
      (when-not (contains? (:entrant-tokens s) (:token ticket))
        (refuse! :ingress/entrant-ticket-invalid {}))
      (swap! (:state c)
             (fn [x] (cond-> (-> x (update :waiting-writer dec)
                                    (update :entrant-tokens disj (:token ticket)))
                       accepted-job-id (update :accepted-queued conj (str accepted-job-id))))))))

(defn start-execution! [c job-id]
  (with-controller-lock c
    (let [id (str job-id)]
      (when-not (contains? (:accepted-queued @(:state c)) id)
        (refuse! :ingress/job-not-queued {:job-id id}))
      (swap! (:state c) #(-> % (update :accepted-queued disj id)
                              (update :executing conj id))))))

(defn finish-execution! [c job-id]
  (with-controller-lock c
    (let [id (str job-id)]
      (when-not (contains? (:executing @(:state c)) id)
        (refuse! :ingress/job-not-executing {:job-id id}))
      (swap! (:state c) #(-> % (update :executing disj id)
                              (update :final-delivery conj id))))))

(defn finish-delivery! [c job-id]
  (with-controller-lock c
    (let [id (str job-id)]
      (when-not (contains? (:final-delivery @(:state c)) id)
        (refuse! :ingress/job-not-delivering {:job-id id}))
      (swap! (:state c) update :final-delivery disj id))))

(defn close-intake! [c]
  (with-controller-lock c
    (swap! (:state c) #(if (= :open (:mode %))
                         (-> % (assoc :mode :closed) (update :generation inc)) %))
    @(:state c)))

(defn drained? [c]
  (when @(:released? c) (refuse! :ingress/controller-released {}))
  (let [{:keys [waiting-writer accepted-queued executing final-delivery]} @(:state c)]
    (and (zero? waiting-writer) (empty? accepted-queued)
         (empty? executing) (empty? final-delivery))))

(defn defer-resume!
  "Durably retain an internal parked completion/deadline resume while closed.
  Repeating the identical id/payload is idempotent; mutation refuses."
  [c resume-id payload]
  (with-controller-lock c
    (when-not (and (string? resume-id) (not-empty resume-id)
                   (map? payload) (= resume-id (:requested-job-id payload)))
      (refuse! :ingress/deferred-payload-schema-invalid {:resume-id resume-id}))
    (let [id resume-id s @(:state c) old (get-in s [:deferred id])]
      (when (= :open (:mode s))
        (refuse! :ingress/defer-while-open {:resume-id id}))
      (when (and old (not= payload (:payload old)))
        (refuse! :ingress/deferred-resume-conflict {:resume-id id}))
      (when-not old
        (let [next-state (-> s
                             (update :deferred-order conj id)
                             (assoc-in [:deferred id] {:payload payload :status :pending}))]
          (validate-projection! (deferred-projection next-state))
          (persist-state! c next-state)))
      id)))

(defn reopen! [c]
  (with-controller-lock c
    (when-not (drained? c)
      (refuse! :ingress/not-drained {}))
    (swap! (:state c) #(-> % (assoc :mode :open) (update :generation inc)))
    (mapv (fn [id] [id (get-in @(:state c) [:deferred id :payload])])
          (:deferred-order @(:state c)))))

(defn acknowledge-resume!
  "Remove a deferred resume only after downstream acceptance under its stable
  requested job id. A retry before acknowledgement replays the same pair."
  [c resume-id]
  (with-controller-lock c
    (let [id (str resume-id) s @(:state c)]
      (when-not (contains? (:deferred s) id)
        (refuse! :ingress/deferred-resume-missing {:resume-id id}))
      (persist-state! c (-> s
                            (update :deferred dissoc id)
                            (update :deferred-order #(vec (remove #{id} %))))))))

(defn- constant-time= [a b]
  (MessageDigest/isEqual (.getBytes (str a) StandardCharsets/UTF_8)
                         (.getBytes (str b) StandardCharsets/UTF_8)))

(defn verification-snapshot
  "Bounded local verification API. Integration must expose it on a distinct
  loopback-only listener; ordinary Agency HTTP routes must not dispatch here."
  [c {:keys [remote-addr auth-token]}]
  (when @(:released? c) (refuse! :ingress/controller-released {}))
  (let [local? (try (.isLoopbackAddress (InetAddress/getByName (str remote-addr)))
                    (catch Throwable _ false))]
    (when-not local? (refuse! :ingress/verification-not-loopback {}))
    (when-not (constant-time= (:auth-token c) auth-token)
      (refuse! :ingress/verification-unauthorized {}))
    (let [s @(:state c)
          drained (and (zero? (:waiting-writer s))
                       (empty? (:accepted-queued s))
                       (empty? (:executing s))
                       (empty? (:final-delivery s)))]
      (assoc (select-keys s [:mode :generation :waiting-writer])
             :accepted-queued (count (:accepted-queued s))
             :executing (count (:executing s))
             :final-delivery (count (:final-delivery s))
             :deferred (count (:deferred s))
             :drained? drained
             :restart-authorized? false))))
