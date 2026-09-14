(ns futon3c.wm.r10-commission
  "Offline R10 commission authority and durable pre-dispatch reservation.

  Every authority path, digest, and reservation root is supplied explicitly.
  This namespace has no production binding and does not call the WM runner."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (java.io PushbackReader StringReader)
           (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path StandardCopyOption StandardOpenOption)
           (java.security MessageDigest)
           (java.time Instant)
           (java.util UUID)))

(def commission-schema :wm/r10-click-commission-v1)
(def reservation-schema :wm/r10-dispatch-reservation-v1)

(defn- refuse! [code data]
  (throw (ex-info "R10 commission refused"
                  (merge {:error/type :r10/commission-refusal
                          :error/code code}
                         data))))

(defn sha256-bytes [^bytes bs]
  (format "%064x" (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256") bs))))

(defn- strict-edn [^String text]
  (let [eof (Object.)]
    (try
      (with-open [r (PushbackReader. (StringReader. text))]
        (let [v (edn/read {:eof eof} r)]
          (when (or (identical? eof v)
                    (not (identical? eof (edn/read {:eof eof} r))))
            (refuse! :r10/authority-invalid {:reason :not-exactly-one-edn-form}))
          v))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (refuse! :r10/authority-invalid {:reason :malformed-edn
                                         :exception (.getName (class e))})))))

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- digest? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))

(defn load-authorized-commission
  "Read one operator-selected authority file once and validate its exact digest
  and commission shape. Request data cannot select these arguments in a wired
  caller; that server-owned binding remains a separate integration packet."
  [{:keys [authority-path authority-sha256]}]
  (when-not (and (nonblank? authority-path) (digest? authority-sha256))
    (refuse! :r10/invalid-commission {:reason :authority-binding-invalid}))
  (let [bytes (try (Files/readAllBytes (Path/of authority-path (make-array String 0)))
                   (catch Throwable e
                     (refuse! :r10/invalid-commission
                              {:reason :authority-unavailable
                               :exception (.getName (class e))})))
        actual (sha256-bytes bytes)]
    (when-not (= authority-sha256 actual)
      (refuse! :r10/invalid-commission
               {:reason :authority-sha256-mismatch :expected authority-sha256
                :actual actual}))
    (let [record (strict-edn (String. bytes StandardCharsets/UTF_8))
          required #{:schema :commission/id :commission/issuer
                     :commission/source-pin :commission/scope}]
      (when-not (and (map? record) (= required (set (keys record)))
                     (= commission-schema (:schema record))
                     (nonblank? (:commission/id record))
                     (nonblank? (:commission/issuer record))
                     (digest? (:commission/source-pin record))
                     (map? (:commission/scope record))
                     (seq (:commission/scope record)))
        (refuse! :r10/invalid-commission {:reason :commission-shape-invalid}))
      (assoc record :authority/path authority-path
                    :authority/sha256 actual))))

(defn- reservation-path ^Path [root commission-id]
  (when-not (and (nonblank? root) (nonblank? commission-id)
                 (re-matches #"[A-Za-z0-9._-]+" commission-id))
    (refuse! :r10/invalid-commission {:reason :reservation-address-invalid}))
  (.resolve (Path/of root (make-array String 0)) (str commission-id ".edn")))

(defn- write-record! [^Path path record options]
  (Files/write path (.getBytes (str (pr-str record) "\n") StandardCharsets/UTF_8)
               (into-array StandardOpenOption options)))

(defn reserve!
  "CREATE_NEW is the durable uniqueness check. Returns only after the intent is
  present. A pre-existing commission is never silently reused."
  [{:keys [reservation-root commission now]}]
  (let [id (:commission/id commission)
        path (reservation-path reservation-root id)
        record {:schema reservation-schema :commission/id id
                :commission/authority-sha256 (:authority/sha256 commission)
                :state :reserved :reserved-at (str (or now (Instant/now)))}]
    (try
      (Files/createDirectories (.getParent path) (make-array java.nio.file.attribute.FileAttribute 0))
      (write-record! path record [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE])
      {:ok true :path (str path) :reservation record}
      (catch java.nio.file.FileAlreadyExistsException _
        (refuse! :r10/duplicate-commission {:commission/id id :path (str path)}))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (refuse! :r10/recording-failed {:phase :reservation
                                        :commission/id id
                                        :exception (.getName (class e))})))))

(defn read-reservation [{:keys [reservation-root commission-id]}]
  (let [path (reservation-path reservation-root commission-id)]
    (when-not (Files/exists path (make-array LinkOption 0))
      (refuse! :r10/reservation-missing {:commission/id commission-id}))
    (let [record (try
                   (strict-edn (slurp (.toFile path)))
                   (catch clojure.lang.ExceptionInfo e
                     (if (= :r10/authority-invalid (:error/code (ex-data e)))
                       (refuse! :r10/reservation-invalid
                                {:commission/id commission-id
                                 :reason :malformed-reservation})
                       (throw e))))
          state (:state record)]
      (when-not (and (= reservation-schema (:schema record))
                     (= commission-id (:commission/id record))
                     (contains? #{:reserved :dispatched :recorded} state))
        (refuse! :r10/reservation-invalid {:commission/id commission-id}))
      {:record record :recovery-state (if (= :reserved state) :dangling state)})))

(defn- transition! [{:keys [reservation-root commission-id]} from to additions]
  (let [path (reservation-path reservation-root commission-id)
        current (:record (read-reservation {:reservation-root reservation-root
                                            :commission-id commission-id}))]
    (when-not (= from (:state current))
      (refuse! :r10/invalid-transition {:from (:state current) :expected from :to to}))
    (let [next (merge current additions {:state to})
          temp (.resolve (.getParent path) (str "." commission-id "." (UUID/randomUUID) ".tmp"))]
      (try
        (write-record! temp next [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE])
        (Files/move temp path (into-array StandardCopyOption
                                          [StandardCopyOption/ATOMIC_MOVE
                                           StandardCopyOption/REPLACE_EXISTING]))
        next
        (catch Throwable e
          (try (Files/deleteIfExists temp) (catch Throwable _))
          (refuse! :r10/recording-failed {:phase :transition :to to
                                          :exception (.getName (class e))}))))))

(defn dispatch-reserved!
  "Reserve before invoking DISPATCH-FN. The server-issued click id is the exact
  dispatch id. A malformed receipt is explicitly after-dispatch and leaves the
  reservation visible as dangling. Successful receipt validation advances to
  :dispatched; evidence recording later advances it to :recorded."
  [{:keys [reservation-root commission dispatch-fn now]}]
  (when-not (fn? dispatch-fn)
    (refuse! :r10/invalid-commission {:reason :dispatch-function-missing}))
  (reserve! {:reservation-root reservation-root :commission commission :now now})
  (let [id (:commission/id commission)
        receipt (dispatch-fn (assoc commission :node :R10))]
    (when-not (and (map? receipt) (= :R10 (:node receipt))
                   (= id (:commission/id receipt))
                   (nonblank? (:dispatch/id receipt))
                   (= (:dispatch/id receipt) (:click/id receipt)))
      (refuse! :r10/unlinked-dispatch-receipt
               {:commission/id id :dispatch/occurred true :receipt receipt}))
    (transition! {:reservation-root reservation-root :commission-id id}
                 :reserved :dispatched
                 {:dispatch/id (:dispatch/id receipt) :dispatched-at (str (or now (Instant/now)))})
    {:ok true :commission commission :receipt receipt}))

(defn mark-recorded! [{:keys [reservation-root commission-id evidence-id now]}]
  (when-not (nonblank? evidence-id)
    (refuse! :r10/recording-failed {:phase :completion :reason :evidence-id-missing}))
  (transition! {:reservation-root reservation-root :commission-id commission-id}
               :dispatched :recorded
               {:evidence/id evidence-id :recorded-at (str (or now (Instant/now)))}))
