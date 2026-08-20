(ns futon3c.apm.campaign-snapshot
  "Assemble and persist immutable campaign projection certificates."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-observe :as observe]
            [futon3c.apm.campaign-reconcile :as reconcile])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file FileAlreadyExistsException Files LinkOption OpenOption Path
            StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defn- build-certificate [body]
  (assoc body :certificate/id (machine/ledger-digest [body])))

(defn snapshot
  "Load LEDGER-PATH, observe runtime facts, reconcile, and return one immutable
  certificate value. OBSERVATION supplies file paths and already-fetched HTTP
  response bodies. NOW is explicit so freshness and certificate identity are
  reproducible."
  [{:keys [ledger-path observation now max-age-ms]
    :or {max-age-ms 60000}}]
  (cond
    (not (instance? Instant now))
    {:ok false :error/code :campaign-snapshot-clock-required}

    :else
    (let [loaded (ledger/read-ledger ledger-path)]
      (if-not (:ok loaded)
        {:ok false :error/code :campaign-snapshot-ledger-unavailable
         :ledger loaded}
        (let [projection (:projection loaded)
              active (:active/frame projection)
              observed (observe/observe-facts
                        (assoc observation :now now
                               :expected-frame-id (:frame-id active)))]
          (if-not (:ok observed)
            {:ok true
             :certificate
             (build-certificate
              {:certificate/type :campaign-projection
               :certificate/version 1
               :generated-at (str now)
               :snapshot/status :stale
               :campaign/id (:campaign/id projection)
               :campaign/version (:campaign/version projection)
               :ledger/digest (:ledger/digest projection)
               :ledger/event-count (:ledger/event-count projection)
               :active/frame-id (:frame-id active)
               :observation/error (:error/code observed)
               :observation/failures (:failures observed)})}
            (let [reconciliation
                  (reconcile/reconcile projection (:facts observed) now max-age-ms)]
              {:ok true
               :certificate
               (build-certificate
                {:certificate/type :campaign-projection
                 :certificate/version 1
                 :generated-at (str now)
                 :snapshot/status (:reconciliation/status reconciliation)
                 :campaign/id (:campaign/id projection)
                 :campaign/series (:campaign/series projection)
                 :campaign/manifest-hash (:campaign/manifest-hash projection)
                 :campaign/status (:campaign/status projection)
                 :campaign/version (:campaign/version projection)
                 :campaign/phase-order (:campaign/phase-order projection)
                 :campaign/block-plan (:campaign/block-plan projection)
                 :campaign/obligation-plan (:campaign/obligation-plan projection)
                 :campaign/claims-required? (:campaign/claims-required? projection)
                 :campaign/permit-usage (:campaign/permit-usage projection)
                 :campaign/blocks (:campaign/blocks projection)
                 :campaign/frames (:campaign/frames projection)
                 :ledger/digest (:ledger/digest projection)
                 :ledger/event-count (:ledger/event-count projection)
                 :facts/digest (:facts/digest reconciliation)
                 :active/block (:active/block projection)
                 :active/frame (:active/frame projection)
                 :active/claim (:active/claim projection)
                 :counts (:counts projection)
                 :reconciliation reconciliation})})))))))

(defn- directory-path [x]
  (cond
    (instance? Path x) x
    (string? x) (Path/of x (make-array String 0))
    :else nil))

(defn persist!
  "Persist CERTIFICATE under DIRECTORY/<certificate-id>.edn using CREATE_NEW.
  An existing byte-equivalent certificate is an idempotent success; any
  content disagreement at that identity is a coherence conflict."
  [directory certificate-value]
  (let [directory (directory-path directory)
        certificate-id (:certificate/id certificate-value)]
    (cond
      (nil? directory)
      {:ok false :error/code :campaign-certificate-directory-invalid}

      (not (and (string? certificate-id)
                (re-matches #"[0-9a-f]{64}" certificate-id)))
      {:ok false :error/code :campaign-certificate-id-invalid}

      (not= certificate-id
            (:certificate/id
             (build-certificate (dissoc certificate-value :certificate/id))))
      {:ok false :error/code :campaign-certificate-content-mismatch}

      :else
      (try
        (Files/createDirectories directory (make-array FileAttribute 0))
        (let [target (.resolve directory (str certificate-id ".edn"))
              text (str (pr-str certificate-value) "\n")]
          (try
            (Files/writeString target text StandardCharsets/UTF_8
                               (into-array OpenOption
                                           [StandardOpenOption/CREATE_NEW
                                            StandardOpenOption/WRITE
                                            StandardOpenOption/SYNC]))
            {:ok true :created? true :path (str target)
             :certificate/id certificate-id}
            (catch FileAlreadyExistsException _
              (try
                (let [existing (edn/read-string
                                (Files/readString target StandardCharsets/UTF_8))]
                  (if (= certificate-value existing)
                    {:ok true :created? false :path (str target)
                     :certificate/id certificate-id}
                    {:ok false
                     :error/code :campaign-certificate-identity-conflict
                     :path (str target) :certificate/id certificate-id}))
                (catch Throwable t
                  {:ok false :error/code :campaign-certificate-existing-unreadable
                   :path (str target) :finding {:message (.getMessage t)}})))))
        (catch Throwable t
          {:ok false :error/code :campaign-certificate-write-failed
           :finding {:message (.getMessage t)}})))))

(defn read-certificate [path]
  (try
    (let [path (directory-path path)]
      (if-not (and path (Files/isRegularFile path (make-array LinkOption 0)))
        {:ok false :error/code :campaign-certificate-not-found}
        (let [value (edn/read-string
                     (Files/readString path StandardCharsets/UTF_8))]
          (if (= (:certificate/id value)
                 (:certificate/id
                  (build-certificate (dissoc value :certificate/id))))
            {:ok true :certificate value}
            {:ok false :error/code :campaign-certificate-content-mismatch}))))
    (catch Throwable t
      {:ok false :error/code :campaign-certificate-read-failed
       :finding {:message (.getMessage t)}})))
