(ns futon3c.wm.run4-trusted-entry
  "Server-owned authentication and pre-click validation for one RUN4 pin.

  The bearer credential is deployment configuration: exactly 64 lowercase
  hexadecimal characters (a 256-bit value). Request JSON cannot provide
  credentials, trust callbacks, or filesystem ports."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.run4-task-pin :as task-pin]
            [futon3c.wm.run4-effective-environment :as effective]
            [futon3c.wm.run4-pinned-run-config :as pinned-config])
  (:import [java.security MessageDigest]
           [java.util UUID]))

(def allowed-request-keys
  #{:run4-pin-ref :run4-attempt-id :author :reviewer :repair-reviewer :trigger})
(def casting-keys [:author :reviewer :repair-reviewer])

(def ^:dynamic *attest-effective-environment*
  "Test seam only; production reads the current process environment and loaded
  consumer Vars through run4-effective-environment/attest."
  effective/attest)

(defn- refuse [code & [data]]
  (merge {:ok false :status 403 :error code} data))

(defn- secure= [a b]
  (and (string? a) (string? b)
       (MessageDigest/isEqual (.getBytes a "UTF-8") (.getBytes b "UTF-8"))))

(defn- credential? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{64}" value))))

(defn- casting? [value]
  (and (map? value)
       (every? #(and (string? %) (not (str/blank? %)))
               (map value casting-keys))
       (not= (:author value) (:reviewer value))))

(defn- relative-ref? [value]
  (and (string? value) (not (str/blank? value))
       (not (.isAbsolute (io/file value)))))

(defn- file-authority? [root allowlist]
  (and (string? root) (not (str/blank? root))
       (.isDirectory (io/file root))
       (set? allowlist) (seq allowlist) (every? relative-ref? allowlist)))

(defn- config-error [cfg]
  (cond
    (not (true? (:enabled? cfg))) :run4-credential-configuration-invalid
    (not= "Joe" (:operator cfg)) :run4-credential-configuration-invalid
    (not (credential? (:bearer-token cfg))) :run4-credential-configuration-invalid
    (not (casting? (:casting cfg))) :run4-casting-configuration-invalid
    (not (and (fn? (:resolve-mission cfg))
              (fn? (:action-admissible? cfg)))) :run4-port-configuration-invalid
    (not (and (file-authority? (:pin-root cfg) (:pin-allowlist cfg))
              (file-authority? (:source-root cfg) (:source-allowlist cfg))))
    :run4-file-authority-invalid
    (not (and (string? (:admission-root cfg))
              (.isDirectory (io/file (:admission-root cfg)))))
    :run4-admission-configuration-invalid
    :else nil))

(defn- authorized-file [root allowlist ref]
  (when (and (relative-ref? ref) (contains? allowlist ref))
    (let [base (.getCanonicalFile (io/file root))
          file (.getCanonicalFile (io/file base ref))]
      (when (and (.startsWith (.toPath file) (.toPath base)) (.isFile file)) file))))

(defn- bearer-token [headers]
  (let [header (get headers "authorization")]
    (when (string? header)
      (second (re-matches #"Bearer ([0-9a-f]{64})" header)))))

(defn- requested-casting [payload]
  (select-keys payload casting-keys))

(defn- attempt-id? [value]
  (and (string? value)
       (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" value))))

(defn- validate-pin [cfg pin-text]
  (let [snapshots (atom {})
        fresh-read-text
        (fn [ref]
          (if-let [f (authorized-file (:source-root cfg)
                                      (:source-allowlist cfg) ref)]
            (slurp f)
            (throw (ex-info "RUN4 source refused"
                            {:reason :source-reference-refused :ref ref}))))
        snapshot-read-text (fn [ref]
                    (if (contains? @snapshots ref)
                      (get @snapshots ref)
                      (let [text (fresh-read-text ref)]
                        (swap! snapshots assoc ref text)
                        text)))
        base-ports {:resolve-mission (:resolve-mission cfg)
                    :action-admissible? (:action-admissible? cfg)}
        validation-ports (assoc base-ports :read-text snapshot-read-text)
        runner-ports (assoc base-ports :read-text fresh-read-text)
        freshness! (fn []
                     (doseq [[ref captured] @snapshots]
                       (when-not (= captured (fresh-read-text ref))
                         (throw (ex-info "RUN4 captured source changed"
                                         {:reason :captured-source-changed
                                          :ref ref})))))]
    (try
      {:envelope (task-pin/validate pin-text validation-ports)
       :ports runner-ports
       :snapshot-read-text snapshot-read-text
       :freshness! freshness!}
      (catch clojure.lang.ExceptionInfo e
        {:refusal (refuse :run4-pin-invalid
                          {:reason (:reason (ex-data e))})})
      (catch Throwable _
        {:refusal (refuse :run4-pin-invalid
                          {:reason :validation-failed})}))))

(defn- prepared-options [cfg pin-text envelope ports runner-opts freshness! attempt-id]
  (let [pin-sha (digest/sha256 pin-text)
        serving-declaration (:run4/serving-declaration runner-opts)
        ;; A first current-value check happens before click creation. The
        ;; callback repeats it at the selector boundary to close prepare/use
        ;; drift without treating this snapshot as authority.
        _ (*attest-effective-environment* serving-declaration)
        used? (atom false)
        trust (fn [{:keys [pin-digest operator-selection]}]
                ;; The task validator below also rereads pin-declared sources.
                ;; This check additionally covers materialized config artifacts
                ;; (for example C-fold seed/kernel bytes) captured at prepare.
                (freshness!)
                (when-not (and (= pin-sha pin-digest)
                               (= "Joe" (:operator operator-selection))
                               (compare-and-set! used? false true))
                  (throw (ex-info "RUN4 attestation refused"
                                  {:reason :digest-operator-or-reuse})))
                (let [environment (*attest-effective-environment*
                                   serving-declaration)]
                  {:status :authenticated
                   :boundary :trusted-serving-context
                   :principal "Joe"
                   :pin-sha256 pin-sha
                   :request-nonce (str (UUID/randomUUID))
                   :effective-environment environment}))]
    (cond
      (not= "Joe" (get-in envelope [:operator-selection :operator]))
      (refuse :run4-operator-mismatch)
      (not= (:casting cfg) (:casting envelope))
      (refuse :run4-casting-mismatch)
      :else
      {:ok true
       :opts (merge runner-opts (:casting cfg)
                    {:run4-task-pin-text pin-text
                     :run4-task-pin-ports ports
                     :run4-trusted-boundary-fn trust})
       :admission-request
       {:attempt-id attempt-id
        :identity {:series-id (get-in envelope [:task-pin :series-id])
                   :trial-id (get-in envelope [:task-pin :trial-id])
                   :pin-sha256 pin-sha
                   :casting (:casting cfg)}}})))

(defn prepare
  "Authenticate and fully validate a RUN4 payload before click creation.

  Validation uses only server-configured read ports. The returned callback
  rechecks the exact digest in the runner and is one-use; it is not HTTP
  request replay protection."
  [server-config headers payload]
  (let [cfg (:run4 server-config)]
    (cond
      (not (map? cfg)) (refuse :run4-disabled)
      (config-error cfg) (refuse (config-error cfg))
      (not (map? payload)) (refuse :run4-request-invalid)
      (seq (remove allowed-request-keys (keys payload)))
      (refuse :run4-request-key-forbidden)
      (not (secure= (:bearer-token cfg) (bearer-token headers)))
      (refuse :run4-authentication-failed)
      (not (attempt-id? (:run4-attempt-id payload)))
      (refuse :run4-attempt-identity-invalid)
      (and (seq (requested-casting payload))
           (not= (requested-casting payload)
                 (select-keys (:casting cfg) (keys (requested-casting payload)))))
      (refuse :run4-casting-mismatch)
      :else
      (if-let [pin-file (authorized-file (:pin-root cfg) (:pin-allowlist cfg)
                                         (:run4-pin-ref payload))]
        (let [pin-text (slurp pin-file)
              validation (validate-pin cfg pin-text)]
          (if-let [refusal (:refusal validation)]
            refusal
            (let [loaded
                  (try
                    {:opts
                     (pinned-config/load! (get-in validation [:envelope :config-pin])
                                          (:snapshot-read-text validation))}
                    (catch clojure.lang.ExceptionInfo e
                      {:refusal
                       (refuse :run4-pinned-config-invalid
                               {:reason (:reason (ex-data e))})}))]
              (or (:refusal loaded)
                  (prepared-options cfg pin-text (:envelope validation)
                                    (:ports validation) (:opts loaded)
                                    (:freshness! validation)
                                    (:run4-attempt-id payload))))))
        (refuse :run4-pin-reference-refused)))))
