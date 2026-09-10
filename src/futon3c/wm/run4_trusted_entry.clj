(ns futon3c.wm.run4-trusted-entry
  "Server-owned authentication and pre-click validation for one RUN4 pin.

  The bearer credential is deployment configuration: exactly 64 lowercase
  hexadecimal characters (a 256-bit value). Request JSON cannot provide
  credentials, trust callbacks, or filesystem ports."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.run4-task-pin :as task-pin])
  (:import [java.security MessageDigest]
           [java.util UUID]))

(def allowed-request-keys
  #{:run4-pin-ref :author :reviewer :repair-reviewer :trigger})
(def casting-keys [:author :reviewer :repair-reviewer])

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

(defn- validate-pin [cfg pin-text]
  (let [read-text (fn [ref]
                    (if-let [f (authorized-file (:source-root cfg)
                                                (:source-allowlist cfg) ref)]
                      (slurp f)
                      (throw (ex-info "RUN4 source refused"
                                      {:reason :source-reference-refused
                                       :ref ref}))))
        ports {:read-text read-text
               :resolve-mission (:resolve-mission cfg)
               :action-admissible? (:action-admissible? cfg)}]
    (try
      {:envelope (task-pin/validate pin-text ports) :ports ports}
      (catch clojure.lang.ExceptionInfo e
        {:refusal (refuse :run4-pin-invalid
                          {:reason (:reason (ex-data e))})})
      (catch Throwable _
        {:refusal (refuse :run4-pin-invalid
                          {:reason :validation-failed})}))))

(defn- prepared-options [cfg pin-text envelope ports]
  (let [pin-sha (digest/sha256 pin-text)
        used? (atom false)
        trust (fn [{:keys [pin-digest operator-selection]}]
                (when-not (and (= pin-sha pin-digest)
                               (= "Joe" (:operator operator-selection))
                               (compare-and-set! used? false true))
                  (throw (ex-info "RUN4 attestation refused"
                                  {:reason :digest-operator-or-reuse})))
                {:status :authenticated
                 :boundary :trusted-serving-context
                 :principal "Joe"
                 :pin-sha256 pin-sha
                 :request-nonce (str (UUID/randomUUID))})]
    (cond
      (not= "Joe" (get-in envelope [:operator-selection :operator]))
      (refuse :run4-operator-mismatch)
      (not= (:casting cfg) (:casting envelope))
      (refuse :run4-casting-mismatch)
      :else
      {:ok true
       :opts (merge (:casting cfg)
                    {:run4-task-pin-text pin-text
                     :run4-task-pin-ports ports
                     :run4-trusted-boundary-fn trust})})))

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
            (prepared-options cfg pin-text (:envelope validation)
                              (:ports validation))))
        (refuse :run4-pin-reference-refused)))))
