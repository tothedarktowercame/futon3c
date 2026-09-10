(ns futon3c.wm.run4-trusted-entry
  "Server-owned authentication boundary for one RUN4 pinned click."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest])
  (:import [java.security MessageDigest]
           [java.util UUID]))

(def allowed-request-keys
  #{:run4-pin-ref :author :reviewer :repair-reviewer :trigger})

(defn- refuse [code] {:ok false :status 403 :error code})

(defn- secure= [a b]
  (and (string? a) (string? b)
       (MessageDigest/isEqual (.getBytes a "UTF-8") (.getBytes b "UTF-8"))))

(defn- configured? [{:keys [enabled? bearer-token operator]}]
  (and enabled? (= "Joe" operator) (string? bearer-token)
       (>= (count bearer-token) 32)
       (not (contains? #{"change-me" "placeholder"} bearer-token))))

(defn- authorized-file [root allowlist ref]
  (when (and (string? root) (string? ref) (contains? (set allowlist) ref))
    (let [base (.getCanonicalFile (io/file root))
          file (.getCanonicalFile (io/file base ref))]
      (when (and (.startsWith (.toPath file) (.toPath base)) (.isFile file)) file))))

(defn prepare
  "Authenticate a RUN4 payload and return only server-derived runner options."
  [server-config headers payload]
  (let [cfg (:run4 server-config)]
    (cond
      (not (map? cfg)) (refuse :run4-disabled)
      (not (configured? cfg)) (refuse :run4-credential-configuration-invalid)
      (not= (set (keys payload))
            (set (filter #(contains? payload %) allowed-request-keys)))
      (refuse :run4-request-key-forbidden)
      (seq (remove allowed-request-keys (keys payload)))
      (refuse :run4-request-key-forbidden)
      :else
      (let [supplied (some-> (get headers "authorization")
                             (str/replace-first #"^Bearer " ""))]
        (if-not (secure= (:bearer-token cfg) supplied)
          (refuse :run4-authentication-failed)
          (if-let [pin-file (authorized-file (:pin-root cfg) (:pin-allowlist cfg)
                                             (:run4-pin-ref payload))]
            (let [pin-text (slurp pin-file)
                  pin-sha (digest/sha256 pin-text)
                  used? (atom false)
                  read-text (fn [ref]
                              (if-let [f (authorized-file (:source-root cfg)
                                                          (:source-allowlist cfg) ref)]
                                (slurp f)
                                (throw (ex-info "RUN4 source refused" {:ref ref}))))
                  trust (fn [{:keys [pin-digest operator-selection]}]
                          (when-not (and (= pin-sha pin-digest)
                                         (= "Joe" (:operator operator-selection))
                                         (compare-and-set! used? false true))
                            (throw (ex-info "RUN4 attestation refused"
                                            {:reason :digest-operator-or-reuse})))
                          {:status :authenticated :boundary :trusted-serving-context
                           :principal "Joe" :pin-sha256 pin-sha
                           :request-nonce (str (UUID/randomUUID))})]
              {:ok true
               :opts {:run4-task-pin-text pin-text
                      :run4-task-pin-ports {:read-text read-text
                                            :resolve-mission (:resolve-mission cfg)
                                            :action-admissible? (:action-admissible? cfg)}
                      :run4-trusted-boundary-fn trust}})
            (refuse :run4-pin-reference-refused)))))))
