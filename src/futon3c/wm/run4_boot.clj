(ns futon3c.wm.run4-boot
  "Server-owned, disabled-by-default RUN4 boot configuration.

  The deployment template and credential location are fixed here rather than
  accepted from an HTTP request.  Enabling this adapter only constructs the
  handler configuration; it does not submit a series step."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.mission-registry :as missions]
            [futon3c.wm.guardrails :as guardrails]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-effective-environment :as effective]
            [futon3c.wm.run4-pinned-run-config :as pinned])
  (:import (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute PosixFilePermission)))

(def template-path
  "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-U88-deployment-2026-09-10/server-config.disabled.edn")

(def credential-path "/home/joe/.config/futon3c/run4/u88.bearer")
(def code-root "/home/joe/code")

(def ^:private credential-permissions
  #{PosixFilePermission/OWNER_READ PosixFilePermission/OWNER_WRITE})

(defn- refuse [reason]
  (throw (ex-info "RUN4 boot configuration refused" {:reason reason})))

(defn- canonical-path [path]
  (.toRealPath (.toPath (io/file path)) (make-array LinkOption 0)))

(defn- production-secret
  "Read the fixed credential only after its path, type, owner and 0600 mode
  have been checked. The returned value is never included in diagnostics."
  []
  (try
    (let [expected (.normalize (.toAbsolutePath (.toPath (io/file credential-path))))
          ^Path actual (canonical-path credential-path)
          owner (str (Files/getOwner actual (make-array LinkOption 0)))
          current (System/getProperty "user.name")]
      (when-not (and (= expected actual)
                     (Files/isRegularFile actual (make-array LinkOption 0))
                     (= current owner)
                     (= credential-permissions (Files/getPosixFilePermissions
                                                actual (make-array LinkOption 0))))
        (refuse :credential-file-authority-invalid))
      (str/trim (Files/readString actual)))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse :credential-file-unavailable))))

(defn- mission-resolver []
  (fn [mission-id]
    (some #(when (= mission-id (:id %)) %)
          (:missions (missions/load-missions code-root)))))

(defn- action-admissible [mission action]
  (let [mission-id (:id mission)]
    (and (string? mission-id)
         (= mission-id (:target action))
         (guardrails/autonomous-admissible?
          action
          {:mission-status-fn
           (fn [target]
             (if (= target mission-id)
               {:open? (contains? #{:open :active :partial :identify}
                                  (:status-class mission))
                :open-hole-count (:open-hole-count mission)}
               {:open? false :open-hole-count 0}))}))))

(defn- one-form [text]
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [value (edn/read {:eof ::empty} r)]
        (when (or (= ::empty value)
                  (not= ::end (edn/read {:eof ::end} r)))
          (refuse :startup-attestation-source-invalid))
        value))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse :startup-attestation-source-invalid))))

(defn- startup-attest!
  "Read the reviewed task/config chain and attest current environment against
  values already captured by loaded consumers. No namespace is loaded here."
  []
  (let [template (one-form (slurp template-path))
        root (:authority-root template)
        allowlist (into (:source-allowlist template) (:pin-allowlist template))
        read-text (fn [ref]
                    (when-not (contains? allowlist ref)
                      (refuse :startup-attestation-source-refused))
                    (let [base (.getCanonicalFile (io/file root))
                          file (.getCanonicalFile (io/file base ref))]
                      (when-not (and (.startsWith (.toPath file) (.toPath base))
                                     (.isFile file))
                        (refuse :startup-attestation-source-refused))
                      (slurp file)))
        pin-ref (first (:pin-allowlist template))
        task-pin (one-form (read-text pin-ref))
        opts (pinned/load! (:config task-pin) read-text)]
    (effective/attest (:run4/serving-declaration opts))))

(defn materialize
  "Construct the optional `{:run4 ...}` handler fragment.

  PORTS is an internal test seam, never request data. Disabled mode is byte/value
  compatible with the old handler options and performs no template, secret or
  mission reads."
  ([enabled?] (materialize enabled? {}))
  ([enabled? {:keys [read-template read-secret resolve-mission admissible? attest!]
              :or {read-template #(slurp template-path)
                   read-secret production-secret
                   admissible? action-admissible
                   attest! startup-attest!}}]
   (when-not (boolean? enabled?) (refuse :activation-not-boolean))
   (if-not enabled?
     {}
     (let [resolver (or resolve-mission (mission-resolver))
           config (deployment/materialize
                   (read-template)
                   {:credential read-secret
                    :resolve-mission resolver
                    :action-admissible? admissible?
                    :enable? true})]
       (attest!)
       config))))
