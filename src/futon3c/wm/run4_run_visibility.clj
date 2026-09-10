(ns futon3c.wm.run4-run-visibility
  "Read-only projection of already durable RUN4 evidence into Voxterm's
  wm/run-visibility-v1 contract.  This is observation, never acceptance."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest])
  (:import (java.nio.channels FileChannel)
           (java.nio.file Files StandardCopyOption StandardOpenOption)))

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 visibility refused"
                  (merge {:error :run4-visibility-refused :reason reason} data))))

(defn- parse-one-file [f kind]
  (when (.exists f)
    (let [text (try (String. (Files/readAllBytes (.toPath f)) "UTF-8")
                    (catch Throwable _ (refuse! :unreadable {:kind kind})))]
      (try
        (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
          (let [v (edn/read {:eof ::empty} r)]
            (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
              (refuse! :malformed {:kind kind}))
            (when-not (map? v) (refuse! :malformed {:kind kind}))
            v))
        (catch clojure.lang.ExceptionInfo e (throw e))
        (catch Throwable _ (refuse! :malformed {:kind kind}))))))

(defn- parse-one-text [text kind]
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [v (edn/read {:eof ::empty} r)]
        (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
          (refuse! :malformed {:kind kind}))
        v))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! :malformed {:kind kind}))))

(defn- exact? [m ks] (and (map? m) (= ks (set (keys m)))))
(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- instant? [x] (and (string? x) (try (java.time.Instant/parse x) true (catch Throwable _ false))))

(defn- identity-ok? [event manifest manifest-sha trial]
  (= [(:series-id manifest) manifest-sha (:ordinal trial) (:trial-id trial)
      (:attempt-id trial) (:pin-sha256 trial)]
     [(:series-id event) (:manifest-sha256 event) (:ordinal event) (:trial-id event)
      (:attempt-id event) (:pin-sha256 event)]))

(defn- started! [v manifest manifest-sha trial]
  (when-not (and (exact? v #{:schema :series-id :manifest-sha256 :ordinal :trial-id
                             :attempt-id :pin-sha256 :click-id :started-at :admission-state})
                 (= :wm/run4-series-started-v1 (:schema v))
                 (identity-ok? v manifest manifest-sha trial) (nonblank? (:click-id v))
                 (instant? (:started-at v)) (= :click-recorded (:admission-state v)))
    (refuse! :invalid-started {:ordinal (:ordinal trial)}))
  v)

(defn- terminal! [v manifest manifest-sha trial]
  (let [attempted? (not= :not-attempted (:task-result v))
        ks (if attempted?
             #{:schema :series-id :manifest-sha256 :ordinal :trial-id :attempt-id
               :pin-sha256 :task-result :infrastructure :evidence-id}
             #{:schema :series-id :manifest-sha256 :ordinal :trial-id :attempt-id
               :pin-sha256 :task-result :infrastructure :reason})]
    (when-not (and (exact? v ks) (= :wm/run4-series-terminal-v1 (:schema v))
                   (identity-ok? v manifest manifest-sha trial)
                   (if attempted?
                     (and (contains? #{:succeeded :failed :blocked} (:task-result v))
                          (contains? #{:safe :unsafe} (:infrastructure v))
                          (nonblank? (:evidence-id v)))
                     (and (= :unsafe (:infrastructure v))
                          (contains? #{:busy-admission-rejected :prior-infrastructure-stop}
                                     (:reason v)))))
      (refuse! :invalid-terminal {:ordinal (:ordinal trial)}))
    v))

(defn- trial-view [manifest manifest-sha root terminal-evidence observed-at trial lifecycle-row]
  (let [prefix (format "%03d" (:ordinal trial))
        started (or (:started lifecycle-row)
                    (some-> (parse-one-file (io/file root (str prefix "-started.edn")) :started)
                            (started! manifest manifest-sha trial)))
        terminal (or (:terminal lifecycle-row)
                     (some-> (parse-one-file (io/file root (str prefix "-terminal.edn")) :terminal)
                             (terminal! manifest manifest-sha trial)))
        evidence (when started (terminal-evidence started))
        _ (when (and terminal (not= :not-attempted (:task-result terminal))
                     (not= (select-keys terminal [:task-result :infrastructure :evidence-id]) evidence))
            (refuse! :terminal-evidence-conflict {:ordinal (:ordinal trial)}))
        [stage result reason]
        (cond
          (and (= :not-attempted (:task-result terminal)) (nil? lifecycle-row))
          (refuse! :unsupported-not-attempted-join {:ordinal (:ordinal trial)})
          (= :busy-admission-rejected (:reason terminal))
          ["blocked" "blocked" "busy admission rejected"]
          (= :prior-infrastructure-stop (:reason terminal))
          ["blocked" "blocked" "prior unsafe infrastructure stop"]
          (= :succeeded (:task-result evidence)) ["complete" "passed" nil]
          (= :failed (:task-result evidence)) ["failed" "failed" nil]
          (= :blocked (:task-result evidence)) ["blocked" "blocked" "unsafe infrastructure"]
          terminal (refuse! :terminal-without-authoritative-evidence {:ordinal (:ordinal trial)})
          evidence ["review" "pending" nil]
          started ["working" "pending" nil]
          :else ["planned" "pending" nil])]
    (cond-> {:trial_id (str (:trial-id trial)) :stage stage :updated_at observed-at
             :worker (get-in manifest [:casting :author])
             :reviewer (get-in manifest [:casting :reviewer]) :result result}
      reason (assoc :blocked_reason reason))))

(defn observe
  "Return nil when no enacted series-open evidence exists. MANIFEST-TEXT and its
  digest are frozen authority; TERMINAL-EVIDENCE must be the strict joined port."
  ([root manifest-text terminal-evidence observed-at]
   (observe root manifest-text terminal-evidence observed-at nil))
  ([root manifest-text terminal-evidence observed-at lifecycle]
  (when-not (and (string? root) (string? manifest-text) (fn? terminal-evidence)
                 (instant? observed-at))
    (refuse! :invalid-input))
  (let [manifest (parse-one-text manifest-text :manifest)
        manifest-sha (digest/sha256 manifest-text)
        open (parse-one-file (io/file root "series.edn") :series-open)]
    (when open
      (when-not (and (exact? open #{:schema :series-id :manifest-sha256 :trial-count})
                     (= :wm/run4-series-open-v1 (:schema open))
                     (= (:series-id manifest) (:series-id open))
                     (= manifest-sha (:manifest-sha256 open))
                     (= (count (:trials manifest)) (:trial-count open))
                     (seq (:trials manifest)))
        (refuse! :series-open-conflict))
      (when (and lifecycle
                 (not= [(:series-id manifest) manifest-sha]
                       [(:series-id lifecycle) (:manifest-sha256 lifecycle)]))
        (refuse! :lifecycle-identity-mismatch))
      (let [rows (into {} (map (juxt :ordinal identity) (:trials lifecycle)))
            trials (mapv #(trial-view manifest manifest-sha root terminal-evidence observed-at %
                                     (get rows (:ordinal %)))
                         (:trials manifest))
            stages (set (map :stage trials))
            [stage result reason]
            (cond
              (stages "blocked") ["blocked" "blocked" "RUN4 evidence requires attention"]
              (stages "working") ["working" "pending" nil]
              (stages "review") ["review" "pending" nil]
              (stages "planned") ["planned" "pending" nil]
              (stages "failed") ["failed" "failed" nil]
              :else ["complete" "passed" nil])]
        (cond-> {:schema "wm/run-visibility-v1" :run_id (str (:series-id manifest))
                 :stage stage :updated_at observed-at
                 :worker (get-in manifest [:casting :author])
                 :reviewer (get-in manifest [:casting :reviewer])
                 :result result :trials trials}
          reason (assoc :blocked_reason reason)))))))

(defn publish!
  "Atomically publish a derived JSON observation. Nil observation writes nothing."
  [target observation]
  (when observation
    (let [target (.getCanonicalFile (io/file target))
          parent (.getParentFile target)
          tmp (io/file parent (str "." (.getName target) ".tmp-" (java.util.UUID/randomUUID)))
          bytes (.getBytes (str (json/generate-string observation) "\n") "UTF-8")]
      (when-not (.isDirectory parent) (refuse! :invalid-output-root))
      (try
        (with-open [ch (FileChannel/open (.toPath tmp)
                                        (into-array StandardOpenOption
                                                    [StandardOpenOption/CREATE_NEW
                                                     StandardOpenOption/WRITE]))]
          (let [buffer (java.nio.ByteBuffer/wrap bytes)]
            (while (.hasRemaining buffer) (.write ch buffer)))
          (.force ch true))
        (Files/move (.toPath tmp) (.toPath target)
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
        (with-open [ch (FileChannel/open (.toPath parent)
                                        (make-array StandardOpenOption 0))]
          (.force ch true))
        observation
        (finally (Files/deleteIfExists (.toPath tmp)))))))
