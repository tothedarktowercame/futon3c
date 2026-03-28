(ns futon3c.dev.invoke
  "Invoke evidence emission, artifact writing, and delivery recording.

   Extracted from futon3c.dev (Phase 1 of TN-dev-clj-decomposition).
   The evidence store atom is passed in or injected — this namespace
   does not own runtime state."
  (:require [futon3c.evidence.store :as estore]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.agents.mfuton-invoke-override :as mfuton-invoke-override]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [futon3c.dev.config :as config]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]))

;; ---------------------------------------------------------------------------
;; Evidence store construction
;; ---------------------------------------------------------------------------

(defn direct-xtdb-enabled?
  "Whether futon3c may write evidence directly to XTDB.
   On the Linode role this defaults to true (same JVM as futon1a).
   Explicit env var FUTON3C_DIRECT_XTDB always overrides."
  [role-cfg]
  (config/env-bool "FUTON3C_DIRECT_XTDB" (:direct-xtdb? role-cfg false)))

(defn make-evidence-store
  "Build the evidence store based on direct-xtdb? flag.
   When true, uses shared XTDB node (evidence persists to futon1a).
   When false, uses in-memory atom (evidence lost on restart)."
  [f1-sys direct-xtdb?]
  (if direct-xtdb?
    (do
      (println "[dev] direct XTDB path: ENABLED")
      (xb/make-xtdb-backend (:node f1-sys)))
    (do
      (println "[dev] direct XTDB path: disabled (using in-memory evidence store)")
      (atom {:entries {} :order []}))))

;; ---------------------------------------------------------------------------
;; Invoke evidence emission
;; ---------------------------------------------------------------------------

(defn emit-invoke-evidence!
  "Emit an invoke lifecycle evidence entry. Fire-and-forget.
   Uses the provided evidence store — no HTTP round-trip."
  [evidence-store agent-id event-type body-map & {:keys [session-id tags]}]
  (when evidence-store
    (future
      (try
        (estore/append* evidence-store
                        {:subject {:ref/type "agent" :ref/id agent-id}
                         :type :coordination
                         :claim-type :step
                         :author agent-id
                         :session-id (or session-id "dev-invoke")
                         :body (assoc body-map
                                      "event" event-type
                                      "agent-id" agent-id
                                      "at" (str (Instant/now)))
                         :tags (into ["invoke" "dev" agent-id]
                                     (or tags []))})
        (catch Exception e
          (println (str "[dev] evidence emit error: " (.getMessage e))))))))

;; ---------------------------------------------------------------------------
;; Artifact helpers
;; ---------------------------------------------------------------------------

(defn sanitize-file-fragment
  "Normalize text for use in local artifact filenames."
  [s fallback]
  (let [raw (or (some-> s str str/trim not-empty) fallback "unknown")
        cleaned (-> raw
                    (str/replace #"[^A-Za-z0-9._-]" "_")
                    (str/replace #"_+" "_"))]
    (if (str/blank? cleaned) fallback cleaned)))

(defn invoke-result-kind
  "Classify invoke result for trace metadata."
  [text]
  (let [raw (str (or text ""))
        trimmed (str/trim raw)]
    (cond
      (str/blank? trimmed) :empty
      (re-find #"^[\{\[]" trimmed) :structured
      :else :text)))

(defn sha256-hex
  "Hex SHA-256 for TEXT."
  [text]
  (let [^java.security.MessageDigest md (java.security.MessageDigest/getInstance "SHA-256")
        bytes (.digest md (.getBytes (str text) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

(defn escape-elisp-string
  "Escape a string for embedding in an elisp double-quoted string."
  [s]
  (-> (str (or s ""))
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn format-delivery-receipt-line
  [invoke-trace-id {:keys [surface destination delivered? note]}]
  (let [status (if (false? delivered?) "failed" "delivered")
        note* (some-> note str str/trim not-empty)
        note* (when note*
                (if (> (count note*) 120)
                  (str (subs note* 0 117) "...")
                  note*))]
    (str "Delivery: " status
         " via " (or (some-> surface str str/trim not-empty) "unknown")
         " -> " (or (some-> destination str str/trim not-empty) "unknown")
         " (trace-id " invoke-trace-id ")"
         (when note* (str " [" note* "]")))))

(defn invoke-meta-trace-id
  "Extract invoke trace id from invoke-meta maps with keyword or string keys."
  [invoke-meta]
  (some (fn [k]
          (let [v (or (get invoke-meta k)
                      (get invoke-meta (name k)))]
            (when-let [tid (some-> v str str/trim not-empty)]
              tid)))
        [:invoke-trace-id :invoke_trace_id :invokeTraceId]))

(defn write-invoke-artifact!
  "Persist full invoke output to a local artifact file.
   Returns absolute file path on success, nil on failure or blank text."
  [agent-id session-id text]
  (let [payload (str (or text ""))]
    (when-not (str/blank? (str/trim payload))
      (try
        (let [root (io/file (or (some-> (config/env "FUTON3C_INVOKE_ARTIFACT_DIR") str/trim not-empty)
                                "/tmp/futon-invoke-artifacts"))
              _ (.mkdirs root)
              agent-frag (sanitize-file-fragment agent-id "agent")
              sid-frag (sanitize-file-fragment (some-> session-id (subs 0 (min 8 (count session-id))))
                                               "nosid")
              file (io/file root (format "%s-%s-%d.txt"
                                         agent-frag
                                         sid-frag
                                         (System/currentTimeMillis)))]
          (spit file payload)
          (.getAbsolutePath file))
        (catch Exception e
          (println (str "[dev] invoke artifact write error: " (.getMessage e)))
          nil)))))

(defn invoke-trace-response-block
  "Build a compact invoke-trace footer for human-facing invoke boards."
  [agent-id session-id invoke-trace-id result-text]
  (let [payload (str (or result-text ""))
        kind (invoke-result-kind payload)
        chars (count payload)
        digest (when (pos? chars) (sha256-hex payload))
        artifact-path (write-invoke-artifact! agent-id session-id result-text)]
    (str "\nTrace: "
         (or invoke-trace-id "unknown")
         " | result=" (name kind)
         " | chars=" chars
         (when digest (str " | sha256=" digest))
         (when artifact-path (str "\nArtifact: " artifact-path))
         "\n")))

(defn write-invoke-delivery-artifact!
  "Persist a delivery receipt line to a local artifact file and return the path."
  [agent-id invoke-trace-id receipt-line]
  (let [payload (str (or receipt-line ""))]
    (when-not (str/blank? (str/trim payload))
      (try
        (let [root (io/file (or (some-> (config/env "FUTON3C_INVOKE_ARTIFACT_DIR") str/trim not-empty)
                                "/tmp/futon-invoke-artifacts"))
              _ (.mkdirs root)
              agent-frag (sanitize-file-fragment agent-id "agent")
              tid-frag (sanitize-file-fragment invoke-trace-id "trace")
              file (io/file root (format "%s-delivery-%s-%d.txt"
                                         agent-frag
                                         tid-frag
                                         (System/currentTimeMillis)))]
          (spit file payload)
          (.getAbsolutePath file))
        (catch Exception e
          (println (str "[dev] invoke delivery artifact write error: " (.getMessage e)))
          nil)))))

;; ---------------------------------------------------------------------------
;; Delivery recording (Emacs blackboard)
;; ---------------------------------------------------------------------------

(defn record-invoke-delivery!
  "Record where an invoke reply was actually delivered.
   mfuton-specific projection is handled by the mfuton invoke seam.
   Generic behavior appends/updates a delivery receipt line in *invoke: <agent>*."
  [agent-id invoke-trace-id receipt]
  (let [aid (some-> agent-id str str/trim)
        tid (some-> invoke-trace-id str str/trim)]
    (when (and (seq aid) (seq tid))
      (let [agent-record (reg/get-agent aid)
            emacs-socket (some-> agent-record :agent/metadata :emacs-socket str str/trim not-empty)
            bb-opts (cond-> {} emacs-socket (assoc :emacs-socket emacs-socket))
            buf-name (str "*invoke: " aid "*")
            pending-line (str "Delivery: pending (trace-id " tid ")")
            receipt-line (format-delivery-receipt-line tid receipt)]
        (if-some [handled? (mfuton-invoke-override/maybe-record-delivery!
                            {:agent-id aid
                             :invoke-trace-id tid
                             :receipt-line receipt-line})]
          handled?
          (let [receipt-artifact-path (write-invoke-delivery-artifact! aid tid receipt-line)
                elisp (str "(let ((buf (get-buffer \"" (escape-elisp-string buf-name) "\")))"
                           "(if (not buf) "
                           "\"missing-buffer\" "
                           "(with-current-buffer buf "
                           "(let ((inhibit-read-only t)) "
                           "(goto-char (point-min)) "
                           "(if (search-forward \"" (escape-elisp-string pending-line) "\" nil t) "
                           (if receipt-artifact-path
                             (str "(progn "
                                  "(delete-region (match-beginning 0) (match-end 0)) "
                                  "(insert-file-contents \"" (escape-elisp-string receipt-artifact-path) "\") "
                                  "\"replaced\") ")
                             (str "(progn (replace-match \"" (escape-elisp-string receipt-line) "\" t t) \"replaced\") "))
                           "(progn "
                           "(goto-char (point-max)) "
                           "(unless (bolp) (insert \"\\n\")) "
                           (if receipt-artifact-path
                             (str "(insert-file-contents \"" (escape-elisp-string receipt-artifact-path) "\") "
                                  "(insert \"\\n\") ")
                             (str "(insert \"" (escape-elisp-string receipt-line) "\\n\") "))
                           "\"appended\"))))))")]
            (loop [attempt 1]
              (let [{:keys [ok output]} (bb/blackboard-eval! elisp bb-opts)
                    out (some-> output str str/trim)
                    status (cond
                             (#{"\"replaced\"" "replaced"} out) :replaced
                             (#{"\"appended\"" "appended"} out) :appended
                             (#{"\"missing-buffer\"" "missing-buffer"} out) :missing-buffer
                             :else :unknown)
                    success? (and ok (#{:replaced :appended} status))]
                (cond
                  success?
                  true

                  ;; Buffer doesn't exist — expected for WS-bridged agents
                  ;; where invoke buffer lives on a different machine.
                  (= :missing-buffer status)
                  false

                  (< attempt 3)
                  (do
                    (Thread/sleep (* 120 attempt))
                    (recur (inc attempt)))

                  :else
                  (do
                    (println (str "[invoke-delivery] failed for " aid
                                  " trace-id=" tid
                                  " status=" (name status)
                                  " ok=" ok
                                  " output=" out))
                    (flush)
                    false))))))))))
