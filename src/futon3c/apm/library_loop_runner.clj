(ns futon3c.apm.library-loop-runner
  "Files-only durable core for the long-lived Library Loop runner.

  This namespace owns state transitions and durable receipts, but deliberately
  knows nothing about Codex, Lean, review transport, or Git banking.  Callers
  persist an intent with `begin-action!`, perform (or idempotently activate)
  the external action, append its receipt, and call `reconcile!`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io FileOutputStream PushbackReader)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files Path StandardCopyOption StandardOpenOption)
           (java.security MessageDigest)
           (java.util UUID)))

(def schema-version 1)

(def phases
  #{:turn-ready :turn-running :gating :checkpoint-ready
    :review-pending :bank-pending :paused})

(def intent-phase
  {:turn :turn-running
   :gate :gating
   :review :review-pending
   :bank :bank-pending})

(def allowed-transitions
  "The runner's complete phase graph. `:paused` is intentionally absent as a
  source: leaving it requires a later packet's explicit review disposition."
  {:turn-ready #{:turn-running :paused}
   :turn-running #{:gating :paused}
   :gating #{:turn-ready :checkpoint-ready :paused}
   :checkpoint-ready #{:review-pending :paused}
   :review-pending #{:bank-pending :paused}
   :bank-pending #{:turn-ready :paused}})

(defn- fail! [finding data]
  (throw (ex-info (name finding) (assoc data :finding finding))))

(defn- sha256 [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str s) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn initial-state
  [{:keys [problem-id workspace base-sha head-sha]}]
  {:schema schema-version
   :problem-id problem-id
   :phase :turn-ready
   :turn 1
   :checkpoint 0
   :workspace (str workspace)
   :base-sha base-sha
   :head-sha head-sha
   :last-green-gate nil
   :failure-fingerprint nil
   :consecutive-same-failures 0
   :pending-bank nil
   :intent nil})

(defn validate-state
  "Returns state or throws with a machine-readable `:finding`."
  [state]
  (when-not (= schema-version (:schema state))
    (fail! :unsupported-schema {:state state}))
  (when-not (and (string? (:problem-id state))
                 (not-empty (:problem-id state)))
    (fail! :invalid-problem-id {:state state}))
  (when-not (contains? phases (:phase state))
    (fail! :invalid-phase {:state state}))
  (when-not (and (pos-int? (:turn state))
                 (nat-int? (:checkpoint state))
                 (string? (:workspace state))
                 (string? (:base-sha state))
                 (string? (:head-sha state)))
    (fail! :invalid-state-fields {:state state}))
  (let [intent (:intent state)]
    (when (and intent
               (not (and (map? intent)
                         (keyword? (:action intent))
                         (string? (:id intent))
                         (= (:problem-id state) (:problem-id intent))
                         (= (:turn state) (:turn intent)))))
      (fail! :invalid-intent {:state state}))
    (when (and intent
               (not= (:phase state) (intent-phase (:action intent))))
      (fail! :intent-phase-mismatch {:state state})))
  state)

(defn state-path [run-dir]
  (.toPath (io/file run-dir "state.edn")))

(defn- fsync-directory! [^Path path]
  (with-open [channel (java.nio.channels.FileChannel/open
                       path (into-array StandardOpenOption
                                        [StandardOpenOption/READ]))]
    (.force channel true)))

(defn write-state!
  "Atomically replaces state.edn using temp-write, file fsync, rename, and
  directory fsync.  The temp file is in the destination directory."
  [run-dir state]
  (validate-state state)
  (let [dir (.toPath (io/file run-dir))
        target (state-path run-dir)
        temp (.resolve dir (str ".state.edn." (UUID/randomUUID) ".tmp"))
        bytes (.getBytes (str (pr-str state) "\n") StandardCharsets/UTF_8)]
    (Files/createDirectories dir (make-array java.nio.file.attribute.FileAttribute 0))
    (try
      (with-open [out (FileOutputStream. (.toFile temp))]
        (.write out bytes)
        (.flush out)
        (.sync (.getFD out)))
      (Files/move temp target
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (fsync-directory! dir)
      state
      (finally
        (Files/deleteIfExists temp)))))

(defn read-state [run-dir]
  (with-open [reader (PushbackReader. (io/reader (.toFile (state-path run-dir))))]
    (validate-state (edn/read {:eof nil} reader))))

(defn receipt-path
  "Receipt names are stable, phase/action scoped, and append-only."
  [run-dir {:keys [action id]}]
  (.toPath (io/file run-dir "receipts" (name action) (str id ".edn"))))

(defn read-receipt [run-dir intent]
  (let [path (receipt-path run-dir intent)]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (with-open [reader (PushbackReader. (io/reader (.toFile path)))]
        (edn/read {:eof nil} reader)))))

(defn append-receipt!
  "Creates one immutable receipt. Repeating the identical write is
  idempotent; conflicting content fails closed."
  [run-dir intent result]
  (let [path (receipt-path run-dir intent)
        receipt {:schema schema-version
                 :problem-id (:problem-id intent)
                 :turn (:turn intent)
                 :action (:action intent)
                 :intent-id (:id intent)
                 :result result}
        bytes (.getBytes (str (pr-str receipt) "\n") StandardCharsets/UTF_8)]
    (Files/createDirectories (.getParent path)
                             (make-array java.nio.file.attribute.FileAttribute 0))
    (try
      (with-open [channel (java.nio.channels.FileChannel/open
                           path
                           (into-array StandardOpenOption
                                       [StandardOpenOption/CREATE_NEW
                                        StandardOpenOption/WRITE]))]
        (.write channel (java.nio.ByteBuffer/wrap bytes))
        (.force channel true))
      (fsync-directory! (.getParent path))
      receipt
      (catch java.nio.file.FileAlreadyExistsException _
        (let [existing (read-receipt run-dir intent)]
          (if (= receipt existing)
            existing
            (fail! :receipt-conflict {:path (str path)
                                      :existing existing
                                      :attempted receipt})))))))

(defn- intent-id [state action]
  (sha256 (pr-str [schema-version (:problem-id state) (:turn state)
                   (:checkpoint state) action (:base-sha state)
                   (:head-sha state)])))

(defn begin-action!
  "Persists an action intent before the caller may perform an external effect.
  Returns the persisted intent. It never invokes the effect itself."
  [run-dir action]
  (let [state (read-state run-dir)
        required-phase (case action
                         :turn :turn-ready
                         :gate :gating
                         :review :checkpoint-ready
                         :bank :review-pending)]
    (when-not (= required-phase (:phase state))
      (fail! :transition-not-allowed {:phase (:phase state) :action action}))
    (let [intent {:schema schema-version
                  :problem-id (:problem-id state)
                  :turn (:turn state)
                  :checkpoint (:checkpoint state)
                  :action action
                  :id (intent-id state action)
                  :base-sha (:base-sha state)
                  :head-sha (:head-sha state)}]
      (write-state! run-dir (cond-> (assoc state
                                           :phase (intent-phase action)
                                           :intent intent)
                              (= :bank action) (assoc :pending-bank intent)))
      intent)))

(defn- validate-receipt! [state receipt]
  (let [intent (:intent state)]
    (when-not (and (= schema-version (:schema receipt))
                   (= (:problem-id intent) (:problem-id receipt))
                   (= (:turn intent) (:turn receipt))
                   (= (:action intent) (:action receipt))
                   (= (:id intent) (:intent-id receipt)))
      (fail! :receipt-intent-mismatch {:intent intent :receipt receipt})))
  receipt)

(defn- settled-state [state receipt]
  (let [action (get-in state [:intent :action])
        result (:result receipt)]
    (case action
      :turn (assoc state :phase :gating :intent nil
                         :head-sha (or (:head-sha result) (:head-sha state)))
      :gate (if (:checkpoint-due? result)
              (assoc state :phase :checkpoint-ready :intent nil
                           :last-green-gate (:receipt/path result))
              (assoc state :phase :turn-ready :intent nil
                           :turn (inc (:turn state))
                           :last-green-gate (:receipt/path result)))
      :review (assoc state :phase :review-pending :intent nil
                           :checkpoint (inc (:checkpoint state)))
      :bank (assoc state :phase :turn-ready :intent nil
                         :turn (inc (:turn state))
                         :base-sha (or (:bank-sha result) (:base-sha state))
                         :head-sha (or (:bank-sha result) (:head-sha state))
                         :pending-bank nil))))

(defn reconcile!
  "Reconciles only the persisted intent. If its receipt exists, validates and
  advances state. Otherwise calls `reconcile-external` for observation only.
  The callback may return a completed result, which is durably receipted before
  state advances, or nil/`:pending`. It must never launch a new action."
  [run-dir reconcile-external]
  (let [state (read-state run-dir)
        intent (:intent state)]
    (if-not intent
      {:status :idle :state state}
      (let [receipt (or (read-receipt run-dir intent)
                        (when-let [result (reconcile-external intent)]
                          (when-not (= :pending result)
                            (append-receipt! run-dir intent result))))]
        (if-not receipt
          {:status :pending :intent intent :state state}
          (let [receipt (validate-receipt! state receipt)
                next-state (settled-state state receipt)]
            (write-state! run-dir next-state)
            {:status :settled :receipt receipt :state next-state}))))))
