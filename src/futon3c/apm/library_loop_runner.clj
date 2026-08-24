(ns futon3c.apm.library-loop-runner
  "Files-only durable core for the long-lived Library Loop runner.

  This namespace owns state transitions and durable receipts, but deliberately
  knows nothing about Codex, Lean, review transport, or Git banking.  Callers
  persist an intent with `begin-action!`, perform (or idempotently activate)
  the external action, append its receipt, and call `reconcile!`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.library-loop-checkpoint :as checkpoint])
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
   :review-reconsideration :review-pending
   :bank :bank-pending})

(def allowed-transitions
  "The runner's complete phase graph. The sole exit from `:paused` is guarded
  by `begin-review-reconsideration!`; generic action/resume paths cannot use
  that edge."
  {:turn-ready #{:turn-running :paused}
   :turn-running #{:gating :paused}
   :gating #{:turn-ready :checkpoint-ready :paused}
   :checkpoint-ready #{:review-pending :paused}
   :review-pending #{:bank-pending :paused}
   :bank-pending #{:turn-ready :paused}
   :paused #{:review-pending}})

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
   :pending-checkpoint nil
   :obligation/id nil
   :consecutive-nonreductions 0
   :pause/finding nil
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
                 (string? (:head-sha state))
                 (nat-int? (:consecutive-same-failures state))
                 (or (nil? (:failure-fingerprint state))
                     (string? (:failure-fingerprint state)))
                 (nat-int? (:consecutive-nonreductions state))
                 (or (nil? (:obligation/id state))
                     (keyword? (:obligation/id state))))
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
  (when (and (= :paused (:phase state))
             (not (and (map? (:pause/finding state))
                       (keyword? (get-in state [:pause/finding :type])))))
    (fail! :invalid-pause-finding {:state state}))
  (when-let [pending (:pending-checkpoint state)]
    (when-not (and (= schema-version (:schema pending))
                   (= (:problem-id state) (:problem-id pending))
                   (= (:turn state) (:turn pending))
                   (= (:checkpoint state) (:checkpoint pending))
                   (= (:head-sha state) (:head-sha pending))
                   (keyword? (:obligation-id pending))
                   (string? (:checkpoint-digest pending))
                   (re-matches #"[0-9a-f]{64}" (:checkpoint-digest pending)))
      (fail! :invalid-pending-checkpoint {:state state})))
  state)

(defn state-path [run-dir]
  (.toPath (io/file run-dir "state.edn")))

(defn- fsync-directory! [^Path path]
  (with-open [channel (java.nio.channels.FileChannel/open
                       path (into-array StandardOpenOption
                                        [StandardOpenOption/READ]))]
    (.force channel true)))

(defn atomic-write-edn!
  "Atomically replaces an arbitrary EDN file with fsync durability."
  [target-file value]
  (let [^Path target (.toPath (io/file target-file))
        dir (.getParent target)
        temp (.resolve dir (str "." (.getFileName target) "."
                                (UUID/randomUUID) ".tmp"))
        bytes (.getBytes (str (pr-str value) "\n") StandardCharsets/UTF_8)]
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
      value
      (finally (Files/deleteIfExists temp)))))

(defn append-edn-once!
  "Creates an immutable EDN artifact. Identical replay is idempotent."
  [target-file value]
  (let [^Path path (.toPath (io/file target-file))
        bytes (.getBytes (str (pr-str value) "\n") StandardCharsets/UTF_8)]
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
      value
      (catch java.nio.file.FileAlreadyExistsException _
        (let [existing (with-open [reader (PushbackReader. (io/reader target-file))]
                         (edn/read {:eof nil} reader))]
          (if (= value existing)
            existing
            (fail! :append-only-artifact-conflict
                   {:path (str path) :existing existing :attempted value})))))))

(defn write-state!
  "Atomically replaces state.edn using temp-write, file fsync, rename, and
  directory fsync.  The temp file is in the destination directory."
  [run-dir state]
  (validate-state state)
  (atomic-write-edn! (.toFile (state-path run-dir)) state))

(defn read-state [run-dir]
  (with-open [reader (PushbackReader. (io/reader (.toFile (state-path run-dir))))]
    (validate-state (edn/read {:eof nil} reader))))

(defn receipt-path
  "Receipt names are stable, phase/action scoped, and append-only."
  [run-dir {:keys [action id]}]
  (.toPath (io/file run-dir "receipts" (name action) (str id ".edn"))))

(declare intent-id)

(def ^:private feedback-diagnostic-keys
  #{:module :created-turn :creation-commit-sha :turn-receipt-id
    :turn-head-sha :problem-id :current-turn :expected :observed
    :expected-base :expected-head :observed-head})

(defn- bounded-feedback-diagnostic [diagnostic]
  (when diagnostic
    (when-not (map? diagnostic)
      (fail! :preceding-gate-diagnostic-invalid {:diagnostic diagnostic}))
    (let [bounded (select-keys diagnostic feedback-diagnostic-keys)]
      (when-not (every? (fn [[_ value]]
                          (or (nil? value) (keyword? value) (integer? value)
                              (boolean? value)
                              (and (string? value) (<= (count value) 256))))
                        bounded)
        (fail! :preceding-gate-diagnostic-invalid {:diagnostic diagnostic}))
      bounded)))

(defn read-receipt [run-dir intent]
  (let [path (receipt-path run-dir intent)]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (with-open [reader (PushbackReader. (io/reader (.toFile path)))]
        (edn/read {:eof nil} reader)))))

(defn preceding-gate-feedback
  "Returns a bounded, state-bound summary of the immediately preceding gate.

  The receipt path is derived from the current durable state and the canonical
  gate intent identity; callers never select a receipt by directory order. A
  receipt at that path must bind the same problem, preceding turn, intent, and
  candidate head or the runner fails closed. Captured process output and
  repository snapshots are deliberately excluded from the returned value."
  [run-dir state]
  (validate-state state)
  (when (and (= :turn-ready (:phase state))
             (> (:turn state) 1))
    (let [gate-turn (dec (:turn state))
          prior-state (assoc state :turn gate-turn :intent nil)
          expected-id (intent-id prior-state :gate)
          intent {:action :gate :id expected-id}
          receipt (read-receipt run-dir intent)]
      (when (and (nil? receipt) (:failure-fingerprint state))
        (fail! :preceding-gate-receipt-missing
               {:problem-id (:problem-id state)
                :turn gate-turn
                :intent-id expected-id}))
      (when receipt
        (let [result (:result receipt)
              inputs (get-in result [:plan :inputs])
              typed-binding (:gate/binding result)
              observed-head (or (:head-sha inputs) (:head-sha typed-binding))
              registration (:registration result)
              outcome (:outcome result)
              diagnostic (bounded-feedback-diagnostic (:diagnostic result))
              finding (or (:finding registration)
                          (:finding result)
                          (when (= :red outcome) :command-failed))]
          (when-not (and (= schema-version (:schema receipt))
                         (= (:problem-id state) (:problem-id receipt))
                         (= gate-turn (:turn receipt))
                         (= :gate (:action receipt))
                         (= expected-id (:intent-id receipt))
                         (map? result)
                         (contains? #{:green :red} outcome)
                         (= (:head-sha state) observed-head)
                         (or (nil? typed-binding)
                             (= {:schema schema-version
                                 :problem-id (:problem-id state)
                                 :turn gate-turn
                                 :base-sha (:base-sha state)
                                 :head-sha (:head-sha state)
                                 :intent-id expected-id}
                                typed-binding)))
            (fail! :preceding-gate-receipt-mismatch
                   {:expected {:problem-id (:problem-id state)
                               :turn gate-turn
                               :action :gate
                               :intent-id expected-id
                               :head-sha (:head-sha state)}
                    :receipt receipt}))
          (cond-> {:schema schema-version
                   :problem-id (:problem-id receipt)
                   :turn gate-turn
                   :intent-id expected-id
                   :head-sha observed-head
                   :outcome outcome}
            finding (assoc :finding finding)
            (:failure-fingerprint result)
            (assoc :failure-fingerprint (:failure-fingerprint result))
            diagnostic (assoc :diagnostic diagnostic)
            (:outcome registration)
            (assoc :registration/outcome (:outcome registration))))))))

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
                   (:head-sha state)
                   (when (#{:review :bank} action)
                     (:pending-checkpoint state))])))

(defn- checkpoint-identity [result]
  (select-keys result
               [:schema :problem-id :turn :checkpoint :head-sha
                :obligation-id :checkpoint-digest]))

(defn- canonical-rejected-review!
  [run-dir state]
  (let [pause (:pause/finding state)
        prior-action (:action pause)
        prior-id (:intent-id pause)
        prior-intent {:action prior-action :id prior-id}
        receipt (when (and (keyword? prior-action) (string? prior-id))
                  (read-receipt run-dir prior-intent))
        result (:result receipt)]
    (when-not (and (= :paused (:phase state))
                   (= :review-not-approved (:type pause))
                   (contains? #{:review :review-reconsideration} prior-action)
                   (map? (:pending-checkpoint state))
                   (= schema-version (:schema receipt))
                   (= (:problem-id state) (:problem-id receipt))
                   (= (:turn state) (:turn receipt))
                   (= prior-action (:action receipt))
                   (= prior-id (:intent-id receipt))
                   (= result (:result pause))
                   (= (:pending-checkpoint state)
                      (checkpoint-identity result))
                   (= :rejected (:outcome result))
                   (false? (:bank-authorized? result))
                   (= :review-not-approved (:finding result)))
      (fail! :prior-review-receipt-invalid
             {:problem-id (:problem-id state)
              :turn (:turn state)
              :prior-action prior-action
              :prior-intent-id prior-id}))
    {:action prior-action :intent-id prior-id}))

(defn begin-review-reconsideration!
  "Persists a distinct review intent chained to the latest canonical
  :review-not-approved receipt. No other paused state is recoverable here."
  [run-dir]
  (let [state (read-state run-dir)]
    (when (:intent state)
      (fail! :action-already-pending
             {:intent (:intent state) :action :review-reconsideration}))
    (let [prior (canonical-rejected-review! run-dir state)
          identity (:pending-checkpoint state)
          id (sha256 (pr-str [schema-version (:problem-id state) (:turn state)
                              (:checkpoint state) :review-reconsideration
                              (:base-sha state) (:head-sha state) identity prior]))
          intent {:schema schema-version
                  :problem-id (:problem-id state)
                  :turn (:turn state)
                  :checkpoint (:checkpoint state)
                  :action :review-reconsideration
                  :id id
                  :base-sha (:base-sha state)
                  :head-sha (:head-sha state)
                  :checkpoint/identity identity
                  :prior-review prior}]
      (write-state! run-dir (assoc state :phase :review-pending :intent intent))
      intent)))

(defn install-checkpoint!
  "Validates and durably binds the exact checkpoint that an independent
  review may judge. Installation is permitted once, in :checkpoint-ready,
  before the review intent is persisted."
  [run-dir checkpoint-record]
  (let [state (read-state run-dir)]
    (when (:intent state)
      (fail! :action-already-pending {:intent (:intent state)
                                      :action :install-checkpoint}))
    (when-not (= :checkpoint-ready (:phase state))
      (fail! :transition-not-allowed {:phase (:phase state)
                                      :action :install-checkpoint}))
    (let [record (checkpoint/validate-checkpoint checkpoint-record)
          identity {:schema schema-version
                    :problem-id (:problem-id state)
                    :turn (:turn state)
                    :checkpoint (:checkpoint state)
                    :head-sha (:head-sha state)
                    :obligation-id (:id record)
                    :checkpoint-digest (checkpoint/checkpoint-digest record)}]
      (when (and (:pending-checkpoint state)
                 (not= identity (:pending-checkpoint state)))
        (fail! :checkpoint-already-installed
               {:existing (:pending-checkpoint state) :attempted identity}))
      (write-state! run-dir (assoc state :pending-checkpoint identity))
      identity)))

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
    (when (:intent state)
      (fail! :action-already-pending {:intent (:intent state)
                                      :action action}))
    (when-not (= required-phase (:phase state))
      (fail! :transition-not-allowed {:phase (:phase state) :action action}))
    (when (and (#{:review :bank} action) (nil? (:pending-checkpoint state)))
      (fail! :missing-pending-checkpoint {:action action :state state}))
    (let [next-phase (intent-phase action)]
      (when-not (or (= (:phase state) next-phase)
                    (contains? (get allowed-transitions (:phase state)) next-phase))
        (fail! :transition-not-allowed {:from (:phase state)
                                        :to next-phase
                                        :action action})))
    (let [intent {:schema schema-version
                  :problem-id (:problem-id state)
                  :turn (:turn state)
                  :checkpoint (:checkpoint state)
                  :action action
                  :id (intent-id state action)
                  :base-sha (:base-sha state)
                  :head-sha (:head-sha state)
                  :checkpoint/identity (when (#{:review :bank} action)
                                         (:pending-checkpoint state))}]
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
    (letfn [(advance [next-phase updates]
              (when-not (or (= (:phase state) next-phase)
                            (contains? (get allowed-transitions (:phase state))
                                       next-phase))
                (fail! :transition-not-allowed
                       {:from (:phase state) :to next-phase :action action}))
              (merge state {:phase next-phase :intent nil} updates))
            (pause [finding]
              (advance :paused
                       {:pause/finding {:type finding
                                        :action action
                                        :intent-id (get-in state [:intent :id])
                                        :result result}}))]
      (case action
        :turn (if (= :ok (:outcome result))
                (advance :gating
                         {:head-sha (or (:head-sha result) (:head-sha state))})
                (pause :turn-failed))
        :gate (if (= :green (:outcome result))
                (if (:checkpoint-due? result)
                  (advance :checkpoint-ready
                           {:last-green-gate (:receipt/path result)
                            :failure-fingerprint nil
                            :consecutive-same-failures 0})
                  (advance :turn-ready
                           {:turn (inc (:turn state))
                            :last-green-gate (:receipt/path result)
                            :failure-fingerprint nil
                            :consecutive-same-failures 0}))
                (let [fingerprint (:failure-fingerprint result)
                      same? (= fingerprint (:failure-fingerprint state))
                      count (if same?
                              (inc (:consecutive-same-failures state))
                              1)
                      updates {:failure-fingerprint fingerprint
                               :consecutive-same-failures count}]
                  (cond
                    (not (and (string? fingerprint) (not-empty fingerprint)))
                    (pause :invalid-gate-failure)

                    (>= count 2)
                    (advance :paused
                             (assoc updates :intent nil
                                    :pause/finding
                                    {:type :repeated-gate-failure
                                     :action :gate
                                     :intent-id (get-in state [:intent :id])
                                     :result result}))

                    :else
                    (advance :turn-ready (assoc updates :turn (inc (:turn state)))))))
        (:review :review-reconsideration)
        (let [observed-identity (checkpoint-identity result)]
                  (cond
                    (and (= :review-reconsideration action)
                         (not= (get-in state [:intent :prior-review])
                               (:prior-review result)))
                    (pause :review-reconsideration-chain-mismatch)

                    (not= (:pending-checkpoint state) observed-identity)
                    (pause :review-checkpoint-identity-mismatch)

                    (and (checkpoint/valid-decision? state result)
                         (= :approved (:outcome result))
                         (true? (:bank-authorized? result)))
                    (advance :review-pending
                             {:obligation/id (:obligation-id result)
                              :consecutive-nonreductions
                              (:consecutive-nonreductions result)})

                    :else
                    (pause (or (:finding result) :review-not-approved))))
        :bank (if (and (= :banked (:outcome result))
                       (= (get-in state [:intent :head-sha]) (:bank-sha result)))
                (advance :turn-ready
                         {:turn (inc (:turn state))
                          :checkpoint (inc (:checkpoint state))
                          :base-sha (or (:bank-sha result) (:base-sha state))
                          :head-sha (or (:bank-sha result) (:head-sha state))
                          :pending-bank nil
                          :pending-checkpoint nil})
                (pause :bank-failed))))))

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
