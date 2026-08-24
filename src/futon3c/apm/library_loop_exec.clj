(ns futon3c.apm.library-loop-exec
  "Executable seams for the files-only Library Loop.

  All process and repository observations are injected. This namespace
  sequences durable intents, pure plans, command evidence, review artifacts,
  exact bank observation, and slate updates; it owns no scheduler."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.library-loop-checkpoint :as checkpoint]
            [futon3c.apm.library-loop-rebuild :as rebuild]
            [futon3c.apm.library-loop-runner :as runner]
            [futon3c.apm.library-loop-slate :as slate]
            [futon3c.apm.library-loop-targets :as targets])
  (:import (java.io PushbackReader)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def default-checkpoint-cadence 20)

(defn- sha256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- command-evidence [run-command command]
  (let [result (run-command command)]
    {:command command
     :executed/argv (:argv result)
     :cwd (:cwd result)
     :exit (:exit result)
     :stdout (or (:stdout result) "")
     :stderr (or (:stderr result) "")}))

(defn- execute-commands [run-command commands]
  (loop [remaining commands evidence []]
    (if-let [command (first remaining)]
      (let [observed (command-evidence run-command command)
            next-evidence (conj evidence observed)]
        (if (zero? (:exit observed))
          (recur (next remaining) next-evidence)
          next-evidence))
      evidence)))

(defn- all-green? [evidence]
  (every? #(zero? (:exit %)) evidence))

(defn- require-intent-action! [intent action]
  (when-not (= action (:action intent))
    (throw (ex-info "unexpected-pending-action"
                    {:finding :unexpected-pending-action
                     :expected action :intent intent})))
  intent)

(defn run-gate!
  "Runs or settles one persisted gate intent. `observe` returns exact Git and
  repository inputs accepted by rebuild/plan and targets/check."
  [run-dir {:keys [observe run-command cadence]
            :or {cadence default-checkpoint-cadence}}]
  (let [state (runner/read-state run-dir)
        intent (require-intent-action!
                (or (:intent state) (runner/begin-action! run-dir :gate))
                :gate)]
    (if (runner/read-receipt run-dir intent)
      (runner/reconcile! run-dir (constantly nil))
      (let [observation (observe state)
            plan (rebuild/plan observation)
            registration-input (assoc (select-keys observation
                                                    [:head-sha :files :targets
                                                     :axiom-audits
                                                     :target-provenance])
                                      :problem-id (:problem-id state)
                                      :turn (:turn state)
                                      :changes (:changes plan))
            registration (try
                           (targets/check
                            registration-input)
                           (catch clojure.lang.ExceptionInfo ex
                             {:outcome :red
                              :finding (:finding (ex-data ex))
                              :snapshot (:snapshot (ex-data ex))}))
            commands (if (= :green (:outcome registration))
                       (execute-commands run-command (:commands plan))
                       [])
            green? (and (= :green (:outcome registration))
                        (all-green? commands))
            finding (when-not green?
                      (or (:finding registration)
                          :command-failed))
            fingerprint (when finding
                          (sha256 [finding (:snapshot registration)
                                   (mapv #(select-keys % [:command :exit :stderr]) commands)
                                   (:inputs plan)]))
            result {:outcome (if green? :green :red)
                    :failure-fingerprint fingerprint
                    :checkpoint-due? (zero? (mod (:turn state) cadence))
                    :receipt/path (str "receipts/gate/" (:id intent) ".edn")
                    :plan plan
                    :registration registration
                    :observation/evidence (:observation/commands observation)
                    :commands commands}]
        (runner/append-receipt! run-dir intent result)
        (runner/reconcile! run-dir (constantly nil))))))

(defn request-review!
  "Installs a validated strategy checkpoint and appends an immutable request
  for an independent reviewer. It does not create a review decision."
  [run-dir checkpoint-record]
  (let [identity (runner/install-checkpoint! run-dir checkpoint-record)
        request {:schema 1
                 :type :library-loop/review-request
                 :identity identity
                 :checkpoint checkpoint-record}
        path (io/file run-dir "review-requests"
                      (str (:checkpoint-digest identity) ".edn"))]
    (runner/append-edn-once! path request)
    {:identity identity :request-path (str path)}))

(defn apply-review!
  "Consumes an independently authored review record and persists its exact,
  state-bound decision as the review receipt."
  [run-dir checkpoint-record review-record]
  (let [state (runner/read-state run-dir)
        intent (require-intent-action!
                (or (:intent state) (runner/begin-action! run-dir :review))
                :review)
        decision (checkpoint/review-decision state checkpoint-record review-record)]
    (runner/append-receipt! run-dir intent decision)
    (runner/reconcile! run-dir (constantly nil))))

(defn- settle-bank! [run-dir state intent slate-path result]
  (runner/append-receipt! run-dir intent result)
  (when slate-path
    (slate/apply-status! slate-path (:problem-id state) result))
  (runner/reconcile! run-dir (constantly nil)))

(defn- status-and-settle!
  [run-dir state intent {:keys [observe-bank run-command status-command slate-path]}
   evidence]
  (let [status-result (command-evidence run-command (status-command intent))
        observed (observe-bank intent)]
    (if (and (zero? (:exit status-result))
             (:landed? observed)
             (= (:head-sha intent) (:bank-sha observed)))
      (settle-bank! run-dir state intent slate-path
                    (merge {:outcome :banked
                            :bank-sha (:bank-sha observed)
                            :ruling (:ruling observed)
                            :status-sha (:status-sha observed)
                            :status-command status-result}
                           evidence))
      {:status :bank-not-observed :status-command status-result
       :observed observed})))

(defn run-bank!
  "Reconciles or performs one exact bank. `observe-bank` must report whether
  the intent's exact candidate is already the trunk head. Rebuild commands run
  before the injected mutation and status recomputation."
  [run-dir {:keys [observe observe-bank run-command bank-command
                   status-command slate-path]}]
  (let [state (runner/read-state run-dir)
        intent (require-intent-action!
                (or (:intent state) (runner/begin-action! run-dir :bank))
                :bank)
        existing-receipt (runner/read-receipt run-dir intent)
        landed (when-not existing-receipt (observe-bank intent))]
    (cond
      existing-receipt
      (do
        (when (and slate-path
                   (= (:head-sha intent)
                      (get-in existing-receipt [:result :bank-sha])))
          (slate/apply-status! slate-path (:problem-id state)
                               (:result existing-receipt)))
        (runner/reconcile! run-dir (constantly nil)))

      (and (:landed? landed) (= (:head-sha intent) (:bank-sha landed)))
      (let [result {:outcome :banked
                    :bank-sha (:bank-sha landed)
                    :ruling (:ruling landed)
                    :status-sha (:status-sha landed)}]
        (settle-bank! run-dir state intent slate-path result))

      (:candidate-landed? landed)
      (status-and-settle! run-dir state intent
                          {:observe-bank observe-bank
                           :run-command run-command
                           :status-command status-command
                           :slate-path slate-path}
                          {:restart/recovered-after-mutation true})

      :else
      (let [observation (observe state)
            plan (rebuild/plan observation)
            builds (execute-commands run-command (:commands plan))]
        (if-not (all-green? builds)
          {:status :bank-preflight-failed :plan plan :commands builds}
          (let [mutation (command-evidence run-command (bank-command intent))]
            (if-not (zero? (:exit mutation))
              {:status :bank-mutation-failed :mutation mutation}
              (status-and-settle! run-dir state intent
                                  {:observe-bank observe-bank
                                   :run-command run-command
                                   :status-command status-command
                                   :slate-path slate-path}
                                  {:plan plan :builds builds
                                   :mutation mutation}))))))))

(defn read-edn-file [path]
  (with-open [reader (PushbackReader. (io/reader path))]
    (edn/read {:eof nil} reader)))

(defn run-dir [root problem-id]
  (io/file root "data" "apm-lane" "runs" problem-id))

(defn init!
  [root {:keys [problem-id workspace base-sha head-sha]}]
  (let [dir (run-dir root problem-id)]
    (when (.exists (io/file dir "state.edn"))
      (throw (ex-info "run-already-exists" {:problem-id problem-id})))
    (runner/write-state! dir
                         (runner/initial-state
                          {:problem-id problem-id :workspace workspace
                           :base-sha base-sha :head-sha head-sha}))))

(defn status [root problem-id]
  (runner/read-state (run-dir root problem-id)))

(defn resume-one!
  "Executes at most one action. A review/bank boundary is returned to the
  caller and never self-approved."
  [root problem-id deps]
  (let [dir (run-dir root problem-id)
        state (runner/read-state dir)]
    (case (:phase state)
      :turn-ready (let [intent (runner/begin-action! dir :turn)
                        result (command-evidence (:run-command deps)
                                                 ((:turn-command deps) state))]
                    (runner/append-receipt!
                     dir intent {:outcome (if (zero? (:exit result)) :ok :failed)
                                 :head-sha ((:observe-head deps))
                                 :command result})
                    (runner/reconcile! dir (constantly nil)))
      :turn-running (runner/reconcile! dir (:reconcile-turn deps))
      :gating (run-gate! dir deps)
      :checkpoint-ready {:status :checkpoint-required :state state}
      :review-pending {:status (if (:intent state) :review-running :review-required)
                       :state state}
      :bank-pending (run-bank! dir deps)
      :paused {:status :paused :state state})))

(defn cli!
  "Testable CLI seam. `deps` supplies root and external effects."
  [args deps]
  (let [[command problem-id & rest] args
        root (:root deps)]
    (case command
      "init" (let [[workspace base-sha head-sha] rest]
               (init! root {:problem-id problem-id :workspace workspace
                            :base-sha base-sha :head-sha head-sha}))
      "status" (status root problem-id)
      "resume" (resume-one! root problem-id deps)
      "checkpoint" (request-review! (run-dir root problem-id)
                                     (read-edn-file (first rest)))
      "apply-review" (apply-review! (run-dir root problem-id)
                                    (read-edn-file (first rest))
                                    (read-edn-file (second rest)))
      "bank" (run-bank! (run-dir root problem-id) deps)
      (throw (ex-info "unknown-library-loop-command" {:command command})))))

(defn- adapter-deps [root problem-id]
  (if-let [namespace-name (System/getenv "LIBRARY_LOOP_ADAPTER_NS")]
    (let [namespace-symbol (symbol namespace-name)
          _ (require namespace-symbol)
          deps-var (ns-resolve namespace-symbol 'deps)]
      (when-not deps-var
        (throw (ex-info "library-loop-adapter-missing-deps"
                        {:namespace namespace-name})))
      (merge {:root root} (deps-var {:root root :problem-id problem-id})))
    {:root root}))

(defn -main [& args]
  (let [root (or (System/getenv "LIBRARY_LOOP_ROOT")
                 (.getCanonicalPath (io/file ".")))
        problem-id (second args)
        external? (contains? #{"resume" "bank"} (first args))]
    (try
      (println (pr-str (cli! args (if external?
                                    (adapter-deps root problem-id)
                                    {:root root}))))
      (catch Throwable ex
        (binding [*out* *err*]
          (println (pr-str {:error (.getMessage ex)
                            :data (ex-data ex)})))
        (System/exit 2)))))
