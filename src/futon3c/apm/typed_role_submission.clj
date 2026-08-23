(ns futon3c.apm.typed-role-submission
  "Controller-owned, content-addressed terminal submissions for live APM roles.

   Agents submit observations only.  Frame identity, role, phase, dispatch,
   agent, and job identity are copied from an immutable authority registered
   by the controller before activation."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [futon3c.apm.campaign-machine :as machine])
  (:import (java.nio.file Files StandardCopyOption)
           (java.util UUID)))

(def ^:dynamic *submission-root*
  "data/apm-role-submissions")

(def completion-contract
  {:path "holes/labs/M-apm-demonstration/role-cards/typed-completion-v1.md"
   :blob "d3351807f1597baf97e8ba7ae5605274f0f9a92c"})

(def authority-fields
  #{:job-id :dispatch/id :agent-id :frame-id :problem-id :phase :role
    :attempt-ordinal :fresh-session-nonce :memory-snapshot})

(def common-required #{:command-own-exit :outcome :failure-account :evidence})

(def evidence-required-by-phase
  {:preflight #{:mutations :clean-before? :clean-after? :statement-unchanged? :lean}
   :solve #{:branch :base-revision :final-head :committed? :statement-unchanged?
            :lean :axioms :clean-before? :clean-after? :mutations}
   :verify #{:branch :base-revision :final-head :committed? :statement-unchanged?
             :lean :axioms :clean-before? :clean-after? :mutations}
   ;; The downstream promotion validators distinguish the Solver-mining and
   ;; deposit forms; the common wrapper is deterministic for both.
   :promote-solver #{}
   :promotion-review #{:candidate-set-digest :base-problem-blob
                       :open-residuals :reviews}
   :student-attempt-1 #{:memory-use}
   :student-attempt-2 #{:memory-use}
   :student-attempt-3 #{:memory-use}
   :guide-intervention-1 #{:channel-audit}
   :guide-intervention-2 #{:channel-audit}
   :scribe-reduce #{:lanes :dispositions :promotion-reviews}
   :close-frame #{:trace-id :result}
   :analyst #{:analysis}})

(defn new-token [] (str (UUID/randomUUID)))

(defn prepare-request [request]
  (let [seed (select-keys request
                          [:dispatch/id :agent-id :frame-id :problem-id :phase
                           :role :attempt :session-id :memory-snapshot-id])]
    (assoc request :submission/token
           (machine/ledger-digest ["apm-role-submission" seed]))))

(defn canonical-job-id [request]
  (str "apm-role-" (machine/ledger-digest
                    ["apm-role-job" (:dispatch/id request)
                     (:agent-id request) (:phase request)])))

(defn with-job-authority [request]
  (assoc request :submission/job-id (or (:submission/job-id request)
                                        (canonical-job-id request))))

(defn- record-path [job-id]
  (io/file *submission-root* (str job-id ".edn")))

(defn- read-record [job-id]
  (let [file (record-path job-id)]
    (when (.isFile file) (edn/read-string (slurp file)))))

(defn- atomic-write! [file value]
  (io/make-parents file)
  (let [temporary (io/file (.getParentFile file)
                           (str "." (.getName file) "." (UUID/randomUUID) ".tmp"))]
    (spit temporary (str (pr-str value) "\n"))
    (Files/move (.toPath temporary) (.toPath file)
                (into-array StandardCopyOption
                            [StandardCopyOption/ATOMIC_MOVE
                             StandardCopyOption/REPLACE_EXISTING])))
  {:ok true})

(defn authority
  "Create the exact authority that will be registered after job announcement."
  [request ticket]
  (select-keys
   (merge request {:job-id (:job-id ticket)})
   (conj authority-fields :submission/token)))

(defn register!
  "Persist immutable job authority. Identical replay is idempotent; conflicting
   authority fails closed."
  [request ticket]
  (let [auth (authority request ticket)
        job-id (:job-id auth)
        existing (read-record job-id)]
    (cond
      (or (not (string? job-id)) (not (string? (:submission/token auth))))
      {:ok false :error/code :role-submission-authority-invalid}

      (and existing (not= auth (:authority existing)))
      {:ok false :error/code :role-submission-authority-conflict}

      existing {:ok true :status :already-registered :authority auth}

      :else
      (let [record {:record/type :apm-role-submission
                    :authority auth :submission nil}]
        (atomic-write! (record-path job-id) record)
        {:ok true :status :registered :authority auth}))))

(defn validate-payload [auth payload]
  (if-not (map? payload)
    {:ok false :error/code :role-submission-payload-invalid
     :findings [:payload-not-map]}
   (let [missing (set/difference common-required (set (keys payload)))
        evidence-required (get evidence-required-by-phase (:phase auth))
        evidence-missing (if evidence-required
                           (set/difference evidence-required
                                           (set (keys (:evidence payload))))
                           #{})
        findings (cond-> []
                   (nil? evidence-required) (conj :phase-schema-unknown)
                   (seq (set/intersection authority-fields (set (keys payload))))
                   (conj :authority-field-supplied-by-agent)
                   (seq missing) (conj :required-fields-missing)
                   (not (map? (:evidence payload))) (conj :evidence-not-map)
                   (seq evidence-missing) (conj :evidence-fields-missing)
                   (and (contains? payload :command-own-exit)
                        (not (int? (:command-own-exit payload))))
                   (conj :command-own-exit-not-integer)
                   (and (contains? payload :failure-account)
                        (not (vector? (:failure-account payload))))
                   (conj :failure-account-not-vector))]
    (if (seq findings)
      {:ok false :error/code :role-submission-payload-invalid
       :findings findings :missing missing :evidence/missing evidence-missing}
      {:ok true}))))

(defn schema [job-id token]
  (let [record (read-record job-id)
        auth (:authority record)]
    (cond
      (nil? record) {:ok false :error/code :role-submission-authority-missing}
      (not= token (:submission/token auth))
      {:ok false :error/code :role-submission-token-mismatch}
      :else {:ok true :phase (:phase auth)
             :required (vec (sort common-required))
             :evidence-required
             (vec (sort (get evidence-required-by-phase (:phase auth))))})))

(defn submit!
  "Validate and persist an agent's observational payload. The canonical result
   is content-addressed and receives authority exclusively from registration."
  [job-id token payload]
  (let [record (read-record job-id)
        auth (:authority record)]
    (cond
      (nil? record) {:ok false :error/code :role-submission-authority-missing}
      (not= token (:submission/token auth))
      {:ok false :error/code :role-submission-token-mismatch}
      :else
      (let [checked (validate-payload auth payload)]
        (if-not (:ok checked)
          checked
          (let [body {:authority (dissoc auth :submission/token)
                      :payload payload}
                submission (assoc body :submission/id
                                  (machine/ledger-digest [body]))
                existing (:submission record)]
            (cond
              (= existing submission)
              {:ok true :status :already-submitted :submission submission}
              existing
              {:ok false :error/code :role-submission-conflict
               :submission/id (:submission/id existing)}
              :else
              (do (atomic-write! (record-path job-id)
                                 (assoc record :submission submission))
                  {:ok true :status :submitted :submission submission}))))))))

(defn submitted [job-id]
  (some-> (read-record job-id) :submission))

(defn command
  "Exact client command placed into the activated role prompt."
  [request ticket]
  (let [base (str "/home/joe/code/futon3c/scripts/apm-submit-role.py"
                  " --job-id " (:job-id ticket)
                  " --token " (:submission/token request))
        payload (str "/tmp/apm-role-" (:job-id ticket) ".json")]
    (str base " --init --payload " payload
         "\n# edit " payload ", then submit:\n"
         base " --payload " payload)))
