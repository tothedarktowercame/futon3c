(ns futon3c.apm.typed-role-submission
  "Controller-owned, content-addressed terminal submissions for live APM roles.

   Agents submit observations only.  Frame identity, role, phase, dispatch,
   agent, and job identity are copied from an immutable authority registered
   by the controller before activation."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.generated-contract :as generated-contract])
  (:import (java.nio.file Files StandardCopyOption)
           (java.util UUID)))

(def ^:dynamic *submission-root*
  "data/apm-role-submissions")

(defn wire-keyword
  "Normalize a JSON string representation at the typed-submission boundary.
  Keywords remain keywords; absent and non-string values remain invalid."
  [value]
  (cond
    (keyword? value) value
    (string? value) (keyword (if (.startsWith ^String value ":")
                               (subs value 1)
                               value))
    :else nil))

(defn normalize-predicate-keys
  "Canonicalize declared trailing-? keys in a JSON-originated map. The sole
  alias is the same key without the final question mark. Absence remains
  absence; conflicting canonical and alias values are refused."
  [value declared-keys]
  (if-not (map? value)
    {:ok true :value value}
    (let [conflicts
          (->> declared-keys
               (keep (fn [declared]
                       (let [spelling (name declared)
                             alias (keyword (subs spelling 0 (dec (count spelling))))]
                         (when (and (contains? value declared)
                                    (contains? value alias)
                                    (not= (get value declared) (get value alias)))
                           {:declared-key declared :alias-key alias
                            :declared-value (get value declared)
                            :alias-value (get value alias)}))))
               vec)]
      (if (seq conflicts)
        {:ok false :error/code :wire-predicate-key-conflict
         :findings [:wire-predicate-key-conflict]
         :conflicts conflicts}
        {:ok true
         :value
         (reduce (fn [normalized declared]
                   (let [spelling (name declared)
                         alias (keyword (subs spelling 0 (dec (count spelling))))
                         alias-present? (contains? normalized alias)
                         alias-value (get normalized alias)
                         declared-present? (contains? normalized declared)]
                     (cond-> (dissoc normalized alias)
                       (and (not declared-present?) alias-present?)
                       (assoc declared alias-value))))
                 value declared-keys)}))))

(def completion-contract
  {:path "holes/labs/M-apm-demonstration/role-cards/typed-completion-v1.md"
   :blob "d3351807f1597baf97e8ba7ae5605274f0f9a92c"})

(def authority-fields
  #{:job-id :dispatch/id :agent-id :frame-id :problem-id :phase :role
    :attempt-ordinal :submission/attempt :predecessor-job-id
    :fresh-session-nonce :memory-snapshot
    ;; The holdout travels with the job authority so every channel that
    ;; serves memories can enforce it. Without this the shelf and the
    ;; cascade withhold an id while the search channel still returns it.
    :shelf/holdout :shelf/withheld-ids})

(def checkpoint-authority-fields
  #{:solver/round :solver/strategy-checkpoint?})

(def memory-search-capable-roles
  #{:student :scribe :zai-scribe :promotion-proctor})

(def common-required #{:command-own-exit :outcome :failure-account :evidence})

(def evidence-shape-by-phase
  {:preflight {:mutations nil :clean-before? nil :clean-after? nil
               :statement-unchanged? nil :lean nil}
   :solve {:branch nil :base-revision nil :final-head nil :committed? nil
           :statement-unchanged? nil :lean nil :axioms nil :clean-before? nil
           :clean-after? nil :mutations nil}
   :verify {:branch nil :base-revision nil :final-head nil :committed? nil
            :statement-unchanged? nil :lean nil :axioms nil :clean-before? nil
            :clean-after? nil :mutations nil}
   ;; The downstream promotion validators distinguish the Solver-mining and
   ;; deposit forms; the common wrapper is deterministic for both.
   :promote-solver {}
   :promotion-review {:candidate-set-digest nil :base-problem-blob nil
                      :open-residuals nil :reviews nil}
   :student-attempt-1 {:memory-use {:used-ids nil}}
   :student-attempt-2 {:memory-use {:used-ids nil}}
   :student-attempt-3 {:memory-use {:used-ids nil}}
   :guide-intervention-1
   {:channel-audit {:direct-student-contact? nil}}
   :guide-intervention-2
   {:channel-audit {:direct-student-contact? nil}}
   :scribe-reduce {:lanes nil :dispositions nil :promotion-reviews nil}
   :close-frame {:trace-id nil :result nil}
   :analyst {:analysis nil}})

(def evidence-required-by-phase
  (update-vals evidence-shape-by-phase #(set (keys %))))

(defn evidence-shape [auth]
  (cond-> (get evidence-shape-by-phase (:phase auth))
    (and (= :scribe-reduce (:phase auth))
         (= :zai-scribe (:role auth)))
    (assoc :memory-candidates nil)
    (and (= :promote-solver (:phase auth))
         (= :scribe (:role auth)))
    (assoc :receipt nil)
    (and (= :solve (:phase auth))
         (true? (:solver/strategy-checkpoint? auth)))
    (assoc :solver/strategy nil)))

(defn evidence-required
  "Return the evidence schema owned by immutable dispatch authority.

   Strategy evidence is required only for an addressed Solver checkpoint;
   ordinary solve turns retain the ordinary proof-report schema."
  [auth]
  (let [shape (evidence-shape auth)]
    (when (some? shape) (set (keys shape)))))

(defn new-token [] (str (UUID/randomUUID)))

(defn prepare-request [request]
  (let [seed (select-keys request
                          [:dispatch/id :agent-id :frame-id :problem-id :phase
                           :role :attempt :submission/attempt
                           :session-id :memory-snapshot-id])]
    (assoc request :submission/token
           (machine/ledger-digest ["apm-role-submission" seed]))))

(defn canonical-job-id [request]
  (str "apm-role-" (machine/ledger-digest
                    ["apm-role-job" (:dispatch/id request)
                     (:agent-id request) (:phase request)
                     (:submission/attempt request)])))

(defn with-job-authority [request]
  (assoc request :submission/job-id (or (:submission/job-id request)
                                        (canonical-job-id request))))

(defn- record-path [job-id]
  (io/file *submission-root* (str job-id ".edn")))

(defn- read-record [job-id]
  (let [file (record-path job-id)]
    (when (.isFile file) (edn/read-string (slurp file)))))

(defn registered-job-ids-for-frame
  "Return job ids whose immutable typed-submission authority names FRAME-ID.
   This is the durable membership authority for frame jobs; the transport
   ledger records delivery but does not own campaign identity."
  [frame-id]
  (let [root (io/file *submission-root*)]
    (if-not (.isDirectory root)
      #{}
      (into #{}
            (comp
             (filter #(.isFile ^java.io.File %))
             (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
             (map #(edn/read-string (slurp ^java.io.File %)))
             (keep (fn [{:keys [authority]}]
                     (when (= frame-id (:frame-id authority))
                       (:job-id authority)))))
            (.listFiles root)))))

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
   (into (conj authority-fields :submission/token)
         checkpoint-authority-fields)))

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
        evidence-required (evidence-required auth)
        evidence-missing (if evidence-required
                           (set/difference evidence-required
                                           (set (keys (:evidence payload))))
                           #{})
        search-check
        (when (and (contains? memory-search-capable-roles (:role auth))
                   (not= :student (:role auth)))
          (let [receipts
                ((requiring-resolve
                  'futon3c.apm.role-memory-search/recorded-receipts-for-job)
                 (:job-id auth))]
            (if (and (contains? #{:scribe :promotion-proctor} (:role auth))
                     (empty? receipts))
              {:ok false :error/code :canonical-pattern-search-required}
              {:ok true :receipts receipts})))
        pattern-check (when (and (:ok search-check)
                                 (contains? #{:scribe :zai-scribe
                                              :promotion-proctor}
                                            (:role auth)))
                        ((requiring-resolve
                          'futon3c.apm.role-memory-search/validate-pattern-accounting)
                         (:receipts search-check) (:evidence payload)))
        student-memory-use (get-in payload [:evidence :memory-use])
        allowed-student-memory-use
        (->> generated-contract/required-submission-schemas
             :student-memory-use :role-authored-fields (map keyword) set)
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
                   (conj :failure-account-not-vector)
                   (and search-check (not (:ok search-check)))
                   (conj :memory-search-receipts-invalid)
                   (and pattern-check (not (:ok pattern-check)))
                   (conj :memory-search-pattern-accounting-invalid)
                   (and (= :student (:role auth))
                        (not (map? student-memory-use)))
                   (conj :student-memory-use-not-map)
                   (and (= :student (:role auth))
                        (map? student-memory-use)
                        (not (and (vector? (:used-ids student-memory-use))
                                  (every? string? (:used-ids
                                                   student-memory-use)))))
                   (conj :student-memory-used-ids-invalid)
                   (and (= :student (:role auth))
                        (map? student-memory-use)
                        (seq (set/difference (set (keys student-memory-use))
                                             allowed-student-memory-use)))
                   (conj :controller-derived-memory-field-supplied-by-agent))]
    (if (seq findings)
      {:ok false :error/code :role-submission-payload-invalid
       :findings findings :missing missing :evidence/missing evidence-missing
       :memory-search/check search-check :memory-search/pattern-check pattern-check}
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
             :evidence-shape (evidence-shape auth)
             :evidence-required
             (vec (sort (evidence-required auth)))})))

(defn authenticate
  "Return immutable controller authority for a valid job token.  Auxiliary
   role services use this boundary rather than trusting role-supplied identity."
  [job-id token]
  (let [record (read-record job-id)
        auth (:authority record)]
    (cond
      (nil? record) {:ok false :error/code :role-submission-authority-missing}
      (not= token (:submission/token auth))
      {:ok false :error/code :role-submission-token-mismatch}
      :else {:ok true :authority auth})))

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
        payload (str "/tmp/apm-role-" (:job-id ticket) ".json")
        search (when (contains? memory-search-capable-roles (:role request))
                 (str "\n# Search the open reviewed mathematics memory corpus; "
                      "the returned receipt is execution evidence:\n"
                      "/home/joe/code/futon3c/scripts/apm-search-memory.py"
                      " --job-id " (:job-id ticket)
                      " --token " (:submission/token request)
                      " --query 'YOUR QUERY'"))]
    (str (or search "")
         (when search "\n")
         base " --init --payload " payload
         "\n# edit " payload ", then submit:\n"
         base " --payload " payload)))
