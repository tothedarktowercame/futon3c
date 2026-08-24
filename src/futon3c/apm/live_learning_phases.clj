(ns futon3c.apm.live-learning-phases
  "Live Student/Guide/Scribe/close adapters for the APM map/reduce cycle."
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-handlers :as handlers]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.role-memory-search :as role-memory]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.apm.workspace-lifecycle :as workspace-lifecycle])
  (:import [java.nio.file Path]
           [java.util UUID]))

(def role-for-kind
  {:student-attempt :student :guide-intervention :guide
   :scribe-reduce :scribe :close-frame :guide})

(defn- producer-phase [contract artifact]
  (some (fn [phase]
          (when (contains? (get-in contract [:phases phase :produces]) artifact)
            phase))
        (:phase-order contract)))

(defn required-input-receipt-ids [contract phase receipts]
  (->> (get-in contract [:phases phase :requires])
       (map #(producer-phase contract %))
       distinct
       (keep #(get-in receipts [% :receipt/id]))
       set))

(defn build-request
  [{:keys [contract action ledger unit role-card seat workspace receipts
           snapshot-access turn-timeout-ms terminal-budgets]
    :or {turn-timeout-ms 3600000}}]
  (let [kind (:kind action)
        phase (:phase action)
        phase-ordinal (get-in contract [:phases phase :ordinal])
        attempt-ordinal (or (:ordinal action)
                            phase-ordinal
                            ({:student-attempt-1 1
                              :student-attempt-2 2
                              :student-attempt-3 3} phase))
        role (role-for-kind kind)
        expected-agent (str (:frame/id unit) "-" (name role))
        terminal-budget (merge driver/default-terminal-budget
                               (get terminal-budgets role))
        input-ids (required-input-receipt-ids contract phase receipts)
        ;; The Student binds to the latest reviewed snapshot: a Guide's
        ;; union snapshot when one was published, else the Solver promotion.
        promotion-receipt (when (= :student-attempt kind)
                            (handlers/latest-snapshot-receipt
                             receipts (or attempt-ordinal 1)))
        required-artifacts (get-in contract [:phases phase :requires])
        expected-input-count (count (distinct (map #(producer-phase contract %)
                                                   required-artifacts)))
        findings (cond-> []
                   (nil? role) (conj :learning-kind-invalid)
                   (not= (:frame/id unit) (:frame-id action)) (conj :frame-mismatch)
                   (not= (:problem/id unit) (:problem-id action)) (conj :problem-mismatch)
                   (not= expected-agent (:agent-id seat)) (conj :seat-mismatch)
                   (not (true? (:invoke-ready? seat))) (conj :seat-not-ready)
                   (and (some? (:ordinal action))
                        (not= (:ordinal action) phase-ordinal))
                   (conj :action-ordinal-mismatch)
                   (not (and (string? (:path role-card)) (string? (:blob role-card))))
                   (conj :role-card-pin-missing)
                   (not (every? pos-int? (vals terminal-budget)))
                   (conj :terminal-budget-invalid)
                   (not= expected-input-count (count input-ids))
                   (conj :required-input-receipts-missing)
                   (and (= :student-attempt kind)
                        (not (string? (:workspace/path workspace))))
                   (conj :student-workspace-missing)
                   (and (= :student-attempt kind)
                        (not (contains? #{1 2 3} attempt-ordinal)))
                   (conj :student-attempt-ordinal-missing)
                   (and (= :student-attempt kind) promotion-receipt
                        (not (and (:ok snapshot-access)
                                  (= (:receipt/snapshot-digest promotion-receipt)
                                     (get-in snapshot-access
                                             [:snapshot :snapshot/digest])))))
                   (conj :student-snapshot-access-unverified))
        findings (cond-> findings
                   (and (= :scribe-reduce kind) (= :promote-solver phase)
                        (not (and (string? (get-in unit [:problem :blob]))
                                  (string? (get-in unit [:problem :path]))
                                  (string? (:receipt/final-head
                                            (get receipts :solve))))))
                   (conj :promotion-residual-inputs-missing))]
    (if (seq findings)
      {:ok false :error/code :live-learning-request-invalid :findings findings}
      (let [body (cond-> {:dispatch/type kind :phase phase :role role
                          :agent-id (:agent-id seat)
                          :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                          :ledger-digest (:digest ledger)
                          :role-card-path (:path role-card) :role-card-blob (:blob role-card)
                          :input-receipt-ids input-ids
                          :terminal-budget terminal-budget
                          :turn-timeout-ms turn-timeout-ms}
                   (= :student-attempt kind)
                   (assoc :attempt-ordinal attempt-ordinal
                          :workspace (:workspace/path workspace)
                          :fresh-session? true
                          :fresh-session-nonce (str (UUID/randomUUID)))
                   ;; The base is what each fresh attempt is reset to and what
                   ;; the archived source is measured against.
                   (and (= :student-attempt kind)
                        (string? (:base-revision workspace)))
                   (assoc :base-revision (:base-revision workspace)
                          :problem-path (:problem/path workspace))
                   (and (= :student-attempt kind) promotion-receipt)
                   (assoc :memory-snapshot
                          {:receipt-id (:receipt/id promotion-receipt)
                           :snapshot-id (:receipt/snapshot-id promotion-receipt)
                           :snapshot-digest (:receipt/snapshot-digest promotion-receipt)
                           :accessible-memory-ids
                           (vec (sort (:accessible-memory-ids snapshot-access)))})
                   (and (= :scribe-reduce kind) (= :promote-solver phase))
                   (assoc :base-problem-blob (get-in unit [:problem :blob])
                          :problem-path (get-in unit [:problem :path])
                          :solver-final-head
                          (:receipt/final-head (get receipts :solve)))
                   (= :guide-intervention kind)
                   (assoc :intervention-ordinal phase-ordinal
                          :input-attempt-id
                          (:receipt/id (get receipts
                                           (keyword (str "student-attempt-"
                                                         phase-ordinal))))
                          ;; Reviewer inputs for a store-mode deposit: the
                          ;; promotion Proctor card refuses to judge without
                          ;; the base blob and the Solver's final head.
                          :base-problem-blob (get-in unit [:problem :blob])
                          :problem-path (get-in unit [:problem :path])
                          :solver-final-head
                          (:receipt/final-head (get receipts :solve))
                          :prior-snapshot
                          (let [prior (handlers/latest-snapshot-receipt
                                       receipts (inc phase-ordinal))]
                            (some-> (handlers/snapshot-binding prior)
                                    (assoc :snapshot-path
                                           (:receipt/snapshot-path prior))))))]
        {:ok true :request (submission/prepare-request
                            (assoc body :dispatch/id
                                   (machine/ledger-digest [body])))}))))

(defn- canonical-close-result [result]
  (cond
    (keyword? result) result
    (string? result) (keyword result)
    :else nil))

(defn validate-terminal [request ticket job]
  (let [kind (:dispatch/type request)
        report (:report job)
        ;; JSON role submissions have historically emitted the query ledger
        ;; beside :memory-use even though the canonical EDN shape nests it.
        ;; Preserve that observation losslessly at the consumer boundary; a
        ;; missing query ledger in both locations still fails closed below.
        memory-use (cond-> (:memory-use report)
                     (and (map? (:memory-use report))
                          (not (contains? (:memory-use report) :queries))
                          (vector? (:queries report)))
                     (assoc :queries (:queries report)))
        snapshot-binding (select-keys memory-use
                                      [:receipt-id :snapshot-id
                                       :snapshot-digest])
        search-authority (get-in job [:typed-submission :authority])
        search-receipt-ids (:memory-search-receipt-ids report)
        search-check (when (= :student-attempt kind)
                       (if (map? search-authority)
                         (role-memory/validate-claims search-authority
                                                     search-receipt-ids)
                         {:ok true :receipts []}))
        searched-memory-ids
        (if (:ok search-check)
          (set (mapcat role-memory/receipt-surfaced-ids
                       (:receipts search-check)))
          #{})
        predecessor-search-ids
        (role-memory/recorded-surfaced-ids-for-job
         (:repair/of-job-id request))
        allowed-memory-ids
        (into (set (get-in request [:memory-snapshot
                                    :accessible-memory-ids]))
              (concat searched-memory-ids predecessor-search-ids))
        surfaced-memory-ids (set (:surfaced-ids memory-use))
        used-memory-ids (set (:used-ids memory-use))
        findings
        (cond-> []
          (not= (:job-id ticket) (:job-id job)) (conj :job-id-mismatch)
          (not= (:agent-id request) (:agent-id job)) (conj :agent-id-mismatch)
          (not= :done (:state job)) (conj :job-not-done)
          (not= 0 (:command-own-exit report)) (conj :command-own-exit-nonzero)
          (not= (:frame-id request) (:frame-id report)) (conj :frame-mismatch)
          (not= (:problem-id request) (:problem-id report)) (conj :problem-mismatch)
          (and (= :student-attempt kind)
               (not (string? (:session-id job)))) (conj :fresh-session-id-missing)
          (and (= :student-attempt kind)
               (not (map? (:memory-use report)))) (conj :memory-use-evidence-missing)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (and (vector? (:surfaced-ids memory-use))
                         (vector? (:used-ids memory-use))
                         (vector? (:queries memory-use))
                         (every? string? (:queries memory-use)))))
          (conj :student-memory-use-ids-invalid)
          (and (= :student-attempt kind)
               (:memory-snapshot request)
               (not= (:memory-snapshot request)
                     (assoc snapshot-binding
                            :accessible-memory-ids
                            (get-in request [:memory-snapshot
                                             :accessible-memory-ids]))))
          (conj :student-memory-snapshot-mismatch)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (:ok search-check)))
          (conj :student-memory-search-receipts-invalid)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (every? allowed-memory-ids surfaced-memory-ids)))
          (conj :student-memory-surfaced-outside-snapshot)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (every? surfaced-memory-ids used-memory-ids)))
          (conj :student-memory-used-without-surfacing)
          (and (= :guide-intervention kind)
               (not= false (get-in report [:channel-audit :direct-student-contact?])))
          (conj :guide-channel-isolation-unproved)
          ;; Store-mode candidates are the Guide's channel to the Student's
          ;; shelf; they must be gate-shaped here so the reviewer never sees
          ;; an unbound candidate, and harness-mode may not carry any.
          (and (= :guide-intervention kind)
               (some? (:candidates report))
               (not (and (vector? (:candidates report))
                         (:ok (pipeline/validate-guide-deposit
                               {:depositor (:agent-id request)
                                :candidates (:candidates report)})))))
          (conj :guide-candidates-invalid)
          (and (= :guide-intervention kind)
               (seq (:candidates report))
               (not= "store-mode" (some-> (:mode report) name)))
          (conj :guide-candidates-outside-store-mode)
          (and (= :scribe-reduce kind)
               (not (every? #(coll? (get report %))
                            [:lanes :dispositions :promotion-reviews])))
          (conj :scribe-reduction-evidence-missing)
          (and (= :promote-solver (:phase request))
               (not (and (vector? (:memory-candidates report))
                         (seq (:memory-candidates report)))))
          (conj :solver-promotion-candidates-invalid)
          (and (= :close-frame kind)
               (not (and (string? (:trace-id report))
                         (contains? #{:closed :partial}
                                    (canonical-close-result
                                     (:result report))))))
          (conj :close-evidence-invalid))]
    (if (seq findings)
      {:ok false :error/code :live-learning-terminal-invalid :findings findings}
      {:ok true :report report})))

(defn receipt [contract action receipts request ticket job validated]
  (let [kind (:kind action)
        report (:report validated)
        common {:receipt/frame-id (:frame-id request)
                :receipt/problem-id (:problem-id request)}
        body
        (merge common
               (case kind
                 :student-attempt
                 (cond-> {:receipt/type :student-attempt
                          :receipt/attempt-ordinal (:attempt-ordinal request)
                          :receipt/fresh-session-id (:session-id job)
                          :receipt/job-id (:job-id ticket)
                          :receipt/outcome (:outcome report)
                          :receipt/failure-account (:failure-account report)
                          :receipt/memory-use (:memory-use report)
                          :receipt/memory-snapshot
                          (select-keys (:memory-snapshot request)
                                       [:receipt-id :snapshot-id :snapshot-digest])}
                   (map? (:source validated))
                   (assoc :receipt/source (:source validated))
                   (map? (:candidate validated))
                   (assoc :receipt/candidate (:candidate validated)))
                 :guide-intervention
                 (let [snapshot (:memory-snapshot report)]
                   (cond-> {:receipt/type :guide-intervention
                            :receipt/intervention-ordinal
                            (get-in contract [:phases (:phase action) :ordinal])
                            :receipt/mode (some-> (:mode report) keyword)
                            :receipt/input-attempt-id
                            (:receipt/id
                             (get receipts
                                  (keyword
                                   (str "student-attempt-"
                                        (get-in contract
                                                [:phases (:phase action) :ordinal])))))
                            :receipt/effect (:effect report)
                            :receipt/channel-audit (:channel-audit report)}
                     ;; Present only when store-mode candidates were reviewed
                     ;; and a union snapshot published; the next Student
                     ;; attempt binds to it.
                     (string? (:snapshot-digest snapshot))
                     (assoc :receipt/snapshot-id (:snapshot-id snapshot)
                            :receipt/snapshot-digest (:snapshot-digest snapshot)
                            :receipt/snapshot-path (:snapshot-path snapshot)
                            :receipt/reviewed-memory-ids
                            (:reviewed-memory-ids snapshot)
                            :receipt/promotion-reviews
                            (:promotion-reviews snapshot)
                            :receipt/independent-review? true)))
                 :scribe-reduce
                 (if (= :promote-solver (:phase action))
                   {:receipt/type :solver-promotion
                    :receipt/input-receipt-ids (:input-receipt-ids request)
                    :receipt/lanes (:lanes report)
                    :receipt/dispositions (:dispositions report)
                    :receipt/promotion-reviews (:promotion-reviews report)
                    :receipt/snapshot-id (get-in report [:memory-snapshot :snapshot-id])
                    :receipt/snapshot-digest
                    (get-in report [:memory-snapshot :snapshot-digest])
                    :receipt/snapshot-path
                    (get-in report [:memory-snapshot :snapshot-path])
                    :receipt/reviewed-memory-ids
                    (get-in report [:memory-snapshot :reviewed-memory-ids])
                    :receipt/independent-review? true}
                   {:receipt/type :scribe-reduce
                    :receipt/input-receipt-ids (:input-receipt-ids request)
                    :receipt/lanes (:lanes report)
                    :receipt/dispositions (:dispositions report)
                    :receipt/promotion-reviews (:promotion-reviews report)})
                 :close-frame
                 (let [observation-missing?
                       (some #(= :student-observation-missing (:receipt/type %))
                             (vals receipts))]
                   {:receipt/type :frame-close
                  :receipt/input-receipt-ids (:input-receipt-ids request)
                  :receipt/trace-id (:trace-id report)
                  :receipt/result (if observation-missing?
                                    :partial
                                    (canonical-close-result (:result report)))
                  :receipt/learning-outcome (if observation-missing?
                                              :partially-observed
                                              :observed)})))
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))]
    (handlers/validate-completion contract action addressed receipts)))

(defn missing-observation-receipt
  "Controller evidence that a Student job ended without a valid typed receipt.
  It is an alternate observation producer, never a Student-authored attempt."
  ([contract action receipts request ticket job repair-attempts collection-evidence]
   (missing-observation-receipt contract action receipts request ticket job
                                repair-attempts collection-evidence nil))
  ([contract action receipts request ticket job repair-attempts collection-evidence
    archive-fn]
   (missing-observation-receipt contract action receipts request ticket job
                                repair-attempts collection-evidence archive-fn nil))
  ([contract action receipts request ticket job repair-attempts collection-evidence
    archive-fn candidate-fn]
  (let [workspace (:workspace request)
        head-result (when (string? workspace)
                      (shell/sh "git" "-C" workspace "rev-parse" "HEAD"))
        ;; The source is archived even without a typed receipt: an
        ;; unobserved attempt's worktree is still evidence.
        archived (when (fn? archive-fn) (archive-fn))
        candidate (when (fn? candidate-fn) (candidate-fn))
        body (cond-> {:receipt/type :student-observation-missing
              :receipt/frame-id (:frame-id request)
              :receipt/problem-id (:problem-id request)
              :receipt/attempt-ordinal (:attempt-ordinal request)
              :receipt/job-id (:job-id ticket)
              :receipt/author :controller
              :receipt/reason :typed-submission-missing
              :receipt/repair-attempts repair-attempts
              :receipt/memory-snapshot
              (select-keys (:memory-snapshot request)
                           [:receipt-id :snapshot-id :snapshot-digest])
              :receipt/harness-observed
              {:job (select-keys job [:job-id :agent-id :state :terminal-code
                                      :session-id])
               :collection collection-evidence
               :workspace {:path workspace
                           :head (when (and head-result (zero? (:exit head-result)))
                                   (str/trim (:out head-result)))
                           :source (if (:ok archived)
                                     (:source archived)
                                     (some-> archived
                                             (select-keys [:error/code :path])))
                           :candidate (when candidate
                                        (if (:ok candidate)
                                          (:candidate candidate)
                                          (select-keys candidate
                                                       [:error/code :head :ref])))}
               :memory {:snapshot (:memory-snapshot request)}}}
               (and candidate (:ok candidate))
               (assoc :receipt/candidate (:candidate candidate)))
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))]
    (if (and candidate (not (:ok candidate)))
      candidate
      (handlers/validate-completion contract action addressed receipts)))))

(defn prepare-student-workspace!
  "Before an original fresh Student attempt, return the Student worktree to
  its registered base so attempt k+1 cannot read attempt k's work. Repairs
  re-dispatch the same attempt and keep the worktree."
  [request reset-fn]
  (cond
    (not (and (= :student-attempt (:dispatch/type request))
              (true? (:fresh-session? request))
              (nil? (:repair/attempt request))))
    {:ok true :status :not-applicable}

    (not (and (string? (:workspace request))
              (string? (:base-revision request))))
    {:ok false :error/code :student-workspace-base-unknown}

    :else
    (let [reset (reset-fn {:workspace/path (:workspace request)
                           :base-revision (:base-revision request)
                           :problem/path (:problem-path request)})]
      (if (:ok reset)
        {:ok true :status :reset :reset reset}
        {:ok false :error/code :student-workspace-reset-failed
         :finding reset}))))

(defn source-archive-directory [state-path phase]
  (str (.resolveSibling (Path/of (str state-path) (make-array String 0))
                        (str (name phase) "-source"))))

(defn archive-student-source!
  "Archive the Student's problem file beside the phase state before the
  worktree is reset for the next attempt or retired with the frame."
  [request state-path archive-fn]
  (if-not (and (string? (:workspace request))
               (string? (:problem-path request)))
    {:ok false :error/code :student-source-unknown}
    (archive-fn {:workspace/path (:workspace request)
                 :problem/path (:problem-path request)
                 :archive-directory (source-archive-directory
                                     state-path (:phase request))})))

(defn prompt [request]
  (str (str/upper-case (:frame-id request)) " " (name (:phase request))
       " — follow frozen role card "
       (:role-card-path request) " at blob " (:role-card-blob request) ".\n"
       "Authority and exact receipt inputs:\n" (pr-str request) "\n"
       (case (:dispatch/type request)
         :student-attempt
         (str "Attempt the problem independently. The :memory-snapshot map is "
              "the reviewed starting shelf. You may also use the controller-owned "
              "open mathematics search command; any additionally surfaced memory "
              "must be covered by typed :memory-search-receipt-ids (including "
              "receipts recorded by an explicit terminal-repair predecessor). "
              "Return :memory-use "
              "with the exact :receipt-id, :snapshot-id, and :snapshot-digest "
              "from the request, plus vector-valued :surfaced-ids and :used-ids. "
              "Also return vector-valued :queries containing the exact search "
              "strings used (an empty vector means no query was run). "
              "Record an explicit failure account even on success.")
         :guide-intervention "Improve only the memory store or harness channel. Do not contact the Student directly."
         :scribe-reduce (if (= :promote-solver (:phase request))
                          (str "Mine the verified Solver trace and return memory "
                               "candidates plus all four typed lane entries. Each "
                               "lane is {:lane KEYWORD :status :ran|:ran-empty|:not-run}; "
                               "empty or unrun lanes require a nonblank :reason. "
                               "The controller owns independent review and snapshot publication.")
                          "Reduce the certified receipts into lanes, dispositions, and promotion reviews.")
         :close-frame "Audit the complete receipt graph and return a content-addressable trace result.")
       (if-let [job-id (:submission/job-id request)]
         (str " Completion is accepted only through the typed submission tool; "
              "follow the shared completion contract "
              (pr-str submission/completion-contract) ". "
              "conversational output is never a receipt. Run the template command, "
              "fill every null in the generated JSON, then run the submit command:\n"
              (submission/command request {:job-id job-id})
              "\nFix any field-level errors before ending the turn.")
         " Await activation before submitting completion.")))

(defn terminal-repair-request
  "Create the sole authority-preserving repair dispatch for an invalid typed
  role terminal. The rejected findings become durable request data."
  [request ticket job failure]
  (let [contract-migration?
        (= :typed-submission-contract-migration (:repair/kind failure))
        migration-nonce (when contract-migration?
                          (machine/ledger-digest
                           [(:dispatch/id request) (:ticket/id ticket)
                            (:job-id job) submission/completion-contract]))
        body (-> request
                 (dissoc :dispatch/id)
                 (assoc :fresh-session? contract-migration?
                        :repair/attempt (if contract-migration?
                                          :typed-contract-migration-1
                                          (:repair/next-attempt failure 1))
                        :repair/of-job-id (:job-id job)
                        :repair/of-ticket-id (:ticket/id ticket)
                        :repair/findings (vec (:findings failure)))
                 (cond-> contract-migration?
                   (assoc :fresh-session-nonce migration-nonce
                          :repair/kind :typed-submission-contract-migration)))]
    {:ok true :request (submission/prepare-request
                        (assoc body :dispatch/id
                               (machine/ledger-digest [body])))}))

(declare guide-promotion-step!)

(defn run-live!
  [{:keys [contract action receipts request state-path agency-base
           snapshot-publish-fn workspace-reset-fn source-archive-fn
           student-candidate-fn preparation guide-promotion]
    :or {agency-base "http://localhost:7070"
         workspace-reset-fn workspace-lifecycle/reset-to-base!
         source-archive-fn workspace-lifecycle/archive-problem-source!
         student-candidate-fn workspace-lifecycle/preserve-student-candidate!}}]
  (driver/drive!
   {:request request :state (runtime/read-state state-path)
    :announce-fn
    (fn [req]
      (let [req (submission/with-job-authority req)
            announced (job-port/announce!
                       agency-base
                       {:agent-id (:agent-id req) :prompt (prompt req)
                        :job-id (:submission/job-id req)})]
        announced))
    :activate-fn
    (fn [req ticket]
      (let [prepared (prepare-student-workspace! req workspace-reset-fn)
            reset-response (when (and (:ok prepared) (:fresh-session? req))
                             (runtime/http-json
                              "POST" (str agency-base "/api/alpha/agents/"
                                          (:agent-id req) "/reset-session") {}))
            reset-ok? (or (nil? reset-response)
                          (and (= 200 (:http/status reset-response))
                               (:ok reset-response)))]
        (cond
          (not (:ok prepared)) prepared
          (not reset-ok?)
          {:ok false :error/code :student-session-reset-failed}
          :else
          (job-port/activate!
           agency-base
           {:agent-id (:agent-id req)
            :prompt (prompt (submission/with-job-authority req))
            :job-id (:job-id ticket)}))))
    :job-fn
    (fn [job-id]
      (job-port/observe agency-base job-id))
    :cancel-fn
    (fn [job-id]
      (job-port/cancel! agency-base job-id
                        "typed-submission activation supersession"))
    :persist-fn #(runtime/atomic-persist! state-path %)
    :ticket-register-fn submission/register!
    :terminal-submission-provider (fn [_ ticket _]
                                    (submission/submitted (:job-id ticket)))
    :terminal-validator validate-terminal
    :terminal-repair-request-fn terminal-repair-request
    :terminal-budget-config (or (:terminal-budget request)
                                driver/default-terminal-budget)
    :missing-observation-provider
    (when (= :student-attempt (:kind action))
      (fn [request ticket job repair-attempts collection-evidence]
        (missing-observation-receipt contract action receipts request ticket job
                                     repair-attempts collection-evidence
                                     #(archive-student-source!
                                       request state-path source-archive-fn)
                                     #(student-candidate-fn
                                       {:lease (get-in preparation
                                                       [:workspaces :student])
                                        :attempt-ordinal
                                        (:attempt-ordinal request)}))))
    :receipt-provider
    (fn [request ticket job validated]
      (cond
        (= :promote-solver (:phase action))
        (if-not (fn? snapshot-publish-fn)
          {:ok false :error/code :solver-snapshot-publisher-missing}
          (let [published (snapshot-publish-fn (:report validated))]
            (if-not (:ok published)
              published
              (let [snap (:snapshot published)
                    report (assoc (:report validated) :memory-snapshot
                                  {:snapshot-id (:snapshot/id snap)
                                   :snapshot-digest (:snapshot/digest snap)
                                   :snapshot-path (:path published)
                                   :reviewed-memory-ids
                                   (mapv :memory-id (:snapshot/memories snap))
                                   :independent-review? true})]
                (receipt contract action receipts request ticket job
                         (assoc validated :report report))))))

        ;; Preserve and compile before certifying: the receipt names an exact
        ;; Git candidate, so a dirty but valid Student result cannot vanish at
        ;; the next reset or retirement boundary.
        (= :student-attempt (:kind action))
        (let [candidate (student-candidate-fn
                         {:lease (get-in preparation [:workspaces :student])
                          :attempt-ordinal (:attempt-ordinal request)})]
          (if-not (:ok candidate)
            candidate
            (let [archived (archive-student-source! request state-path
                                                    source-archive-fn)]
              (if-not (:ok archived)
                archived
                (receipt contract action receipts request ticket job
                         (assoc validated
                                :source (:source archived)
                                :candidate (:candidate candidate)))))))

        ;; A store-mode Guide deposit is reviewed independently and published
        ;; as a union snapshot before the Guide receipt exists, so the receipt
        ;; can carry the snapshot the next Student attempt binds to.
        (and (= :guide-intervention (:kind action))
             (seq (get-in validated [:report :candidates])))
        (if-not (map? guide-promotion)
          {:ok false :error/code :guide-promotion-driver-missing}
          (let [stepped (guide-promotion-step! guide-promotion request
                                               (:report validated))]
            (if (= :certified (:status stepped))
              (receipt contract action receipts request ticket job
                       (assoc-in validated [:report :memory-snapshot]
                                 (:memory-snapshot stepped)))
              stepped)))

        :else
        (receipt contract action receipts request ticket job validated)))}))

(defn guide-promotion-step!
  "Drive the independent review of a Guide's store-mode candidates. The
  review state lives beside the Guide phase state; RUN-FN steps the durable
  promotion machine from it. Returns :awaiting-terminal until the reviewer's
  verdicts are published, then :certified with the union snapshot."
  [{:keys [state-path run-fn]} request report]
  (let [state-path (Path/of (str state-path) (make-array String 0))
        state (runtime/read-state state-path)]
    (cond
      (nil? state)
      (let [gated (pipeline/validate-guide-deposit
                   {:depositor (:agent-id request)
                    :candidates (:candidates report)})]
        (if-not (:ok gated)
          {:ok false :error/code :guide-candidates-invalid
           :findings (:findings gated)}
          (let [seeded {:state/type :promotion :stage :review-pending
                        :deposit {:depositor (:agent-id request)
                                  :dispatch/id (:dispatch/id request)
                                  :prior-snapshot (:prior-snapshot request)}
                        :candidates (:candidates gated)}
                persisted (runtime/atomic-persist! state-path seeded)]
            (if-not (:ok persisted)
              {:ok false :error/code :guide-promotion-persistence-failed}
              (run-fn)))))

      (= :promotion-certified (:state/type state))
      (let [published (:receipt state)]
        {:ok true :status :certified
         :memory-snapshot
         {:snapshot-id (:receipt/snapshot-id published)
          :snapshot-digest (:receipt/snapshot-digest published)
          :snapshot-path (:receipt/snapshot-path published)
          :reviewed-memory-ids (:receipt/reviewed-memory-ids published)
          :promotion-reviews (:receipt/promotion-reviews published)
          :independent-review? true}})

      :else
      (let [stepped (run-fn)]
        (if (= :certified (:status stepped))
          (guide-promotion-step! {:state-path state-path :run-fn run-fn}
                                 request report)
          stepped)))))
