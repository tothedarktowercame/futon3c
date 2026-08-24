(ns futon3c.apm.live-proof-phases
  "Live request and terminal-receipt adapters for preflight, solve, and verify."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-contract :as cycle]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-preflight :as preflight]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-solver-rounds :as solver-rounds]
            [futon3c.apm.typed-role-submission :as submission]))

(def permitted-axioms '#{propext Classical.choice Quot.sound})

(def proof-report-fields
  #{:command-own-exit :branch :base-revision :final-head :committed?
    :statement-unchanged? :lean :axioms :clean-before? :clean-after? :mutations})

(defn normalize-proof-report
  "Normalize equivalent agent renderings before applying the strict gate.

   This accepts only lossless shape variations observed at the live boundary;
   it does not manufacture a successful exit, cleanliness, or proof result."
  [report]
  (let [lean (:lean report)
        warnings (:warnings lean)
        errors (:errors lean)
        axiom-text (when (string? (:axioms report)) (:axioms report))
        bracketed (when axiom-text (second (re-find #"\[([^]]*)\]" axiom-text)))
        axioms (cond
                 bracketed
                 (->> (str/split bracketed #",")
                      (map str/trim) (remove str/blank?) (map symbol) vec)

                 (and (sequential? (:axioms report))
                      (every? string? (:axioms report)))
                 (mapv symbol (:axioms report)))]
    (cond-> report
      (vector? warnings)
      (assoc-in [:lean :warnings] (count warnings))
      (vector? warnings)
      (assoc-in [:lean :sorry-warnings]
                (count (filter #(str/includes? (str %) "sorry") warnings)))
      (vector? errors)
      (assoc-in [:lean :errors] (count errors))
      (and (nil? errors) (vector? warnings))
      (assoc-in [:lean :errors] 0)
      axioms (assoc :axioms axioms)
      (and (nil? (:solver/strategy report))
           (map? (:solver/strategy lean)))
      (assoc :solver/strategy (:solver/strategy lean)))))

(defn- address-request [body]
  (assoc body :dispatch/id (machine/ledger-digest [body])))

(defn request-replay-compatible?
  "Accept a persisted request when fresh authority differs only by migration
   from a control-root-relative card path to its canonical absolute spelling.
   Historical dispatch identity and submission token are never rewritten."
  [current persisted]
  (let [canonicalize
        (fn [request]
          (-> request
              (update :role-card-path
                      (fn [path]
                        (when path
                          (str (.normalize
                                (.toAbsolutePath
                                 (java.nio.file.Path/of
                                  path (make-array String 0))))))))
              (dissoc :dispatch/id :submission/token)))]
    (= (canonicalize current) (canonicalize persisted))))

(defn build-request
  [{:keys [kind action ledger unit role-card checkpoint-role-card seat workspace solve-receipt
           terminal-budget expected-agent-id]}]
  (if (= :preflight kind)
    (let [problem (:problem unit)
          findings (cond-> []
                     (not= (:revision problem) (:base-revision workspace))
                     (conj :workspace-base-mismatch)
                     (not= (:blob problem) (:problem/blob workspace))
                     (conj :workspace-blob-mismatch)
                     (not (string? (:workspace/path workspace)))
                     (conj :workspace-path-missing))]
      (if (seq findings)
        {:ok false :error/code :preflight-workspace-invalid
         :findings findings}
        (update (preflight/build-request
                 {:ledger ledger
                  :unit (assoc-in unit [:problem :repository]
                                  (:workspace/path workspace))
                  :role-card role-card :seat seat
                  :terminal-budget (or terminal-budget
                                       driver/default-terminal-budget)
                  :timeouts
                  {:request-timeout-ms (get-in action [:timeouts :request-ms])
                   :turn-timeout-ms (get-in action [:timeouts :turn-ms])}})
                :request #(submission/prepare-request (assoc % :role :proctor)))))
    (let [problem (:problem unit)
          expected-role (if (= :solve kind) :solver :proctor)
          expected-agent (or expected-agent-id
                             (str (:frame/id unit) "-" (name expected-role)))
          findings (cond-> []
                     (not (contains? #{:solve :verify} kind)) (conj :proof-kind-invalid)
                     (not= (:frame/id unit) (:frame-id action)) (conj :frame-mismatch)
                     (not= (:problem/id unit) (:problem-id action)) (conj :problem-mismatch)
                     (not= expected-agent (:agent-id seat)) (conj :seat-mismatch)
                     (not (true? (:invoke-ready? seat))) (conj :seat-not-ready)
                     (not= (:revision problem) (:base-revision workspace))
                     (conj :workspace-base-mismatch)
                     (not= (:blob problem) (:problem/blob workspace))
                     (conj :workspace-blob-mismatch)
                     (not (and (string? (:path role-card)) (string? (:blob role-card))))
                     (conj :role-card-pin-missing)
                     (and (= :solve kind)
                          (not (and (string? (:path checkpoint-role-card))
                                    (string? (:blob checkpoint-role-card)))))
                     (conj :solver-restrategize-role-card-pin-missing)
                     (and (= :verify kind) (not (string? (:receipt/id solve-receipt))))
                     (conj :solve-receipt-missing))]
      (if (seq findings)
        {:ok false :error/code :live-proof-request-invalid :findings findings}
        {:ok true
         :request
         (submission/prepare-request
          (address-request
           (cond-> {:dispatch/type (keyword (str "frame-" (name kind)))
                   :phase kind :agent-id (:agent-id seat)
                   :role expected-role
                   :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                   :ledger-digest (:digest ledger)
                   :role-card-path (:path role-card) :role-card-blob (:blob role-card)
                   :workspace (:workspace/path workspace)
                   :branch (:branch workspace) :base-revision (:revision problem)
                   :problem-path (:path problem) :problem-blob (:blob problem)
                   :terminal-budget (or terminal-budget
                                        driver/default-terminal-budget)
                   :turn-timeout-ms (get-in action [:timeouts :turn-ms])}
            (= :solve kind)
            (assoc :solver/regular-role-card-path (:path role-card)
                   :solver/regular-role-card-blob (:blob role-card)
                   :solver/restrategize-role-card-path (:path checkpoint-role-card)
                   :solver/restrategize-role-card-blob (:blob checkpoint-role-card))
            (= :verify kind) (assoc :solve-receipt-id (:receipt/id solve-receipt)
                                    :certified-final-head
                                    (:receipt/final-head solve-receipt)))))}))))

(defn validate-terminal [kind request ticket job]
  (let [report (normalize-proof-report (:report job))
        missing (set/difference proof-report-fields (set (keys report)))
        lean (:lean report)
        findings
        (cond-> []
          (not= (:job-id ticket) (:job-id job)) (conj :job-id-mismatch)
          (not= (:agent-id request) (:agent-id job)) (conj :agent-id-mismatch)
          (not= :done (:state job)) (conj :job-not-done)
          (seq missing) (conj :proof-report-fields-missing)
          (not= 0 (:command-own-exit report)) (conj :command-own-exit-nonzero)
          (not= (:branch request) (:branch report)) (conj :branch-mismatch)
          (not= (:base-revision request) (:base-revision report))
          (conj :base-revision-mismatch)
          (not (true? (:committed? report))) (conj :final-head-not-committed)
          (not (and (string? (:final-head report))
                    (re-matches #"[0-9a-f]{40}" (:final-head report))))
          (conj :final-head-invalid)
          (and (= :verify kind)
               (not= (:certified-final-head request) (:final-head report)))
          (conj :verify-final-head-mismatch)
          (not (true? (:statement-unchanged? report))) (conj :statement-changed)
          (not (and (= 0 (:exit lean))
                    (= 0 (:sorry-warnings lean))
                    (= 0 (:errors lean))
                    (nat-int? (:warnings lean))))
          (conj :lean-proof-invalid)
          (not (set/subset? (set (:axioms report)) permitted-axioms))
          (conj :axioms-not-permitted)
          (not (true? (:clean-before? report))) (conj :workspace-not-clean-before)
          (not (true? (:clean-after? report))) (conj :workspace-not-clean-after)
          (not (set/subset? (set (:mutations report)) #{(:problem-path request)}))
          (conj :mutation-outside-problem-file))]
    (if (seq findings)
      {:ok false :error/code :live-proof-terminal-invalid
       :findings findings :missing missing}
      {:ok true :report report})))

(defn receipt [contract kind request ticket _job validated]
  (let [report (:report validated)
        body (case kind
               :solve {:receipt/type :frame-solve
                       :receipt/frame-id (:frame-id request)
                       :receipt/problem-id (:problem-id request)
                       :receipt/job-id (:job-id ticket)
                       :receipt/final-head (:final-head report)
                       :receipt/lean (:lean report)
                       :receipt/axioms (:axioms report)
                       :receipt/statement-unchanged? true}
               :verify {:receipt/type :frame-verify
                        :receipt/frame-id (:frame-id request)
                        :receipt/problem-id (:problem-id request)
                        :receipt/job-id (:job-id ticket)
                        :receipt/solve-receipt-id (:solve-receipt-id request)
                        :receipt/final-head (:final-head report)
                        :receipt/mathematical-sound? true})
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))
        checked (cycle/validate-receipt contract kind addressed)]
    (if (:ok checked) {:ok true :certificate addressed} checked)))

(defn drive!
  [{:keys [kind contract request] :as options}]
  (if (= :preflight kind)
    (driver/drive!
     (assoc (select-keys options [:state :announce-fn :activate-fn :job-fn :persist-fn
                                 :ticket-register-fn :terminal-submission-provider
                                 :terminal-budget-config])
            :request request
            :terminal-validator preflight/validate-terminal
            :receipt-provider (fn [r t j _] (preflight/receipt contract r t j))))
    (driver/drive!
     (assoc (select-keys options [:state :announce-fn :activate-fn :job-fn :persist-fn
                                 :ticket-register-fn :terminal-submission-provider
                                 :terminal-budget-config])
            :request request
            :terminal-validator (partial validate-terminal kind)
            :receipt-provider (partial receipt contract kind)))))

(defn prompt [request]
  (str (str/upper-case (:frame-id request)) " "
       (name (:phase request)) " — use only this frozen dispatch authority:\n"
       (pr-str request) "\n"
       (case (:phase request)
         :solve
         (str
          (if (= 1 (:solver/round request))
            (str "Opening siege. Own a substantial proof episode: search, test multiple "
                 "routes, build missing infrastructure when needed, and continue through "
                 "friction. Do not stop merely because one lemma compiled. ")
            (str "Continue the same solver session and branch from the prior verified "
                 "state. Own a substantial proof episode, not one micro-lemma. "))
          (when (get-in request [:solver/remediation :required?])
            (str "The prior artifact failed terminal validation. Findings: "
                 (pr-str (get-in request [:solver/remediation :findings])) ". "
                 (get-in request [:solver/remediation :instruction]) " "))
          (when (:solver/strategy-checkpoint? request)
            (str "This is a ten-turn strategy checkpoint. Before returning, reassess the "
                 "whole route and include :solver/strategy {:summary STRING, "
                 ":obligations [STRING ...], :decomposition [{:obligation STRING, "
                 ":decision :delegate|:sequential, :reason STRING} ...], "
                 ":next-plan STRING}. Delegate genuinely independent obligations when "
                 "useful, using isolated branches/worktrees; review and integrate their "
                 "results yourself. "))
          (str "Commit the completed proof if reached. If unfinished, commit salvageable "
               "artifacts and report :solver/outcome :progress, an exact :residual, and "
               ":artifact-commits; friction is not a defect."))
         :verify "Independently verify the certified solver head; do not mutate it."
         "Perform the registered read-only preflight.")
       " Return exactly one EDN map with keys "
       (pr-str (if (= :preflight (:phase request))
                 preflight/required-report-fields proof-report-fields)) "."
       (when (contains? #{:solve :verify} (:phase request))
         (str " The nested :lean map must contain integer :exit, :warnings, "
              ":sorry-warnings, and :errors counts. :axioms must be a vector "
              "of symbols. :mutations lists committed changed paths; never put "
              ":solver/strategy inside :lean."))
       (when (= :preflight (:phase request))
         (str " The nested :lean value must be exactly shaped as "
              "{:exit INT :warnings INT :sorry-warnings INT :errors INT :output STRING}."))
       (if-let [job-id (:submission/job-id request)]
         (str "\nCompletion is accepted only through the typed submission tool; "
              "follow the shared completion contract "
              (pr-str submission/completion-contract) ". "
              "conversational output is never a receipt. Run the template command, "
              "fill every null in the generated JSON, then run the submit command:\n"
              (submission/command request {:job-id job-id})
              "\nFix every field-level error before ending the turn.")
         " Await activation before submitting completion.")))

(defn run-live!
  [{:keys [kind contract request state-path agency-base max-rounds terminal-budget]
    :or {agency-base "http://localhost:7070"
         max-rounds solver-rounds/default-max-rounds}}]
  (let [state (runtime/read-state state-path)
        persisted-request (:request state)
        request (if (and persisted-request
                         (request-replay-compatible? request persisted-request))
                  persisted-request
                  request)
        effects
        {:kind kind :contract contract :request request :state state
    :announce-fn
    (fn [req]
      (let [req (submission/with-job-authority req)
            announced (job-port/announce!
                       agency-base
                       {:agent-id (:agent-id req) :prompt (prompt req)
                        :mode (if (= :solve kind) "work" "brief")
                        :job-id (:submission/job-id req)})]
        announced))
    :activate-fn
    (fn [req ticket]
      (job-port/activate!
       agency-base
       {:agent-id (:agent-id req)
        :prompt (prompt (submission/with-job-authority req))
        :mode (if (= :solve kind) "work" "brief")
        :job-id (:job-id ticket)}))
    :job-fn
    (fn [job-id]
      (job-port/observe agency-base job-id))
    :cancel-fn
    (fn [job-id]
      (job-port/cancel! agency-base job-id
                        "typed-submission activation supersession"))
    :persist-fn #(runtime/atomic-persist! state-path %)
    :ticket-register-fn submission/register!
    :terminal-budget-config (or terminal-budget
                                (:terminal-budget request)
                                driver/default-terminal-budget)
    :terminal-submission-provider (fn [_ ticket _]
                                    (submission/submitted (:job-id ticket)))}]
    (if (= :solve kind)
      (solver-rounds/drive!
       (assoc effects
              :validate-solved (partial validate-terminal :solve)
              :provide-receipt (partial receipt contract :solve)
              :max-rounds max-rounds))
      (drive! effects))))

(defn resume-solver-remediation-live!
  "Resume exactly one halted corrective Solver round through Agency.

   Legacy strategy checkpoints are accepted only when their last two terminal
   failures are identical; live-solver-rounds enforces that condition."
  [{:keys [request state-path agency-base]
    :or {agency-base "http://localhost:7070"}}]
  (let [state-path (if (instance? java.nio.file.Path state-path)
                     state-path
                     (java.nio.file.Path/of (str state-path)
                                            (make-array String 0)))
        state (runtime/read-state state-path)
        announce-fn
        (fn [req]
          (job-port/announce!
           agency-base {:agent-id (:agent-id req) :prompt (prompt req)
                        :mode "work"}))
        activate-fn
        (fn [req ticket]
          (job-port/activate!
           agency-base {:agent-id (:agent-id req) :prompt (prompt req)
                        :mode "work" :job-id (:job-id ticket)}))]
    (solver-rounds/resume-remediation!
     {:state state :request request :announce-fn announce-fn
      :activate-fn activate-fn
      :persist-fn #(runtime/atomic-persist! state-path %)})))
