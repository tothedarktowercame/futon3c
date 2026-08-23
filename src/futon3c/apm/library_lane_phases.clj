(ns futon3c.apm.library-lane-phases
  "The Codex-only library lane's OWN cycle machinery.

  Two cycle machines run on this box. The countdown/learning machine drives
  frames with students, guides and memory snapshots; this one drives Codex-only
  library increments. They do different things, so they must not BE the same
  running machinery -- otherwise a dispatch policy chosen for one silently
  retargets the other, and their traffic interleaves in one ledger. That is not
  hypothetical: on 2026-08-22 an activate-fn change made for this lane landed on
  the countdown's driver while frame f24 was live.

  Sharing CODE is encouraged and this namespace does it freely: request
  construction, terminal validation, receipt shaping and prompt rendering are
  all taken from futon3c.apm.live-proof-phases as pure functions. What is NOT
  shared is the running object -- this lane announces, activates, polls and
  persists through its own driver, so a change here reaches nothing else."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-contract :as cycle]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.live-preflight :as preflight]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-proof-phases :as proof]
            [futon3c.apm.live-solver-rounds :as solver-rounds]
            [futon3c.apm.workspace-lifecycle :as workspace]))

(def surface "emacs-repl")
(def caller "library-lane")

(defn validate-workspace
  "workspace-lifecycle/validate, tolerating a solver's own committed advance.

  A siege resumes from the head its previous round committed, so HEAD and the
  problem blob legitimately differ from the pins. The shared validator is
  strict by design and stays that way; this lane relaxes it for itself only,
  and only forward: the pinned base must remain an ANCESTOR, so a head that
  forked off the pin is still a mismatch. The frozen statement is protected
  separately by :statement-unchanged? on the solve report."
  [lease]
  (let [strict (workspace/validate lease)]
    (if (:valid? strict)
      strict
      (let [head (:head strict)
            base (:base-revision lease)
            advanced?
            (and (string? head) (string? base) (not= base head)
                 (zero? (:exit (shell/sh "git" "-C"
                                         (str (:workspace/path lease))
                                         "merge-base" "--is-ancestor"
                                         base head))))
            tolerated #{:workspace-head-mismatch :workspace-problem-blob-mismatch}
            remaining (remove tolerated (:findings strict))]
        (if (and advanced? (empty? remaining))
          (assoc strict :valid? true :findings [] :advanced? true)
          strict)))))

(def workspace-roles
  "This lane provisions a solver worktree only -- no student arm."
  #{:solver})

(def seat-types
  "Codex-only cast: solver proves, proctor preflights and verifies."
  {:solver :codex :proctor :codex})

(def timeouts
  {:request-timeout-ms 300000 :turn-timeout-ms 3600000})

(defn- prepare-workspace
  [result unit role leases workspace-exists? provision-fn validate-workspace-fn]
  (let [lease (get leases role)
        exists? (workspace-exists? unit role)]
    (cond
      (and exists? (nil? lease))
      (reduced {:ok false :error/code :existing-workspace-without-lease :role role})
      exists?
      (let [validation (validate-workspace-fn lease)]
        (if (:valid? validation)
          (assoc-in result [:workspaces role] {:lease lease :validation validation})
          (reduced {:ok false :error/code :existing-workspace-invalid
                    :role role :validation validation})))
      lease
      (reduced {:ok false :error/code :leased-workspace-missing :role role})
      :else
      (let [provisioned (provision-fn unit role)]
        (if-not (:ok provisioned)
          (reduced provisioned)
          (let [new-lease (:lease provisioned)
                validation (validate-workspace-fn new-lease)]
            (if (:valid? validation)
              (assoc-in result [:workspaces role]
                        {:lease new-lease :validation validation})
              (reduced {:ok false :error/code :new-workspace-invalid
                        :role role :validation validation}))))))))

(defn prepare!
  "Prepare this lane's frame: one solver worktree, solver and proctor seats.

  Deliberately NOT live-launch-preparation/prepare!, which provisions the
  countdown's {:solver :student} arms and mints its five-seat cast. Same
  discipline -- every effect injected, leases revalidated on reuse -- as its
  own object, so this lane's role set cannot drift onto the countdown."
  [{:keys [unit ledger role-cards leases workspace-exists? provision-fn
           validate-workspace-fn mint-fn roster-fn]}]
  (let [frame-id (:frame/id unit)
        problem-id (:problem/id unit)]
    (if (or (not (and (string? frame-id) (re-matches #"f[0-9]+" frame-id)))
            (not (string? problem-id))
            (not (every? fn? [workspace-exists? provision-fn
                              validate-workspace-fn mint-fn roster-fn])))
      {:ok false :error/code :library-lane-preparation-input-invalid}
      (let [prepared (reduce (fn [result role]
                               (prepare-workspace result unit role leases
                                                  workspace-exists? provision-fn
                                                  validate-workspace-fn))
                             {:ok true :workspaces {}}
                             workspace-roles)]
        (if-not (:ok prepared)
          prepared
          (let [minted (mint-fn frame-id seat-types timeouts)]
            (if-not (:ok minted)
              {:ok false :error/code :seat-mint-failed :finding minted}
              (let [seats (roster-fn frame-id)]
                ;; roster-fn returns the seats map itself, keyed by role -- the
                ;; same contract the countdown's preparation consumes.
                (if-not (and (map? seats)
                             (every? #(map? (get seats %)) (keys seat-types))
                             (every? #(true? (:invoke-ready? (get seats %)))
                                     (keys seat-types)))
                  {:ok false :error/code :seat-roster-invalid :finding seats}
                  {:ok true :frame-id frame-id :problem-id problem-id
                   :ledger ledger :role-cards role-cards
                   :workspaces (:workspaces prepared)
                   :seats seats
                   :receipt {:workspace/ids
                             (into {} (map (fn [[r w]]
                                             [r (get-in w [:lease :workspace/id])]))
                                   (:workspaces prepared))
                             :seat/ids
                             (into {} (map (fn [[r s]] [r (:agent-id s)]))
                                   seats)}})))))))))

(defn preflight-baseline-ok?
  "Does this Lean evidence describe a live, well-formed library target?

  The countdown's rule is an exact match on {:exit 0 :warnings 1
  :sorry-warnings 1 :errors 0} -- one sorry and NO other warning. That holds
  for a hand-built frame problem and fails on the corpus: t00J02 elaborates
  cleanly with one sorry and one `try 'simp' instead` linter hint, so warnings
  is 2 and the whole problem was ruled :blocked over a style lint.

  This lane asks the questions preflight is actually for -- the file
  elaborates, nothing errored, and the holes are still open -- and lets style
  lints be style lints. It does NOT relax the count of holes: which
  declarations must be discharged is decided by library-lane-runner/
  elaborate-targets against the source, which is a stricter authority than a
  warning tally because it names them."
  [lean]
  (and (map? lean)
       (= 0 (:exit lean))
       (= 0 (:errors lean))
       (pos-int? (:sorry-warnings lean))
       (integer? (:warnings lean))
       (>= (:warnings lean) (:sorry-warnings lean))))

(defn preflight-validate-terminal
  "preflight/validate-terminal under this lane's Lean baseline rule.

  The shared validator stays strict and untouched; it runs first and every
  finding it makes is honoured except the baseline one, which is re-decided
  here. A report that trips anything else is still rejected."
  [request ticket job]
  (let [strict (preflight/validate-terminal request ticket job)]
    (if (:ok strict)
      strict
      (let [report (preflight/normalize-report (:report job))
            remaining (remove #{:lean-baseline-mismatch} (:findings strict))]
        (if (and (empty? remaining)
                 (preflight-baseline-ok? (:lean report)))
          {:ok true :report report}
          strict)))))

(defn preflight-receipt
  "The preflight certificate, minted on this lane's terminal verdict.

  Shaped exactly like the countdown's and checked against the same contract by
  cycle/validate-receipt, so the two machines' receipts stay comparable in the
  shared ledger. It is minted here rather than delegated because
  preflight/receipt re-runs the strict validator internally, and this lane has
  already decided that question differently."
  [contract request ticket job]
  (let [terminal (preflight-validate-terminal request ticket job)]
    (if-not (:ok terminal)
      terminal
      (let [report (:report terminal)
            body {:receipt/type :frame-preflight
                  :receipt/frame-id (:frame-id request)
                  :receipt/problem-id (:problem-id request)
                  :receipt/result :preflight-passed
                  :receipt/job-id (:job-id ticket)
                  :receipt/dispatch-id (:dispatch/id request)
                  :receipt/problem-revision (:problem-revision report)
                  :receipt/problem-blob (:problem-blob report)
                  :receipt/lean (:lean report)
                  :receipt/clean-before? (:clean-before? report)
                  :receipt/clean-after? (:clean-after? report)
                  :receipt/mutations (:mutations report)}
            addressed (assoc body :receipt/id (machine/ledger-digest [body]))
            checked (cycle/validate-receipt contract :preflight addressed)]
        (if (:ok checked)
          {:ok true :certificate addressed}
          checked)))))

(defn- lane-mutation-permitted?
  [request path]
  (or (= path (:problem-path request))
      (= path "ConstructionTargets.lean")
      (and (string? path)
           (boolean
            (re-matches #"ConstructionTargets/[^/]+\.(?:lean|md)" path)))
      (= path (str "problems/" (:problem-id request) "/status.json"))))

(defn- partial-library-increment?
  [report]
  (let [lean (:lean report)
        modules (:library/modules report)]
    (and (= 0 (:exit lean))
         (= 0 (:errors lean))
         (pos-int? (:sorry-warnings lean))
         (vector? modules)
         (seq modules)
         (every? string? modules)
         (= :progress (:solver/outcome report))
         (string? (:residual report))
         (not (str/blank? (:residual report))))))

(defn validate-solve-terminal
  "Validate a solve under the library-increment regime.

  The shared problem-closing validator remains the authority and always runs
  first. This lane re-decides only its problem-file mutation boundary and its
  zero-sorry terminal condition: committed, axiom-clean reusable library work
  may certify as :partial while the keyed problem remains open."
  [request ticket job]
  (let [strict (proof/validate-terminal :solve request ticket job)
        report (proof/normalize-proof-report (:report job))
        lean (:lean report)
        closed? (= {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0}
                   (select-keys lean [:exit :warnings :sorry-warnings :errors]))
        partial? (partial-library-increment? report)
        mutations-permitted?
        (every? #(lane-mutation-permitted? request %) (:mutations report))
        findings (cond-> (vec (remove #{:mutation-outside-problem-file
                                        :lean-proof-invalid}
                                      (:findings strict)))
                   (not mutations-permitted?)
                   (conj :mutation-outside-lane-allowlist)
                   (not (or closed? partial?))
                   (conj :lean-proof-invalid))]
    (if (seq findings)
      (assoc strict :ok false :findings findings)
      {:ok true :report report
       :solve/result (if closed? :closed :partial)})))

(defn solve-receipt
  "Mint the frame-solve certificate on this lane's closed-or-partial verdict."
  [contract request ticket job]
  (let [terminal (validate-solve-terminal request ticket job)]
    (if-not (:ok terminal)
      terminal
      (let [report (:report terminal)
            result (:solve/result terminal)
            body (cond-> {:receipt/type :frame-solve
                          :receipt/frame-id (:frame-id request)
                          :receipt/problem-id (:problem-id request)
                          :receipt/job-id (:job-id ticket)
                          :receipt/final-head (:final-head report)
                          :receipt/lean (:lean report)
                          :receipt/axioms (:axioms report)
                          :receipt/statement-unchanged? true
                          :receipt/result result}
                   (= :partial result)
                   (assoc :receipt/library-modules (:library/modules report)
                          :receipt/residual (:residual report)))
            addressed (assoc body :receipt/id (machine/ledger-digest [body]))
            checked (cycle/validate-receipt contract :solve addressed)]
        (if (:ok checked)
          {:ok true :certificate addressed}
          checked)))))

(defn- certified-replay
  "The certificate a previous attempt already earned for THIS request, or nil.

  Guarded on dispatch/id equality, not merely on frame and phase: the id is a
  content digest of the request, so a replay can only satisfy a caller that
  asked the identical question."
  [state request]
  (when (and (= :live-job-certified (:state/type state))
             (= (:dispatch/id request) (get-in state [:request :dispatch/id]))
             (:receipt state))
    {:ok true :status :certified :state state
     :certificate (:receipt state) :replayed? true}))

(defn run-live!
  "Drive one lane phase to its next durable boundary.

  Same shape as the countdown's driver -- announce, activate, poll, persist --
  but a distinct object with this lane's dispatch policy. announce only
  RESERVES a ledger row (create-invoke-job! notifies nobody), so activation is
  an explicit invoke carrying the announced job-id: build-invoke-response reads
  requested-job-id and create-invoke-job! reuses a non-terminal record, so the
  reserved job is adopted rather than duplicated. See
  futon3c/holes/excursions/E-apm-drainer.md."
  [{:keys [kind contract request state-path agency-base]
    :or {agency-base "http://localhost:7070"}}]
  (let [mode (if (= :solve kind) "work" "brief")
        effects
        {:kind kind :contract contract :request request
         :state (runtime/read-state state-path)
         ;; This lane does not depend on the shared per-agent DRAINER, which
         ;; is the machinery the two cycle machines would otherwise contend
         ;; for. announce here is an id reservation only -- create-invoke-job!
         ;; writes a ledger row and notifies nobody, so nothing is enqueued and
         ;; nothing waits to be drained (E-apm-drainer.md). Dispatch is then
         ;; explicit and immediate, carrying that id.
         ;;
         ;; What remains shared is the job LEDGER: build-invoke-response always
         ;; calls create-invoke-job!, so every invoke lands a row there and only
         ;; futon3c can change that. The lane uses it as a read-only terminal
         ;; oracle for ids it created, and owns its scheduling and durable state
         ;; under data/apm-lane/.
         :announce-fn
         (fn [req]
           (let [response (runtime/http-json
                           "POST" (str agency-base "/api/alpha/invoke/announce")
                           {:agent-id (:agent-id req) :prompt (proof/prompt req)
                            :surface surface :caller caller :mode mode})]
             {:ok (and (= 202 (:http/status response)) (:ok response))
              :job-id (:job-id response)}))
         :activate-fn
         (fn [req ticket]
           ;; The dispatch the countdown gets from its drainer, this lane does
           ;; for itself: POST /invoke with the reserved job-id. build-invoke-
           ;; response reads requested-job-id and create-invoke-job! reuses a
           ;; non-terminal row, so the reservation is adopted, not duplicated.
           ;; On a future so this returns at once and drive! stays a durable
           ;; step machine with the caller polling.
           (let [job-id (:job-id ticket)]
             (when job-id
               (future
                 (try
                   (runtime/http-json
                    "POST" (str agency-base "/api/alpha/invoke")
                    {:agent-id (:agent-id req) :prompt (proof/prompt req)
                     :surface surface :caller caller :mode mode :job-id job-id})
                   (catch Throwable _ nil))))
             {:ok (some? job-id)}))
         :job-fn
         (fn [job-id]
           (runtime/job->terminal
            (runtime/http-json
             "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))))
         :persist-fn #(runtime/atomic-persist! state-path %)}]
    (cond
      (= :solve kind)
      (solver-rounds/drive!
       (assoc effects
              :validate-solved (partial proof/validate-terminal :solve)
              :provide-receipt (partial proof/receipt contract :solve)
              :max-rounds solver-rounds/default-max-rounds))

      ;; A siege RE-ENTERS the same frame: the queue runs the phase list again
      ;; on every attempt, so preflight and verify are asked to run against
      ;; state a previous attempt already certified. The shared driver is a
      ;; single-shot step machine -- it only accepts :live-job-dispatched and
      ;; answers anything else with :live-job-state-invalid -- so re-entry
      ;; reads as corruption and the whole problem is ruled :blocked. Nothing
      ;; is wrong: the phase is DONE. Replay the stored certificate instead,
      ;; and only when the request is byte-identical, which dispatch/id (a
      ;; content digest of the request) decides. A changed request gets a
      ;; changed id and falls through to a real dispatch.
      :else
      (or (certified-replay (:state effects) request)
          (if (= :preflight kind)
            (driver/drive!
             (assoc (select-keys effects [:state :announce-fn :activate-fn
                                          :job-fn :persist-fn])
                    :request request
                    :terminal-validator preflight-validate-terminal
                    :receipt-provider (fn [r t j _]
                                        (preflight-receipt contract r t j))))
            (proof/drive! effects))))))
