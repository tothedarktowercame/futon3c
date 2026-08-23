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
            [clojure.set :as set]
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

(defn validate-verify-terminal
  "Verify under the library-increment regime.

  Verify re-elaborates the head the solver certified. When that head closed the
  problem the shared rule is exactly right; when it landed a sorry-free library
  module and left the problem open, the head STILL carries the problem's sorry,
  and the shared rule reads that as an invalid proof. Relaxed the same way and
  only that way: the head must elaborate with no errors. Whether the landed
  library is itself sorry-free is decided downstream by
  library-lane-effects/outcome, which builds the ConstructionTargets roll-up and
  refuses on any sorry in it -- a stronger check than counting this file."
  [request ticket job]
  (let [strict (proof/validate-terminal :verify request ticket job)]
    (if (:ok strict)
      strict
      (let [report (proof/normalize-proof-report (:report job))
            lean (:lean report)
            open? (pos-int? (:sorry-warnings lean))
            ;; The proctor prints the PROBLEM target's axioms; on a partial
            ;; head that target is still sorry-backed by construction. sorryAx
            ;; is tolerated here exactly when the file reports a sorry; the
            ;; keying target's own axioms are printed at bank.
            axioms-ok? (set/subset? (set (:axioms report))
                                    (cond-> proof/permitted-axioms
                                      open? (conj 'sorryAx)))
            mutations-ok? (every? #(lane-mutation-permitted? request %)
                                  (:mutations report))
            remaining (cond-> (vec (remove #{:lean-proof-invalid
                                             :axioms-not-permitted
                                             :mutation-outside-problem-file}
                                           (:findings strict)))
                        (not axioms-ok?) (conj :axioms-not-permitted)
                        (not mutations-ok?) (conj :mutation-outside-lane-allowlist))]
        (if (and (empty? remaining)
                 (= 0 (:exit lean))
                 (= 0 (:errors lean))
                 (nat-int? (:sorry-warnings lean)))
          {:ok true :report report}
          (assoc strict :findings remaining))))))

(defn verify-receipt
  "The verify certificate, minted on this lane's terminal verdict.

  Same shape and same contract check as the countdown's; minted here because
  live-proof-phases/receipt re-runs the strict validator internally."
  [contract request ticket job]
  (let [terminal (validate-verify-terminal request ticket job)]
    (if-not (:ok terminal)
      terminal
      (let [report (:report terminal)
            body {:receipt/type :frame-verify
                  :receipt/frame-id (:frame-id request)
                  :receipt/problem-id (:problem-id request)
                  :receipt/job-id (:job-id ticket)
                  :receipt/solve-receipt-id (:solve-receipt-id request)
                  :receipt/final-head (:final-head report)
                  :receipt/mathematical-sound? true}
            addressed (assoc body :receipt/id (machine/ledger-digest [body]))
            checked (cycle/validate-receipt contract :verify addressed)]
        (if (:ok checked)
          {:ok true :certificate addressed}
          checked)))))

(defn with-keying-targets
  "Name the episode's keying targets inside the dispatch authority itself.

  The card fixes exactly one keying target per dispatch and forbids changing
  its statement, but the shared request carries no such field, so the solver
  had to pick its own target from the file. library-lane-runner already
  elaborates them; this puts them where the prompt and the frozen authority
  can both see them. The dispatch/id is recomputed, not patched: it is a
  content digest of the body, so adding a field must change it."
  [request targets]
  (if-not (seq targets)
    request
    (let [body (assoc (dissoc request :dispatch/id)
                      :keying-targets (vec targets))]
      (assoc body :dispatch/id (machine/ledger-digest [body])))))

(def control-root
  "Where the role cards live -- which is NOT where the solver stands.

  :role-card-path is repo-relative to this control checkout, but the solver's
  workspace is an apm-lean worktree with no `holes/` directory at all. Telling
  it to read the card `in the workspace` sends it to a path that cannot exist,
  and it proceeds without the contract it was supposed to be reading. The
  prompt must therefore name the card absolutely."
  "/home/joe/code/futon3c-frame18-control")

(def lane-mutation-allowlist-text
  "What the solver is permitted to commit, stated in the prompt.

  It must match lane-mutation-permitted? exactly. A solver told it may land a
  module, by a machine that then rejects the commit, wastes a whole episode --
  which is precisely what happened to t00J02 on 2026-08-22, in reverse."
  (str "You may commit ONLY: the keying problem's lean/Main.lean; "
       "ConstructionTargets/<Module>.lean and ConstructionTargets/<Module>.md; "
       "ConstructionTargets.lean (the roll-up); and the keying problem's "
       "status.json. Anything else fails terminal validation."))

(defn prompt
  "This lane's dispatch prompt.

  live-proof-phases/prompt encodes the countdown's regime -- close one problem
  inside one file -- in two ways that are fatal here. It ends with \"Return
  exactly one EDN map with keys\" and the v5 field set, which EXCLUDES every
  field the library card requires; and it never tells the solver to read the
  role card at all. A solver obeying it literally cannot produce a library
  increment, and on 2026-08-22 one did exactly that: ten rounds, 371 verified
  lines, all inlined into Main.lean, nothing bankable.

  Preflight and verify are unchanged in substance, so they delegate."
  [request]
  (if-not (= :solve (:phase request))
    (proof/prompt request)
    (str (str/upper-case (:frame-id request)) " solve -- library increment. "
         "Use only this frozen dispatch authority:\n"
         (pr-str request) "\n"
         "FIRST read your role card, which is OUTSIDE your workspace, at "
         control-root "/" (:role-card-path request)
         " (blob " (:role-card-blob request) "); it is the "
         "surface contract for this dispatch and it differs from the problem-"
         "closing card. The unit of acceptance is a LIBRARY INCREMENT keyed to "
         "this problem, not the problem's closure: reusable, sorry-free "
         "material landed under ConstructionTargets/ that the keying target "
         "consumes. Work inlined into Main.lean is not an increment and cannot "
         "be banked, however much of it is proved.\n"
         (when-let [targets (seq (:keying-targets request))]
           (str "Keying target(s) for this episode, fixed: "
                (str/join ", " targets)
                ". You may not change their statements.\n"))
         (if (= 1 (:solver/round request))
           (str "Opening siege. Own a substantial proof episode: search Mathlib, "
                "test multiple routes, build missing infrastructure, and continue "
                "through friction. Do not stop merely because one lemma compiled. ")
           (str "Continue the same solver session and branch from the prior "
                "verified state. Own a substantial proof episode, not one "
                "micro-lemma. "))
         (when (get-in request [:solver/remediation :required?])
           (str "The prior artifact failed terminal validation. Findings: "
                (pr-str (get-in request [:solver/remediation :findings])) ". "
                (get-in request [:solver/remediation :instruction]) " "))
         (when (:solver/strategy-checkpoint? request)
           (str "This is a ten-turn strategy checkpoint. Before returning, "
                "reassess the whole route and include :solver/strategy "
                "{:summary STRING, :obligations [STRING ...], :decomposition "
                "[{:obligation STRING, :decision :delegate|:sequential, "
                ":reason STRING} ...], :next-plan STRING}. "))
         lane-mutation-allowlist-text
         " Commit your work and leave the worktree clean.\n"
         "Return exactly one EDN map. It must contain "
         (pr-str proof/proof-report-fields)
         ". The nested :lean map must contain integer :exit, :warnings, "
         ":sorry-warnings and :errors counts; :axioms must be a vector of "
         "symbols; :mutations lists committed changed paths. "
         "If you closed the problem, that is all. If the problem remains open "
         "-- the ordinary outcome -- you must ALSO return :solver/outcome "
         ":progress, a non-blank :residual naming the remaining Lean-level "
         "obligations and their dependency relation, and :library/modules, a "
         "non-empty vector of the ConstructionTargets paths you landed. "
         "Without :library/modules the episode certifies as nothing and banks "
         "nothing. Never put :solver/strategy inside :lean.")))

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
                           {:agent-id (:agent-id req) :prompt (prompt req)
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
                    {:agent-id (:agent-id req) :prompt (prompt req)
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
      ;; A landed increment ENDS the episode, closed or partial. That is the
      ;; card's unit of acceptance, and it is what makes the lane a loop: a
      ;; partial banks, the problem stays in the :library lane (see
      ;; :lane-transition in library-lane-adapters), and the queue hands it
      ;; back for the next increment. Holding out for closure across fifty
      ;; rounds is how 2026-08-22 produced 371 verified lines and banked none
      ;; of them.
      (solver-rounds/drive!
       (assoc effects
              :validate-solved validate-solve-terminal
              :provide-receipt (fn [r t j _] (solve-receipt contract r t j))
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
          (driver/drive!
           (assoc (select-keys effects [:state :announce-fn :activate-fn
                                        :job-fn :persist-fn])
                  :request request
                  :terminal-validator
                  (if (= :preflight kind)
                    preflight-validate-terminal
                    validate-verify-terminal)
                  :receipt-provider
                  (if (= :preflight kind)
                    (fn [r t j _] (preflight-receipt contract r t j))
                    (fn [r t j _] (verify-receipt contract r t j)))))))))
