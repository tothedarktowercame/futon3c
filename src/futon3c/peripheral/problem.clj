(ns futon3c.peripheral.problem
  "Problem peripheral — one registered experimental problem per cycle."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.tools :as tools])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

(def phase-order
  [:register :frame :guided-solve :intervene :student-attempts
   :adjudicate :promote :close])

(def advance :advance-problem-phase)

(def base-phase-tools
  {:register #{:read-registration :validate-registration :snapshot-store
               :freeze-stratum :pin-resources :assign-checkouts advance}
   :frame #{:emit-frame advance}
   :guided-solve #{:dispatch-solver :guide-solver :read-substrate advance}
   :intervene #{advance}
   :student-attempts #{:dispatch-student-fresh :read-attempt-result advance}
   :adjudicate #{:write-disposition :write-use advance}
   :promote #{:promote-artifact advance}
   :close #{:emit-trace :validate-trace :write-authorization advance}})

(def required-outputs
  {:register #{:registration :store-snapshot :stratum-frozen-at
               :environment-revision :harness-revision :environment-checkouts}
   :frame #{:frame :containment-probe}
   :guided-solve #{:solver-attempt :ground-control-events :memory-offers}
   :intervene #{:intervention}
   :student-attempts #{:student-attempts :memory-uses}
   :adjudicate #{:disposition :launch-gate-event}
   :promote #{:promotion-result}
   :close #{:measurement :trace}})

(def all-tools
  (into #{:begin-problem-cycle :load-registration :list-problems
          :problem-save :problem-load}
        (mapcat identity (vals base-phase-tools))))

(def tool-ops
  (zipmap all-tools
          (map #(if (#{:read-registration :validate-registration
                       :read-substrate :read-attempt-result :list-problems}
                     %)
                  :observe :action)
               all-tools)))

(defn- fruit [state]
  (some (fn [{:keys [tool result]}] (when (= :emit-trace tool) result))
        (reverse (:steps state))))

(defn- environment-arms-match [outputs]
  (let [pinned (:environment-revision outputs)
        solver-revision (get-in outputs
                                [:solver-attempt :cycle/environment-revision])
        student-revisions (map :cycle/environment-revision
                               (:student-attempts outputs))
        solver-checkout (get-in outputs
                                [:solver-attempt :cycle/environment-checkout])
        student-checkouts (map :cycle/environment-checkout
                               (:student-attempts outputs))]
    (cond
      (not (and (= pinned solver-revision)
                (every? #(= pinned %) student-revisions)))
      {:failure :environment-mismatch-between-arms
       :pinned pinned :solver solver-revision
       :students (vec student-revisions)}

      ;; A cold attempt has its own tree. Any nil or duplicate checkout leaves
      ;; an unmeasured filesystem channel between attempts.
      (let [checkouts (into [solver-checkout] student-checkouts)]
        (or (some nil? checkouts)
            (not= (count checkouts) (count (set checkouts)))))
      {:failure :environment-shared-checkout
       :solver solver-checkout :students (vec student-checkouts)})))

(defn- autoconf [context config]
  (let [intervention-tool (case (:cycle/mode context)
                            :store-mode :write-substrate
                            :harness-mode :tune-harness
                            nil)]
    (cond-> config
      intervention-tool
      (update-in [:phase-tools :intervene] conj intervention-tool))))

(defn- validate-loaded-state [current loaded]
  (when (not= (:cycle/mode current) (:cycle/mode loaded))
    {:failure :loaded-state-mode-mismatch
     :expected (:cycle/mode current)
     :actual (:cycle/mode loaded)}))

(defn- state-snapshot [state tool result]
  (when (and (= tool :problem-save) (:ok result))
    {:snapshot/subject {:ref/type :peripheral :ref/id "problem"}
     :snapshot/body {:snapshot :problem-save
                     :problem-id (:problem-id state)
                     :cycle-id (:current-cycle-id state)
                     :version (:version result)
                     :phase (:current-phase state)
                     :cycles-completed (:cycles-completed state)
                     :step-count (count (:steps state))}
     :snapshot/tags [:problem :snapshot]}))

(defn- stamp-attempt-environment [attempt assignment]
  (if (map? attempt)
    (assoc attempt
           :cycle/environment-checkout (:checkout assignment)
           :cycle/environment-revision (:base-revision assignment))
    attempt))

(defn- recorded-student-assignments [state]
  ;; :steps spans the whole peripheral session, including completed cycles.
  ;; Reset at the latest assignment so attempt 1 of a new cycle cannot be
  ;; paired with attempt 1's tree from the previous cycle.
  (reduce (fn [assignments {:keys [tool result]}]
            (case tool
              :assign-checkouts []
              :dispatch-student-fresh
              (conj assignments (:environment-checkout result))
              assignments))
          [] (:steps state)))

(defn- recorded-assignment
  "What :assign-checkouts ACTUALLY produced, from the engine's own step record.

  Preferred over the payload because :environment-checkouts otherwise arrives
  through the caller-supplied advance payload like every other output -- so a
  caller could relay a fabricated assignment and everything downstream would
  stamp consistently from the fabrication, with every check passing against
  paths that were never provisioned. The engine writes tool results into :steps
  and the caller cannot edit them. Same shape as `fruit`, which reads the
  :emit-trace result from step history."
  [state payload]
  (or (some (fn [{:keys [tool result]}]
              (when (= :assign-checkouts tool)
                (:environment-checkouts result)))
            (reverse (:steps state)))
      (get-in state [:cycle/outputs :environment-checkouts])
      (:environment-checkouts payload)))

(defn- stamp-environment-outputs [state payload]
  (let [assignments (recorded-assignment state payload)
        solver (:solver assignments)
        students (let [recorded (recorded-student-assignments state)]
                   (if (seq recorded) recorded (vec (:student assignments))))
        revision (:base-revision solver)]
    (cond-> payload
      ;; The recorded assignment overwrites the caller's, exactly as the attempt
      ;; fields do -- the caller may relay it, but the machine owns it.
      (and (some? assignments) (contains? payload :environment-checkouts))
      (assoc :environment-checkouts assignments)

      (some? revision)
      (assoc :environment-revision revision)

      (contains? payload :solver-attempt)
      (update :solver-attempt stamp-attempt-environment solver)

      (sequential? (:student-attempts payload))
      (update :student-attempts
              #(mapv (fn [index attempt]
                       (stamp-attempt-environment attempt (get students index)))
                     (range) %)))))

(def problem-domain-config
  {:domain-id :problem
   :phase-order phase-order
   :phase-tools base-phase-tools
   :setup-tools #{:begin-problem-cycle :load-registration :list-problems}
   :tool-ops (assoc tool-ops
                    :write-substrate :action
                    :tune-harness :action)
   :required-outputs required-outputs
   :enforce-required-outputs? true
   :state-io-tools {:save :problem-save :load :problem-load}
   :always-available-tools #{:problem-save :problem-load}
   :state-runtime-keys #{:cycle-config :evidence-store}
   :state-validate-fn validate-loaded-state
   :state-snapshot-fn state-snapshot
   :output-stamp-fn stamp-environment-outputs
   :output-invariants
   [{:id :environment-arms-match
     :requires #{:environment-revision :solver-attempt :student-attempts}
     :check environment-arms-match}]
   :cycle-begin-tool :begin-problem-cycle
   :cycle-advance-tool advance
   :state-init-fn (fn [context]
                    {:problem-id (:problem-id context)
                     :cycle/mode (:cycle/mode context)
                     :cycle/deposit-state (:cycle/deposit-state context)
                     :cycle/paired-with (:cycle/paired-with context)})
   :autoconf-fn autoconf
   :fruit-fn fruit
   :exit-context-fn (fn [state]
                      {:session-id (:session-id state)
                       :problem-id (:problem-id state)})})

(def problem-spec
  {:peripheral/id :problem
   :peripheral/tools (conj all-tools :write-substrate :tune-harness)
   :peripheral/scope :full-codebase
   :peripheral/entry #{:user-request}
   :peripheral/exit #{:user-request}
   :peripheral/context {}})

(def ^:private dispatch-channels
  {:dispatch-solver :push+pull
   :dispatch-student-fresh :pull-only})

(def ^:private default-problem-state-root "data/problem-state")
(def ^:private problem-state-write-lock (Object.))
(def ^:private frames-script "/home/joe/code/futon3c/scripts/frames.bb")
(def ^:private experiment-frames-root
  "/home/joe/code/futon3c/data/experiment-frames")

(defn- cycle-dir-within
  "Resolve the per-cycle directory and REQUIRE it to sit under root.

  The character-class check alone is not path safety: \"..\" matches
  [A-Za-z0-9._-]+ because both dots are in the class, and a save with that cycle
  id wrote to <root>/../v1.edn -- outside the store. A regex that looks like a
  containment check is worse than none, because it stops anyone looking. So the
  resolved canonical path is compared against the resolved canonical root."
  [root cycle-id]
  (when-not (and (string? cycle-id)
                 (re-matches #"[A-Za-z0-9._-]+" cycle-id)
                 (not (contains? #{"." ".."} cycle-id)))
    (throw (ex-info "Problem state requires a safe cycle id"
                    {:cycle-id cycle-id})))
  (let [root-path (.toAbsolutePath (.normalize (.toPath (io/file root))))
        dir (io/file root cycle-id)
        dir-path (.toAbsolutePath (.normalize (.toPath dir)))]
    (when-not (.startsWith dir-path root-path)
      (throw (ex-info "Problem state path escapes its root"
                      {:cycle-id cycle-id :resolved (str dir-path)})))
    dir))

(defn- safe-cycle-id [state]
  (let [cycle-id (:current-cycle-id state)]
    (when-not (and (string? cycle-id)
                   (re-matches #"[A-Za-z0-9._-]+" cycle-id)
                   (not (contains? #{"." ".."} cycle-id)))
      (throw (ex-info "Problem state requires a safe active cycle id"
                      {:current-cycle-id cycle-id})))
    cycle-id))

(defn- version-number [filename]
  (some->> (re-matches #"v([0-9]+)\.edn" filename)
           second
           parse-long))

(defn- existing-versions [cycle-dir]
  (if (.isDirectory cycle-dir)
    (->> (.listFiles cycle-dir)
         (keep #(version-number (.getName %)))
         sort)
    []))

(defn- save-problem-state! [root state]
  ;; Version selection and rename are one JVM-critical section. ATOMIC_MOVE
  ;; makes publication indivisible, but Java leaves replacement of an existing
  ;; target implementation-specific; serial allocation prevents two engine
  ;; saves from ever presenting it with the same target.
  (locking problem-state-write-lock
    (let [cycle-id (safe-cycle-id state)
          cycle-dir (cycle-dir-within root cycle-id)
          _ (Files/createDirectories (.toPath cycle-dir)
                                     (make-array FileAttribute 0))
          version (inc (or (last (existing-versions cycle-dir)) 0))
          target (.toPath (io/file cycle-dir (str "v" version ".edn")))
          temp (Files/createTempFile (.toPath cycle-dir) ".state-" ".tmp"
                                     (make-array FileAttribute 0))]
      (try
        (spit (.toFile temp) (pr-str state))
        (Files/move temp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE]))
        {:ok true :version version :path (str target)}
        (finally
          (Files/deleteIfExists temp))))))

(defn- load-problem-state [root cycle-id version]
  (when-not (pos-int? version)
    (throw (ex-info "Problem state version must be a positive integer"
                    {:version version})))
  (edn/read-string (slurp (io/file (cycle-dir-within root cycle-id)
                                   (str "v" version ".edn")))))

(defrecord ProblemStateBackend [inner-backend root]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      ;; ToolBackend promises {:ok true :result} | {:ok false :error}. A missing
      ;; version made slurp throw FileNotFoundException straight out of
      ;; execute-tool, so the cycle could not record its own failure -- the third
      ;; instance of this same boundary defect in this session.
      :problem-save
      (let [[state] args]
        (try {:ok true :result (save-problem-state! root state)}
             (catch Throwable t
               {:ok false :error (str "problem-save failed: " (.getMessage t))})))

      :problem-load
      (let [[cycle-id version] args]
        (try {:ok true :result (load-problem-state root cycle-id version)}
             (catch Throwable t
               {:ok false :error (str "problem-load failed: " (.getMessage t))})))

      (tools/execute-tool inner-backend tool-id args))))

(defn make-problem-state-backend [inner-backend root]
  (->ProblemStateBackend inner-backend root))

(defn- require-provision-option [options key]
  (let [value (get options key)]
    (when-not (and (string? value) (not (str/blank? value)))
      (throw (ex-info (str "assign-checkouts requires " key) {:key key})))
    value))

(defn- provision-frame! [options]
  (let [problem (require-provision-option options :problem)
        arm (require-provision-option options :arm)
        batch (require-provision-option options :batch)
        base-rev (require-provision-option options :base-rev)
        seat (require-provision-option options :seat)
        memory-channel (require-provision-option options :memory-channel)
        recall-system (require-provision-option options :recall-system)
        branch (require-provision-option options :branch)
        frame-id (str batch "-" problem "-" arm)
        command ["bb" frames-script "open"
                 "--problem" problem "--arm" arm
                 "--base-rev" base-rev "--seat" seat
                 "--memory-channel" memory-channel
                 "--recall-system" recall-system
                 "--batch" batch "--branch" branch]
        {:keys [exit err]} (apply shell/sh command)]
    (when-not (zero? exit)
      (throw (ex-info "frames.bb open failed"
                      {:exit exit :error err :frame-id frame-id})))
    (let [record (edn/read-string
                  (slurp (io/file experiment-frames-root batch
                                  (str frame-id ".edn"))))]
      (select-keys record [:checkout :base-revision :branch :frame/id :batch]))))

(defn- checkout-options [options arm]
  (let [arm-name (name arm)
        batch (require-provision-option options :batch)
        problem (require-provision-option options :problem)]
    {:problem problem
     :arm arm-name
     :batch batch
     :base-rev (require-provision-option options :base-rev)
     :seat (require-provision-option options
                                    (if (= arm :solver)
                                      :solver-seat
                                      :student-seat))
     :memory-channel (if (= arm :solver) "push" "none")
     :recall-system (require-provision-option options :recall-system)
     ;; The script's default omits batch and collides on the second cycle.
     :branch (str "exp/" batch "-" problem "-" arm-name)}))

(def ^:private apm-root "/home/joe/code/apm-lean")

(defn- rollback-frame!
  "Undo one provisioned frame: worktree, branch, and record.

  Assignment must be ALL-OR-NOTHING. Verified before this existed: when the
  student arm failed, the solver's worktree and branch survived, and the retry
  then died on \"frame already exists\" -- a half-provisioned cycle that could not
  be re-registered without manual cleanup. A sticky failure is worse than a loud
  one, because the obvious response (try again) cannot work."
  [{:keys [checkout branch] :as frame}]
  (when checkout
    (shell/sh "git" "-C" apm-root "worktree" "remove" "--force" checkout))
  (when branch
    (shell/sh "git" "-C" apm-root "branch" "-D" branch))
  (when-let [frame-id (:frame/id frame)]
    (let [batch (or (:batch frame) (first (str/split frame-id #"-")))]
      (io/delete-file (io/file experiment-frames-root batch
                               (str frame-id ".edn")) true)))
  (shell/sh "git" "-C" apm-root "worktree" "prune"))

(defrecord CheckoutProvisioningBackend
    [inner-backend provisioner-fn rollback-fn assignment-context
     provisioned-frames]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (cond
      (= tool-id :assign-checkouts)
      (let [[options] args
            done (atom [])]
        (try
          (let [solver (provisioner-fn (checkout-options options :solver))]
            (swap! done conj solver)
            (reset! assignment-context options)
            (reset! provisioned-frames [solver])
            {:ok true
             :result {:environment-checkouts {:solver solver :student []}}})
          (catch Throwable t
            ;; Roll back whatever already succeeded, so a retry is possible.
            (doseq [frame @done]
              (try (rollback-fn frame) (catch Throwable _ nil)))
            {:ok false
             :error (str "assign-checkouts failed: " (.getMessage t))
             :rolled-back (count @done)})))

      (= tool-id :dispatch-student-fresh)
      (let [[opts packet] args
            context @assignment-context]
        (if-not context
          {:ok false :error "student dispatch has no recorded checkout assignment"}
          (let [arm (str "student-" (UUID/randomUUID))
                frame (try (provisioner-fn (checkout-options context arm))
                           (catch Throwable t t))]
            (if (instance? Throwable frame)
              ;; Roll back NOTHING else. All-or-nothing is correct at
              ;; :assign-checkouts, where no work has been done yet; here the
              ;; solver may have spent the whole cycle in its tree. Rolling the
              ;; assignment back on a student provisioning failure DELETED THE
              ;; SOLVER'S WORKTREE AND BRANCH -- verified: a transient failure on
              ;; student attempt 2 rolled back ["/tree/solver" "/tree/student-1"].
              ;; That inverts the reason all-or-nothing exists: it was so a retry
              ;; would be possible, and this made retry mean redoing the solve.
              ;; frames.bb removes its own partial worktree internally, so a
              ;; failed provision leaves nothing behind to undo.
              {:ok false
               :error (str "student checkout provisioning failed: "
                           (.getMessage ^Throwable frame))
               :rolled-back 0}
              (let [dispatch (tools/execute-tool
                              inner-backend tool-id
                              [(assoc (or opts {})
                                      :environment-checkout (:checkout frame)
                                      :environment-revision (:base-revision frame))
                               packet])]
                (if (:ok dispatch)
                  (do
                    (swap! provisioned-frames conj frame)
                    (assoc-in dispatch [:result :environment-checkout] frame))
                  (do
                    (try (rollback-fn frame) (catch Throwable _ nil))
                    dispatch)))))))

      :else
      (tools/execute-tool inner-backend tool-id args))))

(defn make-checkout-provisioning-backend
  ([inner-backend]
   (make-checkout-provisioning-backend inner-backend provision-frame! rollback-frame!))
  ([inner-backend provisioner-fn]
   (make-checkout-provisioning-backend inner-backend provisioner-fn rollback-frame!))
  ([inner-backend provisioner-fn rollback-fn]
   (->CheckoutProvisioningBackend inner-backend provisioner-fn rollback-fn
                                  (atom nil) (atom []))))

(defrecord GroundControlBackend [inner-backend dispatch-fn]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (if-let [memory-channel (get dispatch-channels tool-id)]
      (let [[opts packet] args
            ;; assoc LAST: the role fixes the channel and a caller cannot
            ;; override it.  A caller-supplied :push to the student would be a
            ;; containment breach, so this precedence is load-bearing.
            dispatch-result (try (dispatch-fn (assoc (or opts {})
                                                     :memory-channel memory-channel)
                                              packet)
                                 (catch Throwable t t))]
        (if (instance? Throwable dispatch-result)
          ;; A failed BELL is not a failed recall.  Recall is best-effort and
          ;; degrades to an empty offer; the bell is the work itself, so it must
          ;; surface as a structured tool failure rather than escape as an
          ;; exception and break the ToolBackend contract.
          {:ok false
           :error (str "ground-control dispatch failed: "
                       (.getMessage ^Throwable dispatch-result))}
          ;; This is the receipt emitted by the dispatcher that made the offer,
          ;; not a second account synthesized by the cycle machine.  Even an
          ;; empty/failed recall has one offered receipt.
          {:ok true
           :result (assoc dispatch-result
                          :memory-offers [(:evidence dispatch-result)])}))
      (tools/execute-tool inner-backend tool-id args))))

(defn make-ground-control-backend
  "Wrap a cycle backend with real ground-control dispatches.

  Dispatch tools take `[opts packet]`.  The role fixes the memory channel:
  solver is `:push+pull`; student is `:pull-only`.  `dispatch-fn` is injectable
  so envelope tests never bell a live agent."
  ([inner-backend]
   (make-ground-control-backend inner-backend
                                dispatch-with-recall/run-dispatch!))
  ([inner-backend dispatch-fn]
   (->GroundControlBackend inner-backend dispatch-fn)))

(defn make-problem
  ([] (make-problem (tools/make-mock-backend)))
  ([backend]
   (make-problem backend dispatch-with-recall/run-dispatch!
                 default-problem-state-root provision-frame!))
  ([backend dispatch-fn]
   (make-problem backend dispatch-fn default-problem-state-root provision-frame!))
  ([backend dispatch-fn state-root]
   (make-problem backend dispatch-fn state-root provision-frame!))
  ([backend dispatch-fn state-root provisioner-fn]
   (cycle/make-cycle-peripheral
    problem-domain-config problem-spec
    (make-checkout-provisioning-backend
     (make-ground-control-backend
      (make-problem-state-backend backend state-root)
      dispatch-fn)
     provisioner-fn))))
