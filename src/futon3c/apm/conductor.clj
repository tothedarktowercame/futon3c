(ns futon3c.apm.conductor
  "Callable orchestration for one APM problem frame.

   This namespace owns sequence, checkpointing, and a compact operation log.
   The problem peripheral remains the sole owner of cycle state and invariants."
  (:require [clojure.string :as str]
            [futon3c.apm.conductor-binding :as binding]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]))

(defn- failure [handle code message & [details]]
  (cond-> (assoc handle :ok false
                 :error {:error/component :apm-conductor
                         :error/code code
                         :error/message message})
    details (assoc-in [:error :error/context] details)))

(defn- logged [handle tool result]
  (update handle :log (fnil conj [])
          (cond-> {:tool tool :ok (true? (:ok result))}
            (:result result) (assoc :result (:result result))
            (:error/code result) (assoc :error/code (:error/code result))
            (:error/message result) (assoc :error/message (:error/message result)))))

(defn- raw-step [handle tool args]
  (if (false? (:ok handle))
    {:handle handle}
    (try
      (let [result (runner/step (:peripheral handle) (:state handle)
                                {:tool tool :args (vec args)})
            next-handle (logged handle tool result)]
        (if (:ok result)
          {:handle (assoc next-handle :state (:state result))
           :result (:result result)}
          {:handle (failure next-handle
                            (or (:error/code result) :tool-refused)
                            (or (:error/message result)
                                (str tool " refused"))
                            (:error/context result))}))
      (catch Throwable t
        {:handle (failure (logged handle tool {:ok false})
                          :tool-threw
                          (str tool " threw: " (.getMessage t)))}))))

(defn- checkpoint [handle]
  (if (false? (:ok handle))
    {:handle handle}
    (raw-step handle :problem-save [])))

(defn- saved-step [handle tool args]
  (let [{h :handle result :result} (raw-step handle tool args)]
    (if (false? (:ok h))
      {:handle h}
      (let [{saved :handle} (checkpoint h)]
        {:handle saved :result result}))))

(defn- advance [handle payload]
  (saved-step handle problem/advance
              ["apm-conductor" (:problem-id (:config handle)) payload]))

(defn- receipt-offers [receipt]
  (let [body (:body receipt)
        job-id (:job-id body)
        memory-ids (get-in body [:memory-use :memory-use/surfaced-ids])]
    (map-indexed (fn [index memory-id]
                   {:offer/id (str "offer/" job-id "/" index)
                    :offer/memory-id memory-id})
                 memory-ids)))

(defn- memory-offers [state]
  (->> (:steps state)
       (keep (fn [{:keys [tool result]}]
               (when (#{:dispatch-solver :dispatch-student-fresh} tool)
                 (:memory-offers result))))
       (mapcat identity)
       (mapcat receipt-offers)
       vec))

(defn- require-mission [handle opts]
  (if (and (string? (:mission opts)) (not (str/blank? (:mission opts))))
    nil
    (failure handle :mission-absent
             "dispatch requires a non-blank :mission")))

(defn- initial-handle [config]
  (let [evidence-store (or (:evidence-store config)
                           (f1b/make-futon1b-backend
                            (:evidence-store-url config)))
        peripheral (or (:peripheral config) (problem/make-problem))
        mode (:mode config)
        context {:session-id (:session-id config)
                 :problem-id (:problem-id config)
                 :cycle/mode mode
                 :cycle/deposit-state (or (:deposit-state config) :n/a)
                 :evidence-store evidence-store
                 :harness-repo (:harness-repo config)
                 :lean-repo (:lean-repo config)
                 :agency-endpoint (:agency-endpoint config)
                 :authorization-revision (:authorization-revision config)
                 :authorization-output (:authorization-output config)
                 :conductor (:conductor config)
                 :author (get-in config [:conductor :agent])}
        started (runner/start peripheral context)
        handle {:ok true :peripheral peripheral :state (:state started)
                :log [] :deposits [] :config config}]
    (if (:ok started)
      handle
      (failure handle (or (:error/code started) :start-refused)
               (or (:error/message started) "problem peripheral refused start")))))

(defn open-frame!
  "Open and register a frame, returning a handle at :guided-solve.

   Tests may supply :peripheral and :evidence-store in config; production uses
   problem/make-problem and the configured Futon1b backend."
  [config]
  (try
    (let [
          ;; First-production findings (frame-3 mis-open, 2026-08-16): a nil
          ;; mode opened a malformed cycle; :deposit-state and :conductor were
          ;; never threaded, silently disabling deposit-state validity and
          ;; the atomic dispatch+park integration. Validate and thread.
          mode (:mode config)
          _ (when-not (contains? #{:store-mode :harness-mode} mode)
              (throw (ex-info (str "open-frame! requires :mode :store-mode or "
                                   ":harness-mode; got " (pr-str mode))
                              {:error/code :invalid-frame-mode})))
          initial (initial-handle config)]
      (if (false? (:ok initial))
        initial
        (let [{h1 :handle begin :result}
              (saved-step initial :begin-problem-cycle
                          [(:conductor config) (:problem-id config)])
              {h2 :handle registration :result}
              (saved-step h1 :read-registration [(:registration-path config)])
              {h3 :handle validation :result}
              (saved-step h2 :validate-registration [registration])
              {h4 :handle snapshot :result} (saved-step h3 :snapshot-store [])
              {h5 :handle frozen :result} (saved-step h4 :freeze-stratum [])
              checkout (merge (:checkout config) {:problem (:problem-id config)})
              {h6 :handle assignment :result}
              (saved-step h5 :assign-checkouts [checkout])
              solver (get-in assignment [:environment-checkouts :solver])
              register-payload
              {:registration (or (:registration validation) registration)
               :store-snapshot snapshot
               :stratum-frozen-at (:cycle/stratum-frozen-at frozen)
               :environment-revision (:base-revision solver)
               :harness-revision (:harness-revision begin)
               :environment-checkouts (:environment-checkouts assignment)}
              {h7 :handle} (advance h6 register-payload)
              frame (:frame config)
              {h8 :handle}
              (saved-step h7 :emit-frame
                          [{:scaffold-path (str (:scaffold-path frame))
                            :closing-path (str (:closing-path frame))
                            :containment-witness-path
                            (some-> (:witness-path frame) str)
                            :containment-claimed? (some? (:witness-path frame))}])
              {h9 :handle} (advance h8 {})
              opened (assoc h9 :cycle-id (get-in h9 [:state :current-cycle-id]))
              conductor (:conductor config)
              agent-id (when (map? conductor) (:agent conductor))
              session-id (when (map? conductor) (:session conductor))]
          (if (and agent-id session-id (not (false? (:ok opened))))
            (let [installed (binding/install! agent-id session-id opened)]
              (if (:ok installed)
                opened
                (failure opened (:error/code installed)
                         "problem conductor binding refused" installed)))
            opened))))
    (catch Throwable t
      (failure {:ok false :peripheral nil :state nil :log [] :deposits []
                :config config}
               :open-frame-threw (.getMessage t)))))

(defn- dispatch! [handle tool opts packet]
  (if-let [refusal (require-mission handle opts)]
    refusal
    (let [parked-version (inc (binding/handle-version handle))
          opts (assoc (or opts {})
                      :conductor/cycle-id (:cycle-id handle)
                      :conductor/version parked-version)]
      (:handle (saved-step handle tool [opts packet])))))

(defn dispatch-solver! [handle opts packet]
  (dispatch! handle :dispatch-solver opts packet))

(defn guide-solver!
  ([handle _opts _packet]
   (failure handle :guidance-type-absent
            "guide-solver requires a typed-bell performative"))
  ([handle bell-type opts packet]
   (if (contains? prereg/guidance-bell-types bell-type)
     (dispatch! handle :guide-solver (assoc (or opts {}) :bell-type bell-type)
                packet)
     (failure handle :guidance-type-invalid
              "guide-solver requires a valid typed-bell performative"
              {:bell-type bell-type}))))

(defn- recorded-results [state tool]
  (->> (:steps state)
       (keep (fn [step]
               (when (= tool (:tool step)) (:result step))))
       vec))

(defn dispatch-student! [handle opts packet]
  (let [handle (if (= :promote-solver (get-in handle [:state :current-phase]))
                 (:handle
                  (advance handle
                           {:promotion-result
                            (recorded-results (:state handle)
                                              :promote-artifact)}))
                 handle)]
    (dispatch! handle :dispatch-student-fresh opts packet)))

(def ^:private scribe-card-path
  "/home/joe/code/futon3c/holes/labs/M-apm-demonstration/role-cards/scribe.md")

(defn- recorded-job-ids [state tool]
  (->> (:steps state)
       (keep (fn [step]
               (when (= tool (:tool step))
                 (get-in step [:result :job-id]))))
       vec))

(defn dispatch-scribe!
  "Dispatch the registered scribe with machine-owned cycle references."
  [handle opts packet]
  (let [state (:state handle)
        context {:problem-id (get-in handle [:config :problem-id])
                 :cycle-id (:cycle-id handle)
                 :solver-job-ids (recorded-job-ids state :dispatch-solver)
                 :student-job-ids (recorded-job-ids state
                                                    :dispatch-student-fresh)
                 :scribe-card-path scribe-card-path}]
    (dispatch! handle :dispatch-scribe (merge (or opts {}) context) packet)))

(defn promote-artifact!
  "Record one promotion through the phase-gated problem tool."
  [handle opts]
  (:handle (saved-step handle :promote-artifact [(or opts {})])))

(defn record-scribe-lanes!
  "Record one scribe lane report through the phase-gated problem tool."
  [handle opts]
  (:handle (saved-step handle :record-scribe-lanes [(or opts {})])))

(defn record-solver-attempt! [handle attempt extra-outputs]
  (try
    (:handle
     (advance handle
              (merge {:solver-attempt attempt
                      :memory-offers (memory-offers (:state handle))}
                     extra-outputs)))
    (catch Throwable t
      (failure handle :record-solver-threw (.getMessage t)))))

(defn deposit! [handle payload]
  (try
    (let [{h1 :handle receipt :result}
          (saved-step handle :write-substrate [payload])]
      (if (false? (:ok h1))
        h1
        (let [memory-id (:memory-id receipt)
              h1 (update h1 :deposits (fnil conj []) memory-id)
              {h2 :handle}
              (advance h1 {:intervention {:kind :store-write
                                           :memory-id memory-id}})]
          h2)))
    (catch Throwable t
      (failure handle :deposit-threw (.getMessage t)))))

(defn record-students! [handle attempts uses]
  (try
    (:handle (advance handle {:student-attempts (vec attempts)
                              :memory-uses (vec uses)}))
    (catch Throwable t
      (failure handle :record-students-threw (.getMessage t)))))

(defn adjudicate! [handle disposition]
  (try
    (let [promotions (vec (or (:promotion-result disposition) []))
          disposition (dissoc disposition :promotion-result)
          {h1 :handle} (saved-step handle :write-disposition [disposition])
          {h2 :handle} (advance h1 {})
          h3 (reduce (fn [h promotion]
                       (if (false? (:ok h))
                         (reduced h)
                         (:handle (saved-step h :promote-artifact [promotion]))))
                     h2 promotions)
          {h4 :handle} (advance h3 {:promotion-result promotions})]
      h4)
    (catch Throwable t
      (failure handle :adjudicate-threw (.getMessage t)))))

(defn close! [handle]
  (try
    (let [{h1 :handle measurement :result}
          (saved-step handle :record-measurement [])
          {h2 :handle} (saved-step h1 :emit-capability-probes [])
          {h3 :handle trace-envelope :result} (saved-step h2 :emit-trace [])
          {h4 :handle validation :result} (saved-step h3 :validate-trace [])
          {h5 :handle} (saved-step h4 :write-authorization [])
          ;; The terminal advance clears the cycle id, so checkpoint immediately
          ;; before it rather than attempting an impossible post-sentinel save.
          {h6 :handle} (checkpoint h5)
          {h7 :handle} (raw-step h6 problem/advance
                                ["apm-conductor"
                                 (:problem-id (:config h6)) {}])
          failures (vec (concat (:producer-failures trace-envelope)
                                (:failures validation)))]
      (assoc h7
             :envelope {:measurement measurement
                        :failures failures
                        :launchable? (true? (:launchable? validation))}
             :cycle-id (or (:cycle-id handle)
                           (get-in handle [:state :current-cycle-id]))))
    (catch Throwable t
      (failure handle :close-threw (.getMessage t)))))

(defn resume
  "Restore a saved cycle version through the peripheral's public load tool.
   A handle is required because it carries the peripheral construction and
   runtime evidence backend; nil is refused rather than guessing either."
  [handle cycle-id version]
  (if-not handle
    (failure {:ok false :peripheral nil :state nil :log [] :deposits []}
             :resume-handle-required
             "resume requires a conductor handle with its peripheral runtime")
    (try
      (let [{loaded :handle restored :result}
            (raw-step handle :problem-load [cycle-id version])]
        (if (false? (:ok loaded))
          loaded
          ;; For a load, the tool result IS the restored state. Retaining the
          ;; runner's wrapper state would embed that state again as the load
          ;; step's result; its runtime evidence-store then points back through
          ;; the emitted load evidence, making the next EDN checkpoint cyclic.
          ;; Install the validated public-tool result, while the conductor log
          ;; remains the takeover record, then checkpoint the restored state.
          (:handle (checkpoint (assoc loaded :state restored)))))
      (catch Throwable t
        (failure handle :resume-threw (.getMessage t))))))

(defn resume-fresh
  "Rebuild the normal peripheral runtime, then load a named saved cycle."
  ([source-handle cycle-id version]
   (resume-fresh source-handle cycle-id version nil))
  ([source-handle cycle-id version conductor-identity]
   (let [config (cond-> (:config source-handle)
                  conductor-identity
                  (update :conductor merge conductor-identity))
         fresh (initial-handle config)]
     (if (false? (:ok fresh)) fresh (resume fresh cycle-id version)))))
