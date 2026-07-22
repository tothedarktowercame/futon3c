(ns futon3c.agents.zaif-controller
  "Pure v0 arm controller for the zaif profile.

   The model never sees these terms. The runner may use the selected arm around
   the model and records the decision as evidence for later scoring."
  (:require [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary])
  (:import [java.security MessageDigest]
           [java.util UUID]))

(def constants
  "Fixed, uncalibrated Z2 v0 constants. These are deliberately documented in
   data so tests can assert the arithmetic without tuning against outcomes."
  {:retrieve-eig-scale 1.0
   :retrieve-token-cost 0.0005
   :default-retrieve-tokens 800
   :act-pragmatic-scale 1.0
   :ask-eig-scale 1.0
   :operator-attention-cost 0.65
   :yield-baseline 0.0})

(defn- finite-number?
  [x]
  (and (number? x)
       (not (Double/isNaN (double x)))
       (not (Double/isInfinite (double x)))))

(defn- as-double
  [x fallback]
  (if (finite-number? x)
    (double x)
    (double fallback)))

(defn gamma-for-mission
  "Read γ(mission), using the R14 coerce-state semantics for unburned or missing
   cells: uniform prior 1.0."
  [gamma mission]
  (let [cell (or (get gamma mission)
                 (get gamma (keyword mission))
                 (get gamma (str mission)))]
    (cond
      (finite-number? cell) (double cell)
      (finite-number? (:policy-precision cell)) (double (:policy-precision cell))
      :else 1.0)))

(defn- idf-ish
  "D1-compatible posting-stat EIG proxy: rarer observed terms have more value.
   Normalized by log(total+1) into [0,1) so the proxy is commensurate with
   the ask/act value scales — unnormalized, any non-trivial context text
   yields values in the 2.5-4 range and :retrieve dominates every arm at
   every constant, which would empty Z3a's divergent-round set."
  [stats]
  (let [total (max 1.0 (as-double (:total-docs stats) 1.0))
        dfs (seq (or (:dfs stats)
                     (when (contains? stats :df) [(:df stats)])))]
    (if dfs
      (/ (reduce + (map #(Math/log (/ (inc total) (inc (max 0.0 (as-double % total)))))
                        dfs))
         (* (double (count dfs))
            (Math/log (inc total))))
      0.0)))

(defn- retrieve-value
  ([observations]
   (retrieve-value observations constants))
  ([observations consts]
   (let [eig (or (:retrieve-eig observations)
                (:eig observations)
                (idf-ish (:posting-stats observations)))
        tokens (or (:estimated-tokens observations)
                   (get-in observations [:posting-stats :estimated-tokens])
                   (:default-retrieve-tokens consts))]
     (- (* (:retrieve-eig-scale consts) (as-double eig 0.0))
        (* (:retrieve-token-cost consts) (as-double tokens (:default-retrieve-tokens consts)))))))

(defn- act-value
  ([task-belief gamma-used]
   (act-value task-belief gamma-used constants))
  ([task-belief gamma-used consts]
   (* (:act-pragmatic-scale consts)
      gamma-used
      (as-double (or (:act-value task-belief)
                     (:pragmatic-value task-belief)
                     (:expected-utility task-belief))
                 0.0))))

(defn- ask-value
  ([c-belief]
   (ask-value c-belief constants))
  ([c-belief consts]
   (- (* (:ask-eig-scale consts)
        (as-double (or (:operator-c-uncertainty c-belief)
                       (:uncertainty c-belief)
                       (:ask-eig c-belief))
                   0.0))
     (:operator-attention-cost consts))))

(defn- choose-arm
  [g-terms]
  (->> [:retrieve :act :ask :yield]
       (map (fn [arm] [arm (get g-terms arm)]))
       (sort-by (fn [[arm value]] [(- (double value)) (case arm :act 0 :retrieve 1 :ask 2 :yield 3)]))
       ffirst))

(defn decide
  "Choose one of :retrieve/:act/:ask/:yield from already-built beliefs.

   Input shape is intentionally plain data:
     {:task-belief {:act-value ...}
      :c-belief {:operator-c-uncertainty ...}
      :gamma {mission {:policy-precision ...}} ; or scalar cells
      :mission \"M-...\"
      :observations {:posting-stats {:total-docs n :dfs [...]}
                     :estimated-tokens n}
      :constants-override {:operator-attention-cost 0.15, ...}}

   Higher v0 G term wins. Asymmetric admissibility is upstream: callers pass
   only beliefs/actions that are allowed to be considered.

   When :constants-override is supplied, its entries replace the
   corresponding defaults in `constants` — this is how Z3a's dual-constant
   paired comparison runs the same pure kernel under two costs without
   duplicating the arithmetic (the shared-kernel discipline)."
  [{:keys [task-belief c-belief gamma mission observations constants-override]}]
  (let [consts (if constants-override
                 (merge constants constants-override)
                 constants)
        gamma-used (gamma-for-mission gamma mission)
        g-terms {:retrieve (retrieve-value observations consts)
                 :act (act-value task-belief gamma-used consts)
                 :ask (ask-value c-belief consts)
                 :yield (:yield-baseline consts)}
        arm (choose-arm g-terms)]
    {:arm arm
     :g-terms g-terms
     :gamma-used gamma-used
     :mission mission
     :operator-attention-cost (:operator-attention-cost consts)
     :why (format "zaif v0 chose %s: retrieve %.3f, act %.3f, ask %.3f, yield %.3f"
                  (name arm) (:retrieve g-terms) (:act g-terms)
                  (:ask g-terms) (:yield g-terms))}))

(def ^:private z3a-sweep-constant
  "The Z3a sweep value for operator-attention-cost. At 0.15, ZU-2's replay
   showed clean arm-separation (all corrections → :ask, all non-corrections
   → :act). NOT shipped — recorded alongside the shipped 0.65 for paired
   comparison (z3-prereg.md §Z3a)."
  0.15)

(defn dual-constants
  "Return the two operator-attention-cost values Z3a records per round:
   the shipped constant and the sweep value. The Z3a scorer pairs decisions
   across these two costs from the same inputs."
  []
  [{:operator-attention-cost (:operator-attention-cost constants)
    :label :shipped}
   {:operator-attention-cost z3a-sweep-constant
    :label :sweep}])

(defn dual-decide
  "Run decide() under both Z3a constants from the same inputs. Returns a
   sequence of {:constant-override :label :decision} maps, one per constant.
   The inputs are shared (the shared-kernel discipline); only the constant
   differs. Pure and deterministic — re-derivable by calling decide again
   with the recorded inputs and constant-override."
  [inputs]
  (for [{:keys [operator-attention-cost label]} (dual-constants)]
    {:label label
     :operator-attention-cost operator-attention-cost
     :decision (decide (assoc inputs :constants-override
                              {:operator-attention-cost operator-attention-cost}))}))

(defn- sha256-16
  [x]
  (let [s (pr-str x)
        md (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %)
                    (take 8 (.digest md (.getBytes s "UTF-8")))))))

(defn inputs-digest
  [inputs]
  {:sha256-16 (sha256-16 inputs)
   :chars (count (pr-str inputs))})

(defn decision-evidence-entry
  "Build an evidence entry for a ZAIF arm decision.

   Optional ctx keys for Z3a dual-constant recording:
     :constant       — the operator-attention-cost used (0.65 or 0.15)
     :constant-label — :shipped or :sweep
     :pairing-key    — shared key linking paired decisions from the same round
     :round          — the round number within the turn
   When :constant is absent, the entry is backward-compatible with the
   pre-Z3a shape (single decision per round)."
  [{:keys [agent-id sid turn-id decision inputs constant constant-label pairing-key round]}]
  (let [base-body {:event :zaif-arm-choice
                   :turn-id (some-> turn-id str)
                   :arm (:arm decision)
                   :g-terms (:g-terms decision)
                   :gamma-used (:gamma-used decision)
                   :mission (:mission decision)
                   :operator-attention-cost (:operator-attention-cost decision)
                   :why (:why decision)
                   :inputs-digest (inputs-digest inputs)
                   :inputs-snapshot inputs}]
    {:evidence/id (str "e-" (UUID/randomUUID))
     :evidence/subject {:ref/type :agent :ref/id (str agent-id)}
     :evidence/type :coordination
     :evidence/claim-type :step
     :evidence/author (str agent-id)
     :evidence/session-id (str sid)
     :evidence/at (str (java.time.Instant/now))
     :evidence/tags [:zaif :arm-choice]
     :evidence/body (cond-> base-body
                      (some? constant)       (assoc :constant constant)
                      (some? constant-label) (assoc :constant-label constant-label)
                      (some? pairing-key)    (assoc :pairing-key pairing-key)
                      (some? round)          (assoc :round round))}))

(defonce ^:private !persistence-status
  (atom {:ok-count 0 :failure-count 0 :last-error nil :last-evidence-id nil}))

(defn persistence-status
  "Return loss-accounting counters for ZAIF arm-decision writes."
  []
  @!persistence-status)

(defn persist-decision!
  "Synchronously persist a ZAIF arm decision through the durable boundary."
  [{:keys [evidence-store] :as ctx}]
  (let [entry (decision-evidence-entry ctx)
        evidence-id (:evidence/id entry)]
    (try
      (when-not evidence-store
        (throw (ex-info "ZAIF decision evidence store is unavailable"
                        {:agent-id (:agent-id ctx) :turn-id (:turn-id ctx)})))
      (let [receipt (boundary/append! evidence-store entry)]
        (when-not (:ok receipt)
          (throw (ex-info "ZAIF decision persistence was rejected"
                          {:evidence-id evidence-id :receipt receipt})))
        (swap! !persistence-status
               (fn [status]
                 (-> status
                     (update :ok-count (fnil inc 0))
                     (assoc :last-error nil :last-evidence-id evidence-id))))
        receipt)
      (catch Throwable t
        (swap! !persistence-status
               (fn [status]
                 (-> status
                     (update :failure-count (fnil inc 0))
                     (assoc :last-error (.getMessage t)
                            :last-evidence-id evidence-id))))
        (binding [*out* *err*]
          (println (str "[zaif-evidence] FATAL " evidence-id ": " (.getMessage t)))
          (flush))
        (throw t)))))

(defn env-profile
  [s]
  (let [p (cond
            (keyword? s) (name s)
            (some? s) (some-> s str str/trim str/lower-case)
            :else nil)]
    (case p
      "zaif" :zaif
      "zai" :zai
      nil :zai
      "" :zai
      :zai)))
