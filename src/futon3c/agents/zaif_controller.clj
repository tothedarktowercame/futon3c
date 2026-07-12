(ns futon3c.agents.zaif-controller
  "Pure v0 arm controller for the zaif profile.

   The model never sees these terms. The runner may use the selected arm around
   the model and records the decision as evidence for later scoring."
  (:require [clojure.string :as str])
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
  "D1-compatible posting-stat EIG proxy: rarer observed terms have more value."
  [stats]
  (let [total (max 1.0 (as-double (:total-docs stats) 1.0))
        dfs (seq (or (:dfs stats)
                     (when (contains? stats :df) [(:df stats)])))]
    (if dfs
      (/ (reduce + (map #(Math/log (/ (inc total) (inc (max 0.0 (as-double % total)))))
                        dfs))
         (double (count dfs)))
      0.0)))

(defn- retrieve-value
  [observations]
  (let [eig (or (:retrieve-eig observations)
                (:eig observations)
                (idf-ish (:posting-stats observations)))
        tokens (or (:estimated-tokens observations)
                   (get-in observations [:posting-stats :estimated-tokens])
                   (:default-retrieve-tokens constants))]
    (- (* (:retrieve-eig-scale constants) (as-double eig 0.0))
       (* (:retrieve-token-cost constants) (as-double tokens (:default-retrieve-tokens constants))))))

(defn- act-value
  [task-belief gamma-used]
  (* (:act-pragmatic-scale constants)
     gamma-used
     (as-double (or (:act-value task-belief)
                    (:pragmatic-value task-belief)
                    (:expected-utility task-belief))
                0.0)))

(defn- ask-value
  [c-belief]
  (- (* (:ask-eig-scale constants)
        (as-double (or (:operator-c-uncertainty c-belief)
                       (:uncertainty c-belief)
                       (:ask-eig c-belief))
                   0.0))
     (:operator-attention-cost constants)))

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
                     :estimated-tokens n}}

   Higher v0 G term wins. Asymmetric admissibility is upstream: callers pass
   only beliefs/actions that are allowed to be considered."
  [{:keys [task-belief c-belief gamma mission observations]}]
  (let [gamma-used (gamma-for-mission gamma mission)
        g-terms {:retrieve (retrieve-value observations)
                 :act (act-value task-belief gamma-used)
                 :ask (ask-value c-belief)
                 :yield (:yield-baseline constants)}
        arm (choose-arm g-terms)]
    {:arm arm
     :g-terms g-terms
     :gamma-used gamma-used
     :mission mission
     :why (format "zaif v0 chose %s: retrieve %.3f, act %.3f, ask %.3f, yield %.3f"
                  (name arm) (:retrieve g-terms) (:act g-terms)
                  (:ask g-terms) (:yield g-terms))}))

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
  [{:keys [agent-id sid decision inputs]}]
  {:evidence/id (str "e-" (UUID/randomUUID))
   :evidence/subject {:ref/type :agent :ref/id (str agent-id)}
   :evidence/type :coordination
   :evidence/claim-type :step
   :evidence/author (str agent-id)
   :evidence/session-id (str sid)
   :evidence/at (str (java.time.Instant/now))
   :evidence/tags [:zaif :arm-choice]
   :evidence/body {:event :zaif-arm-choice
                   :arm (:arm decision)
                   :g-terms (:g-terms decision)
                   :gamma-used (:gamma-used decision)
                   :mission (:mission decision)
                   :why (:why decision)
                   :inputs-digest (inputs-digest inputs)}})

(defn persist-decision!
  "Fire-and-forget persistence. Store lookup is dynamic so a live-loaded runner
   picks up the current dev evidence store; failures never affect the turn."
  [ctx]
  (future
    (try
      (when-let [!store (try (requiring-resolve 'futon3c.dev/!evidence-store)
                             (catch Throwable _ nil))]
        (when-let [store (some-> !store deref deref)]
          (let [append! (requiring-resolve 'futon3c.evidence.boundary/append!)]
            (append! store (decision-evidence-entry ctx)))))
      (catch Throwable _ nil))))

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
