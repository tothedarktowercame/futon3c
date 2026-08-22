(ns futon3c.apm.library-lane-adapters
  "Authority-preserving adapters for one library-lane run.

  This namespace does not loop or dispatch. Callers must supply the pinned
  problem unit, provisioned workspace, registered seats, ledger, and terminal
  evidence. Missing authority is returned as a typed refusal."
  (:require [clojure.string :as str]
            [futon3c.apm.live-launch-preparation :as launch]
            [futon3c.apm.live-proof-phases :as proof])
  (:import [java.math BigInteger]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def codex-workspace-roles #{:solver})
(def codex-seat-types {:solver :codex :proctor :codex})

(defn prepare-codex-only!
  "Prepare the solver workspace and the solver/proctor Codex seats while
  retaining live-launch-preparation's injected authority boundaries."
  [options]
  (launch/prepare! (assoc options :workspace-roles codex-workspace-roles
                          :seat-types codex-seat-types)))

(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str text) StandardCharsets/UTF_8))]
    (format "%064x" (BigInteger. 1 digest))))

(defn codex-frame-id
  "Return a deterministic numeric frame id in the content-addressed `f9...`
  namespace. Existing frame ids are an explicit collision boundary."
  [problem-id revision occupied-frame-ids]
  (let [decimal (.toString (BigInteger. (sha256 [problem-id revision]) 16))
        candidate (str "f9" (format "%078d" (BigInteger. decimal)))]
    (cond
      (not (set? occupied-frame-ids))
      {:ok false :error/code :occupied-frame-id-set-required}

      (contains? occupied-frame-ids candidate)
      {:ok false :error/code :codex-frame-id-collision :frame-id candidate}

      :else {:ok true :frame-id candidate})))

(defn- authority-findings
  [{:keys [unit ledger workspace seats actions state-paths agency-base
           role-card contract]} kind problem-id]
  (let [role (if (= :solve kind) :solver :proctor)]
    (cond-> []
      (not (contains? #{:preflight :solve :verify} kind))
      (conj :phase-kind-invalid)
      (not= problem-id (:problem/id unit)) (conj :problem-unit-mismatch)
      (not (and (string? (:frame/id unit))
                (re-matches #"f[0-9]+" (:frame/id unit))))
      (conj :frame-id-invalid)
      (not (and (map? (:problem unit))
                (every? string? ((juxt :repository :branch :revision :path :blob)
                                 (:problem unit)))))
      (conj :problem-pin-missing)
      (not (map? ledger)) (conj :ledger-missing)
      (not (map? workspace)) (conj :workspace-authority-missing)
      (not (map? (get seats role))) (conj :registered-seat-missing)
      (not (map? (get actions kind))) (conj :phase-action-missing)
      (not (some? (get state-paths kind))) (conj :phase-state-path-missing)
      (not (and (string? agency-base) (not (str/blank? agency-base))))
      (conj :agency-base-missing)
      (not (map? contract)) (conj :frame-contract-missing)
      (not= "a03d58e9fb261fb78b1ee90d9e497d395e4f1dd2" (:blob role-card))
      (conj :library-role-card-pin-mismatch))))

(defn make-phase-inputs-fn
  "Build the adapter injected into library-lane-runner/run-one!.

  CONFIG contains only observed authority. The returned function delegates
  request validation to live-proof-phases/build-request and never repairs a
  rejected request."
  [{:keys [unit ledger workspace seats actions state-paths agency-base] :as config}]
  (fn [{:keys [kind problem-id role-card contract receipts]}]
    (let [authority (assoc config :role-card role-card :contract contract)
          findings (authority-findings authority kind problem-id)
          role (if (= :solve kind) :solver :proctor)
          built (when (empty? findings)
                  (proof/build-request
                   {:kind kind :action (get actions kind) :ledger ledger
                    :unit unit :role-card role-card :seat (get seats role)
                    :workspace workspace
                    :solve-receipt (get receipts :solve)}))]
      (cond
        (seq findings)
        {:ok false :error/code :library-phase-authority-invalid
         :findings findings}

        (not (:ok built)) built

        :else
        {:ok true :kind kind :contract contract :request (:request built)
         :state-path (get state-paths kind) :agency-base agency-base}))))

(defn- safe-lean-name? [value]
  (and (string? value)
       (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_'.]*" value))))

(defn- ruling [outcome]
  (cond
    (and (true? (:verified-proof? outcome))
         (nat-int? (:remaining-sorries outcome))
         (zero? (:remaining-sorries outcome)))
    {:ok true :ruling :closed}

    (and (true? (:verified-library? outcome))
         (nat-int? (:library-sorry-warnings outcome))
         (zero? (:library-sorry-warnings outcome))
         (true? (:problem-open? outcome))
         (string? (:boundary outcome))
         (not (str/blank? (:boundary outcome))))
    {:ok true :ruling :partial-banked :boundary (:boundary outcome)}

    :else
    {:ok false :error/code :bank-ruling-evidence-insufficient
     :finding outcome}))

(defn make-bank-request-fn
  "Build the adapter injected into library-lane-runner/run-one!.

  OUTCOME-FN is the evidence boundary deciding closed versus partial-banked;
  no ruling is inferred merely from reaching the bank phase."
  [{:keys [unit workspace trunk-branch keying-target outcome-fn]}]
  (fn [{:keys [problem-id receipts]}]
    (let [verify-receipt (get receipts :verify)
          solve-receipt (get receipts :solve)
          outcome (when (fn? outcome-fn)
                    (outcome-fn {:problem-id problem-id :receipts receipts}))
          ruled (when outcome (ruling outcome))
          problem (:problem unit)
          path (:path problem)
          findings
          (cond-> []
            (not= problem-id (:problem/id unit)) (conj :problem-unit-mismatch)
            (not (map? workspace)) (conj :workspace-authority-missing)
            (not (string? (:repository problem))) (conj :repository-missing)
            (not (string? (:branch workspace))) (conj :source-branch-missing)
            (not (string? trunk-branch)) (conj :trunk-branch-missing)
            (not (safe-lean-name? keying-target)) (conj :keying-target-invalid)
            (not (and (string? path)
                      (= path (str "problems/" problem-id "/lean/Main.lean"))))
            (conj :problem-path-invalid)
            (not (and (string? (:receipt/id verify-receipt))
                      (re-matches #"[0-9a-f]{64}"
                                  (:receipt/id verify-receipt))))
            (conj :verify-receipt-missing)
            (not= (:receipt/final-head solve-receipt)
                  (:receipt/final-head verify-receipt))
            (conj :verify-head-mismatch)
            (nil? outcome) (conj :bank-outcome-evidence-missing))]
      (cond
        (seq findings)
        {:ok false :error/code :library-bank-authority-invalid
         :findings findings}

        (not (:ok ruled)) ruled

        :else
        (let [ruling (:ruling ruled)
              axiom-script
              (str "{ cat '" path "'; printf '\\n#print axioms "
                   keying-target "\\n'; } | lake env lean --stdin")]
          {:ok true
           :ruling ruling
           :repository (:repository problem)
           :source-branch (:branch workspace)
           :source-head (:receipt/final-head verify-receipt)
           :trunk-branch trunk-branch
           :frame-id (:frame/id unit)
           :problem-id problem-id
           :verify-receipt-id (:receipt/id verify-receipt)
           :lane-transition {:from :library
                             :to (if (= :closed ruling) :done :library)}
           :axiom-command ["bash" "-lc" axiom-script]
           :rollup-command ["lake" "build" "ConstructionTargets"]
           :status-command ["lake" "env" "lean" path]
           :status-path (str "problems/" problem-id "/status.json")
           :receipt/boundary (:boundary ruled)})))))
