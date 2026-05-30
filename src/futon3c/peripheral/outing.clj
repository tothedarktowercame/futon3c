(ns futon3c.peripheral.outing
  "First-outing harness — M-war-machine-first-outing INSTANTIATE.

   Wraps the gated cycle (R-E: G1 conformance / G2 regression / G3 earned-
   discharge), the halt logic (R-D: hard halts + soft stops + stuck-detector),
   the bounds (R-K), and the morning-review run-summary artefact (R-M) around
   war-machine-pilot's begin/close-live-cycle!. The driver is an agent in the
   loop (R-B): it supplies the EVAL choice and the earned PRINT each cycle; this
   ns records the gated outcome and decides continue/halt.

   This refines the VERIFY model (logic/outing_invariants.clj): a cycle is only
   recorded as committed when G1∧G2∧G3 hold; halt conditions mirror R-D. The
   run-summary at data/outings/<date>.edn is the R-M surface.

   Honest scope: the per-cycle G1 verifier and the PRINT/commit are run by the
   driver (the verifier is a bb script; the commit is path-limited per the
   shared-working-tree finding). This ns is the bookkeeping + decision spine,
   not an autonomous executor."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint]
            [futon3c.logic.sorry-closures :as sc]))

(defonce !outings (atom {}))

(def ^:private stuck-window 3)  ; R-D: K consecutive no-progress cycles

(defn start-outing!
  [{:keys [run-id date max-cycles wall-clock-min posture]
    :or {max-cycles 15 wall-clock-min 90 posture :supervised-staging}}]
  (swap! !outings assoc run-id
         {:run-id run-id :date date
          :max-cycles max-cycles :wall-clock-min wall-clock-min
          :posture posture :cycles [] :status :running})
  {:ok true :run-id run-id :max-cycles max-cycles})

(defn- recent-no-progress?
  "Stuck-detector: last `stuck-window` cycles all made NO progress of any kind —
   no earned discharge, no top-shift, AND no ∇-deform (inline decomposition /
   niche-construction). A decompose cycle (authoring an excursion + mining
   bounded sub-sorries in response to a deep sorry) IS progress: the runner,
   being the same system, extends its own substrate rather than stalling. So a
   run of decompose cycles never trips the stuck-detector."
  [cycles]
  (let [tail (take-last stuck-window cycles)]
    (and (= stuck-window (count tail))
         (every? (fn [c] (and (not (:claimed-discharge? c))
                              (not (:top-shift? c))
                              (not (:delta-grad? c)))) tail))))

(defn- hard-halt-reason
  "Contract-breach hard halt — RESERVED for the one thing that poisons trust in
   the whole run: a FAKE-FINISH (claimed a discharge the field does not support,
   G3 / V2 no-teleport). 'War Machine, not a pram': ordinary friction does NOT
   hard-halt. nil if none."
  [{:keys [claimed-discharge? top-shift?]}]
  (when (and claimed-discharge? (not top-shift?)) :g3-fake-finish))

(defn- quarantine-reason
  "Containable friction — record loudly + DO NOT commit, but the run PERSISTS on
   other work (not an ejector-button event): an uncaught error in a cycle, a
   slow/missed tick, a non-conformant frame, or a SINGLE regression. The
   silent-corruption defense is satisfied by detect+surface, not by aborting.
   nil if the cycle is clean."
  [{:keys [g1-conforms? g2-regression-ok? error? tick-timeout?]}]
  (cond
    error?                     :uncaught-error
    tick-timeout?              :tick-sla-breach
    (false? g1-conforms?)      :g1-nonconformance
    (false? g2-regression-ok?) :g2-regression
    :else nil))

(defn record-cycle!
  "Record one gated cycle and evaluate halt conditions.
   cycle keys: :n :sorry-id :cg-id :predicted :realised :prediction-error
   :top-shift? :claimed-discharge? :g1-conforms? :g2-regression-ok?
   :action-class :tick-wait-s :error? :tick-timeout? :frame-path
   Returns {:continue? bool :halt {:kind :hard|:soft :reason kw} :n-done int}.
   A cycle is :committed? only when all gates pass."
  [run-id {:keys [claimed-discharge?] :as cycle}]
  (let [hard (hard-halt-reason cycle)
        quar (quarantine-reason cycle)
        ;; commit only a clean, earned discharge (no hard breach, no quarantine)
        committed? (boolean (and (nil? hard) (nil? quar) claimed-discharge?))
        rec (assoc cycle :committed? committed?
                         :quarantined? (boolean quar) :quarantine-reason quar
                         :recorded-at (str (java.time.Instant/now)))
        st  (-> (swap! !outings update-in [run-id :cycles] conj rec)
                (get run-id))
        cycles (:cycles st)
        n (count cycles)
        n-regress (count (filter #(= :g2-regression (:quarantine-reason %)) cycles))
        soft (cond
               hard nil
               (>= n (:max-cycles st))         :max-cycles
               (>= n-regress 3)                :regression-storm   ; substrate unstable
               (recent-no-progress? cycles)    :stuck-detector
               :else nil)
        halt (cond hard {:kind :hard :reason hard}
                   soft {:kind :soft :reason soft}
                   :else nil)]
    (when halt (swap! !outings assoc-in [run-id :status]
                      (if (= :hard (:kind halt)) :halted-hard :stopped)))
    {:continue? (nil? halt) :halt halt :quarantined quar :n-done n}))

(defn finalize-outing!
  "Write the R-M run-summary artefact and return it."
  [run-id reason]
  (let [st (get @!outings run-id)
        cycles (:cycles st)
        discharges (filter :committed? cycles)
        decompositions (filter :delta-grad? cycles)
        quarantined (filter :quarantined? cycles)
        regressions (filter #(false? (:g2-regression-ok? %)) cycles)
        nonconf     (filter #(false? (:g1-conforms? %)) cycles)
        sum-pe (reduce + 0.0 (keep :prediction-error cycles))
        summary {:run-id run-id :date (:date st) :posture (:posture st)
                 :end-reason reason :status (:status st)
                 :cycles-attempted (count cycles)
                 :cycles-committed (count discharges)
                 :discharged-sorries (mapv :sorry-id discharges)
                 :decompositions (count decompositions)
                 :excursions-authored (vec (keep :artefact decompositions))
                 :quarantined (count quarantined)
                 :quarantine-reasons (frequencies (keep :quarantine-reason quarantined))
                 :regressions-caught (count regressions)
                 :nonconformances (count nonconf)
                 :sum-prediction-error sum-pe
                 :top-shift-rate (if (seq cycles)
                                   (/ (double (count (filter :top-shift? cycles)))
                                      (count cycles)) 0.0)
                 :tick-wait-seconds (mapv :tick-wait-s cycles)
                 :cycles cycles}
        dir (io/file "data/outings")
        _ (.mkdirs dir)
        path (str "data/outings/" (:date st) "-" (subs run-id 0 (min 13 (count run-id))) ".edn")]
    (spit path (with-out-str (clojure.pprint/pprint summary)))
    (swap! !outings assoc-in [run-id :status] :finalized)
    {:ok true :path path :summary (dissoc summary :cycles)}))

(defn live-regression-ok?
  "Global R-A.2 / G2 check over the whole registry (not just the chosen sorry):
   true iff no discharged sorry is open again."
  []
  (= :ok (:outcome (sc/check))))

(defn register-live-family!
  "R-A.2 dual-use: register :sorry-closures-stick as a live probe family so it
   surfaces on the Arxana Live-Violations surface (hourly probe) in addition to
   the per-cycle G2 gate."
  []
  (sc/register!))
