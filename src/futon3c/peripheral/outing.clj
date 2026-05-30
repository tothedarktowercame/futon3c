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
  "Stuck-detector: last `stuck-window` cycles all had no earned discharge AND no
   top-shift."
  [cycles]
  (let [tail (take-last stuck-window cycles)]
    (and (= stuck-window (count tail))
         (every? (fn [c] (and (not (:claimed-discharge? c))
                              (not (:top-shift? c)))) tail))))

(defn- hard-halt-reason
  "R-D hard halts. nil if none."
  [{:keys [g1-conforms? g2-regression-ok? claimed-discharge? top-shift?
           error? tick-timeout?]}]
  (cond
    error?                                   :uncaught-error
    tick-timeout?                            :tick-sla-breach
    (not g1-conforms?)                       :g1-nonconformance
    (not g2-regression-ok?)                  :g2-regression
    (and claimed-discharge? (not top-shift?)) :g3-fake-finish
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
        gates-pass? (nil? hard)
        rec (assoc cycle :committed? (boolean (and gates-pass? claimed-discharge?))
                         :recorded-at (str (java.time.Instant/now)))
        st  (-> (swap! !outings update-in [run-id :cycles] conj rec)
                (get run-id))
        cycles (:cycles st)
        n (count cycles)
        soft (cond
               hard nil
               (>= n (:max-cycles st))         :max-cycles
               (recent-no-progress? cycles)    :stuck-detector
               :else nil)
        halt (cond hard {:kind :hard :reason hard}
                   soft {:kind :soft :reason soft}
                   :else nil)]
    (when halt (swap! !outings assoc-in [run-id :status]
                      (if (= :hard (:kind halt)) :halted-hard :stopped)))
    {:continue? (nil? halt) :halt halt :n-done n}))

(defn finalize-outing!
  "Write the R-M run-summary artefact and return it."
  [run-id reason]
  (let [st (get @!outings run-id)
        cycles (:cycles st)
        discharges (filter :committed? cycles)
        regressions (filter #(false? (:g2-regression-ok? %)) cycles)
        nonconf     (filter #(false? (:g1-conforms? %)) cycles)
        sum-pe (reduce + 0.0 (keep :prediction-error cycles))
        summary {:run-id run-id :date (:date st) :posture (:posture st)
                 :end-reason reason :status (:status st)
                 :cycles-attempted (count cycles)
                 :cycles-committed (count discharges)
                 :discharged-sorries (mapv :sorry-id discharges)
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
