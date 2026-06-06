(ns futon3c.wm.outing
  "Per-cycle gate runner for WM outings.

   Composes the existing G1/G2/G3 verifiers into one verdict. This namespace
   does not commit, stage, tick, or mutate outing state; the /loop body owns
   those effects."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.logic.sorry-closures :as sorry-closures]))

(def repl-spec-verify-path "scripts/repl_spec_verify.clj")

(defn- load-frame [cycle-result]
  (cond
    (:frame cycle-result) (:frame cycle-result)
    (:frame-path cycle-result) (edn/read-string (slurp (:frame-path cycle-result)))
    :else nil))

(defn- frame->trace [frame]
  (cond
    (map? frame) (or (:trace frame) (:gamma frame) [])
    (vector? frame) frame
    :else []))

(defn- repl-spec-verify-fn []
  (or (try
        (requiring-resolve 'scripts.repl-spec-verify/verify)
        (catch Throwable _ nil))
      (do
        (load-file (.getPath (io/file repl-spec-verify-path)))
        (requiring-resolve 'scripts.repl-spec-verify/verify))))

(defn- run-g1 [cycle-result verify-fn]
  (let [frame (load-frame cycle-result)
        report (verify-fn (frame->trace frame) (when (map? frame) frame))
        pass? (true? (:conforms? report))]
    {:pass? pass?
     :conforms? pass?
     :report report}))

(defn- run-g2 [sorry-check-fn]
  (let [result (sorry-check-fn)
        pass? (= :ok (:outcome result))]
    {:pass? pass?
     :outcome (:outcome result)
     :detail (:detail result)}))

(defn- run-g3 [{:keys [claimed-discharge? top-shift?]}]
  (let [fake-finish? (boolean (and claimed-discharge? (not top-shift?)))]
    {:pass? (not fake-finish?)
     :claimed-discharge? (boolean claimed-discharge?)
     :top-shift? (boolean top-shift?)
     :fake-finish? fake-finish?}))

(defn- gate-error [gate throwable]
  {:pass? false
   :error? true
   :exception (str (.getName (class throwable)) ": " (.getMessage throwable))
   :gate gate})

(defn- safe-gate [gate f]
  (try
    (f)
    (catch Throwable t
      (gate-error gate t))))

(defn- reason-for [gate result]
  (cond
    (and (= gate :g3) (:fake-finish? result)) :g3-fake-finish
    (:error? result) (keyword (str (name gate) "-error"))
    (true? (:pass? result)) nil
    (= gate :g1) :g1-non-conformant
    (= gate :g2) :g2-regression
    (= gate :g3) :g3-failed
    :else (keyword (str (name gate) "-failed"))))

(defn- verdict [gates]
  (cond
    (get-in gates [:g3 :fake-finish?]) :hard-halt
    (every? :pass? (vals gates)) :pass
    :else :quarantine))

(defn run-cycle-gates!
  "Run G1/G2/G3 for one closed WM cycle.

   Input shape:
     {:frame-path ... or :frame ...
      :top-shift? boolean
      :claimed-discharge? boolean
      :run-id string}

   Returns:
     {:verdict :pass|:quarantine|:hard-halt
      :gates {:g1 ... :g2 ... :g3 ...}
      :reasons [...]}"
  ([cycle-result] (run-cycle-gates! cycle-result {}))
  ([cycle-result {:keys [verify-fn sorry-check-fn]
                  :or {sorry-check-fn sorry-closures/check}}]
   (let [verify-fn (or verify-fn (repl-spec-verify-fn))
         gates {:g1 (safe-gate :g1 #(run-g1 cycle-result verify-fn))
                :g2 (safe-gate :g2 #(run-g2 sorry-check-fn))
                :g3 (safe-gate :g3 #(run-g3 cycle-result))}
         reasons (->> gates
                      (keep (fn [[gate result]] (reason-for gate result)))
                      vec)]
     {:verdict (verdict gates)
      :gates gates
      :reasons reasons
      :run-id (:run-id cycle-result)})))
