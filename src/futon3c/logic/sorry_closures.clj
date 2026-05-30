(ns futon3c.logic.sorry-closures
  "R-A.2 — the :sorry-closures-stick invariant: a sorry once discharged must
   stay discharged. A regression / false-closure is a sorry that carries
   discharge metadata (`:resolved-at`) yet is `:status :open` again.

   This is the in-process detector behind both callers (mission §8.2 / §10.4):
     - the per-cycle G2 gate (the harness calls `regression-violations`);
     - a live probe family on the Arxana surface (register `check` via
       `futon3c.logic.probe/register-family-check!` :sorry-closures-stick).

   The check reads the registry file directly (no cross-repo ns dependency),
   and is robust to the R-A.1 relocation: it prefers futon2/resources/sorrys.edn
   and falls back to futon2/data/sorrys.edn.

   Discipline (mission §8.2): re-opening a resolved sorry must NOT clear its
   `:resolved-*` fields — add `:reopened-at` and keep them — so this predicate
   fires."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private home (System/getProperty "user.home"))

(def registry-candidate-paths
  "Preferred → fallback. Survives the R-A.1 move (resources/ first)."
  [(str home "/code/futon2/resources/sorrys.edn")
   (str home "/code/futon2/data/sorrys.edn")])

(defn registry-path []
  (first (filter #(.exists (io/file ^String %)) registry-candidate-paths)))

(defn load-registry
  ([] (load-registry (registry-path)))
  ([path] (when path (edn/read-string (slurp path)))))

(defn regression-violations
  "Sorries that were discharged (`:resolved-at` present) yet are `:status :open`
   again. Returns a vector of {:id :resolved-at :reopened-at}."
  [registry-doc]
  (->> (:sorrys registry-doc)
       (filter (fn [s] (and (:resolved-at s) (= :open (:status s)))))
       (mapv #(select-keys % [:id :resolved-at :reopened-at]))))

(defn check
  "Probe check-fn shape: returns {:outcome :ok|:violation|:inactive :detail _}.
   :inactive when no registry file is found; :violation on any regression."
  [& _]
  (try
    (if-let [doc (load-registry)]
      (let [regs (regression-violations doc)]
        (if (empty? regs)
          {:outcome :ok      :detail {:checked-sorries (count (:sorrys doc))
                                      :path (registry-path)}}
          {:outcome :violation :detail {:regressions regs :count (count regs)
                                        :path (registry-path)}}))
      {:outcome :inactive :detail {:reason "no sorrys.edn found"
                                   :looked-in registry-candidate-paths}})
    (catch Throwable t
      {:outcome :violation
       :detail {:exception (str (.getName (class t)) ": " (.getMessage t))}})))

(defn register!
  "Register :sorry-closures-stick as a live probe family (R-A.2 dual-use).
   Deferred to INSTANTIATE wiring; offline VERIFY does not call this."
  []
  ((requiring-resolve 'futon3c.logic.probe/register-family-check!)
   :sorry-closures-stick check))
