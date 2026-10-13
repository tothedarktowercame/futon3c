(ns futon3c.diagramprover.causal.open-theory-export
  "Public, deterministic interchange fixture for open-theory/glue.

  The artifact records exactly what glue establishes: a validated union DAG
  containing the two theories' arrows and structural equations.  There is no
  independent sequential-composition operation in open-theory, so the export
  explicitly refuses an equality-with-sequential-composition claim."
  (:require [cheshire.core :as json]
            [futon3c.diagramprover.causal.open-theory :as ot]))

(def producer
  (ot/theory :producer {"shared" "source"}
             :inputs ["source"] :interface [:shared]))

(def consumer
  (ot/theory :consumer {"result" "shared"}
             :inputs ["shared"] :interface [:shared]))

(defn- names [xs]
  (mapv name (sort xs)))

(defn- arrow-value [{:keys [from to]}]
  (sorted-map "from" (name from) "to" (name to)))

(defn- theory-value [{:keys [id variables arrows mechanisms interface]}]
  (sorted-map
   "arrows" (mapv arrow-value arrows)
   "id" (name id)
   "interface" (names interface)
   "mechanisms" (into (sorted-map) (map (fn [[k v]] [(name k) v])) mechanisms)
   "variables" (names variables)))

(defn- refusal-value [{:keys [glued? reason details]}]
  (sorted-map
   "details" (into (sorted-map)
                   (map (fn [[k ids]] [(name k) (mapv name ids)])) details)
   "glued" glued?
   "reason" (name reason)))

(defn export-value
  "Call open-theory/glue and project its result into stable, JSON-safe data."
  []
  (let [{:keys [glued? dag owners inputs] :as result}
        (ot/glue [producer consumer])
        undeclared (ot/glue [producer (assoc consumer :interface #{})])]
    (when-not glued?
      (throw (ex-info "public open-theory fixture did not glue" result)))
    (sorted-map
     "claim" (sorted-map
              "kind" "validated-union-dag"
              "sequential-composition-equality" "not-defined-by-this-implementation")
     "glue" (sorted-map
              "arrows" (mapv arrow-value (:arrows dag))
              "glued" true
              "inputs" (names inputs)
              "mechanisms" (into (sorted-map)
                                 (map (fn [[k v]] [(name k) v]))
                                 (get-in dag [:metadata :structural_equations]))
              "owners" (into (sorted-map)
                             (map (fn [[k v]] [(name k) (name v)])) owners)
              "variables" (names (keys (:variables dag))))
     "schema" "futon3c.open-theory-glue/v1"
     "theories" (mapv theory-value [producer consumer])
     "without-declared-interface" (refusal-value undeclared))))

(defn export-json
  "Return the canonical one-line JSON artifact, including its final newline."
  []
  (str (json/generate-string (export-value)) "\n"))
