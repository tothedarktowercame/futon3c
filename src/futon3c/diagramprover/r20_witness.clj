(ns futon3c.diagramprover.r20-witness
  "Generate a checked R20 Lean witness from the pinned field-flow map."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [futon3c.diagramprover.wiring :as wiring]))

(def default-map "holes/labs/M-wm-wiring/r20-field-flow.edn")
(def default-edges "holes/labs/M-wm-wiring/r20-lean-edges.edn")
(def fixture-edges "test/futon3c/diagramprover/fixtures/r20-lean-edges.edn")

(defn read-edn [path] (edn/read-string (slurp path)))

(defn materialise
  "Copy every pinned source named by SPEC into a temporary root, without
  evaluating it."
  [spec]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "r20-witness" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [file (distinct (keep #(get-in % [:site :file]) (:boxes spec)))
            :let [[repo path] (str/split file #"/" 2)
                  {:keys [exit out err]} (sh/sh "git" "-C" (str "/home/joe/code/" repo)
                                               "show" (str (get (:repos spec) repo) ":" path))]]
      (when-not (zero? exit)
        (throw (ex-info "pinned source unavailable" {:repo repo :path path :error err})))
      (io/make-parents (io/file root file))
      (spit (io/file root file) out))
    (str root)))

(defn- comparable [finding]
  (cond-> finding (contains? finding :error) (-> (dissoc :error) (assoc :error? true))))

(defn report [root spec]
  (let [g (wiring/ingest spec)]
    (mapv comparable
          (concat (wiring/written-never-read g)
                  (wiring/read-never-written g)
                  (wiring/multiply-written g)
                  (wiring/conformance root spec {:heuristic? true})))))

(defn site [box]
  (str (get-in box [:site :file]) "#" (get-in box [:site :var])))

(defn- field-name [field] (str (wiring/vertex-field field)))
(defn- record-name [field] (str (or (wiring/vertex-record field) :unscoped)))
(defn- type-name [field-type] (subs (str field-type) 1))

(defn- missing-declaration?
  [findings box field role]
  (some #(and (= :declaration-without-occurrence (:finding %))
              (= (:box/id box) (:box/id %))
              (= (wiring/vertex-key field) (:field %))
              (= role (:role %)))
        findings))

(defn hops
  "Every writer→reader relation over one scoped field. A hop remains present
  when the checker did not find it; its :confirmed flag records that fact."
  [spec findings]
  (let [types (:r20/field-types spec)
        boxes (:boxes spec)]
    (vec
     (for [reader boxes
           field (:reads reader)
           writer boxes
           :when (some #(= (wiring/vertex-key field) (wiring/vertex-key %)) (:writes writer))]
       (let [vertex (wiring/vertex-key field)]
         {:writer (site writer)
          :field (field-name vertex)
          :record (record-name vertex)
          :field-type (type-name (get types field :wiring/field))
          :reader (site reader)
          :confirmed (and (not (missing-declaration? findings writer field :writes))
                          (not (missing-declaration? findings reader field :reads)))
          :sha (get-in spec [:repos "futon2"])})))))

(defn- distinct-sites? [chain]
  (let [sites (concat (map :writer chain) [(-> chain last :reader)])]
    (= (count sites) (count (set sites)))))

(defn- permitted-type? [edge index hop]
  (let [allowed (if (zero? index)
                  #{(:quantity edge)}
                  (conj (set (:carriers edge)) (:quantity edge)))]
    (contains? allowed (keyword (:field-type hop)))))

(defn chains-for-edge
  "All acyclic source→target chains for EDGE. Multiple chains are deliberately
  retained: Lean treats that ambiguity as a failed witness."
  [edge all-hops]
  (letfn [(extend [chain]
            (let [last-hop (peek chain)
                  nexts (filter (fn [candidate]
                                  (and (= (:reader last-hop) (:writer candidate))
                                       (not (contains? (set (concat (map :writer chain)
                                                                    [(:reader last-hop)]))
                                                       (:reader candidate)))))
                                all-hops)]
              (concat
               (when (contains? (set (:target-sites edge)) (:reader last-hop)) [chain])
               (mapcat #(extend (conj chain %)) nexts))))]
    (->> all-hops
         (filter #(contains? (set (:source-sites edge)) (:writer %)))
         (mapcat #(extend [%]))
         (filter (fn [chain]
                   (and (distinct-sites? chain)
                        (every? true? (map-indexed (fn [index hop]
                                                     (permitted-type? edge index hop))
                                                   chain)))))
         vec)))

(defn witness-edges [spec edge-spec findings]
  (let [all-hops (hops spec findings)]
    (mapv (fn [edge]
            {:edge (:edge edge) :hops (chains-for-edge edge all-hops)})
          (:edges edge-spec))))

(defn chain-counts [witness-edges]
  (into {} (map (juxt :edge #(count (:hops %))) witness-edges)))

(defn- lean-string [s] (pr-str s))

(defn- lean-hop [{:keys [writer field record field-type reader confirmed sha]}]
  (str "{ writer := " (lean-string writer)
       ", field := " (lean-string field)
       ", record := " (lean-string record)
       ", fieldType := " (lean-string field-type)
       ", reader := " (lean-string reader)
       ", confirmed := " (if confirmed "true" "false")
       ", sha := " (lean-string sha) " }"))

(defn- lean-edge [edge chain]
  (str "    ⟨." (name edge) ", ["
       (str/join ", " (map lean-hop chain)) "]⟩"))

(defn emit-lean [spec witness-edges]
  (str "import DarkTower.WarMachine.R20Witness\n\n"
       "namespace DarkTower.WarMachine.R20Witness.Generated\n"
       "open DarkTower.WarMachine.R20Witness\n"
       "open DarkTower.WarMachine.R20Graph\n\n"
       "def futon2Witness : Witness :=\n"
       "  { sha := " (lean-string (get-in spec [:repos "futon2"])) "\n"
       "    edges := [\n"
       (str/join ",\n" (mapcat (fn [{:keys [edge hops]}]
                                  (map #(lean-edge edge %) hops))
                                witness-edges))
       "] }\n\n"
       "#eval verdicts futon2Witness\n\n"
       "end DarkTower.WarMachine.R20Witness.Generated\n"))

(defn emit! [map-path edges-path output-path]
  (let [spec (read-edn map-path)
        edges (read-edn edges-path)
        findings (report (materialise spec) spec)
        witnesses (witness-edges spec edges findings)]
    (io/make-parents (io/file output-path))
    (spit output-path (emit-lean spec witnesses))
    {:chain-counts (chain-counts witnesses)
     :output output-path}))

(defn -main [& [map-path edges-path output-path]]
  (let [edges-path (or edges-path
                       (when (.exists (io/file default-edges)) default-edges)
                       fixture-edges)
        result (emit! (or map-path default-map) edges-path
                      (or output-path "holes/labs/M-wm-wiring/R20WitnessFuton2.lean"))]
    (println (pr-str result))
    (shutdown-agents)))
