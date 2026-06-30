#!/usr/bin/env bb
;; E-held-work-ledger POC (C-cascade-real, held/deferred-work facet, standards 2/3).
;; Harvests the structured held-work registries — today five incompatible shapes —
;; into ONE typed :held/* ledger (schema named by the cascade prose), and surfaces
;; the governance gaps (items with no owner / wake-trigger / re-entry = the
;; ungoverned folklore that rots invisibly). Emits held-work-ledger.edn.
;;
;; v0: structured registries only (sorrys + a generic registry tree-scan over the
;; star-graph + pudding registries). Prose harvest, gate-queue, substrate-2 landing,
;; and the real wake-trigger evaluator are the named follow-ons (see the excursion).
(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.walk :as walk]
         '[clojure.pprint :as pp])

(def root "/home/joe/code")
(def out (str root "/futon3c/holes/excursions/held-work-ledger.edn"))

(defn slurp-edn [rel]
  (try (edn/read-string (slurp (str root "/" rel))) (catch Exception _ nil)))

(defn first-line
  "First non-blank line of a value, coercing non-strings (some :rationale fields
   are lists/forms) so the harvester is robust across registry shapes."
  [x]
  (when x
    (-> (if (string? x) x (pr-str x)) str/split-lines first str/trim not-empty)))

;; a status counts as "held" (unresolved, awaiting something) across the registries' vocabularies
(def held-statuses #{:held :open :acknowledged :acknowledged-v1-in-force :gated :deferred :active})

(defn kind->wake-trigger
  "Best-effort wake condition derived from a sorry's :kind (the only structured
   hint the source registries carry). nil where the source names none — which is
   exactly the governance gap the ledger surfaces."
  [kind]
  (case kind
    :external-dependency "external work lands"
    :decision-debt       "a decision is taken"
    :prototyping-forward "a refinement pass is run"
    :technical-debt      "the debt is scheduled + paid"
    :proof-state         "a witness/pudding is produced"
    :kit                 "the apparatus is built"
    nil))

(defn held-item
  "Normalise into the cascade-named :held/* typed object."
  [{:keys [id reason owner kind wake-trigger evidence-condition review-by re-entry source missions raised-at]}]
  {:held/id id
   :held/reason reason
   :held/owner owner
   :held/status :held
   :held/kind kind
   :held/wake-trigger wake-trigger
   :held/evidence-condition evidence-condition
   :held/review-by review-by
   :held/re-entry re-entry
   :held/source source
   :held/missions (vec missions)
   :held/raised-at raised-at})

;; --- typed adapter: sorrys.edn (the richest source shape) -------------------
(defn from-sorrys []
  (let [doc "futon2/resources/sorrys.edn"]
    (->> (:sorrys (slurp-edn doc))
         (filter #(contains? held-statuses (:status %)))
         (mapv (fn [s]
                 (held-item {:id (keyword "sorry" (name (:id s)))
                             :reason (:title s)
                             :owner (:resolved-by-pilot s) ; sorrys name a pilot, not an owner -> usually nil
                             :kind (:kind s)
                             :wake-trigger (kind->wake-trigger (:kind s))
                             :evidence-condition (first-line (:rationale s))
                             :review-by nil
                             :re-entry (first (:links s))
                             :missions (:related-missions s)
                             :raised-at (:raised-at s)
                             :source {:registry :sorrys :ref (:id s) :doc doc}}))))))

;; --- generic adapter: any registry EDN, tree-scanned for held-shaped maps ----
(defn held-shaped? [m]
  (and (map? m) (:title m) (contains? held-statuses (:status m))))

(defn from-registry-scan [doc registry]
  (let [hits (atom [])]
    (walk/postwalk (fn [x] (when (held-shaped? x) (swap! hits conj x)) x) (slurp-edn doc))
    (mapv (fn [m]
            ;; keyword names can't start with a digit, so prefix the title-hash with 'h'
            (held-item {:id (keyword (name registry) (str "h" (Math/abs (hash (:title m)))))
                        :reason (:title m)
                        :owner nil
                        :kind (or (:kind m) (:sorry/kind m))
                        :wake-trigger nil ; these registries name no wake condition -> a gap
                        :evidence-condition (first-line (or (:rationale m) (:grounding m) (:discharge m)))
                        :review-by nil
                        :re-entry (or (:grounding m) (first (:links m)))
                        :missions []
                        :raised-at (:raised-at m)
                        :source {:registry registry :doc doc}}))
          @hits)))

(def ledger
  (vec (concat
         (from-sorrys)
         (from-registry-scan "futon0/holes/missions/M-capability-star-map.graph.edn" :star)
         (from-registry-scan "futon7/holes/pudding-prover-registry.edn" :pudding))))

;; --- governance-gap report (the POC's point: ungoverned folklore made visible)
(defn gap? [k i] (let [v (get i k)] (or (nil? v) (and (coll? v) (empty? v)))))
(def gaps
  {:no-owner        (count (filter #(gap? :held/owner %) ledger))
   :no-wake-trigger (count (filter #(gap? :held/wake-trigger %) ledger))
   :no-re-entry     (count (filter #(gap? :held/re-entry %) ledger))
   :no-mission-link (count (filter #(gap? :held/missions %) ledger))})

(def by-source (frequencies (map #(get-in % [:held/source :registry]) ledger)))

(spit out (with-out-str (pp/pprint {:meta {:facet :held-deferred-work-ledger
                                           :campaign "C-cascade-real"
                                           :v 0 :sources (keys by-source)}
                                    :count (count ledger)
                                    :by-source by-source
                                    :governance-gaps gaps
                                    :items ledger})))

(println "=== held/deferred-work ledger (v0 — structured registries) ===")
(println (format "harvested %d held items across %d registries:" (count ledger) (count by-source)))
(doseq [[reg n] (sort-by val > by-source)] (println (format "  %-9s %d" (name reg) n)))
(println "\ngovernance gaps (the ungoverned folklore — what the ledger exists to surface):")
(doseq [[k n] gaps] (println (format "  %-16s %d / %d items" (name k) n (count ledger))))
(println "\nwrote" out)
