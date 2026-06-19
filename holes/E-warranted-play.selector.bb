#!/usr/bin/env bb
;; E-warranted-play — the STRATEGIC play-selector (the INSTANTIATE kit).
;;
;; Answers Joe's strategy-refinement (2026-06-08): the loop must not flit at random — it must
;; PERSIST on a chosen level. This tool answers *which half-baked pudding to persist on next*,
;; ranked by strategic value, and surfaces its short list. A KIT-BUILD: the tool runs = the witness
;; (C-pudding-prover §8 — a :build certifies a :kit/:capability). Read-only, deterministic.
;;
;; Strategy score: ON-ASCENT (the exploit binding — does it advance the goal-marking?) dominates;
;; then tractable (has a mission + not compute-gated); then generativity (claude-3's provenance prior).
;;
;; Run: bb E-warranted-play.selector.bb

(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.pprint :as pp])

(def home (System/getenv "HOME"))
(def graph (edn/read-string (slurp (str home "/code/futon0/holes/missions/M-capability-star-map.graph.edn"))))
(def caps (:capabilities graph))
(def gmissions (:missions graph))
(def goal :wm-overnight-unsupervised)

;; generativity index (claude-3's phylogeny) — descriptive provenance-credibility prior, optional
(def gen-index
  (try (:generativity-index (edn/read-string (slurp (str home "/code/futon6/data/mission-phylogeny.edn"))))
       (catch Exception _ {})))

;; the ascent = goal's transitive :scope (the place-set on a path to the goal-marking)
(def ascent
  (loop [fr [goal] seen #{}]
    (if-let [c (first fr)]
      (if (seen c) (recur (vec (rest fr)) seen)
        (recur (into (vec (rest fr)) (get-in caps [c :scope] [])) (conj seen c)))
      seen)))

(defn held? [c] (and (= :held (:status (caps c))) (not (:attested (caps c)))))

;; short list = missions that mint this capability (:minted-by ∪ graph-neighbour missions producing it)
(defn short-list [c]
  (vec (distinct (concat (get-in caps [c :minted-by] [])
                         (keep (fn [[mid m]] (when (some #{c} (:produces m)) mid)) gmissions)))))

(defn max-gen [missions] (reduce max 0 (keep #(get gen-index %) missions)))

(defn candidate [c]
  (let [sl (short-list c)
        on-asc (contains? ascent c)
        compute-gated (boolean (:compute-gated (caps c)))
        parked (get-in (caps c) [:operator-disposition :state])
        has-mission (boolean (seq sl))
        gen (max-gen sl)
        tractable (and has-mission (not compute-gated))
        score (+ (if on-asc 100 0) (if tractable 10 0) (* 0.1 gen) (if compute-gated -50 0))]
    {:cap c :score (double score) :on-ascent on-asc :short-list sl :generativity gen
     ;; an operator-set :operator-disposition (e.g. :parked) is a DECISION, not an
     ;; oversight — it overrides the no-mission NAG so the cap stops registering as
     ;; an unaddressed hole.
     :disposition (cond parked parked
                        compute-gated :needs-superpod
                        (not has-mission) :needs-operator-to-name-pre-witness
                        :else :playable)}))

(def ranked (->> (keys caps) (filter held?) (map candidate) (sort-by :score >) vec))

(println ";; E-warranted-play strategic play-selector —" (count ranked) "held capabilities ranked\n")
(doseq [{:keys [cap score on-ascent disposition short-list generativity]} ranked]
  (println (format "  %-30s %7.1f  %s  %-32s gen=%-3d  %s"
                   (name cap) score
                   (if on-ascent "ON-ASCENT" "  off    ")
                   (name disposition) (long generativity)
                   (str short-list))))

(def pick (first (filter #(= :playable (:disposition %)) ranked)))
(println "\n;; STRATEGIC PICK — the level to persist on next:")
(if pick
  (println "   " (name (:cap pick)) "  via short-list" (:short-list pick))
  (println "    (none playable — held caps are superpod-gated, awaiting a named pre-witness, or operator-parked ⇒ NAG)"))

(let [dispositioned (filter #(get-in caps [(:cap %) :operator-disposition]) ranked)]
  (when (seq dispositioned)
    (println "\n;; OPERATOR-DISPOSITIONED — decisions, NOT oversights (see :operator-disposition):")
    (doseq [{:keys [cap disposition]} dispositioned]
      (let [od (get-in caps [cap :operator-disposition])]
        (println (format "   %-30s %-18s %s" (name cap) (name disposition)
                         (or (:revisit od) (some-> (:gated-by od) name (->> (str "gated-by: "))) "")))))))
(System/exit (if pick 0 1))
