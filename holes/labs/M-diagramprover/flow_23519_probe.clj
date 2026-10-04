;; Probe: a six-turn operator/agent flow read as glued open causal theories.
;; Run from futon3c:  clojure -M holes/labs/M-diagramprover/flow_23519_probe.clj
;;
;; The flow is claude-17's buffer from line 23519 (2026-10-02), recorded in
;; futon0/analysis/audits/flow-23519/. Operator turns J1-J3 carry 象's intent
;; readings; agent turns C1-C3 carry their reply-proforma marks (🖖 read as 🈖).
;; Two acts outside this buffer are included because the turns cite them:
;; K, Joe's request in *codex-repl:codex-10* at line 2947, which quotes C1's
;; finding; and N, codex-10's narration from line 2979, which J3 points to.
;;
;; Each act is one open theory. Its exogenous apply-X says the act happened;
;; each variable it owns holds when the act happened and everything the act
;; used holds. "Used" is read off the turn's text: what it quotes, answers or
;; points to. The edges are hand-read from the record, not mined.
(require '[clojure.pprint :refer [pprint]]
         '[futon3c.diagramprover.causal.dag :as dag]
         '[futon3c.diagramprover.causal.dsep :as dsep]
         '[futon3c.diagramprover.causal.open-theory :as ot]
         '[futon3c.diagramprover.causal.scm :as scm])

(defn chain-and
  "Mechanisms making X the conjunction of TERMS, in the binary scm grammar."
  [x terms]
  (let [terms (mapv name terms)]
    (if (= 1 (count terms))
      {(name x) (first terms)}
      (loop [acc (first terms) [t & more] (rest terms) i 1 mechs {}]
        (if (empty? more)
          (assoc mechs (name x) (str acc " and " t))
          (let [h (str (name x) "__" i)]
            (recur h more (inc i) (assoc mechs h (str acc " and " t)))))))))

(defn apply-var [act] (keyword (str "apply-" (name act))))

(defn act
  "An act owning OWNS, a map var -> vars it used (besides having happened)."
  [id owns]
  (let [ext (into #{} (comp (mapcat val) (remove (set (keys owns)))) owns)]
    (ot/theory id
               (into {} (map (fn [[v used]] (chain-and v (into [(apply-var id)] used)))) owns)
               :inputs (map name (conj ext (apply-var id)))
               :interface (into (vec (keys owns)) ext))))

(def acts
  [;; J1 (象: explain, propose) "If we can find those transcripts ..."
   (act :j1 {:transcripts-idea []})
   ;; C1 (㊥ ㊢ 🈖 ㊟ 🈸) only R16, R9, R7, R17 have speakers; corpus plan; ask
   (act :c1 {:four-nodes-finding [:transcripts-idea]
             :corpus-plan [:four-nodes-finding]
             :corpus-ask [:corpus-plan]})
   ;; J2 (象: redirect, report) "No, let's try a different approach"; 象's
   ;; rationale: the "No" answers C1's finding
   (act :j2 {:redirect [:four-nodes-finding]})
   ;; K, codex-10 buffer line 2947: "Only 4 of the 23 nodes have agent transcripts"
   (act :k {:narration-request [:four-nodes-finding]})
   ;; N, codex-10's per-node narration of a stepped click, line 2979
   (act :n {:narration [:narration-request]})
   ;; C2 (㊥ 🈖 ㊟) three steps for using the narration: check, take words, rerun
   (act :c2 {:narration-plan [:redirect]})
   ;; J3 (象: prioritize, report) "places where there *isn't* good alignment ... line 2979"
   (act :j3 {:misalignment-priority [:narration]})
   ;; C3 (㊥ ㊢ 🈖 ㊟ ㊭) six-node table against the R-node tree; propose
   ;; machine-side cues and a detection rerun (C2's steps 2 and 3)
   (act :c3 {:six-node-table [:narration :misalignment-priority :tree-cues]
             :machine-cues-proposal [:six-node-table :narration-plan]})])

(def act-ids [:j1 :c1 :j2 :k :n :c2 :j3 :c3])
(def products [:transcripts-idea :four-nodes-finding :corpus-plan :corpus-ask
               :redirect :narration-request :narration :narration-plan
               :misalignment-priority :six-node-table :machine-cues-proposal])

(def glued (ot/glue acts))
(def d (:dag glued))

(def happened
  (merge (zipmap (map apply-var act-ids) (repeat true))
         {:tree-cues true}
         (zipmap products (repeat true))))

(defn knockout [x]
  (vec (for [p products
             :let [r (scm/counterfactual d {:evidence happened
                                            :intervention {(apply-var x) false}
                                            :outcome p})]
             :when (false? (:answer r))]
         p)))

(defn acts-behind [v]
  (into (sorted-set)
        (keep #(when (= "apply" (first (clojure.string/split (name %) #"-" 2)))
                 (keyword (subs (name %) 6))))
        (dag/ancestors d v)))

(println "== glue")
(pprint (select-keys glued [:glued? :reason :details :inputs]))
(println "== which acts each result depends on")
(pprint (into (sorted-map) (for [v [:corpus-ask :narration-plan :six-node-table :machine-cues-proposal]]
                             [v (acts-behind v)])))
(println "== knockout: had act X not happened, which products are missing")
(pprint (into (sorted-map) (for [x act-ids] [x (knockout x)])))
(println "== dead ends: products nothing later used")
(pprint (vec (for [p products :when (empty? (filter (set products) (dag/descendants d p)))] p)))
(println "== implied independencies among products (conditioning <= 1)")
(let [ps (set products)
      ci (filter #(and (ps (:x %)) (ps (:y %)))
                 (dsep/implied-independencies d {:max-conditioning 1}))]
  (pprint {:count (count ci) :sample (vec (take 8 ci))}))
