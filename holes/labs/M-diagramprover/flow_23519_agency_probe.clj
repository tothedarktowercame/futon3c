;; Probe: the flow at claude-17's buffer line 23519, plus today's two 🈸:yes
;; turns, as Agency events checked by the Agency behaviour skeleton.
;; Run from futon3c:  clojure -M holes/labs/M-diagramprover/flow_23519_agency_probe.clj
;;
;; Reading: an operator turn is a :request bell to the agent, and the agent's
;; reply turn answers it. An agent's 🈸 paragraph is a :query bell to the
;; operator; the operator's answer is :answer. Joe's request in
;; *codex-repl:codex-10* (line 2947) is a :request to codex-10, whose narration
;; is published evidence that C3 reads.
;;
;; Two windows. "As spoken" rings every 🈸. "As recorded" rings only what the
;; offer route recorded: no agent posted an offer for its 🈸, so those query
;; bells were never rung, and the two 🈸:yes answers refer to bells the record
;; does not contain (the turn header's "agreement refused: no-visible-offer").
(require '[clojure.pprint :refer [pprint]]
         '[futon3c.diagramprover.graph :as graph]
         '[futon3c.diagramprover.skeleton.agency :as agency])

(defn turn [id from to & body]
  (concat [{:op :ring :id id :from from :to to :type :request}
           {:op :deliver :agent to :bell id}]
          body))

(defn ask [id from to] {:op :ring :id id :from from :to to :type :query})

(defn spoken
  "The window with every 🈸 rung. RING-ASK? decides whether an agent 🈸 is in it."
  [ring-ask?]
  (let [ask* (fn [id] (when (ring-ask? id) [(ask id :claude-17 :joe)
                                            {:op :deliver :agent :joe :bell id}]))]
    (remove nil?
            (concat
             ;; J1 -> C1 (C1 ends with 🈸 "Shall I build the corpus ...?")
             (turn :j1 :joe :claude-17)
             (ask* :c1-ask)
             [{:op :reply :agent :claude-17 :ref :j1}]
             ;; K: Joe asks codex-10 for a per-node narration, quoting C1
             (turn :k :joe :codex-10
                   {:op :publish :agent :codex-10 :evidence :narration}
                   {:op :reply :agent :codex-10 :ref :k})
             ;; J2 answers C1's ask ("No, let's try a different approach")
             (when (ring-ask? :c1-ask) [{:op :answer :agent :joe :ref :c1-ask}])
             (turn :j2 :joe :claude-17 {:op :reply :agent :claude-17 :ref :j2})
             ;; J3 -> C3 reads the narration; C3 ends with ㊭ "Shall I?", never answered
             (turn :j3 :joe :claude-17 {:op :read :agent :claude-17 :evidence :narration})
             (ask* :c3-ask)
             [{:op :reply :agent :claude-17 :ref :j3}]
             ;; Later today: two 🈸 asks, each answered by a bare 🈸:yes turn
             (ask* :heldout-ask)
             [{:op :answer :agent :joe :ref :heldout-ask}]
             (ask* :join-ask)
             [{:op :answer :agent :joe :ref :join-ask}]))))

(defn dangling-inputs [g]
  (->> (:inputs g)
       (map #(graph/vertex-data g %))
       (filter :dangling)
       (mapv #(select-keys % [:ref :vtype]))))

(defn report [events]
  (let [r (agency/check events)
        g (:diagram (agency/ingest events))]
    {:lawful? (agency/lawful? r)
     :findings (into {} (remove (comp empty? val))
                     (select-keys r [:signature :linearity :identity :concurrency
                                     :routing :unreadable :crossings]))
     :open-bells (:open-bells r)
     :answers-to-unrung-bells (dangling-inputs g)}))

(println "== as spoken (every 🈸 rung)")
(pprint (report (spoken (constantly true))))
(println "== as recorded (no agent 🈸 posted as an offer)")
(pprint (report (spoken (constantly false))))
