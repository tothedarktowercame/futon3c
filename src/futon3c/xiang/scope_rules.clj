(ns futon3c.xiang.scope-rules
  "Classical scope links over the settled 象 act stream.

  These rules are deliberately structural: no model call and no persistence.
  The opener set and close relation remain owned by futon3c.logic.xiang."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.logic.xiang :as lx]
            [futon3c.xiang.turn-acts :as turn-acts]))

(def version "2026-10-15.2")

(def ^:private framing-kinds
  #{:constrain :propose :ask-action :prioritize :redirect})

(defn- reply-index [id]
  (some-> (re-find #"-r-(\d+)$" (str id)) second Long/parseLong))

(defn- fragment? [act]
  (str/includes? (str (:id act)) "-f-"))

(defn- reply? [act]
  (some? (reply-index (:id act))))

(defn- same-turn? [a b]
  (= (:turn a) (:turn b)))

(defn- edge [act opener rule]
  {:act (:id act)
   :within (:id opener)
   :rule rule
   :opener-kind (:kind opener)})

(defn within-edges
  "Apply the mined R3/R5/R4/R1 precedence to ACTS in stream order.

  R3 is the nearest earlier kernel opener in the same turn. R5 is a non-first
  reply paragraph governed by that turn's first reply paragraph. R4 is the
  nearest earlier framing fragment in the same turn. R1 is the first fragment
  of the act's turn. All four look inside the act's own turn, as the mined
  rules did (within_rules.py searches the record's own act order; 95.8% of
  labelled targets are in the same record): a cross-turn opener never
  governs, so the first act of a turn has no edge rather than a self-edge."
  [acts]
  (let [acts (vec acts)]
    (vec
     (keep identity
           (map-indexed
            (fn [idx act]
              (let [earlier (filterv #(same-turn? % act) (subvec acts 0 idx))
                    opener (last (filter #(lx/port-act? %) earlier))
                    first-reply (when (reply? act)
                                  (->> earlier
                                       (filter #(and (same-turn? % act) (reply? %)))
                                       (sort-by reply-index)
                                       first))
                    framing (last (filter #(and (= "operator" (:author %))
                                                (contains? framing-kinds (:kind %)))
                                          earlier))
                    first-fragment (first (filter #(and (same-turn? % act)
                                                       (fragment? %)) acts))]
                (cond
                  opener (edge act opener "R3")
                  (and first-reply (pos? (long (reply-index (:id act)))))
                  (edge act first-reply "R5")
                  framing (edge act framing "R4")
                  (and first-fragment (not= (:id first-fragment) (:id act)))
                  (edge act first-fragment "R1")
                  :else nil)))
            acts)))))

(defn- close-pairs
  "The kernel's per-turn close result joined to explicit/inferred answer ids.
   One ports-context serves every turn: the inferred pairs are the same for
   all of them, and the closed set needs only the two open-port queries per
   turn, not the obligations, in-force and promulgated queries turn-ports
   also answers."
  [acts]
  (let [ctx (turn-acts/ports-context acts)
        by-id (:by-id ctx)
        explicit (lx/explicit-answers acts)
        inferred (map (juxt :answer :opener) (:inferred ctx))
        pairs (distinct (concat explicit inferred))
        closed (into #{} (mapcat (fn [turn]
                                   (let [{:keys [before after]} (turn-acts/turn-closure ctx turn)]
                                     (set/difference before after))))
                     (distinct (map :turn acts)))]
    (for [[answer opener] pairs
          :when (and (contains? closed opener)
                     (get by-id answer) (get by-id opener)
                     (lx/port-act? (get by-id opener)))]
      [(get by-id answer) (get by-id opener)])))

(def ^:private opens-cache
  "OPEN edges by the act stream they were computed from, a few sessions
   deep. close-pairs still asks the kernel two open-port questions per turn
   (about 0.7 s for a 68-turn session on 2026-10-15), and every request for
   a session between two new turns asks over the same acts: the ledger's
   cold build asks once per turn, the Emacs overlay once per refresh. The
   stream is compared by value, so a changed act is a miss."
  (atom {}))

(defn opens-edges
  "Return classical close-to-opener edges for ports the kernel says closed."
  [acts]
  (let [acts (vec acts)
        k (hash acts)
        hit (get @opens-cache k)]
    (if (and hit (= (:acts hit) acts))
      (:edges hit)
      (let [edges (mapv (fn [[act opener]] (edge act opener "OPEN")) (close-pairs acts))]
        (swap! opens-cache (fn [cache]
                             (assoc (if (>= (count cache) 8) {} cache)
                                    k {:acts acts :edges edges})))
        edges))))

(defn scope-for-turn
  "Edges displayed for TURN-ID: that turn's acts plus the previous reply."
  [acts turn-id]
  (let [turns (vec (distinct (map :turn acts)))
        idx (.indexOf turns turn-id)
        previous (when (pos? idx) (nth turns (dec idx)))
        allowed (set (concat (for [a acts :when (= turn-id (:turn a))] (:id a))
                             (for [a acts :when (and (= previous (:turn a))
                                                     (reply? a))] (:id a))))]
    (when (neg? idx)
      (throw (ex-info (str "Unknown turn " turn-id)
                      {:reason :unknown-turn :turn turn-id})))
    (vec (filter #(contains? allowed (:act %))
                 (concat (within-edges acts) (opens-edges acts))))))

(defn acts-for-turn
  "The act projection displayed with SCOPE-FOR-TURN's edges."
  [acts turn-id]
  (let [turns (vec (distinct (map :turn acts)))
        idx (.indexOf turns turn-id)]
    (when (neg? idx)
      (throw (ex-info (str "Unknown turn " turn-id)
                      {:reason :unknown-turn :turn turn-id})))
    (let [previous (when (pos? idx) (nth turns (dec idx)))]
      (vec (filter #(or (= turn-id (:turn %))
                        (and (= previous (:turn %)) (reply? %))) acts)))))
