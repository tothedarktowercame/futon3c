;; PROOF-2a clauses 1-6 on click-001's enactment (claude-10, 2026-09-24).
;; Run from /home/joe/code/futon3c:
;;   bb holes/labs/M-futon-seams/exemplar/clauses_1_6.clj
;; Reads click-001.edn (decision, prediction) and click-001-enactment.edn
;; (attempts, observations, declared rates) and writes
;; click-001-clauses.edn: A, D, F, the Q-link, B, and click-002's prediction
;; under the learned B. Everything is computed exactly over the finite state
;; space; nothing is sampled. Rates and priors marked :declared are inputs,
;; not measurements.
(require '[clojure.edn :as edn] '[clojure.set :as set] '[clojure.pprint :as pp])

(def here (str (.getParent (java.io.File. (System/getProperty "babashka.file")))))
(def click (edn/read-string (slurp (str here "/click-001.edn"))))
(def enact (edn/read-string (slurp (str here "/click-001-enactment.edn"))))
(def cand (get-in click [:decision :selection-certificate :candidate-derivations (:candidate enact)]))
(def pats (into {} (for [[k v] (:interpretations cand)] [k (select-keys v [:guard :produces])])))
(def above (mapv (fn [{:keys [above below]}] {:context above :pattern below})
                 (concat (:containment-order cand) (:co-application-edges cand))))
(def want (set (:want cand)))
(def s0 (set (:initial cand)))
(def T (get-in cand [:prediction :horizon]))

;; --- co-application kernel with a theta PER PATTERN (the list kernel's
;; single theta is the special case of equal thetas)
(defn enabled? [{:keys [guard produces]} s]
  (and (set/subset? (:needs guard) s) (empty? (set/intersection (:forbids guard) s))
       (not (set/subset? produces s))))
(def anc (let [parents (reduce (fn [m {:keys [context pattern]}] (update m pattern (fnil conj #{}) context)) {} above)]
           (into {} (for [i (keys pats)]
                      [i (loop [seen #{} fr (get parents i #{})]
                           (if (empty? fr) seen
                               (recur (into seen fr) (set/difference (reduce set/union #{} (map #(get parents % #{}) fr)) seen))))]))))
(defn frontier [s] (let [en (set (filter #(enabled? (pats %) s) (keys pats)))]
                     (set (remove #(seq (set/intersection (anc %) en)) en))))
(defn subsets [xs] (reduce (fn [acc x] (concat acc (map #(conj % x) acc))) [#{}] xs))
(defn step [theta s]
  (let [f (frontier s)]
    (if (empty? f) {s 1.0}
        (reduce (fn [m S] (update m (reduce set/union s (map #(:produces (pats %)) S))
                                  (fnil + 0.0)
                                  (* (reduce * 1.0 (map theta S)) (reduce * 1.0 (map #(- 1.0 (theta %)) (set/difference f S))))))
                {} (subsets f)))))
(defn rollout [theta]
  (loop [d {s0 1.0} t 0]
    (if (= t T) d
        (recur (reduce (fn [m [s w]] (merge-with + m (update-vals (step theta s) #(* w %)))) {} d) (inc t)))))
(defn want-marginal [d] (reduce (fn [m [s w]] (update m (set/intersection want s) (fnil + 0.0) w)) {} d))
(defn p-all [m] (get m want 0.0))

;; --- clause 1: A, observation likelihood for the recorded observations
(def rates (get-in enact [:observation-model :rates]))
(defn lik [obs wstate]   ; P(o | want-state), checks independent given the state
  (reduce * 1.0 (for [[tok {:keys [check-kind passed]}] obs
                      :let [{:keys [p-pass-if-true p-pass-if-false]} (rates check-kind)
                            p (if (contains? wstate tok) p-pass-if-true p-pass-if-false)]]
                  (if passed p (- 1.0 p)))))

;; --- clause 2: D, the exact posterior over want-states; clause 3: F
(def theta0 (constantly (get-in enact [:theta-prior :mean])))
(def prior (want-marginal (rollout theta0)))          ; predicted, before acting
(def obs (:observations enact))
(def p-o (reduce + (for [[ws w] prior] (* w (lik obs ws)))))
(def posterior (into {} (for [[ws w] prior] [ws (/ (* w (lik obs ws)) p-o)])))
(def F (- (Math/log p-o)))  ; exact posterior: F = -log P(o)

;; --- clause 5: B, Beta update of each pattern's theta from the attempts
(def n0 (get-in enact [:theta-prior :pseudo-count]))
(def m0 (get-in enact [:theta-prior :mean]))
(def trials (reduce (fn [m {:keys [pattern success]}]
                      (-> m (update-in [pattern :attempts] (fnil inc 0))
                          (update-in [pattern :successes] (fnil + 0) (if success 1 0))))
                    {} (:attempts enact)))
(def theta1 (into {} (for [p (keys pats)
                           :let [{:keys [attempts successes] :or {attempts 0 successes 0}} (trials p)
                                 a (+ (* m0 n0) successes) b (+ (* (- 1 m0) n0) (- attempts successes))]]
                       [p {:alpha a :beta b :mean (/ a (+ a b)) :attempts attempts :successes successes}])))
(def click2 (want-marginal (rollout #(get-in theta1 [% :mean]))))

;; --- clause 4 in the enactment's own unit: ATTEMPTS until all wants hold.
;; A kernel step under co-application attempts every frontier pattern once, so
;; it costs |frontier| attempts; under interleaving it costs one. The result is
;; the exact distribution of the number of attempts to completion (capped),
;; with :never for mass that stalls (empty frontier, wants unmet). No horizon
;; is chosen: the enactment's observed count is scored against it.
(def attempt-cap 40)
(defn attempts-dist [kind theta]
  (loop [live {[s0 0] 1.0} done {}]
    (if (empty? live) done
        (let [step1 (for [[[s a] w] live
                          :let [f (frontier s)]]
                      (cond
                        (set/subset? want s) [:done a w]
                        (empty? f) [:done :never w]
                        (>= a attempt-cap) [:done :over-cap w]
                        (= kind :coapp)
                        [:live (for [[s2 p] (step theta s)] [[s2 (+ a (count f))] (* w p)])]
                        :else ; interleaving: one frontier pattern, uniformly
                        [:live (for [q f [s2 p] {(set/union s (:produces (pats q))) (theta q) s (- 1.0 (theta q))}]
                                 [[s2 (inc a)] (/ (* w p) (count f))])]))]
          (recur (reduce (fn [m [tag x]] (if (= tag :live) (reduce (fn [m [k w]] (update m k (fnil + 0.0) w)) m x) m)) {} step1)
                 (reduce (fn [m [tag a w]] (if (= tag :done) (update m a (fnil + 0.0) w) m)) done step1))))))
(defn dist-summary [d observed]
  (let [ks (filter number? (keys d))
        mean (/ (reduce + (map #(* % (d %)) ks)) (max 1e-12 (reduce + (map d ks))))
        p-obs (get d observed 0.0)]
    {:p-exactly-observed p-obs
     :p-at-most-observed (reduce + (for [k ks :when (<= k observed)] (d k)))
     :mean-attempts-if-completes mean
     :p-never (get d :never 0.0) :p-over-cap (get d :over-cap 0.0)
     :log-score-bits (when (pos? p-obs) (/ (Math/log p-obs) (Math/log 2)))}))
(def observed-attempts (count (:attempts enact)))
(def sensitivity
  (for [n [2 5 20]]
    (let [th (into {} (for [p (keys pats)
                            :let [{:keys [attempts successes] :or {attempts 0 successes 0}} (trials p)
                                  a (+ (* m0 n) successes) b (+ (* (- 1 m0) n) (- attempts successes))]]
                        [p (/ a (+ a b))]))]
      {:pseudo-count n :theta-grain-step (th :cascade-construction/choose-the-grain-where-state-lives)
       :theta-others (th :or3/count-every-card-back)
       :click-002-p-all-wants-at-horizon (p-all (want-marginal (rollout th)))
       :attempts-under-learned (dist-summary (attempts-dist :coapp th) observed-attempts)})))

(defn r [x] (/ (Math/round (* 1e6 (double x))) 1e6))
(defn fmt [m] (into (sorted-map-by #(compare (str %1) (str %2))) (for [[k v] m] [(vec (sort k)) (r v)])))

(def out
  {:schema :m-futon-seams/proof2a-clauses-v1
   :computed-by "bb holes/labs/M-futon-seams/exemplar/clauses_1_6.clj"
   :inputs {:click "click-001.edn" :enactment "click-001-enactment.edn" :candidate (:candidate enact)
            :kernel :coapp :horizon T}
   :clause-1-A {:status (get-in enact [:observation-model :status]) :rates rates
                :observations obs
                :W1-measured-rates {:met false :reason (get-in enact [:observation-model :reason])}}
   :clause-2-D {:prior-over-want-states (fmt prior)
                :likelihood-of-observations (fmt (into {} (for [ws (keys prior)] [ws (lik obs ws)])))
                :posterior (fmt posterior)
                :posterior-all-wants-met (r (get posterior want 0.0))
                :not-a-copy-of-facts (not= 1.0 (r (get posterior want 0.0)))}
   :clause-3-F {:P-o (r p-o) :F (r F)
                :statement "q is the exact update, so F = -log P(o) (ExactBeliefTrajectory.exactUpdate_minimises_vfe)"}
   :clause-4-Q {:predicted {:p-all-wants (r (p-all prior)) :source "rollout at theta 0.8, as click-001 recorded"}
                :recorded-in-click (get-in cand [:prediction :p-all-wants])
                :prediction-reproduced? (< (Math/abs (- (p-all prior) (get-in cand [:prediction :p-all-wants]))) 1e-3)
                :realised {:observations obs}
                :surprise-bits (r (/ F (Math/log 2)))
                :link {:action (:candidate enact) :outcome "click-001-enactment.edn :observations" :next-belief ":clause-2-D :posterior"}}
   :clause-4-attempts
   {:unit :attempts
    :observed observed-attempts
    :statement "prediction and outcome in one unit: attempts to completion, not probability at a chosen horizon"
    :coapp-theta-0.8 (update-vals (dist-summary (attempts-dist :coapp theta0) observed-attempts) #(if (number? %) (r %) %))
    :interleaving-theta-0.8 (update-vals (dist-summary (attempts-dist :inter theta0) observed-attempts) #(if (number? %) (r %) %))}
   :clause-5-B {:prior {:mean m0 :pseudo-count n0 :status :declared}
                :per-pattern (into (sorted-map-by #(compare (str %1) (str %2)))
                                   (for [[p v] theta1] [p (-> v (update :mean r) (update :alpha r) (update :beta r))]))
                :click-002-prediction {:p-all-wants (r (p-all click2)) :horizon T :kernel :coapp
                                       :statement "the next click reads the updated thetas before predicting"}
                :changed-the-prediction? (> (Math/abs (- (p-all click2) (p-all prior))) 1e-6)
                :sensitivity-to-declared-prior
                (vec (for [x sensitivity]
                       (-> x (update :theta-grain-step r) (update :theta-others r)
                           (update :click-002-p-all-wants-at-horizon r)
                           (update :attempts-under-learned (fn [m] (update-vals m #(if (number? %) (r %) %)))))))}
   :clause-6 {:E {:status :not-measured} :C {:status :declared :value (vec (sort want))}
              :A {:status :declared-rates} :D {:status :computed} :F {:status :computed}
              :Q {:status :linked} :B {:status :computed-from-declared-prior}}})

(spit (str here "/click-001-clauses.edn") (with-out-str (pp/pprint out)))
(pp/pprint (select-keys out [:clause-4-attempts]))
(pp/pprint (get-in out [:clause-5-B :sensitivity-to-declared-prior]))
