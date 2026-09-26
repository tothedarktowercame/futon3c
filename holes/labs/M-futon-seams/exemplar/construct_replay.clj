;; Replay the War Machine's own constructor on click-001's recorded
;; interpretations (PROOF-2a W_0, construction condition). claude-10, 2026-09-24.
;; Run from /home/joe/code/futon2 (the constructor's namespace):
;;   clojure -M /home/joe/code/futon3c/holes/labs/M-futon-seams/exemplar/construct_replay.clj
;; For each hand-built candidate: feed its interpretations, receipts and the
;; recorded initial state to futon2.aif.interpretation-construction/construct
;; and compare what comes back with the candidate: pattern set, and whether
;; the machine's precedence is a linear extension of the recorded containment
;; order. Writes construct-replay.edn next to this file.
(require '[futon2.aif.interpretation-construction :as ctor]
         '[clojure.edn :as edn] '[clojure.set :as set] '[clojure.pprint :as pp] '[clojure.string :as str] '[clojure.java.shell :as sh])
(def here "/home/joe/code/futon3c/holes/labs/M-futon-seams/exemplar")
(def click (edn/read-string (slurp (str here "/click-001.edn"))))
(def cands (get-in click [:decision :selection-certificate :candidate-derivations]))

(defn linear-extension? [prec edges]
  (let [pos (zipmap prec (range))]
    (every? (fn [{:keys [above below]}]
              (or (not (contains? pos above)) (not (contains? pos below)) (< (pos above) (pos below))))
            edges)))

(defn replay [cid c]
  (let [interp (into {} (for [[k v] (:interpretations c)] [k (select-keys v [:guard :produces])]))
        receipts (into {} (for [[k v] (:interpretations c)] [k (or (:receipt v) {})]))
        tokens (set/union (set (:want c))
                          (reduce set/union #{} (map (fn [p] (set/union (:produces p) (get-in p [:guard :needs]) (get-in p [:guard :forbids]))) (vals interp))))
        observation (into {} (for [t tokens] [t (contains? (set (:initial c)) t)]))
        edges (concat (:containment-order c) (:co-application-edges c))
        result (ctor/construct {:target (str cid) :want (vec (:want c)) :observation observation
                                :interpretations interp :interpretation-receipts receipts
                                :horizon 12 :move-cost 1 :budget {:max-moves 4 :max-expansions 20000}
                                ;; G: empty family worst, then fewer patterns better (the I2 pin)
                                :evaluate-g (fn [x] (if (empty? (:precedence x)) 1.0e9 (double (count (:precedence x)))))})]
    {:candidate cid
     :hand-built-patterns (vec (sort-by str (keys interp)))
     :constructor-status (:status result) :refusal (:kind result)
     :constructed (vec (for [x (:candidates result)]
                         {:precedence (:precedence x)
                          :same-pattern-set? (= (set (:precedence x)) (set (keys interp)))
                          :missing-from-constructed (vec (sort-by str (set/difference (set (keys interp)) (set (:precedence x)))))
                          :extra-in-constructed (vec (sort-by str (set/difference (set (:precedence x)) (set (keys interp)))))
                          :respects-containment-order? (linear-extension? (:precedence x) edges)
                          :receipt-kind (get-in x [:construction-receipt :kind])}))
     :findings (:findings result)}))

(def out {:schema :m-futon-seams/construct-replay-v1
          :constructor "futon2.aif.interpretation-construction/construct"
          :constructor-commit (str/trim (:out (sh/sh "git" "-C" "/home/joe/code/futon2" "rev-parse" "--short=8" "HEAD")))
          :replays (vec (for [[cid c] (sort-by (comp str key) cands)] (replay cid c)))})
(spit (str here "/construct-replay.edn") (with-out-str (pp/pprint out)))
(pp/pprint out)
(shutdown-agents)
