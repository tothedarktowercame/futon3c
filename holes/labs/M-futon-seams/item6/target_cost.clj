;; The cost side of a per-target value for PROOF-2a Clause T, on the
;; M-futon-seams instance cascades. claude-10, 2026-09-24.
;; Run from futon3c: bb holes/labs/M-futon-seams/item6/target_cost.clj
;; Per instance, at a common theta: expected attempts to complete (each
;; pattern of the cascade is attempted until it succeeds), and the share of
;; those attempts spent on patterns whose products no pattern needs and no
;; want asks for (dangling work). Deterministic; no value side (that needs
;; mission-level preferences, which are not in the instance cascades).
(require '[clojure.edn :as edn] '[clojure.set :as set] '[clojure.pprint :as pp])
(def theta 0.8)
(def base "holes/labs/M-futon-seams/proto/")
(def out
  (vec (for [n ["4" "4b" "5" "6" "7"]
             :let [c (edn/read-string (slurp (str base "instance-" n ".edn")))
                   pats (:patterns c)
                   needed (set/union (set (:want c)) (reduce set/union #{} (map #(get-in % [:guard :needs]) (vals pats))))
                   ;; a pattern is useful if any of its products is needed or wanted
                   useful (set (for [[p i] pats :when (seq (set/intersection (:produces i) needed))] p))
                   dangling (set/difference (set (keys pats)) useful)
                   ea (/ (count pats) theta)]]
         {:instance n :patterns (count pats) :wants (count (:want c))
          :expected-attempts ea
          :dangling-patterns (vec (sort-by str dangling))
          :share-of-attempts-dangling (/ (Math/round (* 1000.0 (/ (count dangling) (count pats)))) 1000.0)})))
(spit "holes/labs/M-futon-seams/item6/target-cost.edn" (with-out-str (pp/pprint {:theta theta :targets out})))
(doseq [x out] (println x))
