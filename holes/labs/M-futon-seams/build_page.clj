#!/usr/bin/env bb
;; Emit one JSON bundle per instance for the M-futon-seams page: the cascade,
;; its layered layout, the states where the frontier is wider than one, and
;; the three kernels' numbers at several horizons. Reuses kernels.clj so the
;; page reports the prototype's output rather than a second implementation of
;; it -- the instance-7 defect, avoided here on purpose.
(require '[clojure.edn :as edn] '[clojure.set :as set] '[cheshire.core :as j])
(load-file (str (System/getProperty "user.dir") "/holes/labs/M-futon-seams/proto/kernels_lib.clj"))

(defn depth [anc id] (if (empty? (anc id)) 0 (inc (apply max (map #(depth anc %) (anc id))))))

(defn bundle [file horizons theta]
  (let [c (edn/read-string (slurp file))
        pats (:patterns c) ids (vec (keys pats))
        anc (ancestors-map ids (:above c))
        s0 (:initial c) want (:want c)
        exts (linear-extensions ids anc 200)
        reach (keys (rollout :coapp pats anc nil 0.5 s0 (apply max horizons)))
        wide (for [s reach :let [f (frontier pats anc s)] :when (> (count f) 1)]
               {:state (vec (sort s)) :frontier (vec (sort f))
                :conflicts (vec (conflicts pats f))})]
    {:instance (:instance c) :file file :mission-sha (:mission-sha c)
     :tokens (:tokens c) :initial (vec s0) :want (vec want) :holes (:holes c)
     :patterns (into {} (for [[id p] pats]
                          [id {:needs (vec (sort (get-in p [:guard :needs])))
                               :forbids (vec (sort (get-in p [:guard :forbids])))
                               :produces (vec (sort (:produces p)))
                               :receipt (:receipt p) :forces (:forces p)
                               :depth (depth anc id)}]))
     :above (:above c)
     :linear-extensions (count exts)
     :wide-states (vec (take 40 wide))
     :wide-count (count wide) :reachable (count reach)
     :runs (vec (for [T horizons]
                  (let [lists (map #(summary (rollout :list pats anc % theta s0 T) want) exts)]
                    {:horizon T :theta theta
                     :list {:p-min (r3 (apply min (map :p-all-wants lists)))
                            :p-max (r3 (apply max (map :p-all-wants lists)))
                            :e-min (r3 (apply min (map :e-wants lists)))
                            :e-max (r3 (apply max (map :e-wants lists)))}
                     :coapp (let [s (summary (rollout :coapp pats anc nil theta s0 T) want)]
                              {:p (r3 (:p-all-wants s)) :e (r3 (:e-wants s))})
                     :inter (let [s (summary (rollout :inter pats anc nil theta s0 T) want)]
                              {:p (r3 (:p-all-wants s)) :e (r3 (:e-wants s))})})))}))

(let [[out & files] *command-line-args*]
  (spit out (j/generate-string
             (vec (for [f files] (bundle f [6 10 14] 0.8)))))
  (println "wrote" out (count files) "instances"))
