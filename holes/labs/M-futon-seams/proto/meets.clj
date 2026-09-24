#!/usr/bin/env bb
;; meets.clj — the restricted semilattice condition of PROOF-2a clause 0.
;;
;;   bb holes/labs/M-futon-seams/proto/meets.clj FILE...
;;
;; The carrier is a containment order: a unit sits above another exactly when
;; it contains it. Clause 0 requires a meet only for OVERLAPPING pairs -- two
;; patterns that share a descendant must have a greatest common descendant
;; that is itself a pattern of the cascade. Disjoint pairs need none. A pair
;; that overlaps and has no greatest common descendant is a typed finding,
;; naming the pair and the maximal units of their common part.
(require '[clojure.edn :as edn] '[clojure.set :as set])

(defn below
  "Strict descendants of ID: everything the unit contains, transitively."
  [children id]
  (loop [seen #{} frontier (get children id #{})]
    (if (empty? frontier) seen
        (recur (into seen frontier)
               (set/difference (reduce set/union #{} (map #(get children % #{}) frontier))
                               seen)))))

(defn analyse [file]
  (let [c (edn/read-string (slurp file))
        ids (vec (sort (keys (:patterns c))))
        children (reduce (fn [m {:keys [context pattern]}]
                           (update m context (fnil conj #{}) pattern))
                         {} (:above c))
        desc (into {} (for [i ids] [i (below children i)]))
        pairs (for [a ids b ids :when (neg? (compare (str a) (str b)))] [a b])
        results
        (for [[a b] pairs
              :let [common (set/intersection (desc a) (desc b))]
              :when (seq common)]
          (let [;; maximal units of the common part: in `common`, with no other
                ;; member of `common` above them
                maximal (set (remove (fn [d] (some #(contains? (desc %) d)
                                                   (disj common d)))
                                     common))
                gcd (when (= 1 (count maximal))
                      (let [m (first maximal)]
                        (when (every? #(or (= % m) (contains? (desc m) %)) common) m)))]
            {:pair [a b] :common-count (count common)
             :maximal-common (vec (sort maximal)) :meet gcd}))]
    {:file file :instance (:instance c)
     :patterns (count ids) :overlapping-pairs (count results)
     :with-meet (count (filter :meet results))
     :findings (vec (remove :meet results))
     :all (vec results)}))

(doseq [f *command-line-args*]
  (let [r (analyse f)]
    (println "=====" (:file r) "| instance" (:instance r))
    (println " patterns" (:patterns r)
             "| overlapping pairs" (:overlapping-pairs r)
             "| with a greatest common descendant" (:with-meet r))
    (if (empty? (:findings r))
      (println " no missing meets: every overlapping pair has one")
      (doseq [x (:findings r)]
        (println " MISSING MEET" (pr-str (:pair x))
                 "\n   common part has" (:common-count x) "units; maximal:"
                 (pr-str (:maximal-common x)))))))
