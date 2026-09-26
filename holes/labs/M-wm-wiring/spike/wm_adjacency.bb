#!/usr/bin/env bb
;; wm_adjacency.bb — the wiring map as an adjacency matrix. Rows are writers,
;; columns are readers, an entry is the set of fields sent from the row's box
;; to the column's box (one writer per field, so an entry is the fields the
;; column reads that the row writes). Emits EDN (the matrix, degrees, weakly
;; connected components, step-boundary crossings) and an SVG of the matrix
;; with boxes grouped by step. Generated from the map at a named commit; the
;; lane table is shared with wm_wiring_svg.bb by reading it from that script.
;;   bb wm_adjacency.bb <map-rev> edn|svg
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.java.shell :as sh])
(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def map-rev (first *command-line-args*)) (def mode (or (second *command-line-args*) "edn"))
(def m (edn/read-string {:default tagged-literal} (:out (sh/sh "git" "show" (str map-rev ":" map-path)))))
(def map-sha (str/trim (:out (sh/sh "git" "rev-parse" "--short" map-rev))))
;; the lane table from the drawing script, so the two figures agree on the steps
(def lanes (let [src (slurp "holes/labs/M-wm-wiring/spike/wm_wiring_svg.bb")
                 form (re-find #"(?s)\(def lanes\s+(\[.*?\]\]\])\)" src)]
             (edn/read-string (second form))))
;; a record-scoped entry [field {:record r}] stays as written in the
;; matrix (readable EDN, the map's own form) and is labelled field@r, the
;; projection's port name (WM-PROVER-RECORD-SCOPE-I, futon3c 5e4d9d02)
(defn field-label [f] (if (and (vector? f) (:record (second f))) (str (name (first f)) "@" (name (:record (second f)))) (name f)))
(def boxes (:boxes m))
(def by-id (into {} (map (juxt :box/id identity)) boxes))
(def ordered (vec (mapcat (fn [[_ c t]] (concat c t)) lanes)))
(let [missing (remove (set ordered) (keys by-id))] (when (seq missing) (binding [*out* *err*] (prn {:refused :unplaced-boxes :boxes missing})) (System/exit 2)))
(def lane-of (into {} (for [[i [_ c t]] (map-indexed vector lanes) id (concat c t)] [id i])))
(def writers (reduce (fn [acc b] (reduce (fn [a f] (assoc a f (:box/id b))) acc (:writes b))) {} boxes))
(def matrix (reduce (fn [acc b] (reduce (fn [a f] (if-let [w (writers f)] (if (= w (:box/id b)) a (update a [w (:box/id b)] (fnil conj #{}) f)) a)) acc (:reads b))) {} boxes))
(def out-degree (reduce (fn [acc [[w _] fs]] (update acc w (fnil + 0) (count fs))) {} matrix))
(def in-degree (reduce (fn [acc [[_ r] fs]] (update acc r (fnil + 0) (count fs))) {} matrix))
(def test? #(= :test (:box/kind (by-id %))))
;; weakly connected components over non-test boxes
(def comp-boxes (remove test? ordered))
(def adj (reduce (fn [acc [[a b] _]] (if (or (test? a) (test? b)) acc (-> acc (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))) {} matrix))
(def components (loop [left (set comp-boxes) acc []]
                  (if (empty? left) acc
                      (let [start (first (filter left ordered))
                            comp (loop [seen #{start} frontier [start]]
                                   (if (empty? frontier) seen
                                       (let [nxt (remove seen (mapcat adj frontier))] (recur (into seen nxt) (vec (distinct nxt))))))]
                        (recur (reduce disj left comp) (conj acc (vec (filter comp ordered))))))))
(def boundary-crossings (for [i (range (dec (count lanes)))]
                          [i (inc i) (vec (for [[[a b] fs] matrix :when (and (not (test? b)) (<= (lane-of a) i) (< i (lane-of b)))] [a b (vec fs)]))]))
(def islands (for [[i [title comps _]] (map-indexed vector lanes)
                   :when (empty? (for [[[a b] _] matrix :when (and (not (test? b)) (not= (lane-of a) (lane-of b)) (or (= i (lane-of a)) (= i (lane-of b))))] 1))]
               [i title]))
(case mode
  "edn"
  (prn {:map map-sha :boxes (count ordered) :non-test (count comp-boxes)
        :entries (count matrix) :wires (reduce + (map count (vals matrix)))
        :out-degree (into (sorted-map-by #(compare [(- (out-degree %2 0)) %2] [(- (out-degree %1 0)) %1])) out-degree)
        :in-degree (into (sorted-map-by #(compare [(- (in-degree %2 0)) %2] [(- (in-degree %1 0)) %1])) in-degree)
        :isolated (vec (for [b comp-boxes :when (and (nil? (out-degree b)) (nil? (in-degree b)))] b))
        :components (mapv (fn [c] {:size (count c) :boxes c}) (sort-by (comp - count) components))
        :step-boundaries (vec (for [[i j xs] boundary-crossings] {:from i :to j :wires (count xs) :fields (vec (distinct (mapcat #(nth % 2) xs)))}))
        :island-steps (vec islands)
        :matrix (into (sorted-map-by #(compare (str %1) (str %2))) (for [[k v] matrix] [k (vec (sort-by str v))]))})
  "svg"
  (let [n (count ordered) cell 11 left 190 top 200 w (+ left (* n cell) 30) h (+ top (* n cell) 40)
        idx (into {} (map-indexed (fn [i b] [b i]) ordered))
        lane-starts (reductions + 0 (map (fn [[_ c t]] (+ (count c) (count t))) lanes))
        colors ["#2f5f5c" "#7b3fa0"]
        out (StringBuilder.)
        emit (fn [& xs] (doseq [x xs] (.append out (str x))) (.append out "\n"))
        esc (fn [s] (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;")))]
    (emit (format "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d' font-family='ui-sans-serif, system-ui, Helvetica, Arial, sans-serif'>" w h w h))
    (emit (format "<rect width='%d' height='%d' fill='white'/>" w h))
    (emit "<text x='16' y='28' font-size='18' font-weight='700' fill='#173b39'>The wiring map as an adjacency matrix</text>")
    (emit (format "<text x='16' y='46' font-size='11' fill='#52605f'>Row: the box that writes; column: the box that reads; a cell: at least one field sent from row to column (hover for the fields). Map at futon3c %s; %d boxes, %d wires in %d entries. Teal: reader in the same or a later step; purple: reader in an earlier step. Steps separated by lines, in flight order.</text>" map-sha n (reduce + (map count (vals matrix))) (count matrix)))
    (doseq [[i b] (map-indexed vector ordered)]
      (let [x (+ left (* i cell)) y (+ top (* i cell))]
        (emit (format "<text x='%d' y='%d' font-size='6.5' text-anchor='end' fill='%s'>%s</text>" (- left 4) (+ y 8) (if (test? b) "#8a8a84" "#1d3a38") (esc (name b))))
        (emit (format "<text x='%d' y='%d' font-size='6.5' fill='%s' transform='rotate(-90 %d %d)'>%s</text>" (+ x 8) (- top 4) (if (test? b) "#8a8a84" "#1d3a38") (+ x 8) (- top 4) (esc (name b))))))
    (doseq [[i [title _ _]] (map-indexed vector lanes) :let [s (nth lane-starts i) e (nth lane-starts (inc i)) x0 (+ left (* s cell)) y0 (+ top (* s cell)) sz (* (- e s) cell)]]
      (emit (format "<rect x='%d' y='%d' width='%d' height='%d' fill='%s' stroke='#b9c6c4' stroke-width='0.8'/>" x0 y0 sz sz (if (even? i) "#f3f7f7" "#e9f0f0")))
      (emit (format "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='#7f9391' stroke-width='0.8'/>" x0 top x0 (+ top (* n cell)) ))
      (emit (format "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='#7f9391' stroke-width='0.8'/>" left y0 (+ left (* n cell)) y0))
      (emit (format "<text x='%d' y='%d' font-size='7' font-weight='700' fill='#274d4a' transform='rotate(-90 %d %d)'>%s</text>" (+ x0 2) (- top 130) (+ x0 2) (- top 130) (esc (subs title 0 (min 30 (count title)))))))
    (doseq [[[a b] fs] matrix :let [x (+ left (* (idx b) cell)) y (+ top (* (idx a) cell)) back? (< (lane-of b) (lane-of a))]]
      (emit (format "<rect x='%d' y='%d' width='%d' height='%d' fill='%s' opacity='%s'><title>%s → %s: %s</title></rect>" (inc x) (inc y) (- cell 2) (- cell 2) (if back? (second colors) (first colors)) (if (test? b) "0.35" "0.95") (esc (name a)) (esc (name b)) (esc (str/join " " (map field-label (sort-by str fs)))))))
    (emit (format "<text x='16' y='%d' font-size='10' fill='#3a4a48'>Island steps (no wire in or out, tests excluded): %s. Weakly connected components among non-test boxes: %s (sizes %s).</text>" (- h 12) (esc (str/join "; " (map second islands))) (count components) (esc (str/join ", " (map count (sort-by (comp - count) components))))))
    (emit "</svg>")
    (print (str out))))
