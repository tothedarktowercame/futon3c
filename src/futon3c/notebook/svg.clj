(ns futon3c.notebook.svg
  "Small SVG diagrams for notebooks: a directed graph drawn in layers (each
   node one column right of its furthest predecessor), boxes and arrows.
   No layout engine is installed on this machine, and the graphs here are
   small enough not to need one."
  (:require [clojure.string :as str]))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- layers [nodes edges]
  (let [preds (reduce (fn [m [a b]] (update m b (fnil conj #{}) a)) {} edges)
        depth (memoize (fn depth [n seen]
                         (if (seen n) 0
                             (inc (reduce max -1 (map #(depth % (conj seen n)) (preds n #{})))))))]
    (into {} (for [n nodes] [n (depth n #{})]))))

(defn dag
  "An SVG string drawing NODES (seq of ids or [id label]) and EDGES
   ([from to] ...). OPTS: :fill {id colour}, :width per column, :title."
  [nodes edges & {:keys [fill col-width title] :or {fill {} col-width 170}}]
  (let [nodes (map #(if (vector? %) % [% (name %)]) nodes)
        ids (map first nodes)
        label (into {} nodes)
        layer (layers ids edges)
        cols (group-by layer ids)
        row-h 46 box-w (- col-width 30) box-h 30
        pos (into {} (for [[c ns] cols [i n] (map-indexed vector ns)]
                       [n [(+ 10 (* c col-width)) (+ 30 (* i row-h))]]))
        w (+ 20 (* col-width (inc (reduce max 0 (keys cols)))))
        h (+ 50 (* row-h (reduce max 1 (map count (vals cols)))))]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" w "\" height=\"" h "\" "
         "font-family=\"ui-monospace,Menlo,monospace\" font-size=\"11\">"
         "<defs><marker id=\"a\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"7\" markerHeight=\"7\" orient=\"auto\">"
         "<path d=\"M0,0 L10,5 L0,10 z\" fill=\"#666\"/></marker></defs>"
         (when title (str "<text x=\"10\" y=\"16\" font-size=\"12\" fill=\"#444\">" (esc title) "</text>"))
         (str/join
          (for [[a b] edges :let [[x1 y1] (pos a) [x2 y2] (pos b)] :when (and x1 x2)]
            (str "<line x1=\"" (+ x1 box-w) "\" y1=\"" (+ y1 (/ box-h 2)) "\" x2=\"" x2 "\" y2=\"" (+ y2 (/ box-h 2))
                 "\" stroke=\"#666\" marker-end=\"url(#a)\"/>")))
         (str/join
          (for [n ids :let [[x y] (pos n)]]
            (str "<rect x=\"" x "\" y=\"" y "\" width=\"" box-w "\" height=\"" box-h "\" rx=\"5\" "
                 "fill=\"" (get fill n "#f4f2ea") "\" stroke=\"#999\"/>"
                 "<text x=\"" (+ x 6) "\" y=\"" (+ y 19) "\">" (esc (label n)) "</text>")))
         "</svg>")))
