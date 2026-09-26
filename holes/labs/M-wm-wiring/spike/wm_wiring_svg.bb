#!/usr/bin/env bb
;; wm_wiring_svg.bb — draw the wiring that wm-flight-wiring.edn declares and
;; wm-flight-wiring-test checks: one box per declared site (a var in a file),
;; one arrow per field from its single writer to each reader, lanes in the
;; order the flight runs. Generated from the map, not hand edited: a box the
;; lane table below does not place, or a field with two writers, stops the
;; script rather than vanishing from the figure.
;;
;;   bb holes/labs/M-wm-wiring/spike/wm_wiring_svg.bb [map.edn] > out.svg
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.java.shell :as sh])

(def map-path (or (first *command-line-args*) "holes/labs/M-wm-wiring/wm-flight-wiring.edn"))
(def m (edn/read-string {:default tagged-literal} (slurp map-path)))
(def boxes (:boxes m))
(def by-id (into {} (map (juxt :box/id identity)) boxes))
(def map-sha (str/trim (:out (sh/sh "git" "log" "-1" "--format=%h" "--" map-path))))
(def head-sha (str/trim (:out (sh/sh "git" "rev-parse" "--short" "HEAD"))))

;; Lanes in flight order: [title components tests]
(def lanes
  [["Loop entry, target field" [:loop-entry :r1-target-field :eligibility :r8-overlap :r1-outer-cascade :flight-entry] [:r1-test :r8-test :loop-test]]
   ["Dispatch, clock-in" [:dispatch :clock-in] []]
   ["Read step (C)" [:r2-flight-read :r2-served-by-reading :r2-verifier] [:r2-test]]
   ["Ask step (interpretation)" [:r3-flight-ask :r3-prompt] [:r3-test]]
   ["Construction, order" [:r4-constructor :r4-order-use :r4-kernel :r4-coapply] [:r4-test :r4-coapply-test]]
   ["Rates (A)" [:r6-sourced-rates :r6-cascade-lane] [:r6-test]]
   ["Selection, gate, refusals" [:r9-classify-target :r9-embedding-neighbour :r9-selection-law :r9-decision :r9-class-model :gate-refuse :gate-refusal-read :r9-judge-refusal :r9-judge-refusal-read :r9-judge-refusal-abstention :r9-abstention-carrier :r9-failure-classifier :c8-registry-get :c8-entry :c8-latest]
    [:r9-test :r9-relation-test :gate-refusal-test :r9-judge-refusal-test]]
   ["Grain gate, enactment, W_c" [:r5-flight-call :r5-grain-gate :r0-enact-step :wc-checker] [:r5-test :r0-test]]
   ["Habit (E), publish" [:r7-flight-call :r7-increment :r7-fold :r7-selection :r10-observe-publication] [:r7-call-test :r7-test :r10-test]]
   ["Click, flight record" [:click-start :flight-cast :flight-click :flight-record-summary :flight-record-click :r11-warrants] [:flight-cast-test :flight-click-close-test]]])

(let [placed (mapcat (fn [[_ c t]] (concat c t)) lanes)
      dup (->> placed frequencies (filter #(> (val %) 1)) keys)
      unknown (remove by-id placed)
      missing (remove (set placed) (keys by-id))]
  (when (or (seq dup) (seq unknown) (seq missing))
    (binding [*out* *err*] (prn {:refused :lane-table-does-not-match-map :duplicated dup :unknown unknown :unplaced missing}))
    (System/exit 2)))

;; geometry
(def lane-w 176) (def lane-gap 22) (def box-w 160) (def box-h 38) (def pitch 48)
(def x0 24) (def y0 96) (def test-gap 16)
(def positions
  (into {}
        (for [[i [_ comps tests]] (map-indexed vector lanes)
              [j id] (map-indexed vector (concat comps [::gap] tests))
              :when (not= id ::gap)
              :let [k (if (some #{id} comps) j (dec j))
                    y (+ y0 30 (* pitch k) (if (some #{id} tests) test-gap 0))]]
          [id {:lane i :x (+ x0 (* i (+ lane-w lane-gap)) 8) :y y}])))
(def lane-heights (for [[_ comps tests] lanes] (+ 30 (* pitch (+ (count comps) (count tests))) (if (seq tests) test-gap 0) 12)))
(def body-h (apply max lane-heights))
(def legend-y (+ y0 body-h 28))
(def width (+ x0 (* (count lanes) (+ lane-w lane-gap)) 4))
(def height (+ legend-y 130))

;; fields: one writer each
(def writers (reduce (fn [acc b] (reduce (fn [a f] (update a f (fnil conj []) (:box/id b))) acc (:writes b))) {} boxes))
(let [multi (filter #(> (count (val %)) 1) writers)]
  (when (seq multi) (binding [*out* *err*] (prn {:refused :field-with-two-writers :fields multi})) (System/exit 2)))
(def reads (for [b boxes f (:reads b)] [f (:box/id b)]))
(def built? #(not= :not-built (:status (by-id %))))
(def test? #(= :test (:box/kind (by-id %))))
(def siteless? #(nil? (:site (by-id %))))

(def trace-seq (mapv #(if (map? %) (:box %) %) (:boxes (first (:traces m)))))
(def trace-numbers (reduce (fn [acc [n id]] (update acc id (fnil conj []) (inc n))) {} (map-indexed vector trace-seq)))

(defn esc [s] (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))
(defn short-file [b] (some-> (or (:site b) (:intended-site b)) :file (str/replace #"^futon2/(src/futon2/aif/|scripts/futon2/report/|scripts/futon2/wm/|scripts/|test/futon2/aif/)" "") (str/replace #"^futon3c/src/futon3c/" "futon3c: ")))
(def out (StringBuilder.))
(defn emit [& xs] (doseq [x xs] (.append out (str x))) (.append out "\n"))

(emit (format "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d' font-family='ui-sans-serif, system-ui, Helvetica, Arial, sans-serif'>" width height width height))
(emit "<title>The flight loop as wm-flight-wiring.edn declares it</title>")
(emit "<defs>"
      "<marker id='a-wired' markerWidth='7' markerHeight='7' refX='6' refY='3.5' orient='auto'><path d='M0,0 L7,3.5 L0,7 z' fill='#2f5f5c'/></marker>"
      "<marker id='a-declared' markerWidth='7' markerHeight='7' refX='6' refY='3.5' orient='auto'><path d='M0,0 L7,3.5 L0,7 z' fill='#d48700'/></marker>"
      "<marker id='a-back' markerWidth='7' markerHeight='7' refX='6' refY='3.5' orient='auto'><path d='M0,0 L7,3.5 L0,7 z' fill='#7b3fa0'/></marker>"
      "<marker id='a-unwritten' markerWidth='7' markerHeight='7' refX='6' refY='3.5' orient='auto'><path d='M0,0 L7,3.5 L0,7 z' fill='#9a9a94'/></marker>"
      "<marker id='a-constraint' markerWidth='7' markerHeight='7' refX='6' refY='3.5' orient='auto'><path d='M0,0 L7,3.5 L0,7 z' fill='#11865b'/></marker>"
      "</defs>")
(emit (format "<rect width='%d' height='%d' fill='white'/>" width height))
(emit "<text x='24' y='34' font-size='19' font-weight='700' fill='#173b39'>The flight loop as the wiring map declares it</text>")
(emit (format "<text x='24' y='52' font-size='11' fill='#52605f'>Generated from %s (%s, futon3c HEAD %s) by spike/wm_wiring_svg.bb; not hand edited.</text>" (esc map-path) map-sha head-sha))
(emit "<text x='24' y='66' font-size='11' fill='#52605f'>A box is a source var the map pins; an arrow is a field with exactly one writer, drawn to each reader; lanes follow the flight's order.</text>")
(emit (format "<text x='24' y='80' font-size='11' fill='#52605f'>The registered test wm-flight-wiring-test checks each site against the pinned source (futon2 %s, futon3c %s) and expects %d findings (%d to-do).</text>"
              (get-in m [:repos "futon2"]) (get-in m [:repos "futon3c"]) (count (:expected-findings m)) (count (filter #(= :to-do (:kind %)) (:expected-findings m)))))

;; lanes
(doseq [[i [title _ _]] (map-indexed vector lanes)]
  (let [x (+ x0 (* i (+ lane-w lane-gap)))]
    (emit (format "<rect x='%d' y='%d' width='%d' height='%d' rx='8' fill='%s'/>" x y0 lane-w body-h (if (even? i) "#f7fafa" "#eef4f4")))
    (emit (format "<text x='%d' y='%d' text-anchor='middle' font-size='11.5' font-weight='700' fill='#274d4a'>%s</text>" (+ x (quot lane-w 2)) (+ y0 19) (esc title)))))

;; edges
(defn anchor [id side] (let [{:keys [x y]} (positions id)] (case side :r [(+ x box-w) (+ y (quot box-h 2))] :l [x (+ y (quot box-h 2))])))
(def edge-count (atom {:wired 0 :declared 0 :back 0 :unwritten 0 :constraint 0}))
(doseq [[f rid] reads]
  (let [w (first (writers f))]
    (if (nil? w)
      (let [{:keys [x y]} (positions rid)
            k (count (filter #(and (= (second %) rid) (nil? (first (writers (first %))))) (take-while #(not= % [f rid]) reads)))
            yy (+ y 9 (* k 9))
            role (get-in m [:field-roles f])
            cls (if (= role :constraint) :constraint :unwritten)]
        (swap! edge-count update cls inc)
        (emit (format "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='%s' stroke-width='1.2' stroke-dasharray='3,2' marker-end='url(#a-%s)'><title>%s: read by %s, written by no box%s</title></line>"
                      (- x 46) yy (- x 2) yy (if (= cls :constraint) "#11865b" "#9a9a94") (name cls) (esc f) (esc rid) (if (= cls :constraint) " (the owner's text, a constraint)" "")))
        (emit (format "<text x='%d' y='%d' text-anchor='end' font-size='7' fill='none' stroke='white' stroke-width='2.5'>%s</text><text x='%d' y='%d' text-anchor='end' font-size='7' fill='%s'>%s</text>" (- x 48) (+ yy 2) (esc (name f)) (- x 48) (+ yy 2) (if (= cls :constraint) "#11865b" "#7a7a74") (esc (name f)))))
      (let [lw (:lane (positions w)) lr (:lane (positions rid))
            cls (cond (< lr lw) :back (or (not (built? w)) (not (built? rid))) :declared :else :wired)
            color ({:wired "#2f5f5c" :declared "#d48700" :back "#7b3fa0"} cls)
            [x1 y1] (if (>= lr lw) (anchor w :r) (anchor w :l))
            [x2 y2] (if (>= lr lw) (anchor rid :l) (anchor rid :r))
            same? (= lr lw)
            d (if same?
                (format "M%d,%d C%d,%d %d,%d %d,%d" x1 y1 (+ x1 60) y1 (+ x2 60) y2 x2 y2)
                (format "M%d,%d C%d,%d %d,%d %d,%d" x1 y1 (+ x1 (if (>= lr lw) 70 -70)) y1 (+ x2 (if (>= lr lw) -70 70)) y2 x2 y2))
            lx (if same? (+ x1 46) (+ x1 (* 0.62 (- x2 x1)))) ly (if same? (/ (+ y1 y2) 2.0) (+ y1 (* 0.62 (- y2 y1)) -3))]
        (swap! edge-count update cls inc)
        (emit (format "<path d='%s' fill='none' stroke='%s' stroke-width='1.3' %s marker-end='url(#a-%s)'><title>%s: %s writes, %s reads</title></path>"
                      d color (if (= cls :declared) "stroke-dasharray='5,3'" "") (name cls) (esc f) (esc w) (esc rid)))
        (emit (format "<text x='%.0f' y='%.0f' font-size='7' fill='none' stroke='white' stroke-width='2.5'>%s</text><text x='%.0f' y='%.0f' font-size='7' fill='%s'>%s</text>" (double lx) (double ly) (esc (name f)) (double lx) (double ly) color (esc (name f))))))))

;; boxes
(doseq [b boxes]
  (let [id (:box/id b) {:keys [x y]} (positions id)
        stroke (cond (not (built? id)) "#d48700" (test? id) "#8a8a84" :else "#2f5f5c")
        fill (cond (not (built? id)) "#fff6e5" (test? id) "#fbfbf8" (siteless? id) "#ececea" :else "#ffffff")
        dash (cond (not (built? id)) "stroke-dasharray='6,3'" (test? id) "stroke-dasharray='2,2'" :else "")
        onTrace (trace-numbers id)
        label (or (some-> b :site :var) (some-> b :intended-site :file (str/replace #".*/" "")) (name id))
        sub (or (short-file b) (case id :wc-checker "futon3c proof2a_check.clj (siteless)" :r11-warrants "warrants (siteless)" "no site"))]
    (emit (format "<g><title>%s%s%s</title><rect x='%d' y='%d' width='%d' height='%d' rx='5' fill='%s' stroke='%s' stroke-width='%s' %s/>"
                  (esc id) (if (not (built? id)) " (not built: intended site)" "") (if (seq (:writes b)) (str " writes " (esc (str/join " " (map name (:writes b))))) "")
                  x y box-w box-h fill stroke (if onTrace "2.2" "1.2") dash))
    (emit (format "<text x='%d' y='%d' font-size='9.5' font-weight='600' fill='#1d3a38'>%s</text>" (+ x 7) (+ y 15) (esc (subs label 0 (min 27 (count label))))))
    (emit (format "<text x='%d' y='%d' font-size='7.5' fill='#5e6b6a'>%s</text>" (+ x 7) (+ y 29) (esc (subs sub 0 (min 36 (count sub))))))
    (when onTrace
      (emit (format "<circle cx='%d' cy='%d' r='8' fill='#173b39'/><text x='%d' y='%d' text-anchor='middle' font-size='7.5' font-weight='700' fill='white'>%s</text>"
                    (- x 1) (- y 1) (- x 1) (+ y 2) (str/join "," onTrace))))
    (emit "</g>")))

;; legend
(let [ly legend-y c @edge-count]
  (emit (format "<text x='24' y='%d' font-size='11' font-weight='700' fill='#274d4a'>Legend</text>" ly))
  (doseq [[k [dx text]] (map-indexed vector
                          [[0 (format "solid box: built component at its pinned var (%d)" (count (filter #(and (built? %) (not (test? %))) (keys by-id))))]
                           [1 (format "dashed amber box: declared, not built, drawn at its intended site (%d)" (count (remove built? (keys by-id))))]
                           [2 (format "dotted box: the row's registered test, a declared reader (%d)" (count (filter test? (keys by-id))))]
                           [3 (format "grey box: siteless component, exempt from conformance (%d)" (count (filter siteless? (keys by-id))))]])]
    (emit (format "<text x='24' y='%d' font-size='9.5' fill='#3a4a48'>%s</text>" (+ ly 16 (* k 13)) (esc text))))
  (doseq [[k [color dash text]] (map-indexed vector
                                  [["#2f5f5c" "" (format "arrow: a field, from its one writer to a reader, both built (%d)" (:wired c))]
                                   ["#d48700" "5,3" (format "dashed arrow: declared on a box not yet built (%d)" (:declared c))]
                                   ["#7b3fa0" "" (format "purple arrow: runs against the flight's order, the loop closing (%d)" (:back c))]
                                   ["#9a9a94" "3,2" (format "grey stub: a field read that no box writes (%d); green stub: the owner's text, exogenous (%d)" (:unwritten c) (:constraint c))]])]
    (emit (format "<line x1='560' y1='%d' x2='600' y2='%d' stroke='%s' stroke-width='1.4' %s/>" (+ ly 13 (* k 13)) (+ ly 13 (* k 13)) color (if (seq dash) (str "stroke-dasharray='" dash "'") "")))
    (emit (format "<text x='608' y='%d' font-size='9.5' fill='#3a4a48'>%s</text>" (+ ly 16 (* k 13)) (esc text))))
  (emit (format "<text x='24' y='%d' font-size='9.5' fill='#3a4a48'>Numbered rings: the exemplar trace the map records for %s, in order (%s). Boxes on it have a heavier border.</text>"
                (+ ly 76) (esc (:target (first (:traces m)))) (esc (str/join " > " (map name trace-seq)))))
  (emit (format "<text x='24' y='%d' font-size='9.5' fill='#3a4a48'>What this does not show: the prover reads sites textually, so a field's presence at a var is what is checked, not that the value flows; the eleven code shapes it cannot see are listed in WM-MAP-REPLAY-D. Standing findings (%d) are generic keys the prover cannot scope per box.</text>"
                (+ ly 92) (count (filter #(= :standing (:kind %)) (:expected-findings m))))))
(emit "</svg>")
(print (str out))
