#!/usr/bin/env bb
;; wm_wiring_svg.bb — draw the wiring that wm-flight-wiring.edn declares and
;; wm-flight-wiring-test checks, as a harness: each lane of the flight is a
;; panel holding its boxes (one per declared site) with the fields each box
;; writes and reads listed inside it; a field whose writer and readers share
;; a lane is an arrow inside the panel; a field that crosses lanes is ONE
;; horizontal line in the band beneath, tapped by its writer (dot) and by
;; each reader (arrow). Generated from the map, not hand edited: a box the
;; lane table does not place, or a field with two writers, stops the script
;; rather than vanishing from the figure. A lane that no field enters or
;; leaves is labelled as such on the panel.
;;
;;   bb holes/labs/M-wm-wiring/spike/wm_wiring_svg.bb [map.edn [rev]] > out.svg
;; With REV, the map is read at that commit (git show), never from the
;; working tree, so a figure is never drawn from an uncommitted map.
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.java.shell :as sh])

(def map-path (or (first *command-line-args*) "holes/labs/M-wm-wiring/wm-flight-wiring.edn"))
(def map-rev (second *command-line-args*))
(def m (edn/read-string {:default tagged-literal}
                        (if map-rev
                          (let [r (sh/sh "git" "show" (str map-rev ":" map-path))]
                            (when-not (zero? (:exit r)) (binding [*out* *err*] (println (:err r))) (System/exit 2))
                            (:out r))
                          (slurp map-path))))
;; a record-scoped entry [field {:record r}] is the field field@r, the
;; projection's port name (WM-PROVER-RECORD-SCOPE-I, futon3c 5e4d9d02)
(defn scoped-name [f] (if (and (vector? f) (:record (second f))) (keyword (str (name (first f)) "@" (name (:record (second f))))) f))
(def boxes (mapv (fn [b] (cond-> b (:reads b) (update :reads #(mapv scoped-name %)) (:writes b) (update :writes #(mapv scoped-name %)))) (:boxes m)))
(def by-id (into {} (map (juxt :box/id identity)) boxes))
(def map-sha (if map-rev
               (str/trim (:out (sh/sh "git" "rev-parse" "--short" map-rev)))
               (str/trim (:out (sh/sh "git" "log" "-1" "--format=%h" "--" map-path)))))
(def head-sha (str/trim (:out (sh/sh "git" "rev-parse" "--short" "HEAD"))))

;; Lanes in flight order: [title components tests]
(def lanes
  [["1 Loop entry, target field" [:loop-entry :r1-target-field :eligibility :r8-overlap :r1-outer-cascade :flight-entry] [:r1-test :r8-test :loop-test]]
   ["2 Dispatch, clock-in" [:dispatch :clock-in] []]
   ["3 Read step (C)" [:r2-flight-read :r2-served-by-reading :r2-verifier :r2-store-criteria :r2-store-coverage :r2-store-locators :r2-store-locator-questions :r2-store-locator-declines :r2-store-constraints-read] [:r2-test]]
   ["4 Ask step (interpretation)" [:r3-flight-ask :r3-prompt :flight-click-wants :ask-merge-published :r3-store-criteria :r3-store-coverage :r3-store-locators :r3-store-locator-questions :r3-store-locator-declines :r3-store-constraints-read] [:r3-test]]
   ["5 Construction, order" [:tick-flight-assembly :construction-assemble-one :construction-construct :r4-constructor :r4-order-use :r4-coapply] [:r4-test :r4-coapply-test]]
   ["6 Rates (A)" [:r6-sourced-rates :r6-cascade-lane :r4-kernel] [:r6-test]]
   ["7a Selection, decision, registry"
    [:r9-classify-target :r9-embedding-neighbour :r9-selection-law :r9-decision :selection-candidate-derivations :r9-class-model :c8-registry-get :c8-entry :c8-latest]
    [:r9-test :r9-relation-test]]
   ["7b Selection: refusals, failure record"
    [:gate-refuse :gate-refusal-read :r9-judge-refusal :r9-judge-refusal-read :r9-judge-refusal-abstention :r9-abstention-carrier :r9-phase-kind :r9-failure-classifier :r9-close-cause :r9-finding-store :r9-finding-cause-read]
    [:gate-refusal-test :r9-judge-refusal-test :phase-kind-test :failure-cause-record-test]]
   ["8 Grain gate, enactment, W_c" [:r5-flight-call :r5-grain-gate :r0-enact-step :wc-checker] [:r5-test :r0-test]]
   ["9 Habit (E), publish" [:r7-flight-call :r7-increment :r7-fold :r7-fold-source :r7-fold-call :r7-selection :r10-observe-publication] [:r7-call-test :r7-test :habit-fold-call-test :r10-test]]
   ["10 Click, flight record" [:click-start :flight-cast :flight-click :flight-record-summary :flight-record-click :flight-run :flight-driver-summary :flight-judge-opts :r11-warrants] [:flight-cast-test :flight-click-close-test :click-reason-test]]])

(let [placed (mapcat (fn [[_ c t]] (concat c t)) lanes)
      dup (->> placed frequencies (filter #(> (val %) 1)) keys)
      unknown (remove by-id placed)
      missing (remove (set placed) (keys by-id))]
  (when (or (seq dup) (seq unknown) (seq missing))
    (binding [*out* *err*] (prn {:refused :lane-table-does-not-match-map :duplicated dup :unknown unknown :unplaced missing}))
    (System/exit 2)))

;; classification
(def writers (reduce (fn [acc b] (reduce (fn [a f] (update a f (fnil conj []) (:box/id b))) acc (:writes b))) {} boxes))
(let [multi (filter #(> (count (val %)) 1) writers)]
  (when (seq multi) (binding [*out* *err*] (prn {:refused :field-with-two-writers :fields multi})) (System/exit 2)))
(def writer-of #(first (writers %)))
(def built? #(not= :not-built (:status (by-id %))))
(def test? #(= :test (:box/kind (by-id %))))
(def siteless? #(nil? (:site (by-id %))))
(def lane-of (into {} (for [[i [_ c t]] (map-indexed vector lanes) id (concat c t)] [id i])))
(def n-lanes (count lanes))

;; every (field reader) pair, tests excluded from arrows (they list their reads)
(def read-pairs (for [b boxes :when (not (test? (:box/id b))) f (:reads b) :when (not= (writer-of f) (:box/id b))] [f (:box/id b)]))
(def unwritten (filter (fn [[f _]] (nil? (writer-of f))) read-pairs))
(def written (remove (fn [[f _]] (nil? (writer-of f))) read-pairs))
(def readers-of (reduce (fn [acc [f r]] (update acc f (fnil conj []) r)) {} written))
(def cross-fields (->> readers-of (filter (fn [[f rs]] (some #(not= (lane-of %) (lane-of (writer-of f))) rs))) (map key)))
(def intra-fields (->> readers-of (filter (fn [[f rs]] (every? #(= (lane-of %) (lane-of (writer-of f))) rs))) (map key)))
(def cross? (set cross-fields))

;; geometry
(def header-h 104) (def strip-h 34) (def margin-l 150)
(def panel-w 244) (def inner-l 34) (def box-w 196) (def box-h 62) (def pitch 72) (def test-gap 20) (def panel-pad 44)
(def bus-pitch 13)

;; per-gutter taps: gutter g sits left of lane g; exits from lane g-1 and entries into lane g use it
(def taps-in-gutter
  (reduce (fn [acc f]
            (let [w (writer-of f) wl (lane-of w)
                  acc (update acc (inc wl) (fnil conj []) [f :exit w])]
              (reduce (fn [a r] (if (= (lane-of r) wl) a (update a (lane-of r) (fnil conj []) [f :entry r])))
                      acc (distinct (readers-of f)))))
          {} cross-fields))
(defn gutter-w [g] (+ 16 (* 5 (count (taps-in-gutter g)))))
(def panel-x (loop [i 0 x margin-l acc []] (if (= i n-lanes) acc (let [x (+ x (gutter-w i))] (recur (inc i) (+ x panel-w) (conj acc x))))))
(defn gutter-x [g] (if (zero? g) (- (panel-x 0) (gutter-w 0)) (+ (panel-x (dec g)) panel-w)))
(def width (+ (gutter-x n-lanes) (gutter-w n-lanes) 24))

(def positions
  (into {}
        (for [[i [_ comps tests]] (map-indexed vector lanes)
              [j id] (map-indexed vector (concat comps tests))
              :let [k j
                    y (+ header-h strip-h panel-pad (* pitch k) (if (some #{id} tests) test-gap 0))]]
          [id {:lane i :x (+ (panel-x i) inner-l) :y y}])))
(def panel-h (apply max (for [[_ comps tests] lanes] (+ panel-pad (* pitch (+ (count comps) (count tests))) (if (seq tests) test-gap 0) 8))))
(def bus-y0 (+ header-h strip-h panel-h 48))
(def bus-order (vec (sort-by (fn [f] [(lane-of (writer-of f)) (:y (positions (writer-of f))) (name f)]) cross-fields)))
(def bus-y (into {} (map-indexed (fn [i f] [f (+ bus-y0 (* i bus-pitch))]) bus-order)))
(def legend-y (+ bus-y0 (* bus-pitch (count bus-order)) 40))
(def height (+ legend-y 166))

;; tracks: within a gutter, ordered by bus y so vertical runs fan out in order
(def track-x
  (into {}
        (for [g (range (inc n-lanes))
              [t [f kind who]] (map-indexed vector (sort-by (fn [[f _ _]] (bus-y f)) (taps-in-gutter g)))]
          [[f kind who] (+ (gutter-x g) 8 (* 5 t))])))

;; ports: a box's cross-lane writes exit on the right, its cross-lane reads enter on the left, spread over the box height
(defn port-y [id side f]
  (let [b (by-id id)
        fs (case side :r (filter cross? (:writes b)) :l (filter #(and (cross? %) (not= (writer-of %) id)) (:reads b)))
        n (count fs) k (.indexOf (vec fs) f)
        {:keys [y]} (positions id)]
    (+ y (quot box-h 2) (* 8 (- k (/ (dec n) 2.0))))))

(defn esc [s] (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))
(defn short-file [b] (some-> (or (:site b) (:intended-site b)) :file (str/replace #"^futon2/(src/futon2/aif/|scripts/futon2/report/|scripts/futon2/wm/|scripts/|test/futon2/aif/)" "") (str/replace #"^futon3c/src/futon3c/" "futon3c: ")))
(defn clip [s n] (let [s (str s)] (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))
(def out (StringBuilder.))
(defn emit [& xs] (doseq [x xs] (.append out (str x))) (.append out "\n"))
(defn label [x y size color anchor text & [extra]]
  (emit (format "<text x='%.1f' y='%.1f' font-size='%s' text-anchor='%s' fill='none' stroke='white' stroke-width='3' %s>%s</text><text x='%.1f' y='%.1f' font-size='%s' text-anchor='%s' fill='%s' %s>%s</text>"
                (double x) (double y) size anchor (or extra "") (esc text) (double x) (double y) size anchor color (or extra "") (esc text))))

(def trace-seq (mapv #(if (map? %) (:box %) %) (:boxes (first (:traces m)))))
(def trace-numbers (reduce (fn [acc [n id]] (update acc id (fnil conj []) (inc n))) {} (map-indexed vector trace-seq)))

(def colors {:wired "#2f5f5c" :declared "#d48700" :back "#7b3fa0" :unwritten "#9a9a94" :constraint "#11865b"})
(defn field-class [f r] (let [w (writer-of f)] (cond (< (lane-of r) (lane-of w)) :back (or (not (built? w)) (not (built? r))) :declared :else :wired)))
(defn line-class [f] (let [w (writer-of f) rs (distinct (readers-of f))] (cond (not (built? w)) :declared (every? #(< (lane-of %) (lane-of w)) rs) :back :else :wired)))

(emit (format "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d' font-family='ui-sans-serif, system-ui, Helvetica, Arial, sans-serif'>" width height width height))
(emit "<title>The flight loop as wm-flight-wiring.edn declares it</title>")
(emit "<defs>")
(doseq [[k c] colors] (emit (format "<marker id='a-%s' markerWidth='7' markerHeight='7' refX='6' refY='3.5' orient='auto'><path d='M0,0 L7,3.5 L0,7 z' fill='%s'/></marker>" (name k) c)))
(emit "</defs>")
(emit (format "<rect width='%d' height='%d' fill='white'/>" width height))
(emit "<text x='24' y='36' font-size='24' font-weight='700' fill='#173b39'>The flight loop as the wiring map declares it</text>")
(emit (format "<text x='24' y='58' font-size='12' fill='#52605f'>Generated from %s (%s, futon3c HEAD %s) by spike/wm_wiring_svg.bb; not hand edited. %d boxes, %d fields.</text>" (esc map-path) map-sha head-sha (count boxes) (count (distinct (mapcat #(concat (:reads %) (:writes %)) boxes)))))
(def island-lanes (for [[i _] (map-indexed vector lanes) :when (and (empty? (filter #(= :exit (second %)) (taps-in-gutter (inc i)))) (empty? (filter #(= :entry (second %)) (taps-in-gutter i))))] i))
(emit "<text x='24' y='74' font-size='12' fill='#52605f'>Each panel is one step of the flight, in order. A box is a source var the map pins, listing what it writes (w) and reads (r). A field kept inside a step is an arrow inside its panel.</text>")
(emit (format "<text x='24' y='90' font-size='12' fill='#52605f'>A field that crosses steps is one line in the band below, tapped by its writer (dot) and each reader (arrow). The registered test checks each site at the pins (futon2 %s, futon3c %s), expecting %d findings (%d to-do). %d fields cross steps; %d of %d steps are entered or left by none.</text>"
              (get-in m [:repos "futon2"]) (get-in m [:repos "futon3c"]) (count (:expected-findings m)) (count (filter #(= :to-do (:kind %)) (:expected-findings m))) (count cross-fields) (count island-lanes) n-lanes))

;; panels
(doseq [[i [title comps _]] (map-indexed vector lanes)]
  (let [x (panel-x i)
        crossing (count (filter (fn [[f _ _]] true) (concat (filter #(= :exit (second %)) (taps-in-gutter (inc i))) (filter #(= :entry (second %)) (taps-in-gutter i)))))]
    (emit (format "<rect x='%d' y='%d' width='%d' height='%d' rx='10' fill='%s' stroke='#d9e2e1'/>" x (+ header-h strip-h) panel-w panel-h (if (even? i) "#f7fafa" "#eef4f4")))
    (emit (format "<text x='%d' y='%d' font-size='12.5' font-weight='700' fill='#274d4a'>%s</text>" (+ x 12) (+ header-h strip-h 20) (esc title)))
    (let [on-trace (sort (mapcat #(trace-numbers %) comps))]
      (emit (format "<text x='%d' y='%d' font-size='9' fill='%s'>%s</text>" (+ x 12) (+ header-h strip-h 33)
                    (if (seq on-trace) "#3a4a48" "#b25a00")
                    (if (seq on-trace) (str "exemplar trace steps " (str/join ", " on-trace)) "not on the exemplar trace"))))
    (when (zero? crossing)
      (emit (format "<text x='%d' y='%d' font-size='9.5' font-style='italic' fill='#b25a00'>no field enters or leaves this step in the map</text>" (+ x 12) (+ header-h strip-h 45))))))

;; the hand-off strip: between step i and i+1, the declared fields that cross that boundary rightwards
(doseq [i (range (dec n-lanes))]
  (let [fs (filter (fn [f] (let [wl (lane-of (writer-of f))] (and (<= wl i) (some #(> (lane-of %) i) (readers-of f))))) cross-fields)
        gx (+ (panel-x i) panel-w) gw (gutter-w (inc i)) cy (+ header-h 16)
        ok? (seq fs)]
    (emit (format "<path d='M%d,%d h%d l6,6 l-6,6 h-%d z' fill='%s' stroke='%s' stroke-width='1' %s/>"
                  (- gx 2) (- cy 6) (- gw 4) (- gw 4) (if ok? "#e6f0ef" "#fff0e6") (if ok? "#2f5f5c" "#c0392b") (if ok? "" "stroke-dasharray='3,2'")))
    (if ok?
      (label (+ gx (/ gw 2.0)) (+ cy 3) 8 "#2f5f5c" "middle" (str (count fs)))
      (label (+ gx (/ gw 2.0)) (+ cy 3) 8 "#c0392b" "middle" "0"))
    (label (+ gx (/ gw 2.0)) (+ cy 16) 6.5 (if ok? "#2f5f5c" "#c0392b") "middle" (if ok? (str/join " " (map name fs)) "no declared hand-off"))))
(emit (format "<text x='%d' y='%d' font-size='9.5' fill='#3a4a48'>Hand-off strip: between two steps, how many declared fields cross that boundary in the flight's direction (a purple line below is a field crossing the other way).</text>" (panel-x 0) (+ header-h 4)))

;; intra-lane arrows: left margin of the panel, one track per field
(def intra-track (into {} (for [[i _] (map-indexed vector lanes)
                                [t f] (map-indexed vector (sort-by #(:y (positions (writer-of %))) (filter #(= i (lane-of (writer-of %))) intra-fields)))]
                            [f (+ (panel-x i) 8 (* 4 t))])))
(def edge-count (atom {:wired 0 :declared 0 :back 0 :unwritten 0 :constraint 0}))
(doseq [f intra-fields :let [w (writer-of f) tx (intra-track f) {wx :x wy :y} (positions w) wy (+ wy (quot box-h 2))]]
  (emit (format "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='%s' stroke-width='1.2'/>" wx wy tx wy (colors (line-class f))))
  (emit (format "<circle cx='%d' cy='%d' r='2.2' fill='%s'/>" tx wy (colors (line-class f))))
  (doseq [r (distinct (readers-of f)) :let [{rx :x ry :y} (positions r) ry (+ ry (quot box-h 2) 6) cls (field-class f r)]]
    (swap! edge-count update cls inc)
    (emit (format "<path d='M%d,%d V%d H%d' fill='none' stroke='%s' stroke-width='1.2' %s marker-end='url(#a-%s)'><title>%s: %s writes, %s reads (same step)</title></path>"
                  tx wy ry (- rx 1) (colors cls) (if (= cls :declared) "stroke-dasharray='5,3'" "") (name cls) (esc f) (esc w) (esc r))))
  (let [ys (map #(+ (:y (positions %)) (quot box-h 2) 6) (readers-of f)) my (/ (+ wy (apply max (conj ys wy))) 2.0)]
    (label (+ tx 3) my 8 (colors (line-class f)) "middle" (name f) (format "transform='rotate(-90 %.1f %.1f)'" (double (+ tx 3)) (double my)))))

;; unwritten reads: stubs at the box's left
(doseq [[f r] unwritten]
  (let [{:keys [x y]} (positions r)
        k (count (filter #(= (second %) r) (take-while #(not= % [f r]) unwritten)))
        yy (+ y 8 (* k 9))
        cls (if (= :constraint (get-in m [:field-roles f])) :constraint :unwritten)]
    (swap! edge-count update cls inc)
    (emit (format "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='%s' stroke-width='1.2' stroke-dasharray='3,2' marker-end='url(#a-%s)'><title>%s: read by %s, written by no box%s</title></line>"
                  (- x 26) yy (- x 2) yy (colors cls) (name cls) (esc f) (esc r) (if (= cls :constraint) " (the owner's text, a constraint)" "")))
    (label (- x 14) (- yy 3) 6.5 (colors cls) "middle" (name f))))

;; the bus: one line per crossing field, taps from writer and to readers
(doseq [f bus-order :let [y (bus-y f) w (writer-of f) lc (line-class f)
                          xs (map (fn [[k v]] v) (filter (fn [[[ff _ _] _]] (= ff f)) track-x))
                          x1 (apply min xs) x2 (apply max xs)]]
  (emit (format "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='%s' stroke-width='1.4' %s><title>%s: %s writes; read by %s</title></line>"
                x1 y x2 y (colors lc) (if (= lc :declared) "stroke-dasharray='5,3'" "") (esc f) (esc w) (esc (str/join ", " (map name (distinct (readers-of f)))))))
  (label (- x1 6) (+ y 3) 8 (colors lc) "end" (name f))
  ;; writer tap
  (let [tx (track-x [f :exit w]) {bx :x} (positions w) py (port-y w :r f)]
    (emit (format "<path d='M%d,%.1f H%d V%d' fill='none' stroke='%s' stroke-width='1.1' %s/>" (+ bx box-w) (double py) tx y (colors lc) (if (= lc :declared) "stroke-dasharray='5,3'" "")))
    (emit (format "<circle cx='%d' cy='%d' r='2.6' fill='%s'/>" tx y (colors lc))))
  ;; reader taps
  (doseq [r (distinct (readers-of f)) :when (not= (lane-of r) (lane-of w))
          :let [tx (track-x [f :entry r]) {bx :x} (positions r) py (port-y r :l f) cls (field-class f r)]]
    (swap! edge-count update cls inc)
    (emit (format "<circle cx='%d' cy='%d' r='1.8' fill='%s'/>" tx y (colors cls)))
    (emit (format "<path d='M%d,%d V%.1f H%d' fill='none' stroke='%s' stroke-width='1.1' %s marker-end='url(#a-%s)'><title>%s: %s writes, %s reads</title></path>"
                  tx y (double py) (- bx 1) (colors cls) (if (= cls :declared) "stroke-dasharray='5,3'" "") (name cls) (esc f) (esc w) (esc r)))))

;; boxes (drawn last, over the taps)
(doseq [b boxes]
  (let [id (:box/id b) {:keys [x y]} (positions id)
        stroke (cond (not (built? id)) "#d48700" (test? id) "#8a8a84" :else "#2f5f5c")
        fill (cond (not (built? id)) "#fff6e5" (test? id) "#fbfbf8" (siteless? id) "#ececea" :else "#ffffff")
        dash (cond (not (built? id)) "stroke-dasharray='6,3'" (test? id) "stroke-dasharray='2,2'" :else "")
        onTrace (trace-numbers id)
        lbl (or (some-> b :site :var) (some-> b :intended-site :file (str/replace #".*/" "")) (name id))
        sub (or (short-file b) (case id :wc-checker "futon3c proof2a_check.clj (siteless)" :r11-warrants "warrants (siteless)" "no site"))
        ws (str/join " " (map name (:writes b))) rs (str/join " " (map name (:reads b)))]
    (emit (format "<g><title>%s%s\nwrites: %s\nreads: %s</title><rect x='%d' y='%d' width='%d' height='%d' rx='6' fill='%s' stroke='%s' stroke-width='%s' %s/>"
                  (esc id) (if (not (built? id)) " (not built: intended site)" "") (esc (if (seq ws) ws "—")) (esc (if (seq rs) rs "—"))
                  x y box-w box-h fill stroke (if onTrace "2.4" "1.2") dash))
    (emit (format "<text x='%d' y='%d' font-size='10.5' font-weight='600' fill='#1d3a38'>%s</text>" (+ x 8) (+ y 15) (esc (clip lbl 30))))
    (emit (format "<text x='%d' y='%d' font-size='7.5' fill='#5e6b6a'>%s</text>" (+ x 8) (+ y 27) (esc (clip sub 40))))
    (when (seq ws) (emit (format "<text x='%d' y='%d' font-size='7' fill='#2f5f5c'>w: %s</text>" (+ x 8) (+ y 41) (esc (clip ws 44)))))
    (when (seq rs) (emit (format "<text x='%d' y='%d' font-size='7' fill='#6a6a64'>r: %s</text>" (+ x 8) (+ y (if (seq ws) 53 41)) (esc (clip rs 44)))))
    (when onTrace
      (emit (format "<circle cx='%d' cy='%d' r='9' fill='#173b39'/><text x='%d' y='%d' text-anchor='middle' font-size='8' font-weight='700' fill='white'>%s</text>"
                    (- x 1) (- y 1) (- x 1) (+ y 2) (str/join "," onTrace))))
    (emit "</g>")))

;; legend
(let [ly legend-y c @edge-count]
  (emit (format "<text x='24' y='%d' font-size='12' font-weight='700' fill='#274d4a'>Legend</text>" ly))
  (doseq [[k text] (map-indexed vector
                     [(format "solid box: built component at its pinned var (%d)" (count (filter #(and (built? %) (not (test? %))) (keys by-id))))
                      (format "dashed amber box: declared, not built, drawn at its intended site (%d)" (count (remove built? (keys by-id))))
                      (format "dotted box: the step's registered test, a declared reader; it lists its reads and draws no arrows (%d)" (count (filter test? (keys by-id))))
                      (format "grey box: siteless component, exempt from conformance (%d)" (count (filter siteless? (keys by-id))))])]
    (emit (format "<text x='24' y='%d' font-size='10.5' fill='#3a4a48'>%s</text>" (+ ly 18 (* k 14)) (esc text))))
  (doseq [[k [color dash text]] (map-indexed vector
                                  [[(:wired colors) "" (format "line: a field, from its one writer to its readers, both built (%d reader arrows; %d fields cross steps, %d stay inside one)" (:wired c) (count cross-fields) (count intra-fields))]
                                   [(:declared colors) "5,3" (format "dashed amber: declared on a box not yet built (%d)" (:declared c))]
                                   [(:back colors) "" (format "purple: the reader is an earlier step than the writer, the loop closing (%d)" (:back c))]
                                   [(:unwritten colors) "3,2" (format "grey stub: a field read that no box writes (%d); green stub: the owner's text, exogenous (%d)" (:unwritten c) (:constraint c))]])]
    (emit (format "<line x1='700' y1='%d' x2='740' y2='%d' stroke='%s' stroke-width='1.6' %s/>" (+ ly 14 (* k 14)) (+ ly 14 (* k 14)) color (if (seq dash) (str "stroke-dasharray='" dash "'") "")))
    (emit (format "<text x='748' y='%d' font-size='10.5' fill='#3a4a48'>%s</text>" (+ ly 18 (* k 14)) (esc text))))
  (emit (format "<text x='24' y='%d' font-size='10.5' fill='#3a4a48'>Numbered rings: the exemplar trace the map records for %s, in order (%s). Boxes on it have a heavier border.</text>"
                (+ ly 84) (esc (:target (first (:traces m)))) (esc (str/join " > " (map name trace-seq)))))
  (emit (format "<text x='24' y='%d' font-size='10.5' fill='#3a4a48'>What this does not show: the prover reads sites textually, so a field's presence at a var is what is checked, not that the value flows; the eleven code shapes it cannot see are listed in WM-MAP-REPLAY-D. Standing findings (%d) are generic keys the prover cannot scope per box.</text>"
                (+ ly 100) (count (filter #(= :standing (:kind %)) (:expected-findings m)))))
  (emit (format "<text x='24' y='%d' font-size='10.5' fill='#b25a00'>A red chevron in the hand-off strip is a boundary no declared field crosses in the flight's direction; the code hands data across it through the flight record and the store's published view, which no box carries yet.</text>" (+ ly 132)))
  (emit (format "<text x='24' y='%d' font-size='10.5' fill='#b25a00'>A step marked as entered or left by no field is one whose inputs and outputs the map does not yet declare; that is a gap in the map, not in the drawing.</text>" (+ ly 116))))
(emit "</svg>")
(print (str out))
