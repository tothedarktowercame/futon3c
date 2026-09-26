#!/usr/bin/env bb
;; wm_org_layer.bb -- the organisation layer of the flight's map: which box's
;; site CALLS which box's site, in what order, under what condition. Read-only.
;;
;;   bb holes/labs/M-wm-wiring/spike/wm_org_layer.bb [MAP-REV] [FUTON2-REV]
;;
;; Reads the map (futon3c holes/labs/M-wm-wiring/wm-flight-wiring.edn) at
;; MAP-REV (default HEAD) and every site file from git: futon2 files at
;; FUTON2-REV (default futon2 HEAD), futon3c files at MAP-REV. Writes
;; holes/labs/M-wm-wiring/wm-org-layer.edn (env WM_ORG_OUT names another output file).
;; wm_wiring_svg.bb runs this script for the figure's strip with WM_ORG_OUT set to a
;; temp file, so a figure run does not rewrite the committed layer; run this script
;; directly (no WM_ORG_OUT) to regenerate wm-org-layer.edn.
;; ORG-PATHS-I: a call carries every distinct path to its callee, not the first
;; found: the first is the call's own :site/:via/:conditional/:conditions-along-path
;; (unchanged), the others are :other-paths, deduplicated on :via and the conditions.
;;
;; Method (textual, rewrite-clj; nothing is evaluated):
;; - A box's site is a top-level def of :var in :file. Every symbol in that
;;   form is resolved through the file's ns aliases/refers (or its own ns). A
;;   symbol naming another box's var is a DIRECT call. A symbol naming a
;;   non-box var in futon2/futon3c is followed into that var's own form
;;   (depth <= 6), so a box reached through plain helpers is still found; the
;;   helpers are recorded as :via.
;; - HAND-OFFS: a map entry `:k v` in a box's form, where k ends in `-fn` (the
;;   codebase's convention for a function handed over) or v is a bare var
;;   reference, and v is (or is a local bound to, or an or/when over) a call
;;   of / reference to a box var or a futon factory var, registers key k; a call `(k ...)` of an unresolved local named k in another box's
;;   form is a call of it (:via [:handoff k]). This is how flight/run! calls
;;   the runner's step closures the driver hands it.
;; - :order is the rank of the call's first position in the caller's form;
;;   :conditional is the innermost enclosing if/when/cond/case/and/... test
;;   (text), nil when unconditional.
;; - :constructs true marks a call that builds a closure the caller hands off
;;   (the driver building run!'s steps); the receiver's invocation is its own
;;   call, :via [:handoff k ...].
;; - The residue the text cannot see (HTTP, cross-JVM, siteless boxes) is a
;;   hand-checked table below, each entry with its evidence, marked :hand true.
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.java.shell :refer [sh]]
         '[rewrite-clj.zip :as z] '[rewrite-clj.node :as n] '[clojure.pprint :as pp])

(def home (System/getProperty "user.home"))
(def f3c (str home "/code/futon3c"))
(def f2 (str home "/code/futon2"))
(defn die [& m] (binding [*out* *err*] (apply println "wm_org_layer:" m)) (System/exit 1))
(defn git [dir & args] (let [{:keys [exit out err]} (apply sh "git" "-C" dir args)] (when-not (zero? exit) (die "git" args err)) out))
(def map-rev (str/trim (git f3c "rev-parse" "--short=8" (or (first *command-line-args*) "HEAD"))))
(def f2-rev (str/trim (git f2 "rev-parse" "--short=8" (or (second *command-line-args*) "HEAD"))))
(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def m (edn/read-string (git f3c "show" (str map-rev ":" map-path))))
(def boxes (:boxes m))
(when (empty? boxes) (die "map has no boxes"))

(defn repo-file [path] ;; "futon2/src/..." -> [repo-dir rev rel]
  (let [[repo & more] (str/split path #"/")]
    (case repo "futon2" [f2 f2-rev (str/join "/" more)] "futon3c" [f3c map-rev (str/join "/" more)] nil)))
(def read-file
  (memoize (fn [path] (when-let [[dir rev rel] (repo-file path)]
                        (let [{:keys [exit out]} (sh "git" "-C" dir "show" (str rev ":" rel))] (when (zero? exit) out))))))

(defn node-str [loc] (let [s (str/replace (n/string (z/node loc)) #"\s+" " ")] (if (> (count s) 140) (str (subs s 0 137) "...") s)))
;; --- per-file index: ns, aliases, refers, top-level defs ---
(def def-heads #{"defn" "defn-" "def" "defmacro" "defmulti" "defonce"})
(defn parse-file [path]
  (when-let [text (read-file path)]
    (let [root (z/of-string text {:track-position? true})
          tops (take-while some? (iterate z/right root))
          ns-form (first (filter #(and (= :list (z/tag %)) (= 'ns (z/sexpr (z/down %)))) tops))
          ns-sym (some-> ns-form z/down z/right z/sexpr)
          reqs (when ns-form (->> (z/sexpr ns-form) (filter seq?) (filter #(= :require (first %))) (mapcat rest)))
          libspec (fn [r] (when (vector? r) (let [[lib & opts] r o (apply hash-map (if (odd? (count opts)) (butlast opts) opts))] [lib o])))
          specs (keep libspec reqs)
          defs (into {} (for [t tops :when (= :list (z/tag t))
                              :let [h (z/down t)] :when (and h (= :token (z/tag h)) (def-heads (str (z/sexpr h))))
                              :let [nm (some-> h z/right)] :when (and nm (symbol? (z/sexpr nm)))]
                          [(name (z/sexpr nm)) t]))
          ;; defmethod bodies, by multimethod name, with their dispatch value
          methods (reduce (fn [acc t]
                            (let [h (z/down t)]
                              (if (and (= :list (z/tag t)) h (= :token (z/tag h)) (= "defmethod" (str (z/sexpr h))))
                                (let [nm (z/right h) dv (some-> nm z/right)]
                                  (update acc (name (z/sexpr nm)) (fnil conj []) [t (some-> dv node-str)]))
                                acc)))
                          {} tops)]
      {:path path :ns (str ns-sym) :defs defs :methods methods
       :aliases (into {} (for [[lib o] specs :when (:as o)] [(str (:as o)) (str lib)]))
       :refers (into {} (for [[lib o] specs, r (let [x (:refer o)] (when (sequential? x) x))] [(str r) (str lib)]))})))
(def file-index (memoize parse-file))
(defn ns->path [ns-str]
  (let [rel (str (-> ns-str (str/replace "-" "_") (str/replace "." "/")) ".clj")]
    (first (filter read-file (for [[repo roots] [["futon2" ["src" "scripts" "test"]] ["futon3c" ["src" "dev"]]] r roots] (str repo "/" r "/" rel))))))
(def ns-index (memoize (fn [ns-str] (some-> (ns->path ns-str) file-index))))

;; --- boxes by resolved var ---
(def site-of (fn [b] (:site b)))
(def box-ns (into {} (for [b boxes :let [s (site-of b)] :when (:var s) :let [fi (file-index (:file s))]]
                       [(:box/id b) (if fi (:ns fi) (die "box" (:box/id b) "site unreadable:" (:file s)))])))
(def var->boxes (reduce (fn [acc b] (if-let [ns (box-ns (:box/id b))] (update acc [ns (:var (site-of b))] (fnil conj []) (:box/id b)) acc)) {} boxes))
(doseq [b boxes :let [s (site-of b)] :when (:var s)]
  (when-not (get-in (file-index (:file s)) [:defs (:var s)]) (die "box" (:box/id b) "var" (:var s) "not defined at top level of" (:file s))))

(defn resolve-sym [fi sym]
  (let [s (str sym)]
    (if-let [i (str/index-of s "/")]
      (let [a (subs s 0 i) v (subs s (inc i))] (when-let [ns (get (:aliases fi) a (when (ns-index a) a))] [ns v]))
      (cond (get (:refers fi) s) [(get (:refers fi) s) s]
            (get-in fi [:defs s]) [(:ns fi) s]))))

(def cond-heads #{"if" "if-not" "when" "when-not" "if-let" "when-let" "if-some" "when-some" "cond" "cond->" "cond->>" "case" "and" "or" "some->" "some->>" "when-first"})
(defn conditional-of
  "Innermost enclosing conditional of LOC within ROOT: the test text, or nil."
  [loc]
  (loop [child loc]
    (let [p (z/up child)]
      (when (and p (not= :forms (z/tag p)))
        (let [h (z/down p) hs (when (and h (= :token (z/tag h))) (str (z/sexpr h)))
              kids (vec (take-while some? (iterate z/right h)))
              idx (count (take-while #(not= (z/position %) (z/position child)) kids))]
          (if (cond-heads hs)
            (cond
              (#{"if" "if-not" "when" "when-not" "if-let" "when-let" "if-some" "when-some" "when-first"} hs)
              (if (<= idx 1) (recur p)
                  (str "(" hs " " (node-str (nth kids 1)) ")"
                       (when (#{"if" "if-not" "if-let" "if-some"} hs) (if (= idx 2) " then" " else"))))
              (= "cond" hs) (if (odd? idx) (recur p) (str "(cond " (node-str (nth kids (dec idx))) ")"))
              (#{"cond->" "cond->>"} hs) (if (or (<= idx 1) (even? idx)) (recur p) (str "(" hs " " (node-str (nth kids (dec idx))) ")"))
              (= "case" hs) (if (<= idx 1) (recur p) (str "(case " (node-str (nth kids 1)) (when (even? idx) (str " " (node-str (nth kids (dec idx))))) ")"))
              :else (if (<= idx 1) (recur p) (str "(" hs " ... after " (str/join " " (map node-str (subvec kids 1 idx))) ")")))
            (recur p)))))))
(defn line-of [loc] (first (z/position loc)))

;; every symbol token in a def form, with its loc
(defn symbol-locs [form]
  (let [end (z/right form)]
    (loop [loc (z/next (z/down form)) acc []]
      (if (or (nil? loc) (z/end? loc) (and end (= (z/position loc) (z/position end))) (not (let [[r c] (z/position loc) [r0 c0] (z/position form)] (or (> r r0) (and (= r r0) (>= c c0))))))
        acc
        (recur (z/next loc) (if (and (= :token (z/tag loc)) (symbol? (z/sexpr loc))) (conj acc loc) acc))))))
(defn in-form? [form loc]
  (let [top (loop [l loc] (let [p (z/up l)] (if (or (nil? p) (= :forms (z/tag p))) l (recur p))))]
    (= (z/position top) (z/position form))))
(defn symbols-of [form] (filter #(in-form? form %) (symbol-locs form)))

;; --- hand-off keys: `:k v` map entries whose v is (bound to) a box var call/ref ---
(defn let-bindings [form]
  (into {} (for [loc (filter #(= :vector (z/tag %)) (take-while #(and % (not (z/end? %)) (in-form? form %)) (iterate z/next (z/down form))))
                 :let [p (z/up loc) h (some-> p z/down)] :when (and h (= :token (z/tag h)) (#{"let" "let*" "loop" "when-let" "if-let"} (str (z/sexpr h))))
                 [s v] (partition 2 (take-while some? (iterate z/right (z/down loc))))
                 :when (and (= :token (z/tag s)) (symbol? (z/sexpr s)))]
             [(str (z/sexpr s)) v])))
(defn value-target
  "What a hand-off value V is: {:boxes [...]} when it is (or is bound to) a
  call of / reference to a box var, {:var [ns v]} when a futon non-box var
  (a factory whose closure does the work). Looks through or/and/if/when."
  [fi binds v]
  (let [head (cond (= :list (z/tag v)) (z/down v) :else v)]
    (when (and head (= :token (z/tag head)) (symbol? (z/sexpr head)))
      (let [s (str (z/sexpr head))]
        (if (#{"or" "and" "if" "when" "if-not" "when-not"} s)
          (first (keep #(value-target fi binds %) (rest (take-while some? (iterate z/right head)))))
          (let [r (resolve-sym fi (z/sexpr head))]
            (cond (var->boxes r) {:boxes (var->boxes r)}
                  (and r (re-find #"^futon" (first r))) {:var r}
                  (get binds s) (value-target fi {} (get binds s)))))))))
(defn handoffs-in [fi form]
  (let [binds (let-bindings form)]
    (for [loc (take-while #(and % (not (z/end? %)) (in-form? form %)) (iterate z/next (z/down form)))
          :when (= :map (z/tag loc))
          [k v] (partition 2 (take-while some? (iterate z/right (z/down loc))))
          :when (and (= :token (z/tag k)) (keyword? (z/sexpr k))
                     ;; a function handed over, not data: the codebase's
                     ;; `:*-fn` key convention, or a bare var reference
                     (or (str/ends-with? (name (z/sexpr k)) "-fn") (= :token (z/tag v))))
          ;; a bare value counts only as a var reference, never through a local
          :let [t (value-target fi (if (str/ends-with? (name (z/sexpr k)) "-fn") binds {}) v)] :when t
          h (if (:boxes t) (map (fn [b] {:box b}) (:boxes t)) [{:var (:var t)}])]
      [(name (z/sexpr k)) (assoc h :line (line-of k) :file (:path fi))])))

;; --- direct and followed callees of a var ---
(defn callees-of
  "[[ns var] loc fi method-cond] for every resolvable futon var symbol in the
  def of NS/VAR and in each of its defmethods (method-cond names the dispatch
  value, a condition on the path)."
  [ns v]
  (when-let [fi (ns-index ns)]
    (concat
     (when-let [form (get-in fi [:defs v])]
       (for [loc (symbols-of form) :let [r (resolve-sym fi (z/sexpr loc))] :when (and r (not= r [ns v]))] [r loc fi nil]))
     (for [[form dv] (get-in fi [:methods v])
           loc (symbols-of form) :let [r (resolve-sym fi (z/sexpr loc))] :when (and r (not= r [ns v]))]
       [r loc fi (str "(defmethod " v " " dv ")")]))))
(def box-vars (set (keys var->boxes)))
(declare handoff-table handed-off-by)
(defn box-calls
  "Calls from the var NS/V (a box site) to box vars, followed through non-box
  vars up to DEPTH. A factory this var hands off (`:k (factory ...)` in its
  own form) is not followed from here: its closure runs where it is called.
  Returns EVERY distinct path to each callee, not the first found:
  [{:callee [ns var] :loc first-loc-in-caller :via [...] :conds [...]}], the
  paths of one callee in the order the walk found them (the first is the one
  the layer has always kept), deduplicated on :via and :conds. A callee
  reachable through two branches of one caller (an ordinary click and a
  commissioned one reaching the same click!) is reached under each branch's
  conditions, and a consumer that takes the disjunction sees both."
  [ns v]
  (let [out (volatile! {})
        factories (handed-off-by ns v)]
    (letfn [(walk [cur via conds first-loc depth seen]
              (doseq [[r loc fi mc] (callees-of (first cur) (second cur))
                      :let [fl (or first-loc [loc fi]) c (conditional-of loc) conds' (cond-> conds mc (conj mc) c (conj c))]]
                (cond (box-vars r) (vswap! out update r
                                        (fn [ps] (if (some #(and (= via (:via %)) (= conds' (:conds %))) ps)
                                                   ps
                                                   (conj (or ps []) {:callee r :loc fl :via via :conds conds'}))))
                      (and (< depth 6) (not (seen r)) (re-find #"^futon" (first r)) (not (factories r)))
                      (walk r (conj via (str (first r) "/" (second r))) conds' fl (inc depth) (conj seen r)))))]
      (walk [ns v] [] [] nil 0 #{[ns v]}))
    (vec (mapcat identity (vals @out)))))

(def handoff-table
  (delay (reduce (fn [acc b]
            (let [s (site-of b)]
              (if-let [form (and (:var s) (get-in (file-index (:file s)) [:defs (:var s)]))]
                (reduce (fn [a [k h]] (update a k (fnil conj #{}) h)) acc (handoffs-in (file-index (:file s)) form))
                acc)))
          {} boxes)))
(defn handed-off-by [ns v]
  (if-let [fi (ns-index ns)]
    (if-let [form (get-in fi [:defs v])]
      (set (keep (comp :var second) (handoffs-in fi form)))
      #{})
    #{}))
(defn handoff-calls [ns v]
  (when-let [fi (ns-index ns)]
    (when-let [form (get-in fi [:defs v])]
      (for [loc (symbols-of form)
            :let [s (str (z/sexpr loc)) p (z/up loc)]
            :when (and (@handoff-table s) (nil? (resolve-sym fi (z/sexpr loc)))
                       p (#{:list :fn} (z/tag p)) (= (z/position (z/down p)) (z/position loc)))
            h (@handoff-table s)
            :let [hv [:handoff (keyword s) (str (:file h) ":" (:line h))]
                  c0 (if-let [c (conditional-of loc)] [c] [])]
            call (if (:box h)
                   [{:callee-box (:box h) :loc [loc fi] :via [hv] :conds c0}]
                   (for [c (box-calls (first (:var h)) (second (:var h))), cb (var->boxes (:callee c))]
                     {:callee-box cb :loc [loc fi] :via (into [hv (str/join "/" (:var h))] (:via c)) :conds (into c0 (:conds c))}))]
        call))))

;; --- hand-checked residue: calls the text cannot see ---
(def hand
  ;; :checks are re-verified on every run: each names a form (a var, or :any
  ;; top-level form of the file) that must still contain every string; a
  ;; check that fails stops the script (the edge's evidence moved).
  [{:caller :flight-click :callee :click-start :order 1 :conditional nil
    :checks [{:file "futon2/src/futon2/aif/flight_runner.clj" :var "http-click-fn" :contains ["/api/alpha/wm/click"]}
             {:file "futon3c/src/futon3c/transport/http.clj" :any true :contains ["\"/api/alpha/wm/click\" uri" "handle-wm-click-start request"]}]
    :evidence "http-click-fn POSTs /api/alpha/wm/click (flight_runner.clj:495-511, cross-JVM HTTP); futon3c transport/http.clj:9295 routes the POST to handle-wm-click-start"}
   {:caller :dispatch :callee :clock-in :order 1 :conditional nil
    :checks [{:file "futon2/src/futon2/aif/full_loop_runner.clj" :var "dispatch!" :contains ["/api/alpha/bell" ":mission-id"]}
             {:file "futon3c/src/futon3c/agency/clock_decision.clj" :any true :contains ["lineage/persist-clock!"]}]
    :evidence "full_loop_runner/dispatch! POSTs /api/alpha/bell with :mission-id (cross-JVM HTTP); in futon3c, agency/clock_decision.clj:293 calls clock_lineage/persist-clock!. The hop from the bell to the clock decision is not traced here: this edge is the map's :traces hop, not a read of the path"}
   {:caller :r7-flight-call :callee :wc-checker :order 1 :conditional nil
    :checks [{:file "futon2/src/futon2/aif/flight_runner.clj" :var "wc-verdict-fn" :contains ["proof2a_check"]}
             {:file "futon3c/holes/labs/M-futon-seams/exemplar/proof2a_check.clj" :any true :contains ["--wc"]}]
    :evidence "siteless (futon3c holes/labs/M-futon-seams/exemplar/proof2a_check.clj); wc-verdict-fn runs it as a bb subprocess (flight_runner.clj:915-922, :checker opt; the map's :traces hop r7-flight-call -> wc-checker)"}])

;; re-verify the hand-checked edges' evidence
(defn form-texts [path]
  (when-let [text (read-file path)]
    (for [t (take-while some? (iterate z/right (z/of-string text)))] (n/string (z/node t)))))
(defn check-ok? [{:keys [file var any contains]}]
  (let [forms (cond var (some-> (file-index file) (get-in [:defs var]) z/node n/string vector)
                    any (form-texts file))]
    (some (fn [t] (every? #(str/includes? t %) contains)) forms)))
(doseq [h hand, c (:checks h)]
  (when-not (check-ok? c)
    (die "hand-checked edge" (:caller h) "->" (:callee h) "lost its evidence:" (pr-str c))))

;; --- assemble ---
(def box-by-id (into {} (map (juxt :box/id identity) boxes)))
(def textual-calls
  (vec
   (sort-by (juxt (comp str :caller) :order (comp str :callee))
    (concat
     (for [b boxes :let [s (site-of b)] :when (and (:var s) (= :component (:box/kind b)))
           :let [ns (box-ns (:box/id b))
                 ;; boxes sharing the caller's var are the same code, not its callees
                 all (let [same (set (var->boxes [ns (:var s)]))]
                       (remove #(same (:callee-box %)) (concat (for [c (box-calls ns (:var s)), cb (var->boxes (:callee c))] (assoc c :callee-box cb))
                                                               (handoff-calls ns (:var s)))))
                 ranked (sort-by (fn [c] (z/position (first (:loc c)))) all)
                 ;; boxes this box hands over as `:*-fn` values: it BUILDS them
                 ;; (calls a factory); the receiver invokes them
                 built (set (keep (comp :box second) (handoffs-in (file-index (:file s)) (get-in (file-index (:file s)) [:defs (:var s)]))))
                 order (into {} (map-indexed (fn [i cb] [cb (inc i)]) (distinct (map :callee-box ranked))))]
           cs (vals (group-by :callee-box ranked))
           :let [c (first cs) [loc fi] (:loc c)
                 ;; the other distinct paths to the same callee (same :via and
                 ;; :conds as the kept one, or as each other, are one path)
                 others (loop [seen #{[(:via c) (:conds c)]} out [] [x & xs] (rest cs)]
                          (cond (nil? x) out
                                (seen [(:via x) (:conds x)]) (recur seen out xs)
                                :else (recur (conj seen [(:via x) (:conds x)]) (conj out x) xs)))]]
       (cond-> {:caller (:box/id b) :callee (:callee-box c)
                :site (str (:path fi) ":" (line-of loc))
                :order (order (:callee-box c))
                :conditional (first (:conds c))}
         (seq (:via c)) (assoc :via (:via c))
         (built (:callee-box c)) (assoc :constructs true)
         (< 1 (count (:conds c))) (assoc :conditions-along-path (:conds c))
         (seq others) (assoc :other-paths
                             (mapv (fn [x] (let [[l f] (:loc x)]
                                             (cond-> {:site (str (:path f) ":" (line-of l))
                                                      :conditional (first (:conds x))}
                                               (seq (:via x)) (assoc :via (:via x))
                                               (< 1 (count (:conds x))) (assoc :conditions-along-path (:conds x)))))
                                   others))))
     []))))
;; hand-checked calls go after the caller's textual calls (their position in
;; the body is not a text position)
(def calls
  (vec (sort-by (juxt (comp str :caller) :order (comp str :callee))
                (concat textual-calls
                        (for [h hand :let [n (count (filter #(= (:caller h) (:caller %)) textual-calls))]]
                          (assoc h :hand true :order (inc n)))))))

(def callee-set (set (map :callee calls)))
(def component-ids (map :box/id (filter #(= :component (:box/kind %)) boxes)))
(def roots (vec (for [id component-ids :when (and (not (callee-set id)) (some #(= id (:caller %)) calls))] id)))
(def unplaced (vec (for [id component-ids :when (and (not (callee-set id)) (not (some #(= id (:caller %)) calls)))] id)))
(def out-path (or (System/getenv "WM_ORG_OUT") (str f3c "/holes/labs/M-wm-wiring/wm-org-layer.edn")))
(spit (str out-path ".tmp")
      (with-out-str
        (println ";; GENERATED by holes/labs/M-wm-wiring/spike/wm_org_layer.bb -- do not edit; method in the script header.")
        (pp/pprint {:map-rev map-rev :futon2-rev f2-rev
                    :counts {:boxes (count boxes) :components (count component-ids)
                             :tests (count (filter #(= :test (:box/kind %)) boxes))
                             :components-with-caller (count (filter callee-set component-ids))
                             :roots (count roots) :unplaced (count unplaced)
                             :calls (count calls) :hand (count (filter :hand calls))}
                    :calls calls
                    :roots roots
                    :unplaced unplaced
                    :tests-not-in-call-tree (vec (map :box/id (filter #(= :test (:box/kind %)) boxes)))})))
(.renameTo (java.io.File. (str out-path ".tmp")) (java.io.File. out-path))
(prn {:map-rev map-rev :futon2-rev f2-rev :components (count component-ids) :with-caller (count (filter callee-set component-ids))
      :roots roots :unplaced unplaced :calls (count calls)})
