;; PROOF-2a checker for a hand-worked click record (claude-10, 2026-09-24).
;; Run from /home/joe/code/futon3c:
;;   bb holes/labs/M-futon-seams/exemplar/proof2a_check.clj holes/labs/M-futon-seams/exemplar/click-001.edn
;; Checks W_t (Clause T) and W_0 (Clause 0) of
;; futon2 holes/labs/wm-contract/PROOF-2a-THEOREM-draft-2026-09-24.md (4c502847),
;; then builds each X falsifier's bad extract from the record and confirms the
;; same check fails on it. A falsifier that the check does NOT catch is reported
;; as VACUOUS: that is a defect in this checker, not in the record.
;; Findings (typed, not failures): hand-built construction, missing meets,
;; missing definitions.
(require '[clojure.edn :as edn] '[clojure.set :as set] '[clojure.string :as str])
(load-file (str (.getParent (.getParentFile (java.io.File. (System/getProperty "babashka.file"))))
                "/proto/kernels_lib.clj"))

(def LIB "/home/joe/code/futon3/library")
(defn sha256 [f]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest md (java.nio.file.Files/readAllBytes (.toPath (java.io.File. f))))))))
(defn absent? [x] (and (map? x) (contains? #{:missing-definition :not-measured :not-applicable} (:status x))))

;; ---------------------------------------------------------------- W_t
(defn check-t [tf]
  (let [cons (set (:considered tf)) feas (set (:feasible tf))
        excl (:exclusions tf) ex-t (set (map :target excl))
        cost (:cost-ordering tf) chosen (:chosen tf)]
    (cond-> []
      (empty? cons) (conj "W_t: :considered is empty")
      (not (set/subset? feas cons)) (conj "W_t: a feasible target is not in :considered")
      (not= cons (set/union feas ex-t)) (conj "W_t: :feasible and :exclusions do not partition :considered")
      (seq (set/intersection feas ex-t)) (conj "W_t: a target is both feasible and excluded")
      (some #(not (keyword? (:reason %))) excl) (conj "W_t: an exclusion reason is not typed")
      (and (seq feas) (not (contains? feas chosen))) (conj "W_t: :chosen is not a feasible target")
      (and (empty? feas) (some? chosen)) (conj "W_t: a target is chosen from an empty support")
      (and (empty? feas) (some #(str/blank? (str (:what-would-make-feasible %))) excl))
      (conj "W_t: empty support, but an exclusion omits what would make it feasible")
      (and cost (not (#{:prior :tie-break :not-used} (:entered-as cost))))
      (conj "W_t: :cost-ordering does not say how it entered the choice")
      (and cost (#{:prior :tie-break} (:entered-as cost)) (seq feas)
           (not= chosen (first (filter feas (:ranking cost)))))
      (conj "W_t: chosen target is not the highest-ranked feasible target under the declared cost ordering")
      (not (or (map? (:score-inputs tf)) (absent? (:score-inputs tf))))
      (conj "W_t: :score-inputs neither present nor a typed absence"))))

;; ---------------------------------------------------------------- W_0
(defn cascade-of [cand]
  (let [interp (:interpretations cand)]
    {:patterns (into {} (for [[k v] interp] [k (select-keys v [:guard :produces])]))
     ;; :co-application-edges (:jointly-with, a second parent) are containment
     ;; edges too: in Alexander a unit sits above another exactly when it
     ;; contains it, whichever parent it is.
     :above (mapv (fn [{:keys [above below]}] {:context above :pattern below})
                  (concat (:containment-order cand) (:co-application-edges cand)))
     :initial (set (:initial cand)) :want (set (:want cand))}))

(defn desc-fn [c]
  (let [succ (reduce (fn [m {:keys [context pattern]}] (update m context (fnil conj #{}) pattern)) {} (:above c))]
    (fn [x] (loop [seen #{x} fr #{x}]
              (let [n (set/difference (reduce set/union #{} (map #(get succ % #{}) fr)) seen)]
                (if (empty? n) seen (recur (into seen n) n)))))))

(defn meet-findings [c]
  (let [ids (sort-by str (keys (:patterns c))) desc (desc-fn c)]
    (set (for [a ids b ids :when (neg? (compare (str a) (str b)))
               :let [lb (set/intersection (desc a) (desc b))]
               :when (and (seq lb) (not (some (fn [m] (set/subset? lb (desc m))) lb)))]
           {:pair #{a b}
            :maximal-common (set (remove (fn [x] (some #(and (not= % x) (contains? (desc %) x)) lb)) lb))}))))

(defn acyclic? [c] (let [desc (desc-fn c)
                         succ (reduce (fn [m {:keys [context pattern]}] (update m context (fnil conj #{}) pattern)) {} (:above c))]
                     (not-any? (fn [x] (some #(contains? (desc %) x) (get succ x #{}))) (keys (:patterns c)))))

(defn frontier-conflicts [c]
  (let [pats (:patterns c) anc (ancestors-map (keys pats) (:above c))
        reach (keys (rollout :coapp pats anc nil 0.5 (:initial c) 12))]
    (set (for [s reach :let [f (frontier pats anc s) cs (conflicts pats f)] :when (seq cs)]
           {:state (set s) :pairs (set (map (fn [[p q x]] [p q (set x)]) cs))}))))

(defn- findings-seq
  "Accept a bare vector of findings, or a summary map carrying them
   (:missing for meets, :pairs-by-state/:states for conflicts)."
  [x k]
  (cond (map? x) (get x k []) :else x))
(defn norm-conflicts [xs] (set (map (fn [x] {:state (set (:state x)) :pairs (set (map (fn [[p q t]] [p q (set t)]) (:pairs x)))}) xs)))
(defn norm-meets [xs] (set (map (fn [x] {:pair (set (:pair x)) :maximal-common (set (:maximal-common x))}) xs)))
(defn close? [a b] (and (number? a) (number? b) (< (Math/abs (- (double a) (double b))) 1e-3)))

(defn check-0 [cid cand]
  (let [c (cascade-of cand) pred (:prediction cand) rec (:construction-receipt cand)
        problems (atom []) findings (atom [])
        fail #(swap! problems conj (str "W_0 " cid ": " %))]
    (when (not= :machine-constructed (:kind rec))
      (swap! findings conj {:kind :construction-not-machine :candidate cid :receipt-kind (:kind rec)
                            :note "PROOF-2a W_0 requires :machine-constructed; this clause is not satisfied by this record"}))
    (doseq [[pid i] (:interpretations cand)
            :let [path (get-in i [:receipt :source :path]) f (when path (str "/home/joe/code/" path))]]
      (when (str/blank? (str (:author i))) (fail (str pid " has no interpretation author")))
      (cond (nil? f) (fail (str pid " has no receipt source path"))
            (not (.exists (java.io.File. f))) (fail (str pid " receipt path does not exist: " path))
            (not (str/starts-with? (sha256 f) (str (get-in i [:receipt :source :sha256])))) (fail (str pid " receipt sha256 does not match the file"))
            (str/blank? (str (get-in i [:receipt :source :sha256]))) (fail (str pid " receipt has no sha256"))))
    ;; An empty :initial is legitimate (nothing done yet); an absent one is not.
    (when (empty? (:interpretations cand))
      (fail "candidate carries no :interpretations, so it cannot be recomputed from the record (P_0 requires them in :candidate-derivations)"))
    (when (or (not (contains? cand :initial)) (empty? (:want c))) (fail "candidate has no :initial key or an empty :want, so the kernel cannot be recomputed"))
    (when-not (acyclic? c) (fail "containment order has a cycle"))
    (let [mf (meet-findings c)]
      (when (not= mf (norm-meets (findings-seq (:meet-findings cand) :missing)))
        (fail (str "recorded :meet-findings differ from recomputed " (pr-str mf))))
      (doseq [m mf] (swap! findings conj (assoc m :kind :missing-meet :candidate cid))))
    (when (and (seq (:want c)) (seq (:interpretations cand)) (acyclic? c))
      (let [fc (frontier-conflicts c)]
        (when (not= fc (norm-conflicts (findings-seq (:frontier-conflicts cand) :states)))
          (fail (str "recorded :frontier-conflicts differ from recomputed (X_0(f)) " (pr-str fc)))))
      (if (absent? pred)
        (swap! findings conj {:kind :prediction-absent :candidate cid :status (:status pred)})
        (let [pats (:patterns c) anc (ancestors-map (keys pats) (:above c))
              T (:horizon pred) th (:theta pred)
              co (summary (rollout :coapp pats anc nil th (:initial c) T) (:want c))
              exts (linear-extensions (keys pats) anc 200)
              ls (map #(:p-all-wants (summary (rollout :list pats anc % th (:initial c) T) (:want c))) exts)]
          (when-not (close? (:p-all-wants pred) (:p-all-wants co)) (fail (str "prediction p-all-wants " (:p-all-wants pred) " vs recomputed " (:p-all-wants co))))
          (when-not (close? (:e-wants pred) (:e-wants co)) (fail (str "prediction e-wants " (:e-wants pred) " vs recomputed " (:e-wants co))))
          (let [[lo hi] (get-in pred [:list-spread :p-all-wants])]
            (when-not (and (close? lo (apply min ls)) (close? hi (apply max ls)))
              (fail (str "list spread " [lo hi] " vs recomputed " [(apply min ls) (apply max ls)])))
            (when (not= (get-in pred [:list-spread :extensions]) (count exts))
              (fail (str "list spread extension count " (get-in pred [:list-spread :extensions]) " vs " (count exts))))))))
    {:problems @problems :findings @findings}))

(defn check [rec]
  (let [tf (get-in rec [:decision :target-field])
        cands (get-in rec [:decision :selection-certificate :candidate-derivations])
        r0 (map (fn [[k v]] (check-0 k v)) cands)
        size (count cands)]
    {:problems (vec (concat (check-t tf) (mapcat :problems r0)))
     :findings (vec (concat (mapcat :findings r0)
                            (when (< size 2) [{:kind :candidate-field-size :size size
                                               :note "PROOF-2 W_0 needs at least two semantically different candidates"}])
                            (when (absent? (:score-inputs tf)) [{:kind :score-inputs-absent :status (:status (:score-inputs tf))}])))}))

;; ---------------------------------------------------------------- X falsifiers
(defn first-cand [rec] (first (keys (get-in rec [:decision :selection-certificate :candidate-derivations]))))
(defn falsifiers [rec]
  (let [tf [:decision :target-field] cid (first-cand rec)
        cp [:decision :selection-certificate :candidate-derivations cid]
        cand (get-in rec cp) feas (get-in rec (conj tf :feasible))]
    (cond-> []
      (> (count feas) 0)
      (conj ["X_t(a) drop a feasible target, keep the choice"
             (update-in rec (conj tf :considered) (fn [xs] (vec (remove #{(last feas)} xs))))]
            ["X_t(b) abstain with a non-empty feasible support" (assoc-in rec (conj tf :chosen) nil)])
      true
      (conj ["X_t(c) empty support, what-would-make-feasible stripped"
             (-> rec (assoc-in (conj tf :feasible) []) (assoc-in (conj tf :chosen) nil)
                 (update-in (conj tf :exclusions)
                            (fn [ex] (vec (concat (map #(dissoc % :what-would-make-feasible) ex)
                                                  (map (fn [t] {:target t :reason :injected}) feas))))))])
      (get-in rec (conj tf :cost-ordering))
      (conj ["X_t(cost) chosen target is not the top feasible target of the ranking it claims to use"
             (-> rec (assoc-in (conj tf :cost-ordering :entered-as) :prior)
                 (assoc-in (conj tf :cost-ordering :ranking)
                           (vec (reverse (get-in rec (conj tf :cost-ordering :ranking))))))])
      (seq (:interpretations cand))
      (conj ["X_0(receipt) one receipt sha256 altered"
             (update-in rec (conj cp :interpretations)
                        (fn [m] (let [k (first (keys m))] (assoc-in m [k :receipt :source :sha256] "0000000000000000"))))])
      (seq (findings-seq (:frontier-conflicts cand) :states))
      (conj ["X_0(f) conflicting frontier recorded without its flag" (assoc-in rec (conj cp :frontier-conflicts) [])])
      (seq (findings-seq (:meet-findings cand) :missing))
      (conj ["X_0(meet) a missing meet left out of the record" (assoc-in rec (conj cp :meet-findings) [])])
      (number? (get-in cand [:prediction :p-all-wants]))
      (conj ["X_0(prediction) predicted p(all wants) altered"
             (update-in rec (conj cp :prediction :p-all-wants) #(+ % 0.1))])
      (seq (:containment-order cand))
      (conj ["X_0(cycle) a reversed edge added to the containment order"
             (update-in rec (conj cp :containment-order)
                        (fn [es] (let [{:keys [above below]} (first es)] (conj (vec es) {:above below :below above}))))]))))

(let [f (first *command-line-args*)
      rec (edn/read-string (slurp f))
      {:keys [problems findings]} (check rec)]
  (println "PROOF-2a check:" f)
  (println (if (empty? problems) "  W_t, W_0: PASS" (str "  FAIL (" (count problems) ")")))
  (doseq [p problems] (println "   -" p))
  (println "  findings (typed, not failures):" (count findings))
  (doseq [x findings] (println "   -" (pr-str x)))
  (println "  falsifiers (each bad extract must fail):")
  (let [vac (atom 0)]
    (doseq [[label bad] (falsifiers rec)]
      (let [caught (seq (:problems (check bad)))]
        (when-not caught (swap! vac inc))
        (println "   " (if caught "caught " "VACUOUS") label)))
    (System/exit (if (or (seq problems) (pos? @vac)) 1 0))))
