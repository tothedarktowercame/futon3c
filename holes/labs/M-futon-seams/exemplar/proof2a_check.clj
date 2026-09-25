;; PROOF-2a checker for a hand-worked click record (claude-10, 2026-09-24).
;; Run from /home/joe/code/futon3c:
;;   bb holes/labs/M-futon-seams/exemplar/proof2a_check.clj holes/labs/M-futon-seams/exemplar/click-001.edn [enactment.edn [--wc]]
;; With --wc the run exits after W_c: 0 when the positive control passes and
;; every W_c bad case is caught, 1 otherwise (the W_c checker's own test).
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
;; Defined by the load-file above; declared so a linter can see them (a def
;; without a value leaves an existing root binding alone).
(declare ancestors-map rollout frontier conflicts summary linear-extensions)

(def LIB "/home/joe/code/futon3/library")
;; PROOF-2a W_0 requires :machine-constructed. A hand-built candidate fails W_0.
;; The falsifiers bind this false so each bad extract tests the condition it
;; names, not the construction condition every hand-built record already fails.
(def ^:dynamic *require-machine-construction* true)
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

;; A hand-built candidate meets W_0's construction condition when the War
;; Machine's constructor, replayed on exactly its recorded interpretations and
;; initial state, returns the same pattern set in an order that is a linear
;; extension of the recorded containment order (construct-replay.edn, written
;; by construct_replay.clj). The containment edges themselves are authored, not
;; constructed: the constructor returns a precedence, not an order.
(def replay
  (let [f (java.io.File. (str (.getParentFile (java.io.File. (System/getProperty "babashka.file"))) "/construct-replay.edn"))]
    (when (.exists f) (edn/read-string (slurp f)))))
(defn replayed? [cid]
  (boolean (some (fn [r] (and (= cid (:candidate r)) (= :constructed (:constructor-status r))
                              (some #(and (:same-pattern-set? %) (:respects-containment-order? %)) (:constructed r))))
                 (:replays replay))))

(defn check-0 [cid cand]
  (let [c (cascade-of cand) pred (:prediction cand) rec (:construction-receipt cand)
        problems (atom []) findings (atom [])
        fail #(swap! problems conj (str "W_0 " cid ": " %))]
    ;; The receipt's :kind is a claim; the replay record is the evidence. A
    ;; receipt saying :machine-constructed without a reproducing replay fails.
    (when-not (replayed? cid)
      (if *require-machine-construction*
        (fail (str "construction receipt is " (:kind rec) "; no replay of the constructor on the recorded interpretations reproduces this candidate (PROOF-2a W_0)"))
        (swap! findings conj {:kind :construction-not-machine :candidate cid :receipt-kind (:kind rec)})))
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

;; ---------------------------------------------------------------- W_c
;; Clause C (enactment conformance): the change that was made IS the chosen
;; candidate. Every pattern of the chosen candidate has a successful attempt
;; whose check names what observes its produced token; every attempt names a
;; pattern of that candidate; deviations are typed; and (G_c, X_c(d)) a
;; successful attempt names G_c's pass as its check. Checked against an
;; enactment record (click-001-enactment.edn form).
;;
;; G_c is futon2.aif.grain-gate, loaded from futon2's checkout and RE-RUN here
;; on the enactment's :grain (the grain the candidate chose) against the grain
;; the gated attempt declares, so a recorded pass is evidence only when the
;; gate still passes on the files as they are. A gated attempt's check is
;; {:kind :grain-gate :repo "<dir under /home/joe/code>" :attempt-grain {...}
;;  :result {:status :pass}}.
(load-file "/home/joe/code/futon2/src/futon2/aif/grain_gate.clj")
(def grain-gate (resolve 'futon2.aif.grain-gate/grain-gate))

(defn- gc-failures [enact ok]
  (let [gated (filter #(= :grain-gate (get-in % [:check :kind])) ok)]
    (if (empty? gated)
      ["W_c: no successful attempt names a G_c pass (X_c(d)): the grain attempt's check is not grain-gate"]
      (vec (for [a gated
                 :let [chk (:check a)
                       run (grain-gate {:grain (:grain enact)} {:grain (:attempt-grain chk)}
                                       (str "/home/joe/code/" (:repo chk)))]
                 f [(when-not (= :pass (:status run))
                      (str "W_c: G_c refuses attempt " (:n a) " at " (:pattern a) ": "
                           (:reason run) " (" (:detail run) ")"))
                    (when (and (= :pass (:status run)) (not= :pass (get-in chk [:result :status])))
                      (str "W_c: attempt " (:n a) " records G_c " (pr-str (:result chk))
                           " but the gate passes on re-run"))]
                 :when f]
             f)))))

(defn check-c
  "W_c's failures as a vector (empty is a pass), or, when nothing else fails
  and the click's [:decision :selection-law] carries no :candidate id, the
  typed {:status :join-unverifiable :failures []}: P_c joins the enactment to the
  decision by that id, so without it the enacted-vs-selected test cannot be
  made and the record is neither a pass nor a failure."
  [rec enact]
  (let [cid (:candidate enact)
        cand (get-in rec [:decision :selection-certificate :candidate-derivations cid])
        selected (get-in rec [:decision :selection-law :candidate])
        pats (set (keys (:interpretations cand)))
        atts (:attempts enact)
        ok (filter :success atts)
        failures
    (cond-> []
      (nil? cand) (conj (str "W_c: enactment names candidate " cid ", which the click did not record"))
      (and (some? selected) (not= cid selected))
      (conj (str "W_c: enactment candidate " cid " differs from the click's selected candidate " selected))
      (some #(not (contains? pats (:pattern %))) atts)
      (conj (str "W_c: attempts at patterns outside the chosen candidate: "
                 (vec (remove pats (map :pattern atts)))))
      (seq (remove (set (map :pattern ok)) pats))
      (conj (str "W_c: chosen patterns with no successful attempt: " (vec (sort-by str (remove (set (map :pattern ok)) pats)))))
      (some #(nil? (:check %)) ok)
      (conj (str "W_c: successful attempts with no check: " (vec (map :pattern (filter #(nil? (:check %)) ok)))))
      (some #(not (contains? (set (get-in cand [:interpretations (:pattern %) :produces])) (:produced %))) ok)
      (conj "W_c: an attempt claims a token its pattern does not produce")
      (some #(not (keyword? (:kind %))) (get-in enact [:conformance :deviations]))
      (conj "W_c: a deviation is not typed")
      true (into (gc-failures enact ok)))]
    ;; a failure is decidable without the join; only a record that fails
    ;; nothing else is left unverifiable by the missing id
    (if (or (some? selected) (seq failures))
      failures
      {:status :join-unverifiable
       :reason "[:decision :selection-law] carries no :candidate id, so the enactment cannot be joined to the selected candidate"
       :failures failures})))

(def wc-defects (atom 0))
(defn wc-failures [verdict] (if (map? verdict) (:failures verdict) verdict))

(defn gated
  "ENACT with attempt N's check replaced by a grain-gate check at ATTEMPT-GRAIN
  (the positive control and its bad cases are built from the recorded
  enactment this way; the record itself is never rewritten)."
  [enact n pattern attempt-grain result]
  (update enact :attempts
          (fn [as] (mapv #(if (and (= n (:n %)) (= pattern (:pattern %)))
                            (assoc % :check {:kind :grain-gate :repo "futon3c"
                                             :attempt-grain attempt-grain :result result})
                            %)
                         as))))

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
      ef (second *command-line-args*)
      rec (edn/read-string (slurp f))
      {:keys [problems findings]} (check rec)]
  (println "PROOF-2a check:" f)
  (println (if (empty? problems) "  W_t, W_0: PASS" (str "  FAIL (" (count problems) ")")))
  (let [rest-problems (binding [*require-machine-construction* false] (:problems (check rec)))]
    (println "  with the construction condition set aside:" (if (empty? rest-problems) "PASS" (str "FAIL (" (count rest-problems) ")"))))
  (doseq [p problems] (println "   -" p))
  (println "  findings (typed, not failures):" (count findings))
  (doseq [x findings] (println "   -" (pr-str x)))
  (when ef
    (let [enact (edn/read-string (slurp ef))
          pc (check-c rec enact)
          show (fn [v] (cond (map? v) (str "    " (:status v) " (neither pass nor fail): " (:reason v)
                                           (when (seq (:failures v)) (str "; and FAIL (" (count (:failures v)) ")")))
                             (empty? v) "    PASS"
                             :else (str "    FAIL (" (count v) ")")))
          grain-n 3 grain-p :cascade-construction/choose-the-grain-where-state-lives
          outcome-grain (:grain (edn/read-string (slurp (str (.getParent (.getAbsoluteFile (java.io.File. ef))) "/click-001-outcome.edn"))))
          ;; positive control: the recorded enactment with its grain attempt
          ;; carrying a G_c pass at the candidate's grain, and the click's
          ;; selection law naming the enacted candidate. It must PASS, so each
          ;; bad case below fails for the condition it names.
          good-e (gated enact grain-n grain-p (:grain enact) {:status :pass})
          good-r (assoc-in rec [:decision :selection-law :candidate] (:candidate enact))
          good (check-c good-r good-e)
          first-only (update good-e :attempts (fn [as] (filterv #(= 1 (:n %)) as)))
          untyped (update-in good-e [:conformance :deviations] (fn [ds] (conj (vec ds) {:statement "untyped"})))
          nocheck (update good-e :attempts (fn [as] (mapv #(if (= :grain-gate (get-in % [:check :kind])) % (dissoc % :check)) as)))
          wrong-grain (gated enact grain-n grain-p outcome-grain {:status :pass})
          wrong-arglist (gated enact grain-n grain-p (assoc-in (:grain enact) [:evidence :arglist] "[agent-id]") {:status :pass})
          other-id (assoc-in rec [:decision :selection-law :candidate] :cand/b-observe-first)]
      (println "  W_c (enactment is the chosen candidate):" ef)
      (println (show pc))
      (doseq [p (wc-failures pc)] (println "     -" p))
      (println "  W_c positive control (grain attempt carries a G_c pass; selection law names the candidate):"
               (if (= [] good) "PASS" (str "FAIL " (pr-str good))))
      (let [bad-cases [["X_c(first) only the first attempt (8e5c431e, provider grain)" good-r first-only]
                       ["X_c(check) successful attempts with their checks removed" good-r nocheck]
                       ["X_c(untyped) an untyped deviation" good-r untyped]
                       ["X_c(d) the recorded enactment: its grain attempt names no G_c pass" good-r enact]
                       ["X_c(d) G_c at the outcome's provider grain (:agent-id)" good-r wrong-grain]
                       ["X_c(d) G_c recorded as a pass, re-run refuses :arglist-mismatch" good-r wrong-arglist]
                       ["X_c(join) selection law names a DIFFERENT candidate" other-id good-e]]
            unverifiable (check-c rec good-e)
            vac (atom (if (= [] good) 0 1))]
        (doseq [[label r e] bad-cases]
          (let [v (check-c r e) caught (and (vector? v) (seq v))]
            (when-not caught (swap! vac inc))
            (println "   " (if caught "caught " "VACUOUS") label)))
        (let [ok? (and (map? unverifiable) (= :join-unverifiable (:status unverifiable)) (empty? (:failures unverifiable)))]
          (when-not ok? (swap! vac inc))
          (println "   " (if ok? "typed  " "WRONG  ") "X_c(join) selection law with no candidate id reads :join-unverifiable, neither pass nor fail"))
        (when (pos? @vac) (println "  W_c checker defect: a bad case was not caught or the control did not pass"))
        (reset! wc-defects @vac)
        (when (= "--wc" (nth *command-line-args* 2 nil))
          (System/exit (if (pos? @wc-defects) 1 0))))))
  (println "  falsifiers (each bad extract must fail):")
  (let [vac (atom 0)]
    (doseq [[label bad] (falsifiers rec)]
      (let [caught (binding [*require-machine-construction* false] (seq (:problems (check bad))))]
        (when-not caught (swap! vac inc))
        (println "   " (if caught "caught " "VACUOUS") label)))
    (System/exit (if (or (seq problems) (pos? @vac) (pos? @wc-defects)) 1 0))))
;; Exit 1 on this record today is expected: both candidates are hand-built.
