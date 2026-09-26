#!/usr/bin/env bb
;; wm_coverage.bb -- the coverage obligation of the wire ledger, defined before any
;; flight: which wires a SUCCESSFUL flight's record can witness, which only under a
;; condition, which only through a failure branch, and which no root reaches.
;; Read-only for the map, the ledger and the org layer.
;;
;;   bb holes/labs/M-wm-wiring/spike/wm_coverage.bb [LEDGER-REV] [ORG-LAYER-REV-OR-FILE]
;;
;; Inputs are read from git (default HEAD), never from the working tree:
;;   wm-wire-ledger.edn at LEDGER-REV     the wires [writer reader field]; its :adjacency
;;                                        :map names the map revision
;;   wm-flight-wiring.edn at that map rev the boxes and their kinds
;;   wm-org-layer.edn at ORG-LAYER-REV    the call edges, each with :conditional and, for a
;;                                        call reached through helpers, :conditions-along-path
;; Writes holes/labs/M-wm-wiring/wm-wire-coverage.edn (a new file: the ledger's companion).
;;
;; METHOD
;; 1. A call edge's conditions are its :conditional and its :conditions-along-path.
;; 2. Each condition text is classified by TABLE below, on what the branch means for a
;;    successful flight:
;;      :success      a successful flight takes this branch (a configured step, the
;;                    default arm, no refusal, a test seam absent so the production
;;                    default runs). It is dropped from the path.
;;      :failure      the branch is a refusal, abstention, decline or trip.
;;      :alternative  a valid branch a successful flight may or may not take (a data
;;                    shape, an optional input, a mode).
;;    The SUCCESS PATH is the spike as flight_driver documents it: every wired step given
;;    (--checker --bb --library-root --field-entry --cascades --dispatch-seat --author
;;    --reviewer), the click answering 200 with a click id, the read admitted, an
;;    interpretation admitted, a candidate constructed, selected and enacted, W_c passing,
;;    the publication observed. A condition text NOT in the table is :alternative and is
;;    listed under :undecided, so a new org layer cannot silently promote a wire.
;; 2b. A call may carry :other-paths (ORG-PATHS-I). Every path is an edge, so a box reached
;;    by several branches is reached under the DISJUNCTION of their condition sets, and a
;;    call into a refusal box (decision_gate refuse!) is a :failure whatever arm carries it.
;;    The output's :method lists the wires whose class differs from the first-path-only
;;    method with both path sets. WM_COVERAGE_FIRST_PATH_ONLY=1 selects the older method.
;; 3. A path's condition set is its non-:success conditions. Per box, the minimal sets
;;    over all simple paths from a root (fixpoint).
;; 4. A wire's ends combine: the union of one writer set and one reader set, refused when
;;    the two hold opposite polarities of one test ((if X) then / (if X) else). A reader
;;    that is a :test box has no call path by construction (the org layer's
;;    :tests-not-in-call-tree): it observes the record the writer's run produced, so its
;;    end adds no condition. The wire's class is the best over the combinations:
;;      :witness       some combination is empty
;;      :conditional   else some combination has no :failure condition (the smallest, as
;;                     the conjunction that must hold)
;;      :failure-path  else every combination needs a :failure condition
;;      :unreachable   an end has no path: :why :box-not-in-org-layer (the org layer is
;;                     older than the map), :no-caller (an org-layer :unplaced box), or
;;                     :no-path-from-root; or every combination is contradictory
;;                     (:why :mutually-exclusive-branches)
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.set :as set]
         '[clojure.java.shell :refer [sh]] '[clojure.pprint :as pp])

(def home (System/getProperty "user.home"))
(def f3c (str home "/code/futon3c"))
(defn die [& m] (binding [*out* *err*] (apply println "wm_coverage:" m)) (System/exit 1))
(defn git [& args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" f3c args)]
    (when-not (zero? exit) (die "git" args err)) out))
(def lab "holes/labs/M-wm-wiring/")
(def ledger-rev (str/trim (git "rev-parse" "--short=8" (or (first *command-line-args*) "HEAD"))))
(def org-arg (or (second *command-line-args*) "HEAD"))
;; ORG-LAYER-REV may be a file path (contains "/"): an INFORMATIONAL run over an uncommitted
;; org layer, marked :uncommitted-input in the output; set WM_COVERAGE_OUT so it does not
;; overwrite the committed companion, and do not commit its output.
(def org-file? (str/includes? org-arg "/"))
(def org-rev (if org-file? org-arg (str/trim (git "rev-parse" "--short=8" org-arg))))
(defn show [rev file] (git "show" (str rev ":" lab file)))
(defn read-edn [s] (edn/read-string {:default (fn [_ v] v)} s))

(def ledger (read-edn (show ledger-rev "wm-wire-ledger.edn")))
(def map-rev (get-in ledger [:adjacency :map]))
(def m (read-edn (git "show" (str map-rev ":" lab "wm-flight-wiring.edn"))))
(def org (read-edn (if org-file? (slurp org-arg) (show org-rev "wm-org-layer.edn"))))
(def wires (mapv :wire (:wires ledger)))
(when (empty? wires) (die "the ledger has no wires"))

;; ---------------------------------------------------------------------------
;; The rule for each condition text of the org layer at HEAD. Every entry says why.
(def TABLE
  (into {}
        (map (fn [[t c why]] [t {:class c :why why}]))
        [["(defmethod source-wants :a-exits)" :success "the driver starts every flight with :kind :a-exits (flight_driver.clj flight-for)"]
         ["(if (empty? problems)) then" :failure "no assembled problems: the decision is the abstained one (war_machine.clj:6280, decision-gate/emit! :status :abstained)"]
         ["(if (empty? problems)) else" :success "problems were assembled: the joint cascade decision runs (war_machine.clj:6284 on)"]
         ["(if commissioned?) then" :alternative "commissioned? is (true? (:r10-commissioned payload)) (http.clj:8978): R10 operator-authorised plumbing outside PROOF-2a (COMMISSION-D). The flight must not set the key: the commissioned branch calls click! with only {:config :issuer-provenance}, so the flight's target and wants would never reach the runner. A flight's click is ordinary, so it does not take this branch"]
         ["(call of a refusal box: decision_gate refuse!)" :failure "a call into decision_gate refuse!: the gate's refusal, on every path that reaches it (see refusal-boxes)"]
         ["(if commissioned?) else" :success "the ordinary click (runner-service/click!): the flight's own click, which does not carry :r10-commissioned (COMMISSION-D). It reaches the same click! -> run-opportunity! as the commissioned branch, with no commissioning condition"]
         ["(and ... after (nil? decision))" :failure "no decision recorded: the abstention carrier's arm for a tick with no selection decision (full_loop_runner.clj abstention-carrier)"]
         ["(cond (and (nil? decision) judge-refusal))" :failure "the judge refused the decision typed: the tick abstains with that kind (full_loop_runner.clj abstention-carrier)"]
         ["(cond (cascade-decision? decision))" :success "decision-gate/emit!'s cascade arm: the flight's decision is an admissible cascade decision (an abstention takes the arm before it)"]
         ["(cond (and (map? decision) (get-in decision [:action :type])))" :failure "decision-gate/emit!'s flat-action arm: it refuses :flat-action"]
         ["(when (> (- best (get eligible-marginals chosen-pattern 0.0)) mass-tolerance))" :failure "decision-gate check-cascade-decision!: the body is refuse! (the chosen pattern does not carry the most mass)"]
         ["(when (> (abs (- (double chosen-mass) (double marginal))) mass-tolerance))" :failure "decision-gate: the body is refuse! (recorded chosen mass differs from the marginal)"]
         ["(when (> (abs (- total 1.0)) mass-tolerance))" :failure "decision-gate: the body is refuse! :posterior-not-normalised"]
         ["(when (and (seq (:precedence candidate)) (empty? (:interpretation-receipts candidate))))" :failure "decision-gate check-candidate-receipts!: the body is refuse!"]
         ["(when (empty? marginals))" :failure "decision-gate: the body is refuse! :no-acting-candidate"]
         ["(when (nil? chosen-pattern))" :failure "decision-gate: the body is refuse! :chosen-action-is-not-an-action"]
         ["(when (seq refusals))" :failure "a refusals-present branch: the tick carries refusals"]
         ["(when-not (= (set (keys posterior)) (set (map :id candidates))))" :failure "decision-gate check-queue!: the body is refuse! :ticket-queue-candidates-mismatch"]
         ["(when-not (= receipt law-receipt))" :failure "decision-gate check-queue!: the body is refuse! :ticket-queue-certificate-mismatch"]
         ["(when-not (and (= :wm/ticket-queue-selection-v1 (:schema receipt)) (seq (:entries declaration)) (= targets (:eligible-targets receipt)) (= (mapv #(...)" :failure "decision-gate check-queue!: the body is refuse! :ticket-queue-choice-invalid"]
         ["(when-not (and (map? (:beta decision)) (contains? #{:declared :learned} status) (number? value) (pos? value)))" :failure "decision-gate check-beta!: the body is refuse!"]
         ["(when-not (and (map? candidate) (= :cascade-candidate (:kind candidate))))" :failure "decision-gate check-candidate-receipts!: the body is refuse!"]
         ["(when-not (and (map? posterior) (seq posterior)))" :failure "decision-gate: the body is refuse! :missing-recorded-posterior"]
         ["(when-not (and (number? chosen-mass) (if (seq (get-in decision [:selection-certificate :ticket-queue :eligible-targets])) ;; A finite-support queue...)" :failure "decision-gate: the body is refuse! (the chosen mass is not a number or is out of the declared queue)"]
         ["(when-not (contains? posterior (:action decision)))" :failure "decision-gate: the body is refuse! :chosen-action-not-a-candidate"]
         ["(when-not (some? (:construction-receipt candidate)))" :failure "decision-gate check-candidate-receipts!: the body is refuse!"]
         ["(when-not (some? (:interpretation-receipts candidate)))" :failure "decision-gate check-candidate-receipts!: the body is refuse!"]
         ["(if (::retry %)) then" :alternative "flight_runner: an entry whose first answer failed validation is asked again; a first-time-valid answer takes the other branch"]
         ["(if (string? locator)) else" :alternative "a command locator (observation_checks.clj:320); the other valid form is a namespace locator"]
         ["(cond-> (nil? grain-p))" :alternative "the candidate names no grain pattern: the plan then runs the grain gate on a nil grain (flight_runner.clj), a data shape"]
         ["(when (= :published (:outcome coverage-entry)))" :alternative "the coverage reading was published: depends on the mission text needing coverage, like (when (:coverage? cov-need))"]
         ["(when (= :published (:outcome constraints-entry)))" :alternative "the constraints reading was published: depends on the mission text needing constraints"]
         ["(or ... after (:repair-system-record-fn opts))" :success "a test seam absent: the production record runs"]
         ["(or ... after (explicit-failure-kind t) (some thrower-kind (cause-chain t)))" :alternative "the failure classifier's own fallback chain, on the close-cause path (which a flight that fails takes)"]
         ["(if (identical? historical-verification-completion-token (:historical-verification-completion-token (ex-data e)))) else" :alternative "not the historical-verification completion token: the ordinary failure record, on the close-cause path"]
         ["(when-not (contains? @!noted-trips run-key))" :failure "tripwire: the first noting of a trip, the branch that records the trip finding"]
         ["(or ... after (get classifications target))" :alternative "focus_receipt: the target was not classified in advance, so it is classified with the relation context; whether the flight supplies classifications is not fixed by the spike"]
         ["(cond :else)" :success "the default arm of a cond: reached when no earlier (refusal or special-case) arm fired"]
         ["(if (= :absent located)) else" :success "the locator was located"]
         ["(if (and (seq questions) (empty? (:wants wants)))) else" :success "not (questions asked and no wants): the flight has wants"]
         ["(if (contains? m :config)) else" :alternative "no :config key: the default configuration of the check; either is a valid run"]
         ["(when text)" :success "the target's text was read"]
         ["(if-not (compare-and-set! !status current next-status)) else" :success "the status transition succeeded"]
         ["(if (map? precedence)) then" :alternative "a co-application precedence; the alternative is a chain: one candidate is one or the other"]
         ["(if-let [refusal (missing-interpretation (map pattern-of units))]) else" :success "no missing interpretation"]
         ["(or ... after (:enactment-fold judge-opts))" :success "a test/override seam absent: the production default runs"]
         ["(if (contains? issued ::refused)) else" :success "the request was not refused"]
         ["(if (nil? horizon)) else" :success "a horizon is declared (nil is the :missing-common-horizon refusal)"]
         ["(cond (and (seq candidate-actions) (every? fm/cascade-candidate? candidate-actions)))" :success "the cascade-candidate arm: the flight's candidates are cascade candidates"]
         ["(if (or (:failed plan) (:declined plan))) else" :success "the seat's plan neither failed nor declined"]
         ["(cond (nil? sourced))" :alternative "no locators on the problem: the identity-default arm; a flight with locators takes the sourced arms"]
         ["(if entry) then" :success "a selection entry exists (nil is the abstention)"]
         ["(when enact-fn)" :success "the enact step is configured in the spike"]
         ["(if-let [r (or jr gr)]) then" :failure "a judge or gate refusal exists"]
         ["(if-let [r (or jr gr)]) else" :success "no judge or gate refusal"]
         ["(or ... after publication-observation)" :success "a test seam absent: the production observation runs"]
         ["(if (string? locator)) then" :alternative "a namespace locator; the other valid form is a command locator (observation_checks.clj:320)"]
         ["(cond *halt-on-witness?*)" :alternative "a dynamic mode flag"]
         ["(if (nil? s0)) else" :success "the start state exists"]
         ["(when dispatch-seat)" :success "the spike gives --dispatch-seat"]
         ["(when (:criteria? first-need))" :alternative "the read step's first need is criteria: depends on what the mission text states"]
         ["(if (not= :supported (:status supported))) else" :success "the support is :supported"]
         ["(or ... after (:dispatch-fn opts))" :success "a test seam absent: the production dispatch runs"]
         ["(if (:row a)) else" :alternative "the action carries no :row: the embedding neighbour is looked up another way"]
         ["(if-let [drift (some (fn [[id sections]] (when (< 1 (count (distinct (vals sections)))) {:id id :hashes sections})) hashes)]) else" :success "no hash drift"]
         ["(or ... after (:tripwire/repair-record-fn opts))" :success "a test seam absent: the production record runs"]
         ["(or ... after click-fn)" :success "no click-fn override: the driver's http-click-fn"]
         ["(when ask-fn)" :success "the ask step is configured in the spike"]
         ["(case (:trip/action report))" :failure "a tripwire action: a trip is an anomaly by definition"]
         ["(or ... after (:repair-system-record-fn raw-opts))" :success "a test seam absent: the production record runs"]
         ["(if-not (and (= 200 status) click-id)) else" :success "the click answered 200 with a click id"]
         ["(when read-fn)" :success "the read step is configured in the spike"]
         ["(when (and wc-fn (:enactment enacted)))" :success "wc-fn is configured and an enactment exists"]
         ["(when (map? locators))" :success "the problem carries locators: the sourced-rates path a flight over a located target takes"]
         ["(or ... after answer-fn)" :success "no answer-fn override: the driver's agency-answer-fn"]
         ["(when (empty? stated))" :alternative "the mission states no outcomes: the read step's own path"]
         ["(or ... after dispatch!)" :success "a test seam absent: the production dispatch runs"]
         ["(if (= k n)) else" :alternative "a step counter: the non-final iteration"]
         ["(cond library-root)" :success "the spike gives --library-root"]
         ["(if (or (not= :open (:status f)) (>= (count (:clicks f)) max-clicks))) else" :alternative "the flight is still open and under budget: a second click; the spike runs --max-clicks 1, which takes the other branch"]
         ["(if stopping-rule-data) else" :alternative "no stopping-rule data: the failure classifier's other input"]
         ["(when-not jr)" :success "no judge refusal"]
         ["(if (seq static)) else" :alternative "no static wants: the ask path builds them"]
         ["(cond-> (not (contains? cascade-sources :construction)))" :alternative "the sources carry no :construction: depends on the declared sources"]
         ["(when (get-in cw2 [:source :readings-needed :constraints?]))" :alternative "the read step needs constraints: depends on the mission text"]
         ["(cond (abstention? decision))" :failure "the decision is an abstention"]
         ["(or ... after observe)" :success "a test seam absent: the production observation runs"]
         ["(when (seq stated))" :alternative "the mission states outcomes: the read step's own path"]
         ["(or ... after (:judge-fn opts))" :success "a test seam absent: the production judge runs"]
         ["(if present?) then" :alternative "prior flight records are present (enactment-fold-source/fold-from-flights); the first flight has none"]
         ["(if *handling-trip?*) else" :success "not handling a trip"]
         ["(if (seq route)) then" :success "the observed route is non-empty: the branch that persists the run record (full_loop_runner.clj:678)"]
         ["(when-not (and (sequential? refusals) (seq refusals)))" :failure "decision-gate check-abstention!: the body is refuse! :empty-refusals (corrected from :success in ORG-PATHS-I: the body is the refusal, on the abstention path)"]
         ["(when (:coverage? cov-need))" :alternative "the read step needs coverage: depends on the mission text"]
         ["(if-not (and (= :cascade-candidate (:kind action)) (= 1 (count domains)) (seq want) (map? q0) (seq q0) (model/normalized-exact? q0) (every? set? ...) else" :success "the action is a well-formed cascade candidate over one domain with a normalised q0"]]))

(defn cond-class [t] (or (get TABLE t) {:class :alternative :why "not in TABLE: undecided" :undecided true}))
(defn polarity [t] (when-let [[_ base p] (re-matches #"(?s)(.*) (then|else)" t)] [base p]))
(defn contradictory? [conds]
  (let [ps (keep polarity conds)]
    (boolean (some (fn [[b p]] (some (fn [[b2 p2]] (and (= b b2) (not= p p2))) ps)) ps))))

;; ---------------------------------------------------------------------------
;; The call graph. A call may carry :other-paths (the org layer keeps every distinct
;; path to a callee, not the first found); each path is an edge of its own, so a box
;; reached by two branches is reached under the DISJUNCTION of their condition sets:
;; the reachability below keeps the minimal set per path. FIRST-PATH-ONLY (env
;; WM_COVERAGE_FIRST_PATH_ONLY=1, or the old layer that has no :other-paths) ignores
;; them: it is the earlier method, kept so a change of class can be printed with both.
;; A call INTO a refusal box is a refusal whatever arm of whatever helper carries it:
;; decision_gate refuse! is the refusal, and helper arms such as emit!'s (cond :else)
;; fall through to it, which the text of that arm ("(cond :else)", the default arm) cannot
;; say. Without this rule a fallthrough to :not-a-decision reads as an unconditional
;; path and promotes the gate-refusal wire to :witness.
(def refusal-boxes #{:gate-refuse})
(def refusal-call-cond "(call of a refusal box: decision_gate refuse!)")
(defn edge-of [c from to cond-1 along]
  (let [cs (concat (when cond-1 [cond-1]) along (when (refusal-boxes to) [refusal-call-cond]))]
    {:from from :to to
     :conds (vec (distinct cs))
     :nonsuccess (vec (distinct (remove #(= :success (:class (cond-class %))) cs)))}))
(defn edges-for [use-others?]
  (vec (for [c (:calls org)
             e (cons (edge-of c (:caller c) (:callee c) (:conditional c) (:conditions-along-path c))
                     (when use-others?
                       (for [o (:other-paths c)]
                         (edge-of o (:caller c) (:callee c) (:conditional o) (:conditions-along-path o)))))]
         e)))
(def edges (edges-for true))
(def first-path-edges (edges-for false))
(def roots (vec (:roots org)))
(def org-nodes (into (set (mapcat (juxt :from :to) edges)) (concat roots (:unplaced org) (:tests-not-in-call-tree org))))
(def unplaced (set (:unplaced org)))
(def box-kind (into {} (map (juxt :box/id :box/kind)) (:boxes m)))

(defn minimal [sets] ;; drop supersets: keep only the sets no other set is a subset of
  (into {} (filter (fn [[s _]] (not-any? (fn [[s2 _]] (and (not= s s2) (set/subset? s2 s))) sets))) sets))

(defn reach
  "node -> {condset path}: the minimal non-success condition sets over simple paths from a root."
  [edges]
  (loop [state (into {} (for [r roots] [r {#{} [r]}])) iter 0]
    (let [next-state
          (reduce (fn [st {:keys [from to nonsuccess]}]
                    (reduce (fn [st [s path]]
                              (if (some #{to} path)
                                st
                                (let [ns (set/union s (set nonsuccess))
                                      cur (get st to {})]
                                  (if (some (fn [[s2 _]] (set/subset? s2 ns)) cur)
                                    st
                                    (assoc st to (minimal (assoc cur ns (conj path to))))))))
                            st (get st from {})))
                  state edges)]
      (cond (= next-state state) state
            (> iter 60) (die "reachability did not settle")
            :else (recur next-state (inc iter))))))
(def R (reach edges))
(def R1 (reach first-path-edges))

(def rank {:witness 0 :conditional 1 :failure-path 2})
(defn set-class [s] (cond (empty? s) :witness (some #(= :failure (:class (cond-class %))) s) :failure-path :else :conditional))

(defn end-info [R box]
  (cond (= :test (box-kind box)) {:paths {#{} [:observer]} :observer? true}
        (contains? (set (:tests-not-in-call-tree org)) box) {:paths {#{} [:observer]} :observer? true}
        (not (org-nodes box)) {:why :box-not-in-org-layer}
        (unplaced box) {:why :no-caller}
        (empty? (get R box)) {:why :no-path-from-root}
        :else {:paths (get R box)}))

(defn classify [R [w r f :as wire]]
  (let [we (end-info R w) re (end-info R r)]
    (if-let [why (or (:why we) (:why re))]
      {:wire wire :coverage :unreachable :why why :writer-end (or (:why we) :ok) :reader-end (or (:why re) :ok)}
      (let [combos (for [[ws wp] (:paths we) [rs rp] (:paths re)
                         :let [u (set/union ws rs)] :when (not (contradictory? u))]
                     {:conds u :class (set-class u) :writer-path wp :reader-path rp})]
        (if (empty? combos)
          {:wire wire :coverage :unreachable :why :mutually-exclusive-branches}
          (let [best (first (sort-by (juxt (comp rank :class) (comp count :conds)) combos))]
            (cond-> {:wire wire :coverage (:class best)
                     :writer-path (:writer-path best) :reader-path (:reader-path best)}
              (seq (:conds best)) (assoc :conditions (vec (sort (:conds best))))
              (:observer? re) (assoc :reader-kind :test-observer))))))))

(def first-only? (= "1" (System/getenv "WM_COVERAGE_FIRST_PATH_ONLY")))
(def rows-first (mapv #(classify R1 %) wires))
(def rows (if first-only? rows-first (mapv #(classify R %) wires)))
(defn counts-of [rs] (assoc (into (sorted-map) (frequencies (map :coverage rs))) :wires (count rs)))
;; the wires whose class changed between the first-path-only method and all paths,
;; each with both path sets, so the change is checkable
(def changed
  (vec (for [[a b] (map vector rows-first rows) :when (not= (:coverage a) (:coverage b))]
         {:wire (:wire b)
          :first-path (select-keys a [:coverage :conditions :writer-path :reader-path :why])
          :all-paths (select-keys b [:coverage :conditions :writer-path :reader-path :why])})))
(def counts (counts-of rows))
(when-not (= (count rows) (reduce + (vals (dissoc counts :wires))))
  (die "the counts do not sum to the wire count"))
(def used (into (sorted-map) (for [t (distinct (mapcat :conds edges))] [t (cond-class t)])))

(def out
  {:inputs {:ledger {:rev ledger-rev :map map-rev :wires (count wires)}
            :org-layer {:rev org-rev :uncommitted-input org-file? :map-rev (:map-rev org) :futon2-rev (:futon2-rev org)
                        :calls (count (:calls org)) :roots roots}
            :stale? (not= map-rev (:map-rev org))
            :stale-note "true when the org layer was generated from an older map than the ledger's: boxes added since are :box-not-in-org-layer, not map defects"}
   :counts counts
   :method {:paths (if first-only? :first-path-only :all-paths) :first-path-counts (counts-of rows-first) :changed changed}
   :by-kind {:unreachable (into (sorted-map) (frequencies (map :why (filter #(= :unreachable (:coverage %)) rows))))
             :test-observer-readers (count (filter #(= :test-observer (:reader-kind %)) rows))}
   :conditions used
   :undecided (vec (sort (for [[t c] used :when (:undecided c)] t)))
   :wires (mapv (fn [r] (-> r (update :writer-path #(some-> % vec)) (update :reader-path #(some-> % vec)))) rows)})

(def out-path (or (System/getenv "WM_COVERAGE_OUT") (str f3c "/" lab "wm-wire-coverage.edn")))
(spit (str out-path ".tmp")
      (str ";; GENERATED by holes/labs/M-wm-wiring/spike/wm_coverage.bb -- do not edit; method and rule in the script header.\n"
           (with-out-str (pp/pprint out))))
(.renameTo (java.io.File. (str out-path ".tmp")) (java.io.File. out-path))
(println (pr-str counts) "stale?" (:stale? out) "undecided" (count (:undecided out)))
