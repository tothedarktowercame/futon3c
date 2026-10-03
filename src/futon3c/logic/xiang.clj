(ns futon3c.logic.xiang
  "象's semantics as relations: the oracle the frontends are certified against.

   M-象-2000 asks that an operator's and agents' acts (requests, proposals,
   promises, offers, agreements, withdrawals) be addressable at any past
   moment: what was in force as of T, and how a record came to be. The
   mission wrote the answers in prose and four engines implement pieces of
   them (agency/rule_timeline, agency/obligations, agency/history_constraints,
   emacs/xiang-trace.el's reazon rules); the Element port is a fifth with no
   spec. This namespace states the semantics once, in core.logic, over a flat
   fact base, so a history replayed through any implementation can be checked
   against the same answers.

   Named `xiang`, not `elephant`: McCarthy's Elephant 2000 is the anchor, but
   its one-program-one-customer airline example is not our setting. Ours is an
   operator and many agents, acts dictated, delegated, parked and inferred,
   and the relations here extend as that setting demands.

   Two time axes. Every act has a valid time (`at`, when it was done) and a
   system time (`sys`, when the store learned of it). \"As of\" is a pair
   [t s]: an act counts only if at <= t and sys <= s, so a late-inserted act
   changes the system-time answer and not the valid-time one (P6).

   Facts (pldb relations):
     act id kind author at sys     one act; kind is a keyword (see `kinds`)
     to id addressee                who the act was addressed to
     target id other                the act or standing thing it bears on
     seat id agent session          the exact seat it happened in
     cites id other                 basis: what the act answers or relies on
     carries-out commit act         a commit that implements a proposed/said rule
     beneficiary id who             a promise's creditor
     option id n                    an offer's option, an acceptance's choice
     grantee id who                 a grant's holder
     scope id kind                  a grant's act kind

   The core relation is McCarthy's hasreservation shape: a standing holds at
   [t s] when a visible act created it and no visible, unreversed act
   cancelled it."
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [== fresh run* conde]]
            [clojure.core.logic.pldb :as pldb]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Vocabulary

(def kinds
  "Act kinds and what they do to standings.
   :creates    the act brings a standing into being (its own id names it)
   :cancels    the act ends the standing named by its target
   :reverses   the act undoes the cancellation named by its target
   :closes     the act discharges the obligation named by its target
   :says       the act asserts or asks and changes no standing by itself"
  {:report-problem :says  :explain :says  :report :says  :clarify :says
   :qualify :says  :approve :says  :disagree :says  :collect :says
   :verify :says  :continue :says  :redirect :says  :ask-action :says
   :prioritize :says  :explore :says  :extend :says  :defer :says
   :notice :says  :commit :says
   :constrain :creates     ; an operator rule, said plainly: promulgated at once
   :propose :creates       ; a proposal: in force only when a commit carries it out
   :delegate :creates      ; a request to another agent: an expectation, not a debt
   :offer :creates         ; 🈸 paragraphs in a reply, with options
   :accept :creates        ; "yes [N]": an agreement, if an offer was visible
   :promise :creates       ; a debt: author owes beneficiary by deadline
   :grant :creates         ; authority for a caller to perform an act kind
   :select-card :creates   ; an active pattern card in a seat
   :withdraw :cancels      ; ends the target (an operator's own, or an agent's)
   :retract :cancels       ; an author takes back its own act
   :rule-withdraw :cancels
   :reversal :reverses     ; undo: restores what a provisional withdrawal ended
   :fulfil :closes  :release :closes  :lapse :closes})

(def creating-kinds (set (keep (fn [[k v]] (when (= v :creates) k)) kinds)))
(def cancelling-kinds (set (keep (fn [[k v]] (when (= v :cancels) k)) kinds)))
(def closing-kinds (set (keep (fn [[k v]] (when (= v :closes) k)) kinds)))

;; ---------------------------------------------------------------------------
;; Facts

(pldb/db-rel act ^:index id kind author at sys)
(pldb/db-rel to ^:index id addressee)
(pldb/db-rel target ^:index id other)
(pldb/db-rel seat ^:index id agent session)
(pldb/db-rel cites ^:index id other)
(pldb/db-rel carries-out ^:index commit act)
(pldb/db-rel beneficiary ^:index id who)
(pldb/db-rel option ^:index id n)
(pldb/db-rel grantee ^:index id who)
(pldb/db-rel scope ^:index id kind)
(pldb/db-rel answers ^:index id opener)          ; an explicit dialogical link
(pldb/db-rel answers-inferred ^:index id opener) ; a link the flow inferred from the matrix

;; ---------------------------------------------------------------------------
;; Adjacency: which intents answer which (a prior, to be tuned on evidence)

(def adjacency
  "Opening intent -> {answering intent -> effect on the port}.
   :closes  the port is answered and closes
   :keeps   the answer bears on the port but leaves it open (a caveat, a
            progress report)
   An intent absent as a key opens no port: it is annotative or steering and
   rides the flow. This is the operator's prior over the dialogue game
   (2026-10-03); scripts/xiang_transitions.py tunes it against the readings."
  {:propose        {:approve :closes :disagree :closes :defer :closes :redirect :closes
                    :extend :closes :commit :closes :withdraw :closes :retract :closes}
   :ask-action     {:accept :closes :disagree :closes :qualify :keeps :offer :keeps
                    :report :closes :explain :closes}
   :offer          {:accept :closes :disagree :closes :defer :closes :retract :closes :withdraw :closes}
   :delegate       {:promise :closes :report-problem :keeps :retract :closes :disagree :closes :report :closes}
   :promise        {:fulfil :closes :release :closes :lapse :closes :report-problem :keeps :qualify :keeps}
   :report-problem {:verify :closes :explain :closes :redirect :closes :commit :closes :collect :keeps}
   :clarify        {:explain :closes :report :closes}
   :constrain      {:withdraw :closes :rule-withdraw :closes :qualify :keeps :commit :keeps}
   :verify         {:report :closes :approve :closes :explain :closes :report-problem :closes}})
;; Not ports: select-card, grant and withdraw are standings, answered by the
;; vertical relations (in-forceo, reversedo), not moves awaiting a reply. A
;; withdrawal's undo still links to it through its target.

(def opening-kinds (set (keys adjacency)))

(defn answer-effect
  "What an act of kind ANSWER does to a port opened by OPENER, or nil."
  [opener answer]
  (get-in adjacency [opener answer]))

(defn explicit-answers
  "[[answer-id opener-id] ...]: the dialogical links a history states
   outright. An act answers what it targets, what a commit carries out, and
   what it cites when the adjacency table says that pair is an answer.
   Targets and carries-out are links whatever the table says."
  [history]
  (let [kind-of (into {} (map (juxt :id :kind) history))
        seqv (fn [x] (if (sequential? x) x (when x [x])))]
    (vec (distinct
          (for [{:keys [id kind] :as a} history
                opener (concat (seqv (:target a))
                               (seqv (:carries-out a))
                               (filter #(answer-effect (kind-of %) kind) (seqv (:cites a))))
                :when (contains? kind-of opener)]
            [id opener])))))

(defn- epoch
  "Seconds since the epoch for an ISO instant or a number."
  [x]
  (cond (number? x) (long x)
        (string? x) (.getEpochSecond (java.time.Instant/parse x))
        :else (throw (ex-info "Not a time" {:reason :invalid-time :value x}))))

(defn facts
  "The pldb facts for one history: a sequence of act maps with keys
   :id :kind :author :at and optionally :sys (defaults to :at), :to, :target,
   :agent/:session, :cites (one or many), :carries-out (one or many),
   :beneficiary, :option, :grantee, :scope."
  [history]
  (vec
   (mapcat (fn [{:keys [id kind author at sys] :as a}]
             (when-not (contains? kinds kind)
               (throw (ex-info (str "Unknown act kind " kind) {:reason :unknown-kind :id id :kind kind})))
             (concat
              [[act id kind author (epoch at) (epoch (or sys at))]]
              (when (:to a) [[to id (:to a)]])
              (when (:target a) [[target id (:target a)]])
              (when (:agent a) [[seat id (:agent a) (:session a)]])
              (for [c (let [c (:cites a)] (if (sequential? c) c (when c [c])))] [cites id c])
              (for [x (let [x (:carries-out a)] (if (sequential? x) x (when x [x])))] [carries-out id x])
              (when (:beneficiary a) [[beneficiary id (:beneficiary a)]])
              (when (:option a) [[option id (:option a)]])
              (when (:grantee a) [[grantee id (:grantee a)]])
              (when (:scope a) [[scope id (:scope a)]])))
           history)))

(defn facts-with-links
  "`facts` plus the explicit `answers` links, plus INFERRED [[answer opener] ..]."
  [history & [inferred]]
  (into (facts history)
        (concat (for [[r a] (explicit-answers history)] [answers r a])
                (for [[r a] inferred] [answers-inferred r a]))))

(defn db
  "A pldb database for HISTORY, with its explicit dialogical links and any
   INFERRED ones."
  [history & [inferred]]
  (apply pldb/db (facts-with-links history inferred)))

(defn load-fixture
  "A history from an EDN file: {:name .. :history [..] :expect {..}}."
  [path]
  (edn/read-string (slurp (io/file path))))

;; ---------------------------------------------------------------------------
;; Relations

(defn visibleo
  "Act A is visible as of [t s]: done at or before t, known at or before s."
  [a t s]
  (fresh [k who at sys]
    (act a k who at sys)
    ;; project t and s too: when a caller passes a fresh variable that is
    ;; bound only at run time (agreemento's own acceptance time), a closure
    ;; over the raw variable would compare an LVar to a number.
    (l/project [at t] (== true (<= at t)))
    (l/project [sys s] (== true (<= sys s)))))

(defn kindo [a k]
  (fresh [who at sys] (act a k who at sys)))

(defn authoro [a who]
  (fresh [k at sys] (act a k who at sys)))

(defn ato [a at]
  (fresh [k who sys] (act a k who at sys)))

(defn reversedo
  "Cancellation C has been undone by a visible reversal."
  [c t s]
  (fresh [r]
    (target r c)
    (kindo r :reversal)
    (visibleo r t s)))

(defn cancelledo
  "Standing X has been ended by a visible cancelling act that was not reversed."
  [x t s]
  (fresh [c k]
    (target c x)
    (kindo c k)
    (l/membero k (vec cancelling-kinds))
    (visibleo c t s)
    (l/nafc reversedo c t s)))

(defn closedo
  "Obligation X has been discharged (fulfilled, released or lapsed) visibly."
  [x t s]
  (fresh [c k]
    (target c x)
    (kindo c k)
    (l/membero k (vec closing-kinds))
    (visibleo c t s)))

(defn promulgatedo
  "X is a creating act, visible, and not cancelled: said and still standing.
   For a rule this is the \"adopted\" reading, not yet \"in force\"."
  [x t s]
  (fresh [k]
    (kindo x k)
    (l/membero k (vec creating-kinds))
    (visibleo x t s)
    (l/nafc cancelledo x t s)))

(defn carried-outo
  "A visible commit implements X."
  [x t s]
  (fresh [c]
    (carries-out c x)
    (visibleo c t s)))

(defn in-forceo
  "X is in force as of [t s]. Joe's ruling (P13b): in force means APPLIED,
   so a proposal is in force only once a commit carried it out; a rule said
   plainly (constrain) and every other standing is in force when promulgated."
  [x t s]
  (fresh [k]
    (promulgatedo x t s)
    (kindo x k)
    (conde
     [(== k :propose) (carried-outo x t s)]
     [(l/!= k :propose)])))

(defn visible-offero
  "OFFER is an offer by AGENT in SESSION, standing as of [t s]."
  [offer agent session t s]
  (fresh []
    (kindo offer :offer)
    (seat offer agent session)
    (in-forceo offer t s)))

(defn agreemento
  "ACCEPT made an agreement with OFFER: the acceptance targets an offer that
   was standing in the acceptance's own seat when it was made."
  [accept offer]
  (fresh [agent session at sys]
    (kindo accept :accept)
    (target accept offer)
    (seat accept agent session)
    (act accept :accept (l/lvar) at sys)
    (visible-offero offer agent session at sys)))

(defn obligationo
  "DEBTOR owes CREDITOR under SOURCE as of [t s]: a standing promise that is
   not closed, or a standing agreement (the offeror owes the acceptor)."
  [debtor creditor source t s]
  (conde
   [(kindo source :promise)
    (in-forceo source t s)
    (l/nafc closedo source t s)
    (authoro source debtor)
    (beneficiary source creditor)]
   [(fresh [offer]
      (kindo source :accept)
      (in-forceo source t s)
      (l/nafc closedo source t s)
      (agreemento source offer)
      (authoro offer debtor)
      (authoro source creditor))]))

(defn authorityo
  "CALLER may perform acts of KIND as of [t s]: a standing grant says so."
  [caller kind t s]
  (fresh [g]
    (kindo g :grant)
    (grantee g caller)
    (scope g kind)
    (in-forceo g t s)))

(defn answerso
  "R answers A, explicitly or by inference, with EFFECT from the adjacency
   table (:closes or :keeps). A target or carries-out link whose pair the
   table does not list closes the port: the act bore on it directly."
  [r a effect]
  (fresh [ko kr]
    (conde [(answers r a)] [(answers-inferred r a)])
    (kindo a ko)
    (kindo r kr)
    (l/project [ko kr] (== effect (or (answer-effect ko kr) :closes)))))

(defn closes-porto
  "A visible act closes the port A opened, unless that act was itself
   reversed: an undone withdrawal leaves the move unanswered again."
  [a t s]
  (fresh [r]
    (answerso r a :closes)
    (visibleo r t s)
    (l/nafc reversedo r t s)))

(defn openo
  "A is an open port as of [t s]: a visible opening act with no visible
   closing answer. The horizontal counterpart of in-forceo: in-forceo asks
   whether a standing was cancelled, openo asks whether a move was answered."
  [a t s]
  (fresh [k]
    (kindo a k)
    (l/membero k (vec opening-kinds))
    (visibleo a t s)
    (l/nafc closes-porto a t s)))

(defn derivationo
  "R derives from A: A is cited by R, or carried out by R, or derives from
   something R derives from. The acts an answer was built from."
  [r a]
  (conde
   [(cites r a)]
   [(carries-out r a)]
   [(fresh [m]
      (conde [(cites r m)] [(carries-out r m)])
      (derivationo m a))]))

;; ---------------------------------------------------------------------------
;; Answers (plain data, for tests and for the differential checks)

(defn- as-of [t s] [(epoch t) (epoch (or s t))])

(defn in-force
  "Ids of everything in force as of T (and system time S, default T)."
  [db t & [s]]
  (let [[t s] (as-of t s)]
    (set (pldb/with-db db (run* [x] (in-forceo x t s))))))

(defn promulgated
  [db t & [s]]
  (let [[t s] (as-of t s)]
    (set (pldb/with-db db (run* [x] (promulgatedo x t s))))))

(defn obligations
  "[{:debtor :creditor :source}] as of T."
  [db t & [s]]
  (let [[t s] (as-of t s)]
    (set (pldb/with-db db
           (run* [q]
             (fresh [d c src]
               (obligationo d c src t s)
               (== q {:debtor d :creditor c :source src})))))))

(defn agreement-status
  "For an acceptance act: {:status :agreed :offer ..} or {:status :refused
   :reason :no-visible-offer}, as the agreement route answers."
  [db accept]
  (let [offers (pldb/with-db db (run* [o] (agreemento accept o)))]
    (if (seq offers)
      {:status :agreed :offer (first offers)}
      {:status :refused :reason :no-visible-offer})))

(defn derivation
  "The set of acts R was built from."
  [db r]
  (set (pldb/with-db db (run* [a] (derivationo r a)))))

(defn authority
  [db caller kind t & [s]]
  (let [[t s] (as-of t s)]
    (boolean (seq (pldb/with-db db (run* [q] (authorityo caller kind t s) (== q true)))))))

(defn by-intent
  "The acts of HISTORY grouped by kind: which intents a fixture exercises."
  [history]
  (into (sorted-map) (frequencies (map :kind history))))

(defn open-ports
  "Ids of the ports open as of T (and system time S)."
  [db t & [s]]
  (let [[t s] (as-of t s)]
    (set (pldb/with-db db (run* [a] (openo a t s))))))

(defn- deposits
  "What an act leaves behind that outlives the conversation: a standing
   created, ended, restored or closed, or a commit."
  [{:keys [id kind target carries-out sha]}]
  (case (kinds kind)
    :creates [[:created id]]
    :cancels [[:ended target]]
    :reverses [[:restored target]]
    :closes [[:closed target]]
    (cond-> []
      (= kind :commit) (conj [:commit (or sha id) (let [c carries-out] (if (sequential? c) (vec c) (when c [c])))]))))

(defn infer-links
  "For acts with no explicit answer whose kind can answer something: the
   open port, at the act's own time, that the posterior MATRIX
   ({opener-kind {answer-kind p}}) makes most probable, when p >= THRESHOLD.
   Returns [[answer-id opener-id p] ...]. Classical: no model, only the
   table tuned on counts."
  [history matrix & {:keys [threshold] :or {threshold 0.2}}]
  (let [explicit (set (map first (explicit-answers history)))
        db0 (db history)
        by-id (into {} (map (juxt :id identity) history))]
    (vec
     (for [{:keys [id kind at sys] :as a} (sort-by #(epoch (:at %)) history)
           :when (not (explicit id))
           :let [t (epoch at) s (epoch (or sys at))
                 open (disj (open-ports db0 t s) id)
                 scored (for [o open
                              :let [p (get-in matrix [(:kind (by-id o)) kind] 0.0)]
                              :when (>= p threshold)]
                          [o p])]
           :when (seq scored)
           :let [[o p] (apply max-key second (sort-by first scored))]]
       [id o p]))))

(defn flow
  "The exchange as a flow: for each act in time order, what it opened, what
   it answered and how (:explicit or :inferred), which ports it closed, the
   ports open after it, and what it deposited. OPTS :matrix enables
   inferred links (see `infer-links`)."
  [history & {:keys [matrix threshold] :or {threshold 0.2}}]
  (let [inferred (when matrix (infer-links history matrix :threshold threshold))
        inferred-by (into {} (map (fn [[r a p]] [r [a p]]) inferred))
        linked (db history (map (fn [[r a _]] [r a]) inferred))
        explicit (group-by first (explicit-answers history))]
    (vec
     (for [{:keys [id kind author at sys] :as a} (sort-by #(epoch (:at %)) history)
           :let [t (epoch at) s (epoch (or sys at))
                 answered (vec (map second (explicit id)))
                 [io ip] (inferred-by id)
                 links (cond-> (mapv (fn [o] {:opener o :link :explicit :effect (or (answer-effect (:kind (first (filter #(= o (:id %)) history))) kind) :closes)}) answered)
                         io (conj {:opener io :link :inferred :p ip :effect (or (answer-effect (:kind (first (filter #(= io (:id %)) history))) kind) :closes)}))
                 open-before (open-ports linked (dec t) s)
                 open-after (open-ports linked t s)]]
       {:id id :kind kind :author author :at at
        :opens (boolean (and (opening-kinds kind) (contains? open-after id)))
        :answers links
        :closes (vec (sort (clojure.set/difference open-before open-after)))
        :open-after (vec (sort open-after))
        :deposits (deposits a)
        :annotative (and (not (opening-kinds kind)) (empty? links) (empty? (deposits a)))}))))

(defn transition-counts
  "Evidence of transitions in HISTORY: {:consecutive {[k1 k2] n} :explicit
   {[opener answer] n}}, the two matrices the census tunes the adjacency
   prior with. Consecutive pairs are taken within a seat's session."
  [history]
  (let [by-session (group-by (juxt :agent :session) history)
        consecutive (for [[_ acts] by-session
                          [a b] (partition 2 1 (sort-by #(epoch (:at %)) acts))]
                      [(:kind a) (:kind b)])
        kind-of (into {} (map (juxt :id :kind) history))
        explicit (for [[r a] (explicit-answers history)] [(kind-of a) (kind-of r)])]
    {:consecutive (frequencies consecutive)
     :explicit (frequencies explicit)}))

(defn posterior
  "The transition matrix {opener {answer p}} from COUNTS ({[opener answer] n})
   with the adjacency table as PRIOR pseudo-counts (ALPHA each)."
  [counts & {:keys [alpha] :or {alpha 1.0}}]
  (let [pseudo (for [[o answers] adjacency [a _] answers] [[o a] alpha])
        all (reduce (fn [m [k n]] (update m k (fnil + 0) n)) {} (concat pseudo counts))
        by-opener (group-by (comp first key) all)]
    (into {} (for [[o rows] by-opener
                   :let [total (reduce + (map val rows))]]
               [o (into {} (for [[[_ a] n] rows] [a (/ (double n) total)]))]))))
