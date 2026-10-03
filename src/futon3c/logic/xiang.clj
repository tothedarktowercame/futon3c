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

(defn db
  "A pldb database for HISTORY."
  [history]
  (apply pldb/db (facts history)))

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
