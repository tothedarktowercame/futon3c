(ns zaif-cascade-gate
  "The RUN of worklist row `:LA5` (futon3/holes/labs/library-contract/worklist.edn):
  one seat's recorded task, played once by the constructed cascade and once by the
  v0 arm chooser, on identical inputs.

  WHAT THIS GATE IS FOR, in the row's own words: \"One seat given a constructed
  cascade for one task, REVIEWED BEFORE THE RUN by someone other than its
  constructor ... The comparison is against the v0 arm chooser on the same task,
  and a null result is a delivery.\"  The cascade this reads was constructed and
  committed FIRST (futon3 37f7506, `checks/zaif-cascade.edn`) and reviewed before
  this file existed.  That ordering is the point of the row and it is visible in
  git, not asserted here.

  WHY THE COMPARISON IS PAIRED AND NOT SIMULATED.  `futon3c` recorded 11,874
  `:zaif-arm-choice` evidence entries between 2026-07-22 and 2026-08-09, each
  carrying the v0 controller's arm, its four G terms, and the FULL
  `:inputs-snapshot` it decided from.  So the cascade does not run against a
  reconstruction of what v0 saw; it runs against exactly what v0 saw, and v0's
  answer is read off the record rather than recomputed.  `:v0-rederivation` below
  recomputes it anyway, from the recorded inputs through the real
  `futon3c.agents.zaif-controller/decide`, and a disagreement aborts the gate:
  that is the apparatus control, and it is the analogue of
  `cascade_authority_gate.clj`'s `efe-is-connected` -- a gate whose inputs are not
  connected to the thing under test can only produce a null that refutes itself.

  THE COHORT is `holes/zaif-cohort-a97J05.edn`, a snapshot of one turn:
  `zai-turn-f1513520-b760-4b67-bbd1-b2941d3e0325`, agent `zai-4`, 2026-08-06, a
  PASS-1 Lean proving packet for problem `a97J05`.  103 round numbers, 102 with a
  transcript, 50 with a shipped v0 decision.  Every row carries its own
  `:evidence-id` so a reviewer can check the snapshot against the live store.

  WHAT THE SEAT ACTUALLY DID, which is the oracle: the turn ran to round 102 with
  `:final false` -- it exhausted its tool rounds and never produced a report.  The
  task it was given states a budget in its own text (`Budget ~15-25 minutes per
  problem; move on when the budget is spent`) and states that stopping is
  permitted (`Skipping honestly is BETTER than a forced fake`).

  THE PLAY-GRAIN RULE TABLE, and the two things that keep it from being fitted.
  A cascade member is a pattern, and a pattern acts only through its own THEN.
  `rule-table` below encodes four THENs, each citing a line span re-read from disk
  on every run (`then-correspondence`).  Two of the four are patterns the
  constructor SELECTED; two are patterns it did NOT, and those two are run only in
  an arm labelled `:counterfactual` and are never counted as a result.  Their
  presence is the un-fitting guard: a table written to make the cascade win would
  not carry rules for patterns the cascade does not contain.

  Members with no rule emit nothing.  That is `fire`'s `firstSome`
  (`find_organise.clj:398`), not a gap: a pattern whose THEN says nothing about
  what to do at a decision point has no `then` for a Situation of this type.  The
  report counts them, because how many members of a constructed cascade can act at
  all is the measurement this domain makes and the other three cannot."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [find-organise :as fo]
            [futon3c.agents.zaif-controller :as zaif]))

(def cohort-path "holes/zaif-cohort-a97J05.edn")
(def cascade-path "../futon3/checks/zaif-cascade.edn")
(def report-path "holes/zaif-cascade-gate.edn")
(def library-root "../futon3/library")

(def temperament-under-test
  "The FLOOR temperament.  `construct_zaif_cascade.clj` reports both; the budget
   one halts before admitting anything (its seed of 38 already exceeds the
   transplanted budget of 20), so the floor arm is the one whose membership the
   constructor actually decided.  `construct_ants_cascade.clj` chose the floor arm
   for the same reason."
  :widen-to-the-marginal-gain-floor)

;; ---------------------------------------------------------------------------
;; the situation
;; ---------------------------------------------------------------------------

(def read-only-tools
  "Tools whose call reads and does not write.  `run_shell` is NOT here: it runs
   `sed -n` and `git commit` alike and cannot be classified from the tool name.
   The oracle below reports how many rounds turn on it rather than deciding for
   them silently."
  #{:read_file :search :memory_search :psr_search :grep :list_files})

(def store-tools #{:memory_search :psr_search})
(def write-tools #{:edit_file :write_file :apply_patch})

(defn situations
  "One play-grain Situation per round, built by walking the cohort forward.  Every
   field is either a count over the ROUNDS ALREADY RECORDED or a field of the v0
   decision recorded for this round; nothing is derived from what happened later."
  [cohort]
  (let [budget (get-in cohort [:as-of :tool-round-budget])]
    (:rows
     (reduce
      (fn [acc {:keys [round v0 transcript]}]
        (let [calls (vec (:calls transcript))
              store? (boolean (some store-tools calls))
              wrote? (boolean (some write-tools calls))
              snap (:inputs-snapshot v0)
              gamma (let [g (:gamma snap)]
                      ;; the HTTP JSON view cannot carry a map with a nil key, so
                      ;; the store's `:gamma` arrives pr-str'd.  Read it back
                      ;; rather than passing a string to `gamma-for-mission`,
                      ;; where it would silently fall through to the 1.0 prior.
                      (if (string? g) (try (edn/read-string g) (catch Exception _ {})) (or g {})))
              s {:grain :play
                 :round round
                 :rounds-so-far (dec round)
                 :round-budget budget
                 :store-consulted? (:store-consulted? acc)
                 :edits-so-far (:edits acc)
                 :rounds-since-last-edit (- (dec round) (:last-edit-round acc))
                 :read-only-calls-so-far (:reads acc)
                 :store-consulted-this-round? store?
                 :calls calls
                 :final? (boolean (:final transcript))
                 :has-transcript? (some? transcript)
                 :has-v0? (some? v0)
                 :v0-arm (:arm v0)
                 :v0-g-terms (:g-terms v0)
                 :v0-evidence-id (:evidence-id v0)
                 :inputs (when snap
                           {:task-belief (or (:task-belief snap) {})
                            :c-belief (or (:c-belief snap) {})
                            :gamma gamma
                            :mission (:mission snap)
                            :observations (or (:observations snap) {})})
                 :operator-c-uncertainty (get-in snap [:c-belief :operator-c-uncertainty])}]
          (-> acc
              (update :rows conj s)
              (assoc :store-consulted? (or (:store-consulted? acc) store?))
              (update :edits + (count (filter write-tools calls)))
              (update :reads + (count (filter read-only-tools calls)))
              (assoc :last-edit-round (if wrote? round (:last-edit-round acc))))))
      {:rows [] :store-consulted? false :edits 0 :reads 0 :last-edit-round 0}
      (:rounds cohort)))))

;; ---------------------------------------------------------------------------
;; the play-grain rule table
;; ---------------------------------------------------------------------------
;; Four rules.  Each `:id` is a LIBRARY PATTERN ID and each `:then-source` a line
;; span of that pattern's own `+ THEN:` block, re-read from disk by
;; `then-correspondence` on every run.  `:encodes` quotes the clause of the THEN
;; the rule encodes, so a reader can check the encoding against the quote and the
;; quote against the file.
;;
;; `:in-the-cascade?` is NOT written here -- it is computed against
;; `zaif-cascade.edn` at run time, so a rule cannot claim membership it does not
;; have.

(def rule-table
  [{:id :war-machine/ambient-pattern-retrieval
    :then-source "war-machine/ambient-pattern-retrieval.flexiarg:16-16"
    :encodes "\"Search the library on every turn as a background process.\""
    :if (fn [s] (and (= :play (:grain s)) (:has-transcript? s)))
    ;; the counter-force the pattern's own HOWEVER names is notification
    ;; fatigue -- "low-relevance patterns keep firing" -- so this fires while the
    ;; store has NOT yet been consulted in the turn and falls silent afterwards.
    :however (fn [s] (not (:store-consulted? s)))
    :then (fn [_] :retrieve)}

   {:id :math-strategy/missing-dependency-protocol
    :then-source "math-strategy/missing-dependency-protocol.flexiarg:59-77"
    :encodes "\"Search once more against the installed source ... and look for an adapter to an existing theorem before building\"; \"Put exactly one `sorry` at the observed gap ... Beside that `sorry`, leave the consultation trail\"; \"stop researching once the misses repeat\""
    :if (fn [s] (and (= :play (:grain s)) (:store-consulted? s)))
    ;; "once more", then the trail.  The rule is live when the seat has gone more
    ;; rounds than that without an edit landing: the misses have repeated.
    :however (fn [s] (> (:rounds-since-last-edit s) 1))
    :then (fn [_] :act)}

   ;; --- NOT SELECTED BY THE CONSTRUCTOR.  Counterfactual arm only. ---
   {:id :agent/budget-bounds-exploration
    :counterfactual? true
    :then-source "agent/budget-bounds-exploration.flexiarg:19-19"
    :encodes "\"When budget nears exhaustion, force a decision: commit to best-so-far, escalate to a supervisor, or explicitly expand scope with justification.\""
    :if (fn [s] (and (= :play (:grain s)) (some? (:round-budget s))))
    ;; "nears exhaustion" is read off the RUNNER's own declared constant --
    ;; `tool-round-budget` 24, futon3c/src/futon3c/agents/zai_api.clj:1033-1037 --
    ;; and not off this cohort.  A threshold chosen from the data would make the
    ;; round it fires on a property of the data.
    :however (fn [s] (>= (:rounds-so-far s) (:round-budget s)))
    :then (fn [_] :yield)}

   {:id :agent/pause-is-not-failure
    :counterfactual? true
    :then-source "agent/pause-is-not-failure.flexiarg:19-19"
    :encodes "\"Treat 'pause and ask' as a first-class action with defined triggers: ... confidence below threshold, resource budget near exhaustion.\""
    :if (fn [s] (= :play (:grain s)))
    :however (fn [s] (or (>= (:rounds-so-far s) (:round-budget s))
                         (>= (double (or (:operator-c-uncertainty s) 0.0)) 0.5)))
    :then (fn [_] :ask)}])

(defn then-correspondence
  "Each rule's `:then-source` must name a pattern the library holds, and its cited
   span must lie inside that pattern's `+ THEN:` block, re-read from disk.  Without
   this the table is four Clojure closures that only LOOK like an encoding of four
   authored THENs -- the facade LA1c-restatement.md §11 names, one level below the
   firing loop."
  []
  (mapv (fn [{:keys [id then-source encodes counterfactual?]}]
          (let [[rel span] (str/split then-source #":")
                [from to] (mapv parse-long (str/split span #"-"))
                f (io/file library-root rel)
                lines (when (.exists f) (vec (str/split-lines (slurp f))))
                then-line (when lines
                            (first (keep-indexed (fn [i l] (when (re-find #"^\s*\+ THEN:" l) (inc i)))
                                                 lines)))
                next-block (when (and lines then-line)
                             (or (first (keep-indexed
                                         (fn [i l] (when (and (> (inc i) then-line)
                                                              (re-find #"^\s*\+ [A-Z-]+:" l))
                                                     (inc i)))
                                         lines))
                                 (inc (count lines))))]
            (sorted-map
             :rule id :counterfactual? (boolean counterfactual?)
             :then-source then-source :encodes encodes
             :file-exists? (some? lines)
             :pattern-id-matches-path?
             (= id (keyword (str/replace rel #"\.flexiarg$" "")))
             :then-block [then-line next-block]
             :span-inside-the-then-block?
             (boolean (and then-line next-block from to
                           (> from then-line) (< to next-block)))
             :span-is-non-empty?
             (boolean (and lines from to (<= to (count lines))
                           (seq (str/trim (str/join " " (subvec lines (dec from) to)))))))))
        rule-table))

;; ---------------------------------------------------------------------------
;; the arms
;; ---------------------------------------------------------------------------

(defn- precedence-of-fn
  "Rules are ordered by the CASCADE's precedence, least first -- the same field
   `fo/ordered` reads.  A rule for a pattern the cascade does not carry sorts last."
  [precedence]
  (fn [r] (get precedence (:id r) Long/MAX_VALUE)))

(defn play
  "One arm.  `fo/fire` (find_organise.clj:398) at play grain over `rules`, ordered
   by `precedence`.  When nothing fires the round DEFERS to v0 -- the cascade is a
   carried program that intervenes where it has something to say, which is what
   makes the sham arm below a real control: a cascade with no live rule reproduces
   v0 exactly, round for round."
  [rules precedence sits]
  (mapv (fn [s]
          (let [[rule emitted] (fo/fire rules (precedence-of-fn precedence) s)]
            {:round (:round s)
             :fired (:id rule)
             :arm (or emitted (:v0-arm s))
             :deferred? (nil? emitted)}))
        sits))

(defn oracle
  "What the seat actually did that round, read off the persisted transcript.
   Deliberately coarse, and its coarseness is reported: `run_shell` runs reads and
   writes alike and cannot be classified from the tool name, so a round whose only
   calls are `run_shell` is counted in `:uncertain` and its `:retrieve` label is a
   CHOICE this file makes, not a fact the record carries."
  [s]
  (let [calls (:calls s)]
    (cond
      (some write-tools calls) :act
      (and (:final? s) (empty? calls)) :yield
      :else :retrieve)))

(defn oracle-uncertain? [s]
  (and (seq (:calls s)) (every? #(= :run_shell %) (:calls s))))

(defn- dist [xs] (into (sorted-map) (frequencies xs)))

(defn- agreement [plays sits]
  (let [pairs (remove nil? (map (fn [p s] (when (:has-transcript? s) [(:arm p) (oracle s)])) plays sits))]
    (sorted-map :rounds (count pairs)
                :agree (count (filter (fn [[a o]] (= a o)) pairs))
                :confusion (into (sorted-map) (frequencies pairs)))))

;; ---------------------------------------------------------------------------
;; the report
;; ---------------------------------------------------------------------------

(defn v0-rederivation
  "THE APPARATUS CONTROL.  Recompute v0's arm from the recorded `:inputs-snapshot`
   through the real controller and compare to the recorded arm.  A disagreement
   means the cohort is not what it says it is and nothing downstream is
   interpretable, so this is a hard failure and not a reported number."
  [sits]
  (let [rows (for [s sits :when (and (:has-v0? s) (:inputs s))]
               (let [d (zaif/decide (:inputs s))]
                 {:round (:round s) :recorded (:v0-arm s) :rederived (:arm d)
                  :g-terms-match? (= (:g-terms s) (:g-terms d))}))]
    (sorted-map
     :rounds (count rows)
     :arm-disagreements (vec (for [r rows :when (not= (:recorded r) (:rederived r))] r))
     :g-term-disagreements (count (remove :g-terms-match? rows)))))

(defn report []
  (let [cohort (edn/read-string (slurp cohort-path))
        cascade-report (edn/read-string (slurp cascade-path))
        cascade (get-in cascade-report [:runs temperament-under-test :cascade])
        members (set (:members cascade))
        precedence (:precedence cascade)
        sits (situations cohort)
        selected-rules (remove :counterfactual? rule-table)
        cascade-rules (filter #(contains? members (:id %)) selected-rules)
        ;; THE SHAM.  The F4 falsifier alone, and it carries no rule, so no rule
        ;; can fire: the arm must reproduce v0 exactly on every round.  A sham
        ;; that differed anywhere would mean `play` is not deferring where it says
        ;; it defers, and the treatment's differences would not be the cascade's.
        sham-rules []
        counterfactual-rules (concat cascade-rules (filter :counterfactual? rule-table))
        v0-plays (mapv (fn [s] {:round (:round s) :fired nil :arm (:v0-arm s) :deferred? true}) sits)
        c-plays (play cascade-rules precedence sits)
        s-plays (play sham-rules precedence sits)
        x-plays (play counterfactual-rules
                      (merge precedence
                             ;; a counterfactual rule's pattern is not in the
                             ;; cascade and so has no precedence in it; it sorts
                             ;; after every member rather than being given one.
                             (zipmap (map :id (filter :counterfactual? rule-table))
                                     (repeat (inc (count members)))))
                      sits)
        with-t (fn [ps] (keep-indexed (fn [i p] (when (:has-transcript? (nth sits i)) p)) ps))
        differs (fn [ps] (vec (for [[c v s] (map vector ps v0-plays sits)
                                    :when (and (:has-transcript? s) (not= (:arm c) (:arm v)))]
                                {:round (:round s) :cascade (:arm c) :v0 (:arm v)
                                 :fired (:fired c) :oracle (oracle s)})))
        ;; O4: exchange the precedence of the two members that carry a rule.  The
        ;; six numbers only; `construct_zaif_cascade.clj` applies the predicate.
        two (mapv :id cascade-rules)
        o4-exercisable? (>= (count two) 2)
        swapped (if o4-exercisable?
                  (assoc precedence (first two) (get precedence (second two))
                         (second two) (get precedence (first two)))
                  precedence)
        o4-plays (play cascade-rules swapped sits)
        acting-order (fn [ps] (vec (keep :fired ps)))]
    (sorted-map
     :as-of (sorted-map
             :cohort cohort-path
             :cohort-turn (get-in cohort [:as-of :turn-id])
             :cohort-agent (get-in cohort [:as-of :agent])
             :cascade cascade-path
             :cascade-commit "futon3 37f7506 -- constructed and committed BEFORE this gate existed"
             :temperament temperament-under-test
             :cascade-members (count members)
             :rounds (count sits)
             :rounds-with-a-transcript (count (filter :has-transcript? sits))
             :rounds-with-a-v0-decision (count (filter :has-v0? sits)))
     :controls (sorted-map
                :v0-rederivation (v0-rederivation sits)
                :sham-ties-v0-exactly? (= (mapv :arm s-plays) (mapv :arm v0-plays))
                :sham-rounds-that-differ (count (remove true? (map #(= (:arm %1) (:arm %2))
                                                                   s-plays v0-plays)))
                :then-correspondence (then-correspondence)
                :grain-separation
                (vec (for [r rule-table
                           :let [policy {:grain :policy :members #{} :candidates {}
                                         :budget 20 :epsilon 1.0 :round-budget 24
                                         :rounds-so-far 99 :rounds-since-last-edit 9
                                         :store-consulted? true :has-transcript? true}]
                           :when (try (fo/fires? r policy) (catch Exception _ false))]
                       (:id r)))
                :rules-in-the-cascade (mapv :id cascade-rules)
                :rules-not-in-the-cascade
                (mapv :id (remove #(contains? members (:id %)) rule-table))
                :members-with-no-rule
                (count (remove (set (map :id rule-table)) members)))
     :arms (sorted-map
            :v0 (sorted-map :arms (dist (map :arm (with-t v0-plays)))
                            :agreement-with-the-oracle (agreement v0-plays sits))
            :cascade (sorted-map :arms (dist (map :arm (with-t c-plays)))
                                 :fired (dist (keep :fired (with-t c-plays)))
                                 :rounds-that-fired (count (filter :fired (with-t c-plays)))
                                 :agreement-with-the-oracle (agreement c-plays sits))
            :sham (sorted-map :arms (dist (map :arm (with-t s-plays)))
                              :agreement-with-the-oracle (agreement s-plays sits))
            :counterfactual
            (sorted-map
             :NOT-A-RESULT
             "these two rules encode patterns the constructor did NOT select. The arm exists to measure what the cascade would have done had find returned them, and to keep the rule table from being fitted to the cascade. It is not evidence about the constructor."
             :arms (dist (map :arm (with-t x-plays)))
             :fired (dist (keep :fired (with-t x-plays)))
             :first-round-that-fired (:round (first (filter :fired x-plays)))
             :agreement-with-the-oracle (agreement x-plays sits)))
     :comparison (sorted-map
                  :cascade-vs-v0-rounds-that-differ (differs c-plays)
                  :cascade-vs-v0-differing-round-count (count (differs c-plays))
                  :counterfactual-vs-v0-differing-round-count (count (differs x-plays))
                  :null? (zero? (count (differs c-plays))))
     :oracle (sorted-map
              :classifier "act iff a write tool was called; yield iff the round is final with no calls; retrieve otherwise"
              :distribution (dist (map oracle (filter :has-transcript? sits)))
              :uncertain-rounds (count (filter oracle-uncertain? (filter :has-transcript? sits)))
              :uncertain-note "rounds whose only calls are run_shell. run_shell runs reads and writes alike, so their :retrieve label is this file's choice and not a fact the record carries."
              :turn-outcome
              (let [last-s (last (filter :has-transcript? sits))]
                (sorted-map :last-round (:round last-s)
                            :final? (:final? last-s)
                            :reading (if (:final? last-s)
                                       :the-seat-reported
                                       :the-seat-exhausted-its-rounds-without-reporting))))
     :o4 (if-not o4-exercisable?
           (sorted-map
            :exercised? false
            :reading :not-exercised-fewer-than-two-members-carry-a-play-grain-rule
            :members-carrying-a-rule two
            :note "O4 is a law about a run: precedence changed => acting order or score changed. With one rule there is no pair to exchange, so the law is recorded as not exercised rather than passed on a swap that changes nothing. :LA3 recorded it as :not-exercised-nothing-is-played for the adjacent reason.")
           (sorted-map
          :exercised? true
          :precedence-before (mapv #(get precedence % ) two)
          :precedence-after (mapv #(get swapped %) two)
          :acting-order-before (acting-order (with-t c-plays))
          :acting-order-after (acting-order (with-t o4-plays))
          :score-before (:agree (agreement c-plays sits))
          :score-after (:agree (agreement o4-plays sits))
          :score-is "rounds on which the arm agrees with the transcript oracle")))))

(defn require-pass! [result]
  (let [c (:controls result)
        failures
        (concat
         (for [d (get-in c [:v0-rederivation :arm-disagreements])]
           {:where :controls :finding :v0-does-not-rederive-from-its-own-recorded-inputs :round (:round d)})
         (when (pos? (long (get-in c [:v0-rederivation :g-term-disagreements] 0)))
           [{:where :controls :finding :v0-g-terms-do-not-rederive
             :n (get-in c [:v0-rederivation :g-term-disagreements])}])
         (when (zero? (long (get-in c [:v0-rederivation :rounds] 0)))
           [{:where :controls :finding :no-round-carries-a-v0-decision-to-compare-against}])
         (when-not (:sham-ties-v0-exactly? c)
           [{:where :controls :finding :sham-does-not-tie-v0
             :rounds (:sham-rounds-that-differ c)}])
         (for [t (:then-correspondence c)
               :when (not (and (:file-exists? t) (:pattern-id-matches-path? t)
                               (:span-inside-the-then-block? t) (:span-is-non-empty? t)))]
           {:where :controls :finding :rule-does-not-encode-an-authored-then :rule (:rule t)})
         (for [r (:grain-separation c)]
           {:where :controls :finding :grain-leak :rule r})
         (when (empty? (:rules-not-in-the-cascade c))
           [{:where :controls :finding :rule-table-is-fitted-to-the-cascade}])
         (when (empty? (:rules-in-the-cascade c))
           [{:where :controls :finding :no-cascade-member-carries-a-rule}]))]
    (when (seq failures)
      (throw (ex-info "zaif-cascade-gate: control failed"
                      {:finding (:finding (first failures)) :failures (vec failures)})))
    result))

(def coverage-path "holes/zaif-cascade-coverage.edn")

(defn coverage
  "THE SECOND PRE-RUN REVIEW PACKET.  Everything a reviewer needs to gate the
   rule table -- which members carry a rule, which rounds each rule's antecedent
   holds on, what it would emit, and whether two rules ever contend -- and NOTHING
   about v0, the oracle, or agreement.  The first review (codex-17, 2026-09-02)
   asked for exactly this: \"deterministic coverage counts over all recorded
   rounds without revealing comparative outcomes\".  Withholding the outcomes is
   not politeness: the rule table must not be revised after its author has seen
   how it scores, and the reviewer must not be gating a table whose score they
   already know."
  []
  (let [cohort (edn/read-string (slurp cohort-path))
        cascade-report (edn/read-string (slurp cascade-path))
        cascade (get-in cascade-report [:runs temperament-under-test :cascade])
        members (set (:members cascade))
        precedence (:precedence cascade)
        sits (situations cohort)
        with-t (filter :has-transcript? sits)
        fires-on (fn [r] (vec (for [s with-t :when (try (fo/fires? r s) (catch Exception _ false))]
                                (:round s))))]
    (sorted-map
     :as-of (sorted-map
             :cohort-turn (get-in cohort [:as-of :turn-id])
             :cascade cascade-path
             :temperament temperament-under-test
             :cascade-members (vec (sort members))
             :rounds-with-a-transcript (count with-t)
             :withheld "v0 arms, the transcript oracle, and every agreement or comparison number. This packet is for gating the rule table, not for reading a result off it.")
     :rules (mapv (fn [r]
                    (let [rounds (fires-on r)]
                      (sorted-map
                       :id (:id r)
                       :counterfactual? (boolean (:counterfactual? r))
                       :in-the-cascade? (contains? members (:id r))
                       :precedence (get precedence (:id r) :not-a-member)
                       :then-source (:then-source r)
                       :encodes (:encodes r)
                       :emits ((:then r) (first with-t))
                       :antecedent-holds-on-rounds rounds
                       :antecedent-holds-on-n-rounds (count rounds)
                       :first-round (first rounds)
                       :last-round (last rounds))))
                  rule-table)
     :contention
     (let [live (remove :counterfactual? rule-table)]
       (sorted-map
        :rounds-where-more-than-one-cascade-rule-fires
        (vec (for [s with-t
                   :let [f (filter #(and (contains? members (:id %))
                                         (try (fo/fires? % s) (catch Exception _ false)))
                                   live)]
                   :when (> (count f) 1)]
               {:round (:round s) :rules (mapv :id f)}))
        :conflict-policy "fo/fire (find_organise.clj:398) sorts by the cascade's own precedence, least first, and takes the FIRST rule whose THEN returns a value. There is no second policy and no tie-break of this file's own: a contention is decided by the precedence the constructor wrote, which is what makes O4 a law about this run rather than a property of the table."))
     :members-with-no-rule (vec (sort (remove (set (map :id rule-table)) members)))
     :controls (sorted-map
                :then-correspondence (then-correspondence)
                :grain-separation
                (vec (for [r rule-table
                           :let [policy {:grain :policy :members #{} :candidates {}
                                         :budget 20 :epsilon 1.0 :round-budget 24
                                         :rounds-so-far 99 :rounds-since-last-edit 9
                                         :store-consulted? true :has-transcript? true}]
                           :when (try (fo/fires? r policy) (catch Exception _ false))]
                       (:id r)))
                :rules-not-in-the-cascade
                (mapv :id (remove #(contains? members (:id %)) rule-table))))))

(declare -run-gate)

(defn -main [& args]
  (if (= "coverage" (first args))
    (let [c (coverage)]
      (spit coverage-path (with-out-str (pprint/pprint c)))
      (println (format "coverage over %d rounds of %s; cascade %s has %d members, %d with no rule"
                       (get-in c [:as-of :rounds-with-a-transcript])
                       (get-in c [:as-of :cohort-turn])
                       (name (get-in c [:as-of :temperament]))
                       (count (get-in c [:as-of :cascade-members]))
                       (count (:members-with-no-rule c))))
      (doseq [r (:rules c)]
        (println (format "  %-48s in-cascade %-5s cf %-5s emits %-9s fires on %d rounds (first %s last %s)"
                         (:id r) (:in-the-cascade? r) (:counterfactual? r) (:emits r)
                         (:antecedent-holds-on-n-rounds r) (:first-round r) (:last-round r))))
      (println (format "contentions: %d; grain leaks %d; rules not in the cascade %s"
                       (count (get-in c [:contention :rounds-where-more-than-one-cascade-rule-fires]))
                       (count (get-in c [:controls :grain-separation]))
                       (pr-str (get-in c [:controls :rules-not-in-the-cascade]))))
      (println (format "wrote %s -- NOTHING about v0, the oracle or agreement is in it" coverage-path))
      (shutdown-agents)
      (System/exit 0))
    (-run-gate)))

(defn -run-gate []
  (try
    (let [result (require-pass! (report))]
      (spit report-path (with-out-str (pprint/pprint result)))
      (let [a (:as-of result) c (:controls result) arms (:arms result) cmp (:comparison result)]
        (println (format "cohort %s: %d rounds, %d with a transcript, %d with a v0 decision"
                         (:cohort-turn a) (:rounds a) (:rounds-with-a-transcript a)
                         (:rounds-with-a-v0-decision a)))
        (println (format "cascade %s: %d members, %d carry a play-grain rule, %d do not"
                         (name (:temperament a)) (:cascade-members a)
                         (count (:rules-in-the-cascade c)) (:members-with-no-rule c)))
        (println (format "CONTROL v0 re-derives from its own recorded inputs on %d/%d rounds, %d arm and %d g-term disagreements"
                         (- (long (get-in c [:v0-rederivation :rounds]))
                            (count (get-in c [:v0-rederivation :arm-disagreements])))
                         (get-in c [:v0-rederivation :rounds])
                         (count (get-in c [:v0-rederivation :arm-disagreements]))
                         (get-in c [:v0-rederivation :g-term-disagreements])))
        (println (format "CONTROL sham ties v0 exactly? %s (%d rounds differ)"
                         (:sham-ties-v0-exactly? c) (:sham-rounds-that-differ c)))
        (println (format "v0        arms %s, agrees with the oracle on %d/%d"
                         (pr-str (get-in arms [:v0 :arms]))
                         (get-in arms [:v0 :agreement-with-the-oracle :agree])
                         (get-in arms [:v0 :agreement-with-the-oracle :rounds])))
        (println (format "cascade   arms %s, fired on %d rounds %s, agrees with the oracle on %d/%d"
                         (pr-str (get-in arms [:cascade :arms]))
                         (get-in arms [:cascade :rounds-that-fired])
                         (pr-str (get-in arms [:cascade :fired]))
                         (get-in arms [:cascade :agreement-with-the-oracle :agree])
                         (get-in arms [:cascade :agreement-with-the-oracle :rounds])))
        (println (format "counterfactual (NOT A RESULT) arms %s, first fired round %s, agrees on %d/%d"
                         (pr-str (get-in arms [:counterfactual :arms]))
                         (pr-str (get-in arms [:counterfactual :first-round-that-fired]))
                         (get-in arms [:counterfactual :agreement-with-the-oracle :agree])
                         (get-in arms [:counterfactual :agreement-with-the-oracle :rounds])))
        (println (format "COMPARISON cascade vs v0: %d rounds differ; null? %s"
                         (:cascade-vs-v0-differing-round-count cmp) (:null? cmp)))
        (println (format "oracle %s; %d uncertain (run_shell only); turn outcome %s at round %s"
                         (pr-str (get-in result [:oracle :distribution]))
                         (get-in result [:oracle :uncertain-rounds])
                         (name (get-in result [:oracle :turn-outcome :reading]))
                         (get-in result [:oracle :turn-outcome :last-round])))
        (println (if (get-in result [:o4 :exercised?])
                   (format "O4 six numbers: precedence %s -> %s, acting order %d -> %d entries, score %s -> %s"
                           (pr-str (get-in result [:o4 :precedence-before]))
                           (pr-str (get-in result [:o4 :precedence-after]))
                           (count (get-in result [:o4 :acting-order-before]))
                           (count (get-in result [:o4 :acting-order-after]))
                           (get-in result [:o4 :score-before]) (get-in result [:o4 :score-after]))
                   (format "O4 %s (members carrying a rule: %s)"
                           (name (get-in result [:o4 :reading]))
                           (pr-str (get-in result [:o4 :members-carrying-a-rule])))))
        (println (format "wrote %s" report-path))
        (println "zaif-cascade-gate: PASS exit-convention=0-pass/1-fail"))
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println "zaif-cascade-gate: FAIL" (ex-message e))
      (pprint/pprint (ex-data e))
      (shutdown-agents)
      (System/exit 1))))
