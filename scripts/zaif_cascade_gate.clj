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

  WHAT TWO PRE-RUN REVIEWS CHANGED, before this gate was ever run.  codex-17
  reviewed the cascade (2026-09-02, verdict REVISE) and then this rule table
  (REVISE again), and six of the repairs below are theirs, not mine: the
  comparison is restricted to rounds carrying BOTH a transcript and a v0
  decision; the oracle's ambiguous rounds are excluded from the primary and
  bounded in a sensitivity arm rather than labelled `:retrieve`; the treatment is
  named an OVERRIDE of v0 rather than an alternative to it, because a deferred
  round carries v0's answer and not the cascade's; the
  `missing-dependency-protocol` rule is a one-shot sequence rather than the
  predicate that was true on 68 of 102 rounds; and the sham is recorded as
  tautological rather than offered as an independent control.

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

(def cohort-path
  "The PRIMARY cohort.  `-main` takes an optional path so the SAME cascade and the
   SAME four rules can be run over a second, held-out task without a line of
   either changing -- repair 6 of the second pre-run review, which is right that
   two unused rules are a weak anti-fitting guard and that a task the table was
   not written against is what would establish one."
  "holes/zaif-cohort-a97J05.edn")
(def cascade-path "../futon3/checks/zaif-cascade.edn")
(def library-root "../futon3/library")

(def temperament-under-test
  "The FLOOR temperament.  `construct_zaif_cascade.clj` reports both; the budget
   one halts before admitting anything (its seed of 38 already exceeds the
   transplanted budget of 20), so the floor arm is the one whose membership the
   constructor actually decided.  `construct_ants_cascade.clj` chose the floor arm
   for the same reason.

   AFTER THE CUE LICENCE (futon3 c4b4831) that reason no longer holds: the seed
   is 11, the budget arm admits 9 and reaches 20 members, the floor arm reaches
   17, and neither is degenerate.  The floor arm is kept as the arm under test
   because it is the one `:LA4` used, and because changing which arm is measured
   once the membership is in view is a choice made with the answer visible.  Both
   arms are in `zaif-cascade.edn` and either can be read."
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
              ;; THE ONE-SHOT PHASES of `missing-dependency-protocol`'s THEN,
              ;; computed here so the rule's closure stays legible.  Repair 1 of
              ;; the second pre-run review: the THEN prescribes a SEQUENCE --
              ;; "Search once more ... look for an adapter ... before building",
              ;; then "Put exactly one `sorry` at the observed gap ... leave the
              ;; consultation trail", then "stop researching once the misses
              ;; repeat" -- and the first encoding read it as a standing
              ;; predicate that held on 68 of 102 rounds.  Each flag below is
              ;; true on EXACTLY ONE round and is computed from rounds already
              ;; seen, never from later ones: a live seat at round n could
              ;; compute all three.
              once-more? (and (:store-consulted? acc)          ; the store was consulted BEFORE this round
                              (not (:once-more-done? acc))
                              (seq calls))
              record-gap? (and (:once-more-done? acc)
                               (not (:gap-recorded? acc))
                               (not wrote?))
              ;; "the misses repeat": two further rounds after the gap was
              ;; recorded in which nothing landed.  Two, because that is what
              ;; `repeat` means, not a number chosen from this cohort.
              stop-researching? (and (:gap-recorded? acc)
                                     (not (:stopped? acc))
                                     (>= (- (dec round) (:gap-round acc)) 2))
              s {:grain :play
                 :round round
                 :rounds-so-far (dec round)
                 :md-once-more? once-more?
                 :md-record-gap? record-gap?
                 :md-stop-researching? stop-researching?
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
              (assoc :once-more-done? (or (:once-more-done? acc) once-more?))
              (assoc :gap-recorded? (or (:gap-recorded? acc) record-gap?))
              (assoc :gap-round (if record-gap? round (:gap-round acc)))
              (assoc :stopped? (or (:stopped? acc) stop-researching?))
              (assoc :last-edit-round (if wrote? round (:last-edit-round acc))))))
      {:rows [] :store-consulted? false :edits 0 :reads 0 :last-edit-round 0
       :once-more-done? false :gap-recorded? false :gap-round 0 :stopped? false}
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
    ;; "on every turn" means on every turn.  The first encoding had this fall
    ;; silent after the store was first consulted, which the second pre-run
    ;; review correctly called the author's reading of the HOWEVER's notification
    ;; fatigue rather than anything the THEN says.
    :if (fn [s] (and (= :play (:grain s)) (:has-transcript? s)))
    :however (fn [_] true)
    :encoding-limit "the THEN also says to display results, log the activation history, and tune retrieval from hot/cold zones. The zaif arm set has no term for any of those, so this rule encodes the search clause only and the rest is unencodable rather than encoded."
    :then (fn [_] :retrieve)}

   {:id :math-strategy/missing-dependency-protocol
    :then-source "math-strategy/missing-dependency-protocol.flexiarg:59-77"
    :encodes "\"Search once more against the installed source ... and look for an adapter to an existing theorem before building\"; \"Put exactly one `sorry` at the observed gap ... Beside that `sorry`, leave the consultation trail\"; \"stop researching once the misses repeat\""
    ;; A SEQUENCE, not a standing predicate.  The three phases fire on one round
    ;; each and the rule is silent before and after.  What the first encoding got
    ;; wrong, and the second review named: `(> rounds-since-last-edit 1)` held on
    ;; 68 of 102 rounds, which reads "act whenever nothing has landed lately" and
    ;; not "search once more, then record the gap, then stop".
    :if (fn [s] (and (= :play (:grain s)) (:store-consulted? s)))
    :however (fn [s] (or (:md-once-more? s) (:md-record-gap? s) (:md-stop-researching? s)))
    :then (fn [s] (cond (:md-once-more? s) :retrieve       ; "search once more"
                        (:md-record-gap? s) :act           ; "put exactly one sorry ... leave the trail"
                        (:md-stop-researching? s) :yield   ; "stop researching once the misses repeat"
                        :else nil))}

   ;; --- NOT SELECTED BY THE CONSTRUCTOR.  Counterfactual arm only. ---
   {:id :agent/budget-bounds-exploration
    :counterfactual? true
    :then-source "agent/budget-bounds-exploration.flexiarg:19-19"
    :encodes "\"When budget nears exhaustion, force a decision: commit to best-so-far, escalate to a supervisor, or explicitly expand scope with justification.\""
    :if (fn [s] (and (= :play (:grain s)) (some? (:round-budget s))))
    ;; The constant 24 is the RUNNER's own `tool-round-budget`
    ;; (futon3c/src/futon3c/agents/zai_api.clj:1033-1037) and not a number taken
    ;; from this cohort.  The predicate is `>=`, which is AT OR BEYOND the budget
    ;; and not the THEN's "nears"; it is left as `>=` because any "nears" margin
    ;; would be a number this file chose, and it is named here rather than
    ;; described as what the THEN says.
    :however (fn [s] (>= (:rounds-so-far s) (:round-budget s)))
    :encoding-limit "the THEN offers three terminal choices -- commit to best-so-far, escalate to a supervisor, expand scope with justification -- and asks for reset rules. The zaif arm set has a term for none of the three: :yield is the nearest to `commit to best-so-far`, :ask is the nearest to `escalate`, and nothing expresses `expand scope`. The 102 recorded rounds also cross continuation boundaries this file does not reconstruct, so `rounds-so-far` is rounds since the turn began and not rounds since the current budget window opened."
    :then (fn [_] :yield)}

   {:id :agent/pause-is-not-failure
    :counterfactual? true
    :then-source "agent/pause-is-not-failure.flexiarg:19-19"
    :encodes "\"Treat 'pause and ask' as a first-class action with defined triggers: ... confidence below threshold, resource budget near exhaustion.\""
    ;; The `confidence below threshold` trigger is DROPPED.  The first encoding
    ;; set it at 0.5, a number the THEN does not state and this file invented;
    ;; the second pre-run review named it, and the repair is removal, not a
    ;; different number.  What is left is the one trigger the THEN states in
    ;; terms the record carries.
    :if (fn [s] (= :play (:grain s)))
    :however (fn [s] (>= (:rounds-so-far s) (:round-budget s)))
    :encoding-limit "the THEN also requires the pause to carry a resumption payload (reason, missing info, requested decision, scope impact, confidence/budget snapshot). An arm choice carries none of that, so this rule encodes the trigger and not the payload."
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
             :encoding-limit (:encoding-limit (first (filter #(= id (:id %)) rule-table)))
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

(defn oracle-uncertain?
  "A round whose only calls are `run_shell`.  Repair 3 of the second pre-run
   review: 54 of the 102 rounds are of this kind, so labelling them `:retrieve`
   and reporting the count is not the same as having 102 labelled observations.
   They are EXCLUDED from the primary and bounded in a sensitivity arm instead."
  [s]
  (and (seq (:calls s)) (every? #(= :run_shell %) (:calls s))))

(defn- dist [xs] (into (sorted-map) (frequencies xs)))

(defn paired?
  "Repair 2 of the second pre-run review: a round is a comparison observation only
   if it carries BOTH a transcript and a v0 decision.  52 of the 102 transcript
   rounds have no recorded v0 decision, and on those the deferral fallback would
   hand back `nil` -- an arm neither controller chose."
  [s]
  (and (:has-transcript? s) (:has-v0? s)))

(defn- agreement
  "Agreement with the oracle over the rounds `keep?` admits.  `:rounds` is the
   denominator and is always reported beside `:agree`, because the denominator is
   what the two repairs above change."
  [plays sits keep?]
  (let [pairs (remove nil? (map (fn [p s] (when (keep? s) [(:arm p) (oracle s)])) plays sits))]
    (sorted-map :rounds (count pairs)
                :agree (count (filter (fn [[a o]] (= a o)) pairs))
                :confusion (into (sorted-map) (frequencies pairs)))))

(defn- sensitivity
  "The ambiguous rounds under each label they could carry.  Not a result: a range
   the primary sits inside, so a reader can see how much of any difference is the
   classifier's choice rather than the record's."
  [plays sits]
  (let [amb (fn [label]
              (let [pairs (remove nil?
                                  (map (fn [p s]
                                         (when (and (paired? s) (oracle-uncertain? s))
                                           [(:arm p) label]))
                                       plays sits))]
                {:rounds (count pairs)
                 :agree (count (filter (fn [[a o]] (= a o)) pairs))}))]
    (sorted-map :if-ambiguous-rounds-are-retrieve (amb :retrieve)
                :if-ambiguous-rounds-are-act (amb :act))))

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
                  :recorded-g-terms (:v0-g-terms s) :rederived-g-terms (:g-terms d)
                  :g-terms-match? (= (:v0-g-terms s) (:g-terms d))}))]
    (sorted-map
     :rounds (count rows)
     :arm-disagreements (vec (for [r rows :when (not= (:recorded r) (:rederived r))]
                               (dissoc r :recorded-g-terms :rederived-g-terms)))
     :g-term-disagreements (count (remove :g-terms-match? rows))
     :g-term-disagreement-examples (vec (take 3 (remove :g-terms-match? rows))))))

(defn report [cohort-file]
  (let [cohort (edn/read-string (slurp cohort-file))
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
        ;; the two denominators the second pre-run review required.  `primary?`
        ;; is the one every headline number is over: a round with a transcript,
        ;; a recorded v0 decision, and an oracle label the record actually
        ;; determines.
        primary? (fn [s] (and (paired? s) (not (oracle-uncertain? s))))
        primary (fn [ps] (keep-indexed (fn [i p] (when (primary? (nth sits i)) p)) ps))
        differs (fn [ps] (vec (for [[c v s] (map vector ps v0-plays sits)
                                    :when (and (paired? s) (not= (:arm c) (:arm v)))]
                                {:round (:round s) :override (:arm c) :v0 (:arm v)
                                 :fired (:fired c) :oracle (oracle s)
                                 :oracle-is-uncertain? (oracle-uncertain? s)})))
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
             :cohort cohort-file
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
                (count (remove (set (map :id rule-table)) members))
                :anti-fitting
                (sorted-map
                 :protocol "every rule's :then, :if and :however are written from the cited THEN's own clauses and from nothing else; a rule may read only fields of the Situation that a live seat could compute at that round; no threshold is taken from this cohort."
                 :rules-outside-the-cascade
                 (count (remove #(contains? members (:id %)) rule-table))
                 :what-this-does-NOT-establish
                 "the second pre-run review is right that unused rules are a weak guard: require-pass! only asks that SOME rule lie outside the cascade, and arbitrary unused rules would satisfy it. A frozen encoding protocol reviewed independently of any cascade, and a second task, are what would establish it. Neither is done, and this row does not claim them."))
     :denominators (sorted-map
                    :rounds-with-a-transcript (count (filter :has-transcript? sits))
                    :paired (count (filter paired? sits))
                    :primary (count (filter primary? sits))
                    :dropped-no-v0-decision
                    (count (filter #(and (:has-transcript? %) (not (:has-v0? %))) sits))
                    :dropped-oracle-uncertain
                    (count (filter #(and (paired? %) (oracle-uncertain? %)) sits))
                    :why "repairs 2 and 3 of the second pre-run review. A round with no recorded v0 decision is not a paired observation, and a round whose only calls are run_shell has no oracle label the record determines.")
     :arms (sorted-map
            :v0 (sorted-map :arms (dist (map :arm (primary v0-plays)))
                            :agreement-with-the-oracle (agreement v0-plays sits primary?))
            ;; REPAIR 4.  This arm is NOT "the cascade": on every round where no
            ;; rule fires it carries v0's own answer.  It is v0 with the cascade
            ;; allowed to override, and the fired-round numbers below are the only
            ;; ones that are about the cascade at all.
            :v0-with-cascade-override
            (sorted-map
             :what-this-is "v0's arm on every round, overridden on the rounds where a cascade member's THEN emits. A deferred round is v0's answer and is not evidence about the cascade."
             :arms (dist (map :arm (primary c-plays)))
             :fired (dist (keep :fired (primary c-plays)))
             :rounds-that-fired (count (filter :fired (primary c-plays)))
             :agreement-with-the-oracle (agreement c-plays sits primary?)
             :on-fired-rounds-only
             (agreement c-plays sits (fn [s] (and (primary? s)
                                                  (some #(and (= (:round %) (:round s)) (:fired %))
                                                        c-plays))))
             :sensitivity-over-the-ambiguous-rounds (sensitivity c-plays sits))
            :sham (sorted-map
                   :tautological? true
                   :why "with no rule, every round defers, so the sham reproduces v0 by construction. The second pre-run review is right that this is not an independent control; it is kept as a WIRING check -- if it ever differed, `play` would not be deferring where it says it defers."
                   :arms (dist (map :arm (primary s-plays)))
                   :agreement-with-the-oracle (agreement s-plays sits primary?))
            :counterfactual
            (sorted-map
             :NOT-A-RESULT
             "these two rules encode patterns the constructor did NOT select. The arm exists to measure what the override would have done had find returned them, and to keep the rule table from being fitted to the cascade. It is not evidence about the constructor."
             :arms (dist (map :arm (primary x-plays)))
             :fired (dist (keep :fired (primary x-plays)))
             :first-round-that-fired (:round (first (filter :fired x-plays)))
             :agreement-with-the-oracle (agreement x-plays sits primary?)))
     :comparison (sorted-map
                  :estimand "v0 with cascade override, versus unmodified v0, on the rounds carrying both a transcript and a recorded v0 decision. IN-SAMPLE RETROSPECTIVE: the tension's cues were authored from the same task whose transcript is the oracle, so this is not predictive validation of anything."
                  :override-vs-v0-rounds-that-differ (differs c-plays)
                  :override-vs-v0-differing-round-count (count (differs c-plays))
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
          :acting-order-before (acting-order (primary c-plays))
          :acting-order-after (acting-order (primary o4-plays))
          :score-before (:agree (agreement c-plays sits primary?))
          :score-after (:agree (agreement o4-plays sits primary?))
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
  [cohort-file]
  (let [cohort (edn/read-string (slurp cohort-file))
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
                       :emits (vec (sort (distinct (keep (fn [s] (when (try (fo/fires? r s) (catch Exception _ false))
                                                                          ((:then r) s)))
                                                        with-t))))
                       :encoding-limit (:encoding-limit r)
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

(defn- print-coverage [c]
  (println (format "coverage over %d rounds of %s; cascade %s has %d members, %d with no rule"
                   (get-in c [:as-of :rounds-with-a-transcript])
                   (get-in c [:as-of :cohort-turn])
                   (name (get-in c [:as-of :temperament]))
                   (count (get-in c [:as-of :cascade-members]))
                   (count (:members-with-no-rule c))))
  (doseq [r (:rules c)]
    (println (format "  %-48s in-cascade %-5s cf %-5s emits %-26s fires on %d rounds (first %s last %s)"
                     (:id r) (:in-the-cascade? r) (:counterfactual? r) (pr-str (:emits r))
                     (:antecedent-holds-on-n-rounds r) (:first-round r) (:last-round r))))
  (println (format "contentions: %d; grain leaks %d; rules not in the cascade %s"
                   (count (get-in c [:contention :rounds-where-more-than-one-cascade-rule-fires]))
                   (count (get-in c [:controls :grain-separation]))
                   (pr-str (get-in c [:controls :rules-not-in-the-cascade])))))

(defn -main
  "  -m zaif-cascade-gate                       the primary cohort, full gate
   -m zaif-cascade-gate coverage              the primary cohort, coverage only
   -m zaif-cascade-gate <cohort.edn>          a HELD-OUT cohort, full gate
   -m zaif-cascade-gate coverage <cohort.edn> a held-out cohort, coverage only

   The cascade and the rule table are the same object in every case; only the
   cohort moves.  That is what makes a held-out run mean anything."
  [& args]
  (let [coverage? (boolean (some #{"coverage"} args))
        cohort-file (or (first (remove #{"coverage"} args)) cohort-path)
        held-out? (not= cohort-file cohort-path)
        suffix (if held-out? "-holdout" "")]
    (try
      (if coverage?
        (let [c (coverage cohort-file)
              out (str "holes/zaif-cascade-coverage" suffix ".edn")]
          (spit out (with-out-str (pprint/pprint c)))
          (print-coverage c)
          (println (format "wrote %s -- NOTHING about v0, the oracle or agreement is in it" out))
          (shutdown-agents)
          (System/exit 0))
        (-run-gate cohort-file (str "holes/zaif-cascade-gate" suffix ".edn") held-out?))
      (catch clojure.lang.ExceptionInfo e
        (println "zaif-cascade-gate: FAIL" (ex-message e))
        (pprint/pprint (ex-data e))
        (shutdown-agents)
        (System/exit 1)))))


(defn -run-gate [cohort-file out-path held-out?]
  (try
    (let [result (assoc (require-pass! (report cohort-file)) :held-out? held-out?)]
      (spit out-path (with-out-str (pprint/pprint result)))
      (when held-out?
        (println "HELD-OUT RUN: the cascade and all four rules are byte-identical to the primary run; only the cohort differs."))
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
        (let [o (:v0-with-cascade-override arms) d (:denominators result)]
          (println (format "denominators: %d transcript rounds -> %d paired (%d dropped, no v0 decision) -> %d primary (%d dropped, oracle uncertain)"
                           (:rounds-with-a-transcript d) (:paired d)
                           (:dropped-no-v0-decision d) (:primary d) (:dropped-oracle-uncertain d)))
          (println (format "override  arms %s, fired on %d primary rounds %s, agrees with the oracle on %d/%d (on FIRED rounds only %d/%d)"
                           (pr-str (:arms o)) (:rounds-that-fired o) (pr-str (:fired o))
                           (get-in o [:agreement-with-the-oracle :agree])
                           (get-in o [:agreement-with-the-oracle :rounds])
                           (get-in o [:on-fired-rounds-only :agree])
                           (get-in o [:on-fired-rounds-only :rounds])))
          (println (format "  sensitivity over the %d ambiguous rounds: %s"
                           (:dropped-oracle-uncertain d)
                           (pr-str (:sensitivity-over-the-ambiguous-rounds o)))))
        (println (format "counterfactual (NOT A RESULT) arms %s, first fired round %s, agrees on %d/%d"
                         (pr-str (get-in arms [:counterfactual :arms]))
                         (pr-str (get-in arms [:counterfactual :first-round-that-fired]))
                         (get-in arms [:counterfactual :agreement-with-the-oracle :agree])
                         (get-in arms [:counterfactual :agreement-with-the-oracle :rounds])))
        (println (format "COMPARISON override vs unmodified v0 over %d paired rounds: %d differ; null? %s; counterfactual would differ on %d"
                         (get-in result [:denominators :paired])
                         (:override-vs-v0-differing-round-count cmp) (:null? cmp)
                         (:counterfactual-vs-v0-differing-round-count cmp)))
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
        (println (format "wrote %s" out-path))
        (println "zaif-cascade-gate: PASS exit-convention=0-pass/1-fail"))
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println "zaif-cascade-gate: FAIL" (ex-message e))
      (pprint/pprint (ex-data e))
      (shutdown-agents)
      (System/exit 1))))
