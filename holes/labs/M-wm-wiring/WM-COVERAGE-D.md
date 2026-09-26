# WM-COVERAGE-D — the coverage obligation of the wire ledger, before any flight

claude-2, 2026-09-26. Discovery, read-only for the map, the ledger and the org
layer. New files: `spike/wm_coverage.bb` (the script) and `wm-wire-coverage.edn`
(its output: the ledger's companion, `:coverage` per wire).

Codex's point, restated: the organisation layer has conditional calls and
failure paths. A successful flight takes one branch of each. If the ledger holds
wires on mutually exclusive branches, one record cannot promote all of them, and
"every wire verified" would be a claim no single flight can meet. So the
obligation is defined here, structurally, before the flight, as a partition of
the ledger, and not by exemptions granted after it.

## The counts

Inputs, all from git: ledger `507acc5c` (136 wires, map `1d10314c`), org layer
`507acc5c` (**generated from map `47513216`**, 102 calls, roots
`flight-driver-summary` and `loop-entry`).

| class | wires | meaning |
|---|---|---|
| `:witness` | **98** | both ends lie on a path whose every condition a successful flight satisfies |
| `:conditional` | **14** | reachable on a successful flight only under a condition that is not the success path's |
| `:failure-path` | **4** | reachable only through a refusal, abstention or trip branch |
| `:unreachable` | **20** | an end has no path from a root: 13 `:no-caller`, 7 `:box-not-in-org-layer` |
| total | 136 | the counts sum to the wire count (the script refuses if not) |

**The committed org layer is older than the ledger's map**, so 7 of the 20
unreachable are wires touching boxes the map gained since (`morning-brief-fold`,
`r2-judge-observation`, `r3-aggregate-driver`, `r3a-prediction-error`,
`r3a-channel-prediction-error`, `r7-weighted-error`, `run-record-publication`,
`fpi-policy-free-energy`: eight boxes, seven wires), not map defects. `:inputs :stale?` says so in the
output. An org layer regenerated at map `1d10314c` exists in the working tree
(uncommitted, another lane's set); the script accepts it as a file path for an
**informational** run, marked `:uncommitted-input` and not committed. That run
gives 103 witness / 16 conditional / 4 failure-path / 13 unreachable, all 13
`:no-caller`. Re-run the script when that org layer lands; the committed
companion moves with it. Four new condition texts appear in it and are
`:undecided` (below): they are treated as `:alternative` until someone rules on
them.

## The success path (what "a successful flight satisfies" means)

The spike as `flight_driver` documents it: every wired step given
(`--checker --bb --library-root --field-entry --cascades --dispatch-seat --author
--reviewer`), `--max-clicks 1`, the click answering 200 with a click id, the read
admitted, an interpretation admitted, a candidate constructed, selected and
enacted, W_c passing, the publication observed. A configured step is satisfied
because the spike configures it; a refusal branch is not, because a successful
flight does not take it.

## The rule per condition kind

Each of the 62 condition texts in the org layer is classified in the script's
`TABLE`, one line each with its reason. Three classes:

- **`:success`** (dropped from the path): a step configured in the spike
  (`(when read-fn)`, `(when ask-fn)`, `(when enact-fn)`, `(when dispatch-seat)`,
  `(cond library-root)`, `(when (and wc-fn …))`); a **test/override seam absent**,
  so the production default runs (`(or ... after click-fn)`,
  `(or ... after answer-fn)`, `(or ... after (:judge-fn opts))`, and the other
  seven `or ... after X` forms: the callee is the fallback used when the override
  is not supplied, which is production); **no refusal** (`(if-let [refusal …]) else`,
  `(if (nil? horizon)) else`, `(if (nil? s0)) else`, `(if (not= :supported …)) else`,
  `(if-let [r (or jr gr)]) else`, `(when-not jr)`); **the default arm** of a cond
  (`(cond :else)`), reached when no earlier refusal or special-case arm fired; a
  succeeded transition or answer (`(if-not (compare-and-set! …)) else`,
  `(if-not (and (= 200 status) click-id)) else`); a dispatch value the driver
  always sets (`(defmethod source-wants :a-exits)`).
- **`:failure`**: the branch is a refusal, abstention or trip:
  `(if (empty? problems)) then` (no assembled problems: the abstained decision),
  `(cond (abstention? decision))`, `(if-let [r (or jr gr)]) then`,
  `(case (:trip/action report))`.
- **`:alternative`**: a valid branch a successful flight may or may not take,
  because of a data shape, an optional input, a mode or the flight's position:
  `(if (map? precedence)) then` (co-application versus chain), `(if (string? locator)) then`
  (namespace versus command locator), `(when (:criteria? first-need))` and the other
  read-step needs (they depend on what the mission text states), `(if present?) then`
  (prior flight records exist; the first flight has none), `(cond (nil? sourced))`
  (the identity-default arm), and `(if commissioned?) then` (below).
- **A text not in the table** is `:alternative` and listed in `:undecided`. The
  rule is explicit so a regenerated org layer cannot promote a wire by adding a
  condition nobody has read.

**How I decided the polarity.** Reading the condition text alone got one wrong
and I caught it before writing this: `(if (empty? problems)) else` looked like the
refusal branch and is the main flow (`war_machine.clj:6280`: the `then` branch
emits `{:status :abstained}`). Every polarity-dependent `:success` or `:failure`
entry was therefore checked against the form in the source. Checked at futon2
`bc00cdaf`: `(empty? problems)`, `(= :absent located)`, `(seq questions)/(empty?
wants)`, `missing-interpretation`, `(nil? horizon)`, `(or failed declined)`,
`(not= :supported …)`, `(nil? s0)`, `(string? locator)` (which turned out to be an
alternative, not a success), `(contains? issued ::refused)`, `(if-let [r (or jr
gr)])`, `(seq route)` (the branch that persists the run record: a success, not a
failure as first thought), `(if entry)`, `present?` (an alternative). Not checked
form by form: the ten `or ... after X` seams and the `(cond :else)` arms, which
rest on the rule stated above; a `(cond :else)` arm that is a fallthrough error
would be misjudged and the per-site check would catch it.

**Conjunction and exclusivity.** A path's condition set is its non-success
conditions. A wire's two ends combine as the union of one writer set and one
reader set; a combination holding opposite polarities of one test
(`(if X) then` with `(if X) else`) is refused. A reader that is a `:test` box has
no call path by construction (the org layer's `:tests-not-in-call-tree`, 23
boxes); it observes the record the writer's run produced, so its end adds no
condition (36 wires read by a test). A box reached by several call paths gets the
best of them (a wire is a `:witness` if some combination is empty). Result:
**no wire has all combinations contradictory** (`:mutually-exclusive-branches`
count 0 at this org layer). Mutual exclusion shows up instead as pairs of
`:conditional` wires that need different flights: the `(if (contains? m :config))`
and `(if (string? locator))` branches, `(if commissioned?)` and its absence,
first flight versus later flight.

## The obligation, stated as the flights it needs

The 98 witness wires are promotable from one successful flight's record, subject
to the existing per-wire verification (`:unverified` versus `:witnessed`). The
other 22 (and the 20 unreachable) are not, and the obligation for each is a
specific record:

| condition the wire needs | wires | what would witness it |
|---|---|---|
| `(if commissioned?) then` | 6 conditional + 4 failure-path (the 4 also need a refusal/abstention) | a click whose POST carries `:r10-commissioned true`. `commissioned?` is `(true? (:r10-commissioned payload))` (`http.clj:8978`) and nothing in futon2 sets it (`grep`, `bc00cdaf`): **a flight's click is uncommissioned**, so these wires are not on the spike's path. Either the flight's click is commissioned (a decision for the owner) or these wires wait on a commissioned click as a second record. |
| `(if-let [r (or jr gr)]) then`, `(cond (abstention? decision))`, `(if (empty? problems)) then` | the 4 failure-path wires | a click that abstains or is refused: a **failure record**, promoted from its own run, never expected of the spike |
| `(when (:criteria? first-need))` ∧ `(when (empty? stated))`; `(when (:coverage? cov-need))` ∧ `(when (seq stated))`; `(when (get-in cw2 […:constraints?]))` | 3 | a mission whose text needs that read (criteria, coverage, constraints); which need arises is a property of the target |
| `(if (contains? m :config)) else` ∧ `(if (string? locator)) then` | 2 | a C8 check with no `:config` and a namespace locator |
| `(if present?) then` | 1 | a flight after a first one: prior flight records for the enactment fold |
| `(if (:row a)) else` | 2 | an action with no `:row` (the embedding-neighbour lookup) |

**Unreachable, and what it says.** The 13 `:no-caller` are the outer loop: the
target field, eligibility, pair overlap, the (unbuilt) outer cascade, and
`r9-finding-cause-read`, whose module has no caller in the flight's call tree.
That is H-T-CALLER's finding restated by the ledger: those wires cannot be
witnessed by any flight until the loop entry calls them. The 7
`:box-not-in-org-layer` are the staleness above.

## What this is not

It does not decide which wires *should* be witnessed by the spike; it says which
*can* be. It does not merge `:coverage` into the ledger (a later packet's schema
change). It does not verify any wire. And it inherits the org layer's limits: a
textual call graph, the innermost condition per call site plus those along
helper paths, and three hand-checked residue edges.

## Run it

    bb holes/labs/M-wm-wiring/spike/wm_coverage.bb [LEDGER-REV] [ORG-LAYER-REV]

Reads git only (default HEAD). Writes `wm-wire-coverage.edn`. Set
`WM_COVERAGE_OUT` and pass a file path as the org layer for an informational run
that does not touch the committed companion.
