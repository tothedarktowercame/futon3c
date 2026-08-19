# E-loss-function-shape — the general shape, with a slot for scoped df

**Opened 2026-08-19 by claude-2 (ground control) at Joe's direction**, after a
day in which the same measurement error appeared in six different places. Joe:
*"maybe we can work to a general shape and slot that in when it is ready."*

Written to disk deliberately. Three artefacts today existed only in an agent's
context or an uncommitted working tree — a corpus survey, a correctness fix, and
a set of stop-reports living in bell threads. Two of those were mine.

---

## The governing rule

> **A number and its basis are one object.** A `df` without its population, a
> guidance count without its subtrahend definition, a `used-ids` without whether
> the channel was invoked — each is not a weak measurement, it is not a
> measurement. If the basis changed between frames, the comparison is void and
> the display must SAY SO rather than plot.

Every failure compared across 2026-08-18/19 — `:recall-empty` on an anchor absent
from the store, an honest forward scan over a stale checkpoint, an absolute
`df` band calibrated on a corpus 700× smaller, a silently dropped filter, a
survey counting paper-proof mentions as encoding dependencies — is *a
computation that succeeded against a population nobody had stated* (claude-11's
formulation). The basis is not documentation. It is the missing operand.

## Four bands, kept separate

Deliberately not one scalar. Collapsing them lets a harness repair read as a
mathematical gain, which is precisely what would have happened at f12.

### 1. COST — what was spent
```clojure
{:cost {:commits 5 :dispatches 2 :wall-clock-min 47
        :provenance :measured}}          ; :measured | :estimated — NEVER averaged
```
`t01A07` closed in 5 commits (measured). Singular homology is "1–3 weeks"
(estimated). Averaging those silently is how a plan becomes fiction. The
reusability premium — general vs sphere-specific two-open simply-connected —
is **~1 commit, measured against the alternative**, which is the only
coefficient we have that was measured against a counterfactual rather than
estimated against nothing.

### 2. CAPABILITY — did the problem close
```clojure
{:capability {:disposition :closed          ; :closed :tier-a :tier-b :defective
              :axiom-clean? true
              :statement-unchanged? true
              :residual-sorries 0}}
```
The objective. A frame that repairs its own plumbing and does not solve its
problem has failed, however green the other bands read.

### 3. TRANSFER — did accumulated knowledge shape the work
```clojure
{:transfer {:surfaced 4 :used 1 :channel :pull   ; :push | :pull | :none
            :cost-delta {:milestone :first-per-clause-split
                         :solver 3 :student 1 :provenance :measured}
            :basis {:retrieval :scoped-df-pending   ; <-- THE SLOT
                    :anchor {:term "computes" :satisfied? false
                             :population :whole-index-unfiltered :indexed 149766}}}}
```
**The slot.** Retrieval quality is currently unmeasurable: `?df=` reports
whole-index frequencies and silently ignores every filter, so an anchor is
chosen against 149,766 documents when the discriminating population is the ~770
memories. Until claude-11's scoped `df` lands, `:retrieval` reads
`:scoped-df-pending` and any cross-frame transfer comparison is **void by
declaration**, not merely noisy.

Two channel facts the band must carry, both measured: transfer occurred twice
through the **pull** channel while the metric watched push, and once through a
memory that had been **rejected** at review. A transfer metric keyed on
"offered then used" reads zero in all three cases.

**Cost-delta needs a fixed milestone.** f10 and f11 both show the student
spending less and reaching less — 7 commits/0 residual vs 4/5, and 5/2 vs 2/3.
No ratio between "less time" and "less result" is interpretable. Compare at
*cost to first per-clause split* or *to first closed conjunct*, not at the end,
because the arms have different budgets.

### 4. ACCUMULATION — did the store gain reusable knowledge
```clojure
{:accumulation {:deposits 6 :reviewed-attachments 0
                :reviewer-independent? false   ; guide-reviewed != independent
                :basis {:store-total 211 :from-frame-seats 1}}}
```
Of 211 reviewed attachments in the store, **exactly one** came from an APM frame
seat. The rest were deposited by the operator and ad-hoc sessions. So this band
has never yet measured a working machine, and a zero here is currently
uninformative rather than negative.

`:reviewer-independent?` is load-bearing: attach-then-review with the guide as
reviewer WORKS and is not independent; the independent path is blocked at the
transport. A frame that gains one has not demonstrated separation of powers.

## Three values, never conflated

```clojure
:confirmed | :refuted | :inapplicable | :vacuous | :unmeasured
```

- **`:inapplicable`** — the precondition never occurred. Half of f11's
  predictions: there were no offers to disposition, so an empty disposition list
  is not a failure.
- **`:vacuous`** — it passed, but could not have failed. f11's `F3` passed
  because zero offers means no undispositioned offer exists.
- **`:unmeasured`** — the instrument did not run, or ran against the wrong
  population. Distinct from a measured zero, and today's most common state.

Rendering all of these as `0` is the thing that will mislead us in six frames'
time when nobody remembers which was which.

## Rate of return, and its denominator

Under **completion** the breakdown governs ORDER, not selection — we need all of
it. The quantity that matters is the cost of the covering set, where shared
infrastructure pays back superlinearly.

```clojure
{:rate {:area "singular-homology"
        :blocked-consumers 34        ; NOT references — see below
        :cost-estimate-commits 20
        :basis {:method :source-closure-448-of-448
                :vintage "2026-08-19"
                :coverage 0.62        ; obstruction records exist for 62% of open
                :supersedes ["marker-survey" "mentions-only"]}}}
```

**The denominator is blocked-consumers, not references.** Corrected three times
in one day: 52 (paper-proof mentions) → 16 (statement mentions) → 34 (transitive
closure), and then the metric itself corrected — `measure-integral` shows 100
consumers and 41 open, but a spot-check found **22 of 26** recorded obstructions
name something other than measure theory. References are not gaps. Mathlib is
adequate in an area where problems close at a normal rate, however many mention
it.

**Compounding is currently zero.** The finance framing assumes reinvestment: a
closed problem deposits knowledge that makes the next one cheaper. That term is
the accumulation band, and it has produced one attachment ever. Until it works,
an ROI model must carry a compounding rate of **0** and say so, or the first
plausible projection will assume a yield that is not happening.

## What to build first

The **receipt**, before the metric. A basis reading
`population :whole-index-unfiltered, indexed 149766` beside a band of `[3 150]`
makes the mismatch self-evident on sight; the band is the fix, the receipt is
what makes the fix checkable. Landed for the recall anchor
(futon3c `10937528`); the same treatment is owed to the guidance count, the
transfer channel, and the accumulation denominator.


---

## State as of f13's close, and what f15 can add (2026-08-19)

Joe asked where this stands and whether f15 can populate it.

### Populated

`:loss-components` sits in `series.edn` per frame and is populated **f7–f13**.
f13's was ABSENT until today and has been backfilled — the worst possible frame
to have omitted, since it is the first with a non-zero attested numerator.
**f14 never opened**, so there is nothing from it and never will be.

    frame   used-attested
    f7-f10  (field absent from the schema of the day)
    f11     1     off-instrument, via a REJECTED memory
    f12     0     a producer frame: converted-artifacts 6, numerator zero
    f13     2     FIRST ATTESTED — and both memories are f12's

### The one real datum the series has

f12 deposited 6 converted artifacts and scored a zero numerator. f13 then
attested `USED` on two of them. **That is f12's denominator paying into f13's
numerator** — precisely the thing f12's own note said the loss function as
shaped could not express. It can now, but only by reading two frames together,
which is an argument for a cross-frame view rather than a per-frame one.

### The three slots still unfilled, and why

1. **COST.** Frame records carry no wall-clock, no duration, no attempt cost.
   `:cost-delta` has never been populated for any frame. This is the band that
   makes rate-of-return computable and it has no instrument at all.
2. **`:basis {:retrieval …}`.** Still pending, and for a DIFFERENT reason than
   this document originally recorded. The scoped-df work landed (`6bfe5808`) but
   is not on the code path: `query-anchor-term-memory-df` runs only under
   `--anchor-source memory-df` and the default is `:problem-idf`. So the slot
   cannot be filled by flipping a field; the fix is D60 and it ranks the wrong
   object (the anchor, not the query terms).
3. **`:used-receipted`.** Zero in every frame, including f13 where two uses were
   attested. The trace has never once recorded a use that the seats reported.
   f13's is explained — the halt preceded `dispatch-student!`, which is the only
   thing that harvests `:promotion-result` — but f9–f12 had no such excuse.

### What f15 can populate that no predecessor could

f15 is the first frame with a problem VERIFIED SOUND, a working read path, and
memory-use instrumentation. So for the first time:

- `:used-attested` will mean *"accumulated knowledge helped solve a real
  problem"* rather than *"helped detect a broken one"*, whichever way it falls;
- `:converted-artifacts` will count work on four genuine conjuncts;
- and a **zero** will be informative rather than ambiguous, because the
  registration pre-declares that the store holds essentially no ODE material
  (`gronwall` 0, `flow` 0, `ode` 2, `picard` 1). A zero is then a statement
  about corpus COVERAGE, not about the mechanism.

It still cannot populate COST. That instrument does not exist and should be
built before f16, or the rate-of-return band stays permanently empty.


## COST — what is actually available, measured 2026-08-19

Joe proposed tokens, lemma count, and LOC, noting LOC is directly comparable
across agents. Measured rather than assumed:

### Tokens are NOT available

An Agency job record carries `created-at`, `started-at`, `finished-at`, `state`,
`execution`, `events`, `result` — and **no token or usage field of any kind**.
So the cross-model comparability worry is moot until something records them.
Wall-clock is there and free.

### What the three candidate measures actually say

    frame  solver wall-clock  dispatches  LOC added  new decls  disposition
    f12         6m06              3           931       23      :defective (vacuous)
    f13         7m17              1            77        1      :defective (uninhabited)
    f15         9m53              ?             ?        ?      running

### LOC IS COST, and it works — I had the sign backwards

**Corrected by Joe, 2026-08-19.** I wrote that LOC "inverts as a cost measure"
and "rewards the long way round" because f12's 931 lines outscore f13's 77. That
reasoning had the sign wrong: **high LOC = high cost.** f12 spent 931 lines and
23 declarations across three dispatches; f13 spent 77 lines and one declaration
in a single dispatch. LOC-as-cost therefore ranks f13 as **twelve times cheaper**
for an equally valuable result — which is exactly what we want it to say, and
exactly what I said it failed to.

The measure penalises the long way round. It does not reward it. I was reading
LOC as a score to be maximised and then objecting that the wrong frame won.

### Why LOC is unambiguously on the cost side

The PRODUCT of a frame is the theorem — closed, or its residual reduced. The
lines of Lean are what it took to get there. So:

- **COST** = LOC + new declarations + dispatch count + wall-clock. All four are
  directly comparable across agents, as Joe said: a line of Lean is a line of
  Lean whoever wrote it, and none of them require new instrumentation.
- **YIELD** = the disposition, and only the disposition. Closed / residual
  reduced / defective-and-proved-so.

That is cleaner than the split I proposed, because it stops LOC from having to be
two things at once.

### The disposition still has to travel — for a different reason than I gave

I said the disposition must accompany LOC because otherwise the table ranks f12
first. Wrong reason. The real one: **cost without knowing what was bought is
meaningless.** 931 lines for a vacuous close and 77 lines for a vacuous close are
both expenditures against nothing; the ratio between them only becomes a rate of
return when the disposition says something was actually acquired.

    frame  wall-clock  dispatches  LOC   decls  disposition
    f12       6m06         3        931    23   :defective (vacuous)
    f13       7m17         1         77     1   :defective (uninhabited)
    f15       9m53         ?          ?     ?   running — first SOUND problem

f13 is the cheapest frame in the series by every cost column except wall-clock,
and it is the only one with attested transfer. f15 is the first whose cost can
be divided by a yield that is not zero.

### Tokens ARE available — I said "unavailable" when I meant "I did not look"

**Corrected by Joe, 2026-08-19.** The Agency job record carries no token field,
which is true and is where I stopped. Codex writes `token_count` events into its
rollout files, and the job record's `session-id` is the filename key:

    ~/.codex/sessions/YYYY/MM/DD/rollout-<ts>-<session-id>.jsonl
    {"payload":{"type":"token_count","info":{"total_token_usage":{...}}}}

Extracted for the three solver dispatches that have run on this apparatus:

    frame  total tokens  uncached in   output  reasoning  LOC  disp  wall
    f12      31,599,193      676,459  105,454     46,767  931    3   6m06
    f13       2,606,048      101,830   18,970      7,504   77    1   7m17
    f15       5,160,559      235,071   27,440     14,606    ?    1   9m53

### The result worth having: LOC and total tokens agree

    f12/f13 by total tokens : 12.12x
    f12/f13 by LOC          : 12.09x

Two independently measured cost proxies, agreeing to two significant figures on
the one comparison available. That is n=2 and must not be over-read — but it is
direct support for Joe's original point that **LOC is a good cross-agent cost
proxy**, and LOC is free where tokens need a rollout-file join.

### The caveat that changes the number

`total_tokens` is dominated by CACHED input — 30.8M of f12's 31.5M. Cached input
is re-read context, so total tokens scales with turn count more than with work.
On uncached input + output + reasoning, the ratio is different:

    f12 ~828k   f13 ~128k   ->  6.5x, not 12x

So **which token measure you pick changes the answer by a factor of two**, and
neither is wrong — they measure different things. `total_tokens` tracks how long
the agent was in context; the uncached figure tracks how much new work it did.
State which one a table means, or the number is another unstated population.

### Zai/Codex comparability

Untested. These three are all Codex. A Zai dispatch would need its own rollout
equivalent located before any cross-agent token comparison is claimed, and until
then LOC remains the only measure demonstrated comparable across agent types.

### Zai tokens: the turns are logged, the usage block is not

Joe: *"Zai turns are logged into the Evidence Landscape, so the raw data as it
comes back from z.ai must be somewhere."* Checked. The first half is right and
the second does not follow.

**What IS in the store.** Zai turns are there as coordination evidence:

    {:evidence/body {:event "chat-turn" :transport … :role "user"
                     :turn-id "codex-3-turn-12" :text "…"}
     :evidence/type :coordination :evidence/tags [… :turn …]}

**What is NOT.** No `usage` block, no `prompt_tokens`, no `completion_tokens`.
The body is a distilled chat-turn, not the raw response envelope. A whole-store
search for `total_tokens` returns exactly one hit — *this conversation's own
prompt preview*, because Joe's message quoting the codex JSON was itself indexed
seconds earlier. A false positive from the question being asked.

**And there is no zai rollout file.** Codex writes
`~/.codex/sessions/YYYY/MM/DD/rollout-<ts>-<session-id>.jsonl`; the zai side has
`/tmp/futon-zai-session-id-<seat>` (an id) and `~/.zai-key`, and nothing that
persists responses. `agents/zaif_controller.clj` captures no usage field.

So the raw data existed in the transport and was dropped. **Getting zai tokens
needs the transport to capture the `usage` block at response time and write it
into the evidence record** — small, and the prerequisite for any cross-agent
token comparison. Until then LOC remains the only cost measure demonstrated
comparable across agent types.

### Incidental: the evidence `tag` filter is silently ignored

Found while looking for zai turns, and it is the `?df=` bug's twin:

    GET /api/alpha/evidence?limit=1&tag=:zai-turn
      -> a record tagged [:codex :repl :turn :user]
    GET /api/alpha/evidence?limit=1&tag=:definitely-not-a-real-tag-xyzzy
      -> the SAME record, count 1

A nonsense tag returns a record. The parameter is accepted and dropped, with no
warning and a 200. Anyone filtering evidence by tag has been getting an
unfiltered population and a plausible-looking answer. Owner: claude-11.


## THE SOLVER IS 6% OF THE FRAME. Every cost figure above was ~17x too small.

Joe: *"we could find a way to include the cost of Guide + Scribe + Proctor +
Analyst — probably not cheap."* Measured, and it is the largest correction in
this document.

Claude seats write full per-message usage to
`~/.claude/projects/<project>/<session-id>.jsonl`
(`input_tokens`, `cache_creation_input_tokens`, `cache_read_input_tokens`,
`output_tokens`, `output_tokens_details.thinking_tokens`), and the Agency
registry holds each seat's session-id. So a whole-frame cost has been computable
all along.

### Frame 13, every seat that actually ran

    seat          kind        total tokens   uncached-in    output   reasoning
    f13-solver    codex          2,606,048       101,830    18,970       7,504
    f13-scribe    codex            147,228         6,067     1,641         244
    f13-guide     claude        42,007,935     3,072,343   240,285     118,507
    ---------------------------------------------------------------------------
    FRAME TOTAL                 44,761,211     3,180,240   260,896

    guide share of total tokens : 93.8%
    solver share                :  5.8%
    guide / solver, uncached-in : 30x
    guide / solver, output      : 12.7x

**Conducting costs more than solving, by more than an order of magnitude.**

f13-student, f13-proctor and analyst-3 never ran, so they cost nothing — which
is the lazy-seat argument made in tokens rather than in seat counts.

### What this invalidates

Every cost figure earlier in this document — f12 31.6M, f13 2.6M, f15 5.2M — is
**the solver dispatch alone**, i.e. roughly 6% of what the frame spent. The
rate-of-return denominator has been understated by about 17x throughout. The
LOC/token agreement at 12x still holds, because both were measured on the same
(solver-only) population; it says nothing about frame cost.

### What it changes about the design

The savings are not where I have been looking. Probe mode and lazy seats were
argued from the proctor never being dispatched in five frames — true, but the
proctor costs zero precisely because it never runs. **The expensive thing is
guide turns**, and a probe still pays for a guide. A probe is cheap because its
guide session is SHORT, not because it has fewer seats.

That also sharpens the traffic-discipline point: six bells to f13-guide were not
just a routing hazard, they were most of the frame's budget.

### Caveat, stated because the ratio depends on it

The guide's 42M is dominated by cache reads (38.9M of 42.0M). Cached input is
cheap in money terms, so the TOKEN ratio and the MONEY ratio differ. On uncached
input the guide is still 30x the solver, so the direction holds on either
measure — but a table quoting "44.8M tokens" for a frame must say that ~87% of it
is cache reads or it will be read as spend.

### Not counted here

Ground control's own session (claude-2). It is the largest Claude session in the
project and it is arguably part of every frame's cost. Excluded because it spans
all frames and cannot be attributed to one without a per-frame split that does
not exist. Worth saying rather than quietly omitting: **the true cost of a frame
is higher than 44.8M, and the missing term is the orchestrator.**

## THE LOAD IS GROUND CONTROL. Claude:Codex is 67x, and 91% of Claude is one session.

Joe, 2026-08-19: *"Codex 65% remaining, claude 82% used. So even if we are
dispatching problem solving AND coding to Codex we're not really balancing the
load properly."* Measured across every session file written today.

### Claude, by session

    session    total tokens   uncached-in     output    msgs
    87332988  4,734,994,468    55,008,600 10,762,941    9794   <- claude-2, GROUND CONTROL
    626dc23f    304,178,454     4,300,808  1,019,862     707   <- claude-10
    9699e8b8     58,707,769     2,170,433    327,078     335
    c57ea1de     45,229,974     2,590,086    218,044     284   <- f12-guide
    530f6875     42,007,935     3,072,343    240,285     256   <- f13-guide
    578e66fa     18,673,639     1,667,497    135,118     161   <- f15-guide
    TOTAL     5,203,826,766    68,828,384 12,703,343

### Codex, all seven sessions today

    CODEX TOTAL      77,455,687     2,180,095    257,096

    ratio claude:codex   67.2x total    49.4x output

### What this overturns

An hour ago I told Joe "the guide is 94% of frame cost". True — and **frames are
2% of Claude spend.** The three frame guides together are 106M against ground
control's 4.73 BILLION. **Ground control is 91% of all Claude consumption.**

The coding-handoff protocol works: coding does go to Codex. Codex spent 77M all
day. It is simply not where the load is. **Nothing in the protocol addresses
orchestration, and orchestration is 67x the thing the protocol moves.**

### Why the orchestrator session is so large

9,794 assistant messages at a mean context near 480k. The dominant term is not
output (10.8M) — it is **cache reads: every tool call re-reads the whole
context.** Ground control's working style today has been many small sequential
shell calls, frequently three to six per turn, each one a full context re-read.
That thoroughness found real defects all day; it also *is* the load.

### Levers, ranked by measured impact

1. **Batch tool calls.** One combined call costs one context read; six sequential
   calls cost six. This is the dominant term and it is entirely ground control's
   to fix. No protocol change required, no other agent involved.
2. **Use the proctor seat.** It is a CODEX seat, it has never been dispatched in
   five frames, and the mechanical verification it exists for — re-running
   `lake env lean`, `#print axioms`, gate re-runs — is currently done by the
   guide (Claude) and by ground control (Claude). This is an existing, unused,
   correctly-typed lever for exactly the work that is burning the wrong quota.
3. **Fewer, denser bells.** Six bells to f13-guide were six full guide turns.
   Already noted as a routing hazard; it is also a spend one.
4. Shorter orchestrator context — not directly controllable, and the smallest
   term of the four.

The honest summary: the protocol balanced the load it was written for, and the
load moved. Ground control did not notice because ground control was never
measuring itself.
