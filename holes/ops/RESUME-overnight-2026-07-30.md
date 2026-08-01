# Overnight autonomous loop — claude-9 ground control, 2026-07-30

Joe left ~22:40 local. Standing instruction: **work through the remaining
problems that still carry sorries, keeping ground control in the loop** —
because per-run review has repeatedly caught errors that a bare cron would have
landed silently (statement repairs, queue-integrity bugs, a false causal
diagnosis of my own, three misattributed turn-rounds).

This document is the contract. If you are a continuation of claude-9, or a fresh
session inheriting this role, **read this first and follow it literally.**

## State at handoff

| | |
|---|---|
| corpus | **81 / 135 clean, 146 executable sorries** (started the day at 63 / 172) |
| closed 2026-07-30 | 18 problems + 1 construction target |
| queue | 49 untouched · 31 resolved · 4 held-out · 2 blocked-on-construction-target · 1 blocked-mathlib-frontier · 1 wontfix |
| Codex usage | ~10 % against a 50 % hard stop |
| recall | fixed and verified tonight (`50916c84`, `4fa9af7c`, `647e9a7c`) |

## The chain — this is the whole mechanism

Every turn MUST end by parking. **A turn that does not park ends the loop
silently.** That is the single failure mode that stops overnight progress.

    dispatch → park(awaiting job-id, deadline +45 min)
        → wake (bellback OR deadline)
            → verify → receipt → resolve row → harvest → scribe
                → promote → review → ledger
                    → dispatch next → park …

The deadline is the backstop: if a bellback is lost to pouch eviction, the
deadline wake still fires and the payload says to check the job state directly.

## Stop conditions — enforced inside `scripts/duree_dispatch.py`, do not bypass

- **exit 10** — Codex usage ≥ 50 %. Stop dispatching. Park on a long timer and
  report at wake. Do not raise the threshold.
- **exit 11** — queue exhausted. Stop and write a completion summary.
- **exit 3** — the zai lane is invoking on that problem; both lanes edit the
  same file. Pick another row.

## Rows that must NOT be dispatched

| row | status | why |
|---|---|---|
| `one-sorry-a92j05-lean-main-lean` | `:blocked-on-construction-target` | needs `zeroCountInClosedBall_add_eq` |
| `one-sorry-a94a10-lean-main-lean` | `:blocked-on-construction-target` | same theorem; file says so verbatim at line 140 |
| `rouche-root-count-transfer` | `:blocked-mathlib-frontier` | genuine Mathlib gap, 3 independent confirmations |

`duree_dispatch.py` only selects `:untouched`, so these are already excluded.
Do not "helpfully" reopen them.

## Pre-dispatch checklist — run BEFORE the bell, not after

I got this wrong twice today and wasted two runner slots. Assert all four:

1. row is not in the blocked set above;
2. `'@@' not in packet` — no unreplaced template markers;
3. **the hint's line number actually contains a `sorry` in the current file**
   (this single assertion would have caught both of today's stale-hint wastes);
4. if the row has `:suggested-route`, confirm it reached the packet text.

## Queue editing — READ THIS BEFORE TOUCHING `codex-sorry-queue.edn`

**Adding a key that already exists creates a duplicate, and the EDN reader takes
the FIRST — i.e. the stale one.** This silently defeated a `:suggested-route` I
had carefully written for a01A07: the runner received the old inherited plan
instead. There are currently **11 duplicate-key instances across 8 rows**
(`:receipt` ×3 on three rows, plus `:statement-hint`, `:attempts`,
`:suggested-route`), all created this way.

So when editing a row:

    assert row.count(':status ') == 1          # already done for :status
    assert row.count(':<key> ') == 1           # DO THIS FOR EVERY KEY YOU SET

If the key already exists, **replace its value**; do not append another pair.

**Do NOT attempt a regex de-duplication pass.** I tried one and corrupted four
rows' receipts (one emptied) before restoring from backup. A row-count assertion
does not catch it — the count was right and the contents were wrong. If the
duplicates are to be cleaned, do it as a **parse → mutate → re-emit EDN
round-trip**, verified field-by-field against known-correct values, with nothing
in flight.

**Severity, so this is not over-treated:** the audit trail is intact. Every
receipt resolves in the store and the ⊸ register records each one. The queue's
`:receipt` field is a convenience pointer serving a stale value on 8 rows. It is
a wart, not a breakage. **Take a backup before any bulk edit** —
`cp data/codex-sorry-queue.edn /tmp/queue-backup-$(date +%s).edn` — that is the
only reason tonight's mistake cost nothing.



## A named Mathlib gap is NOT automatically a blocker

a93A04 attempt 1 named its obstruction precisely: `sum_edist_le_eVariationOn_of_mem_disjWithin`
needs an arbitrary finite pairwise-disjoint family of unoriented `uIoc` intervals
ORDERED and embedded into a monotone variation partition — which Mathlib supplies
only in the opposite direction. I recorded that as a frontier and told the next
runner to mark the row `:blocked-mathlib-frontier` if the converse was confirmed
absent.

**Attempt 2 proved the converse in one run** — filter degenerate intervals, sort
the active subtype by left endpoint with `Tuple.sort`/`Tuple.monotone_sort`
through an arbitrary `Fin` equivalence, then use pairwise disjointness to embed
the increments. The problem closed axiom-clean.

**So: do not mark a frontier until a runner has actually TRIED the construction.**

**And when you do count attempts, COUNT ATTEMPTS THAT FAIL AT THE SAME POINT —
not attempts total.** a95J08's three attempts all hit the *same* weighted
Hölder/Jensen estimate; that is a wall, and it was marked. a95A02 has had FOUR
attempts and is *not* a frontier, because each advanced a different piece — three
AC lemmas, then the deduction, then the open-set decomposition it had itself
reported missing, then the image-measure step. **Four attempts, four different
pieces, sixteen declarations: that is a long ASSEMBLY.** Marking it on attempt
count alone would have been exactly wrong.
The distinction that matters is between a gap nobody has attempted and one a
runner has attempted and failed. `rouche-root-count-transfer` had THREE
independent attempts before it was marked; a93A04 had one. A precisely-named gap
is a *construction task* until proven otherwise — naming it well is what makes it
attemptable, not what makes it permanent.


## Collecting a result via the jobs endpoint — print ALL of it

When a run completes during a long processing block its bellback may not arrive
as a turn, so you collect it with `GET /api/alpha/invoke/jobs/<id>`. **The
Memory usage section is at the END of every runner report.** If you truncate the
output — `[:1500]` or similar — you will cut off exactly the fields metric 3
depends on.

**a95A04 (2026-07-31): I did this and then filled the metric-3 fields with the
session's most common value, `:timeout`. Recall had actually COMPLETED and a
memory had supplied the entire proof architecture.** Correction receipt written.

    # collect the whole thing, or grep the section explicitly
    curl -s .../jobs/<id> | python3 -c "import sys,json;print(json.load(sys.stdin)['job']['result'])"
    # ...or at minimum
    ... | grep -A 20 "Memory usage"

**If the section is genuinely unavailable, record
`:metric-3-status :UNKNOWN-RESULT-TRUNCATED` — never a specific outcome.**
Defaulting an unknown to the modal value is the same error as scoring an unknown
as zero, and it biases in whichever direction the mode happens to lie.


## A queue field is NOT a channel to the runner

**Only these row keys reach the packet**, via the template's `@@...@@` markers:

    id · kind · file · line · statement-hint · available-support · suggested-route · unblocks

Anything else you add to a row — however carefully worded — is recorded for
*your* benefit and is **invisible to the runner**.

I hit this twice on 2026-07-31. First a truncated `:suggested-route` (a
duplicate-key bug) dropped a caution mid-string. Then I added
`:placeholder-hypothesis-caution` to a95J06 as a *predictive* flag, dispatched
the row, and only afterwards checked whether it rendered. **It did not.** The
runner got no warning at all.

**So: put anything a runner must see in `:suggested-route`, and VERIFY it
rendered before dispatching:**

    python3 -c "...instantiate_packet(row, template)..." | grep -q "<your marker>"

Custom fields are fine for triage and for the operator. They are not a
communication channel.

## Vacuity: test the CONCLUSION, not the unused hypotheses

a95J03 is counted clean and proves nothing. Its symptoms were an unused `hN`
and `hz` — so the tempting check is "flag unused hypotheses". **That check is
wrong, and a93J07 disproved it within the hour.**

An unused hypothesis makes a theorem **stronger** — it proves more with less.
a93J07's `hz₀ : z₀ ∈ Ω` is unused in one declaration purely because
`hclosed : closedBall z₀ r ⊆ Ω` already implies it, and its conclusion
(`∀ᶠ n, ∀ z ∈ sphere z₀ r, F n z ≠ 0`) is entirely substantive. Flagging that
would be a false positive, and the rule would still miss a vacuous theorem whose
hypotheses happen to be used.

**The correct test is on the CONCLUSION: does its subject unfold to something
real?**

    # 0a. IS THE CONCLUSION ITSELF `True`, or trivially satisfiable?  <-- WORST CASE
    # 0b. DOES THE CONCLUSION'S SUBJECT UNFOLD TO A BARE CONSTANT?     <-- EQUALLY BAD
    # 1. find the definitions the conclusion mentions
    # 2. unfold them - is any a bare constant, or a field typed True?
    # 3. ALSO check the HYPOTHESES for inline no-ops: `(h : True)`
    # 4. only then is an unused hypothesis corroborating evidence

**0b was added after a96A02 (2026-07-31), which step 0a alone would have
passed.** None of a96A02's conclusions is `True` — they are `∃ A > 0, ∀ x y,
|f x − f y| ≤ A|x−y|^(1/2)` and `∃ ε > 0, ∀ δ > 0, … ε ≤ ∑ |f(bᵢ) − f(aᵢ)|`,
both perfectly substantive-looking. But `f` is `spikeFunction`, and
`def spikeFunction : ℝ → ℝ := 0`. Under the constant zero the first is
**trivially true** and the second is **false**.

**That is the generalisation worth carrying: a placeholder subject does not just
make positive statements trivial, it makes negative statements FALSE.** A row can
therefore be simultaneously un-dischargeable and vacuous, and neither the sorry
count nor `#print axioms` distinguishes either case from real work. So run 0b on
*every* declaration a row asks a runner to close, not only on ones a scan
flagged: check whether each conclusion's head symbols are defined, and whether
those definitions have content.

**Step 0 is the one that matters most and I added it last.** a95J06's two
remaining targets conclude `∀ ε > 0, ∃ δ > 0, True` and — literally — `True`.
A runner could discharge both with `trivial`: **sorry count 2 → 0, axioms clean,
problem marked CLEAN, nothing proved.** That is precisely the a95J03 failure,
which is counted clean in this corpus and proves nothing. Scan for it:

    grep -nE '(theorem|lemma)[^:]*:[^:]*\bTrue\s*:=' problems/*/lean/Main.lean

A `True` conclusion is worse than a `True` hypothesis: a no-op hypothesis makes a
theorem *falsifiable-but-unconstrained*, while a `True` conclusion makes it
**unfalsifiable and closable**. The sorry metric rewards closing it.

**Inline `(h : True)` binders are a distinct placeholder form and my first scan
missed them** — it only matched `True`-typed structure FIELDS. a95J04's target
was FALSE precisely because `(hR : True)` stood in for a non-extendability
condition: a `True` hypothesis constrains nothing, so a degenerate witness
(`D = Set.univ`, `f ≡ 0`) satisfies it and refutes the conclusion. Scanning for
the inline form found a SECOND instance, `(hf_ac_compl : True)` in a95J06, which
is flagged predictively on that row.

    grep -nE '\(\w+\s*:\s*True\)' problems/*/lean/Main.lean

a95J03 fails at step 2: `windingNumber` is `:= 0`. a93J07 passes: every subject
is a genuine analytic object. Run the placeholder scan first; treat unused
parameters as a hint to look, never as the finding itself.

## Two measurement traps, both found on the first overnight pair

**1. `count_sorries.sh --corpus` reads the WORKING TREE, not HEAD.** With runs
in parallel, an in-flight runner's uncommitted edits land in the corpus count.
On the first overnight pair the corpus read 82 clean / 143 sorries while the
committed state was 81 / 144 — the difference was a01A06's uncommitted,
unverified, still-mutating file. **Take the corpus reading when no run is in
flight, or take it at HEAD** (`git stash` is NOT acceptable — other runners are
writing). Never quote a corpus number in a receipt for run X that includes
run Y's uncommitted work.

**2. `EXECUTABLE-SORRIES` cannot distinguish consolidation from discharge.**
Rewriting N directly-sorried declarations as reductions onto ONE shared sorried
theorem shows as −(N−1) on the headline metric while **zero** additional
declarations become usable. a01A07 went 4 → 2 with **no** genuine discharges;
the two that stopped being counted still carry `sorryAx` transitively. This is
legitimate refactoring — it localises the remaining work — but it is not
progress toward clean, and the metric scores it identically.

So in every receipt for a blocked/partial run, record **both**:

    :sorry-delta-headline           -2
    :sorry-delta-genuine-discharges  0
    :declarations-newly-axiom-clean [...]
    :declarations-still-sorryax     [...]

Establish the second by re-elaborating `#print axioms` over **every**
declaration in the file, not just the ones the runner names. A declaration with
no direct `sorry` may still be `sorryAx`.

## Per-run verification — the review is a real gate

1. `scripts/count_sorries.sh --problem <id>` — the ONLY sanctioned counter.
   Grep has miscounted three times (prose mentions, inline `:= by sorry`).
2. `git show <sha>` and **read the deleted lines**. Do not grep for
   `theorem|lemma` and count hits — prose comments contain those words. Four
   false alarms today came from counting instead of reading.
3. **If the diff re-adds a theorem line, or adds a `variable`, check for
   capture.** LemniscateComponents re-added its theorem below a new
   `variable (hp : 0 < p.natDegree)`; had it been captured the statement would
   have silently narrowed *while passing every axiom gate*.
4. Re-elaborate `#print axioms` from a `/tmp` scratch copy. Independent
   elaboration, not the runner's transcript.
5. `scripts/check_construction_targets.sh` if a target file changed.

## Receipts — `POST /api/alpha/evidence` with header `x-penholder: api`

Without the header the store returns 403 `:missing-penholder` and `urllib`
hides the body; re-issue under `curl` to see it. Validate the EDN locally with
`clojure.edn`/`edn_format` **before** posting.

**Metric 3 discipline, learned the hard way today:** if the offered record shows
`:recall-reason :timeout`, `:store-unavailable` or `:recall-error`, record
`:metric-3-status :NOT-MEASURABLE` with `:metric-3-exclude-from-benchmark true`.
**Do not record it as zero.** A failed recall is not evidence about retrieval in
either direction. I recorded one as zero this afternoon and built a whole
architectural diagnosis on it; correction receipt
`ae49c591-768a-4db4-a8da-e1cffbf8a7ab`. The packet now carries
`[dispatch-recall-outcome=…]` and runners copy it verbatim — read it.

## Harvest — per SESSION, not per run

- Find the runner's session by **grepping the rollouts for that run's commit
  sha** (unique). Declaration names are ambiguous — problems get re-attempted —
  and file mtime is wrong: the newest files are often other agents.
- `rollout_harvester.bb --session <uuid> --allow-nonfixture --commit`.
- **A harvest writes every new turn in the session, not just your run's.**
  Verify the written turn-round's *body* mentions your problem before citing it
  to the scribe. Pass 26 caught me handing over ids belonging to neighbours.

## Scribe → promote → review

- One scribe pass per proving run (Joe's instruction: run N's memories should be
  available to run N+1).
- **Promotion alone leaves the attachment `:proposed` and therefore
  unrecallable.** Always follow with
  `review_codex_lane_attachments.clj --names-file F --commit` with
  `ATTACHMENT_REVIEW_EVIDENCE_PREFIX=e-review-codexpilot-` and
  `FUTON_SUBSTRATE_URL=http://127.0.0.1:7073`, then verify
  `:attachment-status :reviewed`.
- **Choose patterns deliberately.** Place new memories in *existing* patterns
  unless there is a real new distinction; do not open an 11th component. The
  corpus is 10 flat clusters with 34 % concentration in one.
- **The scribe's drafting judgement beats mine.** It declined seven of my
  suggestions today and was right every time. Supply evidence; let it judge. If
  it declines, record that as a good outcome and do not push back.
- If a run's recall FAILED, tell the scribe explicitly not to draft any
  recall/terrain/coverage memory, and say why. Twice today that instruction kept
  a false inference of mine out of the corpus.

## Then

`python3 scripts/build_surface_payoff.py --commit` — the ledger is generated
from receipts, never hand-edited.

## Morning report

Append to `holes/ops/claude-6.md` (the ⊸ register) as you go; it is the audit
trail. Before Joe returns, write a short summary at the top of
`holes/ops/OVERNIGHT-SUMMARY-2026-07-31.md`: problems closed, sorries delta,
anything blocked, any error you caught in your own work, and anything you are
uncertain about. **Report failures and uncertainties first, not last.**

## Operator visibility

**Is the loop still alive?** — the park is the loop. Use the AGENT-FILTERED
query; the bare endpoint is misleading:

    # CORRECT — shows the live park
    curl -s 'localhost:7070/api/alpha/parked?agent=claude-9' | python3 -m json.tool

    # MISLEADING — returns {"parked":[]} even when a park is live
    curl -s localhost:7070/api/alpha/parked

A non-empty `awaiting` list with a future `deadline-ms` means the chain is
armed. An empty result from the *filtered* query, with no job running, means
**the chain has stopped** and needs a nudge.

    curl -s localhost:7070/api/alpha/invoke/jobs/<job-id> | python3 -m json.tool
    /home/joe/code/futon3c/scripts/count_sorries.sh --corpus
    tail -40 /home/joe/code/futon3c/holes/ops/claude-6.md



## DISPATCH FIRST, THEN PROCESS — the chain has stopped three times

The chain died at 00:20, 02:02 and 03:20. Every time the shape was identical: a
turn's awaited jobs completed, the park was consumed by that completion, the turn
did its processing work, and ended without dispatching. Nothing remained to wake
anything. The heartbeat caught all three, but the loop still lost ~20 minutes
each time.

**So invert the order. On every wake:**

1. **Select and pre-check** the next rows (separate step — the asserts must be
   able to *gate*, so never batch them with the dispatch).
2. **Dispatch** them.
3. **Read the job ids back**, then **park** on them.
4. *Only then* process the completed work — verify, receipt, resolve, harvest,
   scribe, promote, ledger.

This makes the gap structurally impossible rather than a thing to remember.
Processing is long and interruptible; dispatch is short. Do the short
chain-preserving thing first.

Corollary, learned the same way: **never compose a park in the same command as
the dispatch whose id it awaits.** I did this again at 03:25 and the scribe job
was left unparked. Dispatch, read the id back, then park.

## The heartbeat — a dead-man switch, and when to re-arm it

A **heartbeat park** awaits a deliberately non-existent job so its deadline
always fires (~58 min). It exists because the chain has died twice: both times a
turn's work ended exactly when the last awaited job completed, consuming the
park, and the turn then finished without dispatching. **The "every turn must
park" invariant cannot be maintained by intention**, so the heartbeat makes a
missed park recoverable instead of fatal. It has fired twice and caught a stopped
chain both times.

**Re-arm ONLY when the heartbeat has actually FIRED** — not on every dispatch.
There is no unpark endpoint, so arming a second while the first is still live
leaves two, and they drift apart. (I did exactly this at 02:50: `park-afdf57c7`
had 2600 s left when I armed `park-905d7e01`.) Two heartbeats are harmless — an
extra wake — but the correct discipline is:

    curl -s 'localhost:7070/api/alpha/parked?agent=claude-9'
    # count entries whose awaiting[0] does NOT start with "invoke-"
    # arm a new heartbeat only if that count is ZERO

On a heartbeat wake: check for a REAL park (awaiting an `invoke-` id) and whether
any codex agent is busy. If neither, the chain has stopped — restart it from this
document. If it is healthy, just re-arm.

## Store restarts — AUTHORISED (Joe, 2026-07-30 22:50)

**You may restart the evidence store yourself. It does not disrupt Agency.**

    systemctl --user restart futon1b-server        # unit MainPID == the :7073 store

This supersedes the blanket "never restart" reading of I-0 **for the store
only**. The distinction still matters and is not negotiable:

| process | port(s) | restart? |
|---|---|---|
| `futon1b-server` (evidence store) | 7073, 7072 | **YES — authorised** |
| futon3c serving JVM (`make dev`) | 7070, 6768, 3100 | **NO — I-0 stands** |

**When to restart — do not do it reflexively.** Restart when one of:

- two consecutive dispatches record `:recall-reason` of `:store-unavailable`,
  `:recall-error` or `:timeout` in their offered records; or
- store RSS exceeds ~10 GB (`ps -o rss= -p $(systemctl --user show
  futon1b-server -p MainPID --value)`); or
- host available memory falls below ~4 GB (`free -m`, the **available**
  column, not "free").

At handoff: 7.13 GB RSS after 8.5 h uptime, host 14.4 GB available — below all
three thresholds, so no restart was taken.

**Do not restart while a proving run is mid-flight if you can avoid it** —
runners issue error-recall calls during a run and a restart will fail them.
Prefer the gap between finishing one run's turn-round and dispatching the next.

**After restarting, verify before dispatching:**

    curl -s -o /dev/null -m 10 -w "%{http_code} %{time_total}s\n" \
      http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-diagnose-recall-empty-before-declaring-terrain-gap

Expect `200`. Early reads after a restart can 502 or run slow; do not dispatch
into that window and then read the resulting empty recall as evidence.

**What a restart does and does not buy** (measured today, see
`docs/xtdb2-memory-latency-2026-07-30.md`): it resets accumulated RSS and
metaspace — 7.76 GB → 1.82 GB, metaspace 957 MB → 122 MB — and RSS then regrew
+4.3 GB over the following 6 h. It does **not** make list reads fast; it
returns them to an already-slow baseline. So restart to reclaim memory, not to
fix latency.

## Things that will bite you

- Swap was fully consumed at handoff (14.4 GB available, so not yet a hazard).
- Long jobs: use `scripts/bg.py`, never bare `&` — background shells die with
  the pouch.
- Do not bell a recipient not on `GET /api/alpha/agents`; the failure is
  asynchronous and silent.
