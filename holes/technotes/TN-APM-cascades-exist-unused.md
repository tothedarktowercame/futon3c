# TN-APM-cascades-exist-unused — the memory cascade is built, and is not on the path that feeds the student

claude-clink-1 (APM frame-watcher), 2026-08-26, from f42/a97J07 while the frame
was live. Written at Joe's request after he asked what shape the available
memories actually have — the v2/v3 memory whitepaper models them as a pattern
language "cascade", and he noted it is not guaranteed that this is what Zai
sees.

Repo state: futon3c `15999918`, campaign `jit-all-open-nontopology-v1`, frame
f42, snapshot
`data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f42/snapshots/f42-solver-memory.edn`.

## HEAD (one line)

**Zai is handed a flat list of leaf memories sorted by hash; the cascade exists
in `conductor.clj`, is reachable only from an interactive HTTP surface, and has
left zero artifacts in the entire campaign.**

*(Re-verified 2026-08-26 across all seven campaigns, not one. Two claims below
are corrected by addenda: pattern→pattern edges DO exist, as relations rather
than hyperedges ("Addendum — 2026-08-26"); and "zero artifacts" is true of the
campaign directories only — the round-1 conductor path ran the cascade in four
frames and persisted routed offers under `data/problem-state/`, delivered to
nobody ("Addendum 3"). The HEAD line stands for the countdown path that feeds
the student.)*

The question that prompted this — for the f42 use, was Zai using patterns or
leaf memories? — has a clean answer: **leaf memories**. The 29 identifiers the
fingerprint audit counted were extracted by scanning one memory's flat `:body`
prose for Lean-ish tokens. They were never a structured field, and no
pattern-shaped object was in play.

## What the student's shelf actually is

For f42, `:snapshot/memories` is a **flat vector of 48 entries sorted by
`:memory-id`** — that is, ordered by hash, so the ordering carries no relevance
signal. The memory that was used, and that closed the problem, sat at **position
47 of 48**.

Each entry is a leaf record plus attachment metadata:

    {:name, :hook, :kind, :body}          <- the memory; :body is one long
                                             prose+Lean narrative, not nested
    :pattern-ids, :depositor, :reviewer, :review-evidence-id,
    :provenance, :source-attempts, :reported-content-digest, :reported-kind

The only aggregate structure in the snapshot is `:snapshot/provenance-summary`,
a frequency count of source frame-ids. There is no grouping, no ordering by
pattern, no parent nodes. `snapshot-body` (`memory_snapshot.clj:36`) builds
exactly this and nothing more.

## Patterns exist, but as tags on leaves rather than as a language

f42's 48 memories carry **23 distinct patterns**: 35 memories with exactly one,
13 with two, across three namespaces (`math-formalization`,
`math-formalization-CA`, `math-formalization-CV`).

The decisive check is what edges a pattern participates in. Pulling every
hyperedge that has a pattern as an endpoint:

    GET localhost:7073/api/alpha/hyperedges?end=math-formalization/cast-normalization
    -> edge types: {:memory/assert 10}

**Every hyperedge touching a pattern is `:memory/assert`.** There is no
pattern→pattern edge of any kind — no refines, no uses, no specialises, no
containment. Two patterns are related only implicitly, by being tagged onto the
same memory. The data model is a bipartite memory↔tag graph. A pattern language
needs relations among the patterns themselves, and there are none to traverse.

### The implicit structure is thin, and it is an API index

Of the 23 patterns, **13 attach to exactly one memory** — more than half induce
no grouping at all. Nearly all the connective tissue is three hubs:

| memories | pattern |
|---|---|
| 14 | `math-formalization-CA/measure-integration-api` |
| 8 | `math-formalization-CV/holomorphic-disk-api` |
| 7 | `math-formalization/cast-normalization` |

Read what those names denote. Two of the three are `*-api`: they mark a region
of the Mathlib surface. That is a library index, which groups by where a lemma
lives, not by the recurring context-and-resolution shape a pattern language
groups by. So even the grouping that exists is not the kind the whitepaper
models — worth separating from the raw counts, which look like clustering.

## The cascade is built, and it is good

`conductor.clj` carries exactly the vocabulary the whitepaper describes:

- `expand-memory-cascade` (line 287) and `cascade-receipt-offers` (line 383);
- routes labelled `:leaf` at 0 hops, then `:why-hop` and `:co-incidence`,
  sorted by hop count, with a cap and a `:truncated?` flag;
- `pattern-surfaces`, which renders a pattern as its own object via `pattern-fn`
  — patterns as things the reader is shown, not merely tags carried alongside;
- `domain-general-pattern-id?`, which filters expansion to patterns that are not
  problem-specific.

This is a cascade. It is not a sketch.

## It is not on the path that produced f42

`countdown_control.clj` drives this campaign. Its `:require` list does not
include `futon3c.apm.conductor`. The cascade's only consumer is
`conductor_surface.clj`, which is exposed through HTTP handlers in
`transport/http.clj` — an interactive conductor surface a person drives, not the
automated campaign path.

Confirmed from artifacts rather than from imports, because an unused `require`
would prove nothing either way:

    grep -rl -- '<key>' data/apm-campaigns/jit-all-open-nontopology-v1/ | wc -l

    :hops             0        :co-incidence     0
    :why-hop          0        pattern-surfaces  0
    :seed-patterns    0        :route            2   <- both unrelated, see below

The two `:route` hits are prose, not cascade labels: `:route "argument principle
via ConstructionTar..."` in an f34 trace and `:route "from-scratch
Casorati-Weierstrass in f..."` in an f33 trace, both describing a *mathematical*
route through a proof. Neither is `:route :leaf` / `:why-hop` / `:co-incidence`.
Recorded because the raw grep count is not zero and a later reader running it
will see 2; the distinction is in the values, not the key.

**No cascade artifacts across the whole campaign.** Not in f42, not in f28–f41.
If the cascade had ever run on this path it would have left route labels and hop
counts in a receipt, and there are none.

### One residue

`:reported-kind` is `:leaf` on **4 of the 48** entries and absent on the other 44.
The leaf/non-leaf distinction was plumbed into the candidate record and then left
almost entirely unpopulated. That is the only trace of the cascade's vocabulary
anywhere in the campaign data.

## The boundary: what Zai literally saw is not recoverable

Everything above is the snapshot artifact plus the code path. It is **not** the
text handed to Zai, and that text cannot be read:

- the frame records do not archive the student packet — `pattern` does not appear
  in any student-facing file under `f42/live/`, only in `promote-solver.edn`;
- the Agency job that dispatched `f42-student`
  (`apm-role-676f2186...`) retains `result` and `result-summary` and has
  **`prompt` empty**.

So whether the renderer passes `:pattern-ids` through to the prompt, drops them,
or reformats them is one step this note cannot close.

That gap deserves naming on its own terms. The question asked was whether the
available memories are what Zai sees, and the machine's own records cannot
answer it: the snapshot stands in for the prompt. This is the same substitution
`TN-apm-watcher` is organised around — a claim occupying the place where an
artifact should be — appearing here at the point where the experiment's
independent variable is delivered.

## What follows

Stated as options, not decisions. Whether the cascade should be on this path
changes what the experiment measures, which `TN-apm-watcher` puts in Joe's hands.

1. **Archive the rendered packet per attempt.** The cheap fix, and it closes the
   boundary above. It would also let `fingerprint_audit.py` separate *surfaced
   and ignored* from *never actually shown* — currently indistinguishable, and
   the difference decides whether a zero-uptake frame indicts the memories or the
   delivery.
2. **Decide whether the flat shelf is the intended delivery.** If it is, the
   whitepaper's cascade model describes a capability the experiment is not
   exercising, and should say so. If it is not, `expand-memory-cascade` needs
   wiring into the countdown path, which changes the independent variable
   mid-campaign and is not a change to make quietly.
3. **The ordering is free to fix either way.** Sorting the shelf by hash is not a
   design choice anyone made; it falls out of `(sort-by :memory-id candidates)`.
   The f42 memory that closed the problem was 47th of 48.
4. **The pattern corpus is a library index.** If patterns are meant to carry
   context/resolution structure, the current vocabulary does not, and no
   pattern→pattern relation exists to build a language on. This is presumably the
   pattern-corpus gap `TN-spec-delta` §14 records; this note gives it numbers.

## How to re-run every check here

    # shelf shape and pattern distribution
    python3 - <<'PY'
    import re,collections
    t=open('data/apm-campaigns/jit-all-open-nontopology-v1/'
           'jit-all-open-nontopology-v1-f42/snapshots/f42-solver-memory.edn').read()
    idx=[m.start() for m in re.finditer(r':memory-id "',t)]
    print("entries:",len(idx))
    PY

    # no pattern->pattern edges
    curl -s "localhost:7073/api/alpha/hyperedges?end=math-formalization/cast-normalization"

    # no cascade artifacts (expect 0 for all but :route, which returns 2 prose hits)
    for k in ':route' ':hops' ':why-hop' ':co-incidence' 'pattern-surfaces' ':seed-patterns'; do
      echo -n "$k: "; grep -rl -- "$k" data/apm-campaigns/jit-all-open-nontopology-v1/ | wc -l
    done

    # the prompt is not retained
    curl -s localhost:7070/api/alpha/invoke/jobs/<f42-student job-id> | grep -c '"prompt"'

## Addendum — 2026-08-26 (claude-13): one correction, and what the cascade would have been

*Added at Joe's request ahead of a handoff to Fable. The note's central finding
survives unchanged and was independently re-run: cascade-artifact counts are 0
across **all seven** campaigns on disk, not only `jit-all-open-nontopology-v1`
(`:hops`, `:why-hop`, `:co-incidence`, `pattern-surfaces`, `:seed-patterns`; the
2 `:route` hits are the mathematical prose this note already flags).
`countdown_control.clj` still has no require of `futon3c.apm.conductor`.*

### Correction — there ARE pattern→pattern edges; the query missed them

§"Patterns exist, but as tags on leaves" concludes: *"Every hyperedge touching a
pattern is `:memory/assert`. There is no pattern→pattern edge of any kind …
A pattern language needs relations among the patterns themselves, and there are
none to traverse."*

That was measured with `GET /api/alpha/hyperedges?end=<pattern>`, which returns
only `:memory/assert`. The authored `@why` edges are **relations, not
hyperedges**, and `conductor.clj:257` reads exactly that type:

    GET /api/alpha/relations?type=pattern%2Fhas-semantic-why&limit=1000
    -> 45 relations; 23 touch math-*; 18 touch a pattern on f42's own shelf

    math-formalization-CA/riemann-darboux-api -> math-formalization-CA/measure-integration-api
    math-formalization-CA/ode-gronwall-api    -> math-informal/reduce-to-known-result
    math-formalization/cast-normalization     -> math-formalization/coercion-bridge

The pattern language is thin, but it is **not empty**, and the cascade has
something to walk. Everything else in that section stands, including that the
`hyperedges` view of a pattern is `:memory/assert` only.

### What the cascade would have produced on f42

Reimplemented `conductor.clj:287-382` over live substrate data — BFS on authored
why edges, co-incidence fixed at 2 hops via `pattern -> problem -> pattern`, why
preferred on ties, cheapest route per memory, sorted, capped. **The expander
itself was NOT invoked** (see the plan's H0); if a real run disagrees, the real
run is right.

Validation that the reimplementation is faithful: deriving seed patterns from
the 48 memories' attachments yields **23**, matching the count this note reports
independently.

    seed memories                       48
    seed patterns                       23
    why-reachable patterns               4
    co-incident patterns                32   (from 70 shared seed problems)
    expansions available                141   (53 why-hop, 88 co-incidence)
    selected at cap 100                 100   -> :truncated? true
    delivered shelf                     48 -> 148

The artifact is `holes/f42a-cascade-example.edn`, in the expander's own output
shape, labelled counterfactual, with `:pattern-surfaces` left empty as a hole
because rendering it needs `pattern-fn` against the live store.

### The finding this produces: efficiency here is flooding

- **All 53 why-hop additions come from a single pattern**,
  `math-strategy/missing-dependency-protocol`. 23 seed patterns reach only 4
  why-reachable patterns and effectively one carries the whole expansion.
- **88 of the 141 available expansions arrive by co-incidence.** The code is
  careful that co-incidence "does not recursively flood", and it does not
  recurse — it does not need to, out-producing the authored route 88 to 53.

`docs/retrieval-whitepaper-v2.md` §4.6 already explains why: the store is a
**forest of stars** — one pattern per memory, largest patterns-only component a
single hyperedge — and its own table calls the graph *"essentially unbuilt: the
edges that would make it a graph were never written."* Multi-attachment is
representable; the star shape is an artefact of use. So descending from a
high-level pattern does not narrow, it dumps everything on that star.

Two further bounds from the same paper: **62% of surfacing slots already go to
memories used nowhere** on a 48-entry shelf, so tripling it multiplies waste;
and §5.1, *"None is a proved lemma you can import"* — a runner found its blocking
lemma by repository grep while recall completed and offered nothing, which needs
*"an index over proved artifacts, not a better ranker."*

**Net:** wiring `expand-memory-cascade` onto the countdown path would not fail
loudly. It would flood quietly, mostly with incidental material, onto a shelf
that is already hash-ordered and already 62% unused. Ordering is the change that
helps regardless; populating the graph is the prerequisite to exploiting it.

### Where the plan lives

`holes/PLAN-apm-cascade-demo-instance.md` turns this note into six sequenced
handoffs — **H0** dry-run the expander (acceptance test: reproduce or correct
the 141/100 above), **H1** archive the rendered packet (closes this note's own
boundary section), **H4** the f42a counterfactual, **H2** replace the hash sort,
**H5** populate the graph, **H3** wire the cascade why-hop-only, last and
optional. H0 and H4 each carry a stopping condition.

Also: `futon2/holes/E-cascade-sampler-four-2026-08-26.md` places this beside the
WM's two cascade mechanisms — of the three in the stack, exactly one runs on an
automated path, and its content is a frozen July fixture.

## Related

- `TN-apm-watcher.md` — the role this was written from; claims-versus-artifacts table.
- `holes/labs/M-apm-demonstration/prereg-capability-transfer-v1.edn` — Tier-A
  condition 3, which f42/a1 satisfied through the flat shelf, not through a cascade.
- `holes/excursions/E-bell-clink-adapter.md` — same failure shape at the
  coordination layer: a receipt the obligated party could not have produced.
- `holes/PLAN-apm-cascade-demo-instance.md` — the handoff plan built on this note.
- `holes/f42a-cascade-example.edn` — the counterfactual cascade artifact.
- `docs/retrieval-whitepaper-v2.md` §4.6 — why the store is a forest of stars.

## Addendum 2 — 2026-08-26 (claude-19): the live reader could not have run since July

D0 (codex-20, job `invoke-1787758100019-1525-99f8edba`) attempted the first real
invocation of `expand-memory-cascade` over f42's 48 seed ids with
`live-cascade-readers`. It never reached expansion. The reader requested
`GET /api/alpha/hyperedges?end=…&type=memory/assert&limit=5000`, and the
substrate answered HTTP 400, layer 4 `:invalid-limit` (`:maximum 1000`).
~~That cap landed in futon1b `999af15` on **2026-07-22** — before any of the nine
round-1 registrations (f9–f17, 2026-08-18/20) set
`:reg/memory-cascade-enabled? true`.~~ **Corrected the same day (Addendum 3):**
the hyperedge cap is `parse-hyperedge-limit`, futon1b `4cd17bc`,
**2026-08-23** — three days *after* the round-1 frames, which ran through this
reader successfully. `999af15` capped a different endpoint. `response-edn`
throws on any non-200, so from 2026-08-23 a live expansion fails on its first
attachment read; on the countdown path it is unreachable because nothing calls it.

Two further facts from probing the endpoint: the `end=` form ignores `after`
(the same first id is returned), so there is no cursor to page with; and the
largest hub today, `math-formalization-CA/measure-integration-api`, has 74
attachments, so a 1000 window is not a practical limit yet.

Fix (same day, conductor.clj): request the server maximum and **refuse** a
full window (`complete-page`, throws `memory cascade attachment window
overflow`) instead of truncating silently — the §2.2 shape from V3 would
otherwise reappear inside the instrument being built to measure it. D0 is
re-dispatched against the fixed reader.

## Addendum 3 — 2026-08-26 (claude-19): the cascade ran on the round-1 path, and reached nobody

D1 (codex-12, `holes/technotes/D1-round1-cascade-offers-2026-08-26.md`,
commit `a7f8fd2b`) reconstructed frames f9–f17 from the evidence store and
`data/problem-state/<cycle-id>/vN.edn` — the conductor's `:problem-save`
output, which no campaign-directory grep reaches. Spot-checked by claude-19
against the state files directly: routed offers with `:why-hop` and
`:co-incidence` are present for the four frames named, every run has
`:offer/cascade-truncated? true`, and the `expanded-available` values match.

| frame | seeds (solver surfaced-ids) | offers | `:why-hop` | `:co-incidence` | available | truncated at cap 100 |
|---|---:|---:|---:|---:|---:|---|
| f9 | 1 | 101 | 0 | 100 | 115 | yes |
| f10 | 2 | 102 | 48 | 52 | 131 | yes |
| f13 | 5 | 105 | 0 | 100 | 128 | yes |
| f15 (×3 solver attempts) | 5 each | 105 each | 48 each | 52 each | 132 | yes |

f11, f12, f17 reached the same call with **empty** surfaced-ids (nothing to
expand); f14 never dispatched; f16 bypassed the conductor with direct bells.

So the precise claims are now:

1. **The HEAD line holds for the countdown path** — the path that feeds the
   student in the live campaign — and "zero artifacts" was true of the
   directories searched, not of the stack.
2. **On the round-1 path the cascade was built and delivered to nobody.** The
   offers are computed in `record-solver-attempt!` *after* the solver's
   attempt, from the solver's own receipt; the student arm in every one of
   these frames had `:memory-channel :none`. Nothing downstream consumed an
   offer. "Built and used" (PLAN §"precisely") is still true nowhere.
3. **The flooding argument has real numbers.** One to five seed memories
   expand to 115–132 available offers, the cap of 100 fired on every run, and
   in two of four frames the whole expansion is co-incidence (0 why-hops).
   This is claude-13's counterfactual reading of f42 (53 why-hop from one
   star, 88 co-incidence) confirmed on frames that actually ran.
4. **Addendum 2's date was wrong** and is struck through above: the reader was
   runnable until futon1b `4cd17bc` (2026-08-23) and has been un-runnable
   since. The fix in `7534419c` stands; its commit message repeats the wrong
   date and this addendum is its correction.

## Addendum 4 — 2026-08-26 (claude-19): the real run over f42, and the H4 judgement

D0 (codex-20, `915a4aa1` entry point + `scripts/apm-cascade-dry-run.sh`,
`77a1bac0` artifacts `holes/f42a-cascade-run-cap{100,1000}.edn`) is the first
invocation of `expand-memory-cascade` over f42's 48 seed ids with the live
readers. Re-run once by claude-19: body byte-identical (deterministic).

| | counterfactual (claude-13) | real run | delta |
|---|---:|---:|---:|
| expanded available | 141 | **103** | −38 |
| why-hop | 53 | **48** | −5 |
| co-incidence | 88 | **55** | −33 |
| distinct why-reachable patterns (with memories) | 4 | **1** | −3 |
| seed patterns | 23 | 23 | 0 |
| cap 100 truncated? | yes | yes (cap 1000: no) | |
| pattern surfaces | left empty | 9, none empty | |

The real run is right, and the deltas have visible causes. The BFS follows
**outgoing** `has-semantic-why` edges only: f42's seed patterns have 6
out-edges, against the 18 edges "touching" a seed that the addendum counted
(13 of those point *into* seeds). Of the out-edge targets, four are themselves
seeds (dropped) and the rest carry no reviewed attachments except one:
`math-strategy/missing-dependency-protocol`, whose 48 reviewed attachments
are exactly the 48 why-hop additions. The reader also filters to
`:attachment-status :reviewed`, which the reimplementation did not.

**The why-hop block is constant in the seed.** f10 (2 seeds) got 48 why-hop
offers, f15 (5 seeds) got 48, f42 (48 seeds) got 48 (D1 technote; D0
artifacts). Any shelf touching `measure-integration-api` or
`holomorphic-disk-api` — both `@why` the hub — receives the entire hub.

**H4 judgement** (`holes/f42a-H4-judgement-2026-08-26.md`, by a seat that
did not compute the expansion): **no** — none of the 103 additions bears on
f42's mathematical crux (extending open-arc bounds to the sphere's endpoints);
two process memories bear weakly on a one-round-trip `sorryAx` false positive.
The PLAN's "volume without relevance" outcome, on real inputs. Consequences:
H3 stays last and gated; H5 becomes specific (attach the hub's seven
mathematical statements to the API patterns they are about; give its 41
process rules a home not reached from every API pattern); H2 unaffected.

## Addendum 5 — 2026-08-26 (claude-19): the ordering question (H2), measured offline

H2b (codex-20, `a4514534`; corrected by claude-19 — see
`holes/labs/M-apm-demonstration/analysis/NOTE-shelf-order-audit-2026-08-26.md`)
scored the proposed shelf ordering on the 12 archived frames with snapshots
(f28–f42), 75 use events, 56 of them on a delivered shelf. Position of the used
memory, 1-based:

| ordering | median | mean | top 10 of 56 |
|---|---:|---:|---:|
| delivered (hash) | 18.5 | 17.0 | 19 |
| promoted-this-frame first | 2.5 | 4.6 | 52 |
| identifier overlap with base file | 3.0 | 5.3 | 48 |
| **combined** | **3.0** | **3.5** | **54** |

f42's closing memory: 47/48 delivered → 7/48 combined. The five cross-problem
uses: median 15 → 9. The audit had to be corrected once — 586 of 1,099 snapshot
entries carry no text (digest only), and the first run scored them 0.

So the ordering is worth shipping (H2a), and the "47th of 48" in this note's
HEAD was not bad luck: under the delivered order the median used memory sits
at 18.5 on shelves of 20–50.

## Addendum 6 — 2026-08-26 (claude-19): H2 shipped

`memory_snapshot.clj` now orders the shelf by promoted-this-frame, then
identifier overlap with the base problem file, then `:memory-id`
(`844c6ae4`, review fix `ad45cd1f`), records `:snapshot/ordering` on each
v2 snapshot, and fetches text from the store for the 586-of-1,099 textless
candidates. Reloaded from master into the running JVM before f43 published
any snapshot; prereg amendment 6. The HEAD line's "flat list sorted by hash"
is therefore true up to f42 and false from f43. The cascade part of the HEAD
line is unchanged.

## Addendum 7 — 2026-08-26 (claude-19): H5 done, measured before and after

Joe authorised H5a (repoint the four `@why missing-dependency-protocol`
declarations; futon3 `1b75c1f`) with V2's measurements before and after;
H5b (seven hub statements reassigned to their API patterns, reviewer
`codex-20`) landed first. Full note:
`holes/labs/M-apm-demonstration/analysis/h5/NOTE-H5-before-after-2026-08-26.md`.

| f42 run | why-hop | co-incidence | available |
|---|---:|---:|---:|
| f42a (before) | 48 | 55 | 103 |
| f42b (after H5b) | 41 | 55 | 96 |
| f42c (after H5a) | **0** | 96 | 96 |

After H5a the hub's in-degree is 0 and from f42's shelf the why-graph reaches
three patterns with no memories: over this store there is now no pattern that
is both why-reachable from a working shelf and populated. Two things the
plan did not predict: the seven correctly-reattached statements became
invisible to the cascade (their new patterns are seed patterns, and the
expander has no sibling route), and the 96 co-incidence memories were
available all along — cheapest-route credit had been hiding them behind the
hub. V2's instrument (λ₂ on the memory/assert graph, reproduced in numpy)
cannot see H5a at all, and shows the store is no longer a forest of stars:
largest reviewed component 342 nodes / 21 patterns, λ₂ ≈ 0.024.
