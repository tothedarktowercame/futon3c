# CL-35 — M-repl-circus acceptance obligations 2–4 survey (2026-11-17)

## Result

None of obligations 2, 3, or 4 is fully discharged.

- Obligation 2 is **PARTIAL**: live Circus GCC writes, a current Giraffe pass and
  priority projection, and a War Machine reader all exist, but no receipt traces
  one operator turn through those stages to a reader decision. No correction or
  rejection has been followed through to that reader. Joe has not named the
  question/decision or accepted a narrower reader scope.
- Obligation 3 is **PARTIAL**: recent complete turns carry the expected animal
  fields and GCC outcomes, and CL-23 through CL-34 made failures durable. There
  is still no declared REPL/turn population with numerical capture, processing,
  projection and freshness deadlines. Two known turns lost their replies before
  alignment, and the current checks do not join all stages under one bound.
- Obligation 4 is **PARTIAL**: the inputs are individually pinned or versioned,
  writes are mostly content-addressed/idempotent, late output and failures are
  visible, and superseded/rejected Giraffe work is retained. There is no complete
  declared source inventory and no incremental/replay-versus-rebuild semantic
  graph comparison has been run.

This is a read-only survey. It did not run a rebuild, replay, backfill, reload,
store write, deep health check, or process signal. Public examples contain turn
ids/timestamps and aggregate counts only; they contain no operator or memory
text.

## Method and evidence snapshot

I read the obligation text in `holes/missions/M-repl-circus.md:803-844`, its
obligation-6 witness, CL-26, the CL-31 findings and CL-25..CL-34 review log in
`~/notes/claude-13/closure-plan-2026-11-16.md`, the relevant producers/readers,
the latest Giraffe run, and bounded local turn metadata.

The latest run inspected was
`storage/giraffe/20261117T005232.101Z-e8a98330`. Its verified manifest records:

- futon3c git SHA `7ebdcd371d4eae89282a4e500b0c870466cbc3b9`;
- readings generation
  `846cf52702cb7267e30217ae0ac4652c5d2b3db77e0eba3e28702ab204de50fc`;
- pass input artifact `input.edn`, SHA
  `e8a9833043be146fa9e3c03be22d747c27da4534d54f5fe02d4209f0b6c33106`;
- edges source SHA
  `2c728646aaa9f2b7dd067abdd37fc48df86cdc41baf373900f21cd9cfffa240e`;
- Hou focus input: producer `:hou`, six-hour window ending
  `2026-11-17T00:51:56.840230941Z`, 19 header clusions and 842 evidence pins;
- priority projection present, cutoff 20, SHA
  `d0f45dc80922391653945930f6027fa8d2c95417dd65a7cb2d63a05591271c0a`.

A read of the 60 newest local records (00:10–01:01Z) found six operator turns.
All six had replies, `:lu_ranked`, `:dog_recall`, `:alignment`,
`:pattern_verdicts`, and `:pattern_occurrences`; none retained a current
`*_failure` receipt. This is a useful recent sample, not a declared population
or deadline check.

## Obligation 2 — One complete live chain

### Clause 2.1

> “Witness an operator turn producing a meaningful GCC change, Giraffe updating
> against that change, and a named reader (Joe's view or the War Machine)
> consuming the result and citing its basis.”

**PARTIAL.** The obligation-6 live query witnesses the first segment: after
CL-29/CL-30, the 驴+馬+狗 join returned 252 rows over 33 turns in 83 ms, and
turn→mention→猴/Giraffe joins returned rows. The latest run proves Giraffe
subsequently produced pinned `pass.edn` and `priority.edn` artifacts from Hou
focus inputs. The named War Machine implementation reads verified Giraffe
`manifest.edn` and `pass.edn` in
`futon2/src/futon2/aif/outer_observation.clj:216-275`; its ticket-readiness path
also reads the manifest-pinned `priority.edn` in
`futon2/src/futon2/aif/full_loop_runtime.clj:319-335`.

What is missing is the composition witness: no recorded consumer receipt names
one of the 33 turn ids, its GCC records, the Giraffe run/input pin derived from
it, and the War Machine or Joe-view decision that consumed it. The WM click-loop
receipts found locally stop on 2026-11-07, before the inspected run.

Smallest next step (**live run**): choose one new operator turn that names an
item, retain its turn/GCC record ids, wait for the resulting Giraffe run, then
run one bounded War Machine/Joe-view read and preserve its receipt containing
the run id, manifest/pass or priority hash, selected item and cited basis.

### Clause 2.2

> “State what question or decision that reader serves.”

**ABSENT.** The implementation exposes possible readers, but the acceptance
record does not say whether this witness serves Joe's “what next?” view, War
Machine outer selection, ticket preparation, or another decision.

Smallest next step (**question only Joe can answer**): name the accepting reader
and its decision/question. Record that answer before selecting the live witness.

### Clause 2.3

> “Exercise a correction or rejection as well, showing how it supersedes the
> earlier interpretation and reaches the reader.”

**ABSENT.** `[:supersedes 1]`, Giraffe superseded requests, `rejected.edn`, and
source-change handling exist and remain inspectable, but no correction/rejection
has been traced from a turn through a newer graph interpretation and Giraffe run
to the same reader.

Smallest next step (**live run**): correct one item relation or reject one
reading using the normal typed path; record predecessor/successor or rejection
ids, mint the replacement run, and show the named reader refusing the old pin
and consuming the new one.

### Clause 2.4

> “Mention records alone do not establish readiness, focus or an accountable
> next action.”

**WITNESSED as a constraint, not as chain completion.** The live witness goes
beyond mentions: `[:hou/focus 1]`/`[:hou/focus-relation 1]`, `[:needs 1]`, a
pinned pass and a priority projection exist. This prevents a mention-only claim.
It does not supply the missing consumer receipt in clause 2.1.

### Clause 2.5

> “The promised scope remains the turn-to-War-Machine composition unless Joe
> explicitly accepts a narrower outcome; one demonstrated reader does not
> silently discharge obligations to another promised reader.”

**PARTIAL.** The War Machine reader exists in code, but there is no current
turn-to-WM live receipt. No explicit narrower acceptance by Joe was found.

Smallest next step (**question only Joe can answer**, then **live run**): Joe
either confirms War Machine scope or explicitly accepts Joe/Mojira as narrower
scope; run the clause-2.1 witness against the accepted reader.

## Obligation 3 — Bounded completeness

### Clause 3.1

> “Define the population of REPLs and turns, and numerical deadlines for
> capture, animal processing, GCC projection and priority freshness.”

**ABSENT.** `turn-coverage` has a historical bounded run (52/52 for claude-13
since 2026-11-12T00:00Z), and individual operations have transport/query
timeouts, but there is no acceptance contract enumerating REPLs/surfaces and
giving four numerical stage deadlines.

Smallest next steps:

1. (**Question only Joe can answer**) approve the authoritative REPL/surface
   population and the four deadlines.
2. (**Code**) make one read-only staged coverage report take that population and
   deadlines and emit counts/overdue ids separately for capture, animals, GCC,
   and priorities.

### Clause 3.2

> “Check these stages separately: each required animal result is present or has
> a typed absence with a reason; failures and overdue work remain visible.”

**PARTIAL.** Presence and typed failure mechanisms now exist:

- 驴 `:lu_ranked`/`:lu_ranked_failure`, retry cap 3 (CL-23/25);
- 象 `:analysis_dispatch :dispatch_failures` (CL-19);
- 狗 typed recall states/drop counts, outer receipts, late-undelivered claims,
  and per-hook GCC outcomes (CL-18/28/30);
- 馬 typed model outcome, `:ma_listener_failure`, retry cap 3, and
  `:pattern_occurrences` outcome (CL-20/21/29/33);
- mention and 猴 staged receipts, with 猴 blocked on mention failure (CL-34);
- durable Giraffe service failures (CL-32).

CL-26 supplied an aggregate cross-stage census, and the newest six operator
turns are complete in the fields checked. But no single report applies declared
deadlines to every stage or labels overdue work. Absence receipts are also not
yet normalized into one shared status vocabulary.

Smallest next step (**code**): build the read-only staged report from clause
3.1, mapping each animal's existing success/absence/failure receipt without
changing any writer.

### Clause 3.3 — T-alpha and T-beta

**ABSENT at reply capture; correctly not classified as 馬 loss.**

- `T-alpha` (`turn-RFt13l`, 2026-11-16T17:24:38Z) is an operator record with
  驴 output but no `:reply_text`, `:reply_at`, ports, alignment, 馬 verdict or
  occurrence outcome.
- `T-beta` (`turn-UmRxcn`, 2026-11-16T17:49:07Z) has the same gap.

The reply should have been recorded on each canonical local turn JSON record
under `~/.emacs-graph/session-turn-analysis/turn-*.json` as `:reply_text`
(with `:reply_at`), before turn-service computed `:ports`/`:alignment` and
raised the later lifecycle events. The operator turns themselves and 驴 results
survived; the agent replies did not enter the record. This is obligation-3
reply capture, outside the 馬 listener.

Smallest next step (**code**): add a bounded reconciliation check that reports
operator records whose dispatched/completed agent turn lacks `:reply_text` and
its source reply/evidence id; do not synthesize the missing reply. If the source
reply still exists, a separate reviewed recovery packet can attach it.

### Clause 3.4

> “Check current item/document digests, the pass's input digest and the focus
> version used for priorities.”

**PARTIAL.** The latest manifest pins the readings generation, edge-source
SHA, the `input.edn` SHA consumed by the pass, 842 focus evidence pins, a
six-hour Hou window, the priority artifact SHA, and producer git SHA.
Item/readings source drift is represented in the manifest. Thus the item/source
and pass-input pin portions are witnessed for this run. The focus is pinned by
evidence set/window but does not carry an explicit schema/version field called
`focus-version`.

Smallest next step (**code**): add an explicit focus record/type version to the
run manifest and make the existing verifier compare it with the current focus
source; retain the existing `input.edn` digest check.

### Clause 3.5

> “Repeatedly renewing a request must not make stalled work appear healthy.”

**PARTIAL.** The durable reread queue preserves attempts, leases, supersession
and service failures; CL-32 added retained recent service failures. These are
the right primitives, but no acceptance check compares original requested time
against the declared freshness deadline independently of renewals.

Smallest next step (**code**): report age from the first unsatisfied request and
landed-reading time, never `updated_at`, and fail freshness after the approved
bound even if leases/requests are renewed.

### Clause 3.6

> “Retain the attachment invariant: a stepper is attached to exactly one REPL,
> shows that REPL's newest record, and capture works without a stepper attached.”

**PARTIAL.** Indexed stepper buffers and the Reazon invariant exist
(`f5526ed01`, `983896667`), while `turn-coverage` checks capture independently
of stepper display. No current live rerun was recorded for this closure, and
T-alpha/T-beta prove that operator capture alone does not imply reply capture.

Smallest next step (**live run**): run the attachment check for every declared
REPL, then create one operator/reply turn with no stepper attached and show both
evidence and canonical turn record, including `:reply_text`, within deadline.

## Obligation 4 — Recoverable projection

### Source inventory as it exists today

There is no single authoritative inventory document. The recoverable inputs I
could identify are:

| Source | Store/pin today | Version or identity |
|---|---|---|
| Participant evidence | futon1b :7073 Evidence Landscape | evidence ids, bodies/tags and timestamps; irreplaceable operator turns, replies and inter-agent dispatches |
| Circus turn projection | `~/.emacs-graph/session-turn-analysis/turn-*.json` | record `:version 1`; current sample `:interpretation_version 3`; source evidence id and codepoint offset convention |
| Pattern definitions | `/home/joe/code/futon3/library/*.flexiarg` | content SHA in external references |
| Work items/documents | mission/excursion/ticket files across the code roots plus their futon1b entity/clusion records | current document SHA; `[:workplan/mention 2]`, `[:supersedes 1]` |
| Circus GCC | durable futon1b :7073 Arxana records | `[:pattern/proposal 1]`, `[:interpretation 1]`, `[:pattern/occurrence 1]`, `[:memory/recall 1]`, `[:memory/use 1]`, `[:memory/burial 1]`, `[:memory/repetition 1]`, `[:workplan/mention 2]`, `[:hou/focus 1]`, `[:hou/focus-relation 1]`, `[:needs 1]`, `[:supersedes 1]` |
| Memories | migrated durable futon1b :7073 records and external references | memory ids/revisions; Circus relations above are version 1 |
| Giraffe readings | immutable corpus generation currently pointed at generation `846cf5…50fc` | member source SHA plus checked JSONL reading |
| Giraffe queue/failures | `~/.local/state/futon3c/giraffe-reread/requests.sqlite` | attempts, state, source SHA, supersession and retained service failures |
| Giraffe products | immutable directories under `storage/giraffe/<run-id>` | manifest producer git SHA, readings generation, source pins, file SHA256s |
| Migrated workplan judgements | futon1b :7073 (formerly :7094) | introduced entities, verdicts and record ids; not recomputable from turn records alone |

The source-code snapshot inspected was futon3c `fa56bec2a`, futon1b
`5be8ce86c`, and futon2 `d430ce58b`. These are observation pins, not a declared
inventory version.

### Clause 4.1

> “Declare and preserve the complete versioned source inventory.”

**PARTIAL.** The table above can be reconstructed from code and manifests, and
the important stores remain present. However, it is not declared as one
versioned authority, and the old claim “rebuild from turn records” omits
introductions, verdicts, readings, serves judgements and participant evidence.

Smallest next step (**documentation/data declaration**): publish one
machine-readable inventory listing each authoritative source, schema/version,
pin method, retention rule and rebuild role; include the non-recomputable
migrated records explicitly.

### Clause 4.2

> “Demonstrate that duplicate delivery, late output, interruption and replay
> converge to the same semantic graph as a rebuild from those sources,
> including supersession and retraction.”

**PARTIAL, with the central comparison ABSENT.** Deterministic record/message
ids and tests cover duplicate delivery; CL-30 demonstrates late output as an
undelivered claim; CL-23/28/32/33/34 preserve interruption failures; item and
queue supersession are implemented. No full replay/rebuild comparison has been
run, and no result compares semantic graph projections while excluding
operational fields. Retraction convergence has not been demonstrated.

Smallest next steps:

1. (**Code**) implement a read-only semantic graph digest that excludes declared
   operational fields and sorts records canonically.
2. (**Live/offline run**) project a fixed inventory snapshot twice—incremental
   replay with duplicate/late/interrupted deliveries, and clean rebuild—then
   compare digests and typed differences. This survey deliberately did not run it.
3. (**Live/offline run**) include one supersession and one retraction fixture in
   that comparison rather than inferring them from unit tests.

### Clause 4.3

> “Record the comparison basis and any excluded operational fields.”

**ABSENT**, because no comparison exists.

Smallest next step (**documentation emitted by the comparison code**): store
the inventory pin, schema versions, semantic field allowlist/exclusion list,
both graph digests, counts and differences beside the run.

### Clause 4.4

> “Rejected and superseded readings must remain queryable so recovery can
> distinguish unexplored paths from previously rejected ones.”

**PARTIAL.** Giraffe runs retain `rejected.edn`; the SQLite queue keeps
`:superseded` rows and reasons; `[:supersedes 1]` remains queryable in the GCC.
There is no single recovery query joining these histories, and no rebuild
witness proves they survive reconstruction.

Smallest next step (**code**): add one bounded read-only recovery query/report
that returns current, rejected, and superseded reading states with predecessor,
reason and source pin; exercise it in the comparison run.

### Clause 4.5

> “Preserve irreplaceable sources before retiring stores or deleting run
> directories.”

**PARTIAL.** Participant evidence is durable in futon1b; :7093 memories and
:7094 workplan records were migrated before retirement; current Giraffe run
directories and immutable reading generations remain. Without the complete
inventory and reconstruction test, preservation completeness is not proved.

Smallest next step (**code/policy gate**): make retirement/deletion refuse
unless every inventory entry marked irreplaceable has a verified destination
pin and the latest recovery comparison names it.

## Packet-sized next steps, ordered

1. **Joe decision:** name obligation 2's accepting reader and decision; confirm
   War Machine scope or explicitly accept a narrower Joe/Mojira scope.
2. **Live chain receipt:** one new turn → GCC ids → Giraffe run pins → accepted
   reader decision with cited basis.
3. **Correction receipt:** one normal correction/rejection through the same
   chain, proving the reader refuses the predecessor.
4. **Bounds declaration:** authoritative REPL population and four numerical
   deadlines, approved by Joe.
5. **Staged completeness report:** existing fields/receipts plus deadlines,
   including reply-capture absence and first-request age.
6. **Run pins:** canonical items+relations digest and explicit focus version in
   the manifest/verifier.
7. **Reply-capture reconciliation:** detect completed agent turns missing
   `:reply_text`; T-alpha/T-beta are the witness fixtures.
8. **Versioned source inventory:** one machine-readable declaration including
   migrated, non-recomputable records.
9. **Semantic graph comparator:** canonical digest/diff plus duplicate, late,
   interruption, supersession and retraction replay fixtures.
10. **Recovery history query and retirement gate:** make rejection/supersession
    queryable together and block deletion without preserved source pins.
