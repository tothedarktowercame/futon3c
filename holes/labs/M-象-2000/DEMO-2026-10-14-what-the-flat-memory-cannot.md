# DEMO 2026-10-14 — six things this memory can do that the flat one cannot

Written 2026-10-14T17:56Z (claude-16), every output below run live against the minus40
store (:7093, `minus40-store.service`) and the 象 turn records. "The standard
one" is Claude Code's memory: one directory of `*.md` files per project plus a
`MEMORY.md` index, read whole at session start. It holds the current text of
each memory and nothing else: no past versions, no times beyond a `modified`
stamp, no links across projects, no check that two memories agree, no join to
the conversation that produced them.

## 1. What did memory say on a given day?

The flat store has only today's text. minus40 keeps every version with its
valid time, so `asof` answers for any date.

```text
$ m40.py asof 2026-09-24 zai exhausted
As of 2026-09-24, 2 facts in 2 memories matching zai exhausted:
wm-closure-strategy-zai-owners   said 2026-09-16: "... Codex nearly exhausted ..."
zai-seat-allocation ...

$ m40.py asof 2026-10-14 zai five
As of 2026-10-14, 9 facts in 2 memories matching zai five:
zai-five-hour-window   said 2026-10-12: "Zai has a five-hour token window AND a weekly/monthly limit ..."
```

On 09-24 the stack "knew" Zai was out weekly; the five-hour window, the thing
that actually bit on 10-11, was not in memory until 10-12. The standard store
cannot show that the lesson postdates the incident.

## 2. How did a lesson evolve?

```text
$ m40.py history zai window
17 events for zai window (+ said, - stopped being said):
2026-09-06 + subscription-usage-endpoints  **Zai** — GET .../usage/quota/limit ...
2026-09-27 + proof2-dispatch-pool          Quota facts observed 2026-09-24: ... Zai's weekly limit ended
2026-10-12 + zai-five-hour-window          Zai's binding quota is the five-hour token window ...
2026-10-12 - zai-five-hour-window          (same three lines withdrawn 4 h later)
2026-10-12 + zai-five-hour-window          "Zai has a five-hour token window AND a weekly/monthly limit ..." (merged, cross-project)
```

Three memories in three projects over five weeks, with a same-day rewrite.
The flat store shows one file's final text.

## 3. Net change versus everything that happened

The E-cool-memories distinction, proved in Lean: the endpoint delta hides
facts said and withdrawn inside the interval. Over 10-11 to 10-14, six real
facts are transient:

```text
2026-10-12T15:34 → 19:52  zai-five-hour-window        Zai's binding quota is the five-hour token window ...
2026-10-12T15:34 → 19:52  zai-five-hour-window        **How to apply:** run at most 3 at once ...
2026-10-13T05:10 → 05:14  blueprint-parallel-lemmas   **How to apply:** assign seats per lemma, not per problem ...
2026-10-13T21:18 → 22:20  blueprint-parallel-lemmas   Easiest-first closure (Joe 10-13 evening ...)
2026-10-14T17:33 → 17:35  apm-progress-vs-codex       **Why:** Joe is planning time to completion by usage window ...
```

`net-changed` over the same interval does not list them; `events-between`
does. The checker accepted today's live delta: `M40-DELTA-ACCEPTED 1953→1979`.
The flat store has neither operation.

## 4. Two memories that contradict each other

```text
$ cat ~/code/data/minus40/sorries-open.txt
18 open sorries; 36 memories challenged

A voxterm/route-by-standing-principle      "The needs-joe channel matches ruling-*shaped* blocker text"
B voxterm/surfacing-reads-blocker-fields   "must read ruling-shaped :blocker text, not just owner/status/class"
why: B requires the needs-joe channel to select rows by ruling-shaped blocker
text, while A calls that same matching a defect ... neither note states which
rule is current.
```

Each conflict is a `:memory/sorry` edge with both quotes verified verbatim,
dischargeable with a note, and as-of queries before the discharge still show
it open. The flat store will happily hand a session both notes.

## 5. The same lesson learned twice in different projects

```text
$ GET :7093/api/alpha/hyperedges?type=memory/relation
21 relation edges  (instance-of 4, related 14, refines 3), 8 cross-project
refines      ~/zai-five-hour-window  ⟶  ~/code/voxterm/zai-seat-allocation
             Both notes give the same lesson that exhausted Zai quota makes jobs fail silently ...
related      ~/futon1b-evidence-index-reads  ⟶  ~/code/futon1b-deep-health-can-oom-a-full-heap
             Both notes concern futon1b heap pressure from expensive reads ...
instance-of  ~/lean-top-is-analytic  ⟶  ~/code/statement-registry-policy
```

Written at write time by `relate.py` and handed to the writing session as a
notice; the Zai case proposed the nearest common project as the lesson's home,
and that merge is the 10-12 rewrite seen in example 2. The two futon1b heap
notes are the ones that describe this morning's :7073 GC spiral. Claude Code
memories are per project and never see each other.

## 6. The conversation side: what was asked and never answered

The 象 records are a memory of the dialogue, not of files. The resume brief
reads them:

```text
## Asked by the operator, no answer linked (1)
- ask-action: @proof-scribe please produce the final frozen summary of t94A01 ... — 2026-10-14
## Facts (3)
- 2026-10-12 read-the-clock-before-logging-a-time: On 2026-10-12 it recurred in the M-象-2000 mission file ...
- 2026-10-12 zai-five-hour-window: ...
- 2026-10-11 minus40-memory-store: ...
```

The last section is example 1's store joined to the mission: the facts in
force now that mention M-象-2000, read from minus40 in one second. The
McCarthy checks (`xiang_acceptance.py`) then ask of the same records whether
every turn was answered, whether the past is well ordered, whether every
promise is accounted for, and whether each turn can be rewound to its
starting commits. Five of six pass for this session today.

## What none of this does yet (revised 2026-10-14T18:17Z)

The two halves now meet in three places: the brief's Facts section, each
fact's line naming the operator turn it was written under, and the brief's
closing of bell asks by their job's state. Still open: the open view does not
yet show a memory version beside its writing turn (the brief does); R19's
start-heads are not recorded; and the `:7073` evidence rows written before
18:14Z today carry the operator row one turn id behind its record (see the
lab note's addendum).
