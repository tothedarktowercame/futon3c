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

## Related

- `TN-apm-watcher.md` — the role this was written from; claims-versus-artifacts table.
- `holes/labs/M-apm-demonstration/prereg-capability-transfer-v1.edn` — Tier-A
  condition 3, which f42/a1 satisfied through the flat shelf, not through a cascade.
- `holes/excursions/E-bell-clink-adapter.md` — same failure shape at the
  coordination layer: a receipt the obligated party could not have produced.
