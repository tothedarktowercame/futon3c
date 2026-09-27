# E-象-2000-wm-seam: problem statement to War Machine wants

Opened 2026-09-27 by claude-17 at Joe's direction. It follows M-象-2000's
INSTANTIATE and is not part of the first pass. Joe (~16:02Z, via claude-8): the
front end that turns a problem statement into wants "can be owned by M-象-2000
for now, we might need an adapter or 'seam' for the WM, but certainly it is my
aim that the 2 projects meet in the middle."

## What the War Machine accepts at the seam (claude-8, 2026-09-27)

Record on the WM side: futon2 `holes/labs/wm-contract/PROOF-2a-PLAN.md`, LOG
under <1>3 (HINTERP-GRAIN-D, -D2, -D3, OUTCOME-WANTS-I, LIFECYCLE-EXITS-D).

| | requirement | WM code site (futon2) |
|---|---|---|
| R1 | A want is a token with a positively stated, authored text, never the negation of a problem ("the problem no longer holds" is how you build "kill all humans"). The text must be stable when the compiler reruns on unchanged input; a reworded statement is a new token and orphans its check. | `src/futon2/aif/outcome_wants.clj` (ae2e12dfb) |
| R2 | Every want is mechanically observable: a C3/C4/C5/C6/C8 locator. C8 (a registered passing run of a test namespace or command) carries absence and compound checks. A want with no admitted locator waits as `:no-admitted-locator` and is never dropped. | `cascade_problems.clj:81-85`, `:192-196`; `mission_reading.clj:53-62` |
| R3 | Provenance by quotation: the problem statement quoted with its span, the classification steps, and the pattern ids reached, as data on the want's criterion. | `mission_reading.clj:173-207` (validate-locator) |
| R4 | `:role :why` (primary: the outcome) or `:how` (secondary: the lifecycle's eight phase exits, which LIFECYCLE-EXITS-I supplies; the front end need not). | |
| R5 | Patterns used for construction carry `:produces #{…}` and `:guard {:needs #{…} :forbids #{…}}`; A sits above B when A produces what B's guard needs. Library flexiargs do not carry these; deriving them is the adapter's main open job. | `construction.clj` containment order (~:125-170) |
| R6 | Excursions and other non-lifecycle targets get no secondary wants. | |

The WM does not need a verdict on whether an outcome holds (each click's
observation gives that), weights, or any gate before action.

Joe's framing: outcome statements are the primary wants (the why), lifecycle
items the secondary (the how). This matches the library's `@why` (causal, backwards)
and `@how` (pragmatic, forwards) edges. The pattern cascade acts as a compiler from a
problem statement to a proof plan, narrowing step by step ("an Analysis problem
… a Complex analysis problem …"). The WM's per-target cascade is the small
modular cascade at the edge of the giant connected one.

## What M-象-2000 already has toward it

- **R5 for the patterns a mission cascade covers:** `cascade-象2000-v2.edn` gives
  33 patterns in exactly the WM's shape, and `cascade_check`/`wiring_check` pass.
  The open part is flexiargs no cascade has covered yet.
- **R1/R4:** the mission's wants are positive, and its exit criteria, with
  inline verdicts, read correctly through futon2 `mission-criteria/criteria`
  (8 criteria).
- **R3's carriers:** minted act ids (P6b) and the `@why`/`@how` edges
  (`mined_pattern_graph.py`, futon3c 31e51877; 象 `@how` in futon3 aa671d8).

## Open questions, answered provisionally (claude-17)

- **Q1: who supplies each classification step for an unattended flight?**
  Retrieval proposes; 象 interprets, and the interpretation has a version and can
  be refuted; the classification takes effect only when an authorised party
  confirms it (象/释义非授). That party is the operator at mission-writing time,
  or a standing grant once P3 exists. Until then, an unattended flight uses only
  confirmed classifications, and the rest wait.
- **Q2: is a first seam of "statement in, ranked pattern ids with their @why chain
  out" an existing packet?** No, it is new, but mostly wiring: `xlate.py find`
  ranking plus a walk of the authored `why`/`how` edges, with the output pinned
  to the library commit and index hash (R1) and quoting the statement's span
  (R3). It sits next to P7b.

## Candidate packets (after INSTANTIATE; none dispatched)

1. **S1 ranked patterns with chains:** statement in; ranked pattern ids, each with
   its `@why`/`@how` chain, the quoted span, and the pinned library commit out.
   Acceptance: running it twice on unchanged input gives byte-identical output.
2. **S2 criteria as wants:** emit M-象-2000's completion criteria as
   `{:stated … :role :why}` with a C8 locator each (for example P0 as the
   command), and read them back through futon2 `outcome_wants.clj`.
3. **S3 adapter from flexiarg to cascade shape:** propose `:produces`/`:needs`
   for a flexiarg the cascade does not cover, as an interpretation (象/释义非授)
   that is confirmed before the constructor uses it.

## Found during INSTANTIATE (2026-09-27)

P0 (`scripts/xiang2000_p0.py --check`) is the natural C8 locator for M-象-2000's first
completion criterion, but it cannot be registered yet: the test registry's command
validator accepts Clojure test namespaces and Lean builds only (codex-4), while the
WM's C8 description names bb and sh gates as well (futon2 `mission_reading.clj:53-62`).
Either the validator widens to the commands the WM already describes, or P0 gets a bb
wrapper. A small first task for this excursion.
