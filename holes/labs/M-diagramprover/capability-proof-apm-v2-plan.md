# capability-proof-apm v2 — revision plan

**Written 2026-08-14 by claude-2**, at Joe's request that the capability proof
be revised "to make it more formal / checkable", now that the precursor system
is in place. The EF packet (`E-apm-halftime-pre-go-live-EF.md`) is the
*informal* description of the system; this is the plan for making the paper
state it formally.

---

## 1. The critique, stated precisely

Joe: *"the previous capability-proof just amounts to 'if Codex is good at math
it can solve these problems' which isn't very interesting."*

That is unfair to v1 as written and exactly right about what v1 **certifies**.
v1 does not claim solve-rate-as-capability: it reads the claim constructively,
decomposes it into nine contracted sub-claims, grades each warrant, states
transport as a selection diagram rather than an induction, and withdrew the
a96A08 use-witness the same day an executed check found 12 errors. That
machinery is sound and should be kept.

But sort v1's own warrant table by whether the node is certified:

| certified | uncertified |
|---|---|
| N1 extra resources fill Mathlib holes — inductive n=3 | **N5 retrieval serves the need — weak** |
| N2 work transports between agents — inductive n=4 | **N6 capability transports to held-out — designed** |
| N3 the store records learning — inductive n=4 | **N7 outcomes mechanically scoreable — designed** |
| N4 agents consult when instructed — inductive n=1 (controlled) | **N8 the process learns at ability level — designed** |
| N9 the pipeline runs continuously — inductive n=2 | |

**The certified column is "a relay exists, runs, and hands work between
agents." The uncertified column is the entire thesis.** Joe's complaint is
therefore structurally true even though the document is honest: honesty about
which nodes are weak is not the same as saying which nodes the claim *rests
on*.

### 1.1 The single most important structural change

v1 presents N1–N9 as a flat list. **v2 must rank them by load-bearing status**,
so the reader sees immediately that the four uncertified nodes are the thesis
and the five certified ones are scaffolding. Concretely: add a
**`depends-on`** column, or split the table into *Thesis nodes* and *Enabling
nodes*. A capability proof whose certified nodes are all enabling nodes should
say so in its own table rather than leave it to be discovered by sorting.

This converts the criticism into a feature: the document already refuses to
launder correlation into mechanism; it should equally refuse to let scaffolding
read as thesis.

---

## 2. What each uncertified node needs to become checkable

The EF reframe supplies the missing definitions. **N7 is the keystone** — none
of the others can be certified while outcomes are not scoreable.

### N7 — outcomes mechanically scoreable → give it an operational definition

v1 says "delta-form endpoint, executed-witness-only scoring" but never defines
the endpoint. Define it as the **per-round measurement vector**, every quantity
drawn from an artifact the loop already produces:

| role | measurable | direction |
|---|---|---|
| Formalizer | statement defects found at review | ↓ |
| Reviewer | **escape rate** — defects the prover hits that review missed | ↓ |
| Freeze | **contract leaks** — post-freeze changes to `def` bodies | ↓ |
| Prover | outcome ∈ {closed, TierA, TierB, defective}; attempts; residual sorries | ↓ |
| Scribe | memories promoted; **hunger audit — queries returning empty** | ↓ |
| Scribe (join) | promoted memories later surfaced *and used* on a later problem | ↑ |

and the scalar

```
L(i) = cost_to_close(i) + residual(i) + rework(i)
```

Warrant becomes **mechanical** the moment one round emits a complete vector.
That is a far cheaper certificate than v1's implied one, and it is dry-runnable
on the canned CT problem.

### N8 — the process learns → state it as a slope, not a property

"The process learns at the ability level" is not checkable as written. Checkable
form:

> **N8.** `dL/d(problems seen) < 0`, conditioned on memory being available.

**The confound must be in the statement, not a footnote:** `L(i)` is comparable
across problems only if difficulty is comparable, so a falling `L` is otherwise
indistinguishable from an easier tail of the corpus. The stratification or
matching must be **pre-declared** (F2). This is exactly the shape v1 already
uses for N6 — a claim that is identified by design or refused — applied to
learning.

### N6 — transport → discharge the obligation v1 already names

v1 states N6 correctly as a transport claim with selection variable `S`
switching APM vs held-out BPM, and calls it "a derivation obligation, not an
extrapolation." v2 should **carry out the derivation** (or record that the
causal engine refuses it), because an obligation that is never discharged is
indistinguishable from an aspiration.

⚠ **CORRECTION 2026-08-14 (MAP addendum 2; ams-codex-2 audit `5de70dbd`,
verified by claude-2).** The parenthetical above — "or record that the causal
engine refuses it" — **is not currently available**, and v2 must not plan
around it.

- **v1's terminology is correct.** "Transport" at line 278 is Bareinboim–Pearl
  transportability, properly used: *"'solves APM ⇒ capable on held-out BPM' is
  formally a transport claim, not an induction."* No terminology collision with
  the literature.
- **v1's engine description is false.** Line 275 lists the engine as
  "backdoor and front-door adjustment, general identification, **transportability
  via selection diagrams**". The first three exist (`identify.clj`, `idalg.clj`);
  **the fourth is not implemented.** `identify` takes `[causal-dag treatment
  outcome]` — one domain. There is no S-node, no source/target domain input, no
  transport entrypoint anywhere in `causal/`.
- **The likely cause is a collision inside the engine's own vocabulary.**
  `receipts.clj` has `r1-selection-variant` and the refusal
  `:open-selection-backdoors`, but that is selection **bias** in one domain, not
  a selection **diagram** across two. Same word, different concept.

**v2 must therefore either (a) delete "transportability via selection diagrams"
from line 275, or (b) implement it.** Shipping v2 with that phrase intact would
be a capability proof overstating its own instrument — the exact failure the
document exists to prevent, and the fifth instance of this mission's
documentation-drift pattern.

If (b): the extension is bounded, not a rewrite. `sID`/`sTR` builds on
`admg/latent-project`, `idalg/identify-effect`, `dsep`, and `surgery`, all
present. What is missing is S-node semantics, source/target domain inputs,
per-domain observational availability, and mechanism-invariance declarations.

### N5 — retrieval serves the need → run the instrument that exists

The **hunger audit** is already specified in the scribe template and is the
closest thing in the system to a gradient signal on retrieval. It has never
been run at campaign scale — largely because `ams-scribe-1` was repurposed as a
bridge-lane seat and role 5 was *staffed but not performed*. Running it is a
measurement, not a build.

---

## 3. Three preconditions, promoted to formal conditions

v1 discusses the failed assay in prose. v2 should state the invalidators as
**conditions a round must satisfy to count**, because all three are silent —
a round can fail any of them and still look like a clean measurement.

1. **Headroom.** The control arm must not already succeed. Assay 1 died here:
   the control found the full route unaided, so the goal had no headroom for
   the candidate techniques.
2. **Arm independence.** Separate sessions, demonstrated. Assay 1 died here
   too: the treatment arm saw the control run — one seat, one session across
   the queue.
3. **Elicitation verified.** *(New — not in v1, and not caught by 1 or 2.)*
   E9/E10 is a preregistered controlled contrast, same agent and same store:
   **0 store lookups under an invitation frame vs 21 under a two-part task
   frame.** So task framing silently determines whether the treatment arm
   consults memory at all. If it never looks, the round measures nothing while
   appearing valid.

⚠ **CORRECTION 2026-08-14 (MAP Track D, codex-4; verified by claude-2).**
An earlier version of this section stated that `scripts/batch2r_pair.sh`
"dispatches both arms with verified-fresh sessions and is the mechanism for
(2)". **That was read off the script's comment, not its code.** Reading the
57 lines:

- what it DOES do: dispatches the two arms to **different seats**
  (`mem` → `ams-codex-2`, `ctl` → `ams-codex-1`) in **separate frames and
  checkouts**. That genuinely addresses Assay 1's failure mode, which was
  *one seat, one session across the queue*.
- what it does NOT do: **verify that either session is fresh.** The phrase
  "verified-fresh sessions" occurs exactly once in the file — on line 2, in a
  comment. There is no check, no `--new-session`, nothing.

So precondition (2) is **structurally attempted but unenforced**. If a seat
carries a session across dispatches, contamination recurs silently — and the
runner will still report success.

This is the guiding light again, and claude-2 propagated it: a claim was read
from a comment and repeated as a mechanism, in this document and to the
operator, without reading the code beneath it.

---

## 4. What the infrastructure now supplies that v1 could not assume

Joe: *"the proof didn't assume any of what we have just got sorted."* The
following are new and should be cited as enabling conditions, with hashes:

- **Experiment stages pin to hashes.** futon3c, futon6 and futon1b are at
  inbox zero and even with origin, so a run can name the exact tree it ran
  against. Before today the working trees held uncommitted state — including,
  in futon6, *stale pre-08-07 copies sitting on top of committed corrections*
  that would have silently reverted a gate test, a status assertion and a
  documented calibration fix.
- **The evidence corpus is reachable and verifiable.** 154 files under
  `data/evidence/`, `sha256sum -c` against the manifest passing 154/154, and
  `backup_evidence.sh` as the sync basis. Before today the manifest was
  committed while 152 of its 155 rows existed only on one host's disk.
- **The substrate can be measured.** A3 is closed: `pattern/library` 1288 rows
  == 1288 distinct names, `pattern/clause` 9668 == 9668, zero non-qualified
  ids, zero non-qualified relation endpoints. An entity census is now a
  measurement rather than an archaeology exercise.
- **The identity convention is written down**, so conformance is a one-line
  audit rather than a discovery.

---

## 5. Corrections v2 must make

- **The odometer is stale and its gauge reads low.** v1 says ~27% closed.
  `status.json` now gives **186 of 475 at zero sorries = 39.2%** — and E5
  established that `status.json` *understates* closure (`a94A09` reports 1
  sorry for a problem with a closing commit, `a266157`). So the true figure is
  ≥39.2%, and **any status-derived percentage needs a comment-aware sorry
  detector before it is quoted**. A capability proof should not carry an
  odometer it knows is miscalibrated.
- **The evidence base is larger than v1 knew.** A Dionysus sweep (oxf-codex-1)
  plus claude-3's two-corpus analysis surfaced material absent from the tree v1
  was written against — E9/E10 elicitation, the ConstructionTargets natural
  experiment (a92J05: stall 18 July → lemma created 29 July → closed 1 August),
  and the 154-file frozen corpus with its 17/45 load-bearing adjudication.
- **State the one effect-size datum, with its limit.** `TN-raw-ctl-reanalysis`
  gives corrected, paper-matched, finals-only figures: enriched 3/25 = 12.0%,
  raw 4/26 = 15.4%, over 51 inference edges. **This is not a directional
  prior** — the entire difference is *one resolved warrant edge*, and the
  document says so itself. It is a **sizing lesson**: the 16-paper corpus
  already yields 419 inference edges and 383 warrants, ~16× the evidence per
  arm, via a bounded local run reproducible with `scripts/rawctl2.py`. That is
  the costed path to an effect-size estimate.
- **Freeze scope is a finding.** The 2026-08-01 freeze excluded
  `capability-proof-store.md` — the cleanest controlled mechanism result in the
  directory it was drawn from. How the next corpus is scoped belongs in the
  pre-registration.

---

## 6. What v2 is *not*

Not a rewrite. The constructive framing, the warrant classes, the
author≠reviewer certificate discipline, the causal layer and the refusal
taxonomy are the document's contribution and should survive intact. The change
is: **rank the nodes by load-bearing status, give the four thesis nodes
operational definitions, promote the three invalidators to conditions, and fix
the odometer.**

## 7. Suggested order

1. **§1.1 the ranked table** — cheapest, and it reframes everything after it.
2. **N7's operational definition** — unblocks N8 and N5.
3. **The three preconditions** as a numbered subsection.
4. **The corrections** in §5, especially the odometer.
5. **N6's derivation** — largest, and it can trail the others.

Steps 1–4 are prose against facts already established. Step 5 needs the causal
engine and is the only one that could fail on its own terms — which is the
right property for the last step of a capability proof to have.

---

# 8. Squaring with the V3 memory whitepaper

Joe: *"we should square with the V3 memory whitepaper, b/c that describes
implementation level details for storage and retrieval (which are crucial for
N8, i.e. 'conditioned on memory availability')."*

Reading `futon3c/docs/retrieval-whitepaper-v3.md` changes one conclusion in
§2 above and supplies the missing definition for another.

## 8.1 N8 already has a witness — the capability proof does not know it

v1 grades **N8 "the process learns at the ability level"** as `designed`,
"lessons travel via versioned packet templates; automation of template deltas
not yet built."

But V3 §6 records a **complete demand → build → closure revolution**, dated
2026-08-10, in ~90 minutes:

> three votes for Schwarz–Pick rigidity → `ConstructionTargets.SchwarzPick`
> (16 axiom-clean declarations, `10eac91b`, target theorem stated by its
> voter) → callback → **one-pass closure of a theorem that had survived three
> prior closer hops** (`a266157d`, merged `087924c`).

That is the ability level: the system identified what it could not do, built
the missing capability, and closed a problem that had defeated three previous
attempts. It is witnessed, hash-carrying, and n=1.

**So N8 should be `inductive-n=1` with the vote ledger as certificate, not
`designed`.** The two documents were not citing each other, which is itself an
instance of the guiding principle below — a witness existed and the document
that needed it could not see it.

*Note the same commit `a266157` is the one E5 found `status.json` contradicting
(it reports 1 sorry for that closed problem). The strongest single learning
witness in the programme sits behind a status file that reads it as open.*

## 8.2 "Conditioned on memory availability" must name a retrieval regime

This is the substantive correction. In §2 above I wrote N8 as

> `dL/d(problems seen) < 0`, conditioned on memory being available.

V3 §4 shows "available" is not a boolean. Availability is a **mechanism with a
measured defect and a shipped repair still under live test**:

- **The defect (§4.1).** Anchor-term selection ranked candidates by rarity in
  the *problem* corpus, which selects artifact vocabulary and **inverts
  relevance**: a01A12's own memory surfaced for the wrong problem and failed
  to surface for its own, whose runner re-derived the content and confirmed on
  interview that it would have accelerated the pass.
- **The repair (§4.3).** Anchors drawn from the pre-cap term pool, filtered to
  a memory-corpus document-frequency band, ranked `[problem-IDF, memory-df]`
  (`6521fd3a`). Offline: the a01A12 miss reverses, negative control clean,
  default path byte-identical. **Status: under live test as batch-2.** Three
  plausible ranking rules failed first, recorded as the argument for
  preregistration.

**Consequence for v2: `L(i)` is not comparable across a retrieval-regime
change.** A falling `L` measured across the `6521fd3a` boundary confounds
"the process learns" with "we fixed the index". So N8 must read:

> `dL/d(problems seen) < 0` **within a fixed retrieval regime**, the regime
> named by commit hash, with any regime change treated as a stratum boundary.

This is precisely the pinning that today's inbox-zero work makes possible, and
it is a second, independent reason the hash-pinning matters — not just
reproducibility, but **identification**.

## 8.3 The users' requirements are the retrieval contract N5 owes

V3 §4.2 is the most reusable thing in either document, and the capability
proof does not mention it. Exit interviews across twelve dispatches give a
consistent search model:

> runners find reusable work by **engine names** (grep for the Mathlib
> declarations they are about to use) and by **structural similarity** —
> **never by concept vocabulary**, which is what the anchor mechanism
> searched.

So N5 ("retrieval serves the need") is not weak for want of tuning. It was
weak because **the index was keyed on the one vocabulary its users never
search by.** v2 should state N5's contract in the users' terms — retrieval is
adequate when it answers engine-name and structural-similarity queries — and
grade it against that, not against a generic relevance notion.

§7 supplies the scale this has to work at: **6,114 unnamed `have`-steps vs
2,139 named lemmas, ~3:1**, with signature clustering finding statement shapes
and text embeddings finding route twins (0.77 cosine on a hand-derived step
matching its two prior occurrences). Proof-term shapes remain the open rung.

---

# 9. The guiding light, reversed

Joe's framing, from the Hitchhiker's Guide:

> *"It was on display in the bottom of a locked filing cabinet stuck in a
> disused lavatory with a sign on the door saying 'Beware of the Leopard.'"*

**Reversed, this is the design rule for both documents:** an artifact counts as
available only when the person or agent who needs it can find it, by the
vocabulary they actually use, at the moment they need it. Technically-present
is not available.

It is not a metaphor here. Every failure repaired on 2026-08-14 was this
failure, and so is the retrieval defect V3 diagnoses:

| the notice | the cellar it was in |
|---|---|
| the 154-file evidence corpus | manifest committed; 152 of 155 rows on one host's disk |
| `turn_queue.clj` operator-hold | caller shipped on master, definition uncommitted on one box |
| its test | written, passing, never committed |
| the retrieval whitepapers | cited three times by a committed document, untracked |
| `rawctl.py` | successor tracked, original not — half an analysis |
| `batch2r_pair.sh` (the session-contamination fix) | on disk, never committed, so no other host could run a clean pair |
| the hunger audit | specified in the scribe template, never run at scale |
| the scribe role itself | staffed, then repurposed as a bridge-lane seat |
| **a01A12's memory (V3 §4.1)** | **in the store, indexed by a vocabulary its own runner would never search** |

The last row is the same defect as the others, one level in: the memory was
not missing, it was **unfindable by its intended user**. That is why N5 is the
binding constraint and why "availability" needs a mechanism, not a flag.

**Both documents should adopt this as an explicit acceptance criterion.** A
warrant may not upgrade on the existence of an artifact — only on a
demonstration that its consumer can reach it. Concretely:

- a memory counts as available when a **query in the consumer's own vocabulary
  returns it** (engine name or structural similarity), not when it exists in
  the store;
- evidence counts as distributed when a **sha-verified sync has placed it**
  where the reader is, not when a manifest names it;
- an implementation counts as shipped when a **clean checkout compiles it**,
  not when it runs on the author's box.

Each of those is mechanically checkable, which is what makes this a warrant
class rather than a slogan. The hunger audit is already the instrument for the
first: **a query that returns nothing is a Leopard sign, recorded.**
