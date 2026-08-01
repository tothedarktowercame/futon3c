# Cover note for Rob — reading V2 on the way to V3

**2026-08-01.** Accompanies `retrieval-whitepaper-v2.md`. Written by Joe with
the Claude team; the mapping onto the causal ladder in §3 is our reading,
offered for correction.

## 1. What this is and why we are sending it now

The attached paper (V2) is the second report on a deployed agent-memory
system: a service that supplies retrieved memories to LLM runners formalising
mathematics in Lean. V1 established the architecture and its trust boundaries;
V2 reports what happened when we measured the system's *behaviour* — four
preregistered experiments on a frozen corpus of 129 dispatches.

Three of the four preregistered expectations were wrong, and the paper treats
that as the result. Its organising claim is a self-application of construct
validity: every instrument we pointed at the system measured a property of a
thing, while every phenomenon we found was a relation between things — and our
own adjudication rubric then reproduced the exact defect we had diagnosed in
the system's instrumentation (§7).

We are sending it now, rather than after V3, because of what you are building.
Your port of "The Art of Why" into an agent harness looks to us like the
missing upper half of this programme — the "as above" to this "so below" — and
we would much rather have your input **on the way to V3** than your comments
on it afterwards. V3's experimental programme is derived but mostly unrun;
this is the point at which a causal model can still change what we do.

## 2. What V2 establishes, in one paragraph

Retrieval failure in this system is not explained by term rarity (the
empty-recall rate is a U-curve in document frequency, p = 0.618) or by term
co-occurrence (predicts strongly, p = 0.0172, with the *wrong sign*). A
spectral admissibility criterion is informative — the real graph sits ~15 SD
below a degree-preserving null — while its threshold inverts across a 59×
range in graph richness. Two-thirds of declined memories are declined as
simply off-topic; the discriminating reasons are a 12% tail. Of memories a
working solver used, 38% were adjudicated load-bearing — and five of seven
observed use-modes are *regulative* (changing what the runner does) rather
than *substitutive* (supplying content). The surviving conjecture, three
measurements converging and none testing it, is that the bottleneck is the
attachment layer rather than the lexical stage.

## 3. Why we think your system is the natural counterpart

Reading V2 with the ladder of causation in hand, the paper locates itself on
it rung by rung without ever using the vocabulary:

- **Rung 1 — association.** Everything V2 establishes is observational: the
  receipts corpus, the U-curve, the wrong-signed co-occurrence, the 38%.

- **Rung 2 — intervention.** V3's flagship experiment (E2) is a do-operation
  we have designed, reviewed through eleven passes, and deliberately not yet
  run: `do(withhold M)` — re-running solved problems from their pre-solution
  state with a specific memory ablated, against a same-corpus noise floor,
  under isolation designed as confounder-blocking (separate user account,
  git history truncated at the target revision, fresh session per run).

- **Rung 3 — counterfactual.** What "load-bearing" actually asserts is *this
  dispatch would not have closed without this memory* — a per-instance
  counterfactual. V2 states plainly that this is unrunnable and that every
  such verdict is a judgement of plausible causal contribution. In your
  terms: our adjudication rubric is an informal rung-3 judgement made with no
  structural model behind it, and the paper's honest hedging about the 38% is
  a rung confusion stated without the name. Rung 3 is reachable only with a
  fitted model — which is to say, only with something like your harness.

The connection runs deeper than the ladder. V2's thesis is Cronbach & Meehl's:
instrument and theory are validated together, inside a nomological net. V2's
instruments failed precisely because the theory layer was implicit — each
instrument inherited its categories from the system it was measuring. **An
explicit causal model is the nomological net this programme has been missing.**
If the constructs live in a stated model, V3's experiments become tests of
that model rather than free-floating measurements, and the
instrument-inherits-defects failure mode gets caught at design time instead
of in a post-hoc methods section.

## 4. What we can offer a causal harness

Most causal-agent work validates against toy environments. We have the
opposite and rarer thing:

1. **A deployed system with frozen, hashed observational data** — 129
   dispatches, byte-reproducible artifacts, corpus hash independently
   reproduced by three parties (V2 Appendix A).
2. **A panel of closed problems with known-achievable outcomes** — ground
   truth for intervention studies, reconstructible to any pre-solution
   revision from git history.
3. **A built intervention apparatus.** The `apmablate` isolation account
   exists, four problem trees are staged at pre-solution revisions, isolation
   probes pass, and the experiment's registration typechecks. It is parked
   behind one bounded refactor, not behind design work.
4. **A typed preregistration facility** (DarkTower) in which a model's
   predicted effect directions could be registered *before* the intervention
   runs. Three of our four preregistered mechanisms were wrong this week; a
   model that predicts E2's outcomes in advance would earn its credibility
   cheaply and publicly.

The trade, bluntly: you supply the "above" — the model layer; we supply a
non-toy "below" with a working do-operator.

## 5. What input would help most, on the way to V3

Four questions, in decreasing order of how much they would change what we do:

1. **Does your framework express our graph — and is it the right graph?** Our
   working DAG is roughly: memory → surfacing → runner trajectory → external
   witness (the Lean compiler), with the queue's blocking relations
   (`:blocked-on`, `:unblocks`) as causal edges *between goals*. We have
   never drawn this explicitly, which is itself a finding of writing this
   note. If you can express it, does the model change **what E2 identifies**
   — which confounders the isolation design actually blocks, and what the
   ablation licenses us to conclude — *before* we spend the ~1M runner
   tokens on it?

2. **Can the harness express route-relative effects?** Our sharpest
   structural finding (staging §G6) is that connectivity is not a property
   of an artifact but of an artifact *relative to a route*: the same lemma
   was unreachable under a Jensen-shaped attack for three attempts and was
   found first-try under a Hölder-shaped one. In causal terms the route is a
   moderator — memory→outcome is not a stable edge. Whether a ported
   Art-of-Why can express that kind of effect modification seems to us a
   real test of the port, not a detail.

3. **Does the regulative/substitutive distinction survive formalisation as
   an edge-target distinction?** A substitutive memory feeds the runner's
   *information*; a regulative one (a stopping rule, a caution, a route
   preference) acts on its *policy* — an intervention on the decision node
   rather than the information node. If that holds up in your formalism, it
   gives our proposed `:memory-use/kind` field a principled definition, and
   it *explains* our worst instrument failure: `used-ids` watches the
   information channel, and five of seven observed use-modes act on a node
   it is not pointed at.

4. **Authored or discovered structure?** If your agents fit structure from
   data, our rung-1 corpus is directly consumable as input. If structure is
   authored, then V3's job for you is the explicit DAG of question 1, and we
   should draw it properly rather than in prose.

## 6. Where things live

| document | path |
|---|---|
| V2 whitepaper (the paper) | `docs/retrieval-whitepaper-v2.md` |
| V1 (frozen predecessor) | `docs/retrieval-whitepaper.md` |
| V3 experimental programme — claims C1–C8, experiments E1–E7, sequencing | `holes/excursions/E-memory-v3-programme.md` |
| V3 staging bank — repairs, instruments, the ablation design (§H), conative structure (§G) | `holes/excursions/E-memory-v3-staging.md` |
| V2 programme (what was frozen, and the gate that held repairs) | `holes/excursions/E-memory-whitepaper-v2-programme.md` |

Suggested reading order for a first pass: this note → V2 abstract and §1 →
V2 §7 (the methodological findings, which are where the paper's contribution
sits) → the claims table in the programme doc (§2) → as deep into the staging
bank as appetite allows.

## 7. The invitation

V3 is the version we want to be *right*, and it is the version your input can
still shape. Comments at any grain are welcome — from "your DAG is wrong,
here is mine" down to marginalia — but the four questions in §5 are where a
week of your attention would move a month of ours.
