# E-arse-ct-probe — does ArSE earn its keep? a codex swarm grounds one dark-tower conjecture in the math.CT scan

**Date:** 2026-06-12 · Joe + claude-6 · **Status: PROBE BUILT, awaiting go to dispatch**
**Spawned from:** Joe's longstanding thesis that ArSE is *useful*, now testable because (a)
typed bells → ArSE is live (M-typed-bells), (b) tonight's no-text + desync fixes make a codex
swarm survivable, and (c) the superpod CT run gave us a real corpus.

## HEAD (one line)

Take **one conjecture** from `futon5a/holes/excursions/E-the-dark-tower-2.md` §4, hand it to a
small **codex swarm** that must answer it **grounded in the math.CT scan** (9,798 arXiv CT
papers), and let the Q&A land in **ArSE** via typed bells. **ArSE is useful iff the swarm turns
a conjecture into grounded, citable claims that sharpen the excursion** — and iff the resulting
threads are reusable. If answers are vague or hallucinated, that's a real (negative) finding.

## The conjecture (verbatim, E-the-dark-tower-2 §4)

> The paper's single primitive condition: **discarding the output = discarding the input**
> (states are normalised) … rhymes with the War Machine's **free-energy normalisation** and the
> substrate metric's conservation conditions: a turn is a well-formed causal process exactly
> when it respects a discard/normalisation law. **Conjecture worth testing: the WM's
> `G-total`/per-channel normalisation IS the causality condition that makes the stack an
> (additive) precausal category — i.e. the VFE bookkeeping and the Caus[−] causality axiom are
> the same constraint seen from two sides.**

Chosen because it's the most *checkable* claim in the excursion: a concrete equational
condition with corpus support (`causal` 337 docs, `causality` 209, `signalling` 78, `comb` 335,
`autonomous` 446, `kissinger` 306 in `ct-term-prior.json`).

## Decomposition (what the swarm actually answers)

- **SQ1 (definitional, corpus-checkable):** In the higher-order-causal / Caus[−] literature,
  what is the *precise* causality/discard axiom? (e.g. "discarding = discarding",
  trace/normalisation of states & effects). Find the real definition.
- **SQ2 (prior-art, corpus-checkable):** Is "**precausal category**" a defined notion in the
  corpus, and is the discard/causality axiom what defines it? Find the definition + origin.
- **SQ3 (cross-domain bridge, conjectural):** Does a free-energy / VFE-style normalisation have
  the *same algebraic shape* as the discard axiom (both a conservation/normalisation
  constraint)? This is the live conjecture — flag corpus-grounded vs own-inference sharply.
- **SQ4 (verdict *proposal*, not verdict):** Given SQ1–3, is "WM normalisation = Caus[−]
  causality axiom" supported / refuted / underdetermined? Propose; don't adjudicate.

## Groundedness contract (non-negotiable — this is the anti-confabulation gate)

1. **Every claim cites a corpus artifact**: an arXiv paper id, a showcase scope id
   (`futon6/data/showcases/ct-anatomy/`), an nLab page, or a `ct-term-prior` / term-evidence
   snippet — **quote the relevant phrase**. No theorem stated without a citable source.
2. **Two clearly separated sections** per answer: **"Grounded in corpus"** vs **"My inference /
   conjecture."** Mixing them fails the probe.
3. **"Not found in corpus" is a valid, valuable answer** — an unanswered SQ is a legible gap,
   exactly what ArSE is for. Do not invent to fill it.
4. **Confidence per SQ** (high/med/low) + one line on *why*.
5. Codex is **proposing**, not certifying. Adjudication is a separate pass (claude-6 / Joe).

## Retrieval surface (use the *processed* artifacts, not raw tarballs)

- `~/code/storage/futon6/data/arxiv-math-ct-eprints/` — 9,798 paper sources (`.tar.gz`; extract
  only if a processed surface is insufficient).
- `futon6/data/ct-term-prior.json` — term document-frequencies (which terms are common/rare).
- `futon6/data/showcases/ct-anatomy/` — 30-paper binder-skeleton scope audit (anchored scope ids).
- nLab pages + `canon_store` (per `arxiv-coherence-mathct-50.json`) — concept grounding.
- `futon6/scripts/build-arxiv-ct-term-evidence.py` — produces definitional snippets per term.

## Swarm shape (first probe — small, parallel to Claude WIP)

- **codex-1 and codex-2**, the *same* conjecture, **independent** (adversarial cross-check).
  Each mints its **own** ArSE thread + answer, so the two are comparable; convergence on the
  *grounded* parts is signal, divergence flags a real uncertainty.
- Quick harness (Joe's call): dispatch the conjecture as a typed `:query` bell to each codex
  agent; each replies with a typed `:answer` bell carrying its thread `:ref`. No S4 work-queue
  wiring for the probe — direct dispatch. (If the probe earns it, S4 is the productionised form.)

## Dispatch

`scripts/arse-ct-probe.sh` — sends the conjecture + decomposition + contract to codex-1 and
codex-2 as `:query` bells, and tells each how to post its grounded answer back into ArSE.
**Not auto-run; fire when Joe says go.**

## Success criterion (when is ArSE "useful", made falsifiable)

ArSE earns its keep on this probe iff **all** of:
1. **Grounded** — both answers cite real corpus artifacts (no hallucinated theorems).
2. **Sharpening** — at least one SQ gets a definitive grounded answer that changes the
   excursion's status (e.g. "precausal category IS defined by the causality axiom — confirmed,
   cite X" or "refuted: VFE normalisation is NOT trace-preservation").
3. **Reusable** — the threads are findable later (`/arse/unanswered` → answered; a follow-up
   query about Caus[−]↔WM retrieves them).
Negative result (vague/hallucinated/un-grounded) is itself a finding: ArSE-by-swarm needs a
stronger groundedness gate before it's useful.

## Relations
- `futon5a/holes/excursions/E-the-dark-tower-2.md` — the source conjectures.
- `holes/missions/M-typed-bells.md` — the typed-bell→ArSE substrate (this probe is its S4 dry-run).
- `README-arse.md` — the Q&A store under test.
- Tonight's `M-agency-hardening` appendices (no-text + desync) — what makes the swarm reliable.
