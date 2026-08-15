# TN — Two machines, one capability

**Written** 2026-08-15 by claude-2, prompted by Joe: *"the 'not implemented 12'
put me in mind of our capability-proof-apm.tex — we made claims about the APM
capability, but only now are 'discovering' the tools that would make that
capability possible."*

The observation is right, but the diagnosis is not the obvious one. The
capability proof's warrants are not hollow. They belong to **a machine that
exists and is running right now** — just not the machine we spent today building.

## The two machines

| | **apm-driver** | **problem peripheral** |
|---|---|---|
| where | `holes/labs/M-diagramprover/apm-driver/` | `src/futon3c/peripheral/problem.clj` |
| what | Python, 8,571 lines | Clojure, ~900 lines |
| state | **running** (2 processes; `axiom-audit.jsonl` written 16:55 today) | 8 of 20 phase tools implemented |
| witnesses | 2 executed chains (Dirichlet, Steinhaus); 213 problems audited axiom-clean | 0 |

`capability-proof-apm.tex` says the proof object *is* the pipeline. The pipeline
it means is the driver. N1–N4 and N9 are warranted by executed chains through the
driver, and the Steinhaus closure is real: build clean, zero `sorry`s, axioms
exactly `[propext, Classical.choice, Quot.sound]`, fidelity adjudicated. Nothing
in today's census touches those warrants.

## The finding: the peripheral's missing 12 are the driver's implemented core

Every unimplemented peripheral tool has a working counterpart in the driver:

| peripheral tool (mock-answered) | driver implementation |
|---|---|
| `:emit-frame` (scaffold/closing hash) | `gates.py:233 statement_hash`, `242 declaration_hashes`, `271 declaration_set_drift` |
| containment / boundary | `gates.py:310 boundary_conformance` |
| `:validate-registration`, axiom cleanliness | `gates.py:493 run_axiom_probe`, `460 impure_axioms` |
| residual sorries | `gates.py:128 sorry_sites`, `135 count_sorries` |
| `:write-disposition` (terminal outcome) | `gates.py:527 _classify` |
| `:write-use`, ledger of transitions | `driver.py:467 append_transition`, `433 fold_ledger` |
| `:promote-artifact` | `promotion-queue.jsonl`, `scribe.md` |

This is not a coincidence of naming. The peripheral's `:frame` and `:adjudicate`
phases are **a second implementation, in a second language, of the driver's gate
and classify stages** — and it is the half that was never built.

## What is genuinely NOT duplicated

The peripheral is not pure redundancy. It adds the thing the driver has no
notion of: **the preregistered experiment.** Store-mode versus harness-mode,
guidance counting against a pinned solver seat, cold student attempts, the F1–F9
runtime invariants, the frozen registration and its validator. The driver closes
problems; the peripheral is supposed to *measure whether memory transfer helps*
while problems are closed.

So the split is clean, and it is not the split we have been building to:

- **the driver already does the closing** — and has warrants for it;
- **the peripheral should do the measuring** — and only the measuring.

## Consequence for frame-1

The twelve unimplemented tools should mostly not be *implemented*. They should be
**wired to the driver**. This is I-4 in `CLAUDE.md`, which we walked past:

> If the answer is "this already exists in a script," the task is wiring, not
> writing.

Two hours ago I committed `apm-driver/axiom-audit.jsonl` to get futon3c to read
clean. That file is the running machine's output. I treated the evidence of the
working system as dirt, on the same day I built a parallel copy of it.

**Open question for the operator, not for me:** does frame-1 run through the
peripheral (which needs twelve tools it should not own), or through the driver
with the peripheral supplying only the measurement apparatus? The second is much
less work and starts from two witnessed chains instead of zero.
