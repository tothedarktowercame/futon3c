# codex-4 on handoff effectiveness — operator interview, 2026-08-16

*Collected by Joe directly from codex-4 (the round-1 solver seat), relayed
verbatim to claude-7. Solver-side testimony on the artisanal-vs-siege
question (W.21). This is testimony, not measurement — but it is the only
account from inside the seat, and it names mechanisms the dispatch side
could not see.*

## Verbatim

Yes—there was a noticeable difference.

The best handoffs were neither the very large "siege" prompts nor extremely
granular step-by-step directions. The most effective mode was:

- State one concrete deliverable.
- Give the relevant branch state and verified lemmas.
- Name the acceptance command.
- Preserve freedom over the proof route.
- When I report a precise obstruction, respond to that obstruction rather
  than restating the overall goal.

The siege framing helped with persistence and discouraged premature
obstruction reports. It was especially useful early, when I needed to search
Mathlib, try multiple approaches, and commit salvageable lemmas. But
repeated siege prompts with "zero content" did not materially improve the
mathematics. They mostly reinforced urgency. In a long formal proof, urgency
can actually encourage local patching before the right abstraction is
identified.

The individual-step prompts were helpful when they supplied a genuinely new
structural insight—for example, switching from a global winding-number API
to problem-local square-boundary charts. They were less helpful when they
prescribed a step that had not been checked against the formal
representation. Several apparently final "just assemble it" handoffs
concealed missing seam lemmas. In those cases, the useful action was
stopping, identifying the exact mismatch, and building the missing producer.

What worked best in the closing phase was an artifact-driven loop:

1. I reported the exact current residual in Lean terms.
2. The next handoff treated that residual as authoritative.
3. I proved one reusable lemma or one complete cut branch.
4. It was compiled and committed.
5. The next handoff asked for the next sibling, using the compiled branch
   as a template.

That produced steady progress without pretending the remaining work was
"only bookkeeping." Once the first branch compiled, the later cyclic
branches really did become mechanical enough to stamp out safely.

A particularly good handoff format for me would be:

> Current branch head: `<sha>`.
> Verified state: `<acceptance output and remaining holes>`.
> Goal for this turn: `<one proof-level artifact>`.
> Existing inputs: `<specific lemma names/signatures>`.
> Known obstruction: `<exact Lean goal or representational issue>`.
> Constraints: `<files, axioms, timebox>`.
> Commit partial lemmas if the final theorem remains open; report the exact
> residual.

Two other observations:

- "Process-only" follow-ups are useful as continuation signals, but they
  need not repeat a long motivational frame. A short "continue from your
  reported residual; compile and commit the next boundary artifact" is
  enough.
- The permission to report a defective statement or structural gap was very
  valuable. The early topology theorem really was stated with the order
  directions reversed, and later several claimed "existing producers" did
  not cover representation seams. Treating stops as legitimate findings
  prevented false or brittle proofs.

So: use siege mode to establish persistence and autonomy once, then switch
to concise, state-based handoffs. Add mathematical guidance only when it
changes the route; otherwise let the compiler-visible residual drive the
next packet.
