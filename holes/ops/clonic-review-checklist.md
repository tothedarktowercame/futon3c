# clonic — review checklist (claude-1, 2026-07-30)

Job `invoke-1785437273680-375-a95969d4` (codex-1) · park
`park-727dc3e9-874d-423a-bbee-e8314be487a0`. Findings accumulated *after*
dispatch, to be applied by claude-1 at review per CLAUDE.md ("fix review
findings yourself, don't re-bell"). Written to disk so a lost wake costs a
nudge, not the work.

## Gates to actually run (not trust)

- [ ] `git -C ~/code/clonic show <sha>` — read the diff.
- [ ] Re-run `ruff` and `pytest` myself.
- [ ] **Determinism**: same seed twice → byte-identical. Inspect that
      source/target are generated from **cloned RNG state**, with
      **draw-and-discard** where a perturbation would skip a draw. This is the
      bug that reversed a headline result in the mmca line (claude-8,
      2026-07-30).
- [ ] M1 runs with the **stub agent only** — zero LLM calls.
- [ ] Query a generated DB and confirm referential integrity actually holds.
- [ ] Confirm the world generator is **specified, not fitted**. If anything
      learns a distribution from data, ground truth is lost and the testbed is
      void.
- [ ] `transport` prints measured-source, predicted-target, measured-target, gap.

## Post-dispatch findings to apply

1. ~~**Remote**~~ **CLOSED.** Corrected by hand *and* independently by codex-1;
   verified at review: `git@github-holtz:tothedarktowercame/clonic.git`, zero
   `holtzermann17` occurrences in tracked files, nothing pushed.

   **Protocol lesson worth keeping — a whistle's client-side timeout does NOT
   cancel the work.** The correction whistle blocked because codex-1 was busy
   with the build, my `timeout 200` killed the *caller*, and I recorded it as
   "may not have landed". It had landed: it queued as its own job
   (`invoke-1785437357458-376-ffe0cdcd`, ~84s after the build job) and completed,
   reporting via auto-bellback after the review was already done. Defensive
   double-fixing cost nothing here, but the model was wrong. Second whistle
   surprise this session — the first being a synchronous whistle that *also*
   fired an auto-bellback (double delivery of identical content). Both belong
   with `futon3c/holes/excursions/E-crossed-bells.md` if that gets written up.

2. **Mirror τ²-bench's vocabulary.** I invented a layout; theirs is proven and
   is the one the target community reads. Theirs:
   `data/tau2/domains/<domain>/{tasks.json, policy doc, tool definitions}`,
   `src/tau2/{domains,agent,gym,orchestrator}/`, `examples/agents/`, results in
   `data/simulations/`. Their `mock` domain plays exactly the role `retail_v0`
   plays here. Align names where cheap — legibility to that community *is* the
   cold-channel strategy (`futon7/holes/E-tonic-osint.md` §5b.5).

3. **ADD A DOMAIN-RANDOMISATION ARM — the most important of these.** Prompted by
   Joe, 2026-07-30: Tonic's departed Head of Research was strong at RL.
   Consequence: **transport between a generated and a real world is, in RL
   vocabulary, the sim2real problem**, and RL's established answer is *domain
   randomisation* — don't model the difference, randomise over it. That is the
   **rival method**, and my spec does not test it.

   As dispatched, `transport` compares two arms (naive vs transport-corrected).
   **It needs three:**

   | arm | prediction of target score |
   |---|---|
   | naive | assume source = target |
   | **domain-randomised** | randomise over the S-node parameters, take the robust/expected score |
   | transport-corrected | selection diagram + transport formula |

   Without the DR arm, "transport beats naive" is a weak result because the
   obvious rival is untested. With it the experiment is publishable **either
   way** — including if DR wins, which is a real possible outcome.

   Cheap to add: stub agent, no LLM, re-running costs nothing.

4. **Fold the worked example into the README** — `retail_v0`'s DAG
   (tenure→discount_applied; tenure→return_requested;
   discount_applied→order_value; discount_applied→return_requested;
   order_value→return_requested), the two planted S-nodes
   (`p_return_given_discount`; `policy_threshold` £100→£50), the returns task
   with its DB goal state, and the sample transport output block. The README
   must explain the **idea**; this is a gift going to Rob.

5. **Do not preclude the Gym framing.** τ²-bench ships `src/tau2/gym/`. If the
   frontier is *training* environments rather than eval (§ below), the harness
   should be shaped so a Gym adapter is a later addition, not a rewrite. Do not
   build it now.

## Why the RL detail matters beyond the checklist

Tonic's site sells *"high-fidelity simulated environments"* for agent training
and *"logically consistent synthetic data"* for **reinforcement learning** —
that is RL-environment language, not eval language. An RL-strong Head of
Research is exactly who turns synthetic data into **training** environments,
which is the higher-value pitch. So the departure is more disruptive than
"they lost a researcher": they lost the person who could make that turn. Cf.
`EnterpriseBench Corecraft: Training Generalizable Agents on High-Fidelity RL
Environments` (arXiv 2602.16179) — the field is moving eval → training envs.

**And the honest counter clonic must answer:** an RL person's first question is
*"why selection diagrams instead of domain randomisation?"* The answer, which
finding 3 makes testable rather than asserted:

- DR buys robustness at a cost in performance and gives **no guarantee**; it
  cannot tell you when transfer is *impossible*.
- Transportability decides, **before collecting data**, whether the transfer is
  licensed and what must be measured in each world.
- DR handles **parametric** differences (friction, mass, lighting). Business
  differences are often **structural** — a mechanism present in one world and
  absent in the other. You cannot randomise over "the deployed approval
  hierarchy differs."
- They are **complements**: DR needs many cheap environments, which is exactly
  what a generator supplies.
