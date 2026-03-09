# Mission: Autonomous Pattern Lifecycle

Date: 2026-03-09
Status: READY

## Derivation

IDENTIFY step. Builds on:
- M-walkie-talkie (HTTP endpoints as universal agent tool surface — DONE)
- M-psr-pur-mesh-peripheral (discipline peripheral with PSR/PUR/PAR tools — DONE)
- M-peripheral-phenomenology (backpack metaphor, pattern card)
- p4ng paper §5–6 (Pattern Selection and Use, Patterns and Active Inference)
- futon3/CLAUDE.md Exploratory Pattern Mode (PSR/PUR logging protocol)
- `resources/sigils/patterns-index.tsv` (pattern library with maturity states)

## The Problem

Pattern selection and use currently requires manual user commands (`!psr`,
`!pur`, `!par` in Emacs repls or IRC). The walkie-talkie endpoints exist
(HTTP POST to `/psr`, `/pur`, `/par`) and the Emacs command dispatch works
(Option 2: infrastructure handles HTTP, agent handles judgement). But the
agent doesn't know it should exercise these capabilities autonomously.

The p4ng paper (PLoP 2025) prototyped autonomous pattern lifecycle via a
tool roster injected into the agent's session prompt:

```
Tool roster (you should emit these signals when you do the corresponding action)
- pattern-select library/<pattern>    <state why you want to read it>
- pattern-use    library/<pattern>    <state where you will apply it>
```

This worked at a basic level but was unstable:
1. CLI-only — signals were stdout parsing, not structured HTTP
2. No belief update loop — selections didn't inform future turns
3. No precision priors — all patterns weighted equally
4. No deviation tracking — agents could ignore suggestions silently
5. Fragile wiring — fucodex JSON streaming + ad-hoc event appending

Now we have walkie-talkie HTTP endpoints, evidence landscape persistence,
backpack state, and pattern search via futon3a. The infrastructure exists
to do this properly.

## The Insight

From the p4ng paper §6: pattern lifecycle is an Active Inference loop.

```
Generative Model ──→ Action Selection ──→ Belief Update
(Pattern lib            (PSR: which          (PUR: did
 as priors)              pattern now?)        it work?)
```

The agent's backpack carries the currently selected pattern (pattern card).
At task boundaries, the agent should:

1. **Select** — query futon3a for candidate patterns matching the current
   context, score them against precision priors, pick one, POST a PSR
2. **Apply** — use the selected pattern as a lens for the current work
3. **Assess** — at task completion, POST a PUR with outcome, prediction
   error, and whether the pattern actually helped
4. **Update** — the PUR feeds back into precision priors for future turns

The key design principle: **AIF guides, it does not control**. The agent
sees a suggestion with probability scores but may override with justification.
Unjustified deviations are logged but not blocked.

## Scope

### In scope (Phase 1: Prompt Integration)

- **System prompt stanza** that tells agents about their pattern lifecycle
  capabilities — what walkie-talkie endpoints exist, when to use them,
  what information to include. Plain language, not tool-roster syntax.
- **Session-start PSR** — on session init, agent queries patterns relevant
  to the stated task and selects one. Infrastructure assists by pre-loading
  context (backpack contents, recent PURs for this agent).
- **Task-boundary PUR** — at natural task boundaries (commit, test pass,
  milestone), agent assesses the pattern it's carrying. Infrastructure
  reminds via lightweight prompt injection at boundary events.
- **Backpack integration** — selected pattern persists in the agent's
  backpack. On session resume, agent sees what pattern it was carrying
  and its last PUR outcome.
- **Evidence persistence** — PSRs and PURs land in the evidence landscape
  with proper typing (`:pattern-selection`, `:pattern-outcome`).

### In scope (Phase 2: Precision Priors)

- **Maturity-state priors** — pattern maturity (`:active`, `:greenfield`,
  `:settled`, `:stub`) maps to precision priors (0.8, 0.4, 0.9, 0.2).
- **Expected free energy scoring** — given candidates, compute G scores
  factoring in precision prior, recency of last PUR, and outcome history.
- **Softmax sampling** — τ parameter (derived from context uncertainty)
  controls exploration/exploitation. Low τ = greedy, high τ = exploratory.
- **AIF suggestion display** — agent sees suggestion with scores:
  "Pattern suggestion: `code-coherence/test-before-commit` (G=0.12, τ=0.72, p=68%)"
- **Deviation justification** — if agent picks a different pattern, the
  system requires a reason. Unjustified deviations are logged.

### In scope (Phase 3: Cross-Session Learning)

- **PUR→prior feedback** — prediction errors from PURs update the precision
  prior for that pattern over time. Patterns that consistently help get
  higher priors; patterns that don't get lower ones.
- **τ adaptation** — uncertainty parameter adjusts based on session context:
  novel tasks → high τ (explore), routine tasks → low τ (exploit).
- **Abstention threshold** — when τ falls below a configurable threshold,
  the system abstains from auto-selection and asks the agent to decide
  explicitly. This routes genuine uncertainty to human review.
- **Cross-agent learning** — PUR outcomes from one agent inform priors for
  all agents (shared pattern library, individual precision adjustments).

### Out of scope

- Changes to the pattern library format itself (futon3a concern)
- Peripheral dispatch changes (agents already inhabit peripherals)
- New transport layers (walkie-talkie HTTP is sufficient)
- Replacing the user's ability to manually invoke !psr/!pur/!par

## Gates

### Gate A: Agent Self-Selects Pattern (Phase 1)

An agent, given a coding task, autonomously:
1. Queries futon3a for relevant patterns
2. Selects one with stated rationale
3. POSTs a PSR to the evidence landscape
4. Carries the pattern in its backpack
5. At task completion, POSTs a PUR with outcome assessment

Evidence: PSR and PUR entries in evidence store with matching pattern-id,
agent-id, and session continuity. No manual !psr/!pur commands used.

### Gate B: AIF Scoring Influences Selection (Phase 2)

Given two candidate patterns, the AIF layer:
1. Computes G scores from precision priors and outcome history
2. Produces softmax probabilities
3. Suggests the higher-probability pattern
4. Agent follows or deviates with justification

Evidence: `:aif/summary` event in evidence store showing G scores, τ,
probabilities, and whether the agent followed or deviated.

### Gate C: Cross-Session Prior Update (Phase 3)

Over 5+ sessions:
1. PUR prediction errors accumulate for a pattern
2. Precision prior adjusts (patterns that work get higher prior)
3. Changed prior visibly affects subsequent AIF suggestions

Evidence: Prior trajectory for at least one pattern showing movement
from initial value based on accumulated PUR outcomes.

### Gate D: τ Adaptation and Abstention

In a session with genuinely novel context:
1. τ rises above exploration threshold
2. AIF spreads probability across candidates instead of concentrating
3. If τ drops below abstention threshold, system explicitly abstains
4. Agent makes explicit choice, logged as manual override

Evidence: Session log showing τ trajectory, abstention event, and
explicit agent choice with rationale.

## Implementation Notes

### Where the system prompt stanza lives

For Claude Code CLI: `CLAUDE.md` or system prompt injection
For Emacs repls: `agent-chat--system-prompt` or surface contract
For IRC bridge: `irc-invoke-prompt` in dev.clj

The stanza should be factual (surface contract style, per I-1):

> You have a walkie-talkie with pattern lifecycle capabilities. Your
> backpack currently contains: [pattern-card contents or "empty"].
> Your recent pattern outcomes: [last 3 PURs or "none"].
> When you start work, search for a relevant pattern and select one.
> When you complete a unit of work, assess how the pattern helped.
> Use plain HTTP POSTs — the infrastructure handles evidence persistence.

### Boundary detection

The hardest part: knowing when to trigger PSR/PUR. Options:
1. **Explicit boundaries** — commit events, test results, user approval
2. **Turn-count heuristic** — every N turns, prompt for PUR assessment
3. **Agent initiative** — trust the agent to recognize boundaries
4. **Hybrid** — infrastructure suggests, agent confirms or defers

Phase 1 should start with option 3 (agent initiative with prompt guidance)
and add option 1 (explicit boundaries) if agents don't self-trigger reliably.

### Relationship to existing surfaces

- `!psr`/`!pur`/`!par` (Emacs walkie-talkie) — remain as user-initiated
  overrides. The autonomous lifecycle supplements, not replaces, manual use.
- `/psr`/`/pur`/`/par` (Claude Code skills) — remain as agent skills.
  The autonomous lifecycle means agents should use these more often, not
  that we remove them.
- IRC `!psr`/`!pur`/`!par` — same: manual override surface, unchanged.

### Precision prior storage

Options:
1. **In-memory atom** — fast, lost on restart (like current backpacks pre-fix)
2. **EDN file** — `~/code/storage/futon3c/pattern-priors.edn`
3. **Evidence landscape query** — derive priors from PUR history on demand

Option 2 for Phase 2 (simple, durable). Option 3 for Phase 3 (ground truth
from evidence, but slower).

## Prior Art

- p4ng paper (PLoP 2025) §5–6: Pattern Selection and Use, Active Inference
- Alexander's star-rating system for pattern confidence
- Friston's Active Inference Framework (free energy principle)
- Matović et al. on stochastic pattern sequencing
- futon3/CLAUDE.md Exploratory Pattern Mode (PSR/PUR logging protocol)
- fucodex CLI wrappers (fuclaude, fucodex) with tool roster injection
