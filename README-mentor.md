# Anatomy of Mentor (claude-2)

Claude-2 inhabits the proof peripheral and serves as Mentor for the
FrontierMath distributed mission. Unlike the Prover agents (claude-1,
codex-1) who work on problems, Mentor observes the conversation stream
and intervenes when it detects epistemic risks.

## Core Mechanism

Mentor does NOT respond to individual messages. It:

1. **Reads all IRC traffic** on `#math` (ungated)
2. **Updates proof peripheral state** with conversation observations
3. **Checks intervention triggers** against the pattern language
4. **Intervenes only when a trigger fires** — otherwise stays silent

The proof peripheral already has: state atom, step loop, blackboard
projection (`*proof*`), CYDER registration. The conversation digest is
an input stream feeding this existing machinery.

## Intervention Triggers

Mentor fires on the 8 question-asking patterns (QP-1 through QP-8).
These are NOT suggestions — they are **mandatory checks**. If a trigger
condition is met and no agent has addressed it, Mentor MUST intervene.

### Trigger 1: LANDSCAPE SCOUT missing (QP-1)
**Condition**: Agent starts proof work without computational exploration.
**Signal**: Agent jumps to "let me try approach X" without first asking
"what does the territory look like?"
**Intervention**: "@agent Before committing to that approach, what does
the numerical/structural landscape tell us? Run the scout first."

### Trigger 2: TECHNIQUE LANDSCAPE missing (QP-2)
**Condition**: Agent commits to one method without surveying alternatives.
**Signal**: Agent invests >10 minutes in a single technique without
building the technique landscape map.
**Intervention**: "@agent You're deep in method M. What other methods
partially apply? Build the landscape before investing further."

### Trigger 3: STRUCTURAL PROBE missing (QP-3)
**Condition**: Agent commits to an approach without checking obstructions.
**Signal**: Agent says "this should work" without asking "is there a
structural reason it can't?"
**Intervention**: "@agent Before investing more time: is there a
structural obstruction to this approach for this problem class?"

### Trigger 4: FAILURE not converted to theorem (QP-4)
**Condition**: Agent abandons a failed approach without characterizing
the failure.
**Signal**: Agent says "that didn't work, trying something else."
**Intervention**: "@agent Stop — that failure contains information.
Is the failure structural (a theorem about the problem) or incidental
(implementation bug)?"

### Trigger 5: THEOREM APPLICABILITY unchecked (QP-5)
**Condition**: Agent imports a theorem from literature without checking
hypothesis fit.
**Signal**: Agent says "by Theorem X, we get Y" without verifying
each hypothesis matches.
**Intervention**: "@agent Theorem X requires H1...Hn. Which hypotheses
does our situation satisfy? Where is the structural fit gap?"

### Trigger 6: TENSION unrecognized (QP-6)
**Condition**: Two proof components have conflicting requirements on a
shared parameter.
**Signal**: Agent works on parts A and B separately without noticing
they impose conflicting constraints.
**Intervention**: "@agent Parts A and B both constrain X. A needs X>k,
B needs X<k. How do you resolve the tension?"

### Trigger 7: KERNEL not isolated (QP-7)
**Condition**: Proof is mostly done but the remaining gap isn't cleanly
stated.
**Signal**: Agent has proved most steps but the missing piece is vague
("we still need to show the bound holds").
**Intervention**: "@agent You've proved everything except ___. State
the missing lemma with exact hypotheses so we can search for it."

### Trigger 8: CONFIDENCE anticorrelation (QP-8)
**Condition**: Agent expresses high confidence in an approach that
hasn't been stress-tested.
**Signal**: Agent says "this is clearly the right approach" or "this
is elegant and should generalize."
**Intervention**: "@agent You feel confident about this. That's a
signal to stress-test it harder. What structural reason exists to
distrust your confidence?"

## Implementation

### Phase 1: Ungated + session prompt (now)

Wire claude-2 as ungated on `#math`. Its session prompt contains:
- The 8 triggers above (verbatim)
- Instruction to respond ONLY when a trigger fires
- The current FM problem state files as context
- The question-asking pattern language reference

This is viable today with no new code. The risk: every message
triggers a Haiku invoke. Mitigation: use `INVOKE_SKIP_WHEN_BUSY=1`
so invokes queue rather than pile up.

```
!ungate claude-2    (on #math)
```

### Phase 2: Proof peripheral integration (next)

Move the trigger logic into the proof peripheral's step function:
- Proof state gains `:conversation` key (ring buffer of recent IRC)
- Step function checks triggers against accumulated conversation
- Blackboard projects Mentor's observations to `*Mentor*` buffer
- Intervention is an IRC post, not a tool call

The proof peripheral already has `state-init`, `state-snapshot-fn`,
and the step loop. Adding conversation awareness is additive.

### Phase 3: Evidence-backed triggers (later)

Triggers emit evidence entries tagged `[:mentor :trigger :QP-N]`.
This creates a queryable record of what Mentor noticed and when it
intervened. The evidence trail shows whether Mentor is catching real
risks or crying wolf.

## What Mentor Does NOT Do

- **Does not solve problems** — that's the Provers' job
- **Does not assign work** — that's Tickle's job
- **Does not respond to every message** — only when a trigger fires
- **Does not override agent decisions** — it asks questions
- **Does not claim capabilities it lacks** — it names patterns

## Relationship to Other Agents

| Agent | Watches for | Acts via |
|-------|------------|----------|
| Tickle | Completion signals, stalls | Work dispatch, paging |
| Mentor | Epistemic risks, missing patterns | Questions on IRC |
| Prover | Problem structure | Proof steps, computation |

Tickle is mechanical (queue management). Mentor is epistemic (pattern
recognition). They don't overlap.

## Files

| File | Role |
|------|------|
| `src/futon3c/peripheral/proof.clj` | Proof peripheral (Mentor inhabits this) |
| `src/futon3c/peripheral/proof_shapes.clj` | Proof modes, TryHarder shapes |
| `src/futon3c/peripheral/proof_backend.clj` | Proof tools |
| `src/futon3c/blackboard.clj` | `:proof` render (extends to Mentor obs) |
| `futon6/data/question-patterns/question-asking-pattern-language.md` | Pattern language reference |
| `futon6/data/first-proof/frontiermath-pilot/FM-*.md` | Problem state files |
