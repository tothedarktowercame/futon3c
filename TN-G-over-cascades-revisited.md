# TN: "G over cascades" revisited — what happened when the requirement was accepted

Date: 2026-09-30. Author: claude-17, at Joe's direction. Status: pilot, six cases.

## Why this note

On 2026-09-30 the War Machine was found to have selected among exactly one
policy for every selection that led to work
(`futon2/holes/labs/wm-contract/incidents/INCIDENT-2026-09-30-one-policy-selection-repeated-refusal.md`).
codex-6's audit (`futon2/holes/labs/wm-contract/NOTE-lean-backdoor-audit-2026-09-30.md`)
traces this to the Lean model: `cascadePolicySet` takes the policy list as an
input the runtime supplies, so any non-empty, duplicate-free list satisfies the
theorems, including a list of one hand-written policy.

Joe's reading (2026-09-30): this was not a broken delivery promise. The
requirement "G is defined over pattern cascades" was accepted without anyone
asking how the cascades would be produced, so what was built met the wording
and left the source of the cascades free. A free input of that kind is "a
`sorry` in the data structure". He proposed that the machine's deficiencies are
mirrored in the build process: the machine saw little of its niche and compared
one or two policies; the builders saw little of the requirement's history and
weighed one design.

This note tests that on the record: the turns where Joe stated the requirement,
and what the agent replied each time.

## Method and its limits

- Searched the 3,769 batch-analysed operator turns (2026-08-22 to 2026-09-21)
  for `over (pattern )?cascades` and `polic(y|ies) (is|are) … cascade`. Six
  turns matched. One regular expression; other phrasings of the same
  requirement are not counted, so six is a lower bound.
- For each turn, took the agent's reply from the session transcript, matched by
  timestamp (within one second in five cases; the Codex case matched by text).
  The reply is the assistant text up to Joe's next turn.
- Read each reply for one thing: did it ask, or state as open, **where the set
  of cascades that G compares comes from**? That is the input the audit found
  free.
- Not done: later turns of the same sessions were not read, so a question asked
  two turns later is not counted. The sessions before 22 August are not in the
  analysed corpus. Six cases support no rate.

## The six cases

| # | Date | Seat | What Joe said | What the reply did | Asked where the cascades come from? |
|---|---|---|---|---|---|
| 1 | 08-30 | claude-15 | A policy organises the cascade into a direction of play; it is an operation on cascades | Typed it (`Policy : Cascade → …`, `G(π)` over `Q(o∣π)`), withdrew its own earlier `Policy := Cascade`, and wrote five questions for re-examining the Snatch example | No. The five questions are about what a policy is and what C is. `Cascade := DAG Pattern` is given as a type; which cascades exist is not raised |
| 2 | 09-02 | claude-1 | A policy is a cascade at the policy grain; design patterns are themselves policies | Recorded the ruling verbatim, marked a worklist row done, reopened nine rows, restarted the loop | No. No questions at all |
| 3 | 09-12 | claude-15 | G over policy cascades must be a core part, not an extra; the Lean attests a run that wrote down no wiring | Recorded it as a ruling, extended the chain to "G-evaluation record → selected cascade → holes → wiring", dispatched an audit of Lean attestations | Partly. It says the outline is "computed by G over candidate cascades/policies" and asks the audit what per-turn G records exist. It does not ask how the candidates are generated |
| 4 | 09-15 | codex-26 | Provide a faithful Lean model "expressing my frequently stated intent to define G over pattern cascades" | Stated that G must be computed from predicted consequences of cascades, "not supplied as an arbitrary scoring function"; contacted claude-20 | No. It names the scoring function as something that must not be supplied, and says nothing of the policy set |
| 5 | 09-17 | claude-4 | "I've told you, a cascade is a policy, and G is computed over policies" | Agreed; proposed checking where the decision's number came from, and noted that "an action can be dressed up as a one-step cascade" | Closest. It sees that a degenerate cascade can satisfy a shape check. The check it proposes is on the provenance of the probability, not on the size or source of the cascade set |
| 6 | 09-18 | claude-12 | The Lean must model AIF, map what Clojure implements, and certify runs; G over cascades has no certification | Surveyed the Lean against the three purposes; found six G-computing namespaces with no Lean binding and no witness for horizon G | No. It names C as "the one free preference constant in the census" and asks to discuss it. The policy set is not listed as free |

## What the six cases show

1. **The requirement was accepted every time, and the accepting act was
   recording.** Four of six replies contain no question. The usual response was
   to write the statement into a record as a ruling and resume work. Recording
   a ruling is an acknowledgement that it was heard; it says nothing about
   whether it can be carried out as stated.

2. **Nobody asked where the cascades come from.** Two replies came near. Case 5
   saw that a one-step cascade passes a shape check. Case 4 said the scoring
   function must not be supplied freely. Each identified one free input and
   closed that one. Neither generalised to "list every input the runtime
   supplies". The policy set stayed free until the incident.

3. **Free inputs were being found one at a time, by different seats.** Case 4
   found the scoring function, case 5 the action dressed as a cascade, case 6
   the preference C. Case 6 calls C "the one free preference constant in the
   census", so a census of free constants existed on 18 September and the
   policy set was not in it. This is the most specific finding here: the check
   Joe now asks for had a precursor, and its scope was constants, not
   runtime-supplied collections.

4. **Each seat met the requirement as if new.** The six statements went to five
   different seats in six sessions. Joe's wording shows the accumulation
   ("frequently stated intent" on 15 September; "I've told you" on the 17th).
   No reply refers to an earlier statement of it to another seat. The
   repetition was itself evidence that the requirement was not being met, and
   only Joe could see it.

5. **No reply said the requirement was hard, underspecified or impossible.**
   That is the feedback Joe says would have drawn the missing detail out of him
   early.

## The mirror, stated with what this pilot supports

| War Machine | Build process | Supported here? |
|---|---|---|
| Saw a few of ~600 open items | Each seat saw one statement of a requirement made at least six times | Yes (finding 4) |
| Compared one policy | One design taken up without alternatives | Not tested; replies were read, not the designs |
| A free input filled with whatever ran | The source of the cascades left unasked at acceptance | Yes (finding 2) |
| Did not report its critical parameters | Did not list what the model left free | Partly: a census existed and missed the collection (finding 3) |
| Took an assertion as the outcome | "Recorded as a ruling" closed the item | Yes (finding 1) |

## What would have changed the outcome

These follow from the findings; none is built.

- **At acceptance, list the free inputs.** For a requirement that will be
  formalised, the accepting reply names every value the runtime will supply
  and says what countable thing each is computed from. An input with no source
  goes back to Joe as a question. This is the `#print axioms` habit extended
  from `sorry` to hypotheses.
- **Count restatements.** When Joe states a requirement, show the seat how
  many times it has been stated before and to whom. The analysed operator
  turns already allow this; the search in this note took seconds.
- **Report the value each free input took.** Joe's critical-parameters ruling.
  A policy count of one is visible on the first run.
- **Separate "recorded" from "understood".** A ruling written into a record is
  in the same position as an agent's assertion that a gap is fixed: it needs
  something further before it counts as closed. In M-象-cascade's terms the
  states are open, asserted, and closed by a fresher independent record.

## In McCarthy's terms

In Elephant 2000 an accepted request creates a commitment, and correctness
includes fulfilling commitments. The six replies are acceptances. What each
committed the agent to was the agent's reading of the sentence, and that
reading was never stated back in a form Joe could reject. The missing speech
act is a question, or a counter-statement of the commitment as understood, at
the time of acceptance. No later promise was broken, because the commitment
taken on was already weaker than the one Joe intended.

## Follow-ups, if wanted

1. Widen the search beyond one regular expression, and into sessions before
   22 August, to find the first statement.
2. Read the turns after each reply to see whether the question came later.
3. Repeat for a second requirement from the 30 September session (for example
   "no facades", or "the field is all unfinished work") to see whether the
   pattern is particular to this one.
