# The dispatch side of the seam

A turn record is one half. The other half is **who interprets it**, and that
boundary is the one that worked: four delegate seats were swapped in a single
evening with no code change, and when a seat vanished from the registry
mid-run the failure was *"that address is gone"*, not *"the other side
broke"*.

That is the property to copy. The appliance does not know which generator.

## The contract

A dispatch carries a **brief**, and a brief is **self-contained**. The
delegate is assumed to have no standing knowledge of the task, the
conversation, or the system. Everything it needs is in the brief:

| the brief must name | why |
|---|---|
| the record's path | the delegate reads the turn, it is not handed the text |
| the tool that produces the output shape | so the delegate does not invent one |
| the tool that publishes the result | publication is an act, not a reply |
| what it must *not* assume | "you did not receive this turn"; "nobody is waiting on a reply" |
| what to record when it declines | a rejected reading is evidence, not silence |

If the brief needs the delegate to already know something, the seam is not
declared — it is a convention between two particular parties.

## Choosing the delegate

One variable. Not a conditional, not a prefix match on an identifier, not a
lookup keyed by which provider an agent happens to be. The caller asks for the
**role** — *interpret a turn* — and a binding says which seat plays it.

This is the same defect as the turn record's, one level up, and the
[mission](../../holes/missions/M-futon-seams.md) records what it cost to get
wrong: an implementation that read the provider out of an agent identifier
returned "which provider is this agent" where the design had asked "which seat
plays this role". Every want came back partial.

## Fire-and-forget, and what it owes you

A dispatch must not delay the conversation, so it does not wait. That is
right, and it has a price the caller must pay:

- **A successful send means delivered, not done.** The brief arrived. The
  delegate may still decline it — for a missing requisition, a policy, a
  format it will not accept.
- **So record the job the dispatch created.** Without it, a delegate that
  refused and a delegate that is busy leave the same state. 152 records
  accumulated in exactly that state before anything here noticed, and the
  only thing that eventually reported it was an automated bounce.
- **Then ask, later, what became of it**, and write the answer onto the
  record: `requested` → `analyzed` | `refused` | `failed`.

## What the delegate returns

Not a reply. A **published artefact** beside the record, plus the record's own
`analysis_status` set to `analyzed` and `analysis_file` naming it. A reply is
a message to whoever is listening; an artefact is a fact that outlives the
conversation.

If the delegate cannot do the work, it says so on the record rather than
succeeding quietly. A missing interpretation is recoverable. A wrong one that
claims to be right is not.
