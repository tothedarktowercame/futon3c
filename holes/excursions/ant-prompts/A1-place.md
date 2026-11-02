## Kind A1: place

The item is open and nobody has placed it on the map of the stack. Your question: what is it,
is it still open, which of the stack's four therefores does it serve, which declared way of
working it belongs to, and what one action would move it. You are placing, not deciding; a
reviewer scores your placement against another reader's.

Wings (the four therefores of the apex thesis, plus two others):
- A: records carry warrant — an assertion severed from its derivation cannot be checked or learned from.
- B: one queryable self-account — knowing what exists and its state, with provenance, at stack scale.
- C: feedback reaches every participant — the loop's human and stakeholder edges are where learning dies.
- D: accountable next-action choice — deciding what to do next, with a reason, goals included.
- V: stays alive and paid — machines, history, money; the body and energy of the stack.
- none: not about the stack's ways of working (e.g. a paper's content, an outside commitment).

Ways of working (pick the one the item most serves or most lacks; `none` if no fit):
records-carry-warrant, inbox-zero, operator-turns-logged (A); one-queryable-self-account,
backlog-understood (B); feedback-reaches-every-participant, real-users, intents-are-tracked (C);
selection-gives-a-reason (D); stays-alive-and-paid (V); eat-your-own-tail (the agent is inside the process it runs).

Output per item:
```json
{"stem": "<id>", "kind": "place",
 "what": {"value": "<one sentence: what the item is for>", "cite": "path:line", "quote": "..."},
 "status": {"value": "open|closed|stopped|unsure", "cite": "path:line", "quote": "<the status line or the line that shows it>"},
 "wing": {"value": "A|B|C|D|V|none", "cite": "path:line", "quote": "<the line that most shows which therefore it serves>"},
 "way": {"value": "<way id or none>", "cite": "path:line", "quote": "..."},
 "action": {"value": "work|close|retire|held|merge-into:<id>|unsure", "cite": "path:line", "quote": "<the line that supports the action>"},
 "finish": {"value": "<what done would look like, from the file>", "cite": "path:line", "quote": "..."} OR {"unsure": "no finish stated"},
 "beneficiary": {"value": "self|company|grant-math|product|job|unsure", "cite": "path:line", "quote": "..."},
 "confidence": "high|medium|low"}
```
Rules for this kind: a kimi-task requisition (`E-kimi-task-N`) is a record of one dispatch; its
"what" is the task it carried, its status is the requisition line near the top. A draft mission
marked NON-LIVE is `held`, not `work`. Do not place an item by its filename alone.
