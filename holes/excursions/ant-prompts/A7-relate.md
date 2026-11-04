## Kind A7: relate

The War Machine picks the next task by how each candidate relates to the operator's
current focus. Your question for each item: how does this item relate to that focus? You are
reading, not deciding: a reviewer checks your row before it enters the relation corpus, and a
row with a weak citation is worse than `unsure`.

The focus (retained in `futon2/resources/wm/focus/commit-facets-v1.json`): two facets, joined
by a declared same-focus edge.
- **WM**: the War Machine itself. The futon2 outer and inner loops (`futon2/src/futon2/aif/`,
  `wm-contract`, the click loop, its Lean spec `mathlib4/DarkTower/WarMachine/`), AIF policy
  and G, selection, injuries and repair, the meta loop.
- **APM**: the formal proof campaign (APM problems proved in Lean, its seats, registry, blueprints).

Relations (the only three; Lean `CTauClassPreference.lean`, `TargetRelation`):
- `focus`: the item's own work IS WM or APM work: finishing it changes the War Machine or the
  APM campaign directly.
- `associated`: the item is not itself WM/APM work, but the file shows that WM/APM work uses it, depends on it, or
  is blocked by it, or it names a WM/APM mission as its parent or consumer. Cite the line that shows the link.
- `useful-elsewhere`: real work for another part of the stack (memory, Emacs/Arxana, voxterm,
  futon6, money, papers, infrastructure...) with no stated link to WM/APM.

Rules for this kind:
- Judge by what the item's work is FOR, from its own text. A file that mentions the War
  Machine in passing is not `focus`. Shared vocabulary (AIF, G, receipt) is not a link.
- `associated` needs a cited line naming the WM/APM thing it serves or feeds. No such line means
  `useful-elsewhere`, or `unsure` if you suspect a link you cannot cite.
- A ticket (`T-...`) usually names its parent mission or finding; follow that one file if the
  ticket alone does not say what it is for, and cite it.
- An excursion that is a requisition record (`E-kimi-task-N`, `E-...` with a **Requisition:**
  line) relates as the task it carried.

Output per item:
```json
{"stem": "<id>", "kind": "relate",
 "what": {"value": "<one sentence: what the item is for>", "cite": "path:line", "quote": "..."},
 "facet": {"value": "WM|APM|other:<short name>", "cite": "path:line", "quote": "..."},
 "relation": {"value": "focus|associated|useful-elsewhere", "cite": "path:line", "quote": "<the line that decides it>"} OR {"unsure": "<why>"},
 "link": {"value": "<the WM/APM item or path it serves>", "cite": "path:line", "quote": "..."}   (REQUIRED when relation is associated; omit otherwise),
 "confidence": "high|medium|low"}
```
