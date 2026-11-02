## Kind A4: lesson

The item is a workflow the operator describes as "a prototype we can learn from", not a feature:
a method that was used for a while and then stopped being used. Your question: what was it, when
was it last used, what does the record say about why it stopped, and what did it teach? Only the
files are evidence; the operator's own recollection is not available to you and must not be invented.

Output per item:
```json
{"stem": "<id>", "kind": "lesson",
 "what": {"value": "<the method in one sentence>", "cite": "path:line", "quote": "..."},
 "last-use": {"value": "<date or record of the last real use>", "cite": "path:line", "quote": "..."} OR {"unsure": "..."},
 "friction": [ {"value": "<a recorded difficulty, cost or complaint>", "cite": "path:line", "quote": "..."} ],
 "lessons": [ {"value": "<something the record shows was learned>", "cite": "path:line", "quote": "..."} ],
 "lives-on": {"value": "<any part that continues elsewhere: file, mission, repo>", "cite": "path:line", "quote": "..."} OR {"unsure": "..."},
 "confidence": "high|medium|low"}
```
