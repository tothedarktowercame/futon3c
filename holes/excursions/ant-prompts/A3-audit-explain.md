## Kind A3: audit-explain

The item is open with a few acceptance boxes unchecked, and the operator asked for an audit:
what did this mission actually accomplish, what is still open, and would closing or dropping it
lose anything? The page must carry an explanation either way, so the explanation is the product.

Output per item:
```json
{"stem": "<id>", "kind": "audit-explain",
 "accomplished": [ {"value": "<one thing the mission delivered>", "cite": "path:line", "quote": "..."} ],
 "open": [ {"box": {"value": "<unchecked line>", "cite": "path:line", "quote": "..."},
            "progress": {"value": "<any record of partial work toward it>", "cite": "path:line", "quote": "..."} OR {"unsure": "no record found in the file or under <paths>"}} ],
 "consumers": {"value": "<who or what uses the delivered parts today: file, mission, endpoint>", "cite": "path:line", "quote": "..."} OR {"unsure": "..."},
 "explanation": {"value": "<=80 words, plain, what it accomplished and what closing would leave undone", "cite": "path:line", "quote": "..."},
 "recommend": {"value": "close|drop|continue", "cite": "path:line", "quote": "<the line that most supports this>"},
 "confidence": "high|medium|low"}
```
