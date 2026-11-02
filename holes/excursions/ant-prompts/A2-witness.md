## Kind A2: witness

The item was read as "closeable": its stated finish may already have been met. Your question:
what artifact does the finish name, and does it exist?

Output per item:
```json
{"stem": "<id>", "kind": "witness",
 "finish": {"value": "<the finish as the file states it>", "cite": "path:line", "quote": "..."},
 "artifact": {"value": "<path, commit sha, registry id, or endpoint that the finish names>", "cite": "path:line", "quote": "..."}
              OR {"unsure": "the finish names no artifact"},
 "exists": {"value": true|false, "cite": "<the artifact path you checked, or the commit>", "quote": "..."} OR {"unsure": "..."},
 "residue": {"value": "<what the file says is still open after that artifact>", "cite": "path:line", "quote": "..."} OR {"unsure": "..."},
 "confidence": "high|medium|low"}
```
