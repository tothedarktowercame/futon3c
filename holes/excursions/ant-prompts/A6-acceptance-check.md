## Kind A6: acceptance-check

The item is an older mission whose cluster has moved on to later marks. Its acceptance checklist
still has unchecked boxes. Your question, for EACH unchecked box: is it met by a later record,
superseded by a later decision, or still open with no record either way?

Where to look: the later marks live in `/home/joe/code/futon6/holes/` as `M-mark8.md`,
`mark7-*.md`, `mark8-*.md`, and under `/home/joe/code/futon6/holes/missions/`. Search those for
the box's own terms (corpus size, stage 9b, cosine, LeanDojo, learn-to-swim, corpus_ws_bridge,
FAISS, precision, BGE). Do not infer "met" from a plan; only a record of a result counts as met.

Output per item:
```json
{"stem": "<id>", "kind": "acceptance-check",
 "boxes": [
   {"box": {"value": "<the checklist line>", "cite": "path:line", "quote": "..."},
    "verdict": "met|superseded|open",
    "by": {"value": "<the later record that meets or supersedes it, in one sentence>", "cite": "path:line", "quote": "..."} OR {"unsure": "searched <paths>, found nothing that speaks to it"}}
 ],
 "confidence": "high|medium|low"}
```
