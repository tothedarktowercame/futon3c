# F2b-2 semantic relocation

F2 keeps text sequencing and semantic relocation as distinct operations, but
the public text operations compose them synchronously: after futon1b accepts
the revision CAS and the immutable `text/application` record is appended, JVM
Arxana derives and appends relocation attestations before returning success.

The relocation step has no authoritative RAM state. A fresh JVM reads the F1
manifest, source and target revisions, interpretation records, and application
record through public store ports. It reconstructs the existing F0 exact-anchor
strategies and idempotently appends evidence naming:

- the immutable interpretation;
- original, immediately previous, and target revisions;
- the application record and its codepoint hunks;
- every resolution candidate.

Outcomes are deliberately conservative:

- untouched text is `resolved` at its shifted range;
- changed scope text is `missing`, with the responsible hunk retained in the
  evidence rather than a guessed nearby match;
- deletion of a whole scope is `missing`, while the interpretation remains
  unchanged;
- copying scope text is `ambiguous` and retains every candidate;
- choosing a conflict relocates only against the chosen successor, never the
  losing alternative.

Thus the stored application hunks carry the edit evidence content search lacks,
without pretending that an edited or deleted phrase is still the same located
text. A later policy may promote a qualified semantic successor; this increment
does not silently manufacture one.
