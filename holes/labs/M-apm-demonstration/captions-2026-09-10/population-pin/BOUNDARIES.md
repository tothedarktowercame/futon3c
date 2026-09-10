# APM caption population pin boundaries

This pin is a population and present-admissibility census, not a judgment of
caption-worthiness.

## Operational boundary

“APM-relevant” means a memory ID that appears in
`:request :memory-snapshot :accessible-memory-ids` in any archived Student
attempt for frames f190 through f213 under `data/apm-campaigns/jit-all-open-v3`,
or is one of the 57 deposits pinned by the prior audit readback. The shelves
are the actual prompt-exposure boundary and retain older campaigns; the audit
cohort adds three newest deposits made after their own prompt was assembled.
The census reads the relevant attempt and snapshot EDN files, then performs
only HTTP GETs against futon1b:
`/api/alpha/evidence/{id}` for each memory and recorded review pointer, and
`/api/alpha/hyperedges?end={id}&type=memory/assert&limit=1000` for current
attachment state.  No search endpoint, agent, POST, credential, or store write
is used.

The endpoint-specific hyperedge query returned fewer than its 1000-row limit
for every ID, so no pagination boundary was hit. Evidence lookup is a single-ID
endpoint and has no pagination. The archived shelves range from 304 through
361 IDs; the union is deliberately used rather than assuming the final shelf
contains every historically exposed ID.

## Inclusion and dispositions

Rows retain the archived depositor, review pointer, deposit campaign/frame/
problem when known, and typed memory-use kind. Missing typed kind is reported
as `other`; it is not inferred. `admissible` means the memory body is currently
GET-readable, a current reviewed `memory/assert` edge is GET-visible, and a
recorded review pointer is GET-readable. Other states name the failed
condition. Body SHA-256 is computed over the current memory body string;
the archived signed content digest is retained separately.

The prior audit's 57 IDs are marked by `audited-57-subset`; the count must be
57. Snapshot records outside both boundaries are listed in
`snapshot-only-excluded`, not promoted into invented population rows. Any
unreadable memory/review or absent current attachment is preserved with an
explicit state in the JSON. Unknown campaign/frame/problem and review pointers
remain null or absent rather than reconstructed.

## Reproduction

From the futon3c repository root, with the read-only store at the default
`http://127.0.0.1:7073` (or an explicit `FUTON1B_URL`):

```sh
bb holes/labs/M-apm-demonstration/captions-2026-09-10/population-pin/census.bb
```

Two consecutive runs on 2026-09-10 were byte-identical. See `checks.txt` for
the recorded gates and artifact digest.
