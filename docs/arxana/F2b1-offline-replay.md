# F2b-1 offline replay experiment

Date: 2026-11-08

The private-stack acceptance used two independent JVM Arxana service
configurations sharing one futon1b/XTDB store. Starting from the F1 exported
conversation, the online side landed eleven successor applications from two
authors: ten ordinary edits (including a deletion and an edit inside an
earlier insertion) plus an attributable conflict choice. The offline side
retained the original revision and proposed five hunks, including four
far-apart edits and one overlap with the online deletion.

Two replay forms were exercised from stored records:

| Replay form | Offline hunks rebased | Conflicting hunks | Alternatives stored | Lost intent |
|---|---:|---:|---:|---:|
| One five-hunk patch | 4 (carried in each result alternative) | 1 | 2 | 0 |
| Five one-hunk submissions | 4 | 1 | 2 | 0 |

The test counts intent as retained only when its distinctive replacement is
present either in the authoritative head or in a stored conflict alternative.
It reads heads, revisions, conflicts and alternatives back through the public
ports. The conflict record identifies the old deletion patch that actually
overlaps the offline hunk rather than merely naming the last patch in the
application chain.

**Verdict: single CAS sequencer suffices for offline replay (no intent lost).**

This does not say a CRDT can never be warranted. It says the specified offline
replay falsifier did not re-arm one: general sequential composition plus
append-only conflicts retained every edit without a second sequencer.

Known open crash window: a process can fail after futon1b accepts the revision
CAS but before its application record is appended. A later stale replay then
returns the typed application-chain-gap refusal. This increment records but
does not repair that atomicity boundary.
