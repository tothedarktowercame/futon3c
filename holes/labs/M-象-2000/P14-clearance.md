# P14 incident clearance

One new minted hyperedge type: `incident/clearance`. It records explanation,
permission to end the three named rule descriptions/versions, and 42 individual
compensation debts. It performs no rule withdrawal and no settlement. Its limited
recognition claim is exactly “this rule recognises this recorded sequence”.
Adoption/grant remains `:unrecorded`; provenance is retrospective, recorded now
by codex-4. Storage valid time is recording time, not historical resolution time.

Resolution source: claude-10's 2026-09-24T15:54:29.765197427Z assistant turn,
`emacs-40abad9cad76117c720c8c22a8d7dd25`, cites the investigation commit
`d5e3147e77710149c1915416c5848564469d63ba`. “Resolved” means explained; it does
not assert that consumed usage or delivered notices were undone.

`incident-clearance/validate!` takes a record and independently queried context.
The CLI calls P0's `--clearance-context` read-only capture, sharing its exact
origin-backed notice set and system-time pin, then validates before any POST.
It resolves measure IDs to rule records owned by this incident and checks exact
compensation membership, not just 42 as a count. The saved validation-context
fixture contains actual query inputs (unused rule properties omitted). Each
refusal test mutates the real clearance record, and checks no write occurred.
This packet records unsettled debts only; settlement needs a separate sourced act.

From the master checkout:

```
clojure -M -m futon3c.agency.incident-clearance holes/labs/M-象-2000/P14-clearance-record.edn
clojure -M -m futon3c.agency.incident-clearance --write holes/labs/M-象-2000/P14-clearance-record.edn
python3 scripts/xiang2000_p0.py --snapshot DIR --check
```

Default CLI is validation only. `--write` uses P6b minting/idempotency and verifies
exact readback. No route changes or JVM reload. P0 captures `clearance.json` in
its integrity manifest; row 12 validates the notice set against the record before
printing. The retrieval-ordering detail remains unresolved and unchanged.
