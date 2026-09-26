# CODEX-HANDOFF — S1: codex rollout → evidence-store harvester

Mission: `holes/missions/M-codex-sorry-loop.md` S1. Prepared 2026-07-28
by claude-6. **Delivery: Agency bell from claude-6. Bell claude-6 back
with summary + commit SHAs.** Owner reviews after landing.

## Goal

A babashka ingester that turns Codex CLI rollout JSONL
(`~/.codex/sessions/YYYY/MM/DD/rollout-<ts>-<session-uuid>.jsonl`) into
**turn-round-shaped evidence rows** in the futon1b store (:7073), so the
scribe mining lanes can consume codex sessions exactly as they consume
zai sessions. You are building the codex analogue of the zaif profile's
`persist-round!`, as a *post-session batch*, not a live stream.

## Files

`:in` (READ-ONLY):
- `~/.codex/sessions/2026/07/22/rollout-2026-07-22T20-53-09-019f8b63-a009-79e0-9a22-e7402848c822.jsonl`
  — the FIXTURE session (8,890 events; census: 1,818 reasoning, 1,206
  function_call+output, 657 custom_tool_call, 396 message, 273
  patch_apply_end, 85 turn_context).
- `holes/labs/M-typed-memories/connectivity_meter.bb` — bounded-query /
  retry / write-once conventions.
- One zai `:turn-round` evidence row (GET
  `127.0.0.1:7073/api/alpha/evidence?type=coordination&limit=1&tags=turn-round`
  or similar) — match its shape/spirit; the scribe already mines that
  shape.

`:out` (create under `holes/labs/M-codex-sorry-loop/`):
- `rollout_harvester.bb`
- `harvest-dryrun-019f8b63.edn` — full dry-run output on the fixture
- `harvest-report-20260728.edn` — per-run report (rows written /
  skipped-existing / errors)
- `s1-note.md` (≤40 lines)

## Contract

1. **Granularity**: ONE evidence row per turn (turn_context boundaries;
   ~85 for the fixture), body containing: turn number, the turn's
   reasoning texts (full), agent messages (full), and a COMPACT tool
   digest (function/tool name + first ~200 chars of args/output;
   patch_apply files touched + status). Hard cap ~16KB body per row —
   the store browned out on GB-scale before; truncation is flagged
   in-row, never silent.
2. **Idempotency**: evidence id derived deterministically from
   session-uuid + turn seq (e.g. `e-codexroll-<uuid8>-t<NNN>`). Before
   writing, probe for the id; re-ingest of an ingested session writes 0
   rows. (Outbox principle: id fixed at creation.)
3. **Provenance per row**: `:profile :codex-rollout`, session uuid,
   rollout file path, event seq range, harvester version. Tags:
   `[:codex :turn-round :codex-rollout]` + agent id when derivable from
   session meta. Type/claim-type: mirror the zai turn-round convention
   (`:evidence/type :coordination`).
4. **Dry-run FIRST**: default mode writes rows to the dry-run EDN file
   only. `--commit` gates store writes. The dry-run fixture output is a
   review artifact — commit it.
5. **Write discipline**: EDN body via the append endpoint with
   `x-penholder` header, SUBSTRATE base (:7073) not the agency base (the
   three receipt footguns apply verbatim); batched with a small sleep
   (≥100ms) between writes; every write verified by read-back; on 503 /
   `:expensive-read-busy` back off 5s, retry once, then stop and report
   (never hammer a browning store).
6. **CLI**: `bb rollout_harvester.bb --session <uuid> [--commit]` (find
   the file under ~/.codex/sessions by uuid) and `--file <path>`
   variant. `--limit N` to cap rows per run.
7. **Live acceptance is ONE session**: after dry-run review, live-ingest
   the fixture session only. Do not bulk-ingest history — that is a
   later, gated decision.

## Acceptance checklist

- [ ] Dry run on the fixture produces ~85 turn rows; spot-checkable
      against the raw JSONL (turn numbers, reasoning presence, digest
      compactness); committed as the dry-run EDN.
- [ ] In-script fixture assertions: row count == turn_context count;
      every row body ≤ cap; ids deterministic (re-parse → identical ids).
- [ ] Live ingest of the fixture session with `--commit`: report shows
      N written, read-back verified; immediate re-run shows N
      skipped-existing / 0 written.
- [ ] `clj-kondo` 0 errors; `check-parens` clean; `git diff --stat`
      only this packet's `:out` files.
- [ ] Bell claude-6 with summary + SHAs.

## Out of scope

Bulk historical ingest; live streaming; mining (S4); any store write
outside the append path; zai rows.
