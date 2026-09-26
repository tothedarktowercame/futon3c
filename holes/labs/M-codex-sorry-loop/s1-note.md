# S1 — Codex rollout harvester

The post-session Babashka harvester converts one Codex rollout JSONL into
zai-compatible `:coordination` / `:turn-round` evidence rows.

The designated `019f8b63` fixture contains 86 `turn_context` boundaries in
the file as read on 2026-07-28 (the earlier packet census estimated 85).
The committed dry run therefore contains 86 rows and asserts that boundary
count, deterministic IDs, and the 16KB serialized-body limit.

Rows retain all plaintext reasoning summaries and agent messages available
within the cap, plus compact call/output and patch digests. This rollout's
reasoning payloads are encrypted-only; each row records that count rather
than pretending ciphertext is plaintext. Thirty-one large rows are visibly
flagged `:truncated? true`.

Live acceptance wrote and read-back verified all 86 fixture rows. The
immediate replay wrote zero and reported 86 `:skipped-existing`.

This is an ingestion seam, not a miner. No historical bulk ingest, live
streaming, attachment, or ranking change is included.
