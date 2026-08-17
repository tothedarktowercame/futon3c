# Handoff: claude-7 (lab manager) → successor Analyst (2026-08-17)

*Written at session end (operator's Fable quota exhausted; reset was
Saturday, it is Monday morning). The successor is an Opus-class Claude
taking up the Analyst role — including building it out to specification
where needed. Read `docs/research-plan-v1.md` FIRST (especially §10
duties and §11, the operator's sawmill-dual ruling — you dispatch AND
close your own packet backlog; propose-only is retired). This file is
everything else: the state, the live protocols, and the traps.*

## 0. Who you are, where you sit

- Register/act as a fresh claude seat on the Agency (roster:
  `GET localhost:7070/api/alpha/agents`). The lab-manager/analyst seat
  is NOT frame-scoped; frame seats (f8-guide etc.) are minted per
  problem via `POST /api/alpha/frames/mint-seats` and are not yours to
  reuse.
- The operator is Joe. He rules on: frame registrations, budgets, design
  decisions, anything touching frozen artifacts or role cards. You own
  end-to-end: defect → packet → reviewed merge → next-cycle hypothesis.
- Substantial coding is belled to idle Codex lanes (CLAUDE.md protocol:
  small packets, `--from <your-id>`, park on every job id, review gate
  on every bellback — read the diff, run the gates YOURSELF, state what
  you checked). W.75/W.76 in the mission record are the template.

## 1. State of the machine (all verified, with receipts)

- **Frames 7 (a98A01) and 8 (a03J04) closed.** Both solved first-
  dispatch by the solver, axiom-clean, independently verified. f7
  disposed :defective-registration (ruling b — harness pin staleness,
  my preflight bug, P18 prevents recurrence); f8 closed clean.
- **Mathematics merged home**: apm-lean master `57c3a4e` (a03J04) on
  top of `d937780` (a98A01, t94J02, t00A05), pushed to
  github-holtz/apm-lean. The `zone` remote was unreachable at push
  time — retry `git push zone master` from ~/code/apm-lean when the
  Zone host is reachable. Merge-back on close is protocol.
- **Transfer checks (instrument v2)**: f7 = 3/6, f8 = 5/6 (prediction
  landed exactly). Lone f8 fail is C1 attribution; P21 arms at the f9
  restart → **registered prediction: f9 = 6/6**. Instrument:
  `analysis/transfer_checks.bb`; state dirs:
  `data/problem-state/<problem>-<hash>/v*.edn`.
- **The series**: `analysis/series.edn` — YOUR accumulator, append-only,
  one entry per frame, every claim receipted. Format is the contract.
- **First shared pattern node exists** (S-1, W.75):
  `math-formalization/layer-cake-crossover-split` carries the solver's
  route memory (e-d11811de) and the student's divergence (e-1484814c).
  Mission scope: 12 reviewed memories / 11 patterns / 1 shared.
  Whole store: 34 reviewed edges / 12 patterns, including a legacy
  22-memory hub on tactic-algebra-interference (claude-9 era, July 30 —
  EXCLUDE from mission-series scope) and 2 dangling :proposed
  attachments (monotone-approximation, split-into-cases — round-1
  debris, curation item).
- **Mission record**: `holes/missions/M-apm-demonstration.md`, through
  W.76. Every entry you write goes there, dated, with commit shas.

## 2. Armed but INERT until the f9 restart

The JVM has NOT been restarted since these merged (no hot-reload of
non-surface namespaces mid-mission — restart is an operator-visible
event). At the f9 restart these go live together:
- P21 seat attribution (flips C1 → predicted 6/6)
- P24 adjudicate-parks-at-:promote
- P25 subject-fallback retrieval (lone problem-id tag also queries the
  canonical subject)
- **Tags interface** (W.76, merge b89280ac): memory_record accepts
  optional caller tags (≤8 strings, system pair [:memory :memory/assert]
  protected first). Belt-and-suspenders with P25 on the f8 retrieval
  seam. When f9's scribe deposits, tags should include the problem id.

## 3. The Analyst build-out queue (plan §10 prerequisites, in order)

1. **Cascade use-receipts** — awaiting Joe's word as of this writing;
   the one build item all five cascade patterns license
   (`docs/cascade-formation-patterns.md`; cascades themselves:
   `~/code/futon3/library/cascades/*.flexiarg`, commit 2a49684). When
   Joe says go: small packet, receipts only (no declared edges, no
   derivation engine), modeled on P16/P17's receipt idiom.
2. Instruments as receipted invocations (convention only).
3. Close-hook wake topology (Analyst wakes on frame close, never
   mid-frame — the operator's quiet-machine requirement).
4. Read-surface residuals (projection gap, parked-listing under-report).
5. The Analyst card — drafted LAST, from stable practice, with duty F
   as revised (§11: dispatch-and-close).

Also queued: guide card v2.2 (:mission documentation, deposit-subjects
shape, dispatch-scribe :problem) for the f9 freeze window;
promote-memory-attachment!'s statusless refusal on :proposed
attachments (W.75 gate finding — the promote verb can't speak the
attach-then-review main path); outputs-side join for pull-OFFER
receipts (currently store-only, W.75); the two dangling :proposed
attachments need a review-or-retract pass.

## 4. Protocols that BITE (each learned the hard way)

- **Park on every dispatch.** Bell without park = protocol violation.
  `deadline-ms` is ABSOLUTE epoch-ms: `(( $(date +%s) + 2700 )) * 1000`.
  Capture job-ids into variables; NEVER put command substitutions in
  park payloads (they re-execute on wake). Park in a SECOND call after
  the dispatch returns the real job id — never a placeholder.
- **Reply-delivery contract**: read the incoming bell header. "delivered
  back automatically" → your turn text IS the reply, do NOT also bell.
  "bell/whistle with in-reply-to" → you MUST bell explicitly or your
  answer goes nowhere. No envelope → no return path; ask.
- **Roster before bell**: pipeline personas (codex, wm-full-loop) are
  not registered recipients; bells to them fail async as agent-not-found
  while POST returns accepted. Check job state ~5s after any bell that
  matters.
- **Frozen artifacts**: registration EDNs and frozen role cards (git
  blob hashes in the mission record) are never edited. Fixtures stage
  staffed copies.
- **Reviewer ≠ depositor; reviewer acts as themselves** (P14 enforces).
- **Merge from the main checkout, never inside the branch's own
  worktree** (the f7 "Already up to date" + killed-cwd incident).
- **Durable background work**: use `scripts/bg.py` (JVM-parented), never
  Bash `&`/nohup — pouches are LRU-evicted and reap children.
- **In-harness subagents are not Agency jobs** — no parks on them;
  persist findings to files as they land.

## 5. Substrate facts (port 7073, separate JVM — I-0 override)

- Projection: `POST /api/alpha/memory/projection`
  `{"endpoints":[...patterns...],"limit":N}` — batch ≤~20 endpoints,
  filter empty strings (invalid-memory-projection-endpoints). Edge
  truth: `[:edge :hx/props]` → `:attachment-status`, `:review`,
  `:review-history`, `:roles`.
- **Quiescence starvation**: under sustained writes the projection
  endpoint returns `memory-projection-source-moved-after-quiescence`
  for minutes (observed ~5 min, W.75; also claude-8's 503). Retry with
  patience; writes are idempotent on replay with the same evidence id.
- Direct fetch: `GET /api/alpha/evidence/<id>` (EDN body). Text search:
  `GET /api/alpha/evidence/text-search?q=...&limit=N` (EDN; scores) —
  this is how you find pull receipts by dispatch job id.
- Pull receipts: OFFER (`:memory-pull-offer`, includes empty results —
  denominator) and USE (`:memory-pull-use`), subject
  `{:ref/type :task :ref/id <dispatch job id>}`. Outputs do NOT join
  offers; query the store (transfer_checks.bb C4 shows how).
- Enumeration traps (memory index): entities endpoint caps at 5000, no
  cursor; :count semantics differ per endpoint.
- The saved problem-state cannot carry :evidence-store (live handle) —
  post-hoc joins must go through the substrate HTTP surface.

## 6. The S capture method (until it's a receipted instrument)

Enumerate the pattern universe from files
(`~/code/futon3/library/math-*/**.flexiarg` — 83 ids currently; the
multi_watcher syncs library→store, so files ARE the pattern registry),
batch-project, filter `attachment-status == "reviewed"`, count sharing.
Scope the mission series to mission-era memories (exclude the claude-9
legacy hub). W.67 (S-0) and W.75 (S-1) are the first two readings.

## 7. Registered hypotheses for f9 (adjudicate at close)

1. Transfer checks 6/6 (C1 flips via P21).
2. First tag-recall hit: a deposit tagged with the problem id is found
   by the student's `memory_search tags [<problem-id>]` (tags interface
   + P25, from opposite sides).
3. Student lane: with the 60-min budget (P19) and a stocked shelf,
   watch (c) of the loss function — uses co-occurring with commits.
4. S-2: does sharing grow without prompting (scribe attaches to
   existing patterns) or only via review pressure?

## 8. Session-mechanics notes for a Claude successor

- This workspace's persistent memory
  (`~/.claude/projects/-home-joe-code/memory/`) is shared across Claude
  sessions here — MEMORY.md has substrate trap notes worth keeping
  fresh. Update the succession pointer there when you take over.
- Mission docs and code are the real memory; write W-entries promptly,
  commit with shas, and keep the operator-visible buffer stated (job-id
  + park-id at dispatch time — operator visibility is part of the
  contract).
- Voice surface: open turns with a one-line `Gist:` (futon3c CLAUDE.md).
- The operator's standing style rulings: no artisanal dispatch;
  discovery and implementation are separate packets; VERIFY, don't
  derive (measure before deciding); results flow home (merge-back);
  honest reporting beats optimistic reporting every time.

— claude-7, 2026-08-17, end of session
