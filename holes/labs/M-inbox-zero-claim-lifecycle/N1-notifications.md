# N1 — notification routing repair for the sweeper dirty-notice lane (C8)

**Mission:** [M-inbox-zero-claim-lifecycle](../../missions/M-inbox-zero-claim-lifecycle.md), criterion C8 and
checkpoint e00730bf (Joe's live claude-1 example). Design only; no production
or runtime change in this packet. Independently implementable from the
unfinished commit-transaction work: this lane sends notices and never
commits, claims, or touches the witness store.

## 1. Source facts (traced, futon3c HEAD)

- `src/futon3c/inbox_zero/sweeper.clj:65-103` — dirty entries carry only
  path/status/mtime (untracked dir ⇒ newest descendant mtime).
- `sweeper.clj:155-179` (`default-windows`) — job-ledger windows (open-ended
  when unfinished) **plus** a synthetic `now-30min→now` window for every
  roster agent reported `invoking`. Windows carry no session id, repo, or
  mission.
- `sweeper.clj:190-195` (`live-candidates`) — mtime ∈ window ⇒ candidate.
  `sweeper.clj:197-223` (`attribute`) — entry assigned to **every** live
  candidate, tagged `:shared-with`; entries with **no** candidate are dropped
  from attribution entirely.
- `sweeper.clj:225-256` (`recipients`, `notice-prompt`) — ranks by overlap
  count, emits Joe's exact "were written during your turns. Commit or
  delete…" wording.
- `sweeper.clj:392-393` — recipient session = **current** roster session; a
  window from an older job routes to the seat's newer session (rollover
  misroute).
- Backlog (`write-backlog!`, sweeper.clj:322-345) is written **only** from
  the `(empty? targets)` branch (sweeper.clj:397-408): repos where *no* entry
  overlapped any live window. **Confirmed omission:** in a mixed repo, entries
  with no overlap vanish from every surface — no recipient entries, no
  backlog row.
- Dedupe: followup `:dedupe-key` (sweeper.clj:259-262, total count ÷
  threshold bucket) and the notices ledger `due?` (sweeper.clj:305-311):
  count-growth or `renotify-ms` staleness. Content changes with unchanged
  count renotify on staleness; mtime churn on the same set does not renotify
  early (good), but a *recipient-set* change with same count also does not
  renotify (a file flips apparent "owner" silently).
- Consumers: `operator-backlog.edn` has **no programmatic reader** in
  src/dev/scripts/docs — it is an operator-read file (discoverability is a
  docs obligation, C7). Followups are delivered via
  `/api/alpha/followups` (transport/http.clj:9342-9353).
- Wording consumer: `scripts/xiang2000_p6o3.py:28` `INBOX` regex
  `fullmatch`es the exact current template; `scripts/test_xiang2000_p6o3.py:8`
  pins a sample. It is an append-only historical interpreter: old notices in
  history must stay classifiable, so the rule must accept **both** templates.
- Tests preserving current behavior: `test/futon3c/inbox_zero/sweeper_test.clj`
  overlap attribution + duplication (145-184 per owner trace), backlog tests
  at 96-135, dedupe at 75-95.

## 2. Evidence categories (routing input)

| Cat | Evidence | Weight |
|---|---|---|
| E1 | Exact-session edit witness (claim `:witness/id` tool_use id) | authorship-grade |
| E2 | Confirmed attribution record (futon3 confirm.clj path) | authorship-grade |
| E3 | Declared triage responsibility (escalation routing decision naming a recipient for the repo) | routes *work*, never authorship |
| E4 | Temporal overlap: job-ledger window with matching **session id** | diagnostic |
| E5 | Temporal overlap: job window without session id / synthetic invoking window | weakest diagnostic |
| E6 | mtime only | none |

The sweeper currently uses only E4/E5/E6 and presents them as authorship.
Job records in the ledger do carry session identity upstream
(`agency/inbox.clj:25-40` persists job payloads); `default-windows` must
propagate `:session-id` when present so E4 can be distinguished from E5.
Triage authority: the escalation ledger (`turn_promotion.clj` route-fn
decisions) names per-repo responsibility; that is safe existing triage
authority to *route a bounded honest task* — it does not prove who wrote
bytes and the notice must say so.

## 3. Routing rule (the repair)

Per dirty entry, per repo:
1. **E1/E2 present** ⇒ eligible for a *personal* notice to that exact
   session: "these N files carry edit witnesses from your current session;
   commit or leave them; anything not yours, leave alone" (keep the existing
   exemption wording; cite evidence type).
2. **E3 present for the repo** and no E1/E2 ⇒ one *triage task* notice:
   "you hold triage for \<label\>: M dirty files, K with no authorship
   evidence; route or escalate — authorship unknown." Bounded: one recipient,
   dedupe like today.
3. **E4/E5/E6 only** ⇒ **no personal cleanup assignment**. Entries go to the
   operator backlog with their diagnostic overlap list attached as labeled
   diagnostic (`:overlap-diagnostic`, agents + window kind), never as
   `:shared-with` authorship.
4. **Backlog completeness fix:** backlog rows are emitted per repo whenever
   uncertain entries exist, **including mixed repos** where other entries
   have E1/E2/E3 recipients (today's `empty? targets` gate is the bug).
   Row: label, total, per-category counts, newest uncertain paths.
5. **Session rollover:** E1/E2/E4 route only when the evidence session ==
   current roster session; otherwise the entry degrades to E6 (backlog),
   never to the seat's new session.
6. **Dedupe:** extend the ledger key to `[label agent category paths-hash]`
   where paths-hash = sha of sorted covered paths; count-bucket and
   renotify staleness unchanged. Recipient-set flips and mtime churn
   neither spam nor silently reroute: a flip produces a new key (one
   notice), churn alone changes no key.

## 4. Joe's example, rerouted

Repo futon3c-d, 10 dirty, 5 named to claude-1 today. Under N1, assuming no
E1/E2 claims exist for the dirty bytes (commit `727518e6` explains the
s0 script's *history*, not its current dirty content — not authorship
evidence for the bytes):

- **claude-1: no notice.** All five overlaps are E4/E5 diagnostics.
- Backlog gains one row: `futon3c-d — 10 dirty (3 untracked); 0
  witness-attributed; 10 uncertain`, newest first:
  `holes/labs/M-inbox-zero-claim-lifecycle/s0-git-transaction.py`,
  `scripts/test_xiang2000_p0_origins.py`,
  `holes/labs/M-象-2000/p0-expected.edn`, `scripts/xiang2000_p0.py`,
  `emacs/session-mode.el`, each with
  `:overlap-diagnostic [codex-4(E5) kimi-9(E4/E5) 象(E5)]` — labeled
  diagnostic, not assignment. Actual owners remain **unknown** unless
  independently established; nothing here routes to kimi-9 either, because
  E4/E5 cannot carry authorship.
- If an E3 triage decision exists for futon3c-d, its holder gets the single
  bounded triage notice instead of silence.

## 5. Consumer compatibility and tests

- `xiang2000_p6o3.py:28`: add `INBOX_V2` alternative in the same `INBOX`
  rule (old template OR new), same classification `'inbox-zero'`; update
  `test_xiang2000_p6o3.py` with old-sample (still matches — historical
  replay safety) and new-sample tests. New template keeps the
  `inbox-zero: <label> is carrying N dirty file(s)…` prefix so downstream
  prefix tooling is stable.
- `sweeper_test.clj`: revise 145-184 overlap tests to assert
  overlap-only ⇒ backlog with `:overlap-diagnostic`, **no** personal notice;
  add mixed-repo backlog test (one E1 path + one uncertain path ⇒ recipient
  notice for the first AND a backlog row for the second); session-rollover
  test (window session ≠ roster session ⇒ backlog, no delivery); dedupe
  tests for the new key (flip ⇒ one new notice; churn ⇒ none); triage test
  (E3 decision ⇒ one bounded task notice, wording contains "authorship
  unknown", never "written during your turns").
- Prefer structured evidence: followup `:metadata` gains
  `:evidence/category`, `:evidence/session-id`, `:uncertain-count` so future
  consumers classify on fields, not regex.

## 6. Acceptance matrix (C8)

| Case | Behavior | Test |
|---|---|---|
| Unrelated simultaneous agents | no personal cleanup assignment; overlap is backlog diagnostic only | revised overlap tests |
| Declared triage responsibility | one bounded honest task notice; wording denies authorship | new triage test |
| Uncertain files visible | backlog row incl. mixed repos; operator file remains the consumed surface | mixed-repo backlog test |
| Session rollover | evidence session ≠ current ⇒ degrade to backlog | rollover test |
| Dedupe | flip ⇒ one notice; churn ⇒ none; count-growth/renotify unchanged | dedupe tests |
| Mixed known/unknown repo | recipient notice for known + backlog row for unknown | mixed-repo test |

## 7. Smallest implementation packet (after owner authorization)

One packet, futon3c only: `sweeper.clj` (windows carry `:session-id`;
`attribute` → category classifier; `notice-prompt` split into
personal/triage templates; backlog gate moved from `empty? targets` to
`seq uncertain`; dedupe key extension; metadata fields),
`sweeper_test.clj` (above), `scripts/xiang2000_p6o3.py` + its test (regex
union). Checks: `clj-kondo`, `futon4/dev/check-parens.el`,
`clojure -M:test -n futon3c.inbox-zero.sweeper-test`,
`python3 -m unittest scripts.test_xiang2000_p6o3` (from repo root),
py_compile both scripts. No Agency writes, ledger changes, or sweeper
ticks during implementation review.

## 8. Evidence → route → surface wiring (brief)

```
git status (mtime) ─┐
job ledger (session) ─┼─► classifier (E1..E6) ─┬─ E1/E2 ─► personal notice ─► followups
confirm records ────┤                        ├─ E3 ────► triage notice ───► followups
escalation ledger ──┘                        └─ E4-6 ──► backlog row ─────► operator-backlog.edn
claims (witness ids) ───────────────────────────┘            (diagnostics attached)
```

**Unresolved blocker (precise):** E1 depends on the witness/claim work that
is still DERIVE-pending, so at N1 implementation time most entries will
classify E4-E6 and the lane's personal notices will nearly vanish in favor
of backlog + triage. That is the honest interim state, not a defect — but
if the owner judges backlog-only operation unacceptable, the blocker is
exactly the missing claim-authorization evidence, and N1 should ship
wording/routing first with E1 wiring present but usually empty.
