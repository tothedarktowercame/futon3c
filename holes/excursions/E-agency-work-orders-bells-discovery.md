# E-agency-work-orders step 1 discovery: bells as turns

**VERDICT (2026-10-09, provisional):** DONE — Discovery-only deliverable complete: every claim marked [checked]/[inferred], hook points, volume and cost findings all written; recent. _(WM status classification by zai-1, high confidence; not yet confirmed by the author.)_

Author: kimi-1, 2026-10-05, for claude-17 (requisition bell invoke-1791183050805-32677-0bf5f12b).
Discovery only. Each claim is marked **[checked: `<command>`]** or **[inferred — basis]**.

Terminology: a "bell" or "whistle" becomes an *invoke job* in the Agency
invoke-jobs ledger. "Turn" means a 象 ledger record
(`futon3c/src/futon3c/xiang/turn_service.clj` `record-turn!`); today only
operator REPL turns are recorded there — no invoke job is.

## 1. Hook points (futon3c/src/futon3c/transport/http.clj)

All three surfaces funnel into the same two functions: `create-invoke-job!`
(open) and `finalize-invoke-job!` (close). Those are the natural places to
open/close a 象 port.

### Bell (async) — accepted
`handle-bell`, http.clj:6061 (`POST /api/alpha/bell`).
**[checked: `sed -n '6061,6160p' src/futon3c/transport/http.clj`]**
Fields parsed at acceptance: `agent-id` (recipient), `prompt`, `caller`
(default "http-caller"), `surface` (default "bell"), requested `job-id`,
`mode` (work|brief, else inferred from prompt), `mission-id`,
`in-reply-to` / `reply-to` (6061+~30; comment: "a bell that answers another
bell carries its id"), typed-bell `type`/`ref` (behind
`typed-bells-enabled?`, default OFF, http.clj:1238), `warrants`, `timeout-ms`,
`model`, `reasoning-effort`. Job is created by `create-invoke-job!`
(http.clj:1963) → `create-invoke-job-ledger!` (http.clj:1883), which stores
`:request-commission {:agent-id :caller :prompt :surface}`, `:request-digest`,
`:mode`, `:bellback-of`, `:session-id nil` (not known yet), and event
`accepted`. **The recipient's session id is NOT known at acceptance** — it is
stamped at finalize. **[checked: `sed -n '1883,1962p'`]**

### Whistle (sync) — accepted
`handle-whistle`, http.clj:6867 (`POST /api/alpha/whistle`); delegates to
`handle-whistle-stream*` (http.clj:6705) when `stream=true`; also
`handle-whistle-stream` (http.clj:6858, `POST /api/alpha/whistle-stream`).
Fields parsed: `agent-id`, `prompt`, `timeout-ms`, `caller`, `warrants`,
stream flag. It uses "the canonical supervised invoke-job engine" — same
ledger, same finalize; at the strict response timeout the caller gets a
pollable overrun job instead of a lost one (docstring, 6867-6872).
**[checked: `sed -n '6858,6900p'`]**

### Job end (all three)
`finalize-invoke-job!`, http.clj:2239, called with
`[job-id terminal-state terminal-code terminal-message result sid]` from
`run-invoke-job!` (http.clj:5621, finalize call at 5460), the ack path
(`handle-ack-invoke-job`, 7090+), and the ceiling reaper (2640). Terminal
states: done / cancelled / timeout / failed (`finalizer-written-states`,
http.clj:389). Fields available here: the full `result` map (result text,
`:invoke-meta`, `:usage`, `:total-cost-usd`), `sid` = the recipient's session
id (stored as `:session-id`), derived `:result-summary`, `:result-text`
(bounded to 8000 chars, whitespace preserved, http.clj:2248-2252),
`:artifact-ref`, `:trace-id`. **[checked: `sed -n '2239,2360p'`]**

### Auto-bellback — created, not accepted
No endpoint. At finalize, `auto-bellback-request` (http.clj:1392) decides
(`should-auto-bellback?`, http.clj:1316: recipient type in
`auto-bellback-recipient-types` (1280), caller registered and not
"auto-bellback", no prior `:auto-bellback`, no suppressing park); the ledger
job is stamped `:auto-bellback {:sent? true :bell-job-id "auto-bellback-<id>"
:at ...}` (http.clj:~2336) and `enqueue-auto-bellback!` (http.clj:1429)
creates the reply job with `caller/surface "auto-bellback"` and
`:bellback-of <original-job-id>` (bell-router, default ON, http.clj:1220).
The bellback prompt carries the result text: "🔔 RE: your bell — job `<id>` to
<agent> finished …" plus the 8000-char `:result-text`
(`auto-bellback-prompt`, http.clj:1354-1377). Delivery is recorded by
`record-bell-completion-delivery!` (http.clj:2451).
**[checked: `sed -n '1213-1500p'` and `2239,2360p'`]**

Ledger endpoint: `handle-invoke-jobs`, http.clj:7067 — only param is
`limit` (default 20). **[checked: `sed -n '7067,7078p'`]**

Retention caveat for any reader: terminal jobs are compacted after 1 day
(`invoke-terminal-detail-retention-ms`, http.clj:479) and tombstoned after 7
(http.clj:485); compaction keeps `:request-commission` (prompt survives) but
drops live events. The durable copy is `/tmp/futon3c-invoke-jobs.edn`
(`invoke-jobs-store-path`, http.clj:464-467, env `FUTON3C_INVOKE_JOBS_FILE`).
**[checked: `sed -n '464,522p'`; compaction key list at 518-521]**

## 2. Volume, 2026-10-02..04

Source: the durable ledger file (the HTTP endpoint only returns ~the last
1000 jobs ≈ 18 h). **[checked: `clojure -M -i /tmp/kimi-jobstats6.clj -e nil`
parsing `/tmp/futon3c-invoke-jobs.edn` (65 MB, 6860 jobs); scripts copied the
queries shown below]**

Per-day counts of invoke jobs with surface bell/whistle/auto-bellback:

| day        | bells+whistles+auto-bellbacks | work | brief | whistles | auto-bellbacks | median prompt chars |
|------------|------------------------------|------|-------|----------|----------------|---------------------|
| 2026-10-02 | 614 | 435 | 179 | 2 | 141 | 3790 |
| 2026-10-03 | 682 | 574 | 108 | 2 | 81  | 8916 |
| 2026-10-04 | 918 | 445 | 473 | 0 | 385 | 985  |

Whistles are nearly unused (2, 2, 0). Auto-bellbacks are 23%/12%/42% of the
day's jobs.

Caller-registration split (bells only, excluding auto-bellback):
**[checked: `/tmp/kimi-reg.clj`, registered set = `curl
http://localhost:7070/api/alpha/agents` → 63 agent ids]**

| day        | bells | from callers registered *today* | unregistered/system callers |
|------------|-------|--------------------------------|------------------------------|
| 2026-10-02 | 471 | 188 | turn-capture 275, wm-flight 6, wm-cascade-revision 2 |
| 2026-10-03 | 599 | 94  | turn-capture 499, wm-flight 6 |
| 2026-10-04 | 533 | 523 | wm-flight 5, turn-capture 5 |

[inferred — basis: the registry is the *current* one; a caller that
de-registered since would read as unregistered. turn-capture and wm-* are
system callers that never register, so those rows are reliable.]

Prompt length: median over all bell/whistle/auto-bellback prompts per day in
the table above. Excluding system callers and auto-bellbacks, genuine
agent-to-agent bells: 196 (10-02), 100 (10-03), 528 (10-04), median
1628/1898/1463 chars, total chars 410k/211k/1143k.
**[checked: `/tmp/kimi-cost4.clj`]** The 10-03 median of 8916 overall is
inflated by 499 turn-capture analysis jobs (median prompt ≈ 8916 chars).

Auto-bellback count: 141 / 81 / 385 (table, column "auto-bellbacks").

## 3. Linking fields, and one traced chain

Available join keys:

- **`:bellback-of`** on the auto-bellback job → original job id (bell-router,
  http.clj:1402). Reliable, machine-written.
- **`:auto-bellback {:bell-job-id}`** on the original job → the bellback job
  id. So the close-edge is stored in *both* directions.
  **[checked: chain trace below]**
- **`in-reply-to` / `reply-to`** accepted by `handle-bell` (http.clj:~6090)
  for agent-authored replies. [inferred — basis: present in the parser, but
  nil on every job in the traced chain; agents today "just respond" in-thread
  rather than sending a reply bell, so it is rarely populated.]
- **`Agency-Job` / `Dispatched-By` commit trailers**, written by
  `scripts/git-hooks/prepare-commit-msg` (lines 10, 33) from
  `~/.futon/agent-context/<agent>.json`. Links a commit → the job that
  commissioned it → the caller. **[checked: `cat
  scripts/git-hooks/prepare-commit-msg`, `cat
  ~/.futon/agent-context/codex-23.json`]**
- **Parks** (`futon3c/src/futon3c/agency/parked_on.clj`): a parked record's
  `:awaiting` set holds dep job ids (line 431, `index-add` at 317);
  `parked-on-notify!` runs at finalize (http.clj:~2303) and a suppressing
  park is recorded on the job as `:auto-bellback {:suppressed? true :park-id
  ...}` (http.clj:1296-1311). So a park record names the job(s) it waits on.
- **`request-digest`** (content digest of the commission) — dedup, not a
  thread link.

Worked example: claude-4 → codex-23 c1-capture packet, 2026-10-04
(packet text verified identical to `/tmp/pyreg/overnight/psw2/c1-capture.md` /
`.sent.md`). **[checked: `diff`-equivalent eyeball of the file vs.
`:request-commission :prompt`; `/tmp/kimi-chain4.clj`]**

1. `invoke-1791152410965-32287-c9db2bca` — claude-4 → codex-23, created
   2026-10-04T22:20:10.96Z, surface `bell`, mode `work`, state `done`
   (finished 22:23:03). Recipient session `01a10771-aad6-7cb1-b417-c77aae5b92ae`
   stamped at finalize. Links present: `:auto-bellback {:sent? true
   :bell-job-id auto-bellback-invoke-1791152410965-32287-c9db2bca}`.
   Links missing: `bellback-of` nil (correct — it opens the thread),
   `in-reply-to` nil, and `:artifact-ref` is a bg-process id
   (`1791152580886`), not the commit.
2. Commit `bd019496` ("Add observation-only elaborated term capture") in
   `/tmp/mfs-pool-1` carries trailers `Agency-Job:
   invoke-1791152410965-32287-c9db2bca`, `Dispatched-By:
   claude-4/9ef8e479-e1b7-4755-894d-1264cacb6c3b`, `Agent-Id: codex-23`.
   **[checked: `git -C /tmp/mfs-pool-1 log --format=%(trailers) -1 bd019496`]**
3. `auto-bellback-invoke-1791152410965-32287-c9db2bca` — auto-bellback →
   claude-4, created 22:23:07, done, `:bellback-of
   invoke-1791152410965-32287-c9db2bca`; its result-summary carries the
   codex-23 reply "㊢ (codex-23, capture) … committed tools/term_capture.py
   (`bd019496`) …".

So the open→close pair is fully linked by job id in both directions, and the
commit links back to the job by trailer. Missing in the wider thread: the
follow-up bells in the same conversation (three "From claude-4: your
bg-… finished" bells, e.g. `invoke-1791152985763-32307-f16160b9`) are NEW
thread-opening bells with no `in-reply-to`, and the final report bell
(`invoke-1791169842363-32516-89a0c5c4`, 2026-10-05T03:10) likewise has
`bellback-of` nil — the thread beyond one request/answer pair exists only in
the prompt texts. A 象 turn adapter would have to treat every non-reply bell
as opening a new port unless the caller passes `in-reply-to`.

## 4. Cost of reading

Unit cost of a 象 reading: the caller `turn-capture` analysis jobs in the
same ledger — 1019 jobs since 2026-10-02 (~340/day), median prompt 8916
chars, states done 1009 / failed 10.
**[checked: `/tmp/kimi-cost2.clj`, `/tmp/kimi-cost4.clj`]**
Token/USD price per reading is **not recorded**: `:usage` and
`:total-cost-usd` are nil on all turn-capture jobs; in the whole window only
495/2741 jobs (claude-4/codex callers) carry any usage, and those are
session-level token totals, not per-prompt, so they cannot calibrate a
per-1000-char price. **[checked: `/tmp/kimi-cost3.clj`]** A per-1k-char
dollar figure is therefore [inferred — not computable from the ledger today].

What can be computed in reading units: one 象 reading handles ~9k chars.
Reading every bell, whistle and auto-bellback of 2026-10-04 (918 jobs,
~1.6M prompt chars counting bellback bodies) would be ~180 reading-equivalents
per day, i.e. adding ~50% on top of the ~340 readings/day turn-capture
already runs — but ~900 extra LLM calls/day, one per job. [inferred — basis:
char totals from `/tmp/kimi-cost4.clj`; calls-per-job assumed 1 as for
turn-capture.]

Assessment: affordable in characters (bell prompts are short: median ~1.5k),
but wasteful in structure. Bells are machine-routed messages with all acts
already explicit in fields — caller, recipient, open at `accepted`, answer at
`auto-bellback`, carry-out via `Agency-Job` trailer — and 23-42% of the volume
is the auto-bellback template, which a classical reader recognizes by
`surface="auto-bellback"` with zero tokens. Recommendation: classical reading
of marked bells first (emit acts from the ledger fields; 大象 only for the
free-text prompt bodies of agent-authored work bells, ~100-530/day), rather
than 象-reading every job. This also matches the excursion's goal: the
open/close port semantics are already exact in `create-invoke-job-ledger!` /
`finalize-invoke-job!`; an LLM would only be re-deriving them.

### What it would take (synthesis of 1-4)

- Open a port at `create-invoke-job-ledger!` (http.clj:1883): turn id =
  job id, record = {:prompt :caller :agent-id :mode :in-reply-to}.
- Close it at `finalize-invoke-job!` (http.clj:2239): terminal state +
  `:result-text` (already bounded to 8000) + recipient `:session-id`; the
  auto-bellback edge is already materialized there in both directions.
- Thread continuity beyond one pair needs either agents passing
  `in-reply-to` (supported at 6090, unused) or grouping by
  `(caller, agent-id)` session — the codex-23 chain shows all follow-ups
  share one recipient session id.
- Commits join via the `Agency-Job` trailer (verified on bd019496).

*Commands/scripts cited: /tmp/kimi-jobstats{,2,3,4,5,6}.clj,
/tmp/kimi-chain{,2,3,4}.clj, /tmp/kimi-cost{,2,3,4}.clj, /tmp/kimi-reg.clj —
all read-only against /tmp/futon3c-invoke-jobs.edn, run with
`cd /home/joe/code/futon3c && clojure -M -i <script> -e nil`.*
