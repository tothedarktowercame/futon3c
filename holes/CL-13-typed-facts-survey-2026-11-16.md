# CL-13 — two remaining prose-to-typed-fact reads

Survey only, 2026-11-16 UTC. No production data or source was changed. Live
counts came from one bounded turn-store census, one Agency job-list read, and
the existing workplan census. Examples below are invented shapes; no operator,
memory, or mission body is reproduced.

## Result and order

Do supersession first. It is the smaller closure: the GCC type already exists,
the file census already notices precisely the relevant edits, and only the
inventory publisher and 猴 reader need to change. Job failure typing crosses
provider adapters, Agency's durable job/public view, both JVM and Emacs reapers,
and legacy-job compatibility.

## 1. 象 analysis-job failure typing

### The prose read and all decisions made from it

`src/futon3c/xiang/turn_record.clj:1251-1262` reduces a job to the last event's
text/message/error/reason (then terminal message/state). `job-outcome` at
`:1264-1294` types only the broad outcome. The two narrower facts are then
recovered by regex: quota at `:1296-1299`, store busy at `:1301-1304`.

The only Clojure decision site is `src/futon3c/xiang/turn_service.clj`:

- `:1660-1670` reads the public Agency job and broad outcome;
- `:1691-1699`, on quota, benches the failing seat, chooses an unbenched
  alternate, resets the turn, and redispatches it; the result is `:benched`;
- `:1702-1714`, on store busy, keeps the seat and redispatches after the next
  bounded delay (defaults are 60, 180, 600 seconds at `:79`); the result is
  `:store-busy`;
- `:1716-1722`, for every other terminal failure/refusal, stores failed/refused
  plus `{state, reason}`, marks the attempt done, and makes analysis health
  failing. Unreachable and still-running are handled separately at
  `:1671-1682`.

The Emacs implementation duplicates both semantic decisions.
`emacs/session-turn-analysis.el:862-864` recognizes quota prose, and
`:1372-1390` benches/fails over. `:1331-1336` recognizes store-busy prose, and
`:1391-1404` schedules the bounded same-seat retry. The final failure branch at
`:1405` marks analysis done/failing and warns. It therefore must stay in step
until it is retired or changed to consume the same typed field; changing only
the JVM would leave behavior surface-dependent.

### Where the strings and types originate

Agency already has a typed terminal channel, but it is too coarse for the
quota policy. `src/futon3c/transport/http.clj:2126-2148` converts a runner
result's `:error/code` to `terminal-code` and its `:error/message` to
`terminal-message`. `finalize-invoke-job!` persists both and adds the same code
and message to the terminal event (`:2587-2638`); the public/compact view keeps
them (`:541-556`). Thus most runner typing survives, but an adapter that returns
generic `:invoke-error` with provider quota prose has already flattened the
distinction before 象 sees it.

The store-busy source is already fully typed. Clock admission throws ex-info
with `:error/code :clock/store-busy` at
`src/futon3c/agency/clock_decision.clj:72-78`; the HTTP ingress preserves it as
a 503 body and `Retry-After` at `transport/http.clj:13985-13991`. Other store
read/write causes are typed before that too (`clock_decision.clj:47-60`). In
other words, store-busy regex is reparsing a fact that existed upstream.

Provider quota is mixed. HTTP 429 is intrinsically typed at the provider
boundary, and runner error maps can carry `:error/code`; however the generic
Agency classifier preserves whatever adapter code it receives and does not
normalize a provider quota class. The historical guard in
`src/futon3c/apm/live_job_driver.clj:687-706` confirms the loss: it recognizes
quota only when `terminal-code` is `:invoke-error`, by parsing
`terminal-message` again.

### Seven-day live census

At the read time, the last seven days contained 845 turn records with an
`analysis_dispatch.job_id`. A single `GET /api/alpha/invoke/jobs?limit=20000`
returned 6,527 retained jobs (2026-10-20 through 2026-11-16) and contained all
845 ids. Of these analysis jobs, 839 were `done` and 6 `failed`:

| Terminal code | Count | Reason shape |
|---|---:|---|
| `futon1b-read-rejected` | 2 | typed terminal code, explanatory prose |
| `store-busy` | 1 | typed terminal code; message also matches store-busy prose |
| `invoke-error` | 3 | generic typed code, other explanatory prose |

No retained failure in this window matched the quota/rate-limit phrases. The
full observed terminal-code set for failed 象 jobs is therefore exactly those
three. Invented shapes are `{state: "failed", terminal-code:
"store-busy", terminal-message: "<admission detail>"}` and `{state:
"failed", terminal-code: "invoke-error", terminal-message: "<provider
detail>"}`. The turn records themselves ended 841 analyzed and 4 failed; that
difference is expected because a turn records only its current dispatch while
the job census includes failed attempts that were retried to a successful
analysis.

### Replacement

Use the durable job outcome as the typed record; no new GCC clusion is needed
for a per-job terminal receipt. Add a public/persisted `failure-kind` (or make
`terminal-code` carry the same controlled vocabulary) with at least
`:provider/quota`, `:clock/store-busy`, `:evidence/read-rejected`, and
`:invoke/failed`. The adapter nearest the fact sets it: provider adapters map
structured 429/quota responses before producing their result; clock admission
copies the existing ex-data code; the evidence backend copies its existing
read refusal code; Agency finalization merely preserves it. An invented record
shape is `{state: "failed", failure-kind: "provider/quota", terminal-code:
"invoke-error", terminal-message: "<human detail>"}`.

`turn_record/job-outcome` should return `:failure-kind`; `turn_service/reap!`
should branch on exact keywords, never message text, and persist the kind in
`analysis_dispatch.outcome`. For old retained jobs only, a compatibility
projection may classify their prose once at the job-reader boundary and mark
the result `legacy/inferred`; the operational reaper itself should not regex.
The Emacs reaper must either read this same field from a structured reaper
response or cede reaping to the JVM. Keeping two independent regex lists is
not a viable typed interface.

## 2. 猴 supersession

### Current prose read and census

`src/futon3c/circus/operator_focus.clj:20-31` treats a census item whose status
starts `SUPERSEDED` as eligible, slurps the entire document, and extracts
`superseded by <item-id>`. `resolve-tag` at `:33-48` follows at most eight such
links and also regexes the status line as a fallback. `focus-window` invokes it
only for a settled turn with no stored named items and a typed `mission_id`
(`:50-58`).

The current inventory has 711 items. Fourteen status lines start
`SUPERSEDED`; the exact parser used by `with-successors` finds exactly one
successor for 3, none for 11, and multiple successors for 0. (Four additional
status lines contain the word elsewhere but are not eligible under the current
leading-status rule.) The report intentionally names no mission body.

### What already exists

The item census already supplies id, path, source digest, status line and
state, and invalidates its cache by path/mtime/size:
`src/futon3c/arxana/item_inventory.clj:21-54`. Its publisher gives every item
a stable `[:workplan/item 1]` clusion (`:17-19`) and an exact, verified
file-census attestation pinned by path, Git commit and source sha256
(`:68-81`). `turn_mention_writer` and the other GCC readers use that same item
clusion; no second item identity is needed.

The required relation is also already declared. `[:supersedes 1]` has roles
`:successor` and `:predecessor`, both clusion endpoints, empty identity
content, and an acyclic constraint at
`src/futon3c/arxana/type_registry.clj:163-166`. The F1 example constructs the
same direction at `src/futon3c/arxana/formal_f1.clj:139-148`. No production
inventory writer currently asserts mission supersession, and the item record
itself contains only `:item/id`; neither inventory nor GCC currently carries
the relation for these files.

### Replacement

Reuse `[:supersedes 1]`, not a new workplan-specific type:

```clojure
{:type-revision [:supersedes 1]
 :endpoints [{:role :successor :kind :clusion
              :target {:clusion/id <successor-workplan-item-clusion>}}
             {:role :predecessor :kind :clusion
              :target {:clusion/id <predecessor-workplan-item-clusion>}}]
 :asserted-content {}}
```

The item-inventory scan is the natural writer because it already sees the
status line and file stamp change. It may retain the existing file regex as a
single ingestion parser, but should emit one relation only for an unambiguous
declaration whose two item clusions exist. Its attestation should be producer
`futon3c/item-inventory`, basis `:exact`, standing `:verified`, with method
`{:name :supersession-census :version 1}` and source refs carrying path, Git
commit and file sha256. Zero/multiple/unknown successor cases should yield a
typed publication outcome/receipt, not an invented edge.

猴 then reverse-reads `[:supersedes 1]` by `:predecessor`, verifies the exact
attestation/source pin, and follows the returned successor clusion. Empty,
ambiguous, cyclic and overlong results retain today's typed notices. The
operator-focus path no longer reads mission files or parses status prose; only
the inventory writer does that translation once.

## Recommended sequence

1. Publish and read existing `[:supersedes 1]` from the inventory tick; backfill
   the 3 currently unambiguous links and retain typed receipts for the other 11.
2. Add the Agency job `failure-kind` contract at runner boundaries and durable
   finalization; switch JVM and Emacs consumers together, with an explicitly
   marked legacy projection for old jobs.

