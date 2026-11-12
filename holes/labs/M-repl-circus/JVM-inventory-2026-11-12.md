# JVM inventory, Zone, 2026-11-12

For M-repl-circus, "Two JVMs" (Joe, 2026-11-12: every service as routes and
scheduled tasks in the futon3c JVM, every store's data in futon1b). Requested
by claude-13 (bell `invoke-1794445124806-17745-b20d04b3`), written by claude-6.
Read-only discovery, 00:50–01:05Z: code, launch scripts, unit files, the
processes' own logs, and one `GET /arxana/v1/capabilities` per demo port.
Nothing was signalled, restarted or edited. minus40's store (:7093) is out of
scope here (kimi-2's MEM-migration discovery).

Ten JVMs are covered: giraffe-reread and nine Arxana demo processes (:7084,
F1–F6, code↔docs, workplan). The packet counted these as eight; its own list
comes to ten.

## What the nine Arxana processes have in common

All nine run from the canonical futon3c checkout (`cwd` /home/joe/code/futon3c,
same classpath basis as each other) and serve the same two routes from
`futon3c.arxana.http`:

- `POST /arxana/v1`: one arxana.v1 envelope in, one out (`service/dispatch`);
- `GET /arxana/v1/capabilities`.

They differ only in the service configuration built at start: which store,
which fixture is loaded, and so which operations have data behind them. All
bind 127.0.0.1 only. No Caddy block, script or Clojure source outside
`futon3c.arxana` names ports 7084–7091; the only clients found are the futon4a
Emacs packages, one `defcustom` URL per demo.

F1–F6, code↔docs and workplan share one construction
(`formal-f1-demo/start!`), which starts three things inside the process:

1. an XTDB 2 node (in-memory, except workplan);
2. an embedded futon1b HTTP server on a random loopback port, with an Arxana
   SQLite index sidecar in a temp file;
3. the Arxana surface on the demo port, whose graph store is
   `store-http/http-store` pointed at (2), and whose artifact provider is
   `artifact/in-memory-provider`.

So each of those eight JVMs holds a private futon1b. That is where the memory
goes: 0.8–1.6 GB resident each, about 9 GB for the eight.

One fact that bears on every fold below: the live futon1b on :7073 already
registers `/api/alpha/arxana/command` and `/api/alpha/arxana/query`
(`futon1b_server.clj:1719`; a GET returns 405 there and 404 on an unknown
path). Whether its Arxana index sidecar is enabled on the live store was not
confirmed.

## 1. giraffe-reread (timer, a fresh JVM every 5 minutes)

- **Serves:** nothing. `futon3c.giraffe.reread-cli` runs one cycle and
  exits: rebuild the census of open missions, excursions and tickets;
  reconcile it against the re-read queue; hand at most a capped number of
  readings to the reader seats named in its environment file (through Agency
  on :7070, or `scripts/kimi-task.sh` for Kimi seats); batch a mint.
- **Recent use:** continuous. The journal shows a run starting every 5
  minutes on 11-12, each taking 1 to 3 minutes of wall time and 2 to 12
  minutes of CPU (00:37Z run: 11 min 45 s CPU). A run was in flight when read.
- **Data:** files, all durable. Queue: SQLite at
  `~/.local/state/futon3c/giraffe-reread/requests.sqlite`. Mint batching
  state: `mint-batch.edn` beside it. Readings corpus and mint output:
  the corpus pointer directory and `/home/joe/code/storage/giraffe`. No XTDB.
- **Why a separate JVM:** none stated. It was written as a CLI with systemd
  units (`d8d5b2a7f`, 11-06). The unit files still carry the header "NOT
  INSTALLED OR STARTED … a separate operator decision after corpus
  migration", but the timer is installed and active.
- **Owner:** M-giraffe. Code by codex-7, dispatched by claude-6 (commit
  trailers).
- **Fold:** a scheduled task in the futon3c JVM, as
  `futon3c.watcher.multi` does with `scheduleWithFixedDelay`: call
  `reread-cli/run-once!` with the same options. The queue can stay in SQLite
  or move to futon1b; nothing forces that for the fold.
- **Blocks:** (a) configuration comes from an `EnvironmentFile`; the JVM
  needs those four values some other way. (b) Each cycle is minutes of CPU
  (census, a Python checker over the corpus) and blocks while polling reader
  jobs; inside the serving JVM it needs its own single-thread executor and
  should not share the HTTP pool. (c) `scheduleWithFixedDelay` gives
  no-overlap for free, which the timer does not.
- **Effort:** small (one packet), plus disabling the timer at cutover.

## 2. Arxana demo, :7084 (pid 250790)

- **Serves:** the base futon4a demonstration (M-jvm-arxana INSTANTIATE
  increment 6): the committed D4 fixture (draft8 paper sources) plus the APM
  a93J02 fixture, for `futon4a-arxana.el` and the two-pane code-server demo.
- **Recent use:** unknown. Its log (`/tmp/futon4a-demo-jvm.log`) has only the
  three start-up lines, and that file dates from an earlier start (11-06
  22:07Z), not this process (11-07 05:32Z). Capabilities answered 200 in
  0.12 s; resident memory is 32 MB, which suggests it has been idle long
  enough to be swapped out. `arxana-code-server.service` (a transient
  code-server, up since 11-07) may be its client: unconfirmed.
- **Data:** none durable and no futon1b at all. `store/mock-store`,
  in-memory artifact provider, fixture correspondence repository; rebuilt
  from committed fixture files at start.
- **Why a separate JVM:** stated. futon4a README, "Isolation contract": "The
  one-off deployment may use a separate JVM and separate Emacs", loopback
  only, "never a live store"; and "no generic futon1b route may contain
  `arxana/*` writes". Written for a one-off deployment on lucy.
- **Why it is a child of the futon3c JVM:** not found. Its parent is pid
  521296 and its cgroup is `futon3c-zone.service`, so something running
  inside the serving JVM launched it (a seat's shell tool is the likely
  route). Because of that cgroup it will be stopped whenever the futon3c
  unit stops.
- **Owner:** M-jvm-arxana. Code by kimi-1 (dispatched by codex-21), one
  later commit by zai-10.
- **Fold:** mount `arxana.http/handler` in the futon3c server and build this
  stack's config in-process. It needs no store, so nothing moves to futon1b.
- **Blocks:** only the question of whether the fixture demo is still wanted
  once F1–F6 exist.
- **Effort:** small.

## 3–8. Formal Arxana F1–F6, :7085–:7090

| demo | port | pid | adds to the F1 stack | reads at start |
|---|---|---|---|---|
| F1 | 7085 | 1601152 | conversation fixture, markdown export, relocation | committed fixtures |
| F2 | 7088 | 1703400 | collaborative editing over the F1 conversation | committed fixtures |
| F3 | 7086 | 4009302 | informal↔Lean correspondence for a93J02 | pinned commits of `apm-lean`; a judged alignment file under `storage/apm/mining` |
| F4 | 7089 | 1834884 | executable action on the last F1 turn | F1 fixture |
| F5 | 7087 | 1834961 | Lean presentation panels | `futon5/holes/labs/M-formal-arxana/f5-lean` |
| F6 | 7090 | 1352107 | argument page over F5 | F5's inputs |

- **Serve:** the common surface; each to its own Emacs package
  (`futon4a-formal-f1.el` … `-f6.el`).
- **Recent use:** none since the night they were built. Each `demo.log`
  under `/run/user/1000/futon4a-fN-1000/` was last written between 11-08
  23:56Z and 11-09 00:00Z. All six answered capabilities with 200.
- **Data:** in-memory XTDB node, temp-file index, in-memory artifact
  provider. Nothing durable; every record is rebuilt from pinned inputs at
  start, so there is nothing to migrate.
- **Why separate JVMs:** by inheritance from the futon4a isolation contract
  ("Private, self-contained … demonstration stack"; each launch script
  refuses an occupied port and records its own pid). No document says why
  six processes instead of one; each F-step's packet added a `-main` and a
  launcher.
- **Owner:** M-formal-arxana (futon4). Code by codex-21, dispatched by
  claude-13.
- **Fold:** one mount of `/arxana/v1` on :7070 whose graph store is
  `http-store` at the live futon1b :7073, with the six `prepare!` functions
  run once as loaders. The workplan demo's capabilities already list
  `formal-f1/outline` and `formal-f4/execute` beside `workplan/*`, so the
  dispatch is one superset already and one mount can serve all six clients.
- **Blocks:**
  1. **Artifact bytes.** The only `ArtifactProvider` is in-memory. Revisions
     ingested through a futon3c-hosted surface would vanish at restart
     unless the loaders rerun at boot or a durable provider is written.
  2. **Fixture records in the live store.** Folding means writing demo
     fixtures through the live futon1b's Arxana routes, which the futon4a
     README forbids as written. Joe's 11-12 statement supersedes that for
     the merge; the README should be changed in the same cutover.
  3. **Six client URLs.** Each `defcustom` default has to change to the one
     :7070 URL.
  4. `gates/seed-mission-contract!` is called by `start!`; it registers a
     model in futon1b's gate registry and is documented idempotent. It
     should not be needed when the store is the live futon1b: unconfirmed.
- **Effort:** medium for the first (the mount, the store config, the
  durable-provider decision), small for each of the other five.

## 9. Code↔docs demo, :7091 (pid 3590838)

- **Serves:** pinned code↔docs correspondence over futon2, futon4 and
  mathlib4 for the terminal Emacs client.
- **Recent use:** log last written 11-09 02:47Z, an hour after it started.
- **Data:** F1 stack (in-memory XTDB) plus an in-memory correspondence
  repository, loaded from an EDN fixture and the three repo roots.
- **Why separate / owner:** as F1–F6 (codex-21 for claude-13, `e58b95d5d`).
- **Fold and blocks:** as F1–F6, with one more: `corr/in-memory-repository`
  is the only correspondence repository besides the fixture one, so
  correspondence rows also need a futon1b-backed home.
- **Effort:** small after F1.

## 10. Workplan demo, :7094 (pid 2340861)

- **Serves:** the workplan slice (323 items; 3,881 records: 1,720
  clusions, 2,160 attestations, 1 assessment) and the Arxana lexicon (260
  entities per `lexicon_views.clj`). Clients: `futon4a-stepper-review.el`
  (the stepper's REVIEW section in Joe's Emacs),
  `futon3c.arxana.lexicon-loader` (default base URL :7094), and
  `futon4a-workplan.el` (whose default URL is still :7201, the script's
  default port, not the :7094 this instance was started on).
- **Recent use:** yes, and it is the only one in real use. Its log holds
  461,172 store queries and 24 store commands and was last written 11-11
  19:06Z. (That log is 51 MB on the tmpfs under `/run/user/1000`.)
- **Data:** durable, and its own: an XTDB node with local log and storage
  under `~/.local/share/futon4a/workplan-demo` (130 MB) plus
  `arxana-index.sqlite`. At start it found the load already present
  (`:load/status :already-present`). The 24 commands are writes made after
  the first load (lexicon and review records): these exist nowhere else.
- **Why a separate JVM:** not stated. It reuses the F1 stack with
  `:data-dir`, and runs as a transient unit wrapping
  `futon4a/scripts/launch-workplan-demo.sh`.
- **Owner:** M-formal-arxana / the REPL Circus lexicon work. Code by
  codex-21 for claude-13, one commit by claude-1; `lexicon_loader.clj` by
  claude-13 and codex-21.
- **Fold:** the same mount as F1–F6, with the data moved into the live
  futon1b: rerun the workplan load and the lexicon loader against the live
  store (both are written to go through the Arxana write route), then carry
  over whatever was written after the load.
- **Blocks:**
  1. **A second XTDB store with data of record.** The post-load writes have
     to be exported from this store and replayed; the launch script's own
     comment says the full load takes up to ten minutes on a cold store
     through the write route.
  2. **Read cost.** `lexicon_views.clj` notes about a thousand store reads
     and 45 s to build the lexicon index on :7094. On the live store those
     reads share futon1b with everything else; the index should be built
     once and cached in the JVM.
  3. The stepper review URL and the lexicon loader's default base URL.
- **Effort:** medium to large; the only fold here with a data migration.

## Closing table

| JVM | data home | owner | fold route | blocker | effort |
|---|---|---|---|---|---|
| giraffe-reread (timer) | SQLite + EDN under `~/.local/state/futon3c/giraffe-reread`, corpus files; durable | M-giraffe (codex-7 for claude-6) | scheduled task in futon3c, own executor | env-file config; minutes of CPU per cycle | small |
| Arxana demo :7084 | mock store, in-memory; rebuilt | M-jvm-arxana (kimi-1 for codex-21) | `/arxana/v1` mount, in-process config | is it still wanted | small |
| F1 :7085 | in-memory XTDB + embedded futon1b; rebuilt | M-formal-arxana (codex-21 for claude-13) | `/arxana/v1` mount over live futon1b; `prepare!` as loader | in-memory artifact provider; fixtures enter the live store; client URL | medium |
| F2 :7088 | same | same | same mount | client URL | small |
| F3 :7086 | same; reads `apm-lean` pins | same | same mount | client URL | small |
| F4 :7089 | same | same | same mount | client URL | small |
| F5 :7087 | same; reads futon5 lab files | same | same mount | client URL | small |
| F6 :7090 | same | same | same mount | client URL | small |
| code↔docs :7091 | same + in-memory correspondence repository | same | same mount | correspondence repository has no durable form | small |
| workplan :7094 | own durable XTDB, 130 MB, `~/.local/share/futon4a/workplan-demo` | M-formal-arxana / lexicon (codex-21, claude-13, claude-1) | same mount; reload into live futon1b; replay post-load writes | second store with data of record; index read cost; two client URLs | medium–large |

## Suggested order after the minus40 migration

1. **giraffe-reread.** No store, no clients, the largest recurring CPU cost,
   and independent of everything else.
2. **The mount, proved on F1.** `/arxana/v1` on :7070 over the live
   futon1b, with the artifact-provider question settled. This is the piece
   every other demo reuses.
3. **Workplan.** It is the one in daily use and the one holding data, so it
   gains most from being in the store of record, but it should follow (2)
   so the migration is the only new thing in that step.
4. **F2–F6 and code↔docs,** one loader each onto the mount; or retire any
   that Joe no longer wants shown. None has been called since 11-09.
5. **:7084** last: fold or retire.

Stopping the eight idle demo processes would free about 9 GB at once, but
that is a separate decision from folding them and was not in this packet's
remit; nothing here was stopped.

## Not confirmed

- Whether the live futon1b's Arxana index sidecar is enabled (only that the
  two routes are registered).
- What launched :7084 from inside the futon3c JVM, and whether
  `arxana-code-server.service` still uses it.
- Whether `service/dispatch` needs anything per demo beyond the store and
  provider (the per-demo modules were read at the level of their `-main`,
  `start!` and `prepare!`, not their operation handlers).
- Use of :7084–:7091 by anything that leaves no trace in the demos' own
  logs. The request logs are futon1b-side lines from the embedded store, so
  a call to the Arxana surface that touched no store would not appear.
