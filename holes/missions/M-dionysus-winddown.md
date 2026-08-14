# M-dionysus-winddown — get everything off the laptop, safely and usably

**Status:** HEAD complete; IDENTIFY complete; MAP substantially complete (2026-08-14)
**Gate:** operator-decision — placement and sensitivity of raw session history (`~/.claude`, `~/.codex`): live mirror on a Linode, encrypted archive, or offline copy?
**Gate:** operator-decision — one-shot `futon1b/migration/` export to a second site now (buys safety before the deadline) vs building the continuous replication `README-federate.md` specifies (buys the right thing, slower)?
**Gate:** operator-input — FTS5 storage budget for indexing the Evidence Landscape, once MAP Q6 has a real entry count rather than an extrapolation.

Deadline: **2026-08-28** (~14 days from HEAD).

---

## HEAD

### Operator-voice anchor

> "I'll have to return the Dionysus laptop in < 2 weeks so what we really want
> is to have data flow around all of the nodes in the mesh (akin to what it says
> in futon0/README-federate.md) not *just* Dionysus to Zone."

> "We'll deal with getting everything off of the laptop both safely and useably
> — we have about 14 days so I don't think we will run out of time. But we need
> to be careful about what we are doing."

On being offered raw `~/.claude` / `~/.codex` transcripts as the thing to rescue:

> "falling back to ~/.claude and ~/.codex is sort of like bringing a stone axe
> to a lumberjack contest. We have the Evidence Landscape in futon1b (or should)
> ... Moreover, while we're at it, the Evidence Landscape would be a great
> candidate for free text indexing per the futon1b sidecar (which may have
> storage size implications), which would make the Evidence Landscape itself
> much more useful and potentially align it with our V3 memory system."

### What's already felt to be true

- The Evidence Landscape is the right layer. It is a refinement of the raw
  transcripts, not a summary of them, and it is **already capturing
  `turn-commits` events live** — verified below.
- Data movement is a solved shape when the payload is content-addressed. The
  154-file corpus went from 2 replicas to 4 in an afternoon and now repairs
  itself from any peer.
- "Safely" and "usably" are different requirements. A verified copy nobody can
  query is only half the mission; hence the FTS5 strand.

### Anti-glibness discipline

The failure mode is declaring victory on **bytes moved** while the *usability*
half goes unmet — a 21 GB store sitting on a second box with no running server,
no index, and no query path is not a rescue. Every completion criterion below is
therefore stated as something queryable or verifiable from a **non-laptop**
node, not as a transfer having happened.

Second discipline: no claim in this document that was not measured today.
Extrapolations are marked as such (see Q6).

### Working-economy position

This mission underwrites §8.1 of `C-substrate-completion` — the `turn→mission→code`
ground truth the predictive programme (E-α…δ) needs. That corpus is being written
into the Evidence Landscape as work happens. If the Landscape dies with the
laptop, the going-forward accrual restarts from zero on the next machine: the
"evidence base with no past" failure the `.gitignore` comment of 2026-08-13 warns
about in a different context.

It is underwritten by `futon0/README-federate.md` (the governing design) and by
the evidence mesh sync shipped today (`scripts/evidence_mesh_sync.sh`).

### Clarity-gap / carried-forward tensions

1. **Replication is not backup** (F-7). A mirror propagates corruption. A
   separate point-in-time recovery point is still owed and is not addressed by
   anything built so far.
2. **XTDB2 stores are single-process.** A live store directory cannot simply be
   copied; the export path or a stopped node is required. Untested at 21 GB.
3. **One-shot vs continuous.** `futon1b/migration/` is backfill machinery, not
   replication — no per-origin cursor, no durable acknowledgement, no conflict
   policy, no tombstone retention. Using it buys safety, not federation.
4. **The mesh's code plane is incoherent** (lon 401 commits behind, chi 1060).
   Data moved today only because it is *ignored* data on a stable path. Anything
   depending on current checkouts will not work as-is.

### Provenance

Generated from the operator exchange of 2026-08-14 (emacs-repl, claude-3),
during the inbox-zero work recorded in `futon0/README-inbox-zero.md`. All
figures below measured that day.

---

## 1. IDENTIFY

**Motivation.** The Dionysus laptop is returned by 2026-08-28. It currently
holds the only copy of the live Evidence Landscape (21 GB) and 5.5 GB of raw
agent session history. Neither is replicated; the mechanism to replicate them
does not exist.

**Theoretical anchoring.** `futon0/README-federate.md` invariants F-0 (stable
global identity), F-3 (ordered, resumable replication), F-4 (idempotent replay,
loud mismatch), F-7 (replication is not backup); §4 recommended topology
(site-owned writes plus pull replication). `C-substrate-completion` §8.1 for why
the turn corpus matters.

**Scope in.** Everything on Dionysus that is not reproducible from another node:
the futon1b stores, the Evidence Landscape they contain, raw session history,
and any repo working state not yet pushed. Making the Landscape *usable* from a
non-laptop node, including the FTS5 sidecar.

**Scope in — broadened 2026-08-14 (operator).** Not only the FUTON repos. The
box carries an operating environment as well as a stack:

> "if we broaden beyond the FUTON repos, we should also consider things like my
> home dir and passwords in Firefox and other stuff like that — emacs config —
> etc. Not necessarily right away, but before I give the box back."

So: home directory, browser credential stores, Emacs configuration, shell and
tool config, ssh keys and known-hosts, systemd user units, and anything else
whose absence would make the next machine *a different machine* rather than the
same one elsewhere. Inventory owed; none of it is assessed yet.

**Target environment — there is no next laptop.** Recorded 2026-08-14:

> "even though I have working computers available I don't have another laptop
> to migrate things to, so we're migrating *from* a laptop to a server plus a
> DeX-powered Samsung. (I could no doubt buy a cheap laptop if we absolutely
> need one.)"

`futon0/README-termux.md` (2026-08-12, tracked) already designs this shape:
Zone becomes the main machine, the phone is a client, and
`mosh zone -- tmux new-session -A -s main` is the one command — **[verified]**
idempotent from Dionysus. mosh survives the network, tmux survives mosh.
Companion: `~/code/DEX-SETUP.md`.

**Capacity is not the constraint.** Zone: 1.9 T disk with **1.3 T free**,
249 G RAM (169 G available), 32 cores. The 21 GB store is unremarkable there.

**The constraint is that the client is thin.** Everything must run and be
queryable *server-side*; nothing may assume a fat local workstation. This
sharpens C2/C3 — "queryable from a non-laptop node" now means queryable over
mosh/tmux from a phone, not merely present on a box. Anything whose usability
depends on a desktop-class local machine either moves server-side or is
explicitly accepted as lost. It also means the migration has no "copy the home
directory across" step: there is no symmetrical destination.

**Order of work** (operator): the FUTON repos first, then `~`, `~/.config`,
Firefox, and the rest.

**Recoverable ≠ usable.** The operator's framing, and the mission's sharpest
constraint:

> "Given that I'm stashing things on a 2TB drive (LenovoBackup, not currently
> plugged in) that data is recoverable, but there's a difference between
> recoverable and usable w/ seamless transition."

A LenovoBackup copy discharges *recoverable*. It does not discharge *usable*:
an archive on an unplugged drive is not a working environment, and a 21 GB
store that has to be found, restored, re-indexed and re-pointed is a project
rather than a transition. This is the same distinction as C2/C3 — a verified
copy nobody can query is half a rescue — applied to the whole machine.

Practical consequence: the backup drive is the **floor**, not the plan. Work
that ends with "it's on LenovoBackup" has met the weaker of the two bars, and
the mission should say which bar each item has reached.

**Scope out.** Continuous multi-master federation (that is `README-federate.md`'s
own implementation order, not this mission). Repairing the stale checkouts on lon
and chi beyond what is needed to host data. Migrating `~/apm-evidence` — derived,
regenerable, and correctly signalled as a temporary dump by living in `~`.

**Completion criteria** — all stated from a non-laptop node:

- [ ] C1. Every registered corpus verifies on **≥2 non-laptop sites**.
      *(Already true for `futon3c-evidence-20260801`: 3/3.)*
- [ ] C2. The Evidence Landscape is queryable from a non-laptop node, and a
      `turn-commits` event recorded on Dionysus is retrievable there by
      `turn-id`.
- [ ] C3. `/api/alpha/evidence/text-search` returns results against a populated
      `fts5-evidence.db` on that node.
- [ ] C4. A point-in-time recovery copy exists that is **not** a live mirror
      (F-7).
- [ ] C5. Dionysus can be wiped with nothing lost — demonstrated by a written
      reconciliation, not asserted.
- [ ] C6. The stack reaches inbox zero across the repo census (17 repos as of
      2026-08-14; was 14 before futon1b, apm-lean and mathlib4 were added).
      Baseline that day: **22 dirty, 65 untracked**, of which futon3c
      contributed 0.
- [ ] C7. Every item in the broadened scope is recorded at one of two levels —
      *recoverable* (on LenovoBackup or equivalent) or *usable* (present and
      working on a live non-laptop host) — with no item left unstated. Items
      may legitimately stop at *recoverable*; what is not acceptable is not
      knowing which.

**Relationship to other missions.** Enables `C-substrate-completion` §8.1.
Blocks the next machine's usefulness.

**Precondition: `futon0/README-inbox-zero.md`.** Not a nicety — the dependency
is structural, and it bites in three places.

1. **Uncommitted work cannot enter a content-addressed sync.** The mechanism
   shipped today replicates what a manifest names and verifies by sha256. A
   dirty file has no manifest row and no expected hash, so there is nothing to
   check a copy against. Such a file is not "harder" to sync — it is outside
   the scheme entirely. The only alternative is wholesale rsync, which
   abandons verification exactly where the stakes are highest.

2. **A dirty tree is an unreliable narrator, and syncing propagates that.**
   On 2026-08-14 three agents each drew a confident wrong conclusion from
   zone's tree: deleted-but-committed files read as missing artefacts,
   duplicate shas read as stranded work. Those conclusions were relayed
   between agents and compounded. Replicating an ambiguous tree to N sites
   multiplies the ambiguity rather than resolving it, and every site then
   disagrees plausibly.

3. **You cannot evacuate what you cannot enumerate.** C5 ("Dionysus can be
   wiped with nothing lost") is only demonstrable against a known inventory.
   The risk table in MAP exists *because* futon3c reached zero first: what
   remained was 154 catalogued files with hashes, not an indefinite pile. Had
   the 29 dirty files and 154 uncatalogued ones still been in place, the honest
   answer to "what is on this laptop" would have been "we don't know."

Evidence that the precondition is load-bearing: the corpus reached four
verifying replicas in an afternoon *because* it was catalogued and ignored,
while lon and chi sit 401 and 1060 commits behind with unpushed local work —
i.e. the sites whose repos are dirtiest are exactly the ones where nothing
could be trusted to have arrived correctly.

Status: satisfied on Dionysus futon3c and zone futon3c (both at zero,
2026-08-14). **Not** satisfied on lon, chi, or hyperreal, and not yet assessed
for futon1b's own working tree — see the mission's Next section.

**Owner and dependencies.** claude-3 drives; ams-claude-2 owns zone. Repos:
futon1b (stores, FTS), futon3c (mesh sync, this mission), futon0 (design docs).

---

## 2. MAP — survey of 2026-08-14

*Facts, not decisions. Every number below was measured.*

### Inventory: what is on Dionysus and where else it lives

| asset | size | replicas | state |
|---|---|---|---|
| futon3c evidence corpus (154 files) | 5.6 MB | **4** — oxf, ams, lon, chi | **SAFE.** sha 154/154 on every site; timer live |
| futon1b `migration-store-21` | **21 GB** | **1** | **AT RISK.** The live store — server runs `--store-dir migration-store-21` |
| futon1b `migration-store` | 1.5 GB | 1 | at risk; older store |
| futon1b `migration-export` / `-full` | 206 MB / 57 MB | 1 | at risk; export artifacts |
| `~/.claude/projects` | 1.8 GB, 1,702 files | 1 | raw upstream; superseded by the Landscape |
| `~/.codex` | 3.7 GB, 7,348 files | 1 | raw upstream |
| `~/apm-evidence` (on **zone**, not here) | 1.3 GB, 13,519 files | 1 (zone) | derived, regenerable, out of scope |

lucy holds `switchover-store` (4.6 GB) — a *different* store, and its
`futon1b-server` service is **inactive**.

### Inventory: what is in the Evidence Landscape

Live node on Dionysus, `{:ok true, :node-open? true}`:

- `turn-commits` events are being written **as work happens**. A sample entry:

  ```clojure
  {:evidence/body {:event "turn-commits", :turn-id "claude-3-turn-37",
                   :transport "emacs-claude-repl", :commit-count 1,
                   :commits [{:repo "futon3c", :sha "592d1157…",
                              :subject "holes: move TN…"}]}}
  ```

- First 1000 evidence entries contain **193 distinct turn-ids across 12 agents**
  (`claude-1`…`claude-14`, `codex-1/4/5/6`).
- `memory/assert` hyperedges: **264**.
- Entity-axis census returns near-zero (`evidence` 16, `memory`/`pattern`/
  `session`/`agent` 0) — the corpus lives on the hyperedge axis, not the entity
  axis. **Do not read the entity census as the size of the Landscape.**

### Ready vs missing

| ready — no new code needed | missing — the actual work |
|---|---|
| Evidence mesh sync: manifest-driven, sha-verified, pull-from-any-peer, idempotent, systemd timer, receipts (`scripts/evidence_mesh_sync.sh`) | A corpus definition for the futon1b store — it is a live database, not a manifest of files |
| FTS5 schema, oracle, periodic test, text layer (`fts5-evidence.db`, `fts_oracle.clj`, `test_fts_periodic.clj`, `futon1b_text.clj`, `textprobe*`) | **Populating** the index — the db is 32 KB and empty |
| Routes `/api/alpha/evidence/text-search`, `/api/alpha/memory/search` | Any index behind those routes |
| `futon1b/migration/` full export→transform→ingest→verify pipeline | It is one-shot backfill: no cursor, no acknowledgement, no conflict policy, no tombstones |
| `scripts/backup_evidence.sh` — manifest-driven, sha-verifying | A recovery point distinct from a mirror (F-7) |
| lucy: 80 GB free, futon3c checkout present | lucy's futon1b service is inactive; store is a different lineage |

### Survey questions

**Q1. Is the Evidence Landscape already syncing to lucy, as believed?**
**No.** futon1b's README states stores are "gitignored — each box builds its own
from its own futon1a". The two boxes hold differently-named stores of different
sizes and lucy's service is inactive. `README-federate.md` §1 says the same in
the abstract: Agency federation "does not replicate the futon1b graph, evidence,
memory projections, or search indexes." What exists is parallel independent
construction, not replication.

**Q2. Are the raw transcripts the right thing to rescue?**
**No** — the operator is right. They are the input to a refinement that already
exists and is running. Rescuing them would preserve the ore and lose the metal.
They remain a fallback only if the Landscape cannot be moved.

**Q3. Does the FTS5 sidecar exist?**
**Yes, and it is empty.** 32 KB; tables `ev_fts`, `ev_fts_content`,
`ev_fts_data`, `ev_fts_idx`, `ev_fts_docsize`, `ev_fts_config`, `fts_meta`.
The proposal is population, not construction.

**Q4. Can the mesh move data through repos?**
**No.** lon is 401 commits behind, chi 1060, and neither had the manifest.
Today's transfer worked because the corpus is *ignored* data on a stable path,
untouched by a later pull. **The data plane must stay decoupled from the code
plane.**

**Q5. What is the replication factor today?**
`futon3c-evidence-20260801`: `replication-factor=4/4`,
`non-laptop-replication-factor=3/3`, `required-non-laptop=2`. Everything else on
the risk table: **1**.

**Q6. How large would the FTS index be?** *(open)*
Not answerable yet. A 1000-entry sample was 1.38 MB of payload (~1.4 KB/entry),
but the total entry count is unknown — the entity census reports the wrong axis
and no hyperedge-axis total has been obtained. **Get a real count before sizing.**

### Surprises

-1. **`~/code` is not a repository, and 69 loose files live in it.** Among them
   the documents that define how the system is operated:
   `CLAUDE.md` (8,987 B — the workspace handoff protocol, bells-vs-whistles,
   the park discipline), `AGENTS.md` (3,446 B — the Codex-side view of that
   same protocol, which `CLAUDE.md` points at), `DEX-SETUP.md` (5,653 B — the
   setup for the machine we are migrating *to*), `START_HERE.md`,
   `futon-evidence-pack.md` (156 KB), and five setup scripts
   (`general-setup.sh`, `lucy-setup.sh`, `chicago-setup.sh`, `real-setup.sh`,
   `check.sh`). None are symlinks into a repo; `~/code/CLAUDE.md` is a
   different file from `futon3c/CLAUDE.md`. Also present: a 7-byte `SECRET`
   at mode 664.
   These are the instructions for running the fleet and for building its
   replacement, and they are the least protected artifacts on the machine.

0. **`futon0/README-federate.md` is untracked.** The governing design document
   for this mission's federation strand — cited throughout MAP as the authority
   — exists only on the laptop being returned. So does its companion
   `README-deploy.md`. Two mission files are also untracked
   (`futon4/.../M-librarian.md`, `futon7/.../M-becoming-nomad.md`), as are two
   DarkTower Lean sources on the in-use `darktower` branch
   (`GeometricMorphism.lean`, `GrothendieckTopos.lean`). The documents that
   describe how to survive losing the box are themselves not surviving it.

1. **The turn→code corpus already exists, structured and live.** §8.1 describes
   it as something to build; it is being written now. This raises the stakes of
   the 21 GB store from "useful history" to "the substrate of the predictive
   programme".
2. **The believed lucy sync does not exist.** The mission's premise had to be
   corrected during MAP.
3. **The FTS work is mostly done and idle.** A cheaper win than it appeared.
4. **The mesh's repos are ~1000 commits apart.** Any plan routing data through
   checkouts is unworkable; today's success depended on *not* doing that.

---

## Next

DERIVE is blocked on the three gates at the head of this document. The MAP
finding that most constrains DERIVE is Q1: there is no existing replication to
extend, so the design starts from `README-federate.md` §4 rather than from
current behaviour.

Unblocked meanwhile, and safe to do in any order:

- Obtain a real Landscape entry count (closes Q6).
- Test whether `migration-export` can be produced from a **live** 21 GB store,
  or whether the node must stop (carried-forward tension 2). This is the
  single largest unknown in the transfer.
- **Bring futon1b to inbox zero** — it holds the mission's most valuable asset
  and has never been assessed. Surveyed 2026-08-14: in sync with origin, one
  tracked modification (`test_json.clj`), ten untracked. Two of those are
  authored documents that have never been committed —
  `TN-xtdb2-query-ceilings-and-ingest-memory-2026-08-02.md` and
  `holes/DEFECT-bitemporal-as-of-two-routes.md` — the same pattern that cost a
  morning in futon3c. The rest are generated (`fts5-evidence.db{,-shm,-wal}`,
  `logs/full-backfill.log`, `migration-export-full/export-summary.edn`,
  `holes/jstack-hydration-slowness-2026-08-02.txt`) and want ignore rules.
  Small job; do it before the store moves, not after.
- Assess inbox zero on lon, chi and hyperreal. Their repos are 401 and 1060
  commits behind with unpushed local work, so this is a larger job than
  futon1b's and may be scoped out — but it should be a decision, not an
  omission.
