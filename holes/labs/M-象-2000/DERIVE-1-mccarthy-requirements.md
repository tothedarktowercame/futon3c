# DERIVE-1 — Elephant 2000 requirements, translated into the stack

claude-14, 2026-09-26. Source: `storage/references/mccarthy-elephant-2000/elephant.tex`
(line numbers below are from that file; the paper's main body is l.130–1530, the
rest is McCarthy's appended notes and drafts, cited where they add something).
Target language: Clojure + XTDB (futon1b) + library patterns + Agency agents.
Existing coverage: the six `futon3/library/象/` patterns (claude-1, 2026-09-23,
written without the paper, all `尚未评审`).

Format per row: **R-n** — McCarthy (tex line) → requirement in the stack →
existing 象 coverage → gap.

## A. Acts (speech acts and abstract performatives)

| R | McCarthy | In the stack | 象 coverage | Gap |
|---|---|---|---|---|
| R1 | I/O distinguishes requests, questions, offers, acceptances, permissions, answers, assertions, promises, commitments (l.151–158) | Every record of a turn, bell, notice or wake carries an act type and an origin (operator / agent / harness) | 言即行 (type on the envelope) | Act types in use are the miners' 19 intents; no offer, acceptance, permission or withdraw; harness notices are stored as Joe's `:question` |
| R2 | Abstract performatives: internal commitments, not necessarily output, on which correctness depends (l.243–252, 405–413, 796) | Installing a gate, parking, clocking in, loading code are commitments of the system and are recorded as such, even when no message goes out | 诺必践 covers spoken promises only | The 09-24 requisition gate was a standing commitment that existed only as code |
| R3 | Intrinsic correctness conditions generated from the program text: answers truthful, promises kept, commitments fulfilled, authorised commands obeyed (l.172–178, 1880–1886) | From the typed act stream, generate the checks mechanically: each promise yields a fulfilment check, each answer a truth-and-responsiveness check | none (each 象 pattern states one condition; nothing generates them) | No generator |
| R4 | Assertions truthful; sincerity weaker; the programmer may omit a condition, and should say so (l.356–362) | Agent assertions cite the record they rest on; where truth is not checked, the record says "unchecked" rather than nothing | 答必真且中的, 两种规格 | — |
| R5 | Answers truthful *and* responsive; "I don't know" admissible; false presuppositions; responsive = questioner then knows (l.368–378, 838–893) | An answer gives the value in the form asked (the sha, the count), not where it could be found; "unknown" is a typed answer | 答必真且中的 | The knows-what test (l.882) is not stated |
| R6 | Simple promise = internal commitment + truthful output that it exists; publicly it creates an obligation (l.398–410) | Making a promise writes a durable record (beneficiary, content, deadline, test); its outcome is a second act | 诺必践 | Parks are the promises and live in `/tmp`; released records are deleted (MAP Q4) |
| R7 | Two kinds of obligation: the program's, and the operating organisation's (l.418–429, 1750–1756) | An agent's commitment vs a commitment it creates for Joe or for futon; an act under Joe's name commits Joe | none | Notices sent under Joe's name committed him to rules he never made |
| R8 | Illocutionary vs perlocutionary; on input, "hearing that" vs "learning that" (l.265–272, 816–836, sharp version l.2172–2180) | Delivered vs done (outputs); received vs understood (inputs — 象's annotation is the "learning that" record) | 两种规格 | Input side not stated |
| R9 | Three levels of specification: internal, input-output, accomplishment; accomplishment needs axioms about the world (l.1476–1479, 604–611, 2230–2240) | A rule states all three: e.g. requisition gate — internal (installed), I/O (refuses untargeted jobs), accomplishment (quota not exhausted), with the world assumption named | 两种规格 has two of the three | The accomplishment level is what an incident's clearing proof checks; nothing records it |
| R10 | Authority: the program does only what it is authorised to do; an order is proper only if the speaker has authority; authority tree up to people; delegation (l.169, 952–967, 1515–1518, 1784–1788, 2002) | Each act records under whose authority it is made; delegation via bells forms the tree; nobody acts under another's name without a recorded grant | none | New pattern needed |
| R11 | Requests for permission and giving permission (l.151, 2190) | Joe's go-aheads are permissions, attached to what they permit | none | Matches the mined `operator/grant-the-go-ahead` family |
| R12 | Offers and acceptances; joint acts (agreements) where who offered last may be unknown (l.151–153, 1484–1488) | Agent lists options, Joe accepts one: an agreement recorded as a joint act | none | Matches `operator/accept-the-agents-listed-options` |
| R13 | Speech acts are relative to institutions, which change and are designed (l.761–763, 1453–1470, 1779–1781) | Protocols (bell, park, gates, CLAUDE.md rules) are institutions; each is versioned and dated, and an act is judged under the institution in force at its time | none | Red tape = an institution nobody re-examined |
| R14 | Commitments hold nonmonotonically: valid unless there is a specific reason not to (l.766–770, 1766–1772) | A commitment is defeated only by a recorded reason; the reason is itself an act | none | Pairs with R19 revoke |
| R15 | Pick and choose among philosophers' conditions: fulfilment need not be *caused* by the promise (l.1506–1522) | A park is kept if the awaited job completes, whatever made it complete | — | Design freedom, not a gap |
| R16 | Non-Elephant programs can be read as if they were (l.802–806) | Legacy outputs (commits, bells, parks, notices) are interpreted as acts by 象 and the miners | 翻译契约 | This is what the mining is |

## B. Reference to the past

| R | McCarthy | In the stack | 象 coverage | Gap |
|---|---|---|---|---|
| R17 | One virtual history list; recording is a side effect of acting; the program's own actions are included (l.431–441, 676–688) | The evidence store is the history; every act the stack takes is written there, including its own internal ones | 象不忘 | Reloads, queue entries, park records are not in it (rewind finding) |
| R18 | Functions of the past: value at a time, time of an event, first/last time, time-valued functions, sets of intervals (l.452–497) | As-of reads plus aggregate queries ("last time this incident class was cleared") | 象不忘 (as-of view) | `/evidence` has no as-of (MAP Q2) |
| R19 | `exists(t, commitment x)` ≡ arose before t and not revoked since; `make`, `cancel`, `exists` language-level (l.545–551, 1398–1402) | XTDB valid-time write (arises), valid-time delete (revoke), read at T (exists) | 象不忘 in part | Only hyperedges do this today; revoke has no act type (R-withdraw) |
| R20 | Parsing the past: pattern-match the history to bind variables; Prolog-style matching may suffice (l.693–700, 2044–2080) | core.logic relations over the act history (elephantKanren's approach); the same matcher parses turns into pattern cascades | none | New pattern; shared with the cascade-parse work |
| R21 | Modify the program without knowing its data structures ("don't seat Iranians next to Iraqis", l.2050–2064; also l.216–221) | Joe's constraints ("no notices under my name") are rules over acts, stated without knowing `followup_queue.clj` | none | The strongest practical requirement in the paper for this stack |
| R22 | Full set theory in references to the past (l.600, 1789–1791) | Counts and sets over acts (open commitments, capacity-style limits) | — | Query-language capability |
| R23 | Interpreted and compiled forms have the same I/O behaviour; compiled data structures remember only what is needed (l.672–711) | Caches and queues (park file, follow-up queue, clock store) are compiled forms of the history and must be rebuildable from it | none | `/tmp/futon3c-parked-on.edn` is not derivable from the store |

## C. Program as logic, and operation

| R | McCarthy | In the stack | 象 coverage | Gap |
|---|---|---|---|---|
| R24 | One input at a time, serialised by the runtime; inputs matching no statement are rejected by the runtime (l.511–516, 687–692) | Per-agent turn serialisation (exists); untranslatable input returned with a typed reason | 翻译契约 (route-the-untranslatable) | — |
| R25 | The program is a logical sentence; properties follow from it plus domain axioms; `arises`, `outputs`, `revoke` are circumscribed (l.1304–1416, 1436–1446) | Rules as relations; the store is taken as complete for act predicates, so proofs over it assume every act of those types was recorded | none | Circumscription makes R17's completeness a soundness condition for clearing proofs |
| R26 | A program should be able to answer what its commitments are in a given state (l.1428–1436) | `GET` open commitments of agent X as of T: parks, promises, gates it installed, owed and owing | none | Nothing answers this today |
| R27 | The compiler makes assumptions, reports them, and the user can reply `maybe(not p)` (l.1976–1990) | An agent implementing a request states the assumptions it made; Joe's correction is a recorded act that forces the alternative | none | claude-11's "notices go out as the caller" was an unreported assumption |
| R28 | Committed future actions not triggered by an input: at a promised time, or long-running (l.1998–2000) | Parks with deadlines, timers, scheduled jobs (MAP Q4) | 诺必践 | See R6 |
| R29 | Communications among parts of a program may be speech acts (l.1481–1482) | Agent-to-agent bells and harness-internal messages are acts, typed like Joe's | 言即行 | Only Joe's turns are mined |
| R30 | Outputs with long-term meaning, not display updates (l.979–996) | Typed records, not only rendered turn text | 翻译契约 | — |
| R31 | Don't require too much intelligence of the programs you interact with (l.905–909) | Obligations placed on agents and on Joe stay cheap: one-line requisition, cheap incident reports | none | Constraint on every new rule |
| R32 | Mental state includes intentions, authorisations, obligations and "generalized accounts receivable" (l.2004–2008, 2265) | Per-agent ledger: what it owes (promises, parks) and what is owed to it (awaited bellbacks) | none | Same query as R26, both directions |

## Coverage by the six 象 patterns

- 言即行 — R1, R29 (act type on every message). Goes beyond McCarthy with force levels 轻/平/强.
- 诺必践 — R6, R28 (spoken promises with deadlines). Misses R2 (unspoken commitments).
- 两种规格 — R8, two of R9's three levels.
- 答必真且中的 — R4, R5 (lacks the knows-what test).
- 象不忘 — R17, R18, part of R19 (reference by id, as-of reads). Revoke side missing.
- 翻译契约 — R16, R24, R30 (the mining contract).

Uncovered: R3, R7, R10–R14, R20–R23, R25–R27, R31–R32.

## Proposed patterns (names for Joe to judge; not yet written)

| proposed | covers | conclusion (draft) |
|---|---|---|
| 两种规格 → **三层规格** (amend) | R8, R9 | A rule states its internal, I/O and accomplishment specs, and names the world assumption linking the last two; the accomplishment spec is what an incident's clearing proof checks |
| **收回亦是行** | R14, R19, Q6 | Withdrawal is its own act, citing what it withdraws; commitments end only by such an act or a recorded defeating reason (chip `op-drop-stack`) |
| **名分有据** | R7, R10, R11 | Every act records whose authority it is made under; nobody speaks in another's name without a recorded grant |
| **欠与被欠** | R2, R26, R32 | Every agent can answer, as of any T, what it owes and what is owed to it, including commitments it never announced |
| **以史为据** | R20, R21, R22, R25 | Rules are relations over the act history, so a constraint can be stated without knowing the data structures |
| **视图出于史** | R17, R23, R25 | Every cache and queue is rebuildable from the history; an act the history does not record did not happen for proof purposes |
| **制度有时** | R13 | Protocols are dated and versioned; an act is judged under the protocol in force at its time |
| **明言假设** | R27, R31 | An implementation reports the assumptions it made, and a correction to one is a recorded act |
| **要约与接受** | R11, R12 | An offer and its acceptance form an agreement recorded as one joint act |
