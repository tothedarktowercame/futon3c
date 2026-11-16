# CL-7 survey: 狗 outputs and proposed GCC records

Date: 2026-11-16. Scope: read-only survey for M-repl-circus closure. No
production code or live state was changed.

## Summary

狗 has three real, distinct outputs: **recall**, **use**, and **burial**. Recall
selects memories before an agent answers an operator turn; use classifies each
surfaced memory after the reply; burial creates durable memories from Joe's
rulings and records both creations and suppressed "said again" candidates.
Today their local receipts are the turn-record fields `:dog_recall`, `:dog_use`,
and `:dog_buried`. Only buried memory content itself has an external durable
authority (memory file plus evidence/version documents). There are no GCC
records for the three relationships.

Schema 1.1 should add `[:memory/recall 1]`, `[:memory/use 1]`,
`[:memory/burial 1]`, and `[:memory/repetition 1]`. The first consumer to move
should be 狗's use-check: read recalled memories from `[:memory/recall 1]`
instead of the turn field. This gives one production GCC reader without putting
private memory bodies in the GCC.

This is the intended extension point: `E-gcc-schema.md:390-397` leaves 狗
unmapped; `:545` assigns 狗/猴 records to M-repl-circus; `:596-603` says they
remain open specifically for schema 1.1; and `:656-663` requires graph writes,
named readers, ID endpoints rather than copied text, and external references
for memories.

## 1. What 狗 does today

### Entry points and timing

- Requiring `dog_provider.clj` registers a 25 ms Prompt-line provider at
  `src/futon3c/agency/dog_provider.clj:1278-1286`. `provider` reads only the
  in-memory recall cache, starts an asynchronous preparation on a miss, and
  returns a typed `not-ready` segment (`:1259-1276`).
- `prompt_line/render!` runs providers and writes the memory segment's payload
  to the operator turn record as `:dog_recall`
  (`src/futon3c/agency/prompt_line.clj:198-241,252-274`).
- HTTP turns Prompt-line segments into the line shown to the invoked agent at
  `src/futon3c/transport/http.clj:5372-5413`. A hit becomes
  `memory ~project/slug (why)`; `not-ready` is visible; `none` and `failed` are
  omitted from the line while remaining typed on the turn record
  (`dog_provider.clj:1199-1225`).
- The turn-event listener prepares recall on `:recorded` and use/burial on
  `:settled` (`dog_provider.clj:452-478`). `start-live!` wires that listener
  (`:495-498`), but no production source file calls `dog-provider/start-live!`.
  The provider itself is registered by namespace load, so prompt rendering can
  be live while the settled post-hook is not.

### Inputs

Recall reads:

1. The operator turn's `:source_text` for keyword terms
   (`dog_provider.clj:408-441`).
2. The turn's `.patterns.json` candidate sidecar; if absent, it falls back to
   the turn field `:lu_ranked :ranked` (`:211-239`). Thus 狗 has not yet moved
   to CL-3's `[:pattern/proposal 1]` reader.
3. The memory authority's text-search route
   `/api/alpha/evidence/text-search`, currently selected by
   `FUTON_MEMORY_BASE` and otherwise `:7073` (`:61-84,304-314`). The returned
   `:evidence/body` is decoded as a Claude-code memory map and its prose `:hook`
   is shortened into `:why` (`:265-291`). This is the memory-evidence-body
   prose/JSON parsing named for removal.
4. Memory files under `~/.claude/projects/<project>/memory/<slug>.md` to stamp
   metadata and later compare reply/content bigrams (`:504-517,535-545,
   625-647`).

Use reads `:dog_recall`, `:reply_text`, and memory-file bodies. It marks each
memory `named`, `echoed`, or `unused`, and produces surfaced/used ID vectors
(`dog_provider.clj:653-737`). It is stored as `:dog_use` (`:739-765`).

Burial reads structured operator `:acts` plus exact spans of `:source_text`,
and may read a retraction paragraph from `:reply_text`
(`dog_provider.clj:848-917,955-1011`). It suppresses candidates already found
in the memory authority, preserving the suppression as a `said-again` event
(`:932-993`). A kept candidate creates:

- `~/.claude/projects/狗/memory/<slug>.md`;
- an evidence document whose stable memory subject is
  `cc:狗/<slug>` and whose stable version is an `e-m40-…` evidence ID; and
- a `:memory/version` hyperedge joining those IDs
  (`dog_provider.clj:1083-1114,1131-1160`).

The per-turn result is `:dog_buried`, containing status, created item receipts,
and `:said-again` suppressions (`dog_provider.clj:1162-1196`). Therefore
"recall/use/buried" are production concepts, not closure-plan inventions.

### Invented public shapes

These reproduce keys, not private memory prose:

```clojure
{:dog_recall
 {:at "2026-11-16T20:00:00Z" :arm "shown" :query-source "mixed"
  :query-ms 74
  :items [{:memory "project/example" :display "project/example"
           :score 0.72 :why "short public-safe hook" :source "keywords"}]}}

{:dog_use
 {:at "2026-11-16T20:00:04Z" :status "ok"
  :surfaced-memory-ids ["project/example"]
  :used-memory-ids ["project/example"]
  :items [{:memory "project/example" :used true :kind "named"
           :evidence "reply names the memory slug"}]}}

{:dog_buried
 {:at "2026-11-16T20:00:05Z" :status "ok"
  :items [{:slug "joe-2026-11-16-example" :status "buried"
           :path "<memory-authority-path>"}]
  :said-again [{:act "turn-actor-f-s1-0" :kind "constrain"
                :text "<operator span omitted>"
                :memory-ids ["project/existing"]
                :shared [["distinctive" "bigram"]]}]}}
```

## 2. Consumers and prose parsing

| Output | Current consumers | Prose parsing? |
|---|---|---|
| recall cache/segment | Prompt-line provider and HTTP Prompt-line renderer (`dog_provider.clj:1259-1284`; `transport/http.clj:5372-5413`) | Renderer formats structured fields; no body parsing. |
| `:dog_recall` | 狗 use-check (`dog_provider.clj:653-765`); `scripts/dog_helps_report.py:241-263` | Use-check reads memory-file bodies and reply prose to detect names/bigrams. |
| `:dog_use` | `scripts/dog_helps_report.py:241-263`; test/report fixtures | Structured field only. No production feedback reader changes future retrieval. |
| buried memory file + evidence/version docs | Subsequent memory text-search/recall (`dog_provider.clj:265-314`); ordinary memory tooling | Yes: recall decodes `:evidence/body`, and search ranks/indexes body prose. |
| `:dog_buried` | `scripts/dog_helps_report.py:193-213,336-377` | Structured receipts, except the report carries exact suppressed text. |

No other production namespace reads the three turn fields. In particular,
the future learning loop is presently analytical reporting, not an online 狗
consumer.

## 3. Live six-hour volume (read-only)

The turn store contained 338 records: 57 operator, 33 agent, and 248 system.

| Field/output | Turns | Detail |
|---|---:|---|
| `:dog_recall` | 57 | all status `not-ready`; 0 turns with items; 0 recalled items |
| `:dog_use` | 0 | 0 used items |
| `:dog_buried` | 0 | 0 buried items; 0 `said-again` events |

This is consistent with the wiring finding above: the Prompt-line provider is
registered, while the repository has no caller of the listener's `start-live!`.
It is not evidence that the use/burial functions are merely speculative; their
writers and historical fixtures exist, but the current six-hour live path did
not invoke them.

## 4. Proposed schema 1.1 declarations

The declarations below use the style of
`src/futon3c/arxana/type_registry.clj:155-178`.

```clojure
(clusion-type :memory/recall 1
  [{:role :turn   :endpoint/kinds #{:external-ref}}
   {:role :memory :endpoint/kinds #{:external-ref}}]
  [:arm])

(clusion-type :memory/use 1
  [{:role :turn   :endpoint/kinds #{:external-ref}}
   {:role :memory :endpoint/kinds #{:external-ref}}]
  [:outcome])

(clusion-type :memory/burial 1
  [{:role :turn   :endpoint/kinds #{:external-ref}}
   {:role :memory :endpoint/kinds #{:external-ref}}]
  [:outcome])

(clusion-type :memory/repetition 1
  [{:role :turn   :endpoint/kinds #{:external-ref}}
   {:role :memory :endpoint/kinds #{:external-ref}}]
  [:kind])
```

Common endpoints:

- `:turn` reuses the operator Evidence Landscape external reference built by
  `turn_mention_writer/field-evidence` (`turn_mention_writer.clj:65-94`), as
  CL-3 through CL-5 do.
- `:memory` wraps, but does not replace, the existing memory authority ID. For
  current memories that is the evidence subject `cc:<project>/<slug>`; its
  revision is the `e-m40-…` evidence ID joined by the existing
  `:memory/version` hyperedge (`dog_provider.clj:1086-1113`). The Arxana
  external reference should carry the authority base URL, stable `cc:` record
  ID, and evidence/version digest. Memory bodies remain external and private.

Attestations:

- **Recall:** producer `狗`, basis `model`; content has rank, score, `why`,
  query source, arm, query time, and `written-at`. `:arm` is identity content
  because shown versus withheld changes the claimed exposure. Typed
  `not-ready`/`none`/`failed` belongs in operation-attempt receipts, not fake
  recall clusions.
- **Use:** producer `狗`, basis `model`; identity `:outcome` is `:used` or
  `:unused`; attestation content carries `kind` (`named`, `echoed`, `unused`),
  the structured explanation, algorithm version/threshold, reply reference,
  and `written-at`. A later classifier result appends a stamped attestation;
  latest stamp wins.
- **Burial:** producer `狗`, basis `exact` for a successful authority write;
  identity `:outcome` is `:buried`, `:exists`, or `:failed`. Attestation content
  carries source act ID/range, path, evidence/version IDs, status/reason, and
  `written-at`. No memory body is copied.
- **Repetition:** one record per existing memory matched by a suppressed
  candidate; identity `:kind` is `:said-again`. Attestation content carries
  source act/range, shared-bigram hashes or tokens as policy permits, classifier
  version, and `written-at`. Exact operator text should be referenced by range,
  not copied into this public GCC record.

The first reader migration should be `prepare-use-check!`: replace its read of
turn-field `:dog_recall` (`dog_provider.clj:739-759`) with a bounded reverse
lookup of `[:memory/recall 1]` by turn. Prompt rendering remains latency-bound
and can keep the cache. After that, `dog_helps_report.py` can read recall/use/
burial/repetition projections from the GCC instead of turn JSON.

## 5. What should not be forced into GCC records

- The 25 ms provider cache and in-flight marker are ephemeral scheduling state,
  not claims (`dog_provider.clj:114-126,400-447`).
- Prompt-line text is a presentation derived from recall records, not another
  semantic record (`dog_provider.clj:1199-1225`).
- Search queries, FTS scores, and the alternating LU/keyword merge are method
  inputs/attestation content, not graph topology (`dog_provider.clj:293-338,
  340-388`).
- Private memory bodies and buried-memory prose stay in their memory authority.
  GCC stores resolvable references and evidence ranges, never copied JSON or
  prose.
- `not-ready`, disabled, failed, no-recall, and no-reply are attempt/absence
  receipts. Creating relationship clusions for them would falsely assert that
  recall/use/burial occurred.
- The policy decision to suppress stale dated directives is classifier method
  behavior, not an edge (`dog_provider.clj:252-263`).

## Conclusion

狗 is not one output. It is a four-record boundary: recall exposure, use
classification, durable burial, and suppressed repetition. The current turn
fields witness those concepts but provide no named GCC reader, while buried
memory bodies already have a resolvable external authority. Schema 1.1 can
close the gap without publishing private text by storing only turn↔memory
relationships, typed outcomes, stamps, and source references.
