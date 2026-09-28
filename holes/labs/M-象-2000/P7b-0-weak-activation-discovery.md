# P7b-0 — weak activations beside artifacts

Read-only discovery, 2026-09-28. P7b says artifact prose receives the same
retrieval as turns, asynchronously, and records retrieved-but-uncited patterns
beside the artifact (`BUILD-PLAN-象2000.md:147-152`).

## Which retrieval is “the same”

It is the prompt-line/context retrieval: MiniLM semantic search in
`futon3a/scripts/notions_search.py`, called through
`agency.pattern-search/search`. The resident command fixes the embeddings path
and answers JSON line requests (`pattern_search.clj:24-29,101-130`); failures
fall back to the same script/model/embeddings (`:132-160`). `futon3c.dev`
requests top 3 and enriches those maps (`dev.clj:842-850`), then records their
id/title/score/rank as a `context-retrieval` event (`:937-960,992-1020`). This
is what supplies the turn-header `Prompt: pattern ~…` fact.

`scripts/xlate.py find` is different: BM25 over selected flexiarg fields, with
Latin words and CJK bigrams (`xlate.py:1-17,31-44,93-119`). Its cache is keyed
by newest filesystem mtime, not a commit or content hash (`:75-90`). It is a
useful comparison and its parser can identify citations, but substituting it
would make artifact retrieval differ from turn retrieval.

Neither path currently proves R1 stability. The semantic index is a
precomputed JSON matrix and ranking is deterministic for fixed bytes/model in
the resident implementation (`notions_search.py:26-35,96-113`), but its output
does not carry the library commit, model revision, script revision, or index
hash. Pinning only a library commit does not prove that the matrix represents
it. BM25 likewise lacks a commit/content pin. The WM seam explicitly requires
library commit and index hash and byte-identical reruns
(`E-象-2000-wm-seam.md:52-62`). P7b must record all of those; generation must
also write their association into index metadata.

## Artifacts and hook points

| Artifact | Durable source today | Nonblocking hook | Identity |
|---|---|---|---|
| Bell/build packet | `agency_send.py` posts the assembled prompt (`scripts/agency_send.py:189-226,280-288`). `bell-file.sh` only concatenates input files (`scripts/bell-file.sh:49-67`); those files may be temporary. The invoke ledger records the prompt event, truncated at 1500 characters, and later compacts terminal detail (`transport/http.clj:874-897,455-464`). Thus a job is durable for a bounded period, but the original packet file is not. | After `create-invoke-job-ledger!` has atomically persisted the accepted job (`http.clj:1605-1683`), enqueue retrieval with the full in-memory prompt; never await it. | `invoke-job:<job-id>`, plus full-text SHA-256. |
| Mission-doc section | Git blob/commit; commit ingestion already runs off the watcher and records commit structure (`watcher/commit_ingest.clj:1-28`; `watcher/multi.clj:1375`). | The commit-ingest drainer, after reading changed paths; parse changed Markdown headings from the committed blob and enqueue each new/changed section. | `<commit-sha>:<path>:<heading>:<start-line>-<end-line>`. |
| BUILD-PLAN section | Same Git mechanism; it is a Markdown artifact under `holes/labs`. | Same commit hook and section parser, restricted initially to `BUILD-PLAN*.md`. | Same SHA/path/heading/span identity. |

A working-tree save is not the durable write for Markdown. Hooking Emacs save
would race later edits and have no immutable artifact ID. A commit hook sees
the bytes that readers can retrieve later.

## Cited and weak

A citation is an exact canonical library id in artifact text, optionally
prefixed by `~`, backticks, or a Markdown link label/target. Use xlate's ID
grammar as the starting point (`xlate.py:134-139`), resolve it against the
pinned index, and normalize by removing only presentation punctuation. Chinese
ids such as `象/限定随论` follow the same exact-id rule. Titles or bare leaf names
do not count: they can be ambiguous and do not prove which record was cited.

Use semantic top 8, no score floor. Top 3 preserves the existing prompt
behavior; eight is the smallest established bounded review width in this
workspace (`xlate.py find` defaults to eight, `xlate.py:111-119`) and records
useful weaker activations without inventing a cosine threshold. Store top 8 in
the artifact record while the prompt may continue displaying three. `weak` is
exactly ranked top-8 ids minus the cited set. Scores are observations, not an
authority or a gate.

## Record beside the artifact

Write an evidence entry of type `:artifact/weak-activation`, tagged
`[:artifact-retrieval :weak-activation]`, whose subject is the artifact ref.
Body:

- artifact kind/id, repo, commit SHA, path, heading and line span (as applicable);
- artifact text SHA-256;
- retrieval name/version, model revision, library commit, index SHA-256;
- ordered hits `{rank id title score}`;
- sorted cited ids and ordered weak ids;
- observed-at and an error field when retrieval failed.

Use deterministic evidence id over artifact id + text hash + retrieval version
+ library commit + index hash. Replaying identical inputs verifies the existing
body. A changed section, library, index, or retrieval implementation is a new
observation. Failure also gets a deterministic record, so “artifact exists but
has no adjacent record” remains distinguishable from “retrieval ran and
failed.” The write never controls packet delivery or Git acceptance.

## Acceptance run over v1 P2

I read P2 from commit `bed6be53`, lines 30-35 of the then-current
`BUILD-PLAN-象2000.md`, and ran the same MiniLM script against current index
`a97ab9d2015ddd2c4b035046ca86ee0758cbf20a0cc8f548329a03a9df497e4e`.
The current library HEAD is `78c41ada`; the embedding file does not attest that
commit, which is itself the R1 metadata gap.

Neither required pattern appears in top 8 or top 20. Over all 1,582 indexed
entries, `agency/state-atomicity` is rank **827**, score **0.1312**;
`social/idempotent-handoff` is rank **1065**, score **0.0924**. Top 3 was
`象/视图出于史` 0.6423, `象/以史为据` 0.6221, and `象/言即行` 0.6087.
The comparison BM25 run also omitted both from its top 20. Per the acceptance
rule, this is a **retrieval coverage defect**; the test and ranking are not
changed to force a pass.

## First implementation packet

Add one pure `artifact-activation` function taking artifact identity, text and
a pinned retrieval descriptor, returning the closed record above. Hook only
newly accepted bell jobs first: enqueue after durable job creation, run the
resident semantic search, and append the deterministic evidence record.

Tests use a fake search and store: cited ids are removed from weak; exact
Chinese and `~ns/name` citations normalize; repeated inputs are idempotent;
changed text/index yields a new id; search failure records a typed failure and
does not delay job creation. A reconciliation query compares accepted job IDs
with activation subjects; an accepted artifact with neither success nor
failure record is reported as `:activation-missing`. That is the executable
bad case required by 象/限定随论. Mission/BUILD-PLAN commit hooks follow after
the packet seam proves the record contract.
