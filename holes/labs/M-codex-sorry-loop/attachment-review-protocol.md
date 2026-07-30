# Attachment review protocol

For reviewing memory→pattern attachments so that promoted memories are actually
recallable. Written 2026-07-30 by claude-9 after running it on 51 memories, so
every step below is one I performed rather than one I designed.

## Why this is needed

`memory-recall/project-components` excludes any attachment whose
`:attachment-status` is not `:reviewed`. Until an attachment is reviewed, its
memory is invisible to `recall-by-endpoints`, therefore to
`propose-patterns-by-query`, therefore to dispatch-time recall. The codex-lane
promotion pipeline wrote attachments as `:proposed` and never reviewed them, so
**no codex-lane memory was recallable for the lane's entire history**.

## The constraint that makes this a two-party job

`memory-lifecycle/review-attachment!` refuses when the review evidence's author
equals the **memory's** author:

> memory author cannot review their own attachment

This is correct and load-bearing. It is why one agent cannot promote and review
the same memory, and it is what this protocol exists to satisfy.

**Current split of the 86 codex-lane memories:**

| memory `:evidence/author` | count | who may review |
|---|---|---|
| `claude-6` | 51 | claude-9 — **done**, 2026-07-30 |
| `claude-9` | 35 | **codex-5**, or Joe — outstanding |

Going forward the promotion pipeline authors the memory as `codex-5` (the
drafter) and the review as `claude-9` (the owner), so new memories are reviewable
by the owner without any backfill.

## What the review attests — state this honestly

The review attests that **the attachment is warranted**. Concretely:

1. the memory entry is well-formed — `:evidence/type :memory`,
   `:evidence/claim-type :assert`, `:body :name` matching the memory name;
2. its inner `:body` is substantive rather than a stub;
3. its cited turn-round and receipt ids **resolve** in the store — fetch them,
   do not assume;
4. the edge attaches it to a pattern in the `:mathematics` domain and carries
   `:witness-status`.

It does **not** attest that the mathematics was re-derived. Say so in the
provenance text. A review that overclaims is worse than no review.

If a memory fails a check, **decline it** and say which check failed. Declining
is a normal outcome; 0 of my 51 failed, but that was the corpus being sound, not
the checks being lenient.

## The exact evidence contract

Four requirements, each of which I got wrong by guessing before reading
`memory_lifecycle.clj:117-137`. Copy them:

```clojure
{:evidence/id         "e-review-<reviewer>-<memory-name>"
 :evidence/subject    {:ref/type :memory :ref/id "e-codexpilot-<memory-name>"}
 :evidence/type       :memory                  ; NOT :reflection
 :evidence/claim-type :observation             ; or :challenge
 :evidence/at         "<ISO-8601 timestamp>"   ; must be present and non-blank
 :evidence/author     "<reviewer, != memory author>"
 :evidence/body
 {:review/event          :memory-attachment-review   ; exact keyword
  :review/memory-id      "e-codexpilot-<memory-name>"
  :review/pattern-ids    ["math/..." ...]   ; EXACTLY the edge's patterns
  :review/verdict        :approve
  :review/witness-status :independently-witnessed
  :review/provenance     "<what you checked, and what you did not>"
  :review/policy-verdict :approve}}
```

`:review/pattern-ids` must match the attachment exactly — read them from
`hx-codexpilot-<name>` at `[:hx/props :roles :patterns]`, do not retype them.

## Running it

1. Write the review evidence over `POST /api/alpha/evidence` (content-type
   `application/edn`). The in-process `boundary/append!` path did not persist for
   me; the HTTP path did.
2. Apply it with `scripts/review_codex_lane_attachments.clj --names-file F
   --commit`, with `FUTON_SUBSTRATE_URL=http://127.0.0.1:7073` set — otherwise
   the substrate client falls back to the retired :7071 store and every read
   fails.
3. It is idempotent: an already-reviewed attachment reports `:existing`.
4. It is **slow** — four substrate round-trips per memory. 51 memories exceeded a
   10-minute shell timeout. Run it under `scripts/bg.py` and poll.

## Verifying it worked

Do not trust the exit code. Check the effect:

```
recall-by-endpoints {:domain :mathematics} ["math/<pattern>"]   ; expect >0 memories
propose-patterns-by-query {:domain :mathematics} "<query>"      ; expect >0 candidates
```

Before the review, every codex-lane pattern returned 0 memories and every query
returned 0 candidates. After, `math/missing-dependency-protocol` returned 10 and
`indicator riemann` proposed `math/riemann-darboux-api`.

## Known quality caveat, recorded not hidden

Of the 51 reviewed: **13 carry a `:hook` identical to the memory name** and **21
have an empty `:how-to-apply`**. Inner bodies were substantive in all 51. The
hook is what a reader sees when recall surfaces a memory, so a hook that merely
repeats the name is close to useless at the point of use. This does not make an
attachment unwarranted and did not block approval — but it is worth fixing at
the scribe end, and worth knowing before anyone concludes that a surfaced memory
"wasn't helpful".
