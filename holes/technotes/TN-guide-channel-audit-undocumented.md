# TN-guide-channel-audit-undocumented — five frames lost to a contract no card states

Claude (claude-12), 2026-08-30. Campaign `jit-all-open-v2`, frames f54-f58.
Written after being belled to dispose f57; f57 and f58 were disposed
concurrently by codex-17 while I was investigating, so this note is the part
its records do not cover.

## The pattern

Five consecutive frames parked `:role-terminal-repair-frame-park`, every one
of them at `:guide-intervention-1`, every one with
`:error/code :live-job-terminal-repair-exhausted`:

| frame | problem | findings | disposed | repair-required? |
|---|---|---|---|---|
| f54 | a99J05 | `[:guide-candidates-invalid]` | :partial | true |
| f55 | a99J06 | `[:guide-mode-authority-mismatch]` | :partial | false |
| f56 | a99J08 | `[:guide-channel-isolation-unproved :guide-mode-authority-mismatch :guide-candidates-outside-store-mode]` | :partial | true |
| f57 | a99J10 | `[:guide-mode-authority-mismatch]` | :partial | false |
| f58 | aunk04 | `[:guide-mode-authority-mismatch :guide-candidates-invalid :guide-candidates-outside-store-mode]` | :partial | true |

Three of the five were disposed `:partial` before f57 and f58 parked with the
same fault. Under Joe's rule -- continue while the obstruction moves, stop
when it repeats -- this obstruction stopped moving at f55.

## The single cause

Every one of those findings is produced by `channel-audit` not being a map.
From `live_learning_phases.clj:449-470`:

- `(not= false (:direct-student-contact? channel-audit))` -> `:guide-channel-isolation-unproved`
- `(not= (:mode request) submitted-mode)` -> `:guide-mode-authority-mismatch`
- candidate shape checks -> `:guide-candidates-invalid`, `:guide-candidates-outside-store-mode`

f57's guide emitted this:

    :channel-audit "store-mode only: three memories deposited to the
    substrate via record-memory (subject problem a99J10; ids e-d7d3b2ff...,
    e-3a6c29a0..., e-4f4a5625...) and listed in this payload's candidates;
    no harness or retrieval tuning touched, no direct guide-to-student
    message sent, no conductor action submitted."

Prose. Everything the validator wants is *stated* there in English -- store
mode, the candidate ids, and that no direct contact occurred -- and none of
it is readable. `(:direct-student-contact? <string>)` is nil, which is not
`false`, so isolation is unproved. No `:mode` can be lifted, so authority
mismatches.

## Why nobody told the guide

    $ grep -c "channel-audit" role-cards/claude-guide*.md
    claude-guide.md:0   v2.md:0   v2.1.md:0   v2.3.md:0   v2.4.md:0

Zero, in all five cards. There is no mention in any prompt or packet builder
either. The only occurrences of the string anywhere in the frame are the
guide's own output.

So the guide is required to emit a typed object carrying `:mode`,
`:candidates` and `:direct-student-contact? false`, and nothing in its
instructions says that the field exists. It inferred the name from the
repair-loop error and then guessed the shape. Across f57's repair attempts it
guessed three different encodings of the mode:

    :mode :store-mode      ;; keyword    (what the request carried)
    :mode "store-mode"     ;; string
    :mode ":store-mode"    ;; stringified keyword, after the agent decided to
                           ;; "echo the packet mode exactly"

That is an agent reverse-engineering a contract from rejection messages.

## Why the repairs have not held

Three consumer-side normalizations were committed against this:

- `6cfcf68d` normalize guide mode at JSON boundary (adds `wire-keyword`)
- `c77e4f44` normalize JSON predicate key aliases (adds `normalize-predicate-keys`)
- `152c51c3` lift guide mode from the typed channel-audit evidence object

I verified the first two are live in the running JVM -- both resolve --
so the type-coercion symptom is genuinely addressed for `"store-mode"` and
`":store-mode"`. That work is sound and worth keeping.

But `152c51c3` lifts `(select-keys (:channel-audit report) [:mode :candidates])`,
which yields nothing from a string, and no amount of parser normalization
repairs an emitter that was never told the field exists. The normalizations
fix the encodings the guide happened to guess; they do not stop the next
guide writing a paragraph.

This is the same defect class as `TN-opus-F41-analysis.md` section 17, where
the student's work was discarded because `Scratch*.lean` is gitignored and no
card said where work is collected from. Second instance of: **the machine
requires something the role card never states, and the resulting failure is
recorded against the agent.** Note `:repair/fault-origin :agent` in f57's
record -- the machine attributed to the guide a contract it never published.

## What to change

1. **The guide card must specify `channel-audit`** -- that it is an object,
   its required keys, and that `:mode` must equal the mode the packet
   declared. One worked example in the card would have prevented all five
   frames. Cards pin by git blob, so this waits for a pause rather than
   churning the running campaign.
2. **Validation should say what it wanted.** `:guide-mode-authority-mismatch`
   does not tell the guide that `channel-audit` must be a map, which is why
   the repair loop produced three guesses instead of a fix. A finding that
   names the expected shape converts a five-frame outage into one retry.
3. **Do not dispose a sixth frame `:partial` on this cause.** The disposition
   is correct in that no math was lost; it is wrong as a response, because it
   returns the queue to the same defect.

## What f58 additionally shows, worth its own look

codex-17's f58 record reports that the guide correctly returned no candidates
-- the student had closed aunk04 zero-sorry and the same-problem memories
were withheld by design, and the pinned card permits silence -- and that the
controller required a nonempty candidate vector anyway, its repair prompt
"thereby forcing candidate production". If that reads back, the apparatus
pressured an agent into manufacturing deposits it had correctly declined to
make, which is a supply-integrity problem rather than a formatting one. I
have not verified it independently; it deserves its own pass.

## Addendum, same investigation: f58 verified, and a worse defect underneath

I said the f58 "forced candidate production" claim deserved its own pass.
Having run it, codex-17's *conclusion* is right and its *evidence* is not.

### What f58's artifacts actually show

codex-17's record says "The first Guide correctly returned no candidates:
the Student had closed the problem... The controller instead required a
nonempty candidate vector." The first half is not supported.

The student did close aunk04 (`student-attempt-1.edn`, `:outcome "success"`,
committed head `7ead4b45` differing from base `9fa428f7`). But the guide's
first submission did not decline to deposit -- **it failed to parse**:

    :finding/details {:guide-candidates-invalid [:candidates-missing]}
    :report/error {:error/code :report-edn-lint-failed
      "1:43: Invalid symbol: turn:."
      "4:37: Invalid number: 6-14."     ;; an EN-DASH, U+2013
      "4:41: A single colon is not a valid keyword."
      "5:51: :syntax-quote node expects 1 value."   ;; a backtick
      :linter/exit 3}

Four lint errors, and their character is the tell: an en-dash inside a
numeric range, a stray colon, a backtick. That is careful English prose
typed into a slot that must be EDN -- the same failure as the prose
`channel-audit` above, in a different field. `:candidates-missing` here means
"nothing parsed", not "the guide judged there was nothing worth depositing".

So f58 does not demonstrate the apparatus overriding a guide's correct
silence. It demonstrates, a third time, a guide writing prose where the
machine requires data.

### But the defect codex-17 was reaching for is real, and is worse

Independent of f58, the contract genuinely contradicts itself. The pinned
card for this frame (blob `773a8d6d`, verified against the packet's
`:role-card-blob`) says at lines 126-127:

> A round with nothing that passes this test deposits nothing and says so.
> That is a legitimate outcome and always has been — filler is worse than
> silence

The apparatus forbids exactly that:

- `promotion_pipeline.clj:119` -- `(not (and (vector? candidates) (seq candidates)))` -> `:candidates-missing`, so an empty vector is invalid;
- `live_promotion.clj:247` -- the deposit prompt instructs "non-empty vector `:candidates`";
- `live_learning_phases.clj:467` -- `(seq (:candidates report))` with mode other than store-mode -> `:guide-candidates-outside-store-mode`.

Read together: in harness-mode candidates must be empty, in store-mode they
must be non-empty, and the card says depositing nothing is legitimate. **A
store-mode guide with nothing worth depositing has no valid submission
available.** Its choices are to invent filler or to fail the frame, and f58's
repair prompt told it which: "submit a non-empty candidate vector".

That is a supply-integrity defect, not a formatting one. The campaign's whole
claim rests on deposits being things an agent actually judged worth
depositing; a gate that refuses silence manufactures the opposite. It is also
the sharper form of the pattern in this note -- not merely a contract the
card fails to state, but a contract the card **contradicts**.

### Recommended, in order

1. Permit an explicit empty deposit in store-mode -- a `:candidates []` with
   a stated reason, or a `:deposit/declined` with a reason -- so that the
   card's "deposits nothing and says so" has a way to be said. Until then the
   card and the machine cannot both be obeyed.
2. Do not treat an EDN lint failure as a semantic finding.
   `:candidates-missing` for an unparseable report sent codex-17 to a
   conclusion about guide judgement that the artifact does not support, and
   would mislead any reviewer reading the finding alone.
3. Then the card fix from the main note, which addresses all three prose
   incidents at once.
