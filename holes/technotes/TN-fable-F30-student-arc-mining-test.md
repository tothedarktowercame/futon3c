# TN: one-off test — are the F30 Student's self-corrections recoverable as arc-lane rewrite rules?

Author: Claude (Fable 5, `claude-8`), 2026-08-24T20:25Z. Prompted by Joe:
*"Given that F30 is a preserved record we could run a one-off test to see
if the student self-corrections are recoverable."* Companion to
`TN-fable-F29-F30-content-review.md` (which found that nothing mines the
Zai student turns in the countdown machine).

**Short answer: yes.** From the Agency job records alone — no transcript,
no tool outputs — the F30 Student's narration yields **13 leaf rewrite
rules and 3 process memories**, of which **8 are cross-problem general**.
They group naturally under **five design patterns**, each of which is the
kind of `@how` cascade the store is supposed to carry. None of this was
written to the store.

## Method

1. Fetched the three F30 student jobs from `GET /api/alpha/invoke/jobs/<id>`
   (ids from `live/student-attempt-{1,2,3}.edn`). Event shape: `text`
   (the student's narration), `tool_use` (tool name + a ~140-char command
   *preview* — **tool outputs are not stored**), `prompt`, `done`.
   Sizes: attempt-1 14 events; attempt-2 66 (22 text); attempt-3 218 (85
   text, 129 tool calls).
2. Scanned text events for self-correction cues (`error:`, `unsolved
   goals`, `unexpected`, `mismatch`, `no hypothesis`, `Wait —`, `Ah —`,
   `Actually`, `Fix:`, `instead`, …) and paired each with the next tool
   call (the *after*). 44 of attempt-3's 85 text events and 10 of
   attempt-2's 22 carry a cue.
3. Wrote each recoverable span in the scribe-v2 arc-lane schema
   (`scope / before / after / level / confidence / evidence`) by hand, then
   classified by generality and grouped under candidate design patterns.
4. Looked for a fuller transcript: `/tmp/futon-zai-session-id-f30-student`
   holds `zai-c550c259-…`; no on-disk transcript for it was found. So the
   *exact* Lean error text — which `scribe.md` asks for ("capture the
   symptom in the language it was reported") — is available only where the
   student quoted it. That is a retention gap, noted below.

Marks: attempt-3 contains 17 `✓` (self-approval) and **zero `✘` / `💡`**.
The corrections are in prose cues, not marks; a Zai-scribe keyed on marks
alone would have found nothing here.

## The mined rules

Evidence is `job apm-role-615d27b5… seq N` (attempt-3) or
`apm-role-82aafdcb… seq N` (attempt-2). Level: **G** = general Lean/Mathlib
(cross-problem), **A** = Mathlib-API-specific (general within an area),
**L** = local to this proof.

| # | scope | before | after | level | conf | evidence |
|---|---|---|---|---|---|---|
| 1 | `Real.log_pow` | `rw [Real.log_pow (by norm_num : (0:ℝ) < 2)]` → "argument expected ℝ" | `Real.log_pow (x : ℝ) (n : ℕ)` has **no** hypothesis: `rw [Real.log_pow 2 k]` | G | high (probe `p9.lean`) | a3 seq 149–151 |
| 2 | `Real.rpow_le_rpow` | `rpow_le_rpow h1.le hn.1 hα.le` with `h1` about the *larger* base → h₁ mismatch | first arg fixes `x`; give nonneg of the *smaller* base: `rpow_le_rpow (pow_nonneg (by norm_num) k) hn.1 hα.le` | G | high | a3 seq 69 |
| 3 | `inv_le_inv₀` | assumed `(ha : 0 < a) (h : a ≤ b) : b⁻¹ ≤ a⁻¹`, error "expected `0 < ?m`" | positivity arg is the *smaller* term; confirm with `#check @inv_le_inv₀` before use | G | med | a3 seq 74 |
| 4 | `Nat.card` vs `Set.ncard` | `rw [Nat.card_coe_set_eq]` fails on `Nat.card {n // P n}` (subtype display) and inside `((… : ℕ) : ℝ)` casts — hit **six times** | choose one form for the whole proof, or bridge once via `Nat.card_congr (e : α ≃ β)`; never `rw` across the cast | G | high | a3 seq 55, 64, 67, 116, 157, 183, 190 |
| 5 | `Finset.sum_const` | leaves `Fintype.card … • x` (nsmul), later `rw` fails | follow with `nsmul_eq_mul` / `push_cast` | G | high | a3 seq 177 |
| 6 | `Real.rpow` vs `x ^ y` | `rw [Real.rpow_neg …]` "pattern not found" though `#check` unifies them | `rw` is syntactic: after `set f := fun n ↦ Real.rpow …` the goal shows opaque `f n`; unfold `f` (or don't `set`) first. Same family in a2 seq 14–40 (`.rpow` vs HPow display) | G | high (probes p5–p8) | a3 seq 139–145; a2 seq 14, 23, 28, 35 |
| 7 | `locallyFinsuppWithin.single` / `logCounting` | expected `single` to be `…Within`-typed and a `sum_apply` simp lemma to exist | `single` lives in the `…Within` namespace but has type `locallyFinsupp X Y`; `logCounting : locallyFinsupp E ℤ →+ (ℝ → ℝ)`; no `sum_apply` — prove by `Finset.induction` + `coe_add` | A | med-high | a3 seq 13, 15, 60 |
| 8 | `Nat.card_congr` | `apply Nat.card_congr; rintro …` | it takes an `Equiv` (`α ≃ β`), not a bijection; build the equiv or avoid | G | high | a3 seq 64 |
| 9 | `rw` direction | `rw [h3]` with `h3 : ↑⌊u⌋ = ↑⌊u⌋.toNat` to remove `toNat` | `rw [← h3]` | L | high | a3 seq 119, 126 |
| 10 | bracket balance | `refine ⟨_, ⟨?_, ?_⟩, ?_⟩⟩` (extra `⟩`) reported as "unexpected token '⟩'; expected command" **and** a misleading "unsolved goals" at the `refine` | on "expected command" check brackets/unicode with `cat -A`/`xxd` **before** suspecting tactic semantics | G | high (probe) | a3 seq 131–137 |
| 11 | `LT.lt` field access | `hK.not_le` → "invalid field not_le … Real.lt" | `not_le.mpr hK` | G | high | a2 seq 6 |
| 12 | deprecated `mul_le_mul_left'` | `mul_le_mul_left' hz 2` | `mul_le_mul_of_nonneg_left hz (by norm_num)` or `nlinarith` | A | med | a3 seq 53 |
| 13 | `rw` auto-`rfl` | `rw […]` leaves `Real.rpow 2 (1-α) ^ ↑k = (2 ^ (1-α)) ^ ↑k` | defeq at default transparency, not reducible: append `rfl` | A | med | a2 seq 40 |

Process / trajectory memories (not tactic rules):

| # | what | cost | evidence |
|---|---|---|---|
| 14 | Editing `Main.lean` by Python `str.replace` with re-typed unicode (`·`, `↦` vs `→`) silently mismatched; an `assert` before the write discarded the whole batch — "Everything lost again" | ~6 rounds | a3 seq 91, 94, 97, 111, 113 |
| 15 | A replacement that dropped a trailing newline joined two tactic lines and produced an error cascade 100+ lines downstream (577/679) | 2 rounds | a3 seq 105 |
| 16 | *"The compile loop is slow… consider wrapping up with an honest failure account rather than an unbounded proof-fix loop"* — then kept going and never committed a clean tree | the frame's student column | a3 seq 99; ledger `:dirty? true` |

## The design-pattern cascade

These are the general statements the leaves are `@how` instances of.
None is named after a problem, a lemma, or a line number.

- **P1 — Check the signature before you use it.** Explicit vs implicit
  args, hypothesis presence, which side a positivity argument refers to.
  Cheapest probe: `#check @name` in a scratch file. `@how`: 1, 2, 3, 8, 11.
- **P2 — `rw`/`simp` match syntax, not defeq.** Normalise to the displayed
  form first: unfold `set` lets, `push_cast`, pick one of
  `Nat.card`/`Set.ncard`/`Fintype.card` for the whole proof, finish with an
  explicit `rfl`. `@how`: 4, 5, 6, 9, 13.
- **P3 — Parse errors masquerade as tactic failures.** "expected command",
  "unexpected token", "unterminated comment", or an error far from the last
  edit ⇒ brackets / unicode / newlines first. `@how`: 10, 15.
- **P4 — Edit by bytes, not by re-typed strings.** Line-addressed edits;
  never assert-then-write on a whole batch. `@how`: 14, 15.
- **P5 — Submit before the clock.** A compiling partial state on disk beats
  a clean proof in a dirty tree. `@how`: 16; also F29 attempt-1
  (`TN-sonnet-F29-finding`).

P1–P3 are the ones a *Codex* scribe would never produce: Codex did not
stumble on any of them in F30. They came entirely from Zai's stumbles.

## What this says about the two scribes

- **Codex scribe** (`:promote-solver`, mines the certified Solver trace):
  its natural output is *route* memories — which Mathlib API chain the
  solver took and where it departed from the boundary comment. It should be
  judged on generality (no problem identifiers; would help on an unseen
  sibling) and should be producing the **pattern** level, not leaf text.
- **Zai scribe** (should run after the student attempts): its natural
  output is exactly the table above — leaf rewrite rules with a witness,
  hung under P1–P5. It needs (a) the student job traces by `:job-id`,
  (b) `:memory-candidates` authority in the arc lane, (c) cue-based
  extraction (prose cues, not just marks), and ideally (d) tool outputs
  retained so "before" carries the verbatim error.

Two different inputs, two different outputs, two different acceptance
tests ⇒ two role cards. Wiring is a Codex handoff; the card text is not.

## Retention gap (blocking for (d))

Job events keep `previews` of tool calls only. The Zai student's compile
output — the literal error text the arc lane wants — exists nowhere I could
find after the job ends. Until that is retained, a Zai scribe can only
mine what the student chose to quote (which, in F30, was enough for 13
rules, but with weaker "before" sides than the schema asks for).

## One anomaly for the auto-certify fix

Attempt-2's final text (a2 seq 65) reads *"Outcome: success … 0 errors, 0
sorries … the typed submission was accepted"*, and attempt-3 opens with
*"Repair turn — the earlier work ended without a typed submission."* The
ledger has all three attempts `:dirty? true` at base head with no
`sorry-warnings`. Whatever the submission tool accepted at attempt 2, it
did not become a certified head. Worth a look when the submit-early /
auto-certify change is specified.
