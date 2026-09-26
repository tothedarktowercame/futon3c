# Attachment approvals — a94A09 scribe pass

> **Status (2026-08-10): review COMPLETE.** Reviewed by `claude-2`, not
> `claude-10` (Dionysus seats down; Joe reassigned the role). Two approvals and
> one rejection are recorded in the substrate — see "Review pass 2 —
> completion" at the end of this file. **The helper below was NOT used**: it
> hardcodes `claude-10` as author, smears the session id, and cannot express a
> rejection. It is retained only as the record of what was originally proposed.

The memories are authored by `ams-scribe-1`. After reviewing the attachment
table in `README.md`, the reviewer appends independent review evidence and
applies it with the established lifecycle script.

From `/home/joe/code/futon3c`, define:

```bash
approve_a09_attachment() {
  memory_id="$1"
  pattern_id="$2"
  review_id="e-review-claude10-${memory_id#e-a09-}"
  reviewed_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  curl --fail --silent --show-error \
    -H 'Accept: application/edn' \
    -H 'Content-Type: application/edn' \
    -H 'X-Penholder: api' \
    --data-binary "{:evidence/id \"${review_id}\"
                    :evidence/subject {:ref/type :memory :ref/id \"${memory_id}\"}
                    :evidence/type :memory
                    :evidence/claim-type :observation
                    :evidence/author \"claude-10\"
                    :evidence/session-id \"M-zai-learning-loop/a94A09-attachment-review\"
                    :evidence/at \"${reviewed_at}\"
                    :evidence/body {:review/event :memory-attachment-review
                                    :review/memory-id \"${memory_id}\"
                                    :review/pattern-ids [\"${pattern_id}\"]
                                    :review/verdict :approve
                                    :review/witness-status :independently-witnessed
                                    :review/provenance {:kind :owner-attachment-review
                                                        :reviewer \"claude-10\"
                                                        :date \"2026-08-10\"
                                                        :warrant \"Reviewed a94A09 scribe attachment and its per-memory justification.\"}}
                    :evidence/tags [:memory :memory/review :mathematics]}" \
    http://127.0.0.1:7073/api/alpha/evidence
  ATTACHMENT_REVIEWER=claude-10 \
    clojure -M scripts/review_codex_lane_attachments.clj --commit \
      --memory-id "$memory_id" \
      --review-evidence-id "$review_id" \
      --pattern-id "$pattern_id"
}
```

Exact pending calls:

```bash
approve_a09_attachment e-a09-shrink-radius-rouche-fixed-point math/holomorphic-disk-api
approve_a09_attachment e-a09-compact-endpoint-limit-for-scaled-fixed-points math/holomorphic-disk-api
approve_a09_attachment e-a09-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points math/missing-dependency-protocol
```

---

## Review pass 1 (claude-2, Opus, 2026-08-10 15:26Z) — transcribed verbatim by ams-claude-1

The reviewer seat lacked Write permission; the following is its full report,
pasted unmodified from job invoke-1786375254246-3527-60c53844.

I'll start by reading the review materials.I have everything I can verify locally. Now appending the review note.Write permission was also denied, so the review note could not be appended to `APPROVALS.md`. Full review below — it's ready to paste in verbatim.

## Verdicts

| Memory | Verdict |
|---|---|
| `e-a09-shrink-radius-rouche-fixed-point` | **REJECT** — understates the trapped surface (2 blocking defects) |
| `e-a09-compact-endpoint-limit-for-scaled-fixed-points` | **Content verified, approval WITHHELD** — no defect found, but 2 mandated axes unexecutable |
| `e-a09-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points` | **Content verified, approval WITHHELD** — same |

No approval call was executed (see Blockers).

## What I checked

Read in full: `apm-lean/problems/a94A09/lean/Main.lean` (422 lines), `ConstructionTargets/Rouche.lean` (decl list + the theorem statement), `ConstructionTargets/SchwarzEquality.lean`, `problems/a94A09/{status.json,proof-outline.md}`, `LEMMA-INDEX.md` (targeted), `futon3c/scripts/review_codex_lane_attachments.clj`, this lane's `promote.clj`.

- **Commit** — `git` unavailable, so confirmed from `apm-lean/.git/logs/HEAD:2072`: `0195fb92… → 22c5b80c064ae36e83a3b8759607ccf430c76169`, epoch `1786369934` (2026-08-10 ~13:52Z), subject "a94A09 compile scaled fixed-point existence", current reflog tip. `Main.lean` mtime agrees.
- **Importability** — `ConstructionTargets.Rouche.zeroCountInClosedBall_add_eq` exists at `Rouche.lean:443` in `namespace ConstructionTargets.Rouche`, signature matches `LEMMA-INDEX.md:2111` (a `LIB:` row), file is sorry-free. Claim holds.
- **Mathlib names in draft 2** — `IsCompact.tendsto_subseq` (`Topology/Sequences.lean:268`), `tendsto_natCast_div_add_atTop`, `tendsto_nhds_unique` all present. ✔
- **Trapped marking** — `apm_a94a09_exists_scaled_fixed_point` (`Main.lean:239`) and `apm_a94a09_exists_closedBall_fixed_point` (`:284`) re-read; both statements use only Mathlib vocabulary, so "mentions no `apm_` definitions" is accurate, and both are correctly marked trapped.
- **n= / honesty** — `n=1` + uniqueness disclaimed, consistent with the `sorry` at `Main.lean:419` and `status.json` `sorry_count_total: 1`. Draft 2's `closedBall → ball` upgrade matches `:330–339`. Draft 3's `phi_z` matches `:410`; I independently confirmed Mathlib has no `Schwarz.*Pick` anywhere and its only `UnitDisc` file (`Analysis/Complex/UnitDisc/Basic.lean`) is coercion/algebra API with no Möbius — draft 3's "upper half-plane only" claim is correct, and `ConstructionTargets.SchwarzEquality` supplies exactly the affine equality case it says is available.

## Draft 1 — why it's rejected

**D1.** The memory says: *"Apply the importable theorem …`zeroCountInClosedBall_add_eq`…; the identity has zero count one, so the sum has a zero."* The Rouché module exports **neither** the identity zero-count **nor** zero-extraction. `Main.lean` gets there via four problem-local lemmas: `apm_a94a09_zeroCount_aeval_eq_card_filter` (`:101`, ~80 lines, adapted from `a92J05`), `apm_a94a09_zeroCount_id` (`:186`), `apm_a94a09_exists_zero_of_zeroCount_ne_zero` (`:200`), `apm_a94a09_exists_zero_id_add_of_rouche` (`:222`). The problem's own `proof-outline.md:49–50` says this verbatim. The memory's importability section names only *one* trapped lemma, so a reader following it in a fresh problem hits exactly the gap the memory hid.

Corroboration that this is the reusable content: `LEMMA-INDEX.md:2112–2113` already lists that polynomial bridge as problem-local under **both** `a92J05` and `a97A08` — `a94A09` is the **third** re-derivation.

**D2.** `‖f z‖ ≤ 1` on `sphere 0 r` is attributed to "the disk-map bound" with no step named. `r < 1` is *interior*, so the frozen boundary hypothesis doesn't reach it; `Main.lean:260–270` uses `apm_a94a09_mapsTo_closedBall` (`:85`), i.e. maximum modulus via `Complex.norm_le_of_forall_mem_frontier_norm_le`. That's the step that makes shrinking the radius work, and it's missing.

Correct and not at issue in draft 1: `r=(t+1)/2` (`:246–249`), the `AnalyticOnNhd` upgrade (`:250–256`), the strict inequality (`:274–276`), the honesty bound.

**Required amendments:** name the four trapped bridges + state the extraction is not exported (flagging the 3-problem recurrence as the promotion case); name the max-modulus step and its importable theorem.

## Defects in the approval procedure itself

1. **False witness identity.** The helper hardcodes `claude-10` in four places (`:evidence/author`, `:review/provenance`, `ATTACHMENT_REVIEWER`, the `e-review-claude10-…` id). claude-10 did not do this review; I did. Running it as written writes a false record.
2. **Session smear.** `review_codex_lane_attachments.clj:117` hardcodes `:session-id "M-codex-sorry-loop/duree"`, contradicting the `M-zai-learning-loop/a94A09-attachment-review` in the evidence body.
3. **No rejection path.** That script hardcodes `:verdict :approve` (`:147`). Draft 1's rejection can't go through it — its edge must stay `:attachment-status :proposed`.

## Blockers

This seat has no permission for `curl`, `git`, or `Write` (denied even with the sandbox override). So:

- **Runner result unread** — `GET …/jobs/invoke-1786369654355-3517-0994cab1` unreachable; the `:result` field, the "not degraded under load" claim, and both witness receipts are unverified.
- **Near-duplicate check not run** — no text-search against 7073; the scribe's near-duplicate claims (including the "adjacent but not duplicated" reading of `e-codexpilot-package-every-rouche-homotopy-slice-for-the-argument-principle`) are unverified.
- **"Compiled" inferred, not observed** — no build run, no `.olean` for a94A09. Note `Main.lean` still has a `sorry`, which compiles with a warning, so the claim is consistent with but not established by the artifact.

Drafts 2 and 3 look clean, but approving them would assert `:review/witness-status :independently-witnessed` with two mandated axes unchecked — hence withheld, not denied. Grant this seat localhost `curl`, `git -C /home/joe/code/apm-lean`, and write access to the lab dir, and I'll finish the remaining checks and run corrected approval calls under `claude-2`.

---

## Review pass 2 — completion (claude-2, 2026-08-10 15:47Z)

Written by claude-2 directly. Permissions reached this seat on the fourth
invoke (root cause: `register-agent!` on an existing id does not replace the
invoke-fn; the missing `update-agent!` meant three passes ran under the
project read-only allowlist). Both blocked axes have now been run.

### Axis (a) — runner result and witness receipts: PASS

`GET :7070/api/alpha/invoke/jobs/invoke-1786369654355-3517-0994cab1` —
`state :done`, `agent-id "ams-codex-2"`, `artifact-ref` = `22c5b80c…`,
`executed true`, 34 tool events, started 13:47:34Z, finished 13:52:33Z.

The `:result` field settles the one claim I had marked *inferred, not
observed* in pass 1:

- `lake env lean problems/a94A09/lean/Main.lean` → **Exit 0**.
- Axiom audit: `apm_a94a09_exists_scaled_fixed_point` and
  `apm_a94a09_exists_closedBall_fixed_point` each depend on
  `[propext, Classical.choice, Quot.sound]` — **no `sorryAx`**. Both compiled
  lemmas are genuinely sorry-free; the surviving `sorry` is confined to
  uniqueness, exactly as the drafts claim.
- Initial sorry count 1, final sorry count 1. Consistent with `status.json`.
- The runner's `Memory usage` section matches `README.md` verbatim: `USED
  e-codexpilot-package-every-rouche-homotopy-slice-for-the-argument-principle`,
  `IGNORED e-codexpilot-prove-exponential-cubic-injectivity-by-linear-term-domination`.
- Arm (b), Zulip, "returned no disk-automorphism, Schwarz–Pick, or
  two-fixed-point bridge"; arm (c), arXiv, not needed. Grounds draft 3.

Receipts, both fetched from `:7073`:

- `e-fab2e3d9-6877-444a-9949-a11720305918` — `:phase :offered`,
  `:recall-status :ok`, `:recall-ladder-query "rouche"`, ladder rung
  `:required-term`, `:withheld-memory-ids []`, `:receipt-ranking {… :degraded?
  false …}`, surfaced ids = exactly the two the README names. The "not
  degraded under load" claim in the hunger audit **holds**.
- `e-memory-outcome-sweeper-6e8a041ab7506a025951c3b4` — `:phase :outcome`,
  `:recall-outcome :completed-with-memories`, `:memory-use/used-ids` = the
  Rouché packaging memory, `:memory-use/unused-ids` = the injectivity memory.

One classification nuance, not a scribe error: the sweeper files the ignored
memory under `:unused-ids` with `:rejected-ids []` and
`:rejection-reasons []`, even though the runner gave an explicit reason. The
README's "ignored … with a reason" tracks the runner's own text, which is the
right source; the sweeper's `unused` vs `rejected` split just doesn't carry
the reason through.

### Axis (b) — near-duplicate searches: PASS

`GET :7073/api/alpha/evidence/text-search`, results filtered to
`:evidence/type :memory`:

| Query | Memory-typed hits | Bearing |
| --- | --- | --- |
| `compact endpoint limit scaled fixed points subsequence` | only `e-a09-compact-endpoint-limit-for-scaled-fixed-points` itself | no pre-existing duplicate |
| `closed ball fixed point radial contraction` | same, plus two `:coordination` chat rows | no pre-existing duplicate |
| `disk automorphism Schwarz Pick two fixed points` | only `e-a09-open-hunger-…` itself | literal query unoccupied |
| `zeroCountInClosedBall homotopy invariant` | `e-codexpilot-zeroCountInClosedBall-homotopy-invariant`, `e-codexpilot-bridge-logarithmic-derivative-circle-integral-to-divisor-count` | both concern the *formerly missing* argument-principle bridge, as README says |
| `consultation discard reason` | `e-j07-record-consultation-discards-with-reasons` exists | declining the desk-research candidate as a duplicate was correct |

**Adjacency reading — confirmed, and the README understates its own case.**
I read `e-codexpilot-package-every-rouche-homotopy-slice-for-the-argument-principle`
in full. It packages `AnalyticOnNhd` + boundary-nonvanishing for the
straight-line family `f + t·g` on a *single fixed* `closedBall c R` /
`sphere c R`, and its own `:boundary` field reads "`zeroCountInClosedBall_add_eq`
remains sorried" — it predates the completion of the Rouché module. The new
strategy's moving contour `t < r < 1` is different content. Not a duplicate.

Worth carrying forward: that older memory records an
`:axiom-audit {… :sorryax false}`. None of the three a09 drafts do, even
though the runner result supplies exactly that audit. Cheap, and stronger
evidence than "compiled".

### Evidence written (author `claude-2`, session `M-zai-learning-loop/a94A09-attachment-review`, all `:evidence/at "2026-08-10T15:47:40Z"`)

| Evidence id | Verdict | Read-back |
| --- | --- | --- |
| `e-review-claude2-compact-endpoint-limit-for-scaled-fixed-points` | `:approve` | ✔ 3677 bytes, author `claude-2`, `:independently-witnessed` |
| `e-review-claude2-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points` | `:approve` | ✔ 3021 bytes, author `claude-2`, `:independently-witnessed` |
| `e-review-claude2-shrink-radius-rouche-fixed-point` | `:reject` | ✔ 4517 bytes, author `claude-2`, `:independently-witnessed` |

Each row carries a per-axis `:checks` vector recording *what was verified*,
not just the verdict. Written straight to `POST :7073/api/alpha/evidence`
(`X-Penholder: api`), bypassing the defective helper: no `claude-10`
attribution, correct session id, and a `:reject` verdict the helper cannot
express. The rejection row is deliberate — it puts draft 1's reasons in the
substrate rather than only in this file.

### Attachment status: UNCHANGED — all three still `:proposed`

Read back after the writes:

```text
hx-a09-shrink-radius-rouche-fixed-point                            :proposed :self-asserted
hx-a09-compact-endpoint-limit-for-scaled-fixed-points              :proposed :self-asserted
hx-a09-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points :proposed :self-asserted
```

**Read "approved" here as: review evidence is recorded, not attachment
projected.** Flipping `:proposed → :reviewed` and `:self-asserted →
:independently-witnessed` on the edges is the lifecycle projection
(`memory-lifecycle/review-attachment!`), which today is only reachable through
`review_codex_lane_attachments.clj` — the script whose three defects are filed
for separate repair, and which I was directed not to touch in this pass. Once
it is fixed, drafts 2 and 3 can be projected against the two `:approve` rows
above; draft 1 must not be.

### Final disposition

- `e-a09-shrink-radius-rouche-fixed-point` — **REJECTED.** Defects D1 and D2
  from pass 1 stand and are now *strengthened*: the job result confirms this
  dispatch added only the two new lemmas, so the four bridge lemmas the draft
  passes over came from the earlier closer-hop-4 dispatch. The draft credits
  an importable theorem with work that is trapped **and** pre-existing.
  Amendments required before re-submission are listed in pass 1 and in the
  `:required-amendments` field of the rejection row.
- `e-a09-compact-endpoint-limit-for-scaled-fixed-points` — **APPROVED**, three
  non-blocking nits recorded.
- `e-a09-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points` —
  **APPROVED**, one non-blocking nit recorded.

Also still open from pass 1, unaddressed by this pass and not in its scope:
`LEMMA-INDEX.md` was not updated with the six new `a94A09` lemmas from
`22c5b80c`, and `status.json` / `proof-outline.md` still describe the
closer-hop-4 state ("still needs the t-to-1 compactness assembly") that this
commit superseded. The index is what runners search, so the omission has
teeth.
