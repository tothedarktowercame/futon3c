# TN: Tickle CT Review Pipeline — Retrospective

Date: 2026-03-01
Context: Two-session experiment pairing Claude (reviewer) with Codex (author)
via Tickle (IRC orchestrator) to build a PlanetMath category theory corpus.
Repo: `tothedarktowercame/18_Category_theory_homological_algebra`.

## What Happened

Tickle-1 (stateless IRC bot) orchestrated Codex to generate PlanetMath-format
category theory proposals in batches of 5, push them as GitHub PRs, then page
Claude for review. Claude reviewed on GitHub, posting APPROVE or
REQUEST_CHANGES verdicts with per-entry assessments.

**Scale**: 17 PR reviews, ~85 individual proposal assessments, 10 APPROVEs,
7 REQUEST_CHANGES. ~15 duplicate entries caught, 1 off-topic entry flagged.

**Timeline**: Batches 7-10 (modern arXiv 2602.* papers), then classic 1998-1999
papers, then a consolidation phase (PR #52) mopping up stalled entries.

## What Worked

### 1. Cross-Surface Orchestration Held Up

Three agents on three different surfaces (Tickle on IRC, Codex on GitHub,
Claude on Emacs+IRC+GitHub) coordinated through a shared IRC channel and
GitHub PRs. No central controller beyond Tickle's paging. Rob participated
from Houston as a human relay when needed. The surfaces-as-contracts model
(I-1/I-3) worked — each agent inhabited its surface rather than delegating.

### 2. Quality Feedback Loop Was Real

Early batches (PRs #21-#23) had formulaic proposals: two MathOverflow cues
with sequential fabricated-looking IDs, two Math.SE cues, thin synopses.
After REQUEST_CHANGES on PRs #30, #32, #33, #34, #35 with specific feedback,
quality improved substantially. By PR #52, synopses were richer, MO IDs
looked realistic (e.g., MO 503272 vs MO 49620x), and action items were more
specific. Codex responded to review feedback across batches.

### 3. Duplicate Detection Caught Real Problems

Systematic checking of arXiv IDs against (a) existing 313 .tex entries on
main, (b) `docs/proposals/` on main, (c) open PRs, and (d) finalized entries
that referenced the same paper caught ~15 duplicates that would have silently
entered the corpus. The checking protocol was: grep arXiv ID in repo root,
grep in docs/proposals/, check each open PR's file list.

### 4. Format Evolution Happened Organically

Proposals started as outlines in `docs/proposals/`. By PR #50, Codex
shifted to full finalized PlanetMath entries at the repo root. This was the
right direction — the intermediate proposal format added a manual promotion
step with no clear value. The shift happened without explicit instruction,
just through the review feedback pattern.

### 5. Classic Papers Were Higher Quality

Batches covering 1998-1999 foundational papers (PRs #36, #37, #43, #46) were
uniformly excellent — all approved without changes. More established papers
have clearer significance, making synopsis writing easier. This suggests
the pipeline works best when the source material has settled context.

## What Didn't Work

### 1. Stateless Tickle Sent Duplicate Pages

Tickle-1 has no memory between ticks. It would re-page Claude for PRs already
reviewed, send the same message 3x, and lose track of which PRs were pending
vs. resolved. This created noise but was manageable because Claude and Codex
maintained their own state. Still, a stateful orchestrator would eliminate
~40% of the IRC traffic.

### 2. Stalled PRs Caused Cascading Duplicates

PR #22 was approved but never merged (Codex couldn't merge its own PRs without
maintainer action, and the "can't approve your own PR" GitHub limitation meant
reviews were posted as comments, not formal approvals). While #22 sat open,
Codex regenerated the same arXiv IDs in PRs #30, #32, and #35. Each required
a new REQUEST_CHANGES review pointing at the stalled PR. This was the single
biggest source of wasted work.

**Root cause**: No merge automation. Approved PRs needed a human or a bot with
merge permissions to actually land them.

### 3. GitHub "Can't Approve Own PR" Limitation

Because the `gh` CLI authenticated as the repo owner, every `gh pr review
--approve` failed with "Can not approve your own pull request." Reviews had
to be posted as comments instead. This meant GitHub's PR state machine never
reflected the actual review status — PRs showed "no reviews" even after
thorough assessment. This confused both Codex and Tickle.

### 4. Codex Got Stuck / Went Silent

At least once, Codex acknowledged a task ("Working on it now") then reported
"No work has started" and produced nothing. Rob had to relay messages manually.
Joe eventually had to revive Codex from the Emacs surface. There's no
automatic health check or liveness signal — when Codex goes silent, the
pipeline stalls with no feedback until a human notices.

### 5. Off-Topic Entry Slipped Into a Batch

PR #35 included arXiv:2602.13695 (an AI pipeline for math competition
problems) which had no category theory content despite being listed under
math.CT on arXiv. Caught in review, but this shows the proposal generation
has no topical validation step — it trusts arXiv's primary classification
without checking actual content.

### 6. Mixed Batches Blocked Good Entries

When a batch of 5 had 3 approved + 2 rejected entries, the entire PR stalled
because REQUEST_CHANGES blocked the merge. The approved entries couldn't land
until Codex fixed or removed the rejected ones. This was eventually solved by
PR #52's consolidation approach (cherry-pick all approved entries into one PR),
but that was a manual workaround.

## What To Try Next

### 1. Stateful Tickle (Haiku-based)

Replace the stateless tick loop with a Haiku-powered orchestrator that tracks:
- Which PRs are open, reviewed, merged
- Which arXiv IDs have been proposed (dedup at source)
- Which batches are in flight vs. complete
- Health/liveness of each agent

This is already planned as the next Tickle iteration.

### 2. Merge Bot or Separate Reviewer Account

Either: (a) set up a GitHub Action that auto-merges PRs with an "approved"
label, or (b) use a separate GitHub account for reviews so `gh pr review
--approve` works and GitHub's merge machinery engages properly.

### 3. One Entry Per PR

Instead of batching 5 proposals per PR, use 1 entry per PR. This eliminates
the mixed-batch stalling problem entirely. Slightly more GitHub overhead but
dramatically simpler review flow — each PR is either approved+merged or
rejected, never partially blocked.

### 4. Pre-Submission Duplicate Check

Before Codex creates a PR, have it run the same duplicate-detection protocol
Claude uses: grep arXiv ID against existing entries, open PRs, and proposals.
This can be a tool or a pre-commit hook. Would have prevented ~15 of the
duplicates caught in review.

### 5. Topical Validation Gate

Add a lightweight check (keyword scan or abstract review) that verifies a
paper actually contains category theory content before proposing it. The
arXiv primary classification is necessary but not sufficient.

### 6. Liveness Heartbeats

Implement the `liveness-heartbeats` pattern from the futon3c realtime library.
Each agent emits a periodic signal; Tickle monitors for gaps and escalates
(or restarts) when an agent goes silent. This would have caught the Codex
stall automatically instead of requiring human intervention.

### 7. Morning Report

After an overnight run, generate a summary: entries processed, entries merged,
entries rejected, duplicates caught, coverage delta. Post to IRC and/or
email. This closes the loop for humans who weren't watching the overnight run.

## Metrics

| Metric | Value |
|--------|-------|
| PRs reviewed | 17 |
| Individual proposals assessed | ~85 |
| Approved (clean) | 10 PRs |
| Request changes | 7 PRs |
| Duplicate entries caught | ~15 |
| Off-topic entries caught | 1 |
| Entries merged to main | ~45 (proposals) + ~15 (finalized) |
| Classic papers (1998-1999) | 20 entries, 100% approval rate |
| Modern papers (2602.*) | ~65 entries, ~70% approval rate |
| Time span | ~2 sessions, ~6 hours total |

## Key Insight

The pipeline's value isn't in the individual proposals — it's in the
**review feedback loop**. Codex improved measurably across batches because
Claude's reviews were specific ("this arXiv ID duplicates PR #22 entry 3",
"synopsis is too thin — compare with the finalized 18D50 entry"). This is
the social AIF loop working as designed: agents coordinating in real time,
with quality emerging from the interaction rather than from either agent alone.

The main bottleneck was infrastructure (merge permissions, PR batching,
statefulness), not intelligence. Both agents performed their roles well —
the coordination layer just needs to be smoother.
