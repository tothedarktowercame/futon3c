# T-kangaroo-caching-and-compaction — TOMORROW (2026-06-11)

Parked at end of day 2026-06-10. Three linked follow-ons to the joey gate
(commit 042fad2) and the prompt-caching accounting. Do NOT start without Joe.

## Context
A pouch keeps the *process* warm but cannot keep Anthropic's server-side prompt
cache warm. Default cache TTL is 5 min (extendable to 1 hr). Cache read ~0.1×,
write 1.25× (5-min) / 2× (1-hr). Joe runs **long turns (30-min timeout)**, so the
gaps between his turns routinely exceed the 5-min TTL → full-history re-prefill
each turn. (Grounded via the /claude-api skill; see [[project_kangaroo]].)

## 1. 1-hour caching — does the `claude` CLI expose the TTL knob?
`cache_control: {ttl: "1h"}` is set per-request by whoever builds the API call.
For Joe's interactive CLI **and** the pouch agents that's the **`claude` CLI**,
not Agency code. **First step:** find whether Claude Code exposes a cache-TTL
setting (env var / settings.json / flag — check `claude --help`, settings docs,
or ask claude-code-guide). If yes: Joe flips it for his session and the pouch
(`spawn-pouch!` in agent_pouch.clj) passes it through. If no: it's a Claude Code
feature question, not an Agency change. The economics favor 1-hr for Joe's
long-gap cadence (1-hr needs ≥3 reads to beat uncached; his pattern clears that).

## 2. Joey-threshold calibration
Default `FUTON3C_KANGAROO_JOEY_MAX_BYTES` = 2 MiB (a guess). Calibrate against
real sessions: measure transcript bytes vs actual cold-resume time/tokens for a
spread of agents (fable-1 5.8MB ≈ 67s is the anchor monster). Pick the line where
warming stops paying off. `pouch/snapshot` now reports `:session-bytes`/`:joey?`
per agent — use it to survey the live roster.

## 3. Compaction trigger
The real fix for a monster Joe wants warm = shrink it to a joey via compaction
(server-side compaction beta, `compact-2026-01-12`, summarizes earlier context).
Question: can we trigger/encourage compaction for a pouch/session so a monster
becomes a joey, rather than just gating it cold? Again likely a Claude Code /
harness capability question first (does the CLI expose compaction control?),
then how Agency would invoke it.

## FINDINGS (2026-06-11, claude-code-guide research + `claude --help` probe)
Both knobs are **env vars**, NOT CLI flags (confirmed: `claude --help` has neither).

**TTL — `ENABLE_PROMPT_CACHING_1H=1`** → 1-hour cache TTL for API-key users (default
5-min). `FORCE_PROMPT_CACHING_5M=1` forces 5-min. Subscription users get 1h auto.
Source: code.claude.com/docs/en/env-vars.md + prompt-caching.md. → Set in
dev-laptop-env (JVM + pouch children inherit) AND Joe's interactive shell. Biggest
win for the 30-min cadence.

**Compaction:**
- Auto-compaction DOES run in `--print --input-format stream-json` (headless/pouch)
  mode, default ~95% of context. Tune: `CLAUDE_AUTOCOMPACT_PCT_OVERRIDE=N` (compact
  earlier) or `CLAUDE_CODE_AUTO_COMPACT_WINDOW=<tokens>`. Disable:
  `DISABLE_AUTO_COMPACT=1` / `DISABLE_COMPACT=1`.
- **No on-demand trigger in headless** — `/compact` is interactive-only; no
  stream-json stdin control message. So Agency can't force-compact a pouch; only
  tune the auto-threshold to keep pouches leaner.
- `--resume` inherits prior compaction (resumed context IS the summary), but the
  first resumed turn is a cache miss.

## Design implication
1h-caching largely fixes the ACUTE token-bleed (5-min cache always expiring across
Joe's 30-min gaps → full re-prefill every turn). With it, the joey gate's urgency
RELAXES — it becomes spawn-TIME hygiene (`.jsonl` bytes ≈ resume disk-read/spawn
cost), while 1h-caching + auto-compaction handle per-turn TOKEN cost. Calibration:
the joey threshold can probably go UP now. Subtlety to verify: the `.jsonl` is an
append log — it likely does NOT shrink after compaction, so a "monster `.jsonl`" can
be slow to spawn yet cheap per-turn (compacted context) — which is exactly what a
spawn-time gate should still catch, but the token-trap fear is gone.

## Disposition
Research DONE. Next: set `ENABLE_PROMPT_CACHING_1H=1` (low-risk, high-value), then
recalibrate the joey threshold upward. Compaction needs no action (auto-on); tune
`CLAUDE_AUTOCOMPACT_PCT_OVERRIDE` only if we want leaner pouches.
