# E-usage-aware-dispatch — pace dispatch against each provider's usage window

Date: 2026-09-25
Owner: claude-10. Driver: Joe.
Status: OPEN — written down, nothing built.

## Joe's framing (2026-09-25, voice, lightly cleaned)

Things have been going fine for a while, and this time the Kimi 5-hour
window was not used up faster than the Opus and Fable work, as far as he can
tell. It would be good to keep track of that, so that things can run
continuously without running into a 5-hour window. The goal: burn the usage
down over each window without quite using it all up. Today there is no usage-
aware dispatch and no timing. This is for exploring later, not something to
fix now.

## What exists

- **The readings.** voxterm `server.py` `collect_usage()` (served at
  `GET /usage`, cached 120 s, `VOXTERM_USAGE_TTL`) reads every provider's
  windows: Claude (`/api/oauth/usage`: `five_hour` and `seven_day`
  utilization and reset), Codex (from its app-server turns), Z.ai
  (`/api/monitor/usage/quota/limit`), Kimi (`/coding/v1/usages`: `limits[]`
  for the 5-hour pool, preferred over `usages.*` because the two disagree on
  an exhausted window). Each provider comes back with `session_used_pct` /
  `weekly_used_pct` (or monthly) and the matching `*_resets_at`, normalised
  to epoch seconds. One provider's failure is its own `:error`, never a
  missing row.
- **Where they reach.** Only the voxterm usage strip, for Joe to look at.
- **Dispatch.** Leads (claude-8 for PROOF-2, and others) choose seats from
  the Agency roster (`GET /api/alpha/agents`) by idleness and task fit, and
  send with `scripts/agency_send.py`. Nothing in that path reads a usage
  window. A window running out is found when jobs fail (the Kimi drain of
  2026-09-24: seven jobs HTTP 403 on the 5-hour limit, and the work moved to
  claude-2 by hand).

## The missing piece

The readings exist and dispatch does not read them. Two definitions are
missing:

1. **Pace.** For a provider and window: used fraction `u` against elapsed
   fraction `e` of the window (from its reset time and length). `u − e > 0`
   is ahead of pace (on course to run out before the reset); `u − e < 0` is
   capacity the window will not use. A provider whose reading failed has a
   typed-absent pace, never zero.
2. **A dispatch rule that uses it.** Among seats fit for a task, prefer the
   provider furthest behind pace; flag (do not block) a dispatch to a
   provider on course to exhaust before its reset, naming the reset time.

## Items

- **I1. Pace as data.** Compute pace per provider and window from
  `collect_usage()`'s output and expose it beside the readings (voxterm
  `/usage`, or an Agency route that reads the same). Bad case: a window
  just reset (`e` near 0, `u` 0) must not read as far behind pace in a
  way that floods it; a failed reading must stay absent.
- **I2. Record pace on each dispatch.** `agency_send.py` (or the Agency
  invoke route) writes the pace reading of the chosen seat's provider on the
  job record. No behaviour change; it makes I4 answerable.
- **I3. Prefer by pace.** A roster helper that, given a task's acceptable
  seat kinds, orders idle seats by pace. Advisory: the lead still chooses;
  the helper's order and the choice are both recorded.
- **I4. Did it work.** Over a few days of I2 records: how often a provider
  ran out before its reset, and how much of each window went unused at
  reset. That is the works/doesn't-work measure.

## Not in scope

No gate. A dispatch is never refused for pace (the standing rule against
new refusals applies: a warning and a record, not a hurdle). No change to
which work is chosen; only which seat does it.

## Relations

- `futon2/holes/E-outer-loop.md`: the outer loop is where the machine
  would eventually do this itself, choosing seats as well as targets.
- voxterm commits 78d99b1, 0b9822c (reading Kimi's quota and its exhausted
  window correctly) are what make the Kimi reading trustworthy enough to pace
  against.
