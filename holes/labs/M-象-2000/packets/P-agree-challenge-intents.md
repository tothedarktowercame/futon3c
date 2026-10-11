# Packet: map the `agree` and `challenge` reading intents to kernel kinds

From claude-16 (owner, M-象-2000), 2026-10-11. Joe ruled yes (2026-10-11).
Mode: work. Code with tests only; no reload, no live recompute (a separate
packet recomputes live sessions after this and its sibling land).

## Behaviour

象 readings sometimes carry fragment intents `agree` and `challenge`, which the
adapter's `intent-kinds` (`src/futon3c/xiang/turn_acts.clj`, built from
`lx/kinds`) does not know, so those fragments are skipped. Eight records refuse
ports for this reason ("No acts for turn": `turn-5JBr6g`, `turn-JTuY1v`,
`turn-968NVf`, `turn-qX81HV`, `turn-WtIZCU`, `turn-mVytXT`, `turn-VgUJ8K`,
`turn-ABYrM4`). Add an alias map in the adapter: `"agree"` → `:approve`,
`"challenge"` → `:disagree`. The kernel's kind table is unchanged; the readings
are unchanged.

## Tests (`test/futon3c/xiang/turn_acts_test.clj`)

1. A synthetic turn whose only fragments are intents `agree` and `challenge`
   yields two acts of kinds `:approve` and `:disagree`, with `:skipped 0`.
2. An `agree` fragment following a turn whose reply made a 🈸 offer becomes an
   `:accept` of that offer (the existing approve→accept path applies to the
   alias too).
3. A still-unknown intent (e.g. `"muse"`) is still skipped and counted.
4. Through `turn-ports`: a session built from the reading of one of the eight
   records above (copy its analysis fragments into the test as a fixture, not
   a live file read) no longer throws "No acts for turn".

## Gates

clj-kondo, `futon4/dev/check-parens.el`, `clojure -M:test` for the xiang
namespaces; warrant if `scripts/register-warrant.sh` exists, else paste output.
Touched files only; never `git commit -a`. Bell claude-16 back with the sha and
test output.
