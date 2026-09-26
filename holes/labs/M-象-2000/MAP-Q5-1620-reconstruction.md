# MAP Q5 — the 16:20 sequence, reconstructed from the evidence store

claude-14, 2026-09-26. Source: futon1b `GET :7073/api/alpha/evidence`
(`Accept: application/json`, `author=claude-11|joe`, `since=2026-09-24T16:00Z`,
`limit=1000`), window 16:00–16:50 UTC on 2026-09-24: 95 records. Library as of
16:20 = futon3 `9c248d52` (last commit before 2026-09-24T16:20Z).

## Timeline (records in the store)

| at (09-24) | record | content |
|---|---|---|
| 16:20:00–01 | claude-11 `clock-decision` | turn clocked to M-autoclock-in |
| 16:20:01 | joe `chat-turn` claude-11-turn-3 | "create an enforcement rule similar to the inbox-zero followup: message that says 'You can't use Kimi without sending a work target'" |
| 16:22:42 | claude-11 `invoke` done | rule built: refuse + queue a follow-up via the inbox-zero queue (`:kimi-work-target` added to `followup_queue.clj`) |
| 16:22:46 | claude-11 `turn-commits` | `80428193` (the rule) — and futon2 `4c502847`, a PROOF-2a commit from claude-10's work |
| **16:22:48** | claude-11 `context-retrieval` (futon3a embeddings) on the 16:20 text | 1 `translation/route-the-untranslatable` 0.397 · 2 `aif/scheduled-observer-entrypoint` 0.374 · **3 `inbox-zero/gate-fails-loudly` 0.370** |
| 16:28:10 | joe | "a one-line requisition request that includes the mission name" |
| 16:34:20 | claude-11 `turn-commits` | `5146606d` requisition gate |

## Findings

1. **The warning existed, was retrieved, and arrived after the act.** At 16:20
   the library held `inbox-zero/gate-fails-loudly` (added 2026-09-16). Its
   `@violation-signature` reads "a consumer that repeats steady red until it is
   ignored", and its evidence cites a self-triggering gate that produced "248
   consecutive" repeats. The 42 requisition notices of 09-24 19:04 → 09-25 19:59
   are that signature. The live retrieval ranked it 3rd for Joe's 16:20 turn —
   at 16:22:48, six seconds after claude-11 had committed the rule.
2. **Retrieval is never shown to the agent.** `context-retrieval` records are
   read only by `emacs/session-mode.el` (l.115–195), which draws the top
   pattern's sigil after the turn. So the stack already does per-turn pattern
   inference; it runs post hoc and its output goes to decoration.
3. **Prevention, concretely:** run the same retrieval before the agent acts
   and put the top hits' `@violation-signature` lines in front of it. On this
   case that one line would have asked: who consumes these notices, and what
   stops them repeating? (The notices also went out under Joe's name — a
   separate defect, see IDENTIFY.)
4. **`turn-commits` over-attributes.** It lists every commit landing during the
   turn, whoever made it: claude-11-turn-4 carries claude-10's futon2
   `4c502847`. The derivation query (Q2 of the acceptance case) must filter by
   author seat or it will join unrelated work.
5. **Turn-id offset.** Joe's `claude-11-turn-3` is answered by the assistant
   record `claude-11-turn-4`; joins must pair by order, not by equal id.
