# E-voxterm-multiplexed-conversation

**VERDICT (2026-10-09, provisional):** OPEN — Idea logged but explicitly not scheduled ('not suggesting that we should build this feature right now'), with open questions framed 'for when it is picked up'; fresh (2026-09-27). _(WM status classification by zai-5, high confidence; not yet confirmed by the author.)_

Logged 2026-09-27 by claude-17, from Joe's remark during M-象-2000. Not scheduled; Joe: "not suggesting that we should build this feature right away".

## Observation

In the M-象-2000 decision walk-through (13 decisions, spoken over voxterm), the `Gist:` lines were directly useful within the session for the first time. The thing that made them work was that voxterm was set to gist-only and bound to a single buffer (`*claude-repl:claude-17*`), so Joe heard one line per turn and nothing else.

## Idea

Give voxterm a queue across sessions, similar to each coding agent's queue. Gists from several agent sessions would be spoken in turn.

The reason is pace. When one session is working, its conversation has gaps. Several sessions sharing one spoken channel could fill each other's gaps, so the combined conversation might move at a steadier pace than any single session does.

## Open questions (for when it is picked up)

- **Which session is speaking?** Each gist needs a short spoken prefix for the agent, or a distinct voice.
- **Where does dictation go?** At present speak-only-buffer fixes one target. On 2026-09-27 a dictated reply went to claude-1 because of the frame-focus guess. With several sessions, a reply has to be addressed to the session whose gist it answers. That means either "reply to last-heard" or an explicit name.
- **Ordering and holding back.** A gist that asks Joe a question should probably not be followed immediately by another session's gist before he answers.
- **Joe's side.** He expects it may take some getting used to. Try it first with two sessions.

## Related

- README-voxterm.md: the gist-only and speak-only-buffer settings.
- E-repl-lost-dictated-turn.md: the lost dictated turn.
