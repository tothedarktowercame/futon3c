# Matrix bridge: offline slice 1

`MatrixBot` subclasses `IRCBot`. Mention parsing, gating, commands, Agency
announce/invoke, pending-job handling and the invoke queue remain inherited.
No homeserver, account, service or deployment was used for qualification.

Configuration:

- `MATRIX_HOMESERVER_URL`: homeserver base URL (production should use HTTPS).
- `MATRIX_ROOMS`: comma-separated opaque room IDs, never aliases: `!room:server`,
  or `!room` for room version 12, whose IDs carry no server part.
- `MATRIX_TOKEN_DIR`: one `<nick>.token` file per bot. Tokens are read from files
  and sent only in Authorization headers; HTTP redirects are refused.
- `MATRIX_STATE_DIR`: defaults to `~/.local/state/futon-matrix`. One process owns
  a bot's state directory. Keep it with the same account/homeserver/room setup;
  it is not portable between configurations.
- `BRIDGE_BOTS`, `NICK_AGENT_MAP`, `INVOKE_BASE`, `EVIDENCE_BASE`: inherited IRC
  configuration conventions. First configured bot handles bare commands.

Whoami verifies that the configured nick is the token account's localpart.
Configured-room members from any server can invoke; there is no sender allowlist.
Only listed invitations are accepted. Encryption is unsupported.

Before the inherited IRC mention rules run, `_routable_text` adjusts the body
(the transcript evidence keeps the original):
- It drops a reply's quoted fallback (`> <@rob:…> …`), which ement sends.
  Otherwise, replying to a message that mentioned `@codex` invokes codex again.
- It shortens this bot's own MXID to `@nick`. Otherwise,
  `@codex:matrix.paragogy.net: please …` reaches the agent as
  `matrix.paragogy.net: please …`.
- It removes the `@` from a same-named user on another server
  (`@codex:elsewhere`). The inherited rule ends a name at `:`, so it would
  otherwise invoke this bot.
Mentions that exist only in `m.mentions` (an Element autocomplete pill in
mid-sentence) do not trigger, just as bare `codex` in mid-sentence does not
trigger on IRC.

The initial sync seeds the cursor and seen IDs without invoking history. Later
batches reserve event IDs in an atomically replaced/fsynced state file before
calling shared routing. The cursor advances after processing the batch.
The last 4096 IDs are retained, so deduplication is bounded. Reservation before
Agency acceptance prevents duplicate admission on ordinary replay, but a crash
in between can lose work: the inherited queue is not a durable outbox. This is
not an exactly-once completion claim. A failed state write prevents dispatch.
A send retries once with the same UUID transaction ID; each new send gets a new
ID. Text stays in one Matrix message up to 16000 characters, then gets an
explicit truncation marker.

## IRC hooks

- `transport_name` supplies Agency caller/surface and delivery-receipt labels.
- `_dispatch_message` extracts the existing routing branches from `run`;
  `_start_message_handler` retains IRC's threads. Matrix admits in sync order.
- `_transport_context` / `_set_transport_context` carry the incoming room/event
  through the existing worker queue, so asynchronous replies reference the
  correct event rather than the last received event.
- Matrix overrides `_surface_context` and `_emit_success_reply` as transport
  renderers: IRC's latter method summarizes/truncates before `_say` is reached.

Other IRC-specific details remain explicit: inherited command help and some
receipt notes still use the bridge's original name. Transcript recording was
hardwired to IRC in the socket reader/sender. Its payload/thread constructor is
now `post_transport_evidence`, with the original `post_irc_evidence` retained as
a compatibility wrapper. Matrix uses the same single-writer ownership rule and
`EVIDENCE_BASE`, with full MXID authors and Matrix thread IDs/tags. Agency trace
delivery receipts use the Matrix surface and `INVOKE_BASE`.

## Executed qualification (2026-09-14)

- `python3 -m py_compile scripts/matrix_bridge.py scripts/ngircd_bridge.py`: pass.
- `python3 test/matrix_bridge_test.py`: **14/14 pass**, stubbed HTTP and Agency.
  Review (claude-17) added two tests for `_routable_text`: **16/16 pass**.
- `python3 test/ngircd_bridge_windows_test.py`: **1/1 pass**.
- `scripts/test_ngircd_bridge_roster.py`: **6/6 pass**, invoked using the stdlib
  runner below (running this file directly does not discover its functions).
- `python3 test/ngircd_bridge_test.py`: **29/29 pass**; **20/20 consecutive
  runs passed** (580 test executions).
- `git diff --check`: pass.

The IRC tests now set the outbound-evidence fixture as channel owner and verify
that a non-owner sends without recording evidence. The mention-formatting test
stubs invoke preparation instead of querying live Agency state. No production
bridge behavior changed.

Module-wide guards reject `urlopen`, `socket.create_connection`, and raw socket
construction. Cleanup also fails on attempted access swallowed by a bridge
helper. Before adding the missing stub, the guard demonstration failed with:
`Real network forbidden in test_codex_mention_does_not_emit_accepted_ack; stub the dependency`.
That was the only test exposed by the guard. The demonstration's unstubbed call
has been replaced by the explicit preparation stub; the guards remain enabled.
All requested offline test gates now pass; this is not deployment qualification.

The 14 Matrix tests cover all seven requested cases plus inherited-method
identity, Matrix transcript recording, both Agency surface paths, independent queued reply associations,
transaction reuse after ambiguous timeout, long output/truncation, malformed
configuration/state, bounded ID retention, state-write failure, identity
mismatch, and missing cursor refusal. Actual HTTP sockets are stubbed; no
real-server interoperability, crash recovery of queued work, or deployment
claim follows.

Reproducible roster runner (pytest is not installed in this environment):

```sh
python3 - <<'PY'
import importlib.util, sys
sys.path.insert(0, 'scripts')
spec = importlib.util.spec_from_file_location('roster_tests', 'scripts/test_ngircd_bridge_roster.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
tests = [(n, f) for n, f in vars(m).items() if n.startswith('test_') and callable(f)]
for n, f in tests:
    f()
    print('PASS', n)
print(len(tests), 'roster tests passed')
PY
```
