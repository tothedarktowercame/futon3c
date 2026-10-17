# PACKET: the SessionStart brief records what it injected (E-prove-memory-useful, G4 for Claude seats, writer half)

From claude-3. Read `holes/excursions/E-prove-memory-useful.md`, the
sections "What 'useful' has to mean here" (1. Delivery) and "G3's premise,
and G5 first", before starting.

## Why

`minus40/scripts/brief.py` (SessionStart hook, enabled for "all") already
puts memory facts into a Claude seat's context before round 1, without the
seat asking. That is delivery by the harness, which is the first of the
excursion's three checks, but nothing on record says which memories went in,
so the stepper cannot show it. Its only trace is a file under
`~/code/data/minus40/briefs/`.

## Goal (one behaviour)

After `brief.py` has built and capped the brief and before it prints it,
POST one coordination evidence row to Agency, the same route, shape and
never-fail rules as `minus40/scripts/call_hook.py` (read it; reuse its
`post`/origin conventions by import if that is clean, else mirror them):

    evidence/type      coordination
    evidence/tags      ["memory-injected" "minus40"]
    evidence/author    FUTON_AGENT_ID
    evidence/session-id   event session_id
    body {event "memory-injected", hook "SessionStart",
          source <event source: startup|resume|clear|compact>,
          memories [<memory id, e.g. "cc:-home-joe/park-needs-session-id">
                    for every memory with at least one line that survived
                    cap() into the printed brief, in brief order, no dups],
          asks <number of open-ask lines that survived cap()>,
          brief-text <the brief exactly as printed>,
          at <ISO time>}

`recent_facts` iterates memory ids but returns bare lines; it must now
carry the memory id with each line so the surviving set can be computed
after `cap()`. Lines from `contradictions()` count too if they name a
memory; say in your bell what you did with them.

Budget: the POST must fit inside the brief's existing 3 s budget (timeout
the remaining budget, ≤ 1 s); a failed POST logs and the brief is still
printed. The printed brief must be byte-identical to before this change
(test it).

## Not in this packet

Reading the row into stepper frames (`turn_frames.py`): claude-3 does that
after review.

## Files

- in: `minus40/scripts/brief.py`, `minus40/scripts/test_brief.py`,
  `minus40/scripts/call_hook.py`.
- out: `minus40/scripts/brief.py`, `minus40/scripts/test_brief.py`.

## Acceptance

1. Tests: the printed brief is unchanged by the POST; `memories` lists only
   memories whose lines survived `cap()` (build a case where `cap` drops a
   section and check the dropped memory is absent); Agency down → brief
   still printed, exit 0, one log line; a disabled agent posts nothing.
2. Live: run brief.py with a real-shaped SessionStart payload (session id
   `brief-injected-test-<date>`, `FUTON_AGENT_ID=claude-3`, cwd `/home/joe`)
   against :7070 and paste the read-back row (`brief-text` shortened) in
   your bell.
3. Commit on minus40 master.

Bell claude-3 back with a summary + commit shas.
