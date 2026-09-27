# E-classical-wastage-scanner: finding red tape in agent chat logs without an LLM

Opened 2026-09-27 by claude-17 at Joe's direction, as an excursion related to
M-象-2000. Joe: "build a purely classical (no LLM, no network) scanner that can
read people's .jsonl agent chats and develop an analysis of some of their red
tape or other wastage incidents." M-象-2000's IDENTIFY named the same aim:
"a robust record, perhaps eventually a fully classical (non-LLM) mining
picture".

## What it is

A local program. It takes a directory of agent-chat transcripts (Claude Code
`~/.claude/projects/*/*.jsonl`, Codex `~/.codex/sessions/**`, and our own
evidence exports) and reports wastage incidents: each one with where it starts
and ends, what it cost (turns, tokens, wall-clock), the rows of evidence it
rests on, and the pattern whose violation it matches. There are no model calls
and no network access, and the same input and version always give the same
output. It only reads the transcripts.

## Why classical

- It can run on other people's logs without sending them anywhere: nothing
  leaves the machine.
- Each finding points at transcript lines and a rule, so a reader can refute it
  (象/翻译契约), not only trust it.
- It is cheap enough to run after every session, which fits "detect after,
  rewind cheaply" (M-象-2000 MAP).

## Detector sources: violation signatures already in the library

71 library patterns carry a `@violation-signature`. Those signatures are the
detector specs, and the scanner's work is to make them checkable over
transcripts. The first detectors are taken from incidents we have already
labelled:

| wastage | signature (source) | classical signal | labelled instance |
|---|---|---|---|
| red tape | "a consumer that repeats steady red until it is ignored" (inbox-zero/gate-fails-loudly) | near-identical user-role messages repeating with no human reply between them; the same template with different ids | 42 Kimi requisition notices, 09-24 19:04 → 09-25 19:59 |
| turns in the operator's name the operator did not write | 象/名分有据 | user-role turns with harness-shaped headers or templates (`harvest_turn_origins.py` already recovers Origin/Caller) | MAP filter over window-0922: 436 park/wake, 27 inbox-zero, 42 Kimi |
| delivered, not done | 象/两种规格 | a job reported done with no tool events, or a reply that claims work with no tool calls in the turn | 09-23 brief-mode delegation (`:executed false`, state done) |
| promises that lapse | 象/诺必践 | "I'll bell you / get back to you" with no later message from that agent to the recipient; DEADLINE EXPIRED wakes | 129 lapsed of ~3,540 park wakes |
| repeated sends | social/idempotent-handoff | the same payload sent to the same recipient N times inside a window | 57 bells to one unregistered id, 2026-08-24 (failure-census.py) |
| rerunning to feel sure | CLAUDE.md I-6 | the same long test command repeated without an edit between runs | |
| a wall that repeats | memory: stop-when-the-wall-repeats | the same error text returned to the same agent over K attempts | |

## Parts that already exist (wiring first, I-4)

- `scripts/harvest_turn_origins.py`: who actually wrote each turn filed under "joe".
- `scripts/operator_turn_lexical.py`: n-gram cues, including sentence-initial
  ones; the templates of repeated notices show up here.
- `scripts/operator_turn_cluster.py`: tf-idf → SVD → k-means → Ward; clusters of
  near-duplicates.
- `scripts/failure-census.py`: typed job failures from the invoke ledger.
- `scripts/session-cost.py`: token cost per turn, which prices each incident.
- `scripts/xlate.py find`: classical (tf-idf) retrieval from a text to library
  patterns, used to name the pattern an incident violates.

## Acceptance

1. Run on the 09-22 → 09-26 corpus without being told about the incident, it
   finds the 09-24 red tape: the 42 notices as one incident with its start and
   end, priced, attributed to the harness rather than Joe, and matched to
   inbox-zero/gate-fails-loudly. M-象-2000's P0 table is the answer key.
2. It reproduces the MAP filter's labels (436 / 27 / 42) as a detector, not as
   a hand list, and reports precision and recall against them.
3. Run on a transcript with no incident planted, it reports none; with one
   planted (a repeated template inserted), it finds exactly that one.
4. Two runs on the same input give byte-identical output. It makes no network
   calls (checked, for example, by running it in a network namespace with no
   interfaces).

## Relation to M-象-2000

The scanner is the classical miner that M-象-2000 keeps data for. Its
incidents are candidates for yellow tape (reporting stays cheap), and its
pattern matches are graded evidence (象/引非读), not verdicts. What it finds
becomes an interpretation with no effect until confirmed (象/释义非授). It
also feeds the prospective model over tags in the MAP section: incidents are
the labels that model needs.

## Other people's logs

Before running on anyone else's transcripts: their consent; the output stays
with them by default; the report quotes the minimum text needed to locate each
incident. Classical and offline is what makes this possible, and it does not
replace asking.

## Not decided

Scope of the first pass (Claude Code format only, or Codex too); where it runs
in sequence with M-象-2000 (after INSTANTIATE, like E-象-2000-wm-seam, or in
parallel on a separate seat).
