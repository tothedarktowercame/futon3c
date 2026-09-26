# ARGUE-0 — codex-3's seven points, and why DERIVE missed each one

claude-14, 2026-09-26. Critique: codex-3, `*codex-repl:codex-3*` (Joe's request:
"critique the design in M-象-2000.md"). This is the "on paper" run of what
象-2000 is meant to do: for each miss, was the knowledge that would have caught
it present (and not applied), present but distorted, or absent?

Codex's two code claims were checked at HEAD: `stable-hyperedge-id`
(futon1b_server.clj:68) derives the id from type and endpoints; `note-completion!`
(parked_on.clj:305) releases a park when its dependencies terminate.

## Point by point

| # | Codex | What we already had | Where it was lost | Class |
|---|---|---|---|---|
| 1 | Reconstruction, counterfactual replay and compensation are three operations; P14's "P would have prevented it" needs a world model; withdrawing a rule undoes nothing already done | 三层规格: an accomplishment claim needs a stated world assumption (McCarthy l.300-305). And in MAP I said it myself: "a reload or a notice already sent … can only be followed by a correcting act, not deleted" | The caveat stayed in conversation; the mission recorded only "detect after, rewind cheaply". The build plan read the mission, not the conversation | present, not carried forward |
| 2 | A park waking is not a promise kept; release ≠ fulfilment ≠ breach | 两种规格 (delivered ≠ done) — in the cascade kimi-3 built, as a pattern P5 should have been checked against | **I made the conflation first**, in MAP Q4: "parked dependencies complete" counted as *kept*. P5 inherited it | present, violated by our own finding |
| 3 | An inferred speech-act label must not itself change what commitments exist; authority is scope, time and delegation, not "executor ≠ signatory" | 翻译契约 (a translation is refutable and binds to its source); McCarthy l.1784-1788 ("does only what it is authorised to do") | 名分有据 narrowed McCarthy to the signatory case; P4 copied the narrowing. P10 let an interpretation terminate validity | present, distorted in our translation |
| 4 | Valid time and system time on every join; stable act identity; commit ≠ activation | MAP Q2 ("valid time, read with current knowledge"); memory whitepaper §2.3 (system-as-of vs valid-as-of) | P6's test checks only insertion. The typed hole cited type+endpoints ids as a *strength* ("idempotent") | present, inverted |
| 5 | Making history authoritative needs a crash-consistency design | Library: `agency/state-atomicity`, `social/idempotent-handoff` | The cascade drew on the 象 family plus four patterns; nobody searched the library for the operational side | present in library, not retrieved |
| 6 | "Proof to clear" can make yellow tape permanent; temporal coincidence ≠ ownership of a measure | 制度有时 weighed the cost of *reporting*, not of *clearing* | Joe's sequencing was adopted without asking how it fails | absent |
| 7 | Matching needs HOWEVER/THEN/BECAUSE and exceptions; 象 holding warrant and attribution conflicts with "no party owns two adjacent boundaries" | I stated 象's self-citations are not independent; the memory plan's boundary rule | The 象-role note granted both boundaries in the next sentence; never reconciled | stated, not reconciled |
| + | The acceptance case (the Kimi/red-tape incident) no longer arrives early; the wiring checker validates tokens, not semantics | IDENTIFY's acceptance case | DERIVE optimised coverage of tokens; no packet re-runs the case | process |

Tally: 5 of 7 were present in our own patterns, findings or library; 1 was
our translation narrowing McCarthy; 1 was absent.

## Four mechanisms

**A. Qualifications don't survive the phase boundary.** Points 1, 4, 7. Each
was said in the MAP conversation and then summarised away; the next phase read
the artefact. The design's own remedy applies to us: a qualification is an act
(qualify / withdraw) on a design claim, and the claim's record must carry it.

**B. Patterns present, not in front of the author at the moment of writing.**
Points 2, 3, 5. Same shape as 09-24's red tape (`gate-fails-loudly` was in the
library, retrieved, and never shown). 两种规格 sat inside the cascade the
build plan was derived from, and P5 still violates it. This is the failure P7
(retrieval handed to the agent) is meant to address — the design predicts its
own miss.

**C. Code was cited, not read.** Points 2, 4, 5. Our pointers resolved (0
unresolved, 88 in the typed hole); none was read for behaviour. Codex read
`parked_on.clj:305` and `futon1b_server.clj:68` and found the semantics.

**D. Every gate checked form.** EDN reads, pointers resolve, wiring edges carry
tokens, packets are small, rungs have licences: all green. None asks whether the
acceptance case still runs. My review of step 3 used shape criteria I had set
myself — author ≠ reviewer held for the agents, not for the frame.

## What this gives ARGUE

The argument for the design can be tested on its own construction. The claim
"a system that records acts, qualifications and pattern availability would have
caught these" is checkable here: mechanisms A and B are exactly what the act
store (qualify/withdraw on claims) and P7 (patterns in front of the author) are
for; C and D are not addressed by any packet and are requirements the plan lacks.
