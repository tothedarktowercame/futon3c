# within_rules: classical rules vs the labelled scope links

sidecars: 1104; gold within links (act ids): 1786; within-as-name (not scored): 310

## Gold target breakdown
- a: same record: 1711 (95.8%)
- b: in_force id: 0 (0.0%)
- c: open_ports id: 75 (4.2%)
- d: previous_reply_acts id: 0 (0.0%)
- e: none of these: 0 (0.0%)

### R0 — none (baseline)
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 0 | 0 | - | 0.000 |
| moves-scale | 351 | 0 | 0 | - | 0.000 |
| moves-scale2 | 202 | 0 | 0 | - | 0.000 |
| moves-scale3 | 1233 | 0 | 0 | - | 0.000 |
| fragment | 1138 | 0 | 0 | - | 0.000 |
| reply | 648 | 0 | 0 | - | 0.000 |
| moves-scale/fragment | 160 | 0 | 0 | - | 0.000 |
| moves-scale/reply | 191 | 0 | 0 | - | 0.000 |
| moves-scale2/fragment | 44 | 0 | 0 | - | 0.000 |
| moves-scale2/reply | 158 | 0 | 0 | - | 0.000 |
| moves-scale3/fragment | 934 | 0 | 0 | - | 0.000 |
| moves-scale3/reply | 299 | 0 | 0 | - | 0.000 |

top confusions (predicted kind, gold kind):

### R1 — first-fragment
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 1724 | 897 | 0.520 | 0.502 |
| moves-scale | 351 | 350 | 101 | 0.289 | 0.288 |
| moves-scale2 | 202 | 202 | 89 | 0.441 | 0.441 |
| moves-scale3 | 1233 | 1172 | 707 | 0.603 | 0.573 |
| fragment | 1138 | 1076 | 660 | 0.613 | 0.580 |
| reply | 648 | 648 | 237 | 0.366 | 0.366 |
| moves-scale/fragment | 160 | 159 | 58 | 0.365 | 0.362 |
| moves-scale/reply | 191 | 191 | 43 | 0.225 | 0.225 |
| moves-scale2/fragment | 44 | 44 | 34 | 0.773 | 0.773 |
| moves-scale2/reply | 158 | 158 | 55 | 0.348 | 0.348 |
| moves-scale3/fragment | 934 | 873 | 568 | 0.651 | 0.608 |
| moves-scale3/reply | 299 | 299 | 139 | 0.465 | 0.465 |

top confusions (predicted kind, gold kind):
- predicted `approve` but gold `propose`: 36
- predicted `ask-action` but gold `reply-mark`: 27
- predicted `report` but gold `propose`: 26

### R2 — first-framing
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 950 | 452 | 0.476 | 0.253 |
| moves-scale | 351 | 295 | 80 | 0.271 | 0.228 |
| moves-scale2 | 202 | 144 | 81 | 0.562 | 0.401 |
| moves-scale3 | 1233 | 511 | 291 | 0.569 | 0.236 |
| fragment | 1138 | 518 | 261 | 0.504 | 0.229 |
| reply | 648 | 432 | 191 | 0.442 | 0.295 |
| moves-scale/fragment | 160 | 127 | 47 | 0.370 | 0.294 |
| moves-scale/reply | 191 | 168 | 33 | 0.196 | 0.173 |
| moves-scale2/fragment | 44 | 27 | 24 | 0.889 | 0.545 |
| moves-scale2/reply | 158 | 117 | 57 | 0.487 | 0.361 |
| moves-scale3/fragment | 934 | 364 | 190 | 0.522 | 0.203 |
| moves-scale3/reply | 299 | 147 | 101 | 0.687 | 0.338 |

top confusions (predicted kind, gold kind):
- predicted `propose` but gold `reply-mark`: 57
- predicted `propose` but gold `propose`: 36
- predicted `ask-action` but gold `reply-mark`: 27

### R3 — nearest-opener
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 1260 | 1007 | 0.799 | 0.564 |
| moves-scale | 351 | 346 | 239 | 0.691 | 0.681 |
| moves-scale2 | 202 | 193 | 174 | 0.902 | 0.861 |
| moves-scale3 | 1233 | 721 | 594 | 0.824 | 0.482 |
| fragment | 1138 | 694 | 544 | 0.784 | 0.478 |
| reply | 648 | 566 | 463 | 0.818 | 0.715 |
| moves-scale/fragment | 160 | 155 | 100 | 0.645 | 0.625 |
| moves-scale/reply | 191 | 191 | 139 | 0.728 | 0.728 |
| moves-scale2/fragment | 44 | 40 | 34 | 0.850 | 0.773 |
| moves-scale2/reply | 158 | 153 | 140 | 0.915 | 0.886 |
| moves-scale3/fragment | 934 | 499 | 410 | 0.822 | 0.439 |
| moves-scale3/reply | 299 | 222 | 184 | 0.829 | 0.615 |

top confusions (predicted kind, gold kind):
- predicted `propose` but gold `propose`: 24
- predicted `propose` but gold `explain`: 13
- predicted `extend` but gold `explain`: 10

### R4 — nearest-framing
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 950 | 487 | 0.513 | 0.273 |
| moves-scale | 351 | 295 | 99 | 0.336 | 0.282 |
| moves-scale2 | 202 | 144 | 75 | 0.521 | 0.371 |
| moves-scale3 | 1233 | 511 | 313 | 0.613 | 0.254 |
| fragment | 1138 | 518 | 303 | 0.585 | 0.266 |
| reply | 648 | 432 | 184 | 0.426 | 0.284 |
| moves-scale/fragment | 160 | 127 | 63 | 0.496 | 0.394 |
| moves-scale/reply | 191 | 168 | 36 | 0.214 | 0.188 |
| moves-scale2/fragment | 44 | 27 | 24 | 0.889 | 0.545 |
| moves-scale2/reply | 158 | 117 | 51 | 0.436 | 0.323 |
| moves-scale3/fragment | 934 | 364 | 216 | 0.593 | 0.231 |
| moves-scale3/reply | 299 | 147 | 97 | 0.660 | 0.324 |

top confusions (predicted kind, gold kind):
- predicted `propose` but gold `reply-mark`: 59
- predicted `propose` but gold `explain`: 30
- predicted `propose` but gold `report`: 28

### R5 — reply-to-gist (+R4 for fragments)
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 1122 | 543 | 0.484 | 0.304 |
| moves-scale | 351 | 314 | 142 | 0.452 | 0.405 |
| moves-scale2 | 202 | 178 | 106 | 0.596 | 0.525 |
| moves-scale3 | 1233 | 630 | 295 | 0.468 | 0.239 |
| fragment | 1138 | 518 | 303 | 0.585 | 0.266 |
| reply | 648 | 604 | 240 | 0.397 | 0.370 |
| moves-scale/fragment | 160 | 127 | 63 | 0.496 | 0.394 |
| moves-scale/reply | 191 | 187 | 79 | 0.422 | 0.414 |
| moves-scale2/fragment | 44 | 27 | 24 | 0.889 | 0.545 |
| moves-scale2/reply | 158 | 151 | 82 | 0.543 | 0.519 |
| moves-scale3/fragment | 934 | 364 | 216 | 0.593 | 0.231 |
| moves-scale3/reply | 299 | 266 | 79 | 0.297 | 0.264 |

top confusions (predicted kind, gold kind):
- predicted `reply-mark` but gold `propose`: 44
- predicted `reply-mark` but gold `ask-action`: 35
- predicted `propose` but gold `explain`: 27

### R6 — kernel in_force (present only in some batch-3 inputs)
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 575 | 7 | 0.012 | 0.004 |
| moves-scale | 351 | 0 | 0 | - | 0.000 |
| moves-scale2 | 202 | 0 | 0 | - | 0.000 |
| moves-scale3 | 1233 | 575 | 7 | 0.012 | 0.006 |
| fragment | 1138 | 407 | 0 | 0.000 | 0.000 |
| reply | 648 | 168 | 7 | 0.042 | 0.011 |
| moves-scale/fragment | 160 | 0 | 0 | - | 0.000 |
| moves-scale/reply | 191 | 0 | 0 | - | 0.000 |
| moves-scale2/fragment | 44 | 0 | 0 | - | 0.000 |
| moves-scale2/reply | 158 | 0 | 0 | - | 0.000 |
| moves-scale3/fragment | 934 | 407 | 0 | 0.000 | 0.000 |
| moves-scale3/reply | 299 | 168 | 7 | 0.042 | 0.023 |

top confusions (predicted kind, gold kind):
- predicted `delegate` but gold `explain`: 203
- predicted `delegate` but gold `propose`: 53
- predicted `delegate` but gold `constrain`: 34

### Combined — the nearest earlier act with a labelled scope.opens (R3); if none, a reply act beyond r-0 takes its record's r-0 gist (R5); else the nearest earlier framing fragment (R4); else the first fragment (R1)
| slice | gold | predicted | correct | precision | recall |
|---|---|---|---|---|---|
| all | 1786 | 1724 | 1319 | 0.765 | 0.739 |
| moves-scale | 351 | 350 | 241 | 0.689 | 0.687 |
| moves-scale2 | 202 | 202 | 179 | 0.886 | 0.886 |
| moves-scale3 | 1233 | 1172 | 899 | 0.767 | 0.729 |
| fragment | 1138 | 1076 | 811 | 0.754 | 0.713 |
| reply | 648 | 648 | 508 | 0.784 | 0.784 |
| moves-scale/fragment | 160 | 159 | 102 | 0.642 | 0.637 |
| moves-scale/reply | 191 | 191 | 139 | 0.728 | 0.728 |
| moves-scale2/fragment | 44 | 44 | 38 | 0.864 | 0.864 |
| moves-scale2/reply | 158 | 158 | 141 | 0.892 | 0.892 |
| moves-scale3/fragment | 934 | 873 | 671 | 0.769 | 0.718 |
| moves-scale3/reply | 299 | 299 | 228 | 0.763 | 0.763 |

top confusions (predicted kind, gold kind):
- predicted `propose` but gold `propose`: 25
- predicted `propose` but gold `explain`: 14
- predicted `propose` but gold `report-problem`: 13

## Ten instructive combined-rule misses
(one per distinct predicted-kind/gold-kind confusion first, then fill)
- claude-4-turn-130-f-s1-0 [moves-scale3] predicted None (?), gold claude-4-turn-3-f-s1-0 (propose) — quote: '2+ hours under bg.py to get a full trace would be fine with me'
- claude-13-turn-10-r-0 [moves-scale] predicted claude-13-turn-10-f-s5-0 (propose), gold claude-13-turn-10-f-s1-0 (propose) — quote: 'Five groups:'
- claude-2-turn-103-f-s6-0 [moves-scale] predicted claude-2-turn-103-f-s4-0 (propose), gold claude-2-turn-103-f-s5-0 (explain) — quote: 'a pushout.'
- claude-13-turn-27-r-1 [moves-scale] predicted claude-13-turn-27-f-s2-0 (propose), gold claude-13-turn-27-f-s5-0 (report-problem) — quote: 'There is, and for this case the strongest of them is a **hook**'
- claude-2-turn-103-r-5 [moves-scale] predicted claude-2-turn-103-f-s4-0 (propose), gold claude-2-turn-103-f-s1-0 (qualify) — quote: 'claude-17, request for comment only, no code'
- claude-2-turn-109-f-s13-0 [moves-scale] predicted claude-2-turn-109-f-s3-0 (extend), gold claude-2-turn-109-f-s12-0 (explain) — quote: "I don't remember exactly how many iterations there were"
- codex-18-turn-36-r-0 [moves-scale] predicted codex-18-turn-36-f-s4-0 (propose), gold codex-18-turn-36-f-s1-0 (report) — quote: 'Paused feature expansion and committed `3e026ef`'
- claude-18-turn-80-f-s4-0 [moves-scale3] predicted claude-18-turn-80-f-s1-0 (approve), gold claude-18-turn-80-f-s3-0 (report) — quote: "I figured I couldn't solve the problems if I didn't know the terminology."
- claude-4-turn-130-r-0 [moves-scale3] predicted claude-4-turn-130-f-s1-0 (approve), gold claude-4-turn-3-f-s1-0 (propose) — quote: "I've queued a follow-up job that starts as soon as the trace finishes"
- claude-4-turn-236-r-1 [moves-scale3] predicted claude-4-turn-236-r-0 (report), gold claude-4-turn-236-f-s1-0 (redirect) — quote: '(your emulate rule) The brief repeats your rule.'

---

## Reading (zai-4, 2026-10-14)

Gold is 1,786 act-id `within` links (310 sidecars use `within` as a scope
NAME — batch 1/2 style — counted separately, not scored). 95.8% of gold
targets are in the same record; 4.2% are open_ports ids; in_force and
previous_reply_acts ids are never cited (0 each).

Classical structure carries most of it: the combined rule — nearest earlier
act with a labelled `opens` (R3), else a reply's r-0 gist (R5), else nearest
framing fragment (R4), else first fragment (R1) — reaches precision 0.765 /
recall 0.739. R3 alone is the strongest single rule (0.799/0.564): the
labellers' `opens` labels are the load-bearing signal, and "nearest earlier
opener" is almost always right when it fires. R1 (first-fragment) fires on
nearly everything but is right only half the time. R2/R4 (framing intents)
are weak: intent kinds alone don't identify the governing act. R5 alone is
weak because most gold reply links point past the gist to the operator's
opener. R6 (kernel in_force) is near-zero (0.012 precision): standings the
kernel tracks are not what labellers cite — the same-record focus says scope
is read locally, at turn grain.

The instructive misses are one shape: gold points at the *semantically*
governing act (a standing rule, a research-question paragraph, the mission
opener from an earlier turn) while every classical rule can only see
position. The batch-3 cross-turn cases (gold = claude-4-turn-3-f-s1-0, the
mission-defining opener) are exactly the 4.2% open_ports gold; no
within-record rule can get those, which caps recall near 0.96 for anything
built from R1-R5. A cheap linker that treats open_ports as candidate openers
is the obvious next probe, not proposed here.
