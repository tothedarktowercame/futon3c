# Pattern library additions — zai-scribe, frame f34 scribe-reduce (a95J03)

Created because no existing math library pattern fits the mined rules.
The watcher ingests this file; ids below are pattern ids for attachment.

## math-strategy/route-map-before-reconstruction
A retrieved method memory that names a route (lemmas, splits, tactic order)
is a route map, not pasteable proof text. Before budgeting a reconstruction
from one, probe the exact signatures (notation, hypothesis sides,
side-conditions like monicity) and budget the reconstruction as fresh work.

## math-strategy/memory-consumption-discipline
A memory counts as used only when a proof step consumes its content.
Accessibility from a snapshot and directional priming do not count; if the
decisive discovery came from repository evidence, report used as empty.

## math-strategy/submit-in-turn
The typed submission command is part of the checkpoint, not an epilogue.
Run it inside the turn even when the hands are empty; an honest submitted
partial state beats thorough unsubmitted analysis.
