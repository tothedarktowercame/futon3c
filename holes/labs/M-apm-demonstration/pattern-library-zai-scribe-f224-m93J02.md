# Pattern library additions — zai-scribe, f224 scribe-reduce (m93J02)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization/compact-extremum-ordered-min-api

When a compactness argument needs a global extremum of a function on a
compact set, or a minimum over a finite family of radii whose index type is
a Set subtype with no LinearOrder, reach directly for the IsCompact
extremum-on lemmas (exists_isMaxOn / exists_isMinOn, fed a nonemptiness
witness and a continuousOn fact) and take the finite minimum over the image
Finset in the base ordered type rather than over the indices — and
materialise the resulting IsMaxOn/IsMinOn and absolute-value reorderings as
separately type-ascribed hypotheses before handing them to arithmetic
tactics, which cannot unfold set membership or relate f w to |f w| on
their own.

## math-informal/zero-seminorm-degeneracy-witness

To refute a uniform bound by local data, use a witness function whose local
seminorm is degenerate but whose global seminorm is positive — the distance
to a fixed point works for exponents strictly between 0 and 1: its local
seminorm vanishes by a per-epsilon bound through the triangle inequality
and power algebra, while the global quotient at two distinct points is
positive after rescaling.
