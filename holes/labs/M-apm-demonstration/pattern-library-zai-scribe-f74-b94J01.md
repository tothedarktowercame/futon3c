# Pattern library additions — zai-scribe, frame f74 scribe-reduce (b94J01)

Created because no existing math library pattern fits the mined rules below.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment. Rules that fit existing library
patterns (math-formalization/tactic-algebra-interference,
math-formalization/defeq-endpoints-no-rewrite,
math-formalization/query-memory-with-api-names-not-obstacle-prose) are
attached there and not re-coined.

## math-formalization/morphism-map-lemmas-over-simp-unfold

Simp across a bundled morphism's coe rarely unfolds the morphism-law lemmas:
a goal like `e x ^ k = 1` for `e : M ≃* N` sits unchanged under `simp`
("made no progress") because simp does not see the coe as an instance of
`map_pow`. When a goal mixes a bundled equivalence/homomorphism coe with an
algebra operation the morphism law governs, rewrite explicitly with the map
lemma in the direction that consumes the hypothesis (`rw [← map_pow e x k, hx,
map_one]` shape) rather than asking simp to unfold through the coe. Same
signature: `simpa` on a subgroup-kernel membership that simp reduces to `True`
means the goal is already definitional — see the defeq pattern — not that the
hypothesis was lost.

## math-formalization/list-product-api-shapes-fin-vs-list

Product-over-a-list goals have two API families and the lemma names do not
transfer: the `Finset.univ`-indexed product lemmas one remembers (e.g. a
`prod_univ_succ` shape) may simply not exist in the snapshot at hand
("unknown identifier"), while the same mathematics goes through one level
down as `List.map` followed by `List.prod`, whose `cons`/append lemmas are
always present. When a product-over-index proof stalls on a missing univ
lemma, restate the product as a list map/prod over the data you already hold
as a list, instead of converting the list into a `Fin d.length` index and
fighting the univ API.
