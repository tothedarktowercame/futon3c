#!/usr/bin/env bash
# E-exotype-ct-grounding Workstream 1 — ground the 64 iiching-CT generators in mathlib
# x math.CT literature, 4 packs of 16, one codex agent per pack. Each grounds its pack
# and posts a cross-referenced :answer into ArSE. See:
#   futon5a/holes/excursions/E-exotype-ct-grounding.md
#   bash scripts/arse-generator-grounding.sh           # dispatch packs to codex-1..4
#   bash scripts/arse-generator-grounding.sh --dry-run # print payloads, send nothing
set -euo pipefail
cd "$(dirname "$0")/.."
SEND="python3 scripts/agency_send.py"
FROM="claude-6"
DRY=""; [ "${1:-}" = "--dry-run" ] && DRY="--dry-run"

declare -A PACK
PACK[codex-1]="image, functor, relation, bicategory, grothendieck topos, cartesian closed category, reflective subcategory, topos, adjunction, enriched category, cat, 2-category, geometric morphism, monad, adjoint, adjoint functor"
PACK[codex-2]="pushout, coproduct, pullback, equivalence, subobject, tensor unit, base change, model category, initial object, forgetful functor, terminal object, natural transformation, colimit, tensor product, quillen equivalence, quillen adjunction"
PACK[codex-3]="subcategory, category, homotopy, groupoid, grothendieck topology, span, sheaf, kan complex, morphism, opposite category, small category, type, presheaf, monoidal category, descent, yoneda embedding"
PACK[codex-4]="kernel, action, localization, object, abelian category, braiding, weak equivalence, homotopy category, diagram, yoneda lemma, differential, composition, limit, representable functor, natural isomorphism, stable homotopy category"

mk_prompt() {  # $1 = agent id, $2 = pack concepts
cat <<EOF
You are grounding a PACK of 16 category-theory concepts (the futon iiching-CT generator
basis) in BOTH Lean mathlib AND the math.CT literature. This builds a cross-referenced
grounding ledger toward CT-native specifications. Your discipline matters more than speed.

YOUR PACK (16 concepts): $2

For EACH concept, produce a record with these four fields:
  1. MATHLIB ANCHOR: the precise Lean declaration in ~/code/mathlib4/Mathlib/CategoryTheory/
     (namespaced name + file path), e.g. functor -> CategoryTheory.Functor
     (Mathlib/CategoryTheory/Functor/Basic.lean). If NOT formalized there, write
     MISSING-IN-MATHLIB and one line on what an extension would declare.
  2. MATH.CT CROSS-REFERENCE (the key new part): >=1 arXiv id/scope or nLab page where the
     concept is defined/used in the literature -- QUOTE the phrase. Tie the Lean form to corpus use.
  3. FIDELITY NOTE: does the mathlib definition match the literature's? Divergences are findings.
  4. CONFIDENCE: high/med/low + one line why.

GROUNDEDNESS CONTRACT (same as the CT probe -- violating it fails the work):
  - Cite REAL artifacts (mathlib decl path, arXiv id, nLab page) and QUOTE phrases. Never invent a
    declaration or theorem. If you cannot find it, say so.
  - "MISSING-IN-MATHLIB" / "not found in corpus" are VALID, valuable answers -- the missing set is a
    first-class deliverable (mathlib extension targets), not a failure.
  - You are PROPOSING, not certifying. claude-6/Joe adjudicate.

RETRIEVAL:
  - ~/code/mathlib4/Mathlib/CategoryTheory/   (full Lean checkout -- grep for the declaration)
  - futon6/data/ct-term-prior.json , futon6/scripts/build-arxiv-ct-term-evidence.py
  - futon6/data/showcases/ct-anatomy/ , nLab pages / canon_store

HOW TO ANSWER INTO ArSE: your surface header shows your ArSE thread id (ask-...). Post your
16-record answer with -- IMPORTANT: use --from $1 (YOUR OWN id, NOT claude-6) so the answer is
attributed to you:
  python3 /home/joe/code/futon3c/scripts/agency_send.py \\
    --to claude-6 --from $1 --kind bell --type answer --ref <ask-id> <<'ANS'
  <your 16 records>
  ANS
EOF
}

echo "=== Workstream 1: grounding 64 generators in 4 packs of 16 ${DRY:+(dry-run)} ==="
for agent in codex-1 codex-2 codex-3 codex-4; do
  echo "--- $agent (16 concepts) ---"
  mk_prompt "$agent" "${PACK[$agent]}" | $SEND --to "$agent" --from "$FROM" --kind bell --type query $DRY
  echo
done
echo "=== dispatched. Watch: curl -s localhost:7070/api/alpha/arse/unanswered | jq .count ==="
