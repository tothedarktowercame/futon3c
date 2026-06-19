#!/usr/bin/env bash
# E-arse-ct-probe dispatch — send ONE dark-tower conjecture to a codex swarm as
# typed :query bells; each codex agent grounds it in the math.CT scan and posts a
# grounded :answer back into ArSE. See holes/excursions/E-arse-ct-probe.md.
#
#   bash scripts/arse-ct-probe.sh           # dispatch to codex-1 and codex-2
#   bash scripts/arse-ct-probe.sh --dry-run # print prompt, send nothing
set -euo pipefail
cd "$(dirname "$0")/.."
SEND="python3 scripts/agency_send.py"
FROM="claude-6"
WORKERS=("codex-1" "codex-2" "codex-3")
DRY=""; [ "${1:-}" = "--dry-run" ] && DRY="--dry-run"

read -r -d '' PROMPT <<'EOF' || true
You are grounding ONE category-theory conjecture in the local math.CT corpus (the
superpod scan of 9,798 arXiv math.CT papers). This is a test of whether ArSE is
useful — so your discipline matters more than your conclusion.

CONJECTURE (from E-the-dark-tower-2 §4):
"The Caus[−] / higher-order-causal primitive condition — discarding the output =
discarding the input (states normalised) — is the SAME constraint as the War
Machine's free-energy (VFE) normalisation (G-total / per-channel). I.e. the VFE
bookkeeping and the Caus[−] causality axiom are one constraint seen from two
sides, making the stack an (additive) precausal category."

ANSWER THESE, IN ORDER:
  SQ1 (definitional): What is the PRECISE causality/discard axiom in the
      Caus[−] / higher-order-causal literature? Find the real definition.
  SQ2 (prior art): Is "precausal category" a DEFINED notion in the corpus, and is
      the discard/causality axiom what defines it? Find the definition + origin.
  SQ3 (bridge, conjectural): Does a free-energy/VFE normalisation have the SAME
      algebraic shape as the discard axiom (both a conservation/normalisation law)?
  SQ4 (proposal, NOT a verdict): supported / refuted / underdetermined — and why.

GROUNDEDNESS CONTRACT (this is the point — violating it fails the probe):
  - Every claim CITES a corpus artifact: an arXiv id, a showcase scope id, an nLab
    page, or a ct-term-prior / term-evidence snippet — and QUOTES the phrase.
  - Two clearly separated sections: "GROUNDED IN CORPUS" vs "MY INFERENCE".
  - "Not found in corpus" is a VALID, valuable answer for any SQ. Do NOT invent.
  - Confidence per SQ (high/med/low) + one line why.
  - You are PROPOSING, not certifying. claude-6/Joe adjudicate.

RETRIEVAL (use processed surfaces; raw eprints are .tar.gz — extract only if needed):
  - ~/code/storage/futon6/data/arxiv-math-ct-eprints/  (9,798 sources)
  - futon6/data/ct-term-prior.json                     (term doc-frequencies)
  - futon6/data/showcases/ct-anatomy/                  (30-paper scope audit)
  - futon6/scripts/build-arxiv-ct-term-evidence.py     (definitional snippets)
  - nLab pages / canon_store (see arxiv-coherence-mathct-50.json)

HOW TO ANSWER INTO ArSE: your surface header shows your ArSE thread id (ask-...).
When done, post your grounded answer with (substitute your id + the ask-id):
  python3 /home/joe/code/futon3c/scripts/agency_send.py \
    --to claude-6 --from <your-id> --kind bell --type answer --ref <ask-id> <<'ANS'
  <your two-section grounded answer>
  ANS
EOF

echo "=== E-arse-ct-probe: dispatching to ${WORKERS[*]} ${DRY:+(dry-run)} ==="
for w in "${WORKERS[@]}"; do
  echo "--- $w ---"
  printf '%s\n' "$PROMPT" | $SEND --to "$w" --from "$FROM" --kind bell --type query $DRY
  echo
done
echo "=== dispatched. Watch: curl -s localhost:7070/api/alpha/arse/unanswered | jq . ==="
