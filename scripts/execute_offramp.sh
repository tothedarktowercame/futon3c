#!/usr/bin/env bash
# execute_offramp.sh — the Stage-2 EXECUTION witness for the offramp.
#
# travel_offramp.sh crosses outer->inner and comes back with a *predicted*
# construction (data/offramp-wirings/<base>.wiring.edn) via the SEMILATTICE fold
# (futon2.aif.fold-semilattice: one box per pattern, wired by the cascade's own
# descent/co_app edges). That is a paper design. This step EXECUTES the same
# cascade as a SEMILATTICE ROLLOUT and re-observes what actually assembles and
# discharges the mission's want. Only when the construction genuinely chains and
# discharges do we mint the INSTANTIATE formal witness:
#
#     data/offramp-wirings/<base>.executed.edn
#
# read by futon3c.logic.mission-clean/instantiate-witness? to discharge the
# INSTANTIATE phase (V2 earned-closure: a live instance, not a paper design).
#
# EXECUTOR = rollout_execute.py (semilattice rollout; consumes=IF+HOWEVER,
# produces=THEN; discharge=want_coverage), the slice-1 "good results" mechanism
# (M-G-over-cascades.md §4). It reproduces the construction from the patterns'
# OWN TEXT (token-frontier chaining), independent of the predictor's phylogeny-
# edge fold, so realized != expected by construction — no tautology.
#
# NOTE the earlier strict rule-table executor (futon2.aif.enact/engine-wiring ->
# fold_engine.clj) was WRONG for a semilattice cascade: a linear pipeline that
# only folds RULES-listed patterns, it returned boxes=0 for any mission whose
# patterns aren't rule-encoded (M-learning-loop). The cascade is a semilattice,
# not a strict pipeline (operator ruling; previous session).
#
# SCOPE (honest): slice-1 want_coverage PROXY discharge. NOT slice-2a's
# discharge-trained prior nor slice-2b's live meme.gates commit.
#
# WITNESS BAR: mint iff folded-count >= 1 AND discharge > 0 (the construction
# assembles and covers some of the want). discharge magnitude is recorded as the
# realized signal. Otherwise abstain: INSTANTIATE stays a hole, honestly.
#
# ARTIFACT-ONLY: no substrate write, no outward action. Kept SEPARATE from
# travel_offramp so producing the design and promoting it to an executed
# instance are two operator decisions.
#
# Usage: execute_offramp.sh <mission-id> <mission-doc-path>
set -euo pipefail

[[ $# -lt 2 ]] && { echo "usage: $0 <mission-id> <mission-doc-path>" >&2; exit 64; }
MISSION="$1"; DOC="$2"
BASE="${MISSION##*/}"
PY=/home/joe/code/futon3a/.venv/bin/python
LAB=/home/joe/code/futon3a/holes/labs/M-memes-arrows
CASCADE_EDN="/tmp/offramp-${BASE}.edn"
WIRING="/home/joe/code/futon3c/data/offramp-wirings/${BASE}.wiring.edn"
EXECUTED="/home/joe/code/futon3c/data/offramp-wirings/${BASE}.executed.edn"
ROLLOUT_JSON="/tmp/rollout-execute-${BASE}.json"
TICK="$(date +%s)"

[[ -f "$WIRING" ]] || { echo "[execute-offramp] $MISSION no predicted wiring at $WIRING -> run travel_offramp.sh first" >&2; exit 65; }

# --- cascade stage: regenerate so :shown / :semilattice are fresh/deterministic ---
"$PY" "$LAB/offramp_cascade.py" "$DOC" "$CASCADE_EDN" >&2

# --- execution stage: SEMILATTICE ROLLOUT over the cascade's own patterns ---
"$PY" "$LAB/rollout_execute.py" "$DOC" "$CASCADE_EDN" "$ROLLOUT_JSON" > "$ROLLOUT_JSON.full"

# --- witness stage: mint iff the construction assembled AND discharged ---
"$PY" - "$MISSION" "$ROLLOUT_JSON.full" "$EXECUTED" "$TICK" "$CASCADE_EDN" <<'PY'
import sys, json
mission, jf, out, tick, cascade_edn = sys.argv[1:6]
r = json.load(open(jf))
folded = r["folded-count"]; discharge = r["discharge"]
if folded >= 1 and discharge > 0:
    def edn(x):
        if isinstance(x, bool): return "true" if x else "false"
        if isinstance(x, str): return '"' + x.replace('\\','\\\\').replace('"','\\"') + '"'
        if isinstance(x, (int, float)): return repr(x)
        if isinstance(x, list): return "[" + " ".join(edn(v) for v in x) + "]"
        if isinstance(x, dict): return "{" + " ".join(f":{k} {edn(v)}" for k, v in x.items()) + "}"
        return '"' + str(x) + '"'
    witness = {
        "mission": mission,
        "enacted-wiring": r["wiring"],
        "realized-outcome": {"policy": mission, "realized-G": discharge, "tick": int(tick)},
        "executor": "rollout_execute.py/semilattice-rollout",
        "discharge": discharge,
        "provenance": {"source": "semilattice-rollout", "want-signature": r["want-signature"],
                       "folded-count": folded, "unfolded-count": r["unfolded-count"],
                       "want-tokens": r.get("want-tokens", 0), "tick": int(tick),
                       "discharge-grain": "slice-1-want-coverage-proxy", "artifact-only": True},
    }
    open(out, "w").write(edn(witness) + "\n")
    print(f"[execute-offramp] {mission} EXECUTED boxes={folded} discharge={discharge} "
          f"realized-G={discharge} -> {out} (INSTANTIATE witnessed)")
else:
    print(f"[execute-offramp] {mission} NO-DISCHARGE boxes={folded} discharge={discharge} "
          f"-> abstain (construction did not assemble/cover the want; INSTANTIATE stays a hole, honestly)")
PY
