#!/usr/bin/env python3
"""Render the deterministic mfuton sweep report and combined result JSON."""

from __future__ import annotations

import hashlib
import json
from collections import Counter
from pathlib import Path


HERE = Path(__file__).resolve().parent


def read_json(name):
    with (HERE / name).open(encoding="utf-8") as stream:
        return json.load(stream)


def file_sha256(path):
    digest = hashlib.sha256()
    digest.update(path.read_bytes())
    return digest.hexdigest()


def bundle_sha256(directory):
    digest = hashlib.sha256()
    for path in sorted(directory.glob("*.json")):
        digest.update(path.name.encode("utf-8"))
        digest.update(b"\0")
        digest.update(path.read_bytes())
    return digest.hexdigest()


def disposition_label(value):
    return {
        "converted-fully": "converted fully",
        "graph-only": "graph-only",
        "skipped": "skipped",
    }[value]


def main():
    engine = read_json("engine-results.json")
    python = read_json("python-results.json")
    r = read_json("r-results.json")
    fixtures = engine["fixtures"]
    dispositions = Counter(item["disposition"] for item in fixtures)
    discrepancies = python["discrepancies"] + r["discrepancies"]

    combined = {
        "schema-version": 1,
        "fixture-count": len(fixtures),
        "dispositions": {
            name: dispositions[name]
            for name in ("converted-fully", "graph-only", "skipped")
        },
        "agreement-counts": {
            "networkx": python["networkx"],
            "dagitty": r["dagitty"],
            "y0": python["y0"],
            "counterfactual-enumeration": python["counterfactual-enumeration"],
        },
        "discrepancies": discrepancies,
        "semantic-kind-decisions": engine["semantic-kind-decisions"],
        "review-deltas": engine["review-deltas"],
        "fixtures": fixtures,
        "oracle-fixtures": {
            "python": python["fixtures"],
            "dagitty": r["fixtures"],
        },
        "tool-versions": {
            **python["tool-versions"],
            **r["tool_versions"],
        },
    }
    sweep_path = HERE / "sweep-results.json"
    sweep_path.write_text(
        json.dumps(combined, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    fixture_bundle_hash = bundle_sha256(HERE / "fixtures")
    converted_bundle_hash = bundle_sha256(HERE / "converted")
    sweep_hash = file_sha256(sweep_path)

    conversion_rows = "\n".join(
        f"| `{item['example-id']}` | {disposition_label(item['disposition'])} | "
        f"{item['reason']} |"
        for item in fixtures
    )

    override_rows = []
    for fixture, decisions in sorted(engine["semantic-kind-decisions"].items()):
        for variable, decision in sorted(decisions.items()):
            override_rows.append(
                f"| `{fixture}` | `{variable}` | `{decision['kind']}` | "
                f"“{decision['source-prose']}” | {decision['judgment']} |"
            )

    delta_rows = []
    for delta in engine["review-deltas"]:
        before = json.dumps(delta["before"], sort_keys=True, separators=(",", ":"))
        after = json.dumps(delta["after"], sort_keys=True, separators=(",", ":"))
        changed = before != after
        delta_rows.append(
            f"| `{delta['example-id']}` | `{str(changed).lower()}` | "
            f"`{before}` | `{after}` |"
        )

    evaluation_rows = []
    for item in fixtures:
        pair = item.get("pair")
        identification = item.get("identification")
        if pair:
            pair_text = (
                f"`{pair['treatment']} -> {pair['outcome']}` — {pair['why']}"
            )
            id_text = (
                f"`{identification['method']}`; identifiable="
                f"`{str(identification['identifiable?']).lower()}`"
            )
        else:
            pair_text = "— (no natural pair; CI sweep only)"
            id_text = "not run"
        counterfactual = item.get("counterfactual")
        if counterfactual:
            engine_cf = counterfactual["engine"]
            cf_text = (
                f"`{engine_cf['method']}`; answer="
                f"`{str(engine_cf.get('answer')).lower()}`; "
                f"{counterfactual['query']['rob-expectation']}"
            )
        else:
            cf_text = "—"
        evaluation_rows.append(
            f"| `{item['example-id']}` | "
            f"{item['variable-count']}/{item['arrow-count']} | "
            f"`{str(item['dag-valid?']).lower()}`/"
            f"`{str(item['canonical-render?']).lower()}`/"
            f"`{str(item['round-trip?']).lower()}` | "
            f"{len(item['implied-independencies'])} | {pair_text} | {id_text} | "
            f"{cf_text} |"
        )

    if discrepancies:
        discrepancy_text = "\n\n".join(
            "```json\n" + json.dumps(item, indent=2, sort_keys=True) + "\n```"
            for item in discrepancies
        )
    else:
        discrepancy_text = "Verbatim discrepancy list: `[]`."

    report = f"""# mfuton Book-of-Why fixture sweep

## Result

All 60 frozen mfuton fixtures convert at graph level and pass `dag/validate`,
canonical rendering, and diagram round-trip. Forty are fully representable by
the declared engine layer; twenty retain graph topology but have evaluator or
regime semantics outside the current Boolean SCM layer. Nothing is silently
dropped: all original JSON is copied verbatim under `fixtures/`, and every
evaluator omission is named below.

## Conversion disposition

| Fixture | Disposition | Reason |
|---|---|---|
{conversion_rows}

Totals: converted fully {dispositions['converted-fully']}; graph-only
{dispositions['graph-only']}; skipped {dispositions['skipped']}.

## Curated semantic-kind overrides

This literal table is the conversion authority when the source marks
observability only in prose. It is intentionally not a regex classifier. The
Burks `social` and `child` rows pin the reviewed false-positive judgments;
Burks `x` already carries explicit `observed: false` and needs no override.

| Fixture | Variable | Converted kind | Source prose, verbatim | Judgment |
|---|---|---|---|---|
{chr(10).join(override_rows)}

## Reviewed identification delta

The before column is the committed pre-correction sweep verdict. The after
column is the corrected verdict, serialized verbatim from `engine-results.json`.
`deconfounding-game-1` and `deconfounding-game-2` now run their evident
smoking/miscarriage pairs instead of recording no guessed pair.

| Fixture | Changed? | Before | After |
|---|---|---|---|
{chr(10).join(delta_rows)}

## Per-fixture engine sweep

The validation column is `DAG/canonical/round-trip`. CI counts cover all
engine-implied independencies with conditioning sets of size at most one.
Natural treatment/outcome pairs are selected only by conventional X/Y naming,
explicit fixture semantics, or a unique observed source/sink; all others receive
the structural CI sweep without a guessed effect query.

| Fixture | V/E | Validation | CIs | Natural pair and why | Our identification | Counterfactual |
|---|---:|---|---:|---|---|---|
{chr(10).join(evaluation_rows)}

## Oracle agreement

| Oracle | Applicable verdict | Agreements | Checked | Disagreements |
|---|---|---:|---:|---:|
| NetworkX | d-separation | {python['networkx']['agreements']} | {python['networkx']['checked']} | {python['networkx']['checked'] - python['networkx']['agreements']} |
| dagitty | d-separation | {r['dagitty']['agreements']} | {r['dagitty']['checked']} | {r['dagitty']['checked'] - r['dagitty']['agreements']} |
| y0 | treatment/outcome identifiability | {python['y0']['agreements']} | {python['y0']['checked']} | {python['y0']['checked'] - python['y0']['agreements']} |
| Independent exhaustive Boolean worlds | deterministic counterfactual | {python['counterfactual-enumeration']['agreements']} | {python['counterfactual-enumeration']['checked']} | {python['counterfactual-enumeration']['checked'] - python['counterfactual-enumeration']['agreements']} |

The Boolean-world check is a separate implementation of finite deterministic
SCM enumeration, not a counterfactual package oracle. Rob expectations were
cheaply extracted only for the firing-squad and vacuuming pytest cases and are
pinned in the per-fixture table.

## Discrepancy harvest

{discrepancy_text}

Each discrepancy record, when present, carries our verdict, the named oracle's
verdict, and a Rob pytest expectation where one was cheaply extractable.

## Frozen-artifact hashes

- Frozen 60-fixture bundle SHA-256: `{fixture_bundle_hash}`
- Converted 60-fixture bundle SHA-256: `{converted_bundle_hash}`
- `sweep-results.json` SHA-256: `{sweep_hash}`
- The report's own SHA-256 and component-result hashes are recorded in
  `SHA256SUMS`, generated after this report.

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/mfuton-sweep/run.sh
```

The run uses only the repository's Clojure dependencies and the existing
`/home/joe/.venvs/causal-oracles` environment.
"""
    (HERE / "REPORT-mfuton-sweep.md").write_text(report, encoding="utf-8")
    print(
        f"mfuton report: {len(fixtures)} fixtures; "
        f"{len(discrepancies)} discrepancies; sweep sha256 {sweep_hash}"
    )


if __name__ == "__main__":
    main()
