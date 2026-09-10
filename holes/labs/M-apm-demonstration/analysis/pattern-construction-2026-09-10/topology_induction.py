#!/usr/bin/env python3
"""Reproduce analyst-selected witness excerpts and complete root coverage.

Selection is a documented human/agent annotation, not automatic induction.
All source bytes come from the census Git pin and are checked against it.
"""
import hashlib
from collections import Counter
import json
import subprocess
from topology_census import ROOT, HERE, PIN

# Each source is deliberately selected after reading its proof. An import-only
# association is generated separately and must not be interpreted as proof use.
WITNESSES = {
    'T1': [('problems/t96A03/lean/Main.lean', 'eqvGen_iff_coordinateEqvGen'),
           ('problems/t01J03/lean/Main.lean', 'homeomorph_coordinateGluing'),
           ('problems/t02A03/lean/Main.lean', 'productionHomeomorph')],
    'T2': [('problems/t93A04/lean/Main.lean', 'null_critical'),
           ('problems/t92J07/lean/Main.lean', 'measure_iUnion_null'),
           ('problems/t94J05/lean/Main.lean', 'not_surjective_of_fixedChart_coordinateNull'),
           ('problems/t95J04/lean/Main.lean', 'not_surjective_of_fixedChart_coordinateNull')],
    'T3': [('problems/t01A03/lean/Main.lean', 'H1map_injective_of_retraction'),
           ('problems/t91J02/lean/Main.lean', 'fundamentalGroupMap_injective_of_retract')],
    'T4': [('ConstructionTargets/GenericTargetNaturalSourceReconstruction.lean', 'forwardRoundTripImageIso'),
           ('ConstructionTargets/TwiceSpiralSourceEquivalence.lean', 'gluingRoundTripImageIso')],
    'T5': [('problems/t92J02/lean/Main.lean', 'homeomorph_of_compact_to_T2'),
           ('problems/t94J02/lean/Main.lean', 'topology_eq_of_compact_to_hausdorff')],
    'T6': [('ConstructionTargets/LocalHomologyNeighborhoodExcision.lean', 'neighborhoodInclusion_chain_eq'),
           ('ConstructionTargets/CoefficientRelativeSmall.lean', 'comparisonIso_hom'),
           ('ConstructionTargets/CoefficientExcision.lean', 'homologyIso_hom')],
}


def main():
    census = json.loads((HERE / 'topology-census.json').read_text())
    assert census['git_pin'] == PIN
    witnesses = {}
    for pattern, pairs in WITNESSES.items():
        rows = []
        for path, anchor in pairs:
            raw = subprocess.check_output(['git', '-C', str(ROOT), 'show', PIN + ':' + path])
            digest = hashlib.sha256(raw).hexdigest()
            assert digest == census['source_sha256'][path]
            lines = raw.decode().splitlines()
            hits = [i for i, line in enumerate(lines) if anchor in line]
            assert hits, (path, anchor)
            # All hits retained, so a docstring mention cannot hide the proof.
            excerpts = []
            for i in hits:
                start, end = max(0, i - 3), min(len(lines), i + 24)
                excerpts.append({'first_line': start + 1, 'last_line': end,
                                 'text': '\n'.join(lines[start:end])})
            rows.append({'path': path, 'sha256': digest, 'anchor': anchor,
                         'excerpts': excerpts,
                         'completed_consumers_via_imports': census['module_consumers'].get(path, []),
                         'evidence': 'analyst-inspected source construction; imports are availability only'})
        witnesses[pattern] = rows
    coverage = []
    for pid in census['summary']['completed_candidate_ids']:
        path = f'problems/{pid}/lean/Main.lean'
        direct, available = [], []
        for pattern, pairs in WITNESSES.items():
            if any(p == path for p, _ in pairs):
                direct.append(pattern)
            if any(p in census['dependency_closure'][pid] for p, _ in pairs):
                available.append(pattern)
        coverage.append({'problem': pid, 'inspected_direct_witness_for': direct,
                         'witness_module_available_via_imports': available,
                         'no_selected_witness': not direct and not available})
    dispositions = {}
    for ledger in census['worklists']:
        rows = [r for r in ledger['selected']['items'] if r['state'] == 'done']
        dispositions[ledger['path']] = {
            'done': len(rows),
            'author_outcomes': dict(Counter(r.get('evidence', {}).get('outcome') for r in rows)),
            'review_outcomes': dict(Counter(r.get('evidence', {}).get('review', {}).get('outcome') for r in rows)),
            'with_commit': sum(bool(r.get('evidence', {}).get('commit')) for r in rows)}
    result = {'schema': 'topology-pattern-induction-v1', 'git_pin': PIN,
              'method': 'full-population census followed by analyst-selected multi-witness comparisons',
              'candidates': witnesses, 'all_completed_candidate_coverage': coverage,
              'done_dispositions': dispositions,
              'unassigned_roots': [r['problem'] for r in coverage if r['no_selected_witness']]}
    (HERE / 'topology-induction.json').write_text(json.dumps(result, indent=2, ensure_ascii=False) + '\n')
    print(json.dumps({'candidates': len(witnesses), 'coverage_rows': len(coverage),
                      'unassigned': len(result['unassigned_roots']), 'dispositions': dispositions}, indent=2))


if __name__ == '__main__':
    main()
