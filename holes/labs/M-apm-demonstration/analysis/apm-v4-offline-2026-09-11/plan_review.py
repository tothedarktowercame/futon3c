"""Offline review bookkeeping, never a mathematical proof checker.

Input: the retained TA cascade plus a separate, node-indexed review map.
No service, model, filesystem write or implicit library promotion in assess().
"""
import argparse
import json
from pathlib import Path


def assess(packet, reviews):
    errors, pending = [], []
    nodes = packet['nodes']
    by_id = {n['id']: n for n in nodes}
    if len(by_id) != len(nodes):
        errors.append('duplicate-node-id')
    roots = [n['id'] for n in nodes if n['parent'] is None]
    if len(roots) != 1:
        errors.append('expected-one-root')
    for key in reviews:
        if key not in by_id:
            errors.append('unknown-review-node:' + key)
    for n in nodes:
        key = n['id']
        for field in ['goal', 'conditions']:
            if not isinstance(n.get(field), str) or not n[field].strip():
                errors.append('missing-' + field + ':' + key)
        parent = n['parent']
        if parent is not None and (parent not in by_id or key not in by_id[parent]['children']):
            errors.append('parent-not-reciprocal:' + key)
        for child in n['children']:
            if child not in by_id or by_id[child]['parent'] != key:
                errors.append('child-not-reciprocal:' + key)
        for target in n['children'] + n['depends_on']:
            if target not in by_id:
                errors.append('unknown-obligation:' + target)
        for method in n['methods']:
            if not all(method.get(f) for f in ['id', 'git_pin', 'sha256']):
                errors.append('unpinned-method:' + key)
        review = reviews.get(key, {})
        state = review.get('conditions', 'open')
        if state not in ['established', 'open', 'refuted']:
            errors.append('invalid-condition-status:' + key)
        if state != 'established':
            pending.append({'node': key, 'reason': 'conditions-' + state,
                            'conditions': n.get('conditions')})
        elif not isinstance(review.get('condition_argument'), str) or not review['condition_argument'].strip():
            errors.append('missing-condition-argument:' + key)
        if not isinstance(review.get('proof_argument'), str) or not review['proof_argument'].strip():
            pending.append({'node': key, 'reason': 'missing-proof-argument'})
        # A method citation, old paper-proof status or candidate draft never
        # supplies a proof argument or an applicability judgment implicitly.
    colors = {}
    def visit(key):
        if key not in by_id:
            return
        if colors.get(key) == 1:
            errors.append('cyclic-obligation:' + key)
            return
        if colors.get(key) == 2:
            return
        colors[key] = 1
        n = by_id[key]
        for target in n['children'] + n['depends_on']:
            visit(target)
        colors[key] = 2
    # Inspect every component, including disconnected cycles.
    for key in by_id:
        visit(key)
    reachable = set()
    def descend(key):
        if key in reachable or key not in by_id:
            return
        reachable.add(key)
        for child in by_id[key]['children']:
            descend(child)
    if len(roots) == 1:
        descend(roots[0])
        errors.extend('unreachable-node:' + k for k in by_id if k not in reachable)
    return {'schema': 'apm-v4-offline-review-v1',
            'structural_errors': sorted(set(errors)), 'open_obligations': pending,
            'ready_for_ta_review': not errors and not pending,
            'mathematics_verified': False,
            'scope': 'Review completeness only; arguments require independent mathematical adjudication.'}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('cascade', type=Path)
    parser.add_argument('reviews', type=Path)
    args = parser.parse_args()
    print(json.dumps(assess(json.loads(args.cascade.read_text()),
                           json.loads(args.reviews.read_text())), indent=2))
