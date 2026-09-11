"""Offline versioned library revision loop; identities/reviews are supplied attestations.

Pure transitions, no live store writes. The CLI replays a local event packet and
prints its resulting library and audit history. Content hashes bind review/use.
"""
import argparse
import copy
import hashlib
import json
from pathlib import Path

FIELDS = {
    'retrieval': {'caption', 'example_links'},
    'applicability': {'conditions', 'failure_contrasts'},
    'execution': {'construction', 'example_links'},
}


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(',', ':'),
                                     ensure_ascii=False, allow_nan=False).encode()).hexdigest()


def require(ok, reason):
    if not ok:
        raise ValueError(reason)


def text(value):
    return isinstance(value, str) and bool(value.strip())


def initial(entries):
    require(isinstance(entries, dict), 'library must be a map')
    return {'library': {key: {'revision': digest(body), 'body': copy.deepcopy(body)}
                        for key, body in entries.items()},
            'proposals': {}, 'history': [], 'observations': [], 'attempts': {}}


def apply(state, event):
    """Reject invalid transitions without changing the caller's state."""
    state = copy.deepcopy(state)
    kind = event['type']
    if kind == 'propose':
        target = event['pattern']
        require(target in state['library'], 'unknown pattern: new entries need a separate admission policy')
        current = state['library'][target]
        require(event['base_revision'] == current['revision'], 'stale base revision')
        require(text(event['author']), 'missing author')
        trigger = event['trigger']
        require(all(text(trigger.get(k)) for k in ['problem', 'node', 'cascade_digest', 'evidence']),
                'missing triggering obligation or evidence')
        category = event['category']
        patch = event['patch']
        require(category in FIELDS and isinstance(patch, dict) and bool(patch), 'invalid revision category/patch')
        require(set(patch) <= FIELDS[category], 'patch exceeds diagnosed category')
        require(all(text(v) or (isinstance(v, list) and v and all(text(x) for x in v))
                    for v in patch.values()), 'empty revision content')
        require(text(event['expected_change']), 'missing intended behavioral change')
        body = {**current['body'], **patch}
        require(body != current['body'], 'no-op revision')
        proposal_id = digest(event)
        require(proposal_id not in state['proposals'], 'duplicate proposal')
        state['proposals'][proposal_id] = {
            'proposal': copy.deepcopy(event), 'candidate': body,
            'revision': digest(body), 'status': 'proposed'}
    else:
        proposal_id = event['proposal_id']
        require(proposal_id in state['proposals'], 'unknown proposal')
        p = state['proposals'][proposal_id]
        if kind == 'review':
            require(p['status'] == 'proposed', 'proposal already reviewed')
            require(text(event['reviewer']) and event['reviewer'] != p['proposal']['author'],
                    'reviewer must differ from author')
            require(event['revision'] == p['revision'], 'review of wrong revision')
            require(event['verdict'] in ['accept', 'reject'] and text(event['evidence']),
                    'missing review verdict/evidence')
            p['review'] = copy.deepcopy(event)
            p['status'] = 'accepted' if event['verdict'] == 'accept' else 'rejected'
        elif kind == 'publish':
            require(p['status'] == 'accepted', 'publication requires accepted review')
            target = p['proposal']['pattern']
            require(state['library'][target]['revision'] == p['proposal']['base_revision'],
                    'publication conflict: library changed since proposal')
            state['library'][target] = {'revision': p['revision'], 'body': copy.deepcopy(p['candidate'])}
            p['status'] = 'published'
            p['published_sequence'] = len(state['history'])
            p['next_use'] = 'not-yet-observed'
        elif kind == 'begin-use':
            require(p['status'] == 'published', 'use requires publication')
            require(text(event['attempt_id']) and event['attempt_id'] not in state['attempts'], 'duplicate/missing attempt')
            require(text(event['problem']) and event['problem'] != p['proposal']['trigger']['problem'],
                    'next-use must concern a different problem')
            require(text(event['student']), 'missing student')
            entry = state['library'][p['proposal']['pattern']]
            require(entry['revision'] == p['revision'], 'revision superseded: start from current library')
            state['attempts'][event['attempt_id']] = {
                **copy.deepcopy(event), 'revision': p['revision'], 'entry': copy.deepcopy(entry),
                'started_sequence': len(state['history'])}
        elif kind == 'observe':
            require(p['status'] == 'published', 'next-use requires published revision')
            require(event['revision'] == p['revision'], 'observation of wrong revision')
            require(text(event['problem']) and event['problem'] != p['proposal']['trigger']['problem'],
                    'next-use must concern a different problem')
            require(text(event['student']) and text(event['evidence']), 'missing use identity/evidence')
            attempt = state['attempts'].get(event['attempt_id'])
            require(attempt is not None, 'unknown attempt')
            require(all(attempt[k] == event[k] for k in ['proposal_id', 'revision', 'problem', 'student']),
                    'observation does not match started attempt')
            require(attempt['started_sequence'] > p['published_sequence'], 'attempt predates publication')
            require(not any(x['attempt_id'] == event['attempt_id'] for x in state['observations']),
                    'attempt observation already recorded')
            for field in ['retrieved', 'read', 'applicable', 'proof_used', 'useful']:
                require(type(event[field]) is bool or event[field] == 'unknown',
                        'observation fields must be booleans or explicit unknown')
            require(event['read'] is not True or event['retrieved'] is True, 'read without retrieval')
            require(event['proof_used'] is not True or (event['read'] is True and event['applicable'] is True),
                    'proof use without read/applicability')
            if event['useful'] is True:
                require(event['proof_used'] is True and text(event.get('reviewer')) and
                        event['reviewer'] not in [event['student'], p['proposal']['author']] and
                        text(event.get('review_evidence')), 'usefulness requires independent reviewed proof use')
            state['observations'].append(copy.deepcopy(event))
            # A later failed use is retained; it does not erase an earlier witness.
            useful = any(x['proposal_id'] == proposal_id and x['useful'] is True for x in state['observations'])
            p['next_use'] = 'reviewed-useful-example' if useful else 'observed-without-usefulness-witness'
        else:
            raise ValueError('unknown transition')
    state['history'].append({'sequence': len(state['history']), 'event_id': digest(event),
                             'event': copy.deepcopy(event)})
    return state


def proposal_from_obligation(state, cascade, node_id, *, author, pattern, category,
                             patch, evidence, expected_change):
    """Connect a TA diagnosis to an actual node in the proof-plan representation."""
    nodes = [n for n in cascade['nodes'] if n['id'] == node_id]
    require(len(nodes) == 1, 'trigger node missing or ambiguous')
    return {'type': 'propose', 'author': author, 'pattern': pattern,
            'base_revision': state['library'][pattern]['revision'], 'category': category,
            'patch': patch, 'expected_change': expected_change,
            'trigger': {'problem': cascade['problem'], 'node': node_id,
                        'cascade_digest': digest(cascade), 'goal': nodes[0]['goal'],
                        'evidence': evidence}}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('packet', type=Path)
    args = parser.parse_args()
    packet = json.loads(args.packet.read_text())
    state = initial(packet['library'])
    for event in packet['events']:
        state = apply(state, event)
    print(json.dumps(state, indent=2))
