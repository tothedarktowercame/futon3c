"""Check provenance and obligation references; does not verify mathematics."""
import hashlib
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[4]
packet = json.loads((HERE / 'a93A01-ta-cascade.json').read_text())
source = ROOT / packet['proof_source']['path']
assert hashlib.sha256(source.read_bytes()).hexdigest() == packet['proof_source']['sha256']
prior = json.loads((HERE.parent / 'pattern-construction-2026-09-10/prelim-sources.json').read_text())
by_id = {s['path'].removeprefix('library/').removesuffix('.flexiarg'): s
         for s in prior['sources'] if s['repo'] == 'futon3'}
nodes = {n['id']: n for n in packet['nodes']}
assert len(nodes) == len(packet['nodes'])
for node in nodes.values():
    assert node['parent'] is None or node['parent'] in nodes
    for child in node['children']:
        assert nodes[child]['parent'] == node['id']
    for method in node['methods']:
        s = by_id[method['id']]
        assert method['git_pin'] == s['pin'] and method['sha256'] == s['sha256']
        assert hashlib.sha256(s['text'].encode()).hexdigest() == s['sha256']

def visit(key, ancestors):
    assert key not in ancestors, ('cyclic proof dependency', key)
    for needed in nodes[key]['children'] + nodes[key]['depends_on']:
        visit(needed, ancestors | {key})

visit('P', set())
assert not packet['student_trial'] and not packet['live_role_changed']
print('PASS: 11 unique nodes; acyclic obligations; source and pattern revisions verified.')
print('Scope: provenance/structure only. No Student trial or fresh Lean check.')
