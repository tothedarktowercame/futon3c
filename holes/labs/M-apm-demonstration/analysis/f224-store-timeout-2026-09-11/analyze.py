"""Read short-iso Futon1b journal export; emit endpoint timing statistics only."""
import collections
import json
import re
import sys

lines = open(sys.argv[1]).read().splitlines()
reads = []
for line in lines:
    for m in re.finditer(
        r'\[futon1b-request\] end method=(\S+) uri=(\S+) trace-id=(\S+) '
        r'elapsed-ms=(\d+) outcome=([a-z-]+)', line
    ):
        if m[1] == 'GET' and '/api/alpha/hyperedges?end=' in m[2]:
            reads.append({'at': line[:25], 'elapsed_ms': int(m[4]), 'outcome': m[5]})
v = sorted(r['elapsed_ms'] for r in reads)
print(json.dumps({
    'window': ['2026-09-11T18:02:00Z', '2026-09-11T18:18:00Z'],
    'endpoint_reads': len(reads),
    'p50_ms': v[len(v) // 2], 'p95_ms': v[int(len(v) * .95)],
    'p99_ms': v[int(len(v) * .99)], 'max_ms': max(v),
    'over_5000_ms': [r for r in reads if r['elapsed_ms'] > 5000],
    'outcomes': dict(collections.Counter(r['outcome'] for r in reads)),
    'service_executor_rejections': sum('capacity-exhausted=true' in l for l in lines),
    'rejection_scope': 'pool and client unknown; not necessarily main HTTP pool',
}, indent=2))
