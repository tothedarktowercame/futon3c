"""Read-only historical store probe; emit timing/error metadata, no payloads."""
import collections
import hashlib
import json
import re
import subprocess

cmd = ['journalctl', '--user', '-u', 'futon1b-zone.service',
       '--since', '2026-09-10 12:00:00', '--until', '2026-09-11 00:44:30',
       '--no-pager', '-o', 'short-iso']
raw = subprocess.check_output(cmd)
lines = raw.decode().splitlines()
slow = []
evidence = []
reasons = collections.Counter()
for line in lines:
    if 'SQLITE_BUSY' in line:
        reasons['SQLITE_BUSY'] += 1
    m = re.search(r'elapsed-ms=(\d+)', line)
    if m and int(m[1]) >= 25000 and '[futon1b-request] end ' in line:
        method = re.search(r'method=(\S+)', line)
        route = re.search(r'uri=([^? ]+)', line)
        outcome = re.search(r'outcome=(\S+)', line)
        slow.append({'at': line.split()[0], 'duration_ms': int(m[1]),
                     'method': method[1] if method else None,
                     'route': route[1] if route else None,
                     'outcome': outcome[1] if outcome else None,
                     'source_moved': 'memory-projection-source-moved-after-quiescence' in line})
    if 'uri=/api/alpha/evidence/e-apm-promotion-2588028e88b64f282e2b31e7ed5313b0 ' in line:
        evidence.append({'at': line.split()[0], 'event': 'end' if ' end ' in line else 'start',
                         'duration_ms': int(m[1]) if m else None,
                         'disconnected': 'client-disconnected' in line})
print(json.dumps({'command': cmd, 'journal_sha256': hashlib.sha256(raw).hexdigest(),
                  'line_count': len(lines), 'slow_completed_operations': slow,
                  'error_counts': reasons, 'f218_evidence_requests': evidence,
                  'limitations': ['Request durations begin at handler entry, excluding worker admission.',
                                  'Old requests without unique trace IDs cannot be paired unambiguously.',
                                  'Journal receipt timestamps can lag message production.',
                                  'SQLite index lock failures are observed; causality for APM timeout is not proved.']}, indent=2))
