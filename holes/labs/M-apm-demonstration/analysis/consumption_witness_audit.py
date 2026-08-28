#!/usr/bin/env python3
"""Audit which Agency role jobs have a durable performed-consumption witness.

Background
----------
A job's Agency `delivery` record attests that the server *prepared* a response
(`http-response-consumption-unconfirmed`) or that the caller polls rather than
holding a seat (`caller-not-a-registered-seat`).  Neither states that any
payload was received, parsed and validated.  This tool looks instead for a
downstream artifact that cannot exist unless consumption happened.

What counts as a witness
------------------------
ACCEPTED: the job id appears in a phase receipt (`live/*.edn`) or a terminal
record (`terminal/*.edn`).  Those files are written only after the phase read
the terminal payload and validated it, so their existence entails consumption.

REJECTED, deliberately:
  * `certificates/*.edn` -- every one is a `:campaign-projection`.  A
    projection enumerates jobs; it says nothing about consuming their output.
    One projection in f50 names all 20 jobs at once, which would make any
    substring rule report a perfect score.
  * `ledger.edn` -- a campaign event log.  A job id there may record only that
    the job was dispatched.

Substring presence in a shared file is not a witness.  That distinction is the
whole point of this tool: the naive rule scores f50 at 20/20, the defensible
one at 11/20.
"""
import argparse, glob, json, os, re, sys
import urllib.request

ROLE_ID = re.compile(r'apm-role-[0-9a-f]{64}')


def agency_jobs(base, limit=500):
    url = f'{base}/api/alpha/invoke/jobs?limit={limit}'
    with urllib.request.urlopen(url, timeout=20) as r:
        return json.load(r)['jobs']


def witness_index(frame_dir):
    """Map role-job-id -> [receipt files that name it]. Only post-validation files."""
    idx = {}
    for pattern in ('live/*.edn', 'terminal/*.edn'):
        for path in glob.glob(os.path.join(frame_dir, pattern)):
            try:
                text = open(path, errors='ignore').read()
            except OSError:
                continue
            for jid in set(ROLE_ID.findall(text)):
                idx.setdefault(jid, []).append(os.path.basename(path))
    return idx


def audit(frame_dir, frame_prefix, base):
    jobs = [j for j in agency_jobs(base)
            if (j.get('agent-id') or '').startswith(frame_prefix)]
    idx = witness_index(frame_dir)
    rows = []
    for j in sorted(jobs, key=lambda x: x.get('finished-at') or ''):
        receipts = sorted(idx.get(j['job-id'], []))
        rows.append({
            'job-id': j['job-id'],
            'agent-id': j.get('agent-id'),
            'state': j.get('state'),
            'delivery-note': (j.get('delivery') or {}).get('note'),
            'receipts': receipts,
            'verdict': 'consumption-witnessed' if receipts else 'no-durable-witness',
        })
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--campaign', default='jit-all-open-v2')
    ap.add_argument('--frame', default='f50')
    ap.add_argument('--root', default='data/apm-campaigns')
    ap.add_argument('--base', default='http://localhost:7070')
    ap.add_argument('--json', action='store_true')
    a = ap.parse_args()

    frame_dir = os.path.join(a.root, a.campaign, f'{a.campaign}-{a.frame}')
    if not os.path.isdir(frame_dir):
        print(f'no such frame directory: {frame_dir}', file=sys.stderr)
        return 2

    rows = audit(frame_dir, f'{a.frame}-', a.base)
    witnessed = [r for r in rows if r['verdict'] == 'consumption-witnessed']

    if not rows:
        # The Agency job list is a bounded recent window; an older frame's jobs
        # age out of it.  That is not the same as "no job was witnessed", so
        # refuse to report a score rather than print a misleading 0/0.
        msg = (f'no Agency jobs found for agent prefix {a.frame}- '
               '(likely aged out of the job window); no score reported')
        print(json.dumps({'frame': a.frame, 'error': 'no-jobs-in-window'})
              if a.json else f'  {msg}')
        return 3

    if a.json:
        print(json.dumps({'frame': a.frame, 'total': len(rows),
                          'witnessed': len(witnessed), 'rows': rows}, indent=2))
        return 0

    for r in rows:
        mark = 'WITNESSED ' if r['receipts'] else 'no-witness'
        print(f"  {mark}  {(r['agent-id'] or '?')[:24]:24} "
              f"{(r['delivery-note'] or '-')[:38]:38} {','.join(r['receipts']) or '-'}")
    print()
    print(f'  {len(witnessed)}/{len(rows)} jobs have a durable consumption witness')
    gaps = {}
    for r in rows:
        if not r['receipts']:
            gaps[r['agent-id']] = gaps.get(r['agent-id'], 0) + 1
    if gaps:
        print('  gaps by role: ' + ', '.join(f'{k} x{v}' for k, v in sorted(gaps.items())))
    return 0


if __name__ == '__main__':
    sys.exit(main())
