#!/usr/bin/env python3
"""P6o-3: append-only, reconstructed origin interpretations for 22–26 Sep 2026.

--dry-run --plan FILE captures a pinned evidence window and comparison examples.
Review that report before --write --plan FILE. Originals are never changed.
The writer's origin is harness/write-time; the BODY's attribution is explicitly
backfill-inferred. No inference grants authority or changes a promise.
"""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

START = '2026-09-22T00:00:00Z'
END = '2026-09-27T00:00:00Z'
VERSION = 'p6o3-reconstructed-v1'
EXPECTED = {'park-wake': 436, 'inbox-zero': 27, 'kimi-notice': 42}
ROOT = Path(__file__).resolve().parents[2]
TAG = 'xiang2000-p6o3'
PARK = re.compile(r'(?m)^--- resumed: (?:parked dependencies complete \((\d+)\)|DEADLINE EXPIRED with (\d+) of \d+ dependencies complete — the awaited work did NOT finish) ---\n?')
INBOX = re.compile(r'inbox-zero: \S+ is carrying \d+ dirty file\(s\) \([^\n]+\); \d+ of them were written during your turns\. Commit or delete what is yours and leave what is not\. Newest first: .+\. Full list: git -C \S+ status --porcelain', re.S)
KIMI = re.compile(r"You requisitioned kimi-\d+ for (?P<target>[MET]-[^\s,]+)(?: while clocked on [MET]-\S+| while not clocked in|, which is not what your clock said)\. If your work has moved to (?P=target), (?:clock in on it so your clock says what you are doing\.|this reminder clocks you onto it; if it hasn't, clock back onto what you are doing\.)")
REFUSAL = re.compile(r"You can't use a Kimi seat without a requisition\. Put one line in the call: `Requisition: <M-\*\|E-\*\|T-\*> — <one-line purpose>` — your clock says (?P<target>[MET]-\S+); if that is what this is: `Requisition: (?P=target) — <purpose>`\. The seat keeps its conversation per target and (?:clears|compacts) it when the target changes\. \(Refused by kimi-\d+\.\)")


def now():
    return datetime.now(timezone.utc).isoformat().replace('+00:00', 'Z')


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, ensure_ascii=False).encode()).hexdigest()


def classify(text):
    """Match full producer templates, not mentions. A wake is a delivery of
    possibly mixed-authorship text; the attribution concerns the delivery act.
    Fenced/quoted examples of a wake are not delivery evidence.
    """
    if not isinstance(text, str):
        return None
    for match in PARK.finditer(text):
        prefix = text[:match.start()]
        if prefix.count('```') % 2 or prefix.count('~~~') % 2:
            continue
        tail = text[match.end():]
        arrived = int(match.group(1) or match.group(2))
        if (arrived == 0 and not tail.strip()) or re.match(r'• [^\n:]+: ', tail):
            return {'rule': 'park-wake', 'origin': 'harness', 'actor': 'parked-resume',
                    'matched-span': [match.start(), len(text)],
                    'scope': 'delivery-act; prefix and dependency reports retain their own authorship'}
    if INBOX.fullmatch(text):
        return {'rule': 'inbox-zero', 'origin': 'harness', 'actor': 'inbox-zero'}
    if KIMI.fullmatch(text) or REFUSAL.fullmatch(text):
        return {'rule': 'kimi-notice', 'origin': 'harness', 'actor': 'kimi-work-target'}
    return None


class API:
    def __init__(self, base):
        self.base = base.rstrip('/') + '/api/alpha/evidence'

    def request(self, method, suffix='', payload=None):
        headers = {'Accept': 'application/json', 'Content-Type': 'application/json', 'X-Penholder': 'api'}
        data = None if payload is None else json.dumps(payload, ensure_ascii=False).encode()
        req = urllib.request.Request(self.base + suffix, data=data, headers=headers, method=method)
        try:
            with urllib.request.urlopen(req, timeout=90) as response:
                return response.status, json.load(response)
        except urllib.error.HTTPError as error:
            if error.code in (404, 409):
                return error.code, json.loads(error.read())
            raise RuntimeError(f'HTTP {error.code}: {error.read().decode()}') from error

    def rows(self, params, pin):
        params = dict(params, **{'limit': 1000, 'system-as-of': pin})
        rows, seen = [], set()
        while True:
            _, page = self.request('GET', '?' + urllib.parse.urlencode(params))
            if not isinstance(page.get('entries'), list):
                raise ValueError(f'Malformed evidence page: {page}')
            rows.extend(page['entries'])
            cursor = page.get('next-cursor')
            if not cursor:
                if page.get('incomplete'):
                    raise ValueError('Refusing incomplete evidence scan')
                return rows
            key = (cursor['at'], cursor['id'])
            if key in seen:
                raise ValueError('Repeated evidence cursor')
            seen.add(key)
            params.update({'cursor-at': key[0], 'cursor-id': key[1]})


def body(row):
    value = row.get('evidence/body', {})
    if isinstance(value, str):
        # Reuse P0's strict, data-only EDN decoder; never eval historical text.
        from xiang2000_p0 import edn
        value = edn(value)
    return value if isinstance(value, dict) else {}


def turn(row):
    b = body(row)
    return b.get('event') == 'chat-turn' and b.get('role') == 'user'


def example(row):
    text = body(row).get('text', '')
    match = PARK.search(text)
    preview = text[max(0, match.start() - 60):match.end() + 100] if match else text[:350]
    return {'id': row['evidence/id'], 'at': row['evidence/at'], 'text': preview}


def plan(api):
    pin = now()
    rows = api.rows({'author': 'joe', 'since': START, 'before': END}, pin)
    turns = [r for r in rows if turn(r) and START <= r['evidence/at'] < END]
    candidates, already_stamped = [], []
    for row in turns:
        hit = classify(body(row).get('text'))
        if hit:
            if row.get('evidence/origin', {}).get('kind') not in (None, 'unknown'):
                already_stamped.append(row['evidence/id'])
                continue
            candidates.append(dict(hit, **{'source-id': row['evidence/id'], 'source-at': row['evidence/at'],
                                            'source-hash': digest(row)}))
    window = ROOT / 'storage/operator-turns/window-0922'
    raw = [json.loads(line) for line in (window / 'operator-turns.jsonl').read_text().splitlines()]
    kept = [json.loads(line) for line in (window / 'operator-turns-joe.jsonl').read_text().splitlines()]
    raw = [r for r in raw if START <= (r.get('at') or '') < END]
    local_ids, kept_ids = {r['id'] for r in raw}, {r['id'] for r in kept}
    live_ids = {r['evidence/id'] for r in turns}
    counts = Counter(c['rule'] for c in candidates)
    comparison = {}
    for rule, expected in EXPECTED.items():
        hits = [r for r in turns if (classify(body(r).get('text')) or {}).get('rule') == rule]
        local = [r for r in raw if (classify(r.get('text')) or {}).get('rule') == rule]
        mentions = [r for r in turns if any(s in body(r).get('text', '') for s in
                    {'park-wake': ['parked dependencies complete', 'DEADLINE EXPIRED'],
                     'inbox-zero': ['inbox-zero'], 'kimi-notice': ['You requisitioned', "You can't use a Kimi"]}[rule])]
        comparison[rule] = {'expected': expected, 'candidates': counts[rule], 'delta': counts[rule] - expected,
                            'local-reconstructed': len(local),
                            'matched-examples': [example(r) for r in hits[:3]],
                            'live-only-examples': [example(r) for r in hits if r['evidence/id'] not in local_ids][:5],
                            'mentioned-but-not-matched': [example(r) for r in mentions if not classify(body(r).get('text'))][:5],
                            'matched-in-kept-file': [example(r) for r in hits if r['evidence/id'] in kept_ids]}
    return {'version': VERSION, 'window': [START, END], 'system-as-of': pin,
            'rules-status': 'reconstructed; original window export script not found',
            'source-rows': rows, 'candidates': candidates, 'already-stamped': already_stamped,
            'comparison': comparison,
            'corpus': {'live-turns': len(turns), 'local-turns': len(raw), 'kept-turns': len(kept),
                       'live-only': len(live_ids - local_ids), 'local-only': len(local_ids - live_ids),
                       'local-only-examples': [r for r in raw if r['id'] not in live_ids][:3]},
            'files': {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in window.iterdir() if p.is_file()}}


def entry_id(candidate):
    return 'origin-backfill:' + digest([VERSION, candidate['source-id']])


def interpretation(candidate):
    return dict(candidate, **{'basis': 'backfill-inferred', 'rules-version': VERSION,
                              'window': [START, END], 'rules-reconstructed': True})


def write(api, saved):
    if saved['version'] != VERSION or saved['window'] != [START, END]:
        raise ValueError('Plan version/window mismatch')
    # Re-read the same system-time cut and verify all planned source records,
    # before writing any interpretation. No live authority is inferred.
    rows = api.rows({'author': 'joe', 'since': START, 'before': END}, saved['system-as-of'])
    by_id = {r['evidence/id']: r for r in rows}
    for c in saved['candidates']:
        r = by_id.get(c['source-id'])
        if not r or digest(r) != c['source-hash'] or not turn(r):
            raise ValueError('Source mismatch: ' + c['source-id'])
        hit = classify(body(r).get('text'))
        expected = dict(hit or {}, **{'source-id': r['evidence/id'], 'source-at': r['evidence/at'], 'source-hash': digest(r)})
        if c != expected or not START <= r['evidence/at'] < END:
            raise ValueError('Candidate mismatch: ' + c['source-id'])
    existing = {r['evidence/id']: r for r in api.rows({'type': 'origin/backfill', 'tags': TAG}, now())}
    written = skipped = 0
    for c in saved['candidates']:
        eid, b = entry_id(c), interpretation(c)
        if eid in existing:
            if body(existing[eid]) != b:
                raise ValueError('Conflicting existing interpretation: ' + eid)
            skipped += 1
            continue
        timestamp = now()
        payload = {'id': eid, 'type': 'origin/backfill', 'claim-type': 'observation',
                   'author': 'origin-backfill', 'at': timestamp, 'body': b,
                   'subject': {'ref/type': 'evidence', 'ref/id': c['source-id']}, 'tags': [TAG],
                   'origin': {'kind': 'harness', 'actor': 'origin-backfill', 'writer': VERSION,
                              'attributed-author': 'origin-backfill', 'authorization': 'unknown',
                              'recorded-at': timestamp, 'basis': 'write-time'}}
        status, receipt = api.request('POST', payload=payload)
        if status not in (200, 201, 409):
            raise ValueError(f'Write refused: {eid}: {receipt}')
        _, stored = api.request('GET', '/' + urllib.parse.quote(eid, safe=''))
        if body(stored) != b or stored.get('evidence/type') != 'origin/backfill':
            raise ValueError('Readback mismatch: ' + eid)
        if status == 409:
            skipped += 1
        else:
            written += 1
        if (written + skipped) % 25 == 0:
            print(json.dumps({'written': written, 'existing': skipped}), flush=True)
    return {'written': written, 'existing': skipped, 'originals-modified': 0}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--dry-run', action='store_true')
    mode.add_argument('--write', action='store_true')
    parser.add_argument('--plan', type=Path, required=True)
    parser.add_argument('--base', default='http://127.0.0.1:7073')
    args = parser.parse_args()
    api = API(args.base)
    if args.dry_run:
        saved = plan(api)
        args.plan.write_text(json.dumps(saved, ensure_ascii=False, indent=2) + '\n')
        print(json.dumps({k: v for k, v in saved.items() if k not in ('source-rows', 'candidates')}, ensure_ascii=False, indent=2))
    else:
        print(json.dumps(write(api, json.loads(args.plan.read_text()))))


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
