#!/usr/bin/env python3
"""Read-only reconstruction of M-象-2000's acceptance case (P0).

Live:   python3 scripts/xiang2000_p0.py --snapshot /tmp/xiang-p0 --check
Replay: python3 scripts/xiang2000_p0.py --from-snapshot /tmp/xiang-p0 --check
Use --expected FILE to check a different EDN fixture. No third-party packages.
The fixture checks the semantic report; capture timestamps/hashes are separate pins.
Snapshots include raw evidence rows, hyperedge responses, git records and the exact
operator files consumed. They are integrity checked, not cryptographically signed.
Evidence reads use P6 system-as-of and retain raw snapshot replay; evidence/at
is event time, NOT an XTDB system timestamp.
"""
import argparse
import datetime as dt
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
import urllib.parse
import urllib.request

REPO = Path(__file__).resolve().parents[1]
ROOT = REPO.parent
BASE = 'http://localhost:7073/api/alpha/'
EXPECTED = REPO / 'holes/labs/M-象-2000/p0-expected.edn'
COMMITS = ('5146606d', '2ef7a010', '626df9fa', '80428193')
TURNS = ('claude-10-turn-30', 'claude-11-turn-2', 'claude-11-turn-3', 'claude-11-turn-6')
WINDOW = 'storage/operator-turns/window-0922/operator-turns-joe.jsonl'
NOTICE_START = '2026-09-24T19:04:00Z'
NOTICE_END = '2026-09-25T20:00:00Z'  # Includes the entire displayed 19:59 minute.
NOTICE_SOURCE = 'chat-turns author=joe; harness write-time origin or origin/backfill; rule=kimi-notice'


def require(ok, message):
    if not ok:
        raise ValueError(message)


def sha(data):
    return hashlib.sha256(data).hexdigest()


def js(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True)


def now():
    return dt.datetime.now(dt.timezone.utc).isoformat()


def edn(text):
    """Strict data-only EDN subset: maps, vectors, strings, keywords, numbers, booleans.

    Used for the fixture and EDN-encoded evidence bodies. Unsupported forms fail;
    nothing is evaluated. Keywords normalize to strings, as in the JSON API.
    """
    tokens = re.findall(r';[^\n]*|"(?:\\.|[^"\\])*"|[{}\[\]]|[^\s,{}\[\]]+', text)
    tokens = [t for t in tokens if not t.startswith(';')]
    pos = 0

    def read():
        nonlocal pos
        require(pos < len(tokens), 'incomplete EDN')
        t = tokens[pos]
        pos += 1
        if t in ('{', '['):
            close = '}' if t == '{' else ']'
            values = []
            while pos < len(tokens) and tokens[pos] != close:
                values.append(read())
            require(pos < len(tokens), 'unclosed EDN collection')
            pos += 1
            if t == '[':
                return values
            require(len(values) % 2 == 0, 'odd EDN map')
            pairs = list(zip(values[::2], values[1::2]))
            result = dict(pairs)
            require(len(result) == len(pairs), 'duplicate EDN map key')
            return result
        if t.startswith('"'):
            return json.loads(t)
        if t.startswith(':'):
            return t[1:]
        if t in ('true', 'false', 'nil'):
            return {'true': True, 'false': False, 'nil': None}[t]
        require(re.fullmatch(r'-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?', t),
                f'unsupported EDN token: {t}')
        return float(t) if any(c in t for c in '.eE') else int(t)

    result = read()
    require(pos == len(tokens), 'trailing EDN data')
    return result


def body(row):
    value = row.get('evidence/body', {})
    return edn(value) if isinstance(value, str) else value


def one(rows, description):
    rows = list(rows)
    require(len(rows) == 1, f'{description}: expected one record, found {len(rows)}')
    return rows[0]


def eid(row):
    return row['evidence/id']


def at(row):
    return row['evidence/at']


def capture():
    files = {}
    pin = now().replace('+00:00', 'Z')
    pins = {'schema': 1, 'started-at': pin, 'evidence-system-as-of': pin,
            'pin-method': 'snapshot with P6 system-as-of evidence reads', 'reads': [],
            'operator-files': {}}

    def get(route, params):
        url = BASE + route + '?' + urllib.parse.urlencode(params)
        started = now()
        with urllib.request.urlopen(urllib.request.Request(
                url, headers={'Accept': 'application/json'}), timeout=60) as response:
            result = json.load(response)
        require(not result.get('error') and result.get('ok') is not False,
                f'store refused {url}: {result}')
        pins['reads'].append({'url': url, 'started-at': started, 'finished-at': now()})
        return result

    def scan(params):
        params = dict(params, **{'limit': 1000, 'system-as-of': pin})
        result, cursors = [], set()
        while True:
            page = get('evidence', params)
            require(isinstance(page.get('entries'), list), 'evidence response lacks entries')
            result.extend(page['entries'])
            cursor = page.get('next-cursor')
            if not cursor:
                require(not page.get('incomplete'), 'incomplete evidence scan')
                return result
            key = (cursor['at'], cursor['id'])
            require(key not in cursors, 'evidence pagination repeated cursor')
            cursors.add(key)
            params.update({'cursor-at': key[0], 'cursor-id': key[1]})

    files[WINDOW] = (ROOT / WINDOW).read_bytes()
    operators = [json.loads(line) for line in files[WINDOW].splitlines()]
    rows = []
    for seat, since, before in (
            ('claude-10', '2026-09-24T15:40:00Z', '2026-09-24T16:00:00Z'),
            ('claude-11', '2026-09-24T15:40:00Z', '2026-09-24T17:00:00Z'),
            ('claude-14', '2026-09-25T19:00:00Z', '2026-09-25T21:00:00Z')):
        session = one({r['session'] for r in operators
                       if (r['turn_id'] or '').startswith(seat + '-turn-')
                       and since <= r['at'] < before}, seat + ' session')
        rows.extend(scan({'session-id': session, 'since': since, 'before': before}))
    require(len({eid(r) for r in rows}) == len(rows), 'duplicate evidence ids across pages')
    files['evidence.jsonl'] = ''.join(js(r) + '\n' for r in rows).encode()

    for name, params in (
            ('notice-turns.jsonl', {'author': 'joe', 'since': NOTICE_START, 'before': NOTICE_END}),
            ('origin-backfills.jsonl', {'type': 'origin/backfill'})):
        files[name] = ''.join(js(r) + '\n' for r in scan(params)).encode()

    def git(*args):
        return subprocess.check_output(['git', '-C', str(REPO), *args], text=True).strip()

    head = git('rev-parse', 'HEAD')
    records = {}
    for prefix in COMMITS:
        full = git('rev-parse', prefix + '^{commit}')
        subprocess.run(['git', '-C', str(REPO), 'merge-base', '--is-ancestor', full, head],
                       check=True, capture_output=True)
        fields = git('show', '-s', '--format=%H%n%aI%n%cI%n%an%n%s', full).splitlines()
        records[prefix] = dict(zip(('sha', 'author-at', 'commit-at', 'author', 'subject'), fields))
    files['git.json'] = js({'head': head, 'commits': records}).encode()
    pins['git-sha'] = head
    edges = {}
    for hour in ('16', '17'):
        time = f'2026-09-24T{hour}:00:00Z'
        edges[time] = get('hyperedges', {'type': 'code/v05/commit',
                                        'end': records['5146606d']['sha'],
                                        'valid-as-of': time, 'limit': 100})
    files['hyperedges.json'] = js(edges).encode()
    sources = [ROOT / WINDOW]
    for turn in TURNS:
        sources.append(one((ROOT / 'storage/operator-turns/batches').glob(
            f'2026-09-22_2026-09-26_block*/{turn}.json.analysis.json'), turn + ' analysis'))
    for path in sources:
        name = path.relative_to(ROOT).as_posix()
        data = files[name] if name in files else path.read_bytes()
        files[name] = data
        pins['operator-files'][name] = sha(data)
    pins['finished-at'] = now()
    files['pins.json'] = js(pins).encode()
    return files


def write_snapshot(directory, files):
    require(not directory.exists(), f'snapshot directory already exists: {directory}')
    directory.mkdir(parents=True)
    for name, data in files.items():
        path = directory / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
    manifest = {'schema': 1, 'files': {name: sha(data) for name, data in files.items()}}
    (directory / 'manifest.json').write_text(js(manifest) + '\n')


def read_snapshot(directory):
    manifest = json.loads((directory / 'manifest.json').read_text())
    require(manifest.get('schema') == 1, 'unsupported snapshot manifest schema')
    files = {}
    for name, digest in manifest['files'].items():
        p = Path(name)
        require(not p.is_absolute() and '..' not in p.parts, f'unsafe snapshot file: {name}')
        path = directory / p
        require(path.resolve().is_relative_to(directory.resolve()), f'unsafe snapshot file: {name}')
        require(path.is_file(), f'snapshot file missing: {name}')
        data = path.read_bytes()
        require(sha(data) == digest, f'snapshot hash mismatch: {name}')
        files[name] = data
    actual = {p.relative_to(directory).as_posix() for p in directory.rglob('*') if p.is_file()}
    require(actual == set(files) | {'manifest.json'}, 'snapshot contains unmanifested files')
    required = {'pins.json', 'git.json', 'hyperedges.json', 'evidence.jsonl',
                'notice-turns.jsonl', 'origin-backfills.jsonl', WINDOW}
    require(required <= files.keys(),
            'snapshot manifest missing required files')
    return files


def notice_count(files):
    """Count distinct source turns, never interpretation records. Known write-time
    origins take precedence; unknown/absent stamps may use inferred backfills.
    The kimi-notice rule is the reviewed reconstructed P6o-3 producer template.
    """
    from xiang2000_p6o3 import classify
    backfills = [json.loads(line) for line in files['origin-backfills.jsonl'].splitlines()]
    inferred = {body(r).get('source-id') for r in backfills
                if r.get('evidence/type') == 'origin/backfill'
                and body(r).get('basis') == 'backfill-inferred'
                and body(r).get('rule') == 'kimi-notice'
                and body(r).get('origin') == 'harness'}
    ids = set()
    for r in (json.loads(line) for line in files['notice-turns.jsonl'].splitlines()):
        b = body(r)
        if (r.get('evidence/author') != 'joe' or b.get('event') != 'chat-turn'
                or b.get('role') != 'user' or not NOTICE_START <= at(r) < NOTICE_END
                or (classify(b.get('text')) or {}).get('rule') != 'kimi-notice'):
            continue
        kind = (r.get('evidence/origin') or {}).get('kind')
        if kind == 'harness' or (kind in (None, 'unknown') and eid(r) in inferred):
            ids.add(eid(r))
    return len(ids)


def reconstruct(files):
    pins = json.loads(files['pins.json'])
    for name, digest in pins['operator-files'].items():
        require(name in files and sha(files[name]) == digest, f'operator file hash mismatch: {name}')
    git = json.loads(files['git.json'])
    require(git['head'] == pins['git-sha'], 'git pin mismatch')
    commits = git['commits']
    evidence = [json.loads(line) for line in files['evidence.jsonl'].splitlines()]
    by_id = {eid(r): r for r in evidence}
    require(len(by_id) == len(evidence), 'duplicate snapshot evidence ids')
    # Parse only the relevant bodies; raw API values remain untouched in the snapshot.
    bodies = {eid(r): body(r) for r in evidence}
    operators = [json.loads(line) for line in files[WINDOW].splitlines()]
    acts = []
    analyses = []
    intents = ('report-problem', 'constrain', 'propose', 'propose')
    needles = ('5 hour usage limit', 'mission, excursion, or ticket',
               'enforcement rule', 'one-line requisition')
    for turn, intent, needle in zip(TURNS, intents, needles):
        op = one((r for r in operators if r['turn_id'] == turn and needle in r['text']), turn)
        row = by_id.get(op['id'])
        require(row is not None, f'{turn}: missing evidence {op["id"]}')
        b = bodies[eid(row)]
        require(row['evidence/author'] == 'joe' and b.get('role') == 'user'
                and b.get('text') == op['text'] and b.get('turn-id') == turn
                and at(row) == op['at'] and row['evidence/session-id'] == op['session'],
                f'{turn}: operator/evidence join mismatch')
        name = one((n for n in files if n.endswith('/' + turn + '.json.analysis.json')), turn)
        analysis = json.loads(files[name])
        require(analysis['source_text'] == op['text']
                and analysis['source_sha256'] == sha(op['text'].encode()), f'{turn}: stale analysis')
        fragment = one((f for s in analysis['sentences'] for f in s['fragments']
                        if f['intent'] == intent and needle in f['text']), turn + ' intent')
        require(fragment['text'] in op['text'], f'{turn}: fragment not in source')
        acts.append(row)
        analyses.append(name)

    def commit_record(prefix, seat):
        c = commits[prefix]
        matches = []
        for r in evidence:
            b = bodies[eid(r)]
            if r['evidence/author'] != seat or b.get('event') != 'turn-commits':
                continue
            found = [x for x in b['commits'] if x['repo'] == 'futon3c'
                     and x['sha'].strip() == c['sha']]
            if found:
                require(len(found) == 1 and found[0]['subject'] == c['subject'],
                        f'{prefix}: git/turn-commits mismatch')
                assistant = by_id.get(r.get('evidence/in-reply-to'))
                require(assistant is not None, f'{prefix}: missing assistant reply')
                ab = bodies[eid(assistant)]
                require(assistant['evidence/author'] == seat and ab.get('role') == 'assistant'
                        and prefix in ab.get('text', '')
                        and assistant['evidence/session-id'] == r['evidence/session-id'],
                        f'{prefix}: authoring seat not corroborated by assistant report')
                matches.append(r)
        return one(matches, prefix + ' authoring-seat commit')

    landed = commit_record('5146606d', 'claude-11')
    removed = [commit_record(p, 'claude-14') for p in COMMITS[1:3]]
    early = commit_record('80428193', 'claude-11')
    session = landed['evidence/session-id']
    for act in acts[1:]:
        require(act['evidence/session-id'] == session and at(act) < at(landed),
                'derivation: proposal outside authoring session or after commit')
    # Pair by temporal order in a seat/session, never by equal user/assistant turn ids.
    pairs = []
    for act in acts[1:]:
        candidates = sorted((r for r in evidence if r['evidence/author'] == 'claude-11'
                             and r.get('evidence/session-id') == session and at(r) > at(act)
                             and bodies[eid(r)].get('role') == 'assistant'), key=at)
        require(candidates, f'{eid(act)}: no following assistant turn')
        reply = candidates[0]
        require(at(reply) <= at(landed), 'derivation: reply is after target commit')
        pairs.append(f'{bodies[eid(act)]["turn-id"]} -> {bodies[eid(reply)]["turn-id"]}')
    bridge = one((r for r in evidence if r['evidence/author'] == 'claude-11'
                  and r.get('evidence/session-id') == session
                  and at(acts[0]) < at(r) < at(acts[1])
                  and bodies[eid(r)].get('event') == 'invoke-start'
                  and 'From: claude-10\nTo: claude-11' in bodies[eid(r)].get('prompt-preview', '')),
                 '15:48 delegation into claude-11 session')
    retrieval = one((r for r in evidence if r['evidence/author'] == 'claude-11'
                     and r.get('evidence/session-id') == session
                     and bodies[eid(r)].get('event') == 'context-retrieval'
                     and bodies[eid(r)].get('query', '') == bodies[eid(acts[2])]['text'][:100]),
                    '16:20 retrieval')
    hit = one((h for h in bodies[eid(retrieval)]['results']
               if h['id'] == 'inbox-zero/gate-fails-loudly'), 'gate-fails-loudly hit')
    require(hit['rank'] == 3, '16:20 retrieval: expected rank 3')
    edges = json.loads(files['hyperedges.json'])
    present = {}
    for time, response in edges.items():
        require('hyperedges' in response and not response.get('next-cursor'),
                f'{time}: incomplete hyperedge response')
        hits = [h for h in response['hyperedges'] if commits['5146606d']['sha'] in h['hx/endpoints']]
        require(len(hits) == len(response['hyperedges']) and len(hits) <= 1,
                f'{time}: unexpected hyperedge result')
        present[time] = bool(hits)
    require(present == {'2026-09-24T16:00:00Z': False, '2026-09-24T17:00:00Z': True},
            'as-of commit existence differs from acceptance case')

    def row(when, act, who, source, status='QUERY'):
        return dict(when=when, act=act, who=who, source=source, status=status)

    labels = ('Kimi 5-hour limit on a light morning', 'a Kimi job must name its mission',
              'an enforcement rule like the inbox-zero followup', 'a one-line requisition naming the mission')
    table = [row(at(r)[5:10] + ' ' + at(r)[11:16], intent + ': ' + label,
                 'Joe → claude-11' if i == 0 else 'Joe', eid(r) + '; ' + analyses[i])
             for i, (r, intent, label) in enumerate(zip(acts, intents, labels))]
    table += [
        row('09-24 16:34', 'commit ' + commits['5146606d']['sha'][:8] + ': each Kimi call carries a requisition',
            'claude-11', eid(landed) + '; git ' + commits['5146606d']['sha']),
        row('09-24 19:04 → 09-25 19:59', f"{notice_count(files)} notices delivered as turns under Joe's name",
            'harness', NOTICE_SOURCE),
        row('09-25', 'followups removed (' + ', '.join(commits[p]['sha'][:8] for p in COMMITS[1:3]) + ')',
            'Joe, claude-14', '; '.join(eid(r) for r in removed) + '; git 2ef7a010, 626df9fa'),
        row('as of 09-24 16:00', 'requisition rule absent', 'query',
            'hyperedges code/v05/commit 5146606d: absent at valid-as-of 2026-09-24T16:00:00Z'),
        row('as of 09-24 17:00', "v1 said 'present'; v2 says 'committed, not yet live'", 'query',
            'hand-filled IDENTIFY; adopted/committed/live rule record pending P13a/P13b', 'STUB'),
        row('as of 09-25 21:00', 'followup half withdrawn', 'query',
            'hand-filled IDENTIFY; rule-version intervals pending P13a/P13b', 'STUB'),
        row('derivation', 'from 5146606d back to the four operator acts', 'query',
            eid(landed) + ' -> session ' + session + ' -> ' + ', '.join(eid(r) for r in acts)),
        row('clearance', '15:48 incident explained by 15:54 (d5e3147e); the rule was put up after that as its measure; 42 notices need compensation',
            'query', 'hand-filled IDENTIFY; incident/measure/compensation records pending P13a/P14', 'STUB')]
    details = [
        '15:48 route: Joe -> claude-10; explicit delegation to claude-11; bell ' + eid(bridge),
        'Ordered user/assistant pairs: ' + '; '.join(pairs),
        'Commit join: filter turn-commits by authoring seat, exact git SHA and repo; corroborate with linked assistant report. Other listed commits are excluded.',
        'Derivation is a bounded session/order reconstruction, not a stored causal edge.',
        '16:20 retrieval: ' + eid(retrieval) + '; event-at=' + at(retrieval)
        + '; rank=' + str(hit['rank']) + ' ' + hit['id'],
        '80428193 git commit-at=' + commits['80428193']['commit-at'] + '; turn-commits event-at=' + at(early),
        'STUB retrieval system-time ordering: unavailable until P6; the hand-filled six-second claim compares retrieval with invoke completion, not git commit time.',
        'Commit existence query: absent at 2026-09-24T16:00:00Z; present at 2026-09-24T17:00:00Z. Commit existence does not establish runtime rule validity.',
        'STUB rule as-of 17:00 and 09-25 21:00; see rows above.']
    return {'rows': table, 'details': details}, pins


def check(report, expected):
    fixture = edn(expected.read_text())
    require(set(fixture) == set(report), 'fixture/report fields differ')
    want, got = fixture['rows'], report['rows']
    require(len(want) == len(got), f'row count mismatch: expected {len(want)}, got {len(got)}')
    for i, (a, b) in enumerate(zip(want, got), 1):
        require(a == b, f'row {i} ({b["when"]}) mismatch: expected {js(a)}; got {js(b)}')
    require(fixture['details'] == report['details'], 'details mismatch')


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    modes = parser.add_mutually_exclusive_group()
    modes.add_argument('--snapshot', type=Path)
    modes.add_argument('--from-snapshot', type=Path)
    parser.add_argument('--check', action='store_true')
    parser.add_argument('--expected', type=Path, default=EXPECTED)
    args = parser.parse_args()
    files = read_snapshot(args.from_snapshot) if args.from_snapshot else capture()
    report, pins = reconstruct(files)
    if args.check:
        check(report, args.expected)
    if args.snapshot:
        write_snapshot(args.snapshot, files)
    # Nothing is printed until all integrity checks, joins and fixture checks pass.
    print('pins: ' + js(pins))
    for r in report['rows']:
        print(' | '.join(r[k] for k in ('status', 'when', 'act', 'who', 'source')))
    for detail in report['details']:
        print(detail)
    print(f'stubs: {sum(r["status"] == "STUB" for r in report["rows"])} of {len(report["rows"])}')
    if args.check:
        print('check: PASS')


if __name__ == '__main__':
    try:
        main()
    except (ValueError, KeyError, OSError, subprocess.CalledProcessError) as error:
        print(f'ERROR: {error}', file=sys.stderr)
        sys.exit(1)
