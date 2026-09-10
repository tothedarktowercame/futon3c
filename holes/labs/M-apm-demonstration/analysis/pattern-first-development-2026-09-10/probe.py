#!/usr/bin/env python3
"""Offline development census and retrieval; never contacts a service.

freeze reads committed Git objects on the explicit watcher source surface.
replay reads only frozen.json, verifies hashes and recomputes result.json.
No memory attachment is inferred or admitted by this exploratory index.
"""
import argparse
import collections
import hashlib
import json
from pathlib import Path
import re
import sqlite3
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = Path('/home/joe/code')
REPOS = ['futon0', 'futon1', 'futon1a', 'futon2', 'futon3', 'futon3a',
         'futon3b', 'futon3c', 'futon4', 'futon5', 'futon5a', 'futon6',
         'futon7', 'futon7a']
DECL = re.compile(r'^@(?:flexiarg|arg|multiarg)\s+(\S+)', re.M)


def git(repo, *args):
    return subprocess.check_output(['git', '-C', str(ROOT / repo), *args])


def digest(data):
    return hashlib.sha256(data).hexdigest()


def relevant(pid):
    return pid.startswith(('math/', 'math-', 'proof-search/'))


def field(text, name):
    m = re.search(r'^@' + re.escape(name) + r'\s+(.+)$', text, re.M)
    return m.group(1) if m else ''


def spans(text):
    matches = list(re.finditer(r'^[ \t]*\+[ \t]+(IF|HOWEVER|THEN|BECAUSE|context|NEXT-STEPS):', text, re.M))
    return {m.group(1): {'line': text[:m.start()].count('\n') + 1,
                        'text': text[m.end():matches[i + 1].start() if i + 1 < len(matches) else len(text)].strip()}
            for i, m in enumerate(matches)}


def freeze():
    pins, files, declarations, drafts = {}, [], [], []

    def read(repo, path):
        raw = git(repo, 'show', pins[repo] + ':' + path)
        item = {'repo': repo, 'path': path, 'sha256': digest(raw),
                'blob': git(repo, 'rev-parse', pins[repo] + ':' + path).decode().strip(),
                'text': raw.decode()}
        files.append(item)
        return item

    for repo in REPOS + ['apm-lean', 'futon1b']:
        if not (ROOT / repo / '.git').exists():
            continue
        revision = '0949acf2' if repo == 'futon3c' else 'HEAD'
        pins[repo] = git(repo, 'rev-parse', revision).decode().strip()
        if repo not in REPOS:
            continue
        paths = git(repo, 'ls-tree', '-r', '--name-only', pins[repo]).decode().splitlines()
        for path in paths:
            if path.endswith(('.flexiarg', '.multiarg')):
                raw = git(repo, 'show', pins[repo] + ':' + path)
                ids = [p for p in DECL.findall(raw.decode()) if relevant(p)]
                if ids:
                    item = read(repo, path)
                    for pid in ids:
                        declarations.append({'id': pid, 'repo': repo, 'path': path,
                                             'sha256': item['sha256']})
            elif repo == 'futon3c' and re.fullmatch(r'holes/labs/M-apm-demonstration/pattern-library-[^/]+\.md', path):
                item = read(repo, path)
                ids = re.findall(r'^##\s+((?:math[^ /]*|proof-search)/\S+)', item['text'], re.M)
                drafts.append({'path': path, 'ids': ids, 'status': 'committed-coined-deposit-not-review'})

    for path in ['resources/sigils/patterns-index.tsv', 'library/MANIFEST-math-split-RULED.md',
                 'library/MANIFEST-math-split-proposal.md']:
        read('futon3', path)
    for path in git('futon3', 'ls-tree', '-r', '--name-only', pins['futon3'], 'library').decode().splitlines():
        if re.match(r'library/math[^/]*/(?:attestations|proposals)\.edn$', path):
            read('futon3', path)
    sources = ['src/futon3c/peripheral/memory_recall.clj', 'src/futon3c/apm/conductor.clj',
               'src/futon3c/apm/role_memory_search.clj', 'src/futon3c/apm/typed_role_submission.clj',
               'src/futon3c/apm/live_learning_phases.clj', 'src/futon3c/apm/memory_caption_store.clj',
               'src/futon3c/apm/coined_pattern.clj', 'src/futon3c/transport/http.clj',
               'scripts/wire_math_memory_patterns.clj', 'scripts/pattern_store_census.py',
               'scripts/apm-search-memory.py',
               'holes/labs/M-apm-demonstration/role-cards/zai-student-v2.md']
    # Code is pinned by hash/blob; avoid duplicating large transport source in the packet.
    code = []
    for path in sources:
        item = read('futon3c', path)
        files.pop()
        code.append({k: v for k, v in item.items() if k != 'text'})
    item = read('futon1b', 'futon1b_text.clj')
    files.pop()
    code.append({k: v for k, v in item.items() if k != 'text'})
    for problem in ['m00A05', 'm96J04', 'a93A03', 'm02A01', 'a93A01', 'm99A04']:
        for leaf in ['problem.md', 'lean/Main.lean']:
            read('apm-lean', f'problems/{problem}/{leaf}')
    for path in ['holes/labs/M-apm-demonstration/retro-promotion-receipts.edn',
                 'holes/labs/M-apm-demonstration/retro-promotion-manifest.edn',
                 'holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/retrieval.json',
                 'holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/memory.json',
                 'holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/review.json',
                 'holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/basis.json',
                 'holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/lean-check.txt',
                 'holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/EndpointTransfer.lean']:
        read('futon3c', path)
    snapshots, memory_rows = [], {}
    prefix = 'data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f211/snapshots'
    for name in ['f211-solver-memory.edn', 'f211-student-mined-memory.edn',
                 'f211-guide-1-memory.edn', 'f211-guide-2-memory.edn']:
        path = prefix + '/' + name
        raw = (ROOT / 'futon3c' / path).read_bytes()
        selected = json.loads(subprocess.check_output(
            ['bb', str(HERE / 'sanitize_snapshot.bb')], input=raw))
        keys = []
        for memory in selected.pop('memories'):
            key = digest(json.dumps(memory, sort_keys=True).encode())
            memory_rows[key] = memory
            keys.append(key)
        selected['memory_keys'] = keys
        assert (ROOT / 'futon3c' / path).read_bytes() == raw, path
        snapshots.append({'path': path, 'source_kind': 'retained untracked snapshot; exact bytes hashed',
                          'source_sha256': digest(raw), 'selected': selected})
    patterns = []
    tsv = next(f['text'] for f in files if f['path'].endswith('patterns-index.tsv'))
    catalog = []
    for line in tsv.splitlines():
        if not line.strip() or line.startswith('#'):
            continue
        cols = (line.split('\t') + [''] * 5)[:5]
        catalog.append({'id': cols[0], 'rationale': cols[3], 'hotwords': cols[4]})
    for item in files:
        if item['repo'] != 'futon3' or not re.match(r'library/(math[^/]*|proof-search)/[^/]+\.flexiarg$', item['path']):
            continue
        for pid in DECL.findall(item['text']):
            patterns.append({'id': pid, 'path': item['path'], 'sha256': item['sha256'],
                             'fields': {n: field(item['text'], n) for n in
                                        ['title', 'keywords', 'why', 'see-also', 'how', 'provenance']},
                             'spans': spans(item['text']), 'text': item['text']})
    # These are analyst-authored development queries, not held-out evaluation.
    queries = [
        {'id': 'ode-source', 'problem': 'm00A05', 'query': 'differential equation initial value unique solution',
         'target': 'math-formalization-CA/ode-gronwall-api', 'applicability': 'partial-method; endpoint bridge needed'},
        {'id': 'operator-statement', 'problem': 'm96J04', 'query': 'compact linear operator Banach space no eigenvalues',
         'target': 'math-formalization-CA/ode-gronwall-api', 'applicability': 'requires Volterra reduction; not in statement'},
        {'id': 'operator-residual', 'problem': 'm96J04', 'query': 'ODE uniqueness endpoint',
         'target': 'math-formalization-CA/ode-gronwall-api', 'applicability': 'analyst residual query reused from earlier development probe'},
        {'id': 'weak-positive', 'problem': 'a93A03', 'query': 'Hilbert weak convergence norm convergence',
         'target': 'math-formalization-FA/weak-convergence-hilbert', 'applicability': 'established premises for part a'},
        {'id': 'weak-negative', 'problem': 'm02A01', 'query': 'weak convergence does not imply strong convergence counterexample',
         'target': 'math-formalization-FA/weak-convergence-hilbert', 'applicability': 'reject forward upgrade; norm-limit premise absent in unit-vector example'},
        {'id': 'continuity-negative', 'problem': 'm99A04', 'query': 'complete metric spaces dense uniformly continuous extension',
         'target': 'math-formalization-CA/uniform-continuity-boundedness', 'applicability': 'total boundedness not assumed; general bounded-image route invalid'},
    ]
    history = {family: git('futon3', 'log', '-6', '--format=%H %s', pins['futon3'], '--',
                          'library/' + family).decode().splitlines()
               for family in sorted({p['id'].split('/')[0] for p in patterns})}
    untracked = git('futon3c', 'ls-files', '--others', '--exclude-standard',
                    'holes/labs/M-apm-demonstration/pattern-library-*.md').decode().splitlines()
    return {'status': 'offline analyst-selected development; no graph admission or Student execution',
            'pins': pins, 'files': files, 'code': code, 'declarations': declarations,
            'drafts': drafts, 'untracked_deposit_paths_only': untracked, 'history': history,
            'patterns': patterns, 'catalog': catalog, 'queries': queries,
            'snapshots': snapshots, 'memory_rows': memory_rows}


def run(data):
    for item in data['files']:
        assert digest(item['text'].encode()) == item['sha256'], item['path']
    patterns = data['patterns']
    by_id = {p['id']: p for p in patterns}
    assert len(by_id) == len(patterns)
    for p in patterns:
        assert digest(p['text'].encode()) == p['sha256']
        assert any(f['path'] == p['path'] and f['sha256'] == p['sha256'] for f in data['files'])
    db = sqlite3.connect(':memory:')
    db.execute("CREATE VIRTUAL TABLE docs USING fts5(id UNINDEXED, heading, conditions, body, tokenize='unicode61')")
    for p in patterns:
        db.execute('INSERT INTO docs VALUES (?,?,?,?)',
                   (p['id'], p['fields']['title'] + ' ' + p['fields']['keywords'],
                    ' '.join(p['spans'].get(s, {}).get('text', '') for s in ['context', 'IF', 'HOWEVER', 'THEN']), p['text']))

    def quoted(tokens):
        return [f'"{t.replace(chr(34), chr(34) * 2)}"' for t in tokens]

    def search(expr, weights):
        return [{'id': pid, 'score': score, 'path': by_id[pid]['path'], 'sha256': by_id[pid]['sha256']}
                for pid, score in db.execute(
                    f'SELECT id, bm25(docs,{weights}) AS score FROM docs WHERE docs MATCH ? ORDER BY score, id LIMIT 10', (expr,))]

    results = []
    for q in data['queries']:
        # Exact keyword-count algorithm of http/search-patterns, on the frozen
        # corpus subset. Stable ties preserve the original TSV order.
        terms = q['query'].lower().split()
        catalog_hits = []
        for c in data['catalog']:
            if c['id'] not in by_id:
                continue
            text = (c['id'] + ' ' + c['rationale'] + ' ' + c['hotwords']).lower()
            hits = sum(t in text for t in terms)
            if hits:
                catalog_hits.append({'id': c['id'], 'score': hits,
                                     'path': by_id[c['id']]['path'], 'sha256': by_id[c['id']]['sha256']})
        catalog_hits.sort(key=lambda x: -x['score'])
        # Mechanism-only baseline: production tokenizer, default AND and its
        # four-token OR fallback. No reviewed-projection or evidence-store claim.
        lexical = search(' '.join(quoted(terms)), '0,0,0,1')
        strategy = 'AND'
        if not lexical:
            stop = set('a an and before for in of on the to using with'.split())
            fallback = list(dict.fromkeys(t for t in re.findall(r'[a-z0-9_/-]+', q['query'].lower()) if len(t) >= 4 and t not in stop))[:4]
            lexical = search(' OR '.join(quoted(fallback)), '0,0,0,1') if fallback else []
            strategy = 'four-token-OR'
        # Exploratory ranking: all query tokens OR, BM25 weights heading=2,
        # conditions=3, full body=1. Scores are lexical, not applicability.
        ranked = search(' OR '.join(quoted(terms)), '0,2,3,1')
        arms = {'catalog_keyword_count': catalog_hits[:10],
                'fts_component': lexical, 'pattern_fields_exploratory': ranked}
        results.append({**q, 'fts_strategy': strategy, 'arms': arms,
                        'target_rank': {arm: next((i + 1 for i, row in enumerate(rows) if row['id'] == q['target']), None)
                                        for arm, rows in arms.items()}})
    families = collections.Counter(p['id'].split('/')[0] for p in patterns)
    catalog_ids = {c['id'] for c in data['catalog']}
    for key, memory in data['memory_rows'].items():
        assert key == digest(json.dumps(memory, sort_keys=True).encode())
    memories = [data['memory_rows'][key] for s in data['snapshots'] for key in s['selected']['memory_keys']]
    attachments = sorted({(m['memory-id'], pid, m.get('attachment-status', 'unknown'))
                          for m in memories for pid in m.get('pattern-ids', [])})
    attached_ids = {pid for _, pid, _ in attachments}
    versions = collections.defaultdict(set)
    for m in memories:
        versions[m['memory-id']].add(json.dumps(m, sort_keys=True))
    return {'sqlite_version': sqlite3.sqlite_version, 'scope': data['status'],
            'retained_snapshot_counts': [len(s['selected']['memory_keys']) for s in data['snapshots']],
            'retained_distinct_memories': len({m['memory-id'] for m in memories}),
            'retained_distinct_pattern_ids': len(attached_ids),
            'retained_pattern_ids_in_canonical_library': len(attached_ids & set(by_id)),
            'retained_pattern_ids_missing_from_canonical_library': sorted(attached_ids - set(by_id)),
            'retained_memories_with_differing_selected_rows': sorted(mid for mid, rows in versions.items() if len(rows) > 1),
            'retained_attachment_status_counts': dict(collections.Counter(status for _, _, status in attachments)),
            'retained_attachments': [{'memory_id': mid, 'pattern_id': pid, 'status': status}
                                     for mid, pid, status in attachments],
            'canonical_pattern_count': len(patterns), 'family_counts': dict(sorted(families.items())),
            'catalog_coverage': len(set(by_id) & catalog_ids),
            'catalog_missing': sorted(set(by_id) - catalog_ids),
            'declaration_count_watcher_committed': len(data['declarations']),
            'distinct_ids_watcher_committed': len({d['id'] for d in data['declarations']}),
            'declarations_by_repo': dict(sorted(collections.Counter(d['repo'] for d in data['declarations']).items())),
            'committed_coined_deposits': len(data['drafts']),
            'coined_id_occurrences': sum(len(d['ids']) for d in data['drafts']),
            'coined_distinct_ids': len({pid for d in data['drafts'] for pid in d['ids']}),
            'untracked_deposit_path_count': len(data['untracked_deposit_paths_only']),
            'full_pattern_offer_eligible': sum(not re.search(r'-[A-Z]{2,}$', p['id'].split('/')[0]) for p in patterns),
            'explicit_section_counts': {s: sum(s in p['spans'] for p in patterns) for s in ['IF', 'HOWEVER', 'THEN']},
            'results': results}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('mode', choices=['freeze', 'replay'])
    parser.add_argument('--output', type=Path, default=HERE / 'result.json')
    args = parser.parse_args()
    if args.mode == 'freeze':
        data = freeze()
        (HERE / 'frozen.json').write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n')
    else:
        data = json.loads((HERE / 'frozen.json').read_text())
    result = run(data)
    args.output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n')
    print(json.dumps({k: v for k, v in result.items() if k not in ['results', 'retained_attachments']}, indent=2))
    for q in result['results']:
        print(q['id'], q['target_rank'])


if __name__ == '__main__':
    main()
