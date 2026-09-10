#!/usr/bin/env python3
"""Read pinned Git sources; census all topology bundles and import dependencies.

No Lake, services, live worklists or holdout material. Imports are dependency
evidence, not declarations-used evidence. Zero placeholders is a source
observation, not a fresh compilation or mathematical correctness verdict.
"""
import collections
import hashlib
import json
from pathlib import Path
import re
import subprocess

ROOT = Path('/home/joe/code/apm-lean')
HERE = Path(__file__).resolve().parent
PIN = 'f053ab5936725f2e41c937cfff82277f3a23c868'


def git(*args):
    return subprocess.check_output(['git', '-C', str(ROOT), *args])


def code_only(text):
    """Blank nested block comments, line comments and strings; preserve lines."""
    out, i, depth, string = [], 0, 0, False
    while i < len(text):
        two = text[i:i + 2]
        char = text[i]
        if depth:
            if two == '/-':
                depth += 1
                out.append('  ')
                i += 2
            elif two == '-/':
                depth -= 1
                out.append('  ')
                i += 2
            else:
                out.append('\n' if char == '\n' else ' ')
                i += 1
        elif string:
            if char == '\\' and i + 1 < len(text):
                out.append('  ')
                i += 2
            else:
                if char == '"':
                    string = False
                out.append('\n' if char == '\n' else ' ')
                i += 1
        elif two == '/-':
            depth = 1
            out.append('  ')
            i += 2
        elif two == '--':
            end = text.find('\n', i)
            end = len(text) if end < 0 else end
            out.append(' ' * (end - i))
            i = end
        elif char == '"':
            string = True
            out.append(' ')
            i += 1
        else:
            out.append(char)
            i += 1
    assert not depth and not string, 'Unterminated comment/string in input'
    return ''.join(out)


def scanner_checks():
    text = '/- sorry /- admit -/ sorry -/\ntheorem x := by sorry -- admit\n#check "sorry \\" admit"\n'
    cleaned = code_only(text)
    assert re.findall(r'\b(?:sorry|admit)\b', cleaned) == ['sorry']
    assert len(text) == len(cleaned) and text.count('\n') == cleaned.count('\n')
    assert re.findall(r'\b(?:sorry|admit)\b', code_only('by\n  admit')) == ['admit']


def main():
    scanner_checks()
    paths = git('ls-tree', '-r', '--name-only', PIN).decode().splitlines()
    lean_paths = {p for p in paths if p.endswith('.lean')}
    modules = {p[:-5].replace('/', '.'): p for p in lean_paths}
    records, sources = {}, {}

    def read(path):
        raw = git('show', PIN + ':' + path)
        sources[path] = hashlib.sha256(raw).hexdigest()
        return raw.decode()

    def analyze(path):
        if path in records:
            return records[path]
        text = read(path)
        code = code_only(text)
        imports = []
        for match in re.finditer(r'^\s*(?:public\s+)?import\s+([^\n]+)', code, re.M):
            imports.extend(match.group(1).split())
        decls = [{'kind': m.group(1), 'name': m.group(2),
                  'line': code[:m.start()].count('\n') + 1}
                 for m in re.finditer(r'\b(theorem|lemma|def|abbrev|axiom|structure|class)\s+([^\s:(\[\{]+)', code)]
        record = {'path': path, 'sha256': sources[path], 'imports': imports,
                  'local_imports': [modules[m] for m in imports if m in modules],
                  'external_imports': [m for m in imports if m not in modules],
                  'placeholders': [{'token': m.group(), 'line': code[:m.start()].count('\n') + 1}
                                   for m in re.finditer(r'\b(?:sorry|admit)\b', code)],
                  'declared_axioms': [d for d in decls if d['kind'] == 'axiom'],
                  'declarations': decls}
        records[path] = record
        return record

    problem_rows = []
    for path in sorted(p for p in lean_paths if re.fullmatch(r'problems/t[^/]+/lean/Main\.lean', p)):
        pid = path.split('/')[1]
        rec = analyze(path)
        status_path = f'problems/{pid}/status.json'
        status = json.loads(read(status_path)) if status_path in paths else {}
        statement_path = f'problems/{pid}/problem.md'
        statement = read(statement_path) if statement_path in paths else ''
        source_complete = not rec['placeholders'] and not rec['declared_axioms'] and any(
            d['kind'] == 'theorem' for d in rec['declarations'])
        problem_rows.append({'id': pid, 'path': path,
                             'source_complete_candidate': source_complete,
                             'placeholder_count': len(rec['placeholders']),
                             'classification': status.get('classification'),
                             'status_sorry_count': status.get('lean', {}).get('sorry_count_main'),
                             'retained_sorry_audit': status.get('sorry_audit'),
                             'statement_path': statement_path, 'statement': statement})
    completed = [r for r in problem_rows if r['source_complete_candidate']]
    dag = read('TOPOLOGY-DEPENDENCY-DAG.md')
    # Preserve explicit closure rows as evidence, without inferring completion
    # of a whole problem from an explicitly completed branch.
    table = dag.split('## Problems closed during consolidation', 1)[1].split('Bounded partial closures', 1)[0]
    closure_rows = [line for line in table.splitlines() if re.match(r'\| `t\d', line)]
    branch_problems = sorted({p for row in closure_rows if 'branch' in row.split('|')[1]
                              for p in re.findall(r'`(t\d\d[AJ]\d\d)`', row.split('|')[1])})
    # All dependency traversals start from the complete population, plus the
    # separately labelled completed branches of still-open problems.
    branch_roots = [r for r in problem_rows if r['id'] in branch_problems and not r['source_complete_candidate']]
    reach = {}
    for row in completed + branch_roots:
        seen, todo = set(), [row['path']]
        while todo:
            path = todo.pop()
            if path in seen:
                continue
            seen.add(path)
            todo.extend(analyze(path)['local_imports'])
        reach[row['id']] = sorted(seen - {row['path']})
    # Inspect every ConstructionTargets module too: a banked theorem may not
    # yet have a completed consumer. Do not relabel it consumer-certified.
    for path in sorted(p for p in lean_paths if p.startswith('ConstructionTargets/')):
        analyze(path)
    consumers = collections.defaultdict(list)
    for row in completed:
        for path in reach[row['id']]:
            consumers[path].append(row['id'])
    all_ct = [r for p, r in records.items() if p.startswith('ConstructionTargets/')]
    worklists = []
    for name in ['worklist.edn', 'worklist2.edn']:
        path = 'holes/labs/topology-contract/' + name
        text = read(path)
        selected = json.loads(subprocess.check_output(
            ['bb', str(HERE / 'worklist_projection.bb')], input=text.encode()))
        rows = selected['items']
        worklists.append({'path': path, 'sha256': sources[path], 'selected': selected,
                          'states': dict(collections.Counter(r['state'] for r in rows)),
                          'done_kinds': dict(collections.Counter(r['kind'] for r in rows if r['state'] == 'done'))})
    result = {'schema': 'pattern-construction-topology-census-v1', 'git_pin': PIN,
              'scope': 'all 138 canonical t-prefix bundles; every ConstructionTargets Lean module; transitive local imports of all source-complete candidates and separately labelled completed branches',
              'limits': ['source scan, not a fresh Lean build',
                         'imports indicate availability/dependency, not actual declaration use',
                         'DAG branch status does not close the whole problem',
                         'zero source placeholders does not establish semantic faithfulness or absence of transitive axioms'],
              'summary': {'problem_count': len(problem_rows), 'source_complete_candidates': len(completed),
                          'classifications': dict(collections.Counter(r['classification'] for r in problem_rows)),
                          'completed_candidate_ids': [r['id'] for r in completed],
                          'completed_branch_open_problem_ids': [r['id'] for r in branch_roots],
                          'construction_target_modules': len(all_ct),
                          'construction_targets_no_source_placeholders_or_declared_axioms': sum(not r['placeholders'] and not r['declared_axioms'] for r in all_ct),
                          'construction_targets_reached_by_complete_consumers': sum(p.startswith('ConstructionTargets/') for p in consumers),
                          'construction_targets_shared_by_complete_consumers': sum(p.startswith('ConstructionTargets/') and len(cs) > 1 for p, cs in consumers.items())},
              'worklists': worklists,
              'problems': problem_rows, 'dag_closure_rows': closure_rows,
              'dependency_closure': reach,
              'module_consumers': dict(sorted(consumers.items())),
              'source_records': dict(sorted(records.items())), 'source_sha256': dict(sorted(sources.items()))}
    (HERE / 'topology-census.json').write_text(json.dumps(result, indent=2, ensure_ascii=False) + '\n')
    print(json.dumps(result['summary'], indent=2))


if __name__ == '__main__':
    main()
