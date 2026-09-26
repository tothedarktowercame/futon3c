#!/usr/bin/env python3
"""Extract runner memory-usage reports for the V2-3 rejection taxonomy.

Why this exists: the structured receipt fields are almost entirely empty
(7 non-empty :rejection-reasons across the whole corpus). The reasoning lives
in the runners' free-text output, captured in job-results-20260731/. This
script pulls those reports out and joins them to each dispatch's receipt ids so
a coded reason can be attributed to a specific memory.

Read-only, deterministic. Output is a coding worksheet, not a result.

Usage: extract_memory_reports.py <job-results-dir> <receipts.edn> [--out FILE]
"""
import json
import glob
import os
import re
import sys
import collections

# The dispatch prompt itself instructs runners to report memory usage, so a
# naive search matches the prompt echo rather than the answer. Only the
# runner's own output (result / result-summary / text events) is searched.
REPORT_RE = re.compile(
    r'(?i)(memory usage|memories? (?:used|ignored|surfaced|declined))')


def runner_text(job):
    """Everything the runner said, excluding the prompt it was given."""
    parts = []
    for key in ('result', 'result-summary'):
        v = job.get(key)
        if isinstance(v, dict):
            v = json.dumps(v)
        if v:
            parts.append(str(v))
    for e in job.get('events', []):
        if e.get('type') == 'text' and e.get('text'):
            parts.append(e['text'])
    return '\n'.join(parts)


def extract_report(text):
    """Return the memory-usage section, or None."""
    m = REPORT_RE.search(text)
    if not m:
        return None
    # Take from the heading to the end; these sections are terminal in practice.
    return text[m.start():].strip()


def receipt_index(path):
    """job-id -> {surfaced, used, rejected, problem} from the frozen export."""
    s = open(path, errors='replace').read()
    index = {}
    for rec in re.split(r'(?=\{:evidence/body)', s):
        job = re.search(r':job-id "([^"]+)"', rec)
        if not job:
            continue
        def ids(field):
            m = re.search(r'(?:^|[:/])' + field + r'\s+\[([^\]]*)\]', rec)
            return re.findall(r'"([^"]+)"', m.group(1)) if m else []
        prob = re.search(r':problem "([^"]+)"', rec)
        entry = index.setdefault(job.group(1), {
            'problem': prob.group(1) if prob else None,
            'surfaced': [], 'used': [], 'rejected': []})
        for key, field in (('surfaced', 'surfaced-ids'),
                           ('used', 'used-ids'),
                           ('rejected', 'rejected-ids')):
            got = ids(field)
            if got:
                entry[key] = sorted(set(entry[key]) | set(got))
        if prob and not entry['problem']:
            entry['problem'] = prob.group(1)
    return index


def main():
    jobs_dir, receipts = sys.argv[1], sys.argv[2]
    index = receipt_index(receipts)
    rows, stats = [], collections.Counter()

    for path in sorted(glob.glob(os.path.join(jobs_dir, '*.edn'))):
        job = json.load(open(path, errors='replace')).get('job', {})
        job_id = job.get('job-id') or os.path.basename(path)[:-4]
        report = extract_report(runner_text(job))
        stats['files'] += 1
        if report is None:
            stats['no-report'] += 1
            continue
        stats['with-report'] += 1
        meta = index.get(job_id, {})
        surfaced = meta.get('surfaced', [])
        stats['report-and-surfaced-ids'] += bool(surfaced)
        stats['report-but-no-surfaced-ids'] += (not surfaced)
        rows.append({
            'job-id': job_id,
            'problem': meta.get('problem'),
            'agent': job.get('agent-id'),
            'surfaced-ids': surfaced,
            'used-ids': meta.get('used', []),
            'rejected-ids': meta.get('rejected', []),
            'report-chars': len(report),
            'report': report,
            # filled in by the coder, per the pre-registered protocol
            'codes': [],
            'unattributable?': None,
        })

    for k, v in sorted(stats.items()):
        print(f'{k:<28} {v}')
    print(f'\ncodeable rows: {len(rows)}')
    if '--out' in sys.argv:
        out = sys.argv[sys.argv.index('--out') + 1]
        with open(out, 'w') as f:
            json.dump({'source-jobs': jobs_dir,
                       'source-receipts': receipts,
                       'stats': dict(stats),
                       'rows': rows}, f, indent=1, sort_keys=True)
        print(f'wrote {out}')


if __name__ == '__main__':
    main()
