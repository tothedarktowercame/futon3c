#!/usr/bin/env python3
"""Observation-channel audit (Experiment 5) over a frozen receipts export.

Read-only. Deterministic: same input -> same output. Emits the funnel from
dispatched offered halves down to metric-bearing rows, plus the attribution
quality of :inclusion-reasons, which is what Psi-v2 credit assignment depends on.

Usage: observation_channel_audit.py <receipts-export.edn> [--edn out.edn]
"""
import re
import sys
import collections


def receipt_blobs(text):
    """Split the receipt vector into top-level evidence records.

    Two serializations occur: the frozen lab exports use the reader form
    `{:receipts [#:evidence{...}]}`, while a live
    /api/alpha/evidence read returns `{:entries [{:evidence/body ...}]}`.
    Both are accepted so the same audit runs against either.
    """
    for key, opener in ((':receipts [', r'#:evidence\{'),
                        (':entries [', r'\{:evidence/body')):
        if key in text:
            body = text[text.find(key):]
            break
    else:
        raise SystemExit('no :receipts or :entries vector found')
    out = []
    for m in re.finditer(opener, body):
        start = m.start()
        i = body.find('{', start)
        depth, j = 0, i
        while j < len(body):
            if body[j] == '{':
                depth += 1
            elif body[j] == '}':
                depth -= 1
                if depth == 0:
                    break
            j += 1
        out.append(body[start:j + 1])
    return [r for r in out if 'pattern-outcome' in r]


# Keys may be bare (:used-ids) or namespaced (:memory-use/used-ids). Anchor on
# a leading ':' or '/' so that a lookup for :used-ids cannot match inside
# :unused-ids -- a collision that silently inverts the metric-bearing count.
def _pat(key):
    return r'(?:^|[:/])' + re.escape(key.lstrip(':'))


def scalar(rec, key):
    m = re.search(_pat(key) + r'\s+([:\w".\-]+)', rec)
    return m.group(1) if m else None


def id_list(rec, key):
    m = re.search(_pat(key) + r'\s+\[([^\]]*)\]', rec)
    return re.findall(r'"([^"]+)"', m.group(1)) if m else []


def reason_strings(rec):
    m = re.search(r':inclusion-reasons \[(.*?)\]\s*,', rec, re.S)
    if not m:
        return []
    return re.findall(r':reason "([^"]*)"', m.group(1))


def audit(text):
    recs = receipt_blobs(text)
    offered, outcome = {}, {}
    status = collections.Counter()
    reason = collections.Counter()
    for r in recs:
        phase = scalar(r, ':phase')
        job = scalar(r, ':job-id')
        if phase == ':offered':
            offered[job] = r
            status[scalar(r, ':recall-status')] += 1
            rr = scalar(r, ':recall-reason')
            if rr:
                reason[rr] += 1
        elif phase == ':outcome':
            outcome[job] = r

    joined = sorted(set(offered) & set(outcome))
    metric_bearing = [j for j in joined if id_list(outcome[j], ':used-ids')]

    surfaced_per_row = [len(id_list(r, ':surfaced-ids')) for r in offered.values()]
    surfacing_rows = [n for n in surfaced_per_row if n > 0]

    # Attribution quality: distinct reason strings across all offered halves.
    all_reasons = [s for r in offered.values() for s in reason_strings(r)]
    distinct_reasons = sorted(set(all_reasons))
    rows_with_reasons = sum(1 for r in offered.values() if reason_strings(r))

    return {
        'total-halves': len(recs),
        'offered-halves': len(offered),
        'outcome-halves': len(outcome),
        'joined-rows': len(joined),
        'metric-bearing-rows': len(metric_bearing),
        'offered-without-outcome': len(set(offered) - set(outcome)),
        'outcome-without-offered': len(set(outcome) - set(offered)),
        'outcome-half-completion': round(len(outcome) / len(offered), 4) if offered else None,
        'metric-bearing-yield': round(len(metric_bearing) / len(offered), 4) if offered else None,
        'recall-status': dict(status),
        'recall-reason': dict(reason),
        'offered-rows-surfacing-nothing': len(surfaced_per_row) - len(surfacing_rows),
        'offered-rows-surfacing-something': len(surfacing_rows),
        'total-surfaced-ids': sum(surfaced_per_row),
        'surfaced-ids-per-surfacing-row': sorted(surfacing_rows),
        'rows-with-inclusion-reasons': rows_with_reasons,
        'distinct-inclusion-reason-strings': len(distinct_reasons),
        'inclusion-reason-vocabulary': distinct_reasons,
        'rows-with-surfacing-via': sum(1 for r in offered.values() if 'surfacing-via' in r),
    }


def main():
    path = sys.argv[1]
    result = audit(open(path).read())
    width = max(len(k) for k in result)
    for k, v in result.items():
        print(f'{k:<{width}}  {v}')
    if '--edn' in sys.argv:
        out = sys.argv[sys.argv.index('--edn') + 1]
        with open(out, 'w') as f:
            f.write('{:source "%s"\n' % path)
            for k, v in result.items():
                f.write(' :%s %s\n' % (k, repr(v).replace("'", '"')))
            f.write('}\n')
        print(f'\nwrote {out}')


if __name__ == '__main__':
    main()
