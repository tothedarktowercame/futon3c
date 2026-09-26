#!/usr/bin/env python3
"""Warrant/provenance audit (Experiment 6) over the frozen damage-state fixture.

Read-only, deterministic. Answers one question the paper needs and previously
took on report: of the attachments whose :witness-status says
:independently-witnessed, how many carry an actual witness record?

Usage: warrant_audit.py <fixture.edn> [--edn out.edn]
"""
import re
import sys
import collections

WITNESS_FIELDS = (':witness ', ':witnesses ', ':witness-refs', ':witness-evidence')


def _enclosing_map(text, pos):
    """Return (start, end) of the innermost balanced {...} containing pos."""
    depth, i = 0, pos
    while i > 0:
        if text[i] == '}':
            depth += 1
        elif text[i] == '{':
            if depth == 0:
                break
            depth -= 1
        i -= 1
    depth, j = 0, i
    while j < len(text):
        if text[j] == '{':
            depth += 1
        elif text[j] == '}':
            depth -= 1
            if depth == 0:
                break
        j += 1
    return i, j + 1


def records(text, marker=':witness-status'):
    """Partition the fixture into one span per marker occurrence.

    Brace-walking proved unreliable here: the attachment map carrying
    :witness-status and the memory body that would carry a witness record are
    not reliably within a common balanced map in this serialization (verified
    by hand on e-codexpilot-close-a92J05-…, where the two sit 1693 chars apart
    at equal brace depth). Partitioning on marker positions is crude but
    correct for a contiguous per-memory serialization: each span holds exactly
    one attachment and the entry serialized alongside it.
    """
    pos = [m.start() for m in re.finditer(re.escape(marker), text)]
    bounds = pos + [len(text)]
    return [text[bounds[i]:bounds[i + 1]] for i in range(len(pos))]


def audit(text):
    blocks = records(text)
    status = collections.Counter()
    has_witness = collections.Counter()
    reviewed_and_witnessed = 0
    witnessed_no_record = 0
    for b in blocks:
        m = re.search(r':witness-status\s+([:\w-]+)', b)
        if not m:
            continue
        st = m.group(1)
        status[st] += 1
        present = any(f in b for f in WITNESS_FIELDS)
        has_witness[(st, present)] += 1
        if st == ':independently-witnessed':
            if ':attachment-status :reviewed' in b:
                reviewed_and_witnessed += 1
            if not present:
                witnessed_no_record += 1
    total_iw = status.get(':independently-witnessed', 0)
    # The decisive cross-tab: does the presence of a real witness record track
    # the status field at all? Reported per status value, not just for :iw.
    cross = {f'{st}/{"with" if p else "without"}-witness-record': n
             for (st, p), n in sorted(has_witness.items())}
    return {
        'attachments-with-witness-status': sum(status.values()),
        'witness-status-values': dict(status),
        'independently-witnessed': total_iw,
        'independently-witnessed-with-witness-record': total_iw - witnessed_no_record,
        'independently-witnessed-WITHOUT-witness-record': witnessed_no_record,
        'independently-witnessed-and-reviewed': reviewed_and_witnessed,
        'fraction-witnessed-lacking-record': (
            round(witnessed_no_record / total_iw, 4) if total_iw else None),
        'status-x-witness-record': cross,
        'witness-field-names-searched': list(WITNESS_FIELDS),
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
