#!/usr/bin/env python3
"""Weighted spend across Claude Code sessions, from ~/.claude/projects/*/*.jsonl.

Raw token totals are misleading: cache reads are 0.1x base input, cache writes
1.25x (5m TTL) or 2x (1h TTL). This prices each usage record at Opus 5 list
rates so sessions are comparable in the unit that is actually billed.

  ./claude-spend.py            # per project dir
  ./claude-spend.py -s         # per session, plus cost decomposition
  ./claude-spend.py --ttl      # per session: inter-turn gaps, always-5m vs always-1h
  ./claude-spend.py --burn     # per-day burn per vendor, for the weekly burn-down target

The --ttl simulation reproduces measured spend to within 0.1% on the sessions
it was checked against, so its counterfactual arm is worth believing.
"""
from datetime import datetime
import json, glob, os, sys, collections

IN, OUT = 5.0, 25.0            # Opus 5 $/MTok; no long-context premium
RD, W5, W1H = IN*0.1, IN*1.25, IN*2.0

def message_key(record):
    """Key the content-block records emitted for one assistant message."""
    message = record.get('message') or {}
    return message.get('id') or record.get('uuid') or id(record)

def first_record_for_message(record, seen):
    """Count repeated message usage only on its first JSONL record."""
    key = message_key(record)
    if key in seen:
        return False
    seen.add(key)
    return True

def scan(path):
    a, n, ctxs = collections.Counter(), 0, []
    seen = set()
    for line in open(path, errors='replace'):
        try: d = json.loads(line)
        except ValueError: continue
        u = (d.get('message') or {}).get('usage')
        if not u or not first_record_for_message(d, seen): continue
        n += 1
        cd = u.get('cache_creation') or {}
        cc = u.get('cache_creation_input_tokens', 0) or 0
        a['in'] += u.get('input_tokens', 0) or 0
        a['cr'] += u.get('cache_read_input_tokens', 0) or 0
        a['out'] += u.get('output_tokens', 0) or 0
        if cd:
            a['cc5'] += cd.get('ephemeral_5m_input_tokens', 0) or 0
            a['cc1h'] += cd.get('ephemeral_1h_input_tokens', 0) or 0
        else:
            a['cc5'] += cc
        ctxs.append((u.get('input_tokens', 0) or 0) + (u.get('cache_read_input_tokens', 0) or 0) + cc)
    return a, n, ctxs

def cost(a):
    return (a['in']*IN + a['cr']*RD + a['cc5']*W5 + a['cc1h']*W1H + a['out']*OUT) / 1e6

root = os.path.expanduser('~/.claude/projects')

def burn_report(days=14):
    """Per-day burn per vendor. The target is FULL utilisation of all three
    subscriptions each week, so read this for idle days, not for big numbers.

    Two known gaps, both of which understate non-Claude burn:
      - Codex: local ~/.codex/sessions only. Off-site seats (ams-*, oxf-*) keep
        their rollouts on their own boxes and are invisible here.
      - Zai: no usage capture at all until repair/zai-token-usage is merged.
    """
    cl, clt = collections.Counter(), collections.Counter()
    for path in glob.glob(os.path.join(root, '*', '*.jsonl')):
        seen = set()
        for line in open(path, errors='replace'):
            try: d = json.loads(line)
            except ValueError: continue
            u = (d.get('message') or {}).get('usage'); t = d.get('timestamp')
            if not u or not t or not first_record_for_message(d, seen): continue
            cd = u.get('cache_creation') or {}
            cc = u.get('cache_creation_input_tokens', 0) or 0
            c5 = cd.get('ephemeral_5m_input_tokens', 0) or 0 if cd else cc
            c1 = cd.get('ephemeral_1h_input_tokens', 0) or 0 if cd else 0
            cl[t[:10]] += ((u.get('input_tokens', 0) or 0) * IN
                           + (u.get('cache_read_input_tokens', 0) or 0) * RD
                           + c5 * W5 + c1 * W1H
                           + (u.get('output_tokens', 0) or 0) * OUT) / 1e6
            clt[t[:10]] += 1
    cx, cxs = collections.Counter(), collections.Counter()
    for path in glob.glob(os.path.expanduser('~/.codex/sessions/**/rollout-*.jsonl'), recursive=True):
        last = lastts = None
        for line in open(path, errors='replace'):
            if '"token_count"' not in line: continue
            try: d = json.loads(line)
            except ValueError: continue
            tu = ((d.get('payload') or d).get('info') or {}).get('total_token_usage')
            if tu: last, lastts = tu, d.get('timestamp') or lastts
        if last:
            day = (lastts or os.path.basename(path)[8:18])[:10]
            cx[day] += last['total_tokens']; cxs[day] += 1
    print(f"{'date':12} {'claude $':>10} {'cl turns':>9} {'codex tok':>15} {'cx sess':>8}  idle")
    for day in sorted(set(cl) | set(cx))[-days:]:
        flag = 'CODEX IDLE' if not cx[day] else ''
        print(f"{day:12} {cl[day]:10.2f} {clt[day]:9} {cx[day]:15,} {cxs[day]:8}  {flag}")
    print("\nZai: UNMEASURABLE — repair/zai-token-usage is unmerged. A three-way")
    print("burn-down cannot be run with one arm blind. Merge it first.")
    print("Codex figures are LOCAL rollouts only; off-site seats are not counted.")

def ttl_report():
    def when(x): return datetime.fromisoformat(x.replace('Z', '+00:00')).timestamp()
    print(f"{'session':10} {'turns':>6} {'<5m':>6} {'5-60m':>6} {'>60m':>6} "
          f"{'5m $':>9} {'cold':>5} {'1h $':>9} {'cold':>5}  winner")
    for path in sorted(glob.glob(os.path.join(root, '*', '*.jsonl'))):
        turns = []
        seen = set()
        for line in open(path, errors='replace'):
            try: d = json.loads(line)
            except ValueError: continue
            u = (d.get('message') or {}).get('usage')
            if (not u or not d.get('timestamp')
                    or not first_record_for_message(d, seen)): continue
            turns.append((when(d['timestamp']), u.get('input_tokens', 0) or 0,
                          u.get('cache_read_input_tokens', 0) or 0,
                          u.get('cache_creation_input_tokens', 0) or 0))
        if len(turns) < 20: continue
        turns.sort()
        gaps = [turns[k][0] - turns[k-1][0] for k in range(1, len(turns))]
        b = collections.Counter('<5m' if g <= 300 else '5-60m' if g <= 3600 else '>60m' for g in gaps)
        n = len(gaps)
        def run(ttl, W):
            tot, cold = 0.0, 0
            for k, (t, i, cr, cc) in enumerate(turns):
                gap = t - turns[k-1][0] if k else 1e9
                if gap > ttl:            # prefix dead: the WHOLE context is rewritten
                    tot += (i + cr + cc) * W; cold += 1
                else:
                    tot += cr * 0.1 + cc * W + i * 1.0
            return tot * IN / 1e6, cold
        c5, k5 = run(300, 1.25); c1, k1 = run(3600, 2.0)
        print(f"{os.path.basename(path)[:8]:10} {len(turns):6} "
              f"{100*b['<5m']/n:5.1f}% {100*b['5-60m']/n:5.1f}% {100*b['>60m']/n:5.1f}% "
              f"{c5:9.2f} {k5:5} {c1:9.2f} {k1:5}  {'1h' if c1 < c5 else '5m'} by ${abs(c5-c1):.2f}")
    print("\nA cold miss rewrites the ENTIRE context, not the delta — which is why the")
    print("per-prefix break-even rule (1h wins iff P(5-60m gap) > 65%) gives the wrong")
    print("answer for big-context sessions. Compare the simulated totals, not the gaps.")

def main(argv=None):
    argv = sys.argv[1:] if argv is None else argv
    if '--burn' in argv:
        burn_report()
        return
    if '--ttl' in argv:
        ttl_report()
        return

    per_session = '-s' in argv
    rows, grand, gmsgs, gctx = [], collections.Counter(), 0, 0
    groups = collections.defaultdict(lambda: [collections.Counter(), 0, []])
    for p in glob.glob(os.path.join(root, '*', '*.jsonl')):
        a, n, ctxs = scan(p)
        if not n: continue
        key = (os.path.basename(p)[:8] if per_session
               else os.path.basename(os.path.dirname(p)))
        g = groups[key]
        for k, v in a.items(): g[0][k] += v
        g[1] += n; g[2] += ctxs
        for k, v in a.items(): grand[k] += v
        gmsgs += n; gctx += sum(ctxs)

    print(f"{'key':46} {'$est':>9} {'msgs':>7} {'meanctx':>9} "
          f"{'maxctx':>9} {'$/msg':>6}")
    for key, (a, n, ctxs) in sorted(groups.items(),
                                     key=lambda x: -cost(x[1][0])):
        c = cost(a)
        if c < 0.01: continue
        print(f"{key[:46]:46} {c:9.2f} {n:7} {sum(ctxs)/n:9,.0f} "
              f"{max(ctxs):9,} {c/n:6.3f}")
    gc = cost(grand)
    print(f"\nGRAND TOTAL ${gc:,.2f}  msgs {gmsgs:,}  "
          f"mean ctx {gctx/gmsgs:,.0f}  ${gc/gmsgs:.3f}/msg")
    print(f"  uncached-in ${grand['in']*IN/1e6:8.2f}  "
          f"cache-write ${(grand['cc5']*W5+grand['cc1h']*W1H)/1e6:8.2f}"
          f"  cache-read ${grand['cr']*RD/1e6:8.2f}  "
          f"output ${grand['out']*OUT/1e6:8.2f}")


if __name__ == '__main__':
    main()
