#!/usr/bin/env python3
"""Weighted spend across Claude Code sessions, from ~/.claude/projects/*/*.jsonl.

Raw token totals are misleading: cache reads are 0.1x base input, cache writes
1.25x (5m TTL) or 2x (1h TTL). This prices each usage record at Opus 5 list
rates so sessions are comparable in the unit that is actually billed.

  ./claude-spend.py            # per project dir
  ./claude-spend.py -s         # per session, plus cost decomposition
"""
import json, glob, os, sys, collections

IN, OUT = 5.0, 25.0            # Opus 5 $/MTok; no long-context premium
RD, W5, W1H = IN*0.1, IN*1.25, IN*2.0

def scan(path):
    a, n, ctxs = collections.Counter(), 0, []
    for line in open(path, errors='replace'):
        try: d = json.loads(line)
        except ValueError: continue
        u = (d.get('message') or {}).get('usage')
        if not u: continue
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

per_session = '-s' in sys.argv
root = os.path.expanduser('~/.claude/projects')
rows, grand, gmsgs, gctx = [], collections.Counter(), 0, 0
groups = collections.defaultdict(lambda: [collections.Counter(), 0, []])
for p in glob.glob(os.path.join(root, '*', '*.jsonl')):
    a, n, ctxs = scan(p)
    if not n: continue
    key = os.path.basename(p)[:8] if per_session else os.path.basename(os.path.dirname(p))
    g = groups[key]
    for k, v in a.items(): g[0][k] += v
    g[1] += n; g[2] += ctxs
    for k, v in a.items(): grand[k] += v
    gmsgs += n; gctx += sum(ctxs)

print(f"{'key':46} {'$est':>9} {'msgs':>7} {'meanctx':>9} {'maxctx':>9} {'$/msg':>6}")
for key, (a, n, ctxs) in sorted(groups.items(), key=lambda x: -cost(x[1][0])):
    c = cost(a)
    if c < 0.01: continue
    print(f"{key[:46]:46} {c:9.2f} {n:7} {sum(ctxs)/n:9,.0f} {max(ctxs):9,} {c/n:6.3f}")
gc = cost(grand)
print(f"\nGRAND TOTAL ${gc:,.2f}  msgs {gmsgs:,}  mean ctx {gctx/gmsgs:,.0f}  ${gc/gmsgs:.3f}/msg")
print(f"  uncached-in ${grand['in']*IN/1e6:8.2f}  cache-write ${(grand['cc5']*W5+grand['cc1h']*W1H)/1e6:8.2f}"
      f"  cache-read ${grand['cr']*RD/1e6:8.2f}  output ${grand['out']*OUT/1e6:8.2f}")
