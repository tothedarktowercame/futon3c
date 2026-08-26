#!/usr/bin/env python3
"""Why-graph metrics V2 never measured: out-degree of has-semantic-why per pattern,
in-degree of the hub, and which patterns WITH reviewed attachments are why-reachable
from f42's seed patterns (the expander's BFS, outgoing edges only).
Usage: why_graph_metrics.py RELATIONS.edn ATTACHMENTS.edn F42-RUN.edn"""
import re, sys
from collections import Counter, defaultdict
rel=open(sys.argv[1]).read(); att=open(sys.argv[2]).read()
pairs=re.findall(r':relation/from "([^"]+)".*?:relation/(?:to|dst) "([^"]+)"',rel,re.S)
pairs=[(f,d) for f,d in pairs]
out=defaultdict(set)
for f,d in pairs: out[f].add(d)
hub='math-strategy/missing-dependency-protocol'
attached=Counter()
for c in att.split(':hx/ends [')[1:]:
    if ':state :current' in c and ':attachment-status :reviewed' in c:
        m=re.search(r':patterns \[([^\]]*)\]',c)
        for p in re.findall(r'"([^"]+)"',m.group(1) if m else ''): attached[p]+=1
f42=open(sys.argv[3]).read()
seeds=re.findall(r'"([^"]+)"',re.search(r':seed-patterns\s*\[(.*?)\]',f42,re.S).group(1))
seen={s:0 for s in seeds}; q=list(seeds)
while q:
    p=q.pop(0)
    for t in out.get(p,()):
        if t not in seen: seen[t]=seen[p]+1; q.append(t)
reach={p:h for p,h in seen.items() if p not in seeds}
reach_att={p:(h,attached[p]) for p,h in reach.items() if attached[p]}
print('why relations', len(pairs), '| patterns with out-edges', len(out), '| hub in-degree', sum(1 for f,d in pairs if d==hub))
print('into hub from:', sorted(f for f,d in pairs if d==hub))
print('f42 seeds', len(seeds), '| why-reachable patterns', len(reach), '| with reviewed attachments', len(reach_att))
print('reachable-with-attachments {pattern: (hops, attachments)}:', reach_att)
print('memories reachable by why-hop (sum of attachments):', sum(a for _,a in reach_att.values()))
