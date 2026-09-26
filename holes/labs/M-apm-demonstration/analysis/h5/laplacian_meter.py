#!/usr/bin/env python3
"""Reproduces holes/labs/M-typed-memories/connectivity_meter.bb (V1/V2's instrument)
outside Babashka: nodes = memories + patterns (+ distilled e- targets); edges =
memory->pattern attachments and memory->distills, deduplicated by (source,target,type);
reviewed = some duplicate is :state :current AND :attachment-status :reviewed;
components over reviewed edges; unnormalised combinatorial Laplacian L = D - A of
the largest component; lambda_2 = second-smallest eigenvalue. Same definitions,
numpy instead of Jacobi sweeps (the .bb did not finish on 696 rows).
Usage: laplacian_meter.py EXPORT.edn   (raw {:hyperedges [...]} capture)"""
import re, sys
import numpy as np
t=open(sys.argv[1]).read()
chunks=t.split(':hx/ends [')[1:]
edges={}
mems=set()
for c in chunks:
    m=re.search(r':entry "([^"]+)"',c)
    if not m: continue
    mem=m.group(1); mems.add(mem)
    rev = (':state :current' in c) and (':attachment-status :reviewed' in c)
    pm=re.search(r':patterns \[([^\]]*)\]',c)
    for p in re.findall(r'"([^"]+)"',pm.group(1) if pm else ''):
        k=(mem,p,'pattern-attachment'); edges[k]=edges.get(k,False) or rev
    dm=re.search(r':distills \[([^\]]*)\]',c)
    for d in re.findall(r'"([^"]+)"',dm.group(1) if dm else ''):
        if d.startswith('e-'):
            k=(mem,d,'distills'); edges[k]=edges.get(k,False) or rev
rev_edges=[k for k,v in edges.items() if v]
nodes=set(mems)|{s for s,_,_ in edges}|{tg for _,tg,_ in edges}
adj={n:set() for n in nodes}
for s,tg,_ in rev_edges: adj[s].add(tg); adj[tg].add(s)
seen=set(); comps=[]
for n in sorted(nodes):
    if n in seen: continue
    st=[n]; comp=set()
    while st:
        x=st.pop()
        if x in comp: continue
        comp.add(x); st.extend(adj[x]-comp)
    seen|=comp; comps.append(sorted(comp))
comps.sort(key=lambda c:(-len(c),c[0]))
print("edges",len(edges),"reviewed-current",len(rev_edges),"nodes",len(nodes),"reviewed-components",len(comps),
      "size-histogram",{k:v for k,v in sorted(__import__('collections').Counter(len(c) for c in comps).items(),reverse=True)[:6]})
for comp in comps[:3]:
    idx={n:i for i,n in enumerate(comp)}; n=len(comp)
    L=np.zeros((n,n))
    ce=[(s,tg,ty) for s,tg,ty in rev_edges if s in idx and tg in idx]
    for s,tg,_ in ce:
        i,j=idx[s],idx[tg]; L[i,i]+=1; L[j,j]+=1; L[i,j]-=1; L[j,i]-=1
    ev=np.sort(np.linalg.eigvalsh(L)) if n>1 else np.array([0.0])
    pats=[x for x in comp if '/' in x and not x.startswith('e-')]
    print(f"  component size {n} edges {len(ce)} edge-types {sorted(set(ty for _,_,ty in ce))} patterns {len(pats)} lambda_2 {ev[1] if n>1 else None:.6f}" if n>1 else f"  component size {n}")
