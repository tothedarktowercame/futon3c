#!/usr/bin/env python3
"""Second declared test: pairs picked in the first half, counted in the second, against a shuffle."""
import os, random, collections as C, numpy as np, itertools as I, re
src=open('recur.py').read(); head=src[:src.index("rng=random.Random(20261102)")]
exec(head)
NS=int(os.environ.get('NS','500')); rng=random.Random(20261103)
def adj(h):
    by=C.defaultdict(list)
    for t in h: by[t[1]].append(t[2])
    return [(a,b) for s in by.values() for a,b in zip(s,s[1:]) if a and b]
for name in os.environ.get('ARMS','PABCD'):
    Y=arm(name); m=len(Y)//2; H1,H2=Y[:m],Y[m:]
    l1=[t[2] for t in H1 if t[2]]; l2=[t[2] for t in H2 if t[2]]
    c=C.Counter(p for t in l1 for p in I.combinations(sorted(t),2)); sel={p for p,n in c.items() if n>=2}
    count=lambda ls:sum(1 for t in ls for p in I.combinations(sorted(t),2) if p in sel)
    o=count(l2); pool=[x for t in l2 for x in t]; sizes=[len(t) for t in l2]; ns=[]
    for _ in range(NS):
        rng.shuffle(pool); i=0; out=[]
        for n in sizes: out.append(set(pool[i:i+n])); i+=n
        ns.append(count(out))
    p=(sum(x>=o for x in ns)+1)/(NS+1); print(name,'S3 selected',len(sel),'observed',o,'null',round(np.mean(ns),1),'sd',round(np.std(ns),1),'p',round(p,4),'PASS' if p<=0.0042 else 'fail',flush=True)
    a1,a2=adj(H1),adj(H2)
    c=C.Counter((x,y) for a,b in a1 for x in a for y in b if x!=y); sel={k for k,n in c.items() if n>=2}
    cnt=lambda q:sum(1 for a,b in q for x in a for y in b if (x,y) in sel)
    o=cnt(a2); ns=[]; bs=[b for _,b in a2]; as_=[a for a,_ in a2]
    for _ in range(NS):
        rng.shuffle(bs); ns.append(cnt(list(zip(as_,bs))))
    p=(sum(x>=o for x in ns)+1)/(NS+1); print(name,'S4 selected',len(sel),'observed',o,'null',round(np.mean(ns),1),'sd',round(np.std(ns),1),'p',round(p,4),'PASS' if p<=0.0042 else 'fail',flush=True)
