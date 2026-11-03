#!/usr/bin/env python3
"""Run the declared recurrence test for arms A, B and C."""
import json, os, glob, random, collections as C, numpy as np, itertools as I
R=os.path.expanduser('~/.emacs-graph/session-turn-analysis')
it=json.load(open('items.json')); v=np.load('vectors.npy')
lib=[i for i,x in enumerate(it) if x['kind']=='library']; pr=[i for i,x in enumerate(it) if x['kind']=='proposal']
libids={it[i]['id'] for i in lib}
# Arm B: complete-link grouping, strongest pairs first.
S=v[pr]@v[pr].T; np.fill_diagonal(S,0)
idx=np.argwhere(np.triu(S,1)>=0.70); order=sorted(map(tuple,idx),key=lambda ab:-S[ab[0],ab[1]])
grp={i:{i} for i in range(len(pr))}
for a,b in order:
    ga,gb=grp[a],grp[b]
    if ga is gb: continue
    if S[np.ix_(list(ga),list(gb))].min()>=0.70:
        ga|=gb
        for x in gb: grp[x]=ga
SL=v[pr]@v[lib].T
label={}; seen={}
for i in range(len(pr)):
    g=grp[i]; k=id(g)
    if k not in seen:
        l=list(g); j=SL[l].max(0).argmax(); best=SL[l,:][:,j].max()
        seen[k]=it[lib[j]]['id'] if best>=0.70 else 'g:'+min(it[pr[x]]['id'] for x in l)
    label[it[pr[i]]['id']]=seen[k]
groups=C.Counter(id(g) for g in grp.values())
print('proposals',len(pr),'groups',len(groups),'in groups of 2+',sum(n for n in groups.values() if n>1),'largest',max(groups.values()),'to library',sum(l in libids for l in label.values()))
parent={x['id']:x.get('parent') for x in it if x['kind']=='proposal'}
okp=sum(1 for p in parent.values() if p in libids); print('parent resolves',okp,'of',len(parent))
json.dump({'label':label},open('arm-b-labels.json','w'))
T=[]
for a in glob.glob(R+'/*.analysis.json'):
    b=a[:-len('.analysis.json')]
    if not os.path.exists(b): continue
    try: d=json.load(open(b)); A=json.load(open(a))
    except Exception: continue
    if d.get('origin')=='agent': continue
    cited=set(); 
    for s in A.get('sentences',[]):
        for fr in s.get('fragments',[]):
            for r in fr.get('pattern_refs') or []: cited.add(r['id'] if isinstance(r,dict) else r)
    ps=set()
    side=b[:-5]+'.candidates.json'
    if os.path.exists(side):
        try: ps={c['id'] for c in json.load(open(side)).get('candidates',[]) if c.get('id')}
        except Exception: pass
    if d.get('created_at') and d.get('session_id'):
        T.append((d['created_at'],d['session_id'],cited,ps))
T.sort(key=lambda t:t[0]); print('operator turns',len(T),'with labels',sum(bool(t[2]|t[3]) for t in T),T[0][0],T[-1][0])
def arm(name):
    if name=='A': f=lambda p:p
    if name=='B': f=lambda p:label.get(p,p)
    if name=='C': f=lambda p:parent[p] if parent.get(p) in libids else p
    if name=='D':   # exploratory, not declared: top-level library domain of each label
        dom=lambda q:(q.split('/')[0] if q in libids else 'new')
        return [(t[0],t[1],frozenset({dom(x) for x in t[2]}|{dom(parent.get(p) or p) for p in t[3]})) for t in T]
    if name=='P':   # positive control: arm C with 20 planted pairs, each in 6 turns per half
        r=random.Random(7); labs=sorted({x for t in T for x in t[2]}); plant=[tuple(sorted(r.sample(labs,2))) for _ in range(20)]
        Y=[[t[0],t[1],set(t[2]|{parent[p] if parent.get(p) in libids else p for p in t[3]})] for t in T]
        m=len(Y)//2
        for pr_ in plant:
            for lo,hi in ((0,m),(m,len(Y))):
                for i in r.sample(range(lo,hi),6): Y[i][2]|=set(pr_)
        return [(a,b,frozenset(c)) for a,b,c in Y]
    return [(t[0],t[1],frozenset(t[2]|{f(p) for p in t[3]})) for t in T]
def S1(ts):
    return {p for t in ts for p in I.combinations(sorted(t),2)}
def S2(seq):
    o=set()
    for a,b in seq:
        o|={(x,y) for x in a for y in b if x!=y}
    return o
rng=random.Random(20261102); NS=int(os.environ.get("NS","2000"))
res={}
for name in os.environ.get('ARMS','ABC'):
    Y=arm(name); m=len(Y)//2; H=[Y[:m],Y[m:]]
    def adj(h):
        by=C.defaultdict(list)
        for t in h: by[t[1]].append(t[2])
        return [(a,b) for s in by.values() for a,b in zip(s,s[1:]) if a and b]
    L=[[t[2] for t in h if t[2]] for h in H]; AD=[adj(h) for h in H]
    o1=len(S1(L[0])&S1(L[1])); o2=len(S2(AD[0])&S2(AD[1]))
    n1=[];n2=[]
    for _ in range(NS):
        s=[]
        for l in L:
            pool=[x for t in l for x in t]; rng.shuffle(pool); i=0; out=[]
            for t in l:
                n=len(t); out.append(tuple(sorted(set(pool[i:i+n])))); i+=n
            s.append(S1(out))
        n1.append(len(s[0]&s[1]))
        q=[]
        for ad in AD:
            b=[y for _,y in ad]; rng.shuffle(b); q.append(S2([(x,y) for (x,_),y in zip(ad,b)]))
        n2.append(len(q[0]&q[1]))
    for lab,o,n in (('S1',o1,n1),('S2',o2,n2)):
        mean=float(np.mean(n)); p=(sum(x>=o for x in n)+1)/(NS+1); ok=o>=2*mean and p<=0.0042
        print(name,lab,'observed',o,'null mean',round(mean,1),'p',round(p,4),'PASS' if ok else 'fail'); res[name+lab]=dict(o=o,mean=mean,p=p,ok=ok)
json.dump(res,open('recurrence-results-'+os.environ.get('ARMS','ABC')+'.json','w'),indent=1)
