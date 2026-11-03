import json, random, numpy as np, collections as C
it=json.load(open('items.json')); v=np.load('vectors.npy'); ix={x['id']:i for i,x in enumerate(it)}
lab=json.load(open('arm-b-labels.json'))['label']
g=C.defaultdict(list)
for p,l in lab.items(): g[l].append(p)
multi=[m for m in g.values() if len(m)>1]
print('merged groups',len(multi),'sizes',C.Counter(len(m) for m in multi))
r=random.Random(30); pairs=[]
for m in multi:
    for a in range(len(m)):
        for b in range(a+1,len(m)): pairs.append((m[a],m[b]))
print('pairs',len(pairs))
for a,b in r.sample(pairs,30): print(round(float(v[ix[a]]@v[ix[b]]),2),a,'|',b)
pr=[x for x in it if x['kind']=='proposal']; lib=[i for i,x in enumerate(it) if x['kind']=='library']; libids={it[i]['id'] for i in lib}
par=C.Counter(x.get('parent') for x in pr if x.get('parent') in libids)
print('distinct parents',len(par),'of',len(lib),'library'); print(par.most_common(16))
top=sum(n for _,n in par.most_common(30)); print('top 30 parents hold',top,'of',sum(par.values()))
d=[];rd=[]; rank=[]
for x in r.sample([x for x in pr if x.get('parent') in libids],800):
    q=v[ix[x['id']]]; s=v[lib]@q; d.append(float(q@v[ix[x['parent']]])); rd.append(float(s[r.randrange(len(lib))])); rank.append(int((s>d[-1]).sum())+1)
print('cos to parent median',round(np.median(d),3),'to random library',round(np.median(rd),3),'parent rank median',np.median(rank),'in top10',np.mean(np.array(rank)<=10).round(3))
print('dom of parents',C.Counter(p.split('/')[0] for p in par.elements()).most_common(12))
