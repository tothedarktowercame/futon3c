"""Tickets as a tree: status > way or wing > cluster > item, with unplaced shown live. Items with a second way appear
under the first and carry `also`. Dispatch requisitions (E-kimi-task-*) and repair tickets sit apart.

Usage: partition-tree.py GRAPH_JSON OUT_JSON
Written by claude-16 (~/notes/pattern-global/auto/partition_tree.py), moved into futon3c/scripts on 2026-11-02 so the
published page rests on committed code; status is read fresh from each item's file; the graph snapshot supplies the
paths. Still read from ~/notes (advisory, uncommitted): the commons-map placements-[12]*.tsv for rows no ant has read.
Committed inputs: ant-reads/A1-placements.tsv, decisions-2026-11-02.json."""
import csv,json,os,re,glob,collections,sys
H=os.path.expanduser('~/')
A=H+'code/futon3c/holes/excursions/ant-reads/A1-placements.tsv'
reader={r['id']:r for r in csv.DictReader(open(A),delimiter='\t')} if os.path.exists(A) else {}
early={}
for f in glob.glob(H+'notes/commons-map/placements-[12]*.tsv'):
    for r in csv.DictReader(open(f),delimiter='\t'): early[r['id']]=r
D=json.load(open(H+'code/futon3c/holes/excursions/decisions-2026-11-02.json'))['decisions']
# status is read fresh from the files, so new items appear; pass the graph snapshot as argv[1]
G=sys.argv[1]
OUT=sys.argv[2] if len(sys.argv)>2 else 'partition-tree.json'
paths={}
for i in json.load(open(G))['tickets']['items']:
    if i['stem'] not in paths or re.match(r'/home/joe/code/futon[0-9a-z]*/',i['path']): paths[i['stem']]=i['path']
R={};cl={}
for s_,p_ in paths.items():
    try: t=open(p_,errors='replace').read()[:1500]
    except OSError: continue
    m=re.search(r'(?im)^\W*status\W*:?\W*(.{0,110})',t);st=m.group(1).strip() if m else ''
    hits=[(x.start(),x.group(0).upper()) for x in re.finditer(r'\b(IDENTIFY|MAP|DERIVE|ARGUE|VERIFY|INSTANTIATE|DOCUMENT|HEAD|CLOSED|DONE|COMPLETED?|HELD)\b',st,re.I)]
    ph=min(hits)[1] if hits else ''
    R[s_]={}
    if re.match(r'\W*(stopped|superseded|abandoned|retired|archived|dropped|obsolete|withdrawn|merged)\b',st,re.I): cl[s_]='stopped'
    elif ph.startswith(('CLOSED','DONE','COMPLETE')): cl[s_]='finished'
    else: cl[s_]='open'
CL={'superpod work':['M-mark8','M-paper-reverse-morphogenesis','M-superpod-mark3','M-superpod-mark2'],'substrate-2':['E-clean-up-substrate-2','M-populate-substrate-2']}
inv={m:n for n,ms in CL.items() for m in ms}
T={};flat=collections.Counter()
def put(path,leaf):
    n=T
    for p in path: n=n.setdefault(p,{})
    n.setdefault('_items',[]).append(leaf)
for s in R:
    r=reader.get(s);e=early.get(s);ways=[]
    st=cl[s]
    d=D.get(s,{}).get('decision')
    if d: st='decided: '+d.split(':')[0]
    if s.startswith('E-kimi-task'): put(['requisitions (history, not tickets)'],s);flat['req']+=1;continue
    if s.startswith('T-repair-occ'): put([st,'repair tickets (one class)'],s);flat['rep']+=1;continue
    if r:
        ways=[w for w in re.split(r'[,;|\s]+',r['way']) if w and w!='none'];top=ways[0] if ways else 'none: outside the ways'
    elif e and e.get('wing') not in(None,'','unsure'): top='wing '+e['wing']
    else: top='unplaced';flat['unplaced '+st]+=1
    leaf={'id':s}
    if ways[1:]: leaf['also']=ways[1:]
    put([st,top]+([inv[s]] if s in inv else []),leaf)
def cnt(n): return len(n.get('_items',[]))+sum(cnt(v) for k,v in n.items() if k!='_items')
json.dump(T,open(OUT,'w'),ensure_ascii=False,indent=0)
print(len(R),dict(flat),'reader rows',len(reader),'->',OUT,file=sys.stderr)
for k in sorted(T,key=lambda k:-cnt(T[k])):
    print(k,cnt(T[k]),{a:cnt(b) for a,b in sorted(T[k].items(),key=lambda x:-cnt(x[1]) if x[0]!='_items' else 0) if a!='_items'},file=sys.stderr)
