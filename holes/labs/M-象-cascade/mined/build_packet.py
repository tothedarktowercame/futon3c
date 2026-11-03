"""Packet of Joe's own correcting and directing turns, for a reading agent. Train before 10-26, hold out after."""
import glob,json,os,collections,re
R=os.path.expanduser('~/.emacs-graph/session-turn-analysis/')
KEEP={'report-problem','redirect','constrain','disagree','retract','qualify'}
rows=[]
for f in glob.glob(R+'turn-*.json'):
    b=os.path.basename(f)
    if b.count('.')!=1: continue
    try:d=json.load(open(f)); a=json.load(open(f+'.analysis.json'))
    except Exception: continue
    if d.get('origin')!='operator': continue
    t=(d.get('source_text') or '').strip()
    if not 60<=len(t)<=900 or t.startswith(('#','---','claude-','Agent:')): continue
    ints=set(re.findall(r'"intent": "([a-z-]+)"',json.dumps(a)))
    if not ints&KEEP: continue
    rows.append(dict(id=d['turn_id'],at=str(d.get('created_at'))[:16],s=d['session_id'][:8],m=d.get('mission_id'),i=sorted(ints&KEEP),t=t.replace('\n',' ')))
print(len(rows),collections.Counter(r['at'][:7] for r in rows).most_common(8))
rows.sort(key=lambda r:r['at'])
tr=[r for r in rows if '2026-10-01'<=r['at']<'2026-10-26']; ho=[r for r in rows if '2026-10-26'<=r['at']<'2026-11-02']
print(len(tr),len(ho))
def w(n,rs):
    by=collections.defaultdict(list)
    for r in rs: by[r['s']].append(r)
    with open(n,'w') as f:
        for s,x in by.items():
            f.write(f'## session {s}\n')
            for r in x: f.write(f"[{r['id']}] {r['at']} mission={r['m']} {','.join(r['i'])}\n{r['t']}\n\n")
import random; random.seed(20261103)
def cap(rs,n):
    if len(rs)<=n: return rs
    return sorted(random.sample(rs,n),key=lambda r:r['at'])
w('train.md',cap(tr,320)); w('heldout.md',cap(ho,140))
