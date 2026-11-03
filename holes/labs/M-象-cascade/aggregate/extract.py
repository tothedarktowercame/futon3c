#!/usr/bin/env python3
"""Collect library patterns and agent proposals as text, with the turn each proposal came from."""
import glob, json, os, re
LIB='/home/joe/code/futon3/library/'
R=os.path.expanduser('~/.emacs-graph/session-turn-analysis')
items=[]
for f in glob.glob(LIB+'**/*.flexiarg',recursive=True):
    pid=f[len(LIB):-9]
    if pid.split('/')[0] in ('iiching','iching'): continue   # divinatory hexagrams, not candidate parents of a move
    t=open(f,errors='ignore').read()
    title=(re.search(r'^@title\s+(.*)',t,re.M) or [0,pid])[1]
    body=' '.join(l.strip() for l in t.splitlines() if not l.startswith('@') and l.strip())
    items.append(dict(id=pid,kind='library',text=f'{title}. {body}'[:1200]))
props={};bad=0
for p in glob.glob(R+'/*.candidates.json'):
    turn=os.path.basename(p).split('.')[0]
    try: d=json.load(open(p))
    except Exception: bad+=1; continue
    for c in d.get('candidates',[]):
        pid=c.get('id'); 
        if not pid: continue
        txt=' '.join(str(c.get(k,'')) for k in('title','context','if','however','then'))
        e=props.setdefault(pid,dict(id=pid,kind='proposal',text=txt[:1200],parent=c.get('parent'),turns=[]))
        e['turns'].append(turn)
items+=props.values()
json.dump(items,open('items.json','w'))
print(sum(i['kind']=='library' for i in items),len(props),'bad',bad)
