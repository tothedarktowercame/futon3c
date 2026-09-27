import json, sys, time, urllib.request, urllib.parse, datetime as dt
B='http://localhost:7073/api/alpha/evidence'
def get(u):
    r=urllib.request.Request(u,headers={'Accept':'application/json'}); return json.load(urllib.request.urlopen(r,timeout=120))
ids={'retrieval':'e-9c5a5211-25c5-42a5-984c-d0e5d48bac5b','turn-commits':'emacs-19af606bf41e8e62417c17410be3ab8c'}
docs={k:get(f'{B}/{v}') for k,v in ids.items()}
def visible(k,T):
    d=docs[k]; q={'author':d['evidence/author'],'since':d['evidence/at'],'limit':1000,'system-as-of':T}
    if d.get('evidence/session-id'): q['session-id']=d['evidence/session-id']
    es=get(B+'?'+urllib.parse.urlencode(q)).get('entries',[])
    return any(e['evidence/id']==ids[k] for e in es)
iso=lambda t: t.strftime('%Y-%m-%dT%H:%M:%S.%f')[:-3]+'Z'
for k in ids:
    d=docs[k]; print(k, 'author',d['evidence/author'],'session',d.get('evidence/session-id'),'at',d['evidence/at'])
    lo=dt.datetime(2026,9,24,16,0,tzinfo=dt.timezone.utc); hi=dt.datetime(2026,9,27,22,0,tzinfo=dt.timezone.utc)
    assert not visible(k,iso(lo)) and visible(k,iso(hi)), 'bracket'
    while (hi-lo).total_seconds()>0.002:
        mid=lo+(hi-lo)/2
        if visible(k,iso(mid)): hi=mid
        else: lo=mid
    print(k,'first visible at system time between',iso(lo),'and',iso(hi)); sys.stdout.flush()
