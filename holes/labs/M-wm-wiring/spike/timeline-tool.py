#!/usr/bin/env python3
"""Timeline of the third flight of M-autoclock-in, from the records: request
file mtimes (store), Agency job times (caller wm-flight, seat claude-5), click
endpoint, driver process. Every line UTC."""
import json, os, subprocess, datetime, glob, urllib.request, re
def iso(ts): return datetime.datetime.utcfromtimestamp(ts).strftime('%Y-%m-%dT%H:%M:%SZ')
start=open('/tmp/flightN/run-start.txt').read().strip()
rows=[]
rows.append((start,'driver','flight-driver M-autoclock-in --run started from /home/joe/code/futon2-flight2 (futon2 2e509a34), seat claude-5, max-clicks 1'))
plan_start=open('/tmp/flightN/plan-start.txt').read().strip()
rows.append((plan_start,'driver','dry-run plan started (no --run); flight id in plan: flight-74325007'))
store='/home/joe/code/futon2/data/wm-interpretations'
t0=os.path.getmtime('/tmp/flightN/run-start.txt')
for f in glob.glob(store+'/requests/request-*.edn'):
    m=os.path.getmtime(f)
    if m>=t0:
        s=open(f).read(); kind=re.search(r':kind :([a-z-]+)',s); want=re.search(r':want :(exit/[a-z0-9]+)',s)
        rows.append((iso(m),'store','request written '+os.path.basename(f)+' kind '+(kind.group(1) if kind else '?')+(' want '+want.group(1) if want else '')))
for f in glob.glob(store+'/flights/*.edn')+glob.glob(store+'/flights/enactments/*.edn')+[store+'/M-autoclock-in.edn']:
    if os.path.exists(f) and os.path.getmtime(f)>=t0: rows.append((iso(os.path.getmtime(f)),'store','written '+f.replace(store+'/','')))
try:
    d=json.load(urllib.request.urlopen('http://localhost:7070/api/alpha/invoke/jobs?limit=60'))
    js=d if isinstance(d,list) else d.get('jobs',[])
    for j in js:
        if j.get('caller')=='wm-flight' and j.get('created-at','')>=start:
            res=str(j.get('result') or '')
            schema=re.search(r':schema :([a-z/-]+[a-z0-9-]*)',res); decl=':decline' in res[:200]
            prompt=next((e.get('text','') for e in j.get('events',[]) if e.get('type')=='prompt'),'')
            rid=re.search(r'request-([0-9a-f]{16})', prompt)
            reused=''
            if rid:
                f=store+'/requests/request-'+rid.group(1)+'.edn'
                if os.path.exists(f):
                    reused=' request-'+rid.group(1)[:8]+(' (reused, issued '+iso(os.path.getmtime(f))+')' if os.path.getmtime(f)<t0 else ' (written this flight)')
                else: reused=' request-'+rid.group(1)[:8]+' (NO FILE)'
            rows.append((j['created-at'][:19]+'Z','agency','job '+j['job-id']+' to '+str(j.get('agent-id'))+' created'+reused))
            if j.get('finished-at'): rows.append((j['finished-at'][:19]+'Z','agency','job '+j['job-id'][-8:]+' '+j['state']+' reply '+(schema.group(1) if schema else '?')+(' DECLINE' if decl else '')+' ('+str(len(res))+' chars, '+str(res.count('```edn'))+' fenced)'))
except Exception as e: rows.append((iso(datetime.datetime.utcnow().timestamp()),'agency','jobs read failed: '+str(e)))
try:
    c=json.load(urllib.request.urlopen('http://localhost:7070/api/alpha/wm/click'))
    if str(c.get('started-at',''))>=start: rows.append((c['started-at'][:19]+'Z','click','click '+str(c.get('click-id'))+' running? '+str(c.get('running?'))+' phase '+str(c.get('phase'))))
except Exception as e: pass
for f in ['/tmp/flightN/run-end.txt']:
    if os.path.exists(f): rows.append((open(f).read().strip(),'driver','flight-driver process ended: '+open('/tmp/flightN/run.txt').read().strip().split('\n')[-1]))
for f in glob.glob('/home/joe/code/futon2/data/wm-runs/tick-run-record-*flight-74325007*.edn'):
    rows.append((iso(os.path.getmtime(f)),'server','run record written '+os.path.basename(f)))
rows.sort()
print('# Third flight of M-autoclock-in (flight-74325007; the dry-run plan named flight-74325007) — timeline, UTC, generated '+iso(datetime.datetime.utcnow().timestamp())+' by /tmp/flightN/timeline.py from the store, the Agency job list, the click endpoint and the driver process')
print('# operator: Joe, 2026-09-25: "OK to M-auto-clock in but can you please monitor it directly"; "keep meticulous records with timestamps"; flown by claude-8')
print()
for t,src,msg in rows: print(t, src.ljust(7), msg)
