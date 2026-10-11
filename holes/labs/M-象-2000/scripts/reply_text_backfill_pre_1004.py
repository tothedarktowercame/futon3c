#!/usr/bin/env python3
"""Coverage, Agency backfill, and live port replay for pre-2026-10-04 turns."""

import argparse, csv, datetime as dt, hashlib, importlib.util, json, pathlib, sys, time, urllib.request
from collections import Counter, defaultdict

HERE = pathlib.Path(__file__).resolve().parent
def load_module(name):
    spec = importlib.util.spec_from_file_location(name, HERE / (name + ".py"))
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    return module
cov = load_module("reply_evidence_coverage")
bf = load_module("reply_text_backfill")

CUTOFF = "2026-10-04T00:00:00Z"
RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
FIELDS = ["record_id", "turn_id", "session_id", "created_at", "agent", "join_used",
          "reply_evidence_id", "unified_turn_id", "reply_chars", "commits_json",
          "still_open_before", "session_tail_id", "session_tail_open_before",
          "reply_sha256", "still_open_after", "session_tail_open_after", "readback"]
TARGETS = {"564c8e50-c240-46fc-81ad-55afafbc63ee", "9ef8e479-e1b7-4755-894d-1264cacb6c3b"}

def all_evidence():
    params={"tags":"chat,turn","since":"2026-09-22T00:00:00Z","limit":"500"}; out=[]
    while True:
        page=cov.get_json(cov.BASE+"?"+cov.urllib.parse.urlencode(params),timeout=60,attempts=10)
        out.extend(page.get("entries",[])); cursor=page.get("next-cursor")
        if not cursor: return out
        params.update({"cursor-at":cursor["at"],"cursor-id":cursor["id"]})


def records():
    out = []
    for path in cov.record_files():
        try: r = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError): continue
        if r.get("analysis_status") == "analyzed" and (r.get("origin") or "operator") == "operator" \
                and str(r.get("created_at") or "") < CUTOFF:
            out.append((path.stem, r))
    return sorted(out, key=lambda x: (str(x[1].get("created_at") or ""), x[0]))


def coverage(output):
    rs = records()
    cov.START = min(str(r.get("created_at")) for _, r in rs)
    entries = all_evidence()
    children, sessions = defaultdict(list), defaultdict(list)
    for e in entries:
        if e.get("evidence/in-reply-to"): children[str(e["evidence/in-reply-to"])].append(e)
        if e.get("evidence/session-id"): sessions[str(e["evidence/session-id"])].append(e)
    for xs in children.values(): xs.sort(key=lambda e: (cov.at(e), cov.eid(e) or ""))
    for xs in sessions.values(): xs.sort(key=lambda e: (cov.at(e), cov.eid(e) or ""))
    times = defaultdict(list)
    for _, r in rs:
        if r.get("session_id") and r.get("created_at"): times[str(r["session_id"])].append(str(r["created_at"]))
    for xs in times.values(): xs.sort()
    rows=[]
    for rid,r in rs:
        evid=str(r.get("evidence_id") or "")
        chains=[e for e in children.get(evid,[]) if cov.is_assistant(e)]
        if evid and not chains:
            for mid in children.get(evid,[]): chains += [e for e in children.get(cov.eid(mid),[]) if cov.is_assistant(e)]
        chains=list({cov.eid(e):e for e in chains}.values())
        sid=str(r.get("session_id") or ""); created=str(r.get("created_at") or "")
        boundary=next((x for x in times[sid] if x>created),"9999")
        tc=sorted([e for e in sessions.get(sid,[]) if cov.is_assistant(e) and created<cov.at(e)<boundary], key=lambda e:(cov.at(e),cov.eid(e) or ""))
        ch=chains[0] if len(chains)==1 else None; tm=tc[0] if tc else None
        disagree=bool(ch and tm and cov.reply_key(ch)!=cov.reply_key(tm))
        if disagree: outcome="disagree"; choice=None
        elif ch: outcome="join-1"; choice=ch
        elif len(chains)>1: outcome="ambiguous-join-1"; choice=None
        elif tm: outcome="join-2"; choice=tm
        else: outcome="none"; choice=None
        rows.append({"record_id":rid,"turn_id":r.get("turn_id"),"session_id":sid,"created_at":created,
          "agent":r.get("agent") or r.get("agent_id") or "unknown","record_evidence_id":evid or None,
          "existing_reply":bool(str(r.get("reply_text") or "").strip()),"outcome":outcome,
          "reply_evidence_id":cov.eid(choice) if choice else None,
          "chain_evidence_id":cov.eid(ch) if ch else None,"time_evidence_id":cov.eid(tm) if tm else None})
    result={"read_at":dt.datetime.now(dt.timezone.utc).isoformat().replace("+00:00","Z"),
      "population":len(rows),"earliest":rows[0]["created_at"],"existing_reply":sum(r["existing_reply"] for r in rows),
      "with_evidence_id":sum(bool(r["record_evidence_id"]) for r in rows),"outcomes":dict(Counter(r["outcome"] for r in rows)),
      "by_agent":{k:dict(Counter(x["outcome"] for x in rows if x["agent"]==k)) for k in sorted({x["agent"] for x in rows})},
      "by_day":{k:dict(Counter(x["outcome"] for x in rows if x["created_at"][:10]==k)) for k in sorted({x["created_at"][:10] for x in rows})},"rows":rows}
    output.write_text(json.dumps(result,ensure_ascii=False,indent=2)+"\n")
    print(json.dumps({k:v for k,v in result.items() if k!="rows"},indent=2))


def tail(sid):
    xs=bf.all_session_records(sid); return xs[-1] if xs else ("","",{})


def prepare(coverage_path, ledger):
    data=json.loads(coverage_path.read_text()); selected=[x for x in data["rows"] if x["session_id"] in TARGETS and x["outcome"] in ("join-1","join-2") and not x["existing_reply"]]
    bf.evidence_pages=all_evidence
    by_id,groups=bf.evidence_index(); tails={sid:tail(sid) for sid in {x["session_id"] for x in selected}}
    out=[]
    for x in sorted(selected,key=lambda x:(x["session_id"],x["created_at"],x["record_id"])):
        r=bf.load_record(x["record_id"]); e=by_id[x["reply_evidence_id"]]; text,unified=bf.reply_text(e,groups)
        if not text: raise RuntimeError("empty reply "+x["record_id"])
        _,tid,tr=tails[x["session_id"]]
        out.append({"record_id":x["record_id"],"turn_id":x["turn_id"],"session_id":x["session_id"],"created_at":x["created_at"],"agent":x["agent"],"join_used":x["outcome"],"reply_evidence_id":x["reply_evidence_id"],"unified_turn_id":unified,"reply_chars":len(text),"commits_json":json.dumps(bf.parse_commits(r.get("happened_summary")),separators=(",",":")),"still_open_before":bf.open_count(r),"session_tail_id":tid,"session_tail_open_before":bf.open_count(tr),"reply_sha256":hashlib.sha256(text.encode()).hexdigest(),"still_open_after":"","session_tail_open_after":"","readback":""})
    with ledger.open("w",newline="") as f: w=csv.DictWriter(f,FIELDS,delimiter="\t",lineterminator="\n");w.writeheader();w.writerows(out)
    print(json.dumps({"prepared":len(out),"sessions":len(tails)}))


def apply(ledger,result):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t")); bf.evidence_pages=all_evidence; by_id,groups=bf.evidence_index(); done=[]
    for i,x in enumerate(rows):
        text,_=bf.reply_text(by_id[x["reply_evidence_id"]],groups)
        status,response=bf.request_json(f"{bf.AGENCY}/{x['record_id']}/happened",method="POST",payload={"reply":text,"commits":json.loads(x["commits_json"])},attempts=1)
        if status!=200: result.write_text(json.dumps(done+[{"record_id":x["record_id"],"http":status,"response":response}],indent=2)); raise RuntimeError(f"HTTP {status} {x['record_id']}")
        view=bf.request_json(f"{bf.AGENCY}/{x['record_id']}")[1].get("record") or {}
        ok=view.get("reply_text")==text
        done.append({"record_id":x["record_id"],"http":status,"readback":ok})
        result.write_text(json.dumps(done,indent=2)+"\n")
        if i+1<len(rows): time.sleep(1)
    print(json.dumps({"applied":len(done),"readback_ok":sum(x["readback"] for x in done)}))

def drawbridge(record_id):
    token=pathlib.Path(".admintoken").read_text().strip()
    code=("(let [svc (deref (var-get (ns-resolve 'futon3c.transport.http '!xiang-turn-service))) "
          "compute (requiring-resolve 'futon3c.xiang.turn-service/compute-ports!)] "
          f"(compute svc \"{record_id}\" {{:session-cap 10000}}) {{:ok true}})")
    req=urllib.request.Request("http://127.0.0.1:6768/eval",data=code.encode(),method="POST",headers={"x-admin-token":token,"Content-Type":"text/plain"})
    with urllib.request.urlopen(req,timeout=120) as response: return response.read().decode()

def recompute(result):
    rs=[]
    for sid in TARGETS:
        rs.extend((sid,created,rid) for created,rid,_ in bf.all_session_records(sid))
    rs.sort()
    done=json.loads(result.read_text()) if result.exists() else []
    completed={x["record_id"] for x in done}
    for i,(_,_,rid) in enumerate(rs):
        if rid in completed: continue
        receipt=drawbridge(rid); r=bf.load_record(rid); ports=r.get("ports")
        done.append({"record_id":rid,"receipt":receipt,"ports":isinstance(ports,dict),"still_open":bf.open_count(r)})
        result.write_text(json.dumps(done,indent=2)+"\n")
        time.sleep(1)
    print(json.dumps({"recomputed":len(done),"ports":sum(x["ports"] for x in done),"refused":[x["record_id"] for x in done if not x["ports"]]}))

def finalize(ledger):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t")); tails={}
    for x in rows:
        r=bf.load_record(x["record_id"]); x["still_open_after"]=bf.open_count(r); x["readback"]="ok" if str(r.get("reply_text") or "").strip() else "failed"
        sid=x["session_id"]
        if sid not in tails:
            _,tid,tr=tail(sid); tails[sid]=(tid,bf.open_count(tr))
        x["session_tail_open_after"]=tails[sid][1]
    with ledger.open("w",newline="") as f:w=csv.DictWriter(f,FIELDS,delimiter="\t",lineterminator="\n");w.writeheader();w.writerows(rows)
    before={}; after={}
    for x in rows: before[x["session_id"]]=int(x["session_tail_open_before"]);after[x["session_id"]]=int(x["session_tail_open_after"])
    print(json.dumps({"records":len(rows),"readback_ok":sum(x["readback"]=="ok" for x in rows),"tail_before":before,"tail_after":after,"total_before":sum(before.values()),"total_after":sum(after.values())},indent=2))


def main():
    p=argparse.ArgumentParser(); s=p.add_subparsers(dest="cmd",required=True)
    for name,nargs in (("coverage",1),("prepare",2),("apply",2),("recompute",1),("finalize",1)):
        q=s.add_parser(name)
        for i in range(nargs): q.add_argument("paths"+str(i),type=pathlib.Path)
    a=p.parse_args(); ps=[getattr(a,"paths"+str(i)) for i in range(2) if hasattr(a,"paths"+str(i))]
    {"coverage":coverage,"prepare":prepare,"apply":apply,"recompute":recompute,"finalize":finalize}[a.cmd](*ps)

if __name__=="__main__": main()
