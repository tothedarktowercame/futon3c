#!/usr/bin/env python3
"""Ledger and paced live compute-ports replay for Agency's current sessions."""

import argparse, csv, importlib.util, json, pathlib, time, urllib.request
from collections import defaultdict

HERE = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("pre", HERE / "reply_text_backfill_pre_1004.py")
pre = importlib.util.module_from_spec(spec); spec.loader.exec_module(pre)
bf = pre.bf
RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
FIELDS = ["record_id", "session_id", "agents", "created_at", "still_open_before",
          "agent_verify_before", "session_tail_id", "session_tail_open_before",
          "session_tail_agent_verify_before", "still_open_after", "agent_verify_after",
          "session_tail_open_after", "session_tail_agent_verify_after", "ports_result"]

def live_sessions():
    with urllib.request.urlopen("http://127.0.0.1:7070/api/alpha/agents", timeout=30) as response:
        agents=json.load(response)["agents"]
    out=defaultdict(list)
    for aid,a in agents.items():
        sid=a.get("agent/session-id") or a.get("session-id")
        if sid: out[str(sid)].append(aid)
    return {sid:sorted(xs) for sid,xs in out.items()}

def settled(sessions):
    out=[]
    for path in RECORD_DIR.glob("turn-*.json"):
        if path.name.count(".") != 1: continue
        try:r=json.loads(path.read_text())
        except (OSError,json.JSONDecodeError):continue
        sid=str(r.get("session_id") or ""); status=r.get("analysis_status")
        ok=status in ("analyzed","drafted") or (status=="declared" and r.get("origin")=="agent")
        if sid in sessions and ok:out.append((sid,str(r.get("created_at") or ""),path.stem,r))
    return sorted(out)

def counts(record):
    ports=record.get("ports")
    if not isinstance(ports,dict):return "",""
    opened=ports.get("still_open") or []
    av=sum("-r-" in str(x.get("act") or "") and x.get("kind")=="verify" for x in opened)
    return len(opened),av

def prepare(ledger, snapshot):
    sessions=live_sessions(); rows=settled(sessions); by=defaultdict(list)
    for x in rows:by[x[0]].append(x)
    tails={sid:xs[-1] for sid,xs in by.items()}
    output=[]
    for sid,created,rid,r in rows:
        _,_,tid,tr=tails[sid]; before,av=counts(r); tb,tav=counts(tr)
        output.append({"record_id":rid,"session_id":sid,"agents":",".join(sessions[sid]),"created_at":created,
          "still_open_before":before,"agent_verify_before":av,"session_tail_id":tid,
          "session_tail_open_before":tb,"session_tail_agent_verify_before":tav,
          "still_open_after":"","agent_verify_after":"","session_tail_open_after":"",
          "session_tail_agent_verify_after":"","ports_result":""})
    with ledger.open("w",newline="") as f:w=csv.DictWriter(f,FIELDS,delimiter="\t",lineterminator="\n");w.writeheader();w.writerows(output)
    snapshot.write_text(json.dumps({"all_live_sessions":sessions,"included":{sid:len(xs) for sid,xs in by.items()}},indent=2)+"\n")
    print(json.dumps({"live_sessions":len(sessions),"included_sessions":len(by),"records":len(rows)}))

def recompute(ledger, result):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t")); done=json.loads(result.read_text()) if result.exists() else []
    completed={x["record_id"] for x in done}
    for row in rows:
        rid=row["record_id"]
        if rid in completed:continue
        started=time.monotonic()
        receipt=pre.drawbridge(rid); record=bf.load_record(rid); opened,av=counts(record)
        done.append({"record_id":rid,"receipt":receipt,"ports":opened!="","still_open":opened,"agent_verify":av})
        result.write_text(json.dumps(done,indent=2)+"\n")
        time.sleep(max(0,1-(time.monotonic()-started)))
    print(json.dumps({"recomputed":len(done),"ports":sum(x["ports"] for x in done),
                      "refused":[x["record_id"] for x in done if not x["ports"]]}))

def finalize(ledger,result):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t")); results={x["record_id"]:x for x in json.loads(result.read_text())}
    tail_after={}
    for row in rows:
        r=bf.load_record(row["record_id"]); opened,av=counts(r); row["still_open_after"]=opened;row["agent_verify_after"]=av
        row["ports_result"]="ok" if results.get(row["record_id"],{}).get("ports") else "refused"
        tid=row["session_tail_id"]
        if tid not in tail_after:tail_after[tid]=counts(bf.load_record(tid))
        row["session_tail_open_after"],row["session_tail_agent_verify_after"]=tail_after[tid]
    with ledger.open("w",newline="") as f:w=csv.DictWriter(f,FIELDS,delimiter="\t",lineterminator="\n");w.writeheader();w.writerows(rows)
    before={};after={};grew=[]
    for r in rows:
        before[r["session_id"]]=(r["session_tail_open_before"],r["session_tail_agent_verify_before"])
        after[r["session_id"]]=(r["session_tail_open_after"],r["session_tail_agent_verify_after"])
        if r["still_open_before"]!="" and r["still_open_after"]!="" and int(r["still_open_after"])>int(r["still_open_before"]):grew.append(r["record_id"])
    num=lambda x:sum(int(v[0] or 0) for v in x.values())
    print(json.dumps({"records":len(rows),"sessions":len(before),"tail_before":num(before),"tail_after":num(after),
      "tail_agent_verify_before":sum(int(v[1] or 0) for v in before.values()),
      "tail_agent_verify_after":sum(int(v[1] or 0) for v in after.values()),"grew":grew,
      "refused":[r["record_id"] for r in rows if r["ports_result"]=="refused"]},indent=2))

def main():
    p=argparse.ArgumentParser();s=p.add_subparsers(dest="cmd",required=True)
    for name,n in (("prepare",2),("recompute",2),("finalize",2)):
        q=s.add_parser(name)
        for i in range(n):q.add_argument("path"+str(i),type=pathlib.Path)
    a=p.parse_args();paths=[getattr(a,"path"+str(i)) for i in range(2)]
    {"prepare":prepare,"recompute":recompute,"finalize":finalize}[a.cmd](*paths)
if __name__=="__main__":main()
