#!/usr/bin/env python3
"""Freeze current Agency sessions and pace live compute-ports calls."""

import argparse, csv, importlib.util, json, pathlib, time, urllib.request
from collections import defaultdict

HERE = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("pre", HERE / "reply_text_backfill_pre_1004.py")
pre = importlib.util.module_from_spec(spec); spec.loader.exec_module(pre)
bf = pre.bf
RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
FIELDS = ["record_id", "session_id", "agents", "created_at", "still_open_before",
          "session_tail_id", "session_tail_open_before", "still_open_after",
          "session_tail_open_after", "ports_state_after", "ports_reason_after"]

def live_sessions():
    with urllib.request.urlopen("http://127.0.0.1:7070/api/alpha/agents", timeout=30) as response:
        agents = json.load(response)["agents"]
    result = defaultdict(list)
    for agent_id, agent in agents.items():
        session_id = agent.get("agent/session-id") or agent.get("session-id")
        if session_id: result[str(session_id)].append(agent_id)
    return {session_id: sorted(agent_ids) for session_id, agent_ids in result.items()}

def settled(sessions):
    found = []
    for path in RECORD_DIR.glob("turn-*.json"):
        if path.name.count(".") != 1: continue
        try: record = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError): continue
        session_id = str(record.get("session_id") or "")
        status = record.get("analysis_status")
        ok = status in ("analyzed", "drafted") or (status == "declared" and record.get("origin") in ("agent", "system"))
        if session_id in sessions and ok:
            found.append((session_id, str(record.get("created_at") or ""), path.stem, record))
    return sorted(found)

def open_count(record):
    ports = record.get("ports")
    return len(ports.get("still_open") or []) if isinstance(ports, dict) else ""

def prepare(ledger, snapshot):
    sessions = live_sessions(); records = settled(sessions); grouped = defaultdict(list)
    for item in records: grouped[item[0]].append(item)
    tails = {session_id: items[-1] for session_id, items in grouped.items()}
    rows = []
    for session_id, created_at, record_id, record in records:
        tail = tails[session_id]
        rows.append({"record_id":record_id, "session_id":session_id,
          "agents":",".join(sessions[session_id]), "created_at":created_at,
          "still_open_before":open_count(record), "session_tail_id":tail[2],
          "session_tail_open_before":open_count(tail[3]), "still_open_after":"",
          "session_tail_open_after":"", "ports_state_after":"", "ports_reason_after":""})
    with ledger.open("w", newline="") as handle:
        writer=csv.DictWriter(handle,FIELDS,delimiter="\t",lineterminator="\n"); writer.writeheader(); writer.writerows(rows)
    snapshot.write_text(json.dumps({"all_live_sessions":sessions,
      "included":{session_id:len(items) for session_id,items in grouped.items()}},indent=2)+"\n")
    print(json.dumps({"live_sessions":len(sessions),"included_sessions":len(grouped),"records":len(rows)},indent=2))

def recompute(ledger, checkpoint):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t"))
    done=json.loads(checkpoint.read_text()) if checkpoint.exists() else []
    completed={item["record_id"] for item in done}
    for index,row in enumerate(rows,1):
        record_id=row["record_id"]
        if record_id in completed: continue
        started=time.monotonic(); receipt=pre.drawbridge(record_id); record=bf.load_record(record_id)
        status=record.get("ports_status") or {}
        done.append({"record_id":record_id,"receipt":receipt,"state":status.get("state"),"reason":status.get("reason")})
        checkpoint.write_text(json.dumps(done,indent=2)+"\n")
        if index % 100 == 0: print(json.dumps({"progress":index,"total":len(rows)}),flush=True)
        time.sleep(max(0,1-(time.monotonic()-started)))
    print(json.dumps({"recomputed":len(done),"computed":sum(x["state"]=="computed" for x in done),
      "unknown":[x for x in done if x["state"]=="unknown"]},indent=2))

def finalize(ledger, checkpoint):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t")); results={x["record_id"]:x for x in json.loads(checkpoint.read_text())}
    tails={}
    for row in rows:
        record=bf.load_record(row["record_id"]); status=record.get("ports_status") or {}
        row["still_open_after"]=open_count(record); row["ports_state_after"]=status.get("state","")
        row["ports_reason_after"]=status.get("reason") or "-"
        tail_id=row["session_tail_id"]
        if tail_id not in tails: tails[tail_id]=open_count(bf.load_record(tail_id))
        row["session_tail_open_after"]=tails[tail_id]
    with ledger.open("w",newline="") as handle:
        writer=csv.DictWriter(handle,FIELDS,delimiter="\t",lineterminator="\n"); writer.writeheader(); writer.writerows(rows)
    before={row["session_id"]:row["session_tail_open_before"] for row in rows}
    after={row["session_id"]:row["session_tail_open_after"] for row in rows}
    changed=[{"record_id":row["record_id"],"before":int(row["still_open_before"]),"after":int(row["still_open_after"])}
             for row in rows if row["still_open_before"] != "" and row["still_open_after"] != ""
             and int(row["still_open_before"]) != int(row["still_open_after"])]
    print(json.dumps({"records":len(rows),"sessions":len(before),
      "tail_before":sum(int(x or 0) for x in before.values()),"tail_after":sum(int(x or 0) for x in after.values()),
      "changed":changed,"unknown":[x for x in results.values() if x["state"]=="unknown"]},indent=2))

def main():
    parser=argparse.ArgumentParser(); sub=parser.add_subparsers(dest="command",required=True)
    for command in ("prepare","recompute","finalize"):
        child=sub.add_parser(command); child.add_argument("ledger",type=pathlib.Path); child.add_argument("aux",type=pathlib.Path)
    args=parser.parse_args(); globals()[args.command](args.ledger,args.aux)

if __name__ == "__main__": main()
