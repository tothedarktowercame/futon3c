#!/usr/bin/env python3
"""Freeze live sessions, ledger record fields, and pace compute-ports replay."""

import argparse, csv, importlib.util, json, pathlib, time, urllib.request
from collections import defaultdict

HERE = pathlib.Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("pre", HERE / "reply_text_backfill_pre_1004.py")
pre = importlib.util.module_from_spec(spec); spec.loader.exec_module(pre)
bf = pre.bf
RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
FIELDS = ["record_id", "session_id", "agents", "created_at", "still_open_before",
          "acts_before", "standings_before", "obligations_before", "ports_status_before",
          "session_tail_id", "session_tail_open_before", "still_open_after", "acts_after",
          "standings_after", "obligations_after", "ports_status_after", "ports_state_after",
          "ports_reason_after", "session_tail_open_after", "result"]

def live_sessions():
    with urllib.request.urlopen("http://127.0.0.1:7070/api/alpha/agents", timeout=30) as response:
        agents = json.load(response)["agents"]
    out = defaultdict(list)
    for aid, agent in agents.items():
        sid = agent.get("agent/session-id") or agent.get("session-id")
        if sid: out[str(sid)].append(aid)
    return {sid: sorted(ids) for sid, ids in out.items()}

def records(sessions):
    out = []
    for path in RECORD_DIR.glob("turn-*.json"):
        if path.name.count(".") != 1: continue
        try: record = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError): continue
        sid = str(record.get("session_id") or "")
        status = record.get("analysis_status")
        settled = status in ("analyzed", "drafted") or (status == "declared" and record.get("origin") == "agent")
        if sid in sessions and settled:
            out.append((sid, str(record.get("created_at") or ""), path.stem, record))
    return sorted(out)

def open_count(record):
    ports = record.get("ports")
    return len(ports.get("still_open") or []) if isinstance(ports, dict) else ""

def present(record, key): return int(key in record)

def prepare(ledger, snapshot):
    sessions = live_sessions(); found = records(sessions); grouped = defaultdict(list)
    for item in found: grouped[item[0]].append(item)
    tails = {sid: items[-1] for sid, items in grouped.items()}
    rows = []
    for sid, created, rid, record in found:
        tail = tails[sid]
        rows.append({"record_id": rid, "session_id": sid, "agents": ",".join(sessions[sid]),
          "created_at": created, "still_open_before": open_count(record),
          "acts_before": present(record,"acts"), "standings_before": present(record,"standings"),
          "obligations_before": present(record,"obligations"), "ports_status_before": present(record,"ports_status"),
          "session_tail_id": tail[2], "session_tail_open_before": open_count(tail[3]),
          "still_open_after":"", "acts_after":"", "standings_after":"", "obligations_after":"",
          "ports_status_after":"", "ports_state_after":"", "ports_reason_after":"",
          "session_tail_open_after":"", "result":""})
    with ledger.open("w", newline="") as handle:
        writer=csv.DictWriter(handle,FIELDS,delimiter="\t",lineterminator="\n"); writer.writeheader(); writer.writerows(rows)
    snapshot.write_text(json.dumps({"all_live_sessions":sessions,
      "included":{sid:len(items) for sid,items in grouped.items()}},indent=2)+"\n")
    print(json.dumps({"live_sessions":len(sessions),"included_sessions":len(grouped),"records":len(rows),
      "sessions":sessions},indent=2))

def recompute(ledger, result_path):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t"))
    done=json.loads(result_path.read_text()) if result_path.exists() else []
    completed={item["record_id"] for item in done}
    for index,row in enumerate(rows,1):
        rid=row["record_id"]
        if rid in completed: continue
        started=time.monotonic(); receipt=pre.drawbridge(rid); record=bf.load_record(rid)
        status=record.get("ports_status") or {}
        done.append({"record_id":rid,"receipt":receipt,"state":status.get("state"),
                     "reason":status.get("reason"),"ports":isinstance(record.get("ports"),dict)})
        result_path.write_text(json.dumps(done,indent=2)+"\n")
        if index % 100 == 0: print(json.dumps({"progress":index,"total":len(rows)}),flush=True)
        time.sleep(max(0,1-(time.monotonic()-started)))
    print(json.dumps({"recomputed":len(done),"computed":sum(x["state"]=="computed" for x in done),
      "unknown":[x["record_id"] for x in done if x["state"]=="unknown"]},indent=2))

def finalize(ledger,result_path):
    rows=list(csv.DictReader(ledger.open(),delimiter="\t")); results={x["record_id"]:x for x in json.loads(result_path.read_text())}
    tails={}
    for row in rows:
        record=bf.load_record(row["record_id"]); status=record.get("ports_status") or {}
        row.update({"still_open_after":open_count(record),"acts_after":present(record,"acts"),
          "standings_after":present(record,"standings"),"obligations_after":present(record,"obligations"),
          "ports_status_after":present(record,"ports_status"),"ports_state_after":status.get("state", ""),
          "ports_reason_after":status.get("reason") or "", "result":results[row["record_id"]]["state"] or "missing"})
        tid=row["session_tail_id"]
        if tid not in tails: tails[tid]=open_count(bf.load_record(tid))
        row["session_tail_open_after"]=tails[tid]
    with ledger.open("w",newline="") as handle:
        writer=csv.DictWriter(handle,FIELDS,delimiter="\t",lineterminator="\n"); writer.writeheader(); writer.writerows(rows)
    before={r["session_id"]:r["session_tail_open_before"] for r in rows}; after={r["session_id"]:r["session_tail_open_after"] for r in rows}
    fields={key:sum(int(r[key]) for r in rows) for key in ("acts_after","standings_after","obligations_after","ports_status_after")}
    changed=[{"record_id":r["record_id"],"before":int(r["still_open_before"]),"after":int(r["still_open_after"])}
             for r in rows if r["still_open_before"] != "" and r["still_open_after"] != ""
             and int(r["still_open_before"]) != int(r["still_open_after"])]
    print(json.dumps({"records":len(rows),"sessions":len(before),"tail_before":sum(int(v or 0) for v in before.values()),
      "tail_after":sum(int(v or 0) for v in after.values()),"fields":fields,"changed":changed,
      "unknown":[r["record_id"] for r in rows if r["ports_state_after"]=="unknown"]},indent=2))

def main():
    parser=argparse.ArgumentParser(); subs=parser.add_subparsers(dest="command",required=True)
    for command in ("prepare","recompute","finalize"):
        child=subs.add_parser(command); child.add_argument("ledger",type=pathlib.Path); child.add_argument("aux",type=pathlib.Path)
    args=parser.parse_args(); globals()[args.command](args.ledger,args.aux)

if __name__ == "__main__": main()
