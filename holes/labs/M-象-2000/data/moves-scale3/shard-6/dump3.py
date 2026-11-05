import json, sys
for r in sys.argv[1:]:
    d = json.load(open(f"input/{r}.input.json"))
    print("==", r, d.get("mission_id"), d["turn_id"], d["created_at"], "reply_acts:", len(d.get("reply_acts", [])))
    for f in d["fragments"]:
        print("FRAG", f["id"], f["intent"], "|", f["text"][:130])
    for p in d.get("open_ports", [])[:12]:
        print("PORT", p["act"], p.get("kind"), "|", p["text"][:60], "since", p.get("since", "")[:10])
    for a in d.get("reply_acts", []):
        print("REPLY", a["id"], "|", a["text"][:200].replace("\n", " ¶ "))
    for a in d.get("previous_reply_acts", [])[:4]:
        print("PREV", a["id"], "|", a["text"][:60].replace("\n", " "))
