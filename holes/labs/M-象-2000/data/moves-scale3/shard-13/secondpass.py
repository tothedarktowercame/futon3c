#!/usr/bin/env python3
"""Second pass: add uncovered acts, unlabelled entries, and within/opens links."""
import json, glob

def load(rec):
    sc=json.load(open(f"{rec}.moves.json"))
    inp=json.load(open(f"input/{rec}.input.json"))
    return sc, inp

def act(sc, aid):
    for a in sc["acts"]:
        if a["act"]==aid: return a
    return None

def set_scope(a, opens=None, within=None, closes=None):
    s=a["scope"]
    if opens is not None: s["opens"]=opens
    if within is not None: s["within"]=within
    if closes is not None: s["closes"]=closes

def add(sc, entry):
    sc["acts"].append(entry)

def unl(sc, aids):
    sc.setdefault("unlabelled",[])
    sc["unlabelled"].extend(aids)

def save(rec, sc):
    with open(f"{rec}.moves.json","w",encoding="utf-8") as f:
        json.dump(sc,f,ensure_ascii=False,indent=1)

# ---- turn-3vqBlD: cover all 19 remaining fragments ----
sc, inp = load("turn-3vqBlD")
F="claude-17-turn-255-f-"
# stubs to unlabelled
unl(sc, [F+"s5-0", F+"s10-0", F+"s20-0"])  # kernel-split "**1."-style stubs, content in numbered siblings
add(sc, {"act": F+"s2-0", "kind": "report-problem", "move": "report-problem/relay",
  "quote": "The commits don't name the owning agent.", "answers": [],
  "scope": {"opens": None, "closes": None, "within": F+"s1-0"}, "confidence": "high",
  "note": "Joe quoting claude-3's comment back; already labelled in first pass? check"})
add(sc, {"act": F+"s3-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "Its last commit was at 04:26, so it may still be working", "answers": [],
  "scope": {"opens": None, "closes": None, "within": F+"s2-0"}, "confidence": "high"})
add(sc, {"act": F+"s4-0", "kind": "propose", "move": "propose/next-step",
  "quote": "Here's what I'd suggest to them, or to you.", "answers": [],
  "scope": {"opens": "claude-3-comment-response", "closes": None, "within": F+"s1-0"}, "confidence": "high"})
for fid, mv, q, w in [
  ("s9-0","propose/directive","`export` and `page` should refuse to run without the scanner",F+"s6-0"),
  ("s11-0","propose/design","They fit together as the free sample.",F+"s4-0"),
  ("s12-0","explain/mechanism","- `xiaoxiang.py` with an exported model: what kinds of requests you make, by intent.",F+"s11-0"),
  ("s13-0","explain/mechanism","The shipped file is the filtered vocabulary",F+"s11-0"),
  ("s14-0","explain/mechanism","- The gap bars: what your agents did while you weren't watching.",F+"s11-0"),
  ("s15-0","qualify/caveat","This part isn't written yet.",F+"s11-0"),
  ("s16-0","explain/mechanism","All three read the same local logs.",F+"s11-0"),
  ("s17-0","report-problem/observed","The missing piece is a reader for someone else's raw logs",F+"s11-0"),
  ("s18-0","explain/context","小象 currently reads 象's labelled records",F+"s17-0"),
  ("s19-0","constrain/standing-rule","That reader should redact with `secret_scan` before anything is classified",F+"s17-0"),
  ("s21-0","explain/observation","One scanner, two consumers.**",F+"s6-0"),
  ("s22-0","explain/context","Before today's fixes, the capture path would have redacted paths",F+"s21-0"),
  ("s23-0","constrain/standing-rule","the regression tests in `test_secret_scan.py` are what protect both users",F+"s21-0"),
]:
    kind = "constrain" if mv.startswith("constrain") else ("report-problem" if mv.startswith("report-problem") else ("propose" if mv.startswith("propose") else ("qualify" if mv.startswith("qualify") else "explain")))
    add(sc, {"act": F+fid, "kind": kind, "move": mv, "quote": q, "answers": [],
             "scope": {"opens": None, "closes": None, "within": w}, "confidence": "medium"})
# fix the first-pass duplicate: s2-0 was already labelled — dedupe
seen=set(); out=[]
for a in sc["acts"]:
    if a["act"] in seen: continue
    seen.add(a["act"]); out.append(a)
sc["acts"]=out
save("turn-3vqBlD", sc)

# ---- turn-zngcGd: cover r-1..r-5 ----
sc, inp = load("turn-zngcGd")
R="claude-14-turn-69-r-"
add(sc, {"act": R+"1", "kind": "report", "move": "report/findings",
  "quote": "the five surface problems", "answers": [{"act": "claude-14-turn-69-f-s1-0", "quote": "Every source says \"a closed orientable surface of genus g\"", "effect": "closes"}],
  "scope": {"opens": None, "closes": None, "within": R+"0"}, "confidence": "high"})
add(sc, {"act": R+"2", "kind": "qualify", "move": "qualify/caveat",
  "quote": "caveat, t00A02(a)", "answers": [{"act": "claude-14-turn-69-f-s1-0", "quote": "The revision fixes the base surface, but the covers stay arbitrary", "effect": "keeps"}],
  "scope": {"opens": None, "closes": None, "within": R+"0"}, "confidence": "high"})
add(sc, {"act": R+"3", "kind": "report", "move": "report/findings",
  "quote": "(t02A03) The source says", "answers": [{"act": "claude-14-turn-69-f-s1-0", "quote": "The source says \"W … whose boundary is a standardly embedded genus two surface\"", "effect": "closes"}],
  "scope": {"opens": None, "closes": None, "within": R+"0"}, "confidence": "high"})
add(sc, {"act": R+"4", "kind": "propose", "move": "propose/next-step",
  "quote": "what grading should check", "answers": [],
  "scope": {"opens": None, "closes": None, "within": R+"0"}, "confidence": "medium"})
add(sc, {"act": R+"5", "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I prepare the six revisions and send them for grading?", "answers": [],
  "scope": {"opens": None, "closes": None, "within": R+"0"}, "confidence": "high"})
save("turn-zngcGd", sc)

# ---- turn-fRjuGO: s1-1 (quoted already? add) ----
sc, inp = load("turn-fRjuGO")
if not act(sc, "claude-17-turn-256-f-s1-1"):
    add(sc, {"act": "claude-17-turn-256-f-s1-1", "kind": "propose", "move": "propose/design",
      "quote": "include instructions and downloads in a fancy click-to-copy box", "answers": [],
      "scope": {"opens": None, "closes": None, "within": "claude-17-turn-256-f-s1-0"}, "confidence": "medium"})
set_scope(act(sc, "claude-17-turn-256-f-s1-0"), opens="xiaoxiang-packaging")
save("turn-fRjuGO", sc)

# ---- turn-xuT1Xm: r-4 ----
sc, inp = load("turn-xuT1Xm")
add(sc, {"act": "claude-16-turn-102-r-4", "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Shall I do the first step now?", "answers": [],
  "scope": {"opens": None, "closes": None, "within": "claude-16-turn-102-r-3"}, "confidence": "high"})
save("turn-xuT1Xm", sc)

# ---- turn-QLyVYx: s1-2 duplicate stub ----
sc, inp = load("turn-QLyVYx")
if not act(sc, "claude-17-turn-254-f-s1-2"):
    add(sc, {"act": "claude-17-turn-254-f-s1-2", "kind": "propose", "move": "propose/next-step",
      "quote": "see if we could fix the \"I reject your claim\" false positive", "answers": [],
      "scope": {"opens": None, "closes": None, "within": "claude-17-turn-254-f-s1-0"}, "confidence": "medium",
      "note": "kernel duplicate of s1-1"})
save("turn-QLyVYx", sc)
print("coverage pass done")

# ================= scope pass =================
# Matrix proof-room records: the header fragment frames the turn; body within header; @mention within header.
import glob as _g
for _f in sorted(_g.glob("*.moves.json")):
    rec=_f.replace(".moves.json","")
    sc=json.load(open(_f))
    frs=[a for a in sc["acts"] if a["act"].startswith("matrix:")]
    if not frs: continue
    hdr=None
    for a in frs:
        if a["move"]=="explain/role-marker": hdr=a; break
    if hdr is None and frs: hdr=frs[0]
    if hdr is None: continue
    set_scope(hdr, opens=None)
    hdr["scope"]["opens"] = hdr.get("scope",{}).get("opens") or (rec+"-frame")
    for a in frs:
        if a is hdr: continue
        if not a["scope"].get("within"): set_scope(a, within=hdr["act"])
    save(rec, sc)

def _simple(rec, opens_fid, within_map):
    sc,_=load(rec)
    o=act(sc, opens_fid)
    if o: set_scope(o, opens=rec+"-frame")
    for fid, parent in within_map.items():
        a=act(sc, fid)
        if a and not a["scope"].get("within"): set_scope(a, within=parent)
    save(rec, sc)

_simple("turn-FvxTse", None, {
  "claude-16-turn-108-f-s1-0": None,
})
# FvxTse: r-0..r-3 answer f-s2-0; frame: f-s2-0 question governs reply — put r-3 within r-0? Keep simple: f-s3-0 and f-s4-0 within f-s2-0? Actually 'but apart from that question...' and 'happy to continue' stand alone. r-1,r-2 within r-0.
sc,_=load("turn-FvxTse")
for fid,w in [("claude-16-turn-108-r-1","claude-16-turn-108-r-0"),("claude-16-turn-108-r-2","claude-16-turn-108-r-0")]:
    a=act(sc,fid)
    if a and not a["scope"].get("within"): set_scope(a,within=w)
a=act(sc,"claude-16-turn-108-f-s2-0")
if a: set_scope(a, opens="sidecar-count-review")
save("turn-FvxTse",sc)

_simple("turn-IJhnCI","claude-11-turn-43-f-s1-0",{
  "claude-11-turn-43-f-s1-1":"claude-11-turn-43-f-s1-0",
  "claude-11-turn-43-f-s2-0":"claude-11-turn-43-f-s1-0",
  "claude-11-turn-43-f-s3-0":"claude-11-turn-43-f-s1-0",
  "claude-11-turn-43-f-s4-0":"claude-11-turn-43-f-s1-0"})

_simple("turn-UIFG1x","claude-14-turn-874-f-s1-0",{
  "claude-14-turn-874-f-s2-0":"claude-14-turn-874-f-s1-0",
  "claude-14-turn-874-f-s3-0":"claude-14-turn-874-f-s1-0",
  "claude-14-turn-874-f-s3-1":"claude-14-turn-874-f-s2-0"})

_simple("turn-VR7PGI","claude-3-turn-27-f-s1-0",{
  "claude-3-turn-27-f-s1-1":"claude-3-turn-27-f-s1-0",
  "claude-3-turn-27-f-s1-2":"claude-3-turn-27-f-s1-0"})

_simple("turn-Qna6DD","claude-3-turn-25-f-s1-0",{
  "claude-3-turn-25-f-s2-0":"claude-3-turn-25-f-s1-0",
  "claude-3-turn-25-f-s3-0":"claude-3-turn-25-f-s2-0"})

# hand-labeled claude-17 records lacking links
_simple("turn-QLyVYx","claude-17-turn-254-f-s1-0",{
  "claude-17-turn-254-f-s1-1":"claude-17-turn-254-f-s1-0",
  "claude-17-turn-254-f-s1-2":"claude-17-turn-254-f-s1-0"})
print("scope pass done")
