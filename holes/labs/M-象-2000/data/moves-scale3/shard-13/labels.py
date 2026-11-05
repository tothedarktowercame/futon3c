# Batch-3 wave-5 shard-13 labels.
LABELS = {}
def L(rec, acts): LABELS[rec] = acts

# --- claude-17 (259/257/255/260/…) ---
L("turn-0TCLZ8", [
 {"act": "claude-17-turn-259-f-s1-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "turns relayed through Agency often would *not* be typed by me"},
 {"act": "claude-17-turn-259-f-s2-0", "kind": "report", "move": "report/observation",
  "quote": "I am almost entirely using Codex via Agency"},
 {"act": "claude-17-turn-259-f-s3-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Can you fix the Codex logging so that it attributes orgin correctly",
  "within": "claude-17-turn-259-f-s2-0"}])
L("turn-1lhjNB", [
 {"act": "claude-17-turn-257-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "Yeah that box is nice"},
 {"act": "claude-17-turn-257-f-s1-1", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "we can merge it into the actual index.html", "within": "claude-17-turn-257-f-s1-0"},
 {"act": "claude-17-turn-257-f-s1-2", "kind": "clarify", "move": "clarify/caveat",
  "quote": "(the content is slightly different on the live index.html)",
  "within": "claude-17-turn-257-f-s1-1"},
 {"act": "claude-17-turn-257-f-s2-0", "kind": "defer", "move": "defer/until",
  "quote": "I will tweak it further when it is there.", "within": "claude-17-turn-257-f-s1-1"}])
L("turn-3vqBlD", [
 {"act": "claude-17-turn-255-f-s1-0", "kind": "report", "move": "report/relay",
  "quote": "Here are comments from claude-3."},
 {"act": "claude-17-turn-255-f-s6-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "The scanner check fails open.", "within": "claude-17-turn-255-f-s1-0"},
 {"act": "claude-17-turn-255-f-s7-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "The other filters", "within": "claude-17-turn-255-f-s6-0"},
 {"act": "claude-17-turn-255-f-s8-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "a copy without `secret_scan.py` next to it would export silently with no check",
  "within": "claude-17-turn-255-f-s6-0"}])
L("turn-8kjwke", [
 {"act": "claude-17-turn-260-f-s1-0", "kind": "propose", "move": "propose/tentative",
  "quote": "probably shouldn't be hard-coded if we want the system to be maximally useful"},
 {"act": "claude-17-turn-260-f-s2-0", "kind": "explain", "move": "explain/context",
  "quote": "with the solve-loop it is totally automated even if I do sit around reading logs.",
  "within": "claude-17-turn-260-f-s1-0"},
 {"act": "claude-17-turn-260-f-s3-0", "kind": "explore", "move": "explore/open-question",
  "quote": "A gap-time=0 would just show a continuous decomposition of token usage over time",
  "within": "claude-17-turn-260-f-s1-0"},
 {"act": "claude-17-turn-260-f-s3-1", "kind": "explore", "move": "explore/open-question",
  "quote": "a gap-time=1 might be interesting to show things that have been running autonomously",
  "within": "claude-17-turn-260-f-s1-0"}])
L("turn-HhJPAZ", [
 {"act": "claude-17-turn-261-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Can you rebuild staging by merging what's on 7a"},
 {"act": "claude-17-turn-261-f-s1-1", "kind": "constrain", "move": "constrain/for-now",
  "quote": "so that I can read it easily", "within": "claude-17-turn-261-f-s1-0"},
 {"act": "claude-17-turn-261-f-s1-2", "kind": "constrain", "move": "constrain/for-now",
  "quote": "so that we don't loose track of the changes", "within": "claude-17-turn-261-f-s1-0"}])
L("turn-QLyVYx", [
 {"act": "claude-17-turn-254-f-s1-0", "kind": "propose", "move": "propose/directive",
  "quote": "Yeah we could remove the specific first names"},
 {"act": "claude-17-turn-254-f-s1-1", "kind": "propose", "move": "propose/next-step",
  "quote": "see if we could fix the \"I reject your claim\" false positive"}])
L("turn-QdOtiv", [
 {"act": "claude-17-turn-262-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Could you integrate the different figures you created"},
 {"act": "claude-17-turn-262-f-s1-1", "kind": "constrain", "move": "constrain/for-now",
  "quote": "so that a reader quickly sees them", "within": "claude-17-turn-262-f-s1-0"}])
L("turn-fRjuGO", [
 {"act": "claude-17-turn-256-f-s1-0", "kind": "propose", "move": "propose/next-step",
  "quote": "I think it would be more convenient to package it up"},
 {"act": "claude-17-turn-256-f-s2-0", "kind": "propose", "move": "propose/design",
  "quote": "include instructions and downloads in a fancy click-to-", "within": "claude-17-turn-256-f-s1-0"}])
L("turn-kOpEMd", [
 {"act": "claude-17-turn-258-f-s1-0", "kind": "propose", "move": "propose/design",
  "quote": "It would be great if it could also produce a box diagram"},
 {"act": "claude-17-turn-258-f-s2-0", "kind": "report", "move": "report/observation",
  "quote": "https://hyperreal.enterprises"}])
L("turn-mAaowO", [
 {"act": "claude-17-turn-253-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "Sounds good."},
 {"act": "claude-17-turn-253-f-s2-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "I'd love to have a version 0.1 I could put on my webpage rig", "within": "claude-17-turn-253-f-s1-0"}])

# --- matrix proof-room records: labeled programmatically (formulaic) ---
def matrix_labels(d):
    """Matrix proof-room turns: [role | mNNNN] header + body + @mention."""
    out = []
    frags = d["fragments"]
    for i, fr in enumerate(frags):
        t = fr["text"]
        if i == 0 and t.startswith("[") and "|" in t:
            out.append({"act": fr["id"], "kind": "explain", "move": "explain/role-marker",
                        "quote": t.strip(), "scope": {"opens": None, "closes": None, "within": None},
                        "confidence": "high"})
            continue
        if t.startswith("@"):
            out.append({"act": fr["id"], "kind": "delegate", "move": "delegate/to-agent",
                        "quote": t.strip()[:60], "scope": {"opens": None, "closes": None, "within": None},
                        "confidence": "high"})
            continue
        low = t.lower()
        if low.startswith("blocked") or "no further proof work is authorized" in low or "terminal signal" in low:
            mv = "report-problem/blocked"
        elif "the theorem is proved" in low or "the proof is complete" in low or "verified against" in low:
            mv = "report/done"
        elif "no open obligation" in low or "no further moves are owed" in low or "nothing outstanding" in low:
            mv = "report/done"
        elif "should take no further moves" in low or "should start a new problem" in low or "no move is proposed" in low:
            mv = "constrain/for-now"
        elif "do not consult" in low or "work on only" in low:
            mv = "constrain/standing-rule"
        elif "closure acknowledged" in low or "closure m0010" in low:
            mv = "report/done"
        else:
            mv = "explain/mechanism"
        out.append({"act": fr["id"], "kind": fr.get("intent", "explain"),
                    "move": mv if "/" in mv else "explain/" + mv.split("/", 1)[-1],
                    "quote": t.strip()[:80],
                    "scope": {"opens": None, "closes": None, "within": None},
                    "confidence": "medium"})
    return out

import glob as _glob, json as _json
for _f in sorted(_glob.glob("input/*.input.json")):
    _d = _json.load(open(_f))
    _fr = _d["fragments"]
    if _fr and _fr[0]["id"].startswith("matrix:"):
        L(_d["record"], matrix_labels(_d))

# --- claude-14 (topo loops) ---
L("turn-6Iq8Pr", [
 {"act": "claude-14-turn-488-f-s1-0", "kind": "report", "move": "report/status",
  "quote": "progress has been rapid from 111 a few days ago to 85 now."},
 {"act": "claude-14-turn-488-f-s2-0", "kind": "collect", "move": "collect/ask-option",
  "quote": "do you think ghe remaininv problems are amenable to this overall approach?"}])
L("turn-8DcR2B", [
 {"act": "claude-14-turn-119-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "ok to your plan,"},
 {"act": "claude-14-turn-119-f-s1-1", "kind": "extend", "move": "extend/possibility",
  "quote": "if we can close some topology problems by simklar methods we should do so"}])
L("turn-8cQ5oA", [
 {"act": "claude-14-turn-464-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "can you stop the topo loops and make them robust against this failure mode"},
 {"act": "claude-14-turn-464-f-s2-0", "kind": "propose", "move": "propose/tentative",
  "quote": "we need to think about whether to change to the blueprint / lemma approach"}])
L("turn-DTKNSy", [
 {"act": "claude-14-turn-435-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "set up a lemma attempt for the problems you mentioned"}])
L("turn-GJV0OU", [
 {"act": "claude-14-turn-857-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "yes"}])
L("turn-H6Dtgm", [
 {"act": "claude-14-turn-537-f-s1-0", "kind": "propose", "move": "propose/directive",
  "quote": "we shoild be able to build one overall best of str"}])
L("turn-Hj8k9t", [
 {"act": "claude-14-turn-434-f-s1-0", "kind": "verify", "move": "verify/status-check",
  "quote": "you said the loops are running again, did they achieve anything overnight?"}])
L("turn-MO7lUC", [
 {"act": "claude-14-turn-845-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "please carry out your plan"}])
L("turn-O2FN1Z", [
 {"act": "claude-14-turn-463-f-s1-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "the Vox term has lots of different agents listed on it"},
 {"act": "claude-14-turn-463-f-s1-1", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "can you please look into that?", "within": "claude-14-turn-463-f-s1-0"}])
L("turn-UIFG1x", [
 {"act": "claude-14-turn-874-f-s1-0", "kind": "report", "move": "report/observation",
  "quote": "73 problems still seemd like a lot."},
 {"act": "claude-14-turn-874-f-s2-0", "kind": "explain", "move": "explain/reasoning",
  "quote": "if we can close 25 per usage window it is 3 weeks."},
 {"act": "claude-14-turn-874-f-s3-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "That's not out of the question,"},
 {"act": "claude-14-turn-874-f-s3-1", "kind": "propose", "move": "propose/next-step",
  "quote": "maybe we should start keeping track of progress vs codex usage."}])
L("turn-Wd1aN6", [
 {"act": "claude-14-turn-875-f-s1-0", "kind": "report", "move": "report/context",
  "quote": "I also have api credits so we can reset when we hit 0 without issue"}])
L("turn-coGkgU", [
 {"act": "claude-14-turn-112-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "well this seemed quite an effdctive approach!"}])
L("turn-g0mExz", [
 {"act": "claude-14-turn-844-f-s1-0", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "should we focus on the cutandpaste library?"}])
L("turn-hmc3rf", [
 {"act": "claude-14-turn-535-f-s1-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "i don't know if we should spend time on \"narrower\" formulations"}])
L("turn-iixaxY", [
 {"act": "claude-14-turn-465-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "please do blueprint-1"}])
L("turn-jxpxks", [
 {"act": "claude-14-turn-433-f-s1-0", "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "progress report please"}])
L("turn-lPTjPZ", [
 {"act": "claude-14-turn-71-f-s1-0", "kind": "report", "move": "report/observation",
  "quote": "there were a few \"hard\" nontopology problems that have been set aside as well."},
 {"act": "claude-14-turn-71-f-s2-0", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "if we were to go one problem at a tjme following the blueprints"}])
L("turn-lYq09Y", [
 {"act": "claude-14-turn-487-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "yes I approve the new worklists"}])
L("turn-mLxFT9", [
 {"act": "claude-14-turn-876-f-s1-0", "kind": "report", "move": "report/status",
  "quote": "ok i just hit the reset"}])
L("turn-okmC5m", [
 {"act": "claude-14-turn-489-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "ok please start batch 4 and continue with the plan you have outlined"}])
L("turn-rx8nrg", [
 {"act": "claude-14-turn-96-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "let's try this out!"}])
L("turn-uZUTSa", [
 {"act": "claude-14-turn-538-f-s1-0", "kind": "propose", "move": "propose/directive",
  "quote": "why don't we work through from easiest to hardest following this plan"}])
L("turn-vxo2ab", [
 {"act": "claude-1-turn-89-f-s1-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "I'd like the run-repair cycles to start working reliably"},
 {"act": "claude-1-turn-89-f-s1-1", "kind": "propose", "move": "propose/alternative",
  "quote": "perhaps we could go back to the \"loop\" variant rather than relying on bells.",
  "within": "claude-1-turn-89-f-s1-0"},
 {"act": "claude-1-turn-89-f-s2-0", "kind": "report", "move": "report/context",
  "quote": "claude-18 has been refining my topo and solver loops"},
 {"act": "claude-1-turn-89-f-s2-1", "kind": "propose", "move": "propose/alternative",
  "quote": "there was a previous wm-build-loop that could be adapted"}])

# --- claude-16 (M-象-2000 owner seat) ---
L("turn-AE13zu", [
 {"act": "claude-16-turn-100-f-s1-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "i am concerned that we didnt sufficiently cover the Fong/related causal theories"},
 {"act": "claude-16-turn-100-r-0", "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "My edit script checked for my own text, failed to find it, and stopped.",
  "answers": [{"act": "claude-16-turn-100-f-s1-0", "quote": "My edit script checked for my own text, failed to find it", "effect": "keeps"}]},
 {"act": "claude-16-turn-100-r-1", "kind": "report", "move": "report/done",
  "quote": "The new paragraph (2bc17b9cb) says",
  "answers": [{"act": "claude-16-turn-100-f-s1-0", "quote": "a pattern's DAG presents a causal theory in the sense of Fong's", "effect": "closes"}]},
 {"act": "claude-16-turn-100-r-2", "kind": "report", "move": "report/with-options",
  "quote": "two drafts", "answers": [{"act": "claude-16-turn-100-f-s1-0", "quote": "My longer version", "effect": "keeps"}]}])
L("turn-FvxTse", [
 {"act": "claude-16-turn-108-f-s1-0", "kind": "continue", "move": "continue/resume-work",
  "quote": "right..."},
 {"act": "claude-16-turn-108-f-s2-0", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "943 sidecars seems like more than I was expecting at this point in time."},
 {"act": "claude-16-turn-108-f-s3-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "but apart from that question..."},
 {"act": "claude-16-turn-108-f-s4-0", "kind": "approve", "move": "approve/plain",
  "quote": "happy to continue"},
 {"act": "claude-16-turn-108-r-0", "kind": "report", "move": "report/findings",
  "quote": "The count by batch, checked against the files and their first commits",
  "answers": [{"act": "claude-16-turn-108-f-s2-0", "quote": "The count by batch, checked against the files", "effect": "closes"}]},
 {"act": "claude-16-turn-108-r-1", "kind": "report", "move": "report/done",
  "quote": "The standings index in `sample.py` now covers every turn record"},
 {"act": "claude-16-turn-108-r-2", "kind": "report", "move": "report/dispatched",
  "quote": "Carrier packet to zai-2"},
 {"act": "claude-16-turn-108-r-3", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "They can be labelled in one more wave for about a point of the monthly meter",
  "answers": [{"act": "claude-16-turn-108-f-s2-0", "quote": "I have not sent that, since the question was whether 943 was already more than intended", "effect": "keeps"}]}])
L("turn-HYgvM1", [
 {"act": "claude-16-turn-105-f-s1-0", "kind": "report", "move": "report/status",
  "quote": "futon1b has been repaired by an external agent."},
 {"act": "claude-16-turn-105-f-s2-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "can you join things up a bit more?", "within": "claude-16-turn-105-f-s1-0"},
 {"act": "claude-16-turn-105-r-0", "kind": "verify", "move": "verify/checked-live",
  "quote": "Heap 877 of 4096 MB, session evidence query 0.7 s",
  "answers": [{"act": "claude-16-turn-105-f-s1-0", "quote": "Frames for this session fill with HAPPENED again", "effect": "closes"}]},
 {"act": "claude-16-turn-105-r-1", "kind": "report", "move": "report/done",
  "quote": "(joins made)",
  "answers": [{"act": "claude-16-turn-105-f-s2-0", "quote": "Bell ask to Agency job.", "effect": "closes"}]},
 {"act": "claude-16-turn-105-r-2", "kind": "report", "move": "report/findings",
  "quote": "one exchange, two turn ids"},
 {"act": "claude-16-turn-105-r-3", "kind": "qualify", "move": "qualify/caveat",
  "quote": "check on your next turn"},
 {"act": "claude-16-turn-105-r-4", "kind": "report", "move": "report/partial",
  "quote": "still apart"}])
L("turn-JFIufe", [
 {"act": "claude-16-turn-103-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "go ahead with your plan"},
 {"act": "claude-16-turn-103-r-0", "kind": "report", "move": "report/done",
  "quote": "store as a unit",
  "answers": [{"act": "claude-16-turn-103-f-s1-0", "quote": "answered health 18 s later", "effect": "closes"}]},
 {"act": "claude-16-turn-103-r-1", "kind": "report", "move": "report/done",
  "quote": "brief for all seats",
  "answers": [{"act": "claude-16-turn-103-f-s1-0", "quote": "`brief-enabled.txt` now reads `all`", "effect": "closes"}]},
 {"act": "claude-16-turn-103-r-2", "kind": "report", "move": "report/done",
  "quote": "Facts in the resume brief",
  "answers": [{"act": "claude-16-turn-103-f-s1-0", "quote": "shares minus40's store, ten-minute cache and true-at rule", "effect": "closes"}]},
 {"act": "claude-16-turn-103-r-3", "kind": "report", "move": "report/observation",
  "quote": "seen on the way"},
 {"act": "claude-16-turn-103-r-4", "kind": "qualify", "move": "qualify/caveat",
  "quote": "still open from this morning"}])
L("turn-NhxIGZ", [
 {"act": "claude-16-turn-101-f-s1-0", "kind": "report", "move": "report/context",
  "quote": "hi, we did vairious demos that will be interesting later,"},
 {"act": "claude-16-turn-101-f-s1-1", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "I would like to have some live demos based on my current interactions"},
 {"act": "claude-16-turn-101-r-0", "kind": "verify", "move": "verify/checked-live",
  "quote": "Your message was recorded at 16:12:48Z, read by 象-sonnet 90 s later",
  "answers": [{"act": "claude-16-turn-101-f-s1-1", "quote": "the prompt header carried three retrieved patterns", "effect": "closes"}]},
 {"act": "claude-16-turn-101-r-1", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "futon1b heap is 4014 of 4096 MB"},
 {"act": "claude-16-turn-101-r-2", "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "Restart futon1b-zone now?"},
 {"act": "claude-16-turn-101-r-3", "kind": "propose", "move": "propose/next-step",
  "quote": "two small fixes, if you want them"},
 {"act": "claude-16-turn-101-r-4", "kind": "report", "move": "report/done",
  "quote": "live demos based on my current interactions",
  "answers": [{"act": "claude-16-turn-101-f-s1-1", "quote": "All loaded in your running Emacs and checked", "effect": "closes"}]}])
L("turn-Ok0f8i", [
 {"act": "claude-16-turn-99-f-s1-0", "kind": "propose", "move": "propose/tentative",
  "quote": "it could be interesting to write up a summary cover note to send to Aleks Kissinger"},
 {"act": "claude-16-turn-99-r-0", "kind": "report", "move": "report/done",
  "quote": "It meets the project at its four parts in turn.",
  "answers": [{"act": "claude-16-turn-99-f-s1-0", "quote": "It meets the project at its four parts in turn", "effect": "closes"}]},
 {"act": "claude-16-turn-99-r-1", "kind": "qualify", "move": "qualify/caveat",
  "quote": "things to check before sending"}])
L("turn-W0Ij6G", [
 {"act": "claude-16-turn-106-f-s1-0", "kind": "delegate", "move": "delegate/to-agent",
  "quote": "can you dispatch the work packet to a zai agent ?"},
 {"act": "claude-16-turn-106-r-0", "kind": "report", "move": "report/dispatched",
  "quote": "Job `invoke-1792003840456-12198-00dab8d9`",
  "answers": [{"act": "claude-16-turn-106-f-s1-0", "quote": "Job `invoke-1792003840456-12198-00dab8d9`", "effect": "closes"}]},
 {"act": "claude-16-turn-106-r-1", "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "Last turn I said the open view does not show a memory version beside its writing turn. It does"},
 {"act": "claude-16-turn-106-r-2", "kind": "propose", "move": "propose/next-step",
  "quote": "Two more are ready to write at the same size"}])
L("turn-d1Hl5p", [
 {"act": "claude-16-turn-104-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "let's give some examples that this memory system can do that the standard one can't"},
 {"act": "claude-16-turn-104-r-0", "kind": "report", "move": "report/with-options",
  "quote": "1, what memory said on a day",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "On 09-24 memory knew Zai was out on its weekly limit", "effect": "closes"}]},
 {"act": "claude-16-turn-104-r-1", "kind": "report", "move": "report/with-options",
  "quote": "2, how a lesson evolved",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "`history zai window` gives 17 events", "effect": "closes"}]},
 {"act": "claude-16-turn-104-r-2", "kind": "report", "move": "report/with-options",
  "quote": "3, net change versus events",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "six real facts were said and withdrawn inside the interval", "effect": "closes"}]},
 {"act": "claude-16-turn-104-r-3", "kind": "report", "move": "report/with-options",
  "quote": "4, contradictions",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "18 open conflicts between pairs of memories", "effect": "closes"}]},
 {"act": "claude-16-turn-104-r-4", "kind": "report", "move": "report/with-options",
  "quote": "5, the same lesson twice",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "21 relation edges, 8 across projects", "effect": "closes"}]},
 {"act": "claude-16-turn-104-r-5", "kind": "report", "move": "report/with-options",
  "quote": "6, the conversation side",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "answer \"what did Joe ask that nobody answered\"", "effect": "closes"}]},
 {"act": "claude-16-turn-104-r-6", "kind": "qualify", "move": "qualify/caveat",
  "quote": "what none of it does yet",
  "answers": [{"act": "claude-16-turn-104-f-s1-0", "quote": "The two halves meet only in that Facts section", "effect": "keeps"}]}])
L("turn-jKVB3m", [
 {"act": "claude-16-turn-94-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "let's put it on the list."},
 {"act": "claude-16-turn-94-f-s2-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "we still have 8% of zai to use so please dispatch work accordingly"},
 {"act": "claude-16-turn-94-r-0", "kind": "report", "move": "report/findings",
  "quote": "The remaining pool was 274 unlabelled operator turns",
  "answers": [{"act": "claude-16-turn-94-f-s2-0", "quote": "The remaining pool was 274 unlabelled operator turns", "effect": "closes"}]},
 {"act": "claude-16-turn-94-r-1", "kind": "report", "move": "report/done",
  "quote": "Recorded in the mission with your pasted sentence as its source",
  "answers": [{"act": "claude-16-turn-94-f-s1-0", "quote": "Recorded in the mission", "effect": "closes"}]}])
L("turn-xboHs8", [
 {"act": "claude-14-turn-70-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "ok then please go ahead"},
 {"act": "claude-14-turn-70-r-0", "kind": "report", "move": "report/done",
  "quote": "what changed in Lean",
  "answers": [{"act": "claude-14-turn-70-f-s1-0", "quote": "Five surface problems", "effect": "closes"}]},
 {"act": "claude-14-turn-70-r-1", "kind": "verify", "move": "verify/checked-live",
  "quote": "what the grader checked",
  "answers": [{"act": "claude-14-turn-70-f-s1-0", "quote": "Grades and notes are in", "effect": "closes"}]},
 {"act": "claude-14-turn-70-r-2", "kind": "report", "move": "report/findings",
  "quote": "ledgers"},
 {"act": "claude-14-turn-70-r-3", "kind": "report-problem", "move": "report-problem/self-correction",
  "quote": "My first re-routing pass matched node names too broadly"},
 {"act": "claude-14-turn-70-r-4", "kind": "report", "move": "report/done",
  "quote": "topo3 fix"},
 {"act": "claude-14-turn-70-r-5", "kind": "qualify", "move": "qualify/caveat",
  "quote": "what these changes don't remove"}])
L("turn-xuT1Xm", [
 {"act": "claude-16-turn-102-f-s1-0", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "What about the features from E-cool-memories?"},
 {"act": "claude-16-turn-102-r-0", "kind": "report", "move": "report/findings",
  "quote": "Checked this turn, each against the live store",
  "answers": [{"act": "claude-16-turn-102-f-s1-0", "quote": "Checked this turn, each against the live store", "effect": "closes"}]},
 {"act": "claude-16-turn-102-r-1", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "can be brought online"},
 {"act": "claude-16-turn-102-r-2", "kind": "report", "move": "report/status",
  "quote": "fully integrated"},
 {"act": "claude-16-turn-102-r-3", "kind": "propose", "move": "propose/order-of-work",
  "quote": "three steps, in order"}])
L("turn-zngcGd", [
 {"act": "claude-14-turn-69-f-s1-0", "kind": "challenge", "move": "challenge/premise",
  "quote": "um, are these revions warranted by the origianal informal problem statements???"},
 {"act": "claude-14-turn-69-r-0", "kind": "explain", "move": "explain/answer-question",
  "quote": "Yes for both, judging by the source wording.",
  "answers": [{"act": "claude-14-turn-69-f-s1-0", "quote": "Yes for both, judging by the source wording", "effect": "closes"}]}])

# --- remaining sessions ---
L("turn-FjWMVL", [
 {"act": "claude-14-turn-72-f-s1-0", "kind": "delegate", "move": "delegate/to-agent",
  "quote": "let's ask a codex helper tp write blueprints for all"},
 {"act": "claude-14-turn-72-r-0", "kind": "report", "move": "report/findings",
  "quote": "what counts as set aside",
  "answers": [{"act": "claude-14-turn-72-f-s1-0", "quote": "There are 19 non-topology problems with real `sorry`s left", "effect": "keeps"}]},
 {"act": "claude-14-turn-72-r-1", "kind": "report", "move": "report/done",
  "quote": "what the helper is asked to do",
  "answers": [{"act": "claude-14-turn-72-f-s1-0", "quote": "The full instructions are in `~/notes/packets/blueprints/preamble.md`", "effect": "closes"}]},
 {"act": "claude-14-turn-72-r-2", "kind": "report", "move": "report/done",
  "quote": "where it runs",
  "answers": [{"act": "claude-14-turn-72-f-s1-0", "quote": "The helper is a newly registered seat", "effect": "closes"}]},
 {"act": "claude-14-turn-72-r-3", "kind": "report", "move": "report/in-flight",
  "quote": "pilot status",
  "answers": [{"act": "claude-14-turn-72-f-s1-0", "quote": "The pilot is m99A06", "effect": "keeps"}]}])
L("turn-IJhnCI", [
 {"act": "claude-11-turn-43-f-s1-0", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "so, why not port the core faithfully?"},
 {"act": "claude-11-turn-43-f-s1-1", "kind": "clarify", "move": "clarify/ask-decision",
  "quote": "why not port the core faithfully?"},
 {"act": "claude-11-turn-43-f-s2-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "i can't think of anything in the port itself that is sacrosanct."},
 {"act": "claude-11-turn-43-f-s3-0", "kind": "challenge", "move": "challenge/premise",
  "quote": "isn't the point to replucate Lean in Python?"},
 {"act": "claude-11-turn-43-f-s4-0", "kind": "challenge", "move": "challenge/premise",
  "quote": "why pussyfoit around if so?"}])
L("turn-Qna6DD", [
 {"act": "claude-3-turn-25-f-s1-0", "kind": "continue", "move": "continue/resume-work",
  "quote": "Hm..."},
 {"act": "claude-3-turn-25-f-s2-0", "kind": "propose", "move": "propose/tentative",
  "quote": "we could put it in *before* the turn gets to the agent in question."},
 {"act": "claude-3-turn-25-f-s3-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "just as well."}])
L("turn-VR7PGI", [
 {"act": "claude-3-turn-27-f-s1-0", "kind": "report", "move": "report/observation",
  "quote": "FYI another agent has been working on /home/joe/code/futon3c/scripts/xiaoxiang.py"},
 {"act": "claude-3-turn-27-f-s1-1", "kind": "report", "move": "report/observation",
  "quote": "that's a distillation of the actual 象 info"},
 {"act": "claude-3-turn-27-f-s1-2", "kind": "propose", "move": "propose/tentative",
  "quote": "we could integrate with the secrets scanner"}])
L("turn-Zb7yed", [
 {"act": "codex-75-turn-30-f-s1-0", "kind": "report", "move": "report/observation",
  "quote": "they have walked through the whole discussion"}])
L("turn-jk2twD", [
 {"act": "claude-11-turn-45-f-s1-0", "kind": "clarify", "move": "clarify/assertion",
  "quote": "you may be on a different page from me about the goal here."},
 {"act": "claude-11-turn-45-f-s2-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "we are focusing on the C++ code",
  "within": "claude-11-turn-45-f-s1-0"},
 {"act": "claude-11-turn-45-f-s2-1", "kind": "constrain", "move": "constrain/for-now",
  "quote": "which isn't \"some\" but all of the path to compiling a blank lean file.",
  "within": "claude-11-turn-45-f-s1-0"}])
L("turn-kUpBue", [
 {"act": "claude-1-turn-174-f-s1-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "We can restart once we have sorted out this business about critical parameters"},
 {"act": "claude-1-turn-174-f-s2-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "each variable in the Lean model needs an interpretation in terms of real countable things",
  "within": "claude-1-turn-174-f-s1-0"},
 {"act": "claude-1-turn-174-f-s2-1", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "the requirement should be that all countable things need to be taken into account",
  "within": "claude-1-turn-174-f-s1-0"}])
