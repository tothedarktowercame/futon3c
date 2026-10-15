#!/usr/bin/env python3
"""Blind Haiku verdict on every combined-rule within miss.

For each place where the combined position rule and the labeller drew
different `within` edges, builds a blind case (the act's words, its turn's
operator fragments and reply acts, two candidates labelled A/B in seeded
random order) and asks Haiku for a JSON verdict.  The judge is never told
which candidate is the rule's and which the labeller's — a judge told
which edge a labeller drew will tend to confirm it (no self-certification).

  python3 -I moves/judge_within.py --self-test   case builder + decoder on a fixture
  python3 -I moves/judge_within.py --run         full pass, writes the jsonl record
"""
import glob
import importlib.util
import json
import os
import random
import re
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
LAB = os.path.join(HERE, "..")
DATA = os.path.join(LAB, "data")
OUT_DIR = os.path.join(DATA, "judge-within")

sys.path.insert(0, HERE)
import within_rules  # noqa: E402
import show_miss  # noqa: E402

PROMPT = """You are judging a dialogue-move annotation. An act in a multi-act operator turn
(or an agent's reply paragraph) is annotated with a "within" edge: the id of
the earlier act it is answering, elaborating, or working under. Two
annotators disagree. Decide which parent (if either) is right.

The act to place (kind, words):
{act}

The turn it came from (operator fragments in order, then reply paragraphs
if the act is a reply act):
{turn}

Candidate A: {cand_a}
Candidate B: {cand_b}
(neither) means: the act stands alone; no earlier act in this turn governs
how it should be read.

Reply with ONE JSON object, nothing else:
{{"within": "A"|"B"|"neither", "why": "<one sentence about the WORDS of the
acts>", "confidence": "high"|"medium"|"low"}}"""


def candidate(kind, words, aid, here_turn):
    where = "this turn" if show_miss.turn_of(aid) == here_turn \
        else "an earlier turn (%s)" % show_miss.turn_of(aid)
    words = " ".join(str(words or "").split())[:300]
    return "%s (%s): \"%s\"  [%s]" % (aid, kind or "?", words, where)


def misses_of_rule():
    """Recompute the combined rule's misses over all gold links, using
    within_rules' own load/rules_for/combined. Returns [(batch, rec, inp,
    aid, rule_target, gold_target)]."""
    out = []
    for batch, rec, inp in within_rules.load():
        preds, kinds, own = within_rules.rules_for(rec, inp)
        for a in rec.get("acts", []):
            aid = a["act"]
            gold = a["scope"].get("within")
            if not gold or aid not in preds:
                continue
            rule = within_rules.combined(preds[aid], aid)
            if rule != gold:
                out.append((batch, rec, inp, aid, rule, gold))
    return out


def turn_block(inp, aid):
    here = show_miss.turn_of(aid)
    lines = []
    for f in inp.get("fragments", []):
        k, t = f.get("intent") or "?", f.get("text") or ""
        lines.append("  %s %s: \"%s\"" % (f["id"], k, " ".join(t.split())[:160]))
    if "-r-" in aid:
        for a in inp.get("reply_acts", []):
            lines.append("  %s reply: \"%s\"" % (a["id"],
                         " ".join((a.get("text") or "").split())[:160]))
    return "\n".join(lines) if lines else "  (no fragment texts recorded)"


def build_case(batch, rec, inp, aid, rule_target, gold_target, by):
    """The blind case: seeded A/B order; returns the prompt body and the key."""
    here = show_miss.turn_of(aid)
    ak, at = by.get(aid, ("?", "(words not found)"))
    rng = random.Random(aid)
    rule_first = rng.random() < 0.5

    def cand(target):
        if target in (None, "None", "none"):
            return "none of the above (the act stands alone)"
        k, t = by.get(target, ("?", "(words not found)"))
        return candidate(k, t, target, here)

    a_c, b_c = (cand(rule_target), cand(gold_target)) if rule_first \
        else (cand(gold_target), cand(rule_target))
    body = {"act": "%s (%s): \"%s\"" % (aid, ak, " ".join(str(at).split())[:300]),
            "turn": turn_block(inp, aid),
            "cand_a": a_c, "cand_b": b_c}
    prompt = PROMPT.format(**body)
    key = {"A": "rule" if rule_first else "labeller",
           "B": "labeller" if rule_first else "rule"}
    return prompt, key


def decode(reply, key):
    """Parse the model's JSON; map A/B through the key. Returns
    (sides, why, confidence, raw) where sides in
    {rule, labeller, neither, unparseable}."""
    m = re.search(r"\{[^{}]*\}", reply or "", re.S)
    if not m:
        return "unparseable", "", "", reply or ""
    try:
        d = json.loads(m.group(0))
    except (json.JSONDecodeError, ValueError):
        return "unparseable", "", "", reply or ""
    w = str(d.get("within", "")).strip().lower()
    if w not in ("a", "b", "neither"):
        return "unparseable", str(d.get("why", "")), str(d.get("confidence", "")), reply or ""
    if w == "neither":
        return "neither", str(d.get("why", "")), str(d.get("confidence", "")), reply or ""
    return key[w.upper()], str(d.get("why", "")), str(d.get("confidence", "")), reply or ""


def load_judge():
    path = os.path.expanduser("~/code/minus40/scripts/judge.py")
    spec = importlib.util.spec_from_file_location("judge", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


POSITION_WORDS = re.compile(
    r"\b(nearest|first|earliest|earlier one|position|order|preced|"
    r"closest|comes before|comes after|immediately)\b", re.I)


def why_sort(sentence):
    """'words' vs 'position': does the why cite the acts' words/content, or
    only their order?"""
    if not sentence:
        return "empty"
    if POSITION_WORDS.search(sentence):
        return "position" if not re.search(r"(say|word|about|topic|content|"
                                           r"ask|answer|address|respond|refer)", sentence, re.I) \
            else "both"
    return "words"


def self_test():
    """Case builder + decoder on a built-in two-miss fixture, fake asker."""
    inp = {"fragments": [
        {"id": "t-f-s1-0", "intent": "constrain", "text": "Keep the statement frozen."},
        {"id": "t-f-s2-0", "intent": "clarify", "text": "What did you mean by frozen?"},
        {"id": "t-f-s3-0", "intent": "report", "text": "The build is green today."}],
        "reply_acts": [{"id": "t-r-0", "text": "(gist) The build passed."}]}
    rec = {"acts": [
        {"act": "t-f-s2-0", "kind": "clarify", "move": "x", "quote": "What did you mean by frozen?",
         "answers": [], "scope": {"opens": None, "closes": None, "within": "t-f-s1-0"},
         "confidence": "high"},
        {"act": "t-r-0", "kind": "reply-mark", "move": "x", "quote": "(gist) The build passed.",
         "answers": [], "scope": {"opens": None, "closes": None, "within": "t-f-s3-0"},
         "confidence": "high"}]}
    by = {"t-f-s1-0": ("constrain", "Keep the statement frozen."),
          "t-f-s2-0": ("clarify", "What did you mean by frozen?"),
          "t-f-s3-0": ("report", "The build is green today."),
          "t-r-0": ("reply-mark", "(gist) The build passed.")}
    # miss 1: rule says t-f-s1-0 too (same) — use a genuine miss: rule None
    prompt, key = build_case("b", rec, inp, "t-f-s2-0", None, "t-f-s1-0", by)
    assert "Candidate A" in prompt and "Candidate B" in prompt, prompt
    assert "none of the above" in prompt
    # key round-trips
    sides, why, conf, raw = decode('{"within": "A", "why": "the question quotes the constraint", '
                                   '"confidence": "high"}', key)
    assert sides == key["A"], (sides, key)
    # neither decodes without touching the key
    sides2, _, _, _ = decode('{"within": "neither", "why": "stands alone", '
                             '"confidence": "low"}', key)
    assert sides2 == "neither", sides2
    # unparseable
    sides3, _, _, _ = decode("I think A is better because...", key)
    assert sides3 == "unparseable", sides3
    # deterministic A/B order for the same act id
    _, key2 = build_case("b", rec, inp, "t-f-s2-0", None, "t-f-s1-0", by)
    assert key == key2
    # why_sort
    assert why_sort("the question quotes the constraint's words") == "words"
    assert why_sort("it is the nearest earlier act") == "position"
    print("self-test OK: case builder, key round-trip, neither decode, "
          "unparseable, determinism, why_sort")
    return 0


def run():
    by = show_miss.texts()
    ms = misses_of_rule()
    frag = [m for m in ms if "-f-" in m[3]]
    reply = [m for m in ms if "-r-" in m[3]]
    print("misses: %d (%d fragment, %d reply)" % (len(ms), len(frag), len(reply)))
    judge = load_judge()
    os.makedirs(OUT_DIR, exist_ok=True)
    stamp = time.strftime("%Y%m%dT%H%M%SZ")
    out_path = os.path.join(OUT_DIR, "run-%s.jsonl" % stamp)
    rows = []
    for i, (batch, rec, inp, aid, rule_t, gold_t) in enumerate(ms):
        prompt, key = build_case(batch, rec, inp, aid, rule_t, gold_t, by)
        t0 = time.time()
        try:
            reply = judge.ask_claude_cli("haiku", prompt, 20)
            err = None
        except Exception as e:  # continue past failures
            reply, err = "", "%s: %s" % (type(e).__name__, e)
        sides, why, conf, raw = decode(reply, key)
        rows.append({"act": aid, "batch": batch, "is_reply": "-r-" in aid,
                     "rule_target": rule_t, "gold_target": gold_t,
                     "key": key, "sides": sides, "why": why,
                     "confidence": conf, "seconds": round(time.time() - t0, 2),
                     "error": err})
        if (i + 1) % 50 == 0:
            print("  %d/%d" % (i + 1, len(ms)), flush=True)
    with open(out_path, "w") as fh:
        for r in rows:
            fh.write(json.dumps(r, ensure_ascii=False) + "\n")
    print("wrote", out_path, len(rows), "calls")
    return out_path, rows, ms


if __name__ == "__main__":
    if "--self-test" in sys.argv:
        sys.exit(self_test())
    if "--run" in sys.argv:
        run()
        sys.exit(0)
    print(__doc__)
    sys.exit(1)
