#!/usr/bin/env python3
"""Validate and publish agent interpretations of Emacs operator-turn records.

Offsets are Unicode code points, zero based, end exclusive. The request is
immutable; a separate result records provenance and the exact source digest.
No model, network calls, or automatic vocabulary updates occur here.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
from datetime import datetime, timezone
import edn_format
from edn_format import Keyword as K

LIBRARY = Path(__file__).resolve().parents[2] / "futon3" / "library"
RNODE_DEFINITIONS = Path("/home/joe/code/futon0/analysis/audits/rnode-tree/rnode-definitions.edn")
RNODE_VOCABULARY = Path("/home/joe/code/futon0/analysis/audits/rnode-tree/rnode-vocabulary.json")
RNODE_STAGES = Path("/home/joe/code/p4ng/empirics-futon/control-stages.edn")
INTENT_VOCABULARY = Path.home() / ".emacs-graph/session-turn-vocabulary.json"
ROLES = {"context", "condition", "contrast", "action", "rationale", "goal", "dependency"}
RNODE_DROP_REASONS = ("unknown_node", "invalid_operation", "empty_justification",
                      "inexact_span", "intent_phrase", "generic_cue")

MIN_CUE_WORDS = 2
"""Words of cue a sentence may carry however short it is.

Coverage is counted in WORDS, not characters. The rule's purpose is that the
display is not a wall of highlight, and what a reader sees marked is words.
Characters got it wrong three times: twice by refusing two ordinary cue
phrases in a short sentence, and once on `here: <url>`, where the only cue
worth making is a 48-character token that cannot be shortened and is a single
visual object. Two words, so a two-word sentence can still be marked."""


def required_text(value, field):
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{field} must be a nonempty string")
    return value


def template(request):
    return {"labeller": "", "reusable_cues": [], "rnode_cues": [], "sentences": [
        {"id": sentence["id"], "fragments": [], "unresolved_reason": ""}
        for sentence in request["sentences"]],
        "fragment_shape": {"start": 0, "end": 0, "text": "exact source fragment",
                           "intent": "meaningful intent", "target": "what the intent concerns",
                           "rationale": "why this reading fits", "relations": ["goal"],
                           "rnode": {"node": "R14", "quantity": "temperature tau / precision over policies",
                                     "operation": "set", "justification": "one line naming the quantity"},
                           "pattern_refs": [{"id": "family/pattern-name",
                                             "rationale": "why this pattern fits this fragment"}],
                           "display_cues": [{"start": 0, "end": 0, "text": "short keyword phrase"}],
                           "no_surface_cue": "explain here only if display_cues is empty"}}


def load_rnode_contract(definitions=RNODE_DEFINITIONS, stages=RNODE_STAGES):
    definitions_doc = edn_format.loads(Path(definitions).read_text())
    stages_doc = edn_format.loads(Path(stages).read_text())
    stage_by_node = {
        str(row[K("node")]): ("assurance" if row.get(K("band")) == K("assurance")
                              else str(row[K("stage")]).lower())
        for row in stages_doc[K("nodes")]
    }
    return {
        str(row[K("node")]): {
            "label": str(row[K("label")]),
            "quantity": str(row[K("quantity")]),
            "operations": {str(op) for op in row[K("operations")]},
            "stage": stage_by_node[str(row[K("node")])],
        }
        for row in definitions_doc[K("nodes")]
    }


def load_generic_cues(path=RNODE_VOCABULARY):
    return {str(cue).strip().lower()
            for cue in json.loads(Path(path).read_text())["excluded"]["generic"]}


def load_intent_phrases(path=INTENT_VOCABULARY):
    try:
        doc = json.loads(Path(path).read_text())
    except (OSError, ValueError, TypeError):
        return set()
    return {str(phrase).strip().lower()
            for group in doc.get("rules", []) for phrase in group[1:]}


def validate_rnode(value, contract, dropped):
    """Canonicalize one optional fragment R-node reading, or drop and count it."""
    if value is None:
        return None
    if not isinstance(value, dict) or value.get("node") not in contract:
        dropped["unknown_node"] += 1
        return None
    definition = contract[value["node"]]
    if value.get("operation") not in definition["operations"]:
        dropped["invalid_operation"] += 1
        return None
    justification = value.get("justification")
    if not isinstance(justification, str) or not justification.strip():
        dropped["empty_justification"] += 1
        return None
    return {"node": value["node"], "quantity": definition["quantity"],
            "operation": value["operation"], "justification": justification.strip()}


def validate(request, analysis, library=LIBRARY, rnode_contract=None,
             generic_cues=None, intent_phrases=None):
    labeller = required_text(analysis.get("labeller"), "labeller")
    rnode_contract = rnode_contract if rnode_contract is not None else load_rnode_contract()
    generic_cues = generic_cues if generic_cues is not None else load_generic_cues()
    intent_phrases = intent_phrases if intent_phrases is not None else load_intent_phrases()
    dropped = {reason: 0 for reason in RNODE_DROP_REASONS}
    accepted_rnodes = 0
    source = request["source_text"]
    expected = {s["id"]: s for s in request["sentences"]}
    sentences = analysis.get("sentences")
    if not isinstance(sentences, list):
        raise ValueError("sentences must be an array")
    ids = [s.get("id") for s in sentences]
    if len(ids) != len(set(ids)) or set(ids) != set(expected):
        raise ValueError("one analysis per source sentence is required, including unresolved ones")
    canonical = []
    for entry in sentences:
        sentence = expected[entry["id"]]
        fragments = entry.get("fragments")
        if not isinstance(fragments, list):
            raise ValueError("fragments must be an array")
        reason = entry.get("unresolved_reason", "")
        if not isinstance(reason, str):
            raise ValueError("unresolved_reason must be a string")
        if not fragments:
            required_text(reason, "unresolved_reason for an unclassified sentence")
        checked = []
        for fragment in fragments:
            start, end = fragment.get("start"), fragment.get("end")
            if (type(start) is not int or type(end) is not int
                    or not sentence["start"] <= start < end <= sentence["end"]
                    or source[start:end] != fragment.get("text")):
                raise ValueError("fragment offsets/text must match their source sentence exactly")
            item = {key: fragment[key] for key in ("start", "end", "text")}
            item["intent"] = required_text(fragment.get("intent"), "intent")
            if not re.fullmatch(r"[a-z][a-z0-9_-]*", item["intent"]):
                raise ValueError("intent must be a vocabulary label, not a sentence")
            target = fragment.get("target")
            item["target"] = (None if item["intent"] == "withdraw" and target is None
                              else required_text(target, "target"))
            item["rationale"] = required_text(fragment.get("rationale"), "rationale")
            roles = fragment.get("relations")
            if not isinstance(roles, list) or not roles or any(r not in ROLES for r in roles):
                raise ValueError("relations must name at least one documented structural role")
            item["relations"] = roles
            cues = fragment.get("display_cues")
            if not isinstance(cues, list):
                raise ValueError("display_cues must be an explicit array, separate from interpretation spans")
            checked_cues = []
            for cue in cues:
                a, b = cue.get("start"), cue.get("end")
                if (type(a) is not int or type(b) is not int or not start <= a < b <= end
                        or source[a:b] != cue.get("text")):
                    raise ValueError("display cue offsets/text must match inside their interpretation span")
                phrase = source[a:b]
                if len(phrase) > 80 or len(phrase.split()) > 8 or "\n" in phrase:
                    raise ValueError("display cues must be short keyword phrases (at most 8 words / 80 characters)")
                checked_cues.append({"start": a, "end": b, "text": phrase})
            no_cue = fragment.get("no_surface_cue", "")
            if not checked_cues:
                required_text(no_cue, "no_surface_cue when intent has no explicit keyword")
            item["display_cues"] = checked_cues
            item["no_surface_cue"] = no_cue
            refs = fragment.get("pattern_refs", [])
            if not isinstance(refs, list):
                raise ValueError("pattern_refs must be an array")
            checked_refs = []
            for ref in refs:
                pid = required_text(ref.get("id"), "pattern id")
                # CJK is allowed: library/象/ is a real pattern family, and an
                # id that only accepts ASCII would quietly rule it out.
                if not re.fullmatch(
                        r"[\w-]+(?:/[\w'-]+)+", pid, re.UNICODE):
                    raise ValueError("invalid canonical pattern id")
                path = (library / (pid + ".flexiarg")).resolve()
                if not path.is_relative_to(library.resolve()) or not path.is_file():
                    raise ValueError(f"unknown canonical pattern: {pid}")
                content = path.read_text()
                # The loader (futon3a projection.clj) accepts @arg, @flexiarg
                # and @multiarg as the id line; requiring @flexiarg alone made
                # 28 library patterns uncitable (software-design/adapter-pattern
                # among them) while the store ingested them fine.
                if not re.search(r"^@(?:arg|flexiarg|multiarg)\s+" + re.escape(pid) + r"\s*$", content, re.M):
                    raise ValueError(f"pattern declaration does not match: {pid}")
                checked_refs.append({"id": pid, "rationale": required_text(ref.get("rationale"), "pattern fit"),
                                     "status": "candidate", "source_sha256": hashlib.sha256(content.encode()).hexdigest()})
            item["pattern_refs"] = checked_refs

            # What the translator considered and turned down. A citation says
            # one pattern fits; a rejection says a near neighbour does not, and
            # why. Retrieval returns a top hit for every query, so the second
            # kind is what tells the boundary between two patterns -- and until
            # now it survived only in the reply prose and was thrown away.
            rejected = fragment.get("pattern_rejections", [])
            if not isinstance(rejected, list):
                raise ValueError("pattern_rejections must be an array")
            checked_rejections = []
            for ref in rejected:
                pid = required_text(ref.get("id"), "rejected pattern id")
                path = (library / (pid + ".flexiarg")).resolve()
                if not path.is_relative_to(library.resolve()) or not path.is_file():
                    raise ValueError(f"unknown rejected pattern: {pid}")
                checked_rejections.append(
                    {"id": pid,
                     "reason": required_text(ref.get("reason"),
                                             "why the rejected pattern does not fit"),
                     "query": (ref.get("query") or "").strip()})
            item["pattern_rejections"] = checked_rejections
            rnode = validate_rnode(fragment.get("rnode"), rnode_contract, dropped)
            if rnode:
                item["rnode"] = rnode
                accepted_rnodes += 1
            checked.append(item)
        # Check the union across all fragments so dividing a sentence into
        # many short spans cannot recreate total underlining.
        if len(source[sentence["start"]:sentence["end"]].split()) > 8:
            covered = sum(len(cue["text"].split())
                          for item in checked for cue in item["display_cues"])
            total = len(source[sentence["start"]:sentence["end"]].split())
            # At most half a sentence's WORDS, and at least two however short
            # it is. Counting characters refused three correct markings: two
            # short sentences carrying two ordinary cue phrases, and
            # "here: <url>", whose only useful cue is one 48-character token.
            # Counting words accepts all three and still refuses marking 7 of
            # 10 words, which is what the rule is for.
            budget = max(total // 2, MIN_CUE_WORDS)
            if covered > budget:
                # Say which sentence and by how much. The bare refusal cost
                # claude-1 a dozen retries and kimi-1 two on its first turn:
                # the rule is easy to satisfy and impossible to aim at when
                # the error names neither the sentence nor the overshoot.
                marked = sorted(cue["text"] for item in checked
                                for cue in item["display_cues"])
                raise ValueError(
                    f"display cues must leave most of a sentence unmarked: "
                    f"{sentence['id']} marks {covered} of {total} words "
                    f"({100 * covered // total}%); the budget here is {budget} "
                    f"words, so drop about {covered - budget}. "
                    f"Cues on it: " + ", ".join(repr(m) for m in marked))
        canonical.append({"id": entry["id"], "fragments": checked, "unresolved_reason": reason})
    reusable = analysis.get("reusable_cues", [])
    if not isinstance(reusable, list):
        raise ValueError("reusable_cues must be an array")
    learned = []
    for cue in reusable:
        start, end = cue.get("start"), cue.get("end")
        if (type(start) is not int or type(end) is not int or not 0 <= start < end <= len(source)
                or source[start:end] != cue.get("text")):
            raise ValueError("reusable cue must be an exact source span")
        phrase = source[start:end]
        if len(phrase) > 80 or len(phrase.split()) > 8 or "\n" in phrase:
            raise ValueError("reusable cue must be a short phrase")
        intent = required_text(cue.get("intent"), "reusable cue intent")
        if not re.fullmatch(r"[a-z][a-z0-9_-]*", intent):
            raise ValueError("invalid reusable intent")
        learned.append({"start": start, "end": end, "text": phrase, "intent": intent,
                        "rationale": required_text(cue.get("rationale"), "reuse rationale")})
    proposed_rnode_cues = analysis.get("rnode_cues", [])
    if not isinstance(proposed_rnode_cues, list):
        raise ValueError("rnode_cues must be an array")
    checked_rnode_cues = []
    for cue in proposed_rnode_cues:
        if not isinstance(cue, dict) or cue.get("node") not in rnode_contract:
            dropped["unknown_node"] += 1
            continue
        definition = rnode_contract[cue["node"]]
        if cue.get("operation") not in definition["operations"]:
            dropped["invalid_operation"] += 1
            continue
        justification = cue.get("justification")
        if not isinstance(justification, str) or not justification.strip():
            dropped["empty_justification"] += 1
            continue
        start, end = cue.get("start"), cue.get("end")
        if (type(start) is not int or type(end) is not int
                or not 0 <= start < end <= len(source)
                or source[start:end] != cue.get("text")):
            dropped["inexact_span"] += 1
            continue
        phrase = source[start:end]
        normalized = phrase.strip().lower()
        if normalized in intent_phrases:
            dropped["intent_phrase"] += 1
            continue
        if normalized in generic_cues:
            dropped["generic_cue"] += 1
            continue
        checked_rnode_cues.append({
            "start": start, "end": end, "text": phrase, "node": cue["node"],
            "operation": cue["operation"], "justification": justification.strip(),
            "label": definition["label"], "stage": definition["stage"],
        })
    return {"version": 2, "status": "analyzed", "method": "agent-interpretation",
            "interpretation_version": request.get("interpretation_version", 1),
            "vocabulary_version": request.get("vocabulary_version", 1),
            "human_approved": False, "labeller": labeller, "reusable_cues": learned,
            "rnode_cues": checked_rnode_cues,
            "rnode_validation": {"accepted_fragments": accepted_rnodes,
                                 "accepted_cues": len(checked_rnode_cues),
                                 "dropped": dropped},
            "evidence_id": request.get("evidence_id"),
            "created_at": datetime.now(timezone.utc).isoformat(), "source_text": source,
            "source_sha256": hashlib.sha256(source.encode()).hexdigest(),
            "offset_unit": request["offset_unit"], "sentences": canonical}


def complete(request_path, analysis):
    request_path = Path(request_path)
    request = json.loads(request_path.read_text())
    result = validate(request, analysis)
    result["request_file"] = str(request_path.resolve())
    result["request_sha256"] = hashlib.sha256(request_path.read_bytes()).hexdigest()
    output = Path(str(request_path) + ".analysis.json")
    # Exclusive creation refuses accidental replacement of an earlier interpretation.
    # Write a private temporary inode and publish it atomically via a hard link.
    import os
    import tempfile
    fd, name = tempfile.mkstemp(dir=output.parent, prefix=".analysis-")
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(result, stream, ensure_ascii=False, indent=2)
            stream.write("\n")
        os.link(name, output)
    finally:
        os.unlink(name)
    # Say so on the REQUEST too. Until now completion was visible only as the
    # existence of a sibling file, so a finished analysis and a pending one
    # were the same record -- 148 of them, which I mistook for a backlog.
    # The analysis is the authority; this is the flag that makes it findable.
    try:
        request["analysis_status"] = "analyzed"
        request["analysis_file"] = str(output.resolve())
        tmp = Path(str(request_path) + ".tmp")
        tmp.write_text(json.dumps(request, ensure_ascii=False, indent=1) + "\n")
        tmp.replace(request_path)
    except Exception:
        pass          # the analysis is published; a flag is not worth losing it over
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=["template", "complete"])
    parser.add_argument("request", type=Path)
    parser.add_argument("analysis", type=Path, nargs="?")
    args = parser.parse_args()
    try:
        if args.action == "template":
            print(json.dumps(template(json.loads(args.request.read_text())), ensure_ascii=False, indent=2))
        elif args.analysis is None:
            parser.error("complete requires an analysis JSON file")
        else:
            print(complete(args.request, json.loads(args.analysis.read_text())))
    except (ValueError, KeyError, TypeError, OSError) as error:
        parser.exit(1, f"Analysis not published: {error}\n")


if __name__ == "__main__":
    main()
