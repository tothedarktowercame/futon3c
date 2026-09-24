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

LIBRARY = Path(__file__).resolve().parents[2] / "futon3" / "library"
ROLES = {"context", "condition", "contrast", "action", "rationale", "goal", "dependency"}

MIN_CUE_BUDGET = 40
"""Non-space characters of cue a sentence may carry however short it is.

The coverage rule is that cues leave most of a sentence unmarked, which is
right for a long one and wrong for a short one: half of a 64-character
sentence is 32, and two ordinary cue phrases are already 37. About two
modest cues, so a short sentence can still be marked where it matters."""


def required_text(value, field):
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{field} must be a nonempty string")
    return value


def template(request):
    return {"labeller": "", "reusable_cues": [], "sentences": [
        {"id": sentence["id"], "fragments": [], "unresolved_reason": ""}
        for sentence in request["sentences"]],
        "fragment_shape": {"start": 0, "end": 0, "text": "exact source fragment",
                           "intent": "meaningful intent", "target": "what the intent concerns",
                           "rationale": "why this reading fits", "relations": ["goal"],
                           "pattern_refs": [{"id": "family/pattern-name",
                                             "rationale": "why this pattern fits this fragment"}],
                           "display_cues": [{"start": 0, "end": 0, "text": "short keyword phrase"}],
                           "no_surface_cue": "explain here only if display_cues is empty"}}


def validate(request, analysis, library=LIBRARY):
    labeller = required_text(analysis.get("labeller"), "labeller")
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
            for field in ("intent", "target", "rationale"):
                item[field] = required_text(fragment.get(field), field)
            if not re.fullmatch(r"[a-z][a-z0-9_-]*", item["intent"]):
                raise ValueError("intent must be a vocabulary label, not a sentence")
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
            checked.append(item)
        # Check the union across all fragments so dividing a sentence into
        # many short spans cannot recreate total underlining.
        if len(source[sentence["start"]:sentence["end"]].split()) > 8:
            covered = {i for item in checked for cue in item["display_cues"]
                       for i in range(cue["start"], cue["end"]) if not source[i].isspace()}
            total = sum(not c.isspace() for c in source[sentence["start"]:sentence["end"]])
            # Half of a long sentence is generous; half of a short one is not
            # two cue phrases. kimi-1 dropped a correct cue from "I'd like to
            # move that through section by section to a successful conclusion"
            # -- 37 of 64 characters, 57% -- because the rule was written for
            # long sentences and applied to every sentence. The floor is about
            # two modest cues, and 85% of a short sentence is still refused.
            budget = max(total // 2, MIN_CUE_BUDGET)
            if len(covered) > budget:
                # Say which sentence and by how much. The bare refusal cost
                # claude-1 a dozen retries and kimi-1 two on its first turn:
                # the rule is easy to satisfy and impossible to aim at when
                # the error names neither the sentence nor the overshoot.
                marked = sorted(cue["text"] for item in checked
                                for cue in item["display_cues"])
                raise ValueError(
                    f"display cues must leave most of a sentence unmarked: "
                    f"{sentence['id']} marks {len(covered)} of {total} non-space "
                    f"characters ({100 * len(covered) // total}%); the budget here "
                    f"is {budget}, so drop about {len(covered) - budget}. "
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
    return {"version": 2, "status": "analyzed", "method": "agent-interpretation",
            "human_approved": False, "labeller": labeller, "reusable_cues": learned,
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
