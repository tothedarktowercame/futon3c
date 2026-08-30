#!/usr/bin/env python3
"""fingerprint_audit.py — apply the whitepaper's witness standard to every
recorded memory USE in a campaign.

The standard (retrieval-whitepaper-v3 §3.1, set by a94A09): a USE claim counts
when the committed artifact carries the memory's fingerprint; prose attribution
alone is design signal, never outcome data. Today nothing in the frame machine
checks this -- `:used-ids` is a Student self-report validated only for
membership in the controller-derived surfaced set.

This script does the check offline, per (frame, attempt, memory):

  tokens  the Lean identifiers the memory names (its :key-api, its backticked
          spans, and dotted/underscored names in its body), and how many of
          them occur in the attempt's archived closing source;
  paste   the longest run of consecutive non-trivial memory-body lines that
          appear verbatim in that source.

Both are reported because they are different evidence. A memory whose named
APIs appear in the artifact while its body does not is a fingerprinted USE.
A novel token witnesses that use only when it occurs in fewer than 5% of the
problem files on ``origin/master``. Common tokens remain visible as weak
evidence, but cannot establish a fingerprint by themselves.
A memory whose body appears verbatim is a paste -- the f29/f30 failure mode
that `codex-scribe-v1` was written to stop -- and should not be counted as
the same kind of result.

Reviewed ``:regulative`` uses are reported as ``not-adjudicable-by-token``.
Their intended effect is a process or strategy change, for which absence of a
Lean identifier is not negative evidence.  ``:substitutive`` uses retain the
artifact-token standard.  Historical uses without a recorded kind retain the
legacy verdict; the audit never guesses a kind from prose.

BOTH ARE MEASURED AGAINST THE BASE FILE THE STUDENT WAS HANDED, not against
the empty string. An identifier already present in the base problem file is
not evidence that a memory put it there -- the Student would have read it in
the file regardless. Only tokens and lines NOVEL to the attempt's source
count. Without this subtraction the check reports a high hit rate that means
nothing, since memory, base file and certified head all discuss the same
mathematics in the same vocabulary. The base is resolved from the attempt
receipt's own `:base-revision` + `:problem-path` via apm-lean.

Reads only: campaign frame records on disk + the substrate evidence endpoint.

Usage: python3 fingerprint_audit.py [--campaign ID] [--write PATH] [--json]
"""
import argparse, json, math, os, re, subprocess, sys, urllib.request
from collections import Counter

SUBSTRATE = os.environ.get("FUTON1B_BASE", "http://127.0.0.1:7073")
CAMPAIGNS = os.environ.get("APM_CAMPAIGNS", os.path.expanduser("~/code/futon3c/data/apm-campaigns"))
APM_LEAN = os.environ.get("APM_LEAN", os.path.expanduser("~/code/apm-lean"))

USED_IDS = re.compile(r":used-ids\s*\[([^\]]*)\]")
ID_STR = re.compile(r'"(e-[^"]+)"')
PROBLEM_ID = re.compile(r':problem-id\s*"([^"]+)"')
BASE_REV = re.compile(r':base-revision\s*"([0-9a-f]{7,})"')
PROBLEM_PATH = re.compile(r':problem-path\s*"([^"]+)"')
MEMORY_USE_KINDS = re.compile(r":memory-use/kinds\s*\{([^}]*)\}", re.DOTALL)
MEMORY_USE_KIND_PAIR = re.compile(
    r'"(e-[^"]+)"\s+:(substitutive|regulative)')
MEMORY_USE_REASON_KIND = re.compile(
    r':memory-id\s+"(e-[^"]+)"(?:(?!:memory-id).){0,1000}?'
    r':memory-use/kind\s+:(substitutive|regulative)', re.DOTALL)
FRAME_VOID_EVENT = re.compile(r":event/type\s+:frame/stopped")
FRAME_VOID_CERTIFICATE = re.compile(r":certificate/type\s+:frame-void")
VOID_CLASSIFICATION = re.compile(r":classification\s+:([A-Za-z0-9_-]+)")
ACCESSIBLE_IDS = re.compile(r":accessible-memory-ids\s*\[([^\]]*)\]", re.DOTALL)
CASCADE_USED_IDS = re.compile(r":used-via-cascade\s*\[([^\]]*)\]", re.DOTALL)
EVIDENCE_AUTHOR = re.compile(r':evidence/author\s+"([^"]+)"')
EVIDENCE_SUBJECT = re.compile(
    r':evidence/subject\s+\{[^{}]*:ref/id\s+"([^"]+)"[^{}]*\}')
EVIDENCE_KIND = re.compile(r':kind\s+:([A-Za-z0-9_-]+)')
REVIEW_MEMORY_USE_KIND = re.compile(
    r':review\s+\{[^{}]*:memory-use/kind\s+:(substitutive|regulative)[^{}]*\}',
    re.DOTALL)
MANIFEST_REVISION = re.compile(
    r':apparatus\s+\{[^{}]*:revision\s+"([0-9a-f]+)"', re.DOTALL)
MANIFEST_PIN = re.compile(r':pin/id\s+"([0-9a-f]+)"')
MANIFEST_GUIDE_CARD = re.compile(
    r':guide\s+\{:path\s+"([^"]+)"\s*,?\s*:blob\s+"([0-9a-f]+)"\}')
FRAME_AUTHOR = re.compile(r"^f(\d+)-(?:guide|scribe)$")

# A Lean-ish identifier: letters/digits/underscore/dot/prime, at least one
# underscore or interior dot, no hyphen or slash (those are Clojure/EDN keys).
LEAN_TOKEN = re.compile(r"[A-Za-z][A-Za-z0-9_'.]{5,}")
# English/EDN noise that survives the shape filter.
STOPWORDS = {
    "evidence.body", "evidence.id", "evidence.at", "evidence.type",
    "evidence.author", "evidence.tags", "evidence.subject", "evidence.claim",
    "how_to_apply", "key_api", "capture_kind", "scribe_author", "draft_source",
    "runner_model", "statement_integrity", "all_file_declarations",
}

# A token present in at least one problem out of twenty is common enough that
# seeing it does not identify a particular memory. This corpus rule retains
# rare bare lemma names while demoting ubiquitous bare tactics without a
# hand-maintained tactic list.
MAX_WITNESS_DOC_FRACTION = 0.05


def recorded_use_kinds(receipt):
    """Return reviewed use kinds carried by a dispatch/attempt receipt.

    New receipts carry the compact ``:memory-use/kinds`` map.  The inclusion
    reason representation is accepted too, because it is the auditable source
    from which that map is derived.  Missing classification deliberately stays
    missing: historical rows retain the old token verdict instead of being
    guessed from prose.
    """
    pairs = []
    for block in MEMORY_USE_KINDS.findall(receipt):
        pairs.extend(MEMORY_USE_KIND_PAIR.findall(block))
    pairs.extend(MEMORY_USE_REASON_KIND.findall(receipt))
    return {mid: kind for mid, kind in pairs}


def ids_in_block(pattern, receipt):
    """Return unique evidence ids from every durable vector named by pattern."""
    out = []
    for block in pattern.findall(receipt):
        for mid in ID_STR.findall(block):
            if mid not in out:
                out.append(mid)
    return out


def durable_delivery_route(receipt, memory_id):
    """Classify only delivery routes carried by the attempt receipt.

    Cascade is more specific than shelf.  Search prose is deliberately ignored:
    unless a search-result receipt names the id, the report says ``unknown``.
    """
    if memory_id in ids_in_block(CASCADE_USED_IDS, receipt):
        return "cascade"
    if memory_id in ids_in_block(ACCESSIBLE_IDS, receipt):
        return "shelf"
    return "unknown"


def memory_metadata(raw):
    """Read provenance fields that the evidence record actually carries."""
    author = EVIDENCE_AUTHOR.search(raw or "")
    subject = EVIDENCE_SUBJECT.search(raw or "")
    kind = EVIDENCE_KIND.search(body_text(raw))
    use_kind = REVIEW_MEMORY_USE_KIND.search(raw or "")
    return {"origin-author": author.group(1) if author else None,
            "origin-problem": subject.group(1) if subject else None,
            "memory-kind": kind.group(1) if kind else None,
            "memory-use-kind": use_kind.group(1) if use_kind else None}


def transfer_stratum(frame, problem, metadata):
    """Describe provenance distance without claiming that a memory caused work."""
    author = metadata.get("origin-author")
    origin_problem = metadata.get("origin-problem")
    author_frame = FRAME_AUTHOR.match(author or "")
    origin_frame = f"f{author_frame.group(1)}" if author_frame else None
    if origin_frame == frame:
        return "within-frame", origin_frame
    if origin_problem and origin_problem == problem:
        return "prior-frame-same-problem", origin_frame
    if origin_problem and origin_problem != problem:
        return "cross-problem", origin_frame
    return "unknown", origin_frame


def frame_instrument(frame_path):
    """Return pinned apparatus identifiers; do not infer a serving model."""
    manifest = os.path.join(frame_path, "manifest.edn")
    try:
        text = open(manifest, encoding="utf-8", errors="replace").read()
    except OSError:
        return {"apparatus-revision": None, "apparatus-pin": None,
                "guide-card-path": None, "guide-card-blob": None,
                "guide-model": None}
    rev = MANIFEST_REVISION.search(text)
    pin = MANIFEST_PIN.search(text)
    card = MANIFEST_GUIDE_CARD.search(text)
    return {"apparatus-revision": rev.group(1) if rev else None,
            "apparatus-pin": pin.group(1) if pin else None,
            "guide-card-path": card.group(1) if card else None,
            "guide-card-blob": card.group(2) if card else None,
            # Manifests pin code/cards but not the historical effective cast.
            "guide-model": None}


def stratified_counts(rows, field):
    """Count diagnostic and experimental rows separately for one dimension."""
    experimental = Counter()
    diagnostic = Counter()
    for row in rows:
        value = row.get(field) or "unknown"
        diagnostic[value] += 1
        if row.get("experimental-evidence"):
            experimental[value] += 1
    return {"experimental": dict(sorted(experimental.items())),
            "diagnostic-all-rows": dict(sorted(diagnostic.items()))}


def artifact_verdict(use_kind, best, witnessing, novel, hits):
    """Classify one use without treating regulative help as token failure."""
    if use_kind == "regulative":
        return "not-adjudicable-by-token"
    if best >= 3:
        return "paste"
    if witnessing:
        return "fingerprinted"
    if novel:
        return "weak-fingerprint"
    if hits:
        return "already-in-base"
    return "unwitnessed"


def get_text(path, timeout=60):
    req = urllib.request.Request(SUBSTRATE + path, headers={"Accept": "application/edn"})
    return urllib.request.urlopen(req, timeout=timeout).read().decode("utf-8", "replace")


def fetch_memory(mid, cache={}):
    if mid not in cache:
        try:
            cache[mid] = get_text("/api/alpha/evidence/" + urllib.parse.quote(mid, safe=""))
        except Exception as e:
            cache[mid] = ""
            print(f"  ! fetch failed {mid}: {e}", file=sys.stderr)
    return cache[mid]


def body_text(raw):
    """The memory body, with EDN string escapes turned back into text."""
    if not raw:
        return ""
    i = raw.find(":evidence/body")
    j = raw.find(":evidence/session-id")
    seg = raw[i:j] if 0 <= i < j else raw
    return seg.replace("\\n", "\n").replace('\\"', '"')


def tokens_of(body):
    """Lean identifiers the memory names, most-specific first."""
    out = set()
    for m in LEAN_TOKEN.finditer(body):
        t = m.group(0).strip(".").strip("'")
        if "-" in t or "/" in t:
            continue
        if t.lower() in STOPWORDS or t in STOPWORDS:
            continue
        # Require a real word-join: an underscore, a dotted qualified name, or
        # a multi-hump CamelCase name. Mathlib is full of single-word type
        # names carrying neither an underscore nor a dot -- NNReal, ENNReal,
        # MeasureTheory -- and dropping them made a memory whose whole content
        # is `open scoped NNReal ENNReal` score zero tokens, so it read
        # `unwitnessed` in every frame that surfaced it no matter what the
        # student did. Prose capitalises one letter (Trigger, Fourier, Lean);
        # a Lean type name carries two, which is the discriminator.
        if "_" not in t and "." not in t:
            caps = sum(1 for c in t if c.isupper())
            if not (caps >= 2 and any(c.islower() for c in t)):
                continue
        # drop pure prose that happens to be dotted (sentence ends)
        if re.fullmatch(r"[a-z]+\.[a-z]+", t):
            continue
        out.add(t)
    return sorted(out, key=lambda s: (-len(s), s))


def base_file(rev, path, cache={}):
    """The problem file as the Student received it, from apm-lean."""
    key = (rev, path)
    if key not in cache:
        try:
            cache[key] = subprocess.run(
                ["git", "show", f"{rev}:{path}"], cwd=APM_LEAN,
                capture_output=True, text=True, timeout=30, check=True).stdout
        except Exception as e:
            cache[key] = None
            print(f"  ! base blob failed {rev}:{path}: {e}", file=sys.stderr)
    return cache[key]


def master_problem_count(cache={}):
    """Number of problem Main files in the source corpus."""
    if "count" not in cache:
        p = subprocess.run(
            ["git", "ls-tree", "-r", "--name-only", "origin/master", "--", "problems"],
            cwd=APM_LEAN, capture_output=True, text=True, timeout=30, check=True)
        cache["count"] = sum(
            path.endswith("/lean/Main.lean") for path in p.stdout.splitlines())
    return cache["count"]


def token_master_frequency(token, cache={}):
    """Problem-document frequency of a literal token on ``origin/master``."""
    if token not in cache:
        p = subprocess.run(
            ["git", "grep", "-l", "-F", token, "origin/master", "--",
             "problems/*/lean/Main.lean"],
            cwd=APM_LEAN, capture_output=True, text=True, timeout=30)
        if p.returncode not in (0, 1):
            raise subprocess.CalledProcessError(
                p.returncode, p.args, output=p.stdout, stderr=p.stderr)
        count = len([line for line in p.stdout.splitlines() if line])
        total = master_problem_count()
        cache[token] = (count, total, count / total if total else 0.0)
    return cache[token]


def assess_novel_token(token):
    """Return an auditable corpus-frequency witness decision for ``token``."""
    count, total, frequency = token_master_frequency(token)
    witnessing = frequency < MAX_WITNESS_DOC_FRACTION
    information_bits = -math.log2(frequency) if frequency else math.log2(total + 1)
    return {"token": token,
            "master-problem-files": count,
            "master-problem-files-total": total,
            "master-document-frequency": frequency,
            "information-bits": information_bits,
            "witnessing": witnessing,
            "reason": ("rare-in-master-problem-corpus" if witnessing else
                       "common-in-master-problem-corpus")}


def paste_run(body, novel_lines):
    """Longest run of consecutive body lines appearing verbatim and NOVEL."""
    best = cur = 0
    n_hit = 0
    for line in body.splitlines():
        s = line.strip()
        if len(s) > 12 and s in novel_lines:
            cur += 1
            n_hit += 1
            best = max(best, cur)
        else:
            cur = 0
    return best, n_hit


def frame_exclusion(frame_path):
    """Return the durable reason a frame is excluded from experiment totals.

    The ledger remains the authority.  In particular, the existence of a
    student receipt is not evidence that a later apparatus-invalidating void
    should count: the receipt remains available for diagnosis while the frame
    contributes no attempts or memory uses to the experiment.
    """
    ledger = os.path.join(frame_path, "ledger.edn")
    try:
        with open(ledger, encoding="utf-8", errors="replace") as stream:
            text = stream.read()
    except OSError:
        return None
    for line in text.splitlines():
        if FRAME_VOID_EVENT.search(line) and FRAME_VOID_CERTIFICATE.search(line):
            classification = VOID_CLASSIFICATION.search(line)
            return {"reason": "void-frame",
                    "classification": (classification.group(1)
                                           if classification else "unspecified")}
    return None


def attempts(campaign_dir):
    """Attempt records, including any reviewed per-memory use classification."""
    out = []
    for frame_dir in sorted(os.listdir(campaign_dir)):
        frame_path = os.path.join(campaign_dir, frame_dir)
        live = os.path.join(frame_path, "live")
        if not os.path.isdir(live):
            continue
        frame = frame_dir.rsplit("-", 1)[-1]
        exclusion = frame_exclusion(frame_path)
        instrument = frame_instrument(frame_path)
        for n in (1, 2, 3):
            rec = os.path.join(live, f"student-attempt-{n}.edn")
            if not os.path.exists(rec):
                continue
            with open(rec, encoding="utf-8", errors="replace") as stream:
                t = stream.read()
            ids = []
            for block in USED_IDS.findall(t):
                for i in ID_STR.findall(block):
                    if i not in ids:
                        ids.append(i)
            pid = PROBLEM_ID.search(t)
            rev = BASE_REV.search(t)
            ppath = PROBLEM_PATH.search(t)
            srcdir = os.path.join(live, f"student-attempt-{n}-source")
            src = None
            if os.path.isdir(srcdir):
                files = [f for f in os.listdir(srcdir) if f.endswith(".lean")]
                if files:
                    src = os.path.join(srcdir, files[0])
            out.append((frame, n, pid.group(1) if pid else "?", ids,
                        t, instrument, src,
                        rev.group(1) if rev else None,
                        ppath.group(1) if ppath else None, exclusion))
    return out


def audit_campaign(cdir, campaign):
    """Build diagnostic rows and experimental totals for one campaign."""
    rows, excluded_frames = [], []
    summary = {"attempts": 0, "attempts-with-used": 0, "use-events": 0,
               "fingerprinted": 0, "paste": 0, "already-in-base": 0,
               "weak-fingerprint": 0, "unwitnessed": 0,
               "not-adjudicable-by-token": 0,
               "no-source": 0, "no-base": 0, "unfetchable": 0,
               "excluded-void-frames": 0, "excluded-attempts": 0,
               "excluded-use-events": 0}

    excluded_seen = set()
    for (frame, n, pid, ids, receipt, instrument, src, rev, ppath,
         exclusion) in attempts(cdir):
        if exclusion:
            if frame not in excluded_seen:
                excluded_seen.add(frame)
                summary["excluded-void-frames"] += 1
                excluded_frames.append({"frame": frame,
                                        "classification": exclusion["classification"],
                                        "reason": exclusion["reason"]})
            summary["excluded-attempts"] += 1
            summary["excluded-use-events"] += len(ids)
            for mid in ids:
                raw = fetch_memory(mid)
                metadata = memory_metadata(raw)
                stratum, origin_frame = transfer_stratum(frame, pid, metadata)
                rows.append({"frame": frame, "attempt": n, "problem": pid,
                             "memory": mid, "source": (os.path.basename(src)
                                                        if src else None),
                             "memory-use-kind": metadata["memory-use-kind"],
                             **metadata,
                             "origin-frame": origin_frame,
                             "transfer-stratum": stratum,
                             "delivery-route": durable_delivery_route(receipt, mid),
                             "instrument": instrument,
                             "verdict": "excluded-void-frame",
                             "experimental-evidence": False,
                             "exclusion-classification": exclusion["classification"]})
            continue

        summary["attempts"] += 1
        if ids:
            summary["attempts-with-used"] += 1
        if src:
            with open(src, encoding="utf-8", errors="replace") as stream:
                source = stream.read()
        else:
            source = None
        base = base_file(rev, ppath) if (rev and ppath) else None
        novel_lines = set()
        if source is not None and base is not None:
            base_lines = {l.strip() for l in base.splitlines()}
            novel_lines = {l.strip() for l in source.splitlines()
                           if len(l.strip()) > 12 and l.strip() not in base_lines}
        for mid in ids:
            summary["use-events"] += 1
            raw = fetch_memory(mid)
            body = body_text(raw)
            toks = tokens_of(body)
            metadata = memory_metadata(raw)
            stratum, origin_frame = transfer_stratum(frame, pid, metadata)
            row = {"frame": frame, "attempt": n, "problem": pid, "memory": mid,
                   "source": os.path.basename(src) if src else None,
                   "tokens-named": len(toks),
                   **metadata,
                   "origin-frame": origin_frame,
                   "transfer-stratum": stratum,
                   "delivery-route": durable_delivery_route(receipt, mid),
                   "instrument": instrument,
                   "experimental-evidence": True}
            if not raw:
                row["verdict"] = "unfetchable"
                summary["unfetchable"] += 1
            elif source is None:
                row["verdict"] = "no-source"
                summary["no-source"] += 1
            elif base is None:
                row["verdict"] = "no-base"
                summary["no-base"] += 1
            else:
                hits = [t for t in toks if t in source]
                novel = [t for t in hits if t not in base]
                assessments = [assess_novel_token(t) for t in novel]
                witnessing = [x["token"] for x in assessments if x["witnessing"]]
                nonwitnessing = [x["token"] for x in assessments if not x["witnessing"]]
                best, nhit = paste_run(body, novel_lines)
                row.update({"tokens-hit": len(hits), "tokens-novel": len(novel),
                            "novel-hits": novel[:12],
                            "witnessing-novel-hits": witnessing[:12],
                            "nonwitnessing-novel-hits": nonwitnessing[:12],
                            "novel-token-assessments": assessments,
                            "in-base-already": len(hits) - len(novel),
                            "paste-longest-run": best, "paste-lines-hit": nhit})
                row["verdict"] = artifact_verdict(
                    metadata["memory-use-kind"], best, witnessing, novel, hits)
                summary[row["verdict"]] += 1
            rows.append(row)

    strata = {field: stratified_counts(rows, field)
              for field in ("transfer-stratum", "delivery-route",
                            "memory-use-kind", "memory-kind")}
    apparatus = stratified_counts(
        [{**row,
          "instrument-boundary": (row.get("instrument") or {}).get(
              "apparatus-revision")}
         for row in rows], "instrument-boundary")
    return {"campaign": campaign, "substrate": SUBSTRATE,
            "standard": "retrieval-whitepaper-v3 3.1 (artifact carries the memory's fingerprint)",
            "causal-claim": "none; rows report selection and artifact association only",
            "experimental-population": "non-void frames only; void frames remain diagnostic",
            "stratified-use-events": {**strata, "apparatus-revision": apparatus},
            "unavailable-fields": {
                "guide-model": ("not pinned in historical frame manifests; do not infer "
                                "from current seat-cast or agent names"),
                "search-route": ("reported only when a durable id-bearing search receipt "
                                 "exists; prose search claims are not parsed")},
            "excluded-frames": excluded_frames,
            "token-witness-rule": {
                "measure": "origin/master problem-file document frequency",
                "witnessing-when": "frequency < 0.05",
                "threshold": MAX_WITNESS_DOC_FRACTION,
                "corpus-problem-files": master_problem_count()},
            "summary": summary, "rows": rows}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--campaign", default="jit-all-open-nontopology-v1")
    ap.add_argument("--write")
    ap.add_argument("--json", action="store_true")
    a = ap.parse_args()

    cdir = os.path.join(CAMPAIGNS, a.campaign)
    out = audit_campaign(cdir, a.campaign)
    if a.write:
        with open(a.write, "w") as stream:
            stream.write(json.dumps(out, indent=1))
        print("wrote", a.write, file=sys.stderr)
    if a.json:
        print(json.dumps(out, indent=1))
    else:
        for r in out["rows"]:
            print(f"{r['frame']:>4} a{r['attempt']} {r.get('verdict','?'):>15} "
                  f"kind={r.get('memory-use-kind') or '-':<12} "
                  f"novel={r.get('tokens-novel','-')}/{r.get('tokens-named','-'):<4} "
                  f"inbase={r.get('in-base-already','-'):<3} "
                  f"run={r.get('paste-longest-run','-'):<3} {r['memory'][:52]}")
        print("\n", json.dumps(out["summary"], indent=1))


if __name__ == "__main__":
    import urllib.parse
    main()
