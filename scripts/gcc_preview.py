#!/usr/bin/env python3
"""gcc_preview.py — M-repl-circus demo 1a: read-only GCC preview projector.

Projects a handful of work items into the demo-01 preview format
(~/notes/claude-13/demo-01-contract.md) under ~/storage/gcc-preview/, so Joe
can inspect and tweak the proposed record shapes before any store writes
happen.  READ-ONLY against every source; the only writes are under the
output directory.

Sources:
  - holes/ documents: item id, title, path, repo; state group computed the
    way Giraffe computes it (futon2 mission-registry classify-status +
    futon3c.giraffe.missions rule-table, replicated here).
  - Giraffe corpus generation (members.json + batch-*.jsonl): accepted
    readings with their source digests and needs edges.
  - Giraffe reread queue (SQLite, opened read-only): reading lineage, which
    readings a newer reading supersedes.
  - The :7094 workplan stack via /arxana/v1 query operations only
    (workplan/item-view, graph/query): names, serves, mentions, pending
    level, introductions.
  - Xiang turn records (~/.emacs-graph/session-turn-analysis): mentions.
  - futon7 holes/argument-graph/questions.edn: item -> question links.

Record shapes come from an EDN shapes file (default
holes/labs/M-repl-circus/gcc-shapes.edn): each type's output status
(built|proposed) and field names are taken from there, so renaming a field
in the shapes file renames it in the output.

Usage:
  python3 scripts/gcc_preview.py --items M-repl-circus,M-giraffe,...
      [--shapes PATH] [--out DIR] [--corpus DIR] [--queue DB]
      [--turns DIR] [--questions FILE] [--arxana URL | --arxana-fixture DIR | --no-arxana]

Every absence in the output is typed (contract rule 7).
"""

import argparse
import hashlib
import json
import os
import re
import sqlite3
import urllib.request
from datetime import datetime, timezone

# ------------------------------------------------------------------ EDN ---
# Minimal EDN reader: enough for the shapes file and questions.edn (maps,
# vectors, keywords, strings, integers, booleans, nil, symbols, comments).

class Keyword(str):
    pass

def _edn_tokens(text):
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c in " \t\r\n,":
            i += 1
        elif c == ";":
            j = text.find("\n", i)
            i = n if j < 0 else j + 1
        elif c in "{}":
            yield (c, c); i += 1
        elif c in "[]":
            yield (c, c); i += 1
        elif c == '"':
            j = i + 1
            buf = []
            while j < n:
                if text[j] == "\\":
                    buf.append(text[j + 1]); j += 2
                elif text[j] == '"':
                    j += 1; break
                else:
                    buf.append(text[j]); j += 1
            yield ("string", "".join(buf)); i = j
        else:
            j = i
            while j < n and text[j] not in " \t\r\n,{}[]\";":
                j += 1
            yield ("atom", text[i:j]); i = j

class _EdnReader:
    def __init__(self, text):
        self.tokens = list(_edn_tokens(text))
        self.pos = 0

    def read(self):
        kind, val = self.tokens[self.pos]
        self.pos += 1
        if kind == "{":
            out = {}
            while self.tokens[self.pos][0] != "}":
                k = self.read()
                v = self.read()
                out[k] = v
            self.pos += 1
            return out
        if kind == "[":
            out = []
            while self.tokens[self.pos][0] != "]":
                out.append(self.read())
            self.pos += 1
            return out
        if kind == "string":
            return val
        if val.startswith(":"):
            return Keyword(val[1:])
        if val == "nil":
            return None
        if val == "true":
            return True
        if val == "false":
            return False
        try:
            return int(val)
        except ValueError:
            return val  # symbol: keep as string

def edn_load(text):
    return _EdnReader(text).read()

# ------------------------------------------------- state group (Giraffe) --
# Replicates futon3c.giraffe.missions/leading-phrase + rule-table and
# futon2.aif.mission-registry/classify-status over a document's Status line.

STATUS_LINE = re.compile(
    r"^\s*(?:[-*]\s*)?(?:#+\s*)?(?:\*\*)?Status:?(?:\*\*)?\s*:?\s*(.+)$", re.I)
GATE_LINE = re.compile(
    r"^\s*(?:[-*]\s*)?(?:#+\s*)?(?:\*\*)?Gate:?(?:\*\*)?\s*:?\s*(.+)$", re.I)
OPERATOR_GATE = re.compile(
    r"\b(?:await|pending|acceptance|decision|ratification|operator|Joe)\b", re.I)

def leading_phrase(s):
    t = re.sub(r"^:+", "", s.strip())
    t = re.sub(r"\s+", " ", t)
    t = re.sub(r"( — .*|—.*| · .*|·.*|;.*|\(.*|\. .*$|\.$)", "", t)
    return t.strip()

RULE_TABLE = [
    ("started",   re.compile(r"^[*_`\s]*(head|identify|map|derive|argue|verify|instantiate|open)\b", re.I)),
    ("unstarted", re.compile(r"not[ -]yet[ -]implemented|specified|stub", re.I)),
    ("completed", re.compile(r"\bclosed\b|\bdone\b|\bcomplete\b|\bdelivered\b|\banswered\b|\bimplemented\b|\baccepted\b", re.I)),
    ("cancelled", re.compile(r"superseded|nonstarter|dropped|abandoned", re.I)),
    ("unstarted", re.compile(r"\bready\b|blocked[ -]on|\bproposed\b|\bdraft\b|not[ -]started|\bplanned\b|\bscoped\b|greenfield|\bdesign\b|\bchartered\b", re.I)),
    ("started",   re.compile(r"\bidentify(?:ing)?\b|\bmap(?:ping)?\b|\bderive(?:d|ing)?\b|\bargu(?:e|ing|ment)\b|\bverif(?:y|ying|ication)\b|\binstantiate\b|\bdocument(?:ation|ing)?\b|\bopen\b|\bactive\b|in[ -]progress|\bpartial\b|\bpart\b|\bdispatched\b|\bhead\b|\breview\b|\btesting\b|\bconditional\b|\bslice\b|\bpart\b", re.I)),
]

def status_to_group(text):
    if not text or not text.strip():
        return None
    phrase = leading_phrase(text)
    for group, rx in RULE_TABLE:
        if rx.search(phrase):
            return group
    return None

def classify_status(status_line):
    upper = (status_line or "").upper()
    lead = re.sub(r"^[\s>*#`_~-]+", "", upper).strip()
    m = re.search(r"[A-Z][A-Z-]*", lead)
    head = m.group(0) if m else ""
    held = re.search(
        r"^(?:OPEN|ACTIVE|PARTIAL|IDENTIFY|HEAD|INSTANTIATE|MAP|DERIVE|ARGUE|VERIFY)\s*(?:[-—–:/]|\()\s*(PARKED|FROZEN|DEFERRED)\b",
        lead)
    if "SPECIFIED, NOT YET IMPLEMENTED" in upper:
        return "draft"
    if head == "DRAFT":
        return "draft"
    if held:
        return "inactive"
    if any(head.startswith(p) for p in
           ("ARCHIVED", "PARKED", "SUPERSEDED", "ABANDONED", "DEFERRED", "FROZEN")):
        return "inactive"
    if any(head.startswith(p) for p in
           ("COMPLETE", "COMPLETED", "CLOSED", "DONE", "DISCHARGED", "ANSWERED", "DISSOLVED")):
        return "complete"
    if head == "ACTIVE":
        return "active"
    if head == "OPEN":
        return "open"
    if head == "PARTIAL":
        return "partial"
    if head == "IDENTIFY":
        return "identify"
    if head == "HEAD":
        return "open"
    if head in ("INSTANTIATE", "MAP", "DERIVE", "ARGUE", "VERIFY"):
        return "active"
    return "unknown"

def doc_status(lines):
    """(status_line, status_class, operator_gated) from the first 20 lines."""
    status_line = None
    for line in lines[:20]:
        m = STATUS_LINE.match(line)
        if m:
            status_line = m.group(1)
            break
    gated = any(OPERATOR_GATE.search(g.group(1))
                for g in (GATE_LINE.match(l) for l in lines) if g)
    if gated:
        return status_line, "operator-gated", True
    return status_line, classify_status(status_line), False

def state_group(status_line, status_class):
    """Giraffe's five state-group values (pass.clj), as missions.clj computes."""
    live = status_class not in ("complete", "inactive", "draft", "operator-gated")
    rank = status_to_group(status_line)
    if live:
        return "started" if rank == "started" else "unstarted"
    return "completed" if status_class == "complete" else "cancelled"

def doc_title(lines, fallback):
    for line in lines[:20]:
        m = re.match(r"^#\s+(.+)$", line)
        if m:
            return m.group(1).strip()
        m = re.match(r"^\*\*(?:Mission|Excursion|Ticket):\s*(.+?)\*\*\s*$", line)
        if m:
            return m.group(1).strip()
    return fallback

# ----------------------------------------------------------------- misc ---

def sha256_text(s):
    return hashlib.sha256(s.encode("utf-8")).hexdigest()

def record_id(type_key, identity):
    slug = re.sub(r"[^a-z0-9]+", "-", type_key.lower()).strip("-")
    return "preview/%s/%s" % (slug, sha256_text(identity)[:16])

def read_lines(path):
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read().splitlines()

# ---------------------------------------------------------------- corpus ---

def load_corpus(corpus_dir):
    """members.json + batch-*.jsonl of one corpus generation (resolve links)."""
    real = os.path.realpath(corpus_dir)
    with open(os.path.join(real, "members.json"), encoding="utf-8") as f:
        members = json.load(f)
    records = {}
    for name in sorted(os.listdir(real)):
        if re.match(r"batch-.*\.jsonl$", name):
            with open(os.path.join(real, name), encoding="utf-8") as f:
                for line in f:
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        rec = json.loads(line)
                    except ValueError:
                        continue
                    if isinstance(rec, dict) and rec.get("id"):
                        records[rec["id"]] = rec
    return members, records, real

def load_queue(queue_path):
    """reread_requests rows per target, oldest first (read-only)."""
    db = sqlite3.connect("file:%s?mode=ro" % queue_path, uri=True)
    try:
        rows = db.execute(
            "SELECT target_id, source_sha, state, requested_at_ms, "
            "claimed_by, updated_at_ms "
            "FROM reread_requests ORDER BY target_id, requested_at_ms").fetchall()
    finally:
        db.close()
    by_target = {}
    for target, sha, state, at, claimed, updated in rows:
        by_target.setdefault(target, []).append(
            {"source_sha": sha, "state": state, "requested_at_ms": at,
             "claimed_by": claimed, "updated_at_ms": updated})
    return by_target

def find_historical_readings(generations_dir, needed):
    """{(item, sha): reading record} from earlier corpus generations, for
    readings the current generation no longer carries.  Only lines naming a
    needed item are parsed."""
    found = {}
    if not needed or not os.path.isdir(generations_dir):
        return found
    wanted = {}
    for item, shas in needed.items():
        for sha in shas:
            wanted.setdefault(item.encode("utf-8"), set()).add(sha)
    for gen in sorted(os.listdir(generations_dir)):
        gdir = os.path.join(generations_dir, gen)
        if not os.path.isdir(gdir):
            continue
        for name in sorted(os.listdir(gdir)):
            if not re.match(r"batch-.*\.jsonl$", name):
                continue
            try:
                with open(os.path.join(gdir, name), "rb") as f:
                    blob = f.read()
            except OSError:
                continue
            hits = [i for i in wanted if i in blob]
            if not hits:
                continue
            for line in blob.decode("utf-8", "replace").splitlines():
                line = line.strip()
                if not line:
                    continue
                try:
                    rec = json.loads(line)
                except ValueError:
                    continue
                if not isinstance(rec, dict):
                    continue
                iid = rec.get("id")
                sha = (rec.get("source") or {}).get("sha256")
                if iid and sha and (iid, sha) not in found \
                        and sha in wanted.get(iid.encode("utf-8"), set()):
                    found[(iid, sha)] = rec
        if len(found) == sum(len(s) for s in needed.values()):
            break
    return found

# ---------------------------------------------------------------- arxana ---

def arxana_call(url, operation, body, principal="kimi-1/gcc-preview"):
    msg = {"protocol/version": "arxana.v1",
           "message/id": "gcc-preview-%s" % sha256_text(operation + json.dumps(body, sort_keys=True))[:12],
           "operation": ":" + operation,
           "principal/id": principal,
           "body": body}
    req = urllib.request.Request(
        url, data=json.dumps(msg).encode("utf-8"),
        headers={"Content-Type": "application/json"}, method="POST")
    with urllib.request.urlopen(req, timeout=20) as resp:
        return json.loads(resp.read().decode("utf-8"))

class ArxanaSource:
    """Query operations against :7094, or canned item-views from a fixture dir."""
    def __init__(self, url=None, fixture_dir=None):
        self.url = url
        self.fixture_dir = fixture_dir

    def item_view(self, item_id):
        if self.fixture_dir:
            path = os.path.join(self.fixture_dir, item_id + ".json")
            if not os.path.isfile(path):
                return None
            with open(path, encoding="utf-8") as f:
                return json.load(f)
        try:
            resp = arxana_call(self.url, "workplan/item-view", {"item/id": item_id})
        except Exception:
            return None
        return resp.get("ok")

    def introductions(self, entity_clusion_id):
        if self.fixture_dir:
            path = os.path.join(self.fixture_dir, "introductions.json")
            if not os.path.isfile(path):
                return []
            with open(path, encoding="utf-8") as f:
                return json.load(f).get(entity_clusion_id, [])
        try:
            resp = arxana_call(self.url, "graph/query",
                               {"query/type": ":role-reverse",
                                "edge/type": [":workplan/introduction", 1],
                                "endpoint/role": ":entity",
                                "endpoint/id": entity_clusion_id,
                                "time/order": ":asc"})
        except Exception as exc:
            return [{"error": "graph/query role-reverse failed: %s" % exc}]
        ok = resp.get("ok") or {}
        ids = ok.get("result/edge-ids") or []
        out = []
        for eid in ids:
            try:
                ent = arxana_call(self.url, "entity/get", {"entity/id": eid})
            except Exception:
                continue
            rec = (ent.get("ok") or {}).get("record")
            if rec:
                out.append(rec)
        return out

# ------------------------------------------------------------- questions ---

def question_links(questions_path, item_ids):
    """item id -> [question refs] from futon7 questions.edn (parsed, then
    matched against every string value of each question map)."""
    if not questions_path or not os.path.isfile(questions_path):
        return {i: [] for i in item_ids}, "questions file not found: %s" % questions_path
    try:
        with open(questions_path, encoding="utf-8") as f:
            doc = edn_load(f.read())
    except Exception as exc:
        return {i: [] for i in item_ids}, "questions.edn did not parse: %s" % exc

    def strings(x):
        if isinstance(x, str) and not isinstance(x, Keyword):
            yield x
        elif isinstance(x, dict):
            for k, v in x.items():
                yield from strings(v)
        elif isinstance(x, list):
            for v in x:
                yield from strings(v)

    out = {i: [] for i in item_ids}
    for q in doc.get(Keyword("questions"), []):
        qid = q.get(Keyword("q/id"))
        qkind = q.get(Keyword("q/kind"))
        for item in item_ids:
            hits = [name for name, s in
                    (("q/text", q.get(Keyword("q/text"))),
                     ("q/source", q.get(Keyword("q/source"))),
                     ("q/search-terms", q.get(Keyword("q/search-terms"))))
                    if s and any(item in t for t in strings(s))]
            if hits:
                out[item].append({"question_id": qid,
                                  "kind": str(qkind) if qkind else None,
                                  "matched_in": hits,
                                  "text": q.get(Keyword("q/text"))})
    return out, None

# ------------------------------------------------------------- turn mentions

def turn_mentions(turns_dir, item_ids):
    """Xiang session-turn-analysis: an item id in a turn's source_text is a
    mention (preview of what the turn projector must produce at :settled)."""
    out = {i: [] for i in item_ids}
    if not turns_dir or not os.path.isdir(turns_dir):
        return out, "turn analysis dir not found: %s" % turns_dir
    ids_b = {i: i.encode("utf-8") for i in item_ids}
    for name in sorted(os.listdir(turns_dir)):
        if not name.endswith(".json.analysis.json"):
            continue
        path = os.path.join(turns_dir, name)
        try:
            with open(path, "rb") as f:
                blob = f.read()
        except OSError:
            continue
        hits = [i for i, b in ids_b.items() if b in blob]
        if not hits:
            continue
        try:
            data = json.loads(blob.decode("utf-8"))
        except ValueError:
            continue
        turn_id = name.split(".json.analysis.json")[0]
        src = (data.get("source_text") or "").encode("utf-8")
        for item in hits:
            if ids_b[item] not in src:
                continue  # id appeared only in rationale/labels, not the turn text
            out[item].append({
                "turn": turn_id,
                "evidence_id": data.get("evidence_id"),
                "created_at": data.get("created_at"),
                "labeller": data.get("labeller")})
    return out, None

# --------------------------------------------------------------- building ---

def apply_shape(type_key, internal_fields, shapes, kind="record-types"):
    spec = (shapes.get(Keyword(kind)) or {}).get(type_key) or {}
    fmap = {str(k): v for k, v in (spec.get(Keyword("fields")) or {}).items()}
    out = {}
    for k, v in internal_fields.items():
        out[fmap.get(k, k.replace("-", "_"))] = v
    return out, str(spec.get(Keyword("status")) or "proposed")

def basis_of(edge):
    src = edge.get("basis") or edge.get("source")
    return {"exact": "exact", "parser": "parser", "human": "human"}.get(
        src, "model" if src else "parser")

def edge_met(edge_class, kind, target_group, marked_met):
    """pass.clj/edge-met?: closure met when target completed; artifact when
    started or completed; alternative never enters readiness (null); external
    unmet unless the input marks it met."""
    if edge_class == "alternative":
        return None
    if kind == "external":
        return bool(marked_met)
    if edge_class == "artifact":
        return target_group in ("started", "completed")
    return target_group == "completed"

def _date_of_ms(ms):
    if not ms:
        return None
    return datetime.fromtimestamp(ms / 1000, tz=timezone.utc).date().isoformat()

def build_item(item_id, ctx):
    """One items/<id>.json payload. ctx carries all sources."""
    records, links, absences = [], [], []
    shapes = ctx["shapes"]
    disp_mentions, disp_waiting, disp_related = [], [], []
    disp_benefits, disp_names = [], []
    live_sha = None

    def emit_record(type_key, ident, fields, provenance, supersedes=None):
        body, status = apply_shape(type_key, fields, shapes)
        records.append({"type": type_key, "status": status,
                        "id": record_id(type_key, ident),
                        "fields": body, "provenance": provenance,
                        "supersedes": supersedes})
        return records[-1]["id"]

    def emit_link(type_key, frm, to, fields, met=None):
        body, st = apply_shape(type_key, fields, shapes, kind=Keyword("link-types"))
        links.append({"type": type_key, "status": st,
                      "from": frm, "to": to, "fields": body,
                      "computed": {"met": met}})

    # ---- item node: one per item even when several sources name it -------
    member = ctx["members"].get(item_id)
    doc_path = member["path"] if member else None
    doc_sha = member.get("sha256") if member else None
    title = item_id
    group, declared = None, None
    if doc_path and os.path.isfile(doc_path):
        lines = read_lines(doc_path)
        live_sha = sha256_text("\n".join(lines) + "\n")
        title = doc_title(lines, item_id)
        status_line, sclass, _g = doc_status(lines)
        declared = status_line
        group = state_group(status_line, sclass)
        if doc_sha and doc_sha != live_sha:
            absences.append({"what": "pin freshness",
                             "reason": "corpus members.json pin %s lags the live document digest %s"
                                       % (doc_sha[:8], live_sha[:8])})
    else:
        absences.append({"what": "state group",
                         "reason": "item document not found on disk (%s)" % doc_path})
    emit_record("[:workplan/item 1]", "item/" + item_id,
                {"item-id": item_id, "title": title, "path": doc_path,
                 "repo": ctx["repos"].get(item_id), "doc-sha256": doc_sha},
                {"source": "file", "ref": doc_path, "basis": "parser"})
    if group is not None:
        emit_record("[:giraffe/state-group 1]", "state/" + item_id,
                    {"item-id": item_id, "state-group": group,
                     "declared-status": declared, "source-sha256": doc_sha,
                     "basis": "parser"},
                    {"source": "file", "ref": doc_path, "basis": "parser"})

    # ---- Giraffe readings + supersession (queue lineage) -----------------
    corpus_rec = ctx["corpus"].get(item_id)
    queue_rows = [r for r in ctx["queue"].get(item_id, [])
                  if r["state"] in ("landed", "superseded")]
    current_sha = (corpus_rec.get("source") or {}).get("sha256") if corpus_rec else None
    reading_ids = {}
    seen = []
    disp_earlier = []
    disp_reading = None
    for row in queue_rows:
        sha = row["source_sha"]
        if sha in reading_ids:
            continue
        seen.append(sha)
        if corpus_rec and sha == current_sha:
            fields = {"item-id": item_id,
                      "source-path": (corpus_rec.get("source") or {}).get("path"),
                      "source-sha256": sha,
                      "status-as-read": corpus_rec.get("status_as_read"),
                      "purpose": corpus_rec.get("purpose"),
                      "edge-count": len(corpus_rec.get("edges") or []),
                      "record-pin-sha256": None,
                      "queue-state": row["state"],
                      "requested-at-ms": row["requested_at_ms"]}
            prov = {"source": "agent", "ref": "giraffe-corpus:%s" % ctx["corpus_real"],
                    "basis": "model"}
        else:
            fields = {"item-id": item_id, "source-path": None,
                      "source-sha256": sha, "status-as-read": None,
                      "purpose": None, "edge-count": None,
                      "record-pin-sha256": None,
                      "queue-state": row["state"],
                      "requested-at-ms": row["requested_at_ms"]}
            prov = {"source": "agent", "ref": "giraffe-reread-queue", "basis": "model"}
        reading_ids[sha] = emit_record("[:giraffe/reading 1]",
                                       "reading/%s/%s" % (item_id, sha), fields, prov)
        # ---- display: join the queue row to its reading text -------------
        hist = ctx["historical"].get((item_id, sha))
        source_rec = corpus_rec if (corpus_rec and sha == current_sha) else hist
        reader = row.get("claimed_by") or "unknown"
        entry = {"read_at": _date_of_ms(row.get("updated_at_ms")),
                 "of_version": sha[:8],
                 "summary": (source_rec or {}).get("purpose"),
                 "reader": reader}
        if corpus_rec and sha == current_sha:
            warnings = ["self-check: %s" % w
                        for w in (corpus_rec.get("self_check") or [])]
            summary = corpus_rec.get("purpose")
            if ctx["plant_display_raw_sha"]:
                summary = sha  # PLANT: raw hash where words belong
            disp_reading = {
                "summary": summary,
                "status_as_read": corpus_rec.get("status_as_read"),
                "link_count": len(corpus_rec.get("edges") or []),
                "of_current_version": (live_sha == sha) if live_sha else None,
                "read_at": _date_of_ms(row.get("updated_at_ms")),
                "reader": reader,
                "warnings": warnings,
                "earlier_count": 0, "earlier": disp_earlier}
        else:
            disp_earlier.append(entry)
    if not ctx["plant_drop_supersedes"]:
        for prev, nxt in zip(seen, seen[1:]):
            emit_link("[:supersedes 1]", reading_ids[nxt], reading_ids[prev],
                      {"from": reading_ids[nxt], "to": reading_ids[prev]})
    if corpus_rec is None:
        absences.append({"what": "current reading",
                         "reason": "no accepted reading of %s in corpus generation %s"
                                   % (item_id, os.path.basename(ctx["corpus_real"]))})

    # ---- needs edges from the current reading ----------------------------
    for edge in (corpus_rec.get("edges") or []) if corpus_rec else []:
        kind = edge.get("kind")
        reverse = kind == "unblocks"
        frm = edge.get("to") if reverse else item_id
        to = item_id if reverse else edge.get("to")
        ev = edge.get("evidence") or {}
        base = {"quote": ev.get("quote"), "file": ev.get("path"),
                "line": ev.get("line")}
        if kind in ("depends-on", "unblocks"):
            eclass = edge.get("intended_class") or edge.get("intended-class") or "closure"
            tinfo = ctx["docinfo"].get(to)
            if tinfo is None:
                met = edge_met(eclass, "external", None, edge.get("met"))
                kind_of = "external"
            else:
                met = edge_met(eclass, "item", tinfo["group"], edge.get("met"))
                kind_of = "item"
            fields = dict(base, **{"from": frm, "to": to, "class": eclass,
                                   "source": kind_of, "basis": basis_of(edge),
                                   "reading-member": item_id})
            emit_link("[:needs 1]", frm, to, fields, met=met)
            disp_waiting.append({"id": to,
                                 "title": (tinfo or {}).get("title"),
                                 "state_group": (tinfo or {}).get("group"),
                                 "declared_status": (tinfo or {}).get("declared"),
                                 "met": met,
                                 "quote": ev.get("quote"), "line": ev.get("line")})
        else:
            tkey = {"part-of": "[:part-of 1]", "relates-to": "[:relates-to 1]"}.get(
                kind, "[:relates-to 1]")
            emit_link(tkey, frm, to, dict(base, **{"from": frm, "to": to}), met=None)
            tinfo = ctx["docinfo"].get(to)
            disp_related.append({"id": to,
                                 "title": (tinfo or {}).get("title"),
                                 "resolved": tinfo is not None,
                                 "kind": kind})

    # ---- :7094 workplan stack ---------------------------------------------
    view = ctx["arxana"].item_view(item_id) if ctx["arxana"] else None
    if view is None:
        absences.append({"what": "workplan stack records",
                         "reason": "no workplan/item-view for %s on :7094 "
                                   "(item not in the 323-item load, or stack unreachable)" % item_id})
    else:
        drafts = (view.get("attestations") or {}).get("drafts") or []
        level_att = next((a for a in drafts
                          if "pending" in json.dumps(a.get("method", {})).lower()), None)
        if level_att is not None:
            content = level_att.get("content") or {}
            emit_record("[:workplan/pending-level 1]", "pending/" + item_id,
                        {"item-id": item_id,
                         "level": content.get("pending/level", content.get("level")),
                         "method": (level_att.get("method") or {}).get("id"),
                         "basis": str(level_att.get("basis", ":model")).lstrip(":"),
                         "note": level_att.get("note")},
                        {"source": "agent", "ref": ":7094 workplan/item-view",
                         "basis": "model"})
        else:
            absences.append({"what": "pending level",
                             "reason": "no pending/level draft attestation on the item"})
        named_entity_ids = []
        for n in view.get("names") or []:
            ent = n.get("entity") or {}
            slug = (ent.get("asserted-content") or {}).get("entity/slug")
            named_entity_ids.append(ent.get("id"))
            emit_record("[:workplan/names 1]", "names/%s/%s" % (item_id, slug),
                        {"item-id": item_id, "entity-slug": slug,
                         "draft": bool(n.get("draft?"))},
                        {"source": "agent", "ref": ":7094 workplan/item-view",
                         "basis": "parser"})
            disp_names.append({"slug": slug, "label": slug})
        for s in view.get("serves") or []:
            bl = s.get("benefit-line") or {}
            line = (bl.get("asserted-content") or {}).get("benefit/line", bl.get("id"))
            emit_record("[:workplan/serves 1]", "serves/%s/%s" % (item_id, line),
                        {"item-id": item_id, "benefit-line": line,
                         "producer": s.get("producer"),
                         "draft": bool(s.get("draft?"))},
                        {"source": "agent", "ref": ":7094 workplan/item-view",
                         "basis": "model"})
            bname = next(((a.get("content") or {}).get("benefit/name")
                          for a in s.get("benefit/attestations") or []
                          if (a.get("content") or {}).get("benefit/name")), None)
            disp_benefits.append({
                "code": line,
                "text": bname if bname else
                        {"absent": "no benefit/name attestation on the "
                                   "benefit-line 'clusion for %s" % line},
                "producer": s.get("producer"), "draft": bool(s.get("draft?"))})
        if not (view.get("names") or []):
            absences.append({"what": "names links",
                             "reason": "item names no lexicon entity on :7094"})
        if not (view.get("serves") or []):
            absences.append({"what": "serves links",
                             "reason": "item serves no benefit line on :7094"})
        for m in view.get("mentions") or []:
            att = m.get("attestation") or {}
            emit_record("[:workplan/mention 1]",
                        "mention/%s/%s" % (item_id, m.get("record/id")),
                        {"item-id": item_id,
                         "role": str(m.get("role", "")).lstrip(":"),
                         "time": m.get("time"),
                         "ref": att.get("id"),
                         "author": m.get("author")},
                        {"source": "git", "ref": ":7094 workplan/mention",
                         "basis": "exact"})
            disp_mentions.append({
                "date": (m.get("time") or "")[:10] or None,
                "role": str(m.get("role", "")).lstrip(":"),
                "who": m.get("author") or att.get("producer") or "unknown",
                "turn": m.get("evidence/id") or att.get("id")})
        intro_count = 0
        intro_err = False
        for eid in named_entity_ids:
            if not eid:
                continue
            for rec in ctx["arxana"].introductions(eid):
                if "error" in rec:
                    if not intro_err:
                        absences.append({"what": "introductions",
                                         "reason": rec["error"]})
                    intro_err = True
                    continue
                content = rec.get("asserted-content") or {}
                eps = rec.get("endpoints") or []
                turn_ref = next((e.get("target", {}).get("ref/id")
                                 for e in eps if e.get("role") == ":evidence"), None)
                emit_record("[:workplan/introduction 1]",
                            "intro/%s" % rec.get("id"),
                            {"entity-slug": None, "turn-ref": turn_ref,
                             "codepoint-range": content.get("codepoint-range"),
                             "time": content.get("time"),
                             "timing-basis": content.get("timing-basis")},
                            {"source": "joe", "ref": ":7094 workplan/introduction",
                             "basis": "human"})
                intro_count += 1
        if intro_count == 0 and not intro_err:
            absences.append({"what": "introductions",
                             "reason": "no entity named by this item has a "
                                       "workplan/introduction on :7094"})

    # ---- Xiang turn mentions ----------------------------------------------
    for tm in ctx["turns"].get(item_id, []):
        emit_record("[:workplan/mention 1]",
                    "turn-mention/%s/%s" % (item_id, tm["turn"]),
                    {"item-id": item_id, "role": "context",
                     "time": tm.get("created_at"),
                     "ref": tm.get("evidence_id"), "author": tm.get("labeller")},
                    {"source": "turn", "ref": "session-turn-analysis/%s" % tm["turn"],
                     "basis": "model"})
        disp_mentions.append({"date": (tm.get("created_at") or "")[:10] or None,
                              "role": "context",
                              "who": tm.get("labeller") or "unknown",
                              "turn": tm.get("evidence_id") or tm["turn"]})
    if not ctx["turns"].get(item_id):
        absences.append({"what": "turn mentions",
                         "reason": "no analyzed operator turn text names %s" % item_id})

    # ---- item -> question link (or typed absence) --------------------------
    qlinks = ctx["questions"].get(item_id) or []
    for q in qlinks:
        emit_record("[:question/raised-by 1]",
                    "q/%s/%s" % (item_id, q["question_id"]),
                    {"item-id": item_id, "question-id": q["question_id"],
                     "kind": q["kind"], "matched-in": q["matched_in"]},
                    {"source": "file", "ref": ctx["questions_path"], "basis": "parser"})
    if not qlinks:
        absences.append({"what": "question link",
                         "reason": "no question in %s names %s"
                                   % (os.path.basename(ctx["questions_path"] or "?"), item_id)})

    kind = ("mission" if item_id.startswith("M-")
            else "excursion" if item_id.startswith("E-")
            else "ticket" if item_id.startswith("T-") else "other")
    counts = {}
    for r in records:
        counts[r["type"]] = counts.get(r["type"], 0) + 1
    for l in links:
        counts[l["type"]] = counts.get(l["type"], 0) + 1
    item_node = {"id": item_id, "title": title, "path": doc_path,
                 "repo": ctx["repos"].get(item_id),
                 "state_group": group if group is not None
                 else {"absent": "item document not found or status unmapped"}}

    # ---- display block (contract v2): words and dates, never a bare hash --
    repo = ctx["repos"].get(item_id)
    rel_path = None
    if doc_path and repo and ("/%s/" % repo) in doc_path:
        rel_path = doc_path.split("/%s/" % repo, 1)[1]
        rel_path = "%s/%s" % (repo, rel_path)
    if disp_reading is not None:
        disp_reading["earlier_count"] = len(disp_earlier)
        reading_disp = disp_reading
    elif queue_rows:
        reading_disp = {"absent": "queue has %d reading row(s) but none matches "
                                  "the current corpus reading" % len(queue_rows)}
    else:
        reading_disp = {"absent": "no accepted reading of %s in the corpus "
                                  "or the reread queue" % item_id}
    qlinks = ctx["questions"].get(item_id) or []
    if qlinks:
        q0 = qlinks[0]
        question_disp = {"id": q0["question_id"], "text": q0.get("text")}
    else:
        question_disp = {"absent": "no question in the argument graph names %s" % item_id}
    disp_mentions.sort(key=lambda m: (m["date"] or "", m["turn"] or ""))
    mentions_disp = {"count": len(disp_mentions),
                     "first": disp_mentions[0] if disp_mentions else None,
                     "latest": disp_mentions[-1] if disp_mentions else None,
                     "list": disp_mentions}
    display = {"header": {"id": item_id, "title": title,
                          "state_group": group, "declared_status": declared,
                          "path": rel_path},
               "reading": reading_disp,
               "waiting_on": disp_waiting,
               "related": disp_related,
               "why": {"benefit_lines": disp_benefits, "question": question_disp},
               "names": disp_names,
               "mentions": mentions_disp}
    return ({"item": item_node, "display": display, "records": records,
             "links": links, "absences": absences},
            {"id": item_id, "title": title, "kind": kind, "counts": counts})

# ------------------------------------------------------------------ main ---

def compute_groups(members):
    """Per-member display facts for every corpus member (state group, title,
    declared status) — used for met computation and waiting_on/related."""
    info, repos = {}, {}
    for m in members:
        iid, path = m.get("id"), m.get("path")
        repo = None
        if path and "/holes/" in path:
            repo = path.split("/holes/")[0].rstrip("/").rsplit("/", 1)[-1]
        repos[iid] = repo
        entry = {"group": None, "title": None, "declared": None}
        if path and os.path.isfile(path):
            lines = read_lines(path)
            sl, sc, _ = doc_status(lines)
            entry = {"group": state_group(sl, sc),
                     "title": doc_title(lines, iid), "declared": sl}
        info[iid] = entry
    return info, repos

def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--items", required=True, help="comma-separated item ids")
    ap.add_argument("--shapes", default=os.path.normpath(os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "..",
        "holes", "labs", "M-repl-circus", "gcc-shapes.edn")))
    ap.add_argument("--out", default=os.path.expanduser("~/storage/gcc-preview"))
    ap.add_argument("--corpus", default="/home/joe/notes/wm-next/giraffe-reads-2026-11-04/corpus")
    ap.add_argument("--queue", default=os.path.expanduser(
        "~/.local/state/futon3c/giraffe-reread/requests.sqlite"))
    ap.add_argument("--turns", default=os.path.expanduser("~/.emacs-graph/session-turn-analysis"))
    ap.add_argument("--questions", default="/home/joe/code/futon7/holes/argument-graph/questions.edn")
    ap.add_argument("--arxana", default="http://localhost:7094/arxana/v1")
    ap.add_argument("--arxana-fixture", default=None,
                    help="directory of canned item-view JSON (tests)")
    ap.add_argument("--no-arxana", action="store_true")
    args = ap.parse_args(argv)

    with open(args.shapes, encoding="utf-8") as f:
        shapes = edn_load(f.read())
    item_ids = [s.strip() for s in args.items.split(",") if s.strip()]

    members, corpus_records, corpus_real = load_corpus(args.corpus)
    docinfo, repos = compute_groups(members)
    queue = load_queue(args.queue) if os.path.isfile(args.queue) else {}
    # Historical readings: shas the queue names that the current generation
    # no longer carries, looked up in earlier generations for their summaries.
    needed = {}
    for iid in item_ids:
        cur = (corpus_records.get(iid, {}).get("source") or {}).get("sha256")
        shas = {r["source_sha"] for r in queue.get(iid, [])
                if r["state"] in ("landed", "superseded") and r["source_sha"] != cur}
        if shas:
            needed[iid] = shas
    historical = find_historical_readings(args.corpus + ".generations", needed)
    if args.no_arxana:
        arxana = None
    elif args.arxana_fixture:
        arxana = ArxanaSource(fixture_dir=args.arxana_fixture)
    else:
        arxana = ArxanaSource(url=args.arxana)
    questions, q_err = question_links(args.questions, item_ids)
    turns, _t_err = turn_mentions(args.turns, item_ids)

    ctx = {"shapes": shapes, "members": {m["id"]: m for m in members},
           "corpus": corpus_records, "corpus_real": corpus_real,
           "docinfo": docinfo, "repos": repos, "queue": queue,
           "historical": historical,
           "arxana": arxana, "questions": questions,
           "questions_path": args.questions, "turns": turns,
           "plant_drop_supersedes": os.environ.get("GCC_PREVIEW_PLANT_DROP_SUPERSEDES") == "1",
           "plant_display_raw_sha": os.environ.get("GCC_PREVIEW_PLANT_DISPLAY_RAW_SHA") == "1"}

    items_dir = os.path.join(args.out, "items")
    os.makedirs(items_dir, exist_ok=True)
    index_items = []
    for iid in item_ids:
        payload, summary = build_item(iid, ctx)
        if q_err:
            payload["absences"].append({"what": "question link", "reason": q_err})
        with open(os.path.join(items_dir, iid + ".json"), "w", encoding="utf-8") as f:
            json.dump(payload, f, indent=2, ensure_ascii=False)
            f.write("\n")
        index_items.append(summary)
    index = {"generated_at": datetime.now(timezone.utc).isoformat(),
             "shapes_file": os.path.abspath(args.shapes),
             "items": index_items}
    with open(os.path.join(args.out, "index.json"), "w", encoding="utf-8") as f:
        json.dump(index, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print("wrote %d item previews to %s" % (len(index_items), args.out))
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
