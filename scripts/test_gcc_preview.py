#!/usr/bin/env python3
"""test_gcc_preview.py — fixture tests for scripts/gcc_preview.py.

ALL fixture content below is INVENTED for these tests (ids M-fixture-alpha,
M-fixture-beta, E-fixture-gamma): it names no real item, question, reading
or turn, and contains no question text, reading text or turn text from any
real source (futon3c is PUBLIC).

Checks (M-repl-circus demo 1a requisition):
  1. one item node even when two sources name the item (corpus member AND
     a :7094-style item-view fixture for the same id);
  2. a superseded reading is linked to the current one by [:supersedes 1];
  3. met is computed false for an unmet need (closure edge onto an item
     whose state group is started, not completed);
  4. renaming a field in the shapes file renames the output field.
Plant check: GCC_PREVIEW_PLANT_DROP_SUPERSEDES=1 must drop the supersedes
link and make check 2's assertion fail.
"""

import json
import os
import re
import shutil
import sqlite3
import hashlib
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SCRIPT = os.path.join(HERE, "gcc_preview.py")

ALPHA = "M-fixture-alpha"   # INVENTED fixture id
BETA = "M-fixture-beta"     # INVENTED fixture id (unmet-need target, OPEN)
GAMMA = "E-fixture-gamma"   # INVENTED fixture id (no reading, no workplan)

SHA_OLD = "0" * 64          # INVENTED digest, superseded reading


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(text)


def make_fixture(root):
    """An invented, self-contained miniature of every source."""
    # -- holes documents ----------------------------------------------------
    docs = os.path.join(root, "docs")
    alpha_text = "# %s — invented fixture mission\n\nStatus: OPEN — fixture\n" % ALPHA
    write(os.path.join(docs, ALPHA + ".md"), alpha_text)
    write(os.path.join(docs, BETA + ".md"),
          "# %s — invented fixture target\n\nStatus: OPEN — fixture\n" % BETA)
    write(os.path.join(docs, GAMMA + ".md"),
          "# %s — invented fixture excursion\n\nStatus: COMPLETE — fixture\n" % GAMMA)
    # The current reading is pinned to the live doc digest so
    # of_current_version comes out true, as it does for a fresh reading.
    sha_new = hashlib.sha256(alpha_text.encode("utf-8")).hexdigest()

    # -- Giraffe corpus generation ------------------------------------------
    corpus = os.path.join(root, "corpus")
    members = [{"id": ALPHA, "path": os.path.join(docs, ALPHA + ".md"), "sha256": sha_new},
               {"id": BETA, "path": os.path.join(docs, BETA + ".md"), "sha256": "2" * 64},
               {"id": GAMMA, "path": os.path.join(docs, GAMMA + ".md"), "sha256": "3" * 64}]
    write(os.path.join(corpus, "members.json"), json.dumps(members))
    reading = {"id": ALPHA,
               "source": {"path": os.path.join(docs, ALPHA + ".md"), "sha256": sha_new},
               "status_as_read": "open",
               "purpose": "fixture reading",
               "edges": [{"kind": "depends-on", "to": BETA,
                          "evidence": {"path": os.path.join(docs, ALPHA + ".md"),
                                       "line": 3, "quote": "Status: OPEN — fixture"},
                          "basis": "exact"}],
               "unresolved": []}
    write(os.path.join(corpus, "batch-1.jsonl"), json.dumps(reading) + "\n")

    # -- reread queue (SQLite): alpha read twice, old then new ---------------
    queue = os.path.join(root, "requests.sqlite")
    db = sqlite3.connect(queue)
    db.execute("CREATE TABLE reread_requests (target_id TEXT, source_sha TEXT, "
               "state TEXT, requested_at_ms INTEGER, claimed_by TEXT, "
               "updated_at_ms INTEGER)")
    db.execute("INSERT INTO reread_requests VALUES (?,?,?,?,?,?)",
               (ALPHA, SHA_OLD, "superseded", 1000, None, 1500))
    db.execute("INSERT INTO reread_requests VALUES (?,?,?,?,?,?)",
               (ALPHA, sha_new, "landed", 2000, "fixture-seat", 2500))
    db.commit(); db.close()

    # -- :7094 fixture: a canned item-view also naming ALPHA -----------------
    arxana = os.path.join(root, "arxana")
    write(os.path.join(arxana, ALPHA + ".json"), json.dumps({
        "attestations": {"drafts": [], "provenance": []},
        "names": [],
        "serves": [{"benefit-line": {"asserted-content": {"benefit/line": "Z9"},
                                     "id": "fixture-clusion-z9"},
                    "benefit/attestations": [],   # no benefit/name: typed absence
                    "attestation": {}, "producer": "fixture/producer",
                    "draft?": True}],
        "mentions": [], "assessments": []}))
    write(os.path.join(arxana, "introductions.json"), "{}")

    # -- no turn mentions, no questions --------------------------------------
    turns = os.path.join(root, "turns")
    os.makedirs(turns, exist_ok=True)
    questions = os.path.join(root, "questions.edn")
    write(questions, ";; INVENTED fixture questions file\n{:meta {:as-of \"fixture\"}\n :questions []}\n")

    # -- shapes file (small but representative) -------------------------------
    shapes = os.path.join(root, "shapes.edn")
    write(shapes, """
{:meta {:demo "fixture"}
 :record-types
 {"[:workplan/item 1]" {:status :built
                        :fields {:item-id "item_id" :title "title" :path "path"
                                 :repo "repo" :doc-sha256 "doc_sha256"}}
  "[:giraffe/state-group 1]" {:status :proposed
                              :fields {:item-id "item_id" :state-group "state_group"
                                       :declared-status "declared_status"
                                       :source-sha256 "source_sha256" :basis "basis"}}
  "[:giraffe/reading 1]" {:status :proposed
                          :fields {:item-id "item_id" :source-path "source_path"
                                   :source-sha256 "source_sha256"
                                   :status-as-read "status_as_read"
                                   :purpose "purpose" :edge-count "edge_count"
                                   :record-pin-sha256 "record_pin_sha256"
                                   :queue-state "queue_state"
                                   :requested-at-ms "requested_at_ms"}}}
 :link-types
 {"[:needs 1]" {:status :built
                :fields {:from "from" :to "to" :class "class" :source "source"
                         :basis "basis" :quote "quote" :file "file" :line "line"
                         :reading-member "reading_member"}}
  "[:supersedes 1]" {:status :proposed :fields {:from "from" :to "to"}}}}
""")
    return {"docs": docs, "corpus": corpus, "queue": queue, "arxana": arxana,
            "turns": turns, "questions": questions, "shapes": shapes,
            "sha_new": sha_new}


def run_projector(fx, out, shapes=None, env_extra=None):
    env = dict(os.environ)
    if env_extra:
        env.update(env_extra)
    subprocess.run(
        [sys.executable, SCRIPT,
         "--items", "%s,%s,%s" % (ALPHA, BETA, GAMMA),
         "--shapes", shapes or fx["shapes"],
         "--out", out,
         "--corpus", fx["corpus"],
         "--queue", fx["queue"],
         "--turns", fx["turns"],
         "--questions", fx["questions"],
         "--arxana-fixture", fx["arxana"]],
        check=True, env=env, capture_output=True, text=True)
    with open(os.path.join(out, "index.json"), encoding="utf-8") as f:
        index = json.load(f)
    items = {}
    for iid in (ALPHA, BETA, GAMMA):
        with open(os.path.join(out, "items", iid + ".json"), encoding="utf-8") as f:
            items[iid] = json.load(f)
    return index, items


def check_futon1b(root, fx):
    """Invented records over real HTTP/EDN and a real read-only SQLite resolver."""
    import threading
    from http.server import BaseHTTPRequestHandler, HTTPServer
    from urllib.parse import unquote
    import gcc_preview as g

    eid = 'fixture/mission/alpha'
    entity = {'id': eid, 'external-id': ALPHA, 'type': 'mission/doc'}
    sidecar = os.path.join(root, 'entities.sqlite')
    with sqlite3.connect(sidecar) as db:
        db.execute('CREATE TABLE ent_node(id TEXT, type TEXT, external_id TEXT)')
        db.executemany('INSERT INTO ent_node VALUES (?,?,?)',
                       [(eid, ':mission/doc', ALPHA), ('stub', ':mission', ALPHA)])
    def link(rid, kind, role, target, content=None):
        return {'id': rid, 'type-revision': [kind, 1],
                'endpoints': [{'role': role, 'target': target}], 'asserted-content': content or {}}
    benefit = link('benefit', 'benefit-line', 'unused', {}, {'benefit/line': 'fixture-benefit'})
    serve = link('serve', 'workplan/serves', 'item', {'entity/id': eid})
    serve['type-revision'][1] = 2
    serve['endpoints'].append({'role': 'benefit-line', 'target': {'clusion/id': 'benefit'}})
    history = link('history', 'giraffe/legacy-reading', 'item', {'entity/id': eid},
                   {'legacy/freshness': 'unknown', 'legacy/source-record-id': 'old-reading'})
    intro = link('intro', 'workplan/introduction', 'entity', {'clusion/id': 'lexicon'})
    def evidence(rid, subject, content, method='fixture-method'):
        return {'id': rid, 'subject-clusion-id': subject, 'basis': 'model', 'standing': 'unverified',
                'producer': 'fixture-reader', 'method': {'id': method}, 'content': content}
    evidence_rows = [evidence('s1', 'serve', {}, 'first-method'),
                     evidence('s2', 'serve', {}, 'second-method'),
                     evidence('descriptor', 'benefit', {'benefit/name': 'Invented benefit'}),
                     evidence('reading', 'history', {'pending/level': 4, 'sheet/one-line': 'Invented history'})]
    records = {x['id']: x for x in [benefit, serve, history, intro] + evidence_rows}
    mode = {'value': 'ok'}
    calls = []

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *_args):
            pass

        def reply(self, value):
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps(value).encode())

        def do_GET(self):
            assert unquote(self.path) == '/api/alpha/entity/' + eid
            self.reply({'entity': entity})

        def do_POST(self):
            assert self.path == '/api/alpha/arxana/query'
            assert self.headers['Content-Type'] == 'application/edn'
            assert self.headers['x-arxana-principal'] == 'fixture-principal'
            q = g.edn_load(self.rfile.read(int(self.headers['Content-Length'])).decode())
            calls.append(q)
            kind = q['query/type']
            assert isinstance(kind, g.Keyword)
            if mode['value'] == 'stale':
                self.reply({'status': 'stale-index'})
                return
            result = {'status': 'ok', 'watermark': 2 if mode['value'] == 'changed' else 1}
            if kind == 'role-reverse':
                assert isinstance(q['edge-type'][0], g.Keyword)
                ids = [x['id'] for x in records.values() if x.get('type-revision') == q['edge-type']
                       and any(ep.get('role') == q['endpoint-role'] and
                               q['endpoint-id'] in ep.get('target', {}).values()
                               for ep in x.get('endpoints', []))]
                result.update({'edge-ids': ids, 'truncated?': mode['value'] == 'truncated'})
            elif kind == 'attestations':
                ids = [x['id'] for x in evidence_rows if x['subject-clusion-id'] == q['subject-id']]
                result['attestation-ids'] = ['s1'] * 200 if mode['value'] == 'cap' else ids
            elif kind == 'record-by-id':
                result = {'status': 'ok', 'record': records[q['record-id']]}
            else:
                raise AssertionError(kind)
            self.reply(result)

    server = HTTPServer(('127.0.0.1', 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    url = 'http://127.0.0.1:%d' % server.server_port
    try:
        source = g.Futon1bSource(url, sidecar, 'fixture-principal')
        view = source.item_view(ALPHA)
        assert view['entity']['id'] == eid
        assert len(view['serves']) == 2, 'two methods must not collapse into one'
        assert len([r for r in view['records'] if r['id'] == 'serve']) == 1
        assert source.introductions('lexicon') == [intro]
        assert not view['names'] and not view['mentions']
        # Exercise each incomplete read through the actual wire adapter.
        for bad in ('stale', 'truncated', 'changed', 'cap'):
            mode['value'] = bad
            try:
                source.item_view(ALPHA)
            except ValueError:
                pass
            else:
                raise AssertionError('accepted incomplete read: ' + bad)
        mode['value'] = 'ok'
        with sqlite3.connect(sidecar) as db:
            db.execute('INSERT INTO ent_node VALUES (?,?,?)', ('other', ':mission/doc', ALPHA))
        try:
            source.resolve(ALPHA)
        except ValueError as exc:
            assert '2 substrate' in str(exc)
        else:
            raise AssertionError('ambiguous real SQLite identity was silently selected')
        with sqlite3.connect(sidecar) as db:
            db.execute('DELETE FROM ent_node WHERE id=?', ('other',))
        # CLI projection with the real adapter: no workplan/item mirror or invented current pending level.
        out = os.path.join(root, 'futon1b-out')
        subprocess.run([sys.executable, SCRIPT, '--source', 'futon1b', '--futon1b', url,
                        '--arxana-principal', 'fixture-principal', '--entity-sidecar', sidecar,
                        '--items', ALPHA, '--out', out, '--corpus', fx['corpus'],
                        '--queue', fx['queue'], '--turns', fx['turns'], '--questions', fx['questions']],
                       check=True, capture_output=True, text=True)
        with open(os.path.join(out, 'items', ALPHA + '.json')) as f:
            payload = json.load(f)
        assert payload['item']['entity_ref']['id'] == eid
        assert 'not rebuilt yet' in payload['display']['names']['absent']
        assert 'not rebuilt yet' in payload['display']['mentions']['absent']
        types = [r['type'] for r in payload['records']]
        assert '[:workplan/item 1]' not in types
        assert '[:workplan/pending-level 1]' not in types
        assert types.count('[:workplan/serves 2]') == 1
        assert types.count('[:giraffe/legacy-reading 1]') == 1
        by_id = {r['id']: r for r in payload['records']}
        for rid in ('s1', 's2', 'reading'):
            assert by_id[rid]['fields']['record'] == records[rid]
        assert by_id['history']['fields']['freshness'] == 'unknown'
        assert payload['display']['reading']['summary'] == 'fixture reading'
        assert all(q['query/type'] in ('role-reverse', 'record-by-id', 'attestations') for q in calls)
        print('ok — futon1b HTTP/EDN, v2 identity/evidence, historical semantics, ambiguity and incomplete-read checks')
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


def main():
    root = tempfile.mkdtemp(prefix="gcc-preview-test-")
    try:
        fx = make_fixture(root)

        # -- checks 1-3 on one run -------------------------------------------
        index, items = run_projector(fx, os.path.join(root, "out"))

        # 1. one item node even when two sources (corpus + item-view) name it
        assert [i["id"] for i in index["items"]].count(ALPHA) == 1, \
            "index must list %s exactly once" % ALPHA
        item_recs = [r for r in items[ALPHA]["records"] if r["type"] == "[:workplan/item 1]"]
        assert len(item_recs) == 1, "exactly one [:workplan/item 1] record, got %d" % len(item_recs)

        # 2. superseded reading linked by supersedes
        readings = [r for r in items[ALPHA]["records"] if r["type"] == "[:giraffe/reading 1]"]
        assert len(readings) == 2, "two readings (superseded + current), got %d" % len(readings)
        by_sha = {r["fields"]["source_sha256"]: r["id"] for r in readings}
        sup = [l for l in items[ALPHA]["links"] if l["type"] == "[:supersedes 1]"]
        assert len(sup) == 1 and sup[0]["from"] == by_sha[fx["sha_new"]] \
            and sup[0]["to"] == by_sha[SHA_OLD], \
            "current reading must supersede the earlier one: %s" % sup

        # 3. met computed false for an unmet need (closure onto started BETA)
        needs = [l for l in items[ALPHA]["links"] if l["type"] == "[:needs 1]"]
        assert len(needs) == 1 and needs[0]["to"] == BETA, "one needs edge onto %s" % BETA
        assert needs[0]["computed"]["met"] is False, \
            "closure onto a started item is unmet, got %r" % needs[0]["computed"]["met"]
        assert items[BETA]["item"]["state_group"] == "started"

        # -- 4. a shape renamed in the shapes file renames the output field --
        with open(fx["shapes"], encoding="utf-8") as f:
            shapes_text = f.read()
        renamed = os.path.join(root, "shapes-renamed.edn")
        write(renamed, shapes_text.replace(':status-as-read "status_as_read"',
                                           ':status-as-read "reading_status"'))
        _i2, items2 = run_projector(fx, os.path.join(root, "out2"), shapes=renamed)
        fields2 = [r["fields"] for r in items2[ALPHA]["records"]
                   if r["type"] == "[:giraffe/reading 1]"]
        assert all("reading_status" in f and "status_as_read" not in f for f in fields2), \
            "renamed shape field must rename the output field: %s" % list(fields2[0])

        # -- display (contract v2) ------------------------------------------
        HEX64 = re.compile(r"\b[0-9a-f]{64}\b")

        def display_strings(x, path=()):
            if isinstance(x, str):
                yield path, x
            elif isinstance(x, dict):
                for k, v in x.items():
                    yield from display_strings(v, path + (k,))
            elif isinstance(x, list):
                for i, v in enumerate(x):
                    yield from display_strings(v, path + (i,))

        # 5. no 64-hex strings anywhere in display outside earlier[].of_version
        disp = items[ALPHA]["display"]
        for path, s in display_strings(disp):
            if path[:3] == ("reading", "earlier", path[2] if len(path) > 2 else None) \
                    and len(path) == 4 and path[3] == "of_version":
                continue  # short sha only, and it is 8 chars anyway
            assert not HEX64.search(s), \
                "bare hash in display at %s: %s" % (".".join(map(str, path)), s[:80])

        # 6. reading summary present when the corpus has the reading
        assert disp["reading"]["summary"] == "fixture reading", \
            "display.reading.summary must be the reading's purpose: %r" % disp["reading"]
        assert disp["reading"]["of_current_version"] is True
        assert disp["reading"]["earlier_count"] == 1
        assert disp["reading"]["earlier"][0]["of_version"] == SHA_OLD[:8]

        # 7. waiting_on carries the needed item's title
        assert disp["waiting_on"] and disp["waiting_on"][0]["id"] == BETA
        assert disp["waiting_on"][0]["title"] == "%s — invented fixture target" % BETA, \
            disp["waiting_on"]
        assert disp["waiting_on"][0]["met"] is False

        # 8. a missing benefit text is a typed absence
        bl = disp["why"]["benefit_lines"]
        assert bl and bl[0]["code"] == "Z9", bl
        assert isinstance(bl[0]["text"], dict) and "absent" in bl[0]["text"], bl

        # -- plant: a raw sha256 in display.reading.summary must fail check 5
        _i4, items4 = run_projector(fx, os.path.join(root, "out4"),
                                    env_extra={"GCC_PREVIEW_PLANT_DISPLAY_RAW_SHA": "1"})
        plant_failed_as_expected = False
        try:
            for path, s in display_strings(items4[ALPHA]["display"]):
                assert not HEX64.search(s), "bare hash at %s" % (path,)
        except AssertionError:
            plant_failed_as_expected = True
        assert plant_failed_as_expected, \
            "PLANT did not fail: raw sha256 in display.reading.summary went unnoticed"

        # -- plant: dropping the supersedes link must fail check 2 -----------
        _i3, items3 = run_projector(fx, os.path.join(root, "out3"),
                                    env_extra={"GCC_PREVIEW_PLANT_DROP_SUPERSEDES": "1"})
        sup3 = [l for l in items3[ALPHA]["links"] if l["type"] == "[:supersedes 1]"]
        plant_failed_as_expected = False
        try:
            assert len(sup3) == 1, "supersedes link present"
        except AssertionError:
            plant_failed_as_expected = True
        assert plant_failed_as_expected, \
            "PLANT did not fail: supersedes link still present under GCC_PREVIEW_PLANT_DROP_SUPERSEDES=1"

        check_futon1b(root, fx)

        print("ok — 8 checks pass; plants (dropped supersedes, raw sha in display) "
              "fail their checks as expected")
        return 0
    finally:
        shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    raise SystemExit(main())
