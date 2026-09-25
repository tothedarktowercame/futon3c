#!/usr/bin/env python3
"""turn_margin_html.py — render an interpreted operator turn as a margin page.

The analysis of a turn is a structure: spans of what Joe said, each carrying an
intent, a target, a rationale and sometimes a pattern id. Read as JSON, or as a
cascade in a REPL buffer, that structure is legible only to whoever wrote it.
This puts the turn in a reading measure with its interpretation in the margin
beside it, the arrangement the mark7 typeset previews use.

  turn_margin_html.py RECORD.json [...] [--out DIR]
  turn_margin_html.py --latest N [--out DIR]

Marked spans are underlined in the prose; hovering or clicking one highlights
its note. Most of a turn stays unmarked, which is the point -- the underlines
are the operative part and the rest is the surplus they sit in.
"""
import argparse, glob, html, json, os, re, sys
from datetime import datetime, timezone

DEFAULT_RECORDS = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
DEFAULT_OUT = "/var/www/zone.hyperreal.enterprises/wip/turns"

CSS = """
:root {
  --pad: 3vw; --gutter: 2.5rem;
  --measure: clamp(22rem, 38vw, 42rem);
  --margin-w: max(13rem, calc(100vw - (2 * var(--pad)) - var(--measure) - var(--gutter)));
  --note-w: min(var(--margin-w), 24rem);
}
*, *::before, *::after { box-sizing: border-box; }
body { margin: 0; padding: 3rem var(--pad) 6rem var(--pad);
       background: #fffff8; color: #111;
       font: 1.05rem/1.7 et-book, Palatino, "Palatino Linotype", Georgia, serif; }
h1 { font-size: 1.5rem; font-weight: 400; width: var(--measure); }
.meta { color: #666; font-size: .8rem; width: var(--measure); margin-bottom: 2.5rem; }
.meta a { color: #666; }
.turn { display: grid; grid-template-columns: var(--measure) var(--margin-w);
        column-gap: var(--gutter); align-items: start; }
.prose { grid-column: 1; white-space: pre-wrap; }
.notes { grid-column: 2; }
.note { width: var(--note-w); font-size: .78rem; line-height: 1.45;
        margin: 0 0 1.4rem 0; padding-left: .7rem; border-left: 2px solid #ddd;
        color: #333; }
.note.lit { border-left-color: #b8431f; background: #fdf6f2; }
.note .intent { font-variant: small-caps; letter-spacing: .04em; color: #b8431f; }
.note .target { color: #555; font-style: italic; }
.note .rel { color: #888; }
.note .pat { font-family: ui-monospace, Menlo, monospace; font-size: .72rem; }
.cue { border-bottom: 1.5px solid #b8431f; cursor: pointer; }
.cue.lit { background: #fbe6dd; }
.unresolved { color: #999; font-style: italic; }
footer { margin-top: 4rem; width: var(--measure); color: #888; font-size: .78rem; }
"""

JS = """
document.addEventListener('click', e => {
  const cue = e.target.closest('.cue');
  document.querySelectorAll('.lit').forEach(n => n.classList.remove('lit'));
  if (!cue) return;
  cue.classList.add('lit');
  const note = document.getElementById(cue.dataset.note);
  if (note) { note.classList.add('lit'); note.scrollIntoView({block:'nearest', behavior:'smooth'}); }
});
"""


def load(path):
    record = json.load(open(path, encoding="utf-8"))
    analysis_path = path + ".analysis.json"
    analysis = (json.load(open(analysis_path, encoding="utf-8"))
                if os.path.exists(analysis_path) else None)
    # Proposed patterns for the fragments nothing in the library named. They
    # sit beside the record, never in library/: a proposal is a name someone
    # wanted, and admitting it is a separate act.
    # Two spellings in the wild: kimi-2 wrote RECORD.json.candidates.json and
    # kimi-1 wrote RECORD.candidates.json. The brief says the first; accept
    # both rather than dropping a proposal over a filename.
    side = next((c for c in (path + ".candidates.json",
                             path[:-len(".json")] + ".candidates.json")
                 if os.path.exists(c)), None)
    if side:
        record["_candidates"] = json.load(open(side, encoding="utf-8")).get("candidates", [])
    return record, analysis


def fragments(analysis, prefix=""):
    """Every annotated fragment, in source order, with a stable id.

    The id carries the turn, because every turn has a sentence called s1: on
    a page holding one turn a bare n-s1-0 is unique, and in the feed it
    collides with the same id in forty other turns, so clicking a cue lit
    notes in every one of them.
    """
    out = []
    for sentence in (analysis or {}).get("sentences", []):
        for i, frag in enumerate(sentence.get("fragments", [])):
            frag = dict(frag)
            frag["id"] = f"{prefix}n-{sentence['id']}-{i}"
            out.append(frag)
        if not sentence.get("fragments") and sentence.get("unresolved_reason"):
            out.append({"id": f"{prefix}n-{sentence['id']}-u", "unresolved": True,
                        "start": sentence.get("start", 0), "end": sentence.get("end", 0),
                        "rationale": sentence["unresolved_reason"]})
    return sorted(out, key=lambda f: f.get("start", 0))


def mark_prose(source, frags):
    """Underline display cues only. Interpretation spans are never underlined:
    they cover whole sentences, and marking them would assert that every word
    is operative -- which is what the surplus premise denies."""
    cuts = []
    for frag in frags:
        for cue in frag.get("display_cues", []):
            cuts.append((cue["start"], cue["end"], frag["id"]))
    cuts.sort()
    out, at = [], 0
    for start, end, note in cuts:
        if start < at:          # overlapping cues: keep the first
            continue
        out.append(html.escape(source[at:start]))
        out.append(f'<span class="cue" data-note="{note}">'
                   f'{html.escape(source[start:end])}</span>')
        at = end
    out.append(html.escape(source[at:]))
    return "".join(out)


def render(record, analysis, name):
    source = record.get("source_text", "")
    frags = fragments(analysis, prefix=f"{name}-") if analysis else []
    notes = []
    for frag in frags:
        if frag.get("unresolved"):
            notes.append(f'<p class="note unresolved" id="{frag["id"]}">'
                         f'{html.escape(frag.get("rationale", ""))}</p>')
            continue
        bits = [f'<span class="intent">{html.escape(frag.get("intent", "?"))}</span>']
        if frag.get("target"):
            bits.append(f'<span class="target">{html.escape(frag["target"])}</span>')
        if frag.get("rationale"):
            bits.append(html.escape(frag["rationale"]))
        if frag.get("relations"):
            bits.append(f'<span class="rel">{html.escape(" · ".join(frag["relations"]))}</span>')
        for ref in frag.get("pattern_refs", []):
            bits.append(f'<span class="pat">{html.escape(ref["id"])}</span> '
                        + html.escape(ref.get("rationale", "")))
        notes.append(f'<p class="note" id="{frag["id"]}">' + "<br>".join(bits) + "</p>")

    status = (analysis or {}).get("status") or record.get("analysis_status") or "requested"
    meta = " · ".join(filter(None, [
        record.get("created_at", ""), record.get("agent_id", ""),
        f"surface: {record.get('surface', 'unrecorded')}",
        f"interpreted by {(analysis or {}).get('labeller', '—')}",
        f"{len(frags)} fragments", status]))
    return f"""<!DOCTYPE html><html lang="en"><head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>{html.escape(name)}</title><style>{CSS}</style></head><body>
<h1>{html.escape(name)}</h1>
<p class="meta">{html.escape(meta)}</p>
<div class="turn"><div class="prose">{mark_prose(source, frags)}</div>
<div class="notes">{''.join(notes) or '<p class="note unresolved">Not yet interpreted.</p>'}</div></div>
<footer>Underlines are the operative spans — the words that resolve to an intent or a
pattern. Most of a turn is deliberately left unmarked. Click one to light its note.</footer>
<script>{JS}</script></body></html>"""


SESSION_JS = """
// Plain JS on purpose: the page is one static file with no build step, and
// the requirement is that a turn appears without a reload, not a toolchain.
const seen = new Set([...document.querySelectorAll('[data-turn]')]
                     .map(n => n.dataset.turn));
async function tick() {
  try {
    const r = await fetch(MANIFEST + '?t=' + Date.now(), {cache: 'no-store'});
    if (!r.ok) return;
    const turns = await r.json();
    const log = document.getElementById('log');
    for (const t of turns) {
      const existing = document.querySelector(`[data-turn="${t.name}"]`);
      if (existing) {
        // an interpretation can land long after the turn did
        if (existing.dataset.state !== t.state) {
          existing.dataset.state = t.state;
          existing.querySelector('.state').textContent = t.state;
        }
        continue;
      }
      seen.add(t.name);
      const p = document.createElement('p');
      p.className = 'note fresh';
      p.dataset.turn = t.name;
      p.dataset.state = t.state;
      p.innerHTML = `<a href="${t.name}.html">${t.name}</a> — ${t.head}` +
                    `<br><span class="rel">${t.at} · <span class="state">${t.state}</span></span>`;
      log.prepend(p);
    }
  } catch (e) { /* a missed poll is not an event */ }
}
setInterval(tick, POLL * 1000);
tick();
"""


TRANSLATIONS = "/home/joe/code/storage/operator-turns/translations"


def authored_cascade(name):
    """A cascade someone actually wrote for this turn, if one exists."""
    for path in sorted(glob.glob(f"{TRANSLATIONS}/{name}.*.md")):
        text = open(path, encoding="utf-8").read()
        block = re.search(r"```(?:clojure|lisp)?\n(\(cascade.*?)```", text, re.S)
        if block:
            return {"by": os.path.basename(path).split(".")[-2],
                    "text": block.group(1).rstrip()}
    return None


LIB = "/home/joe/code/futon3/library"


def resolves(pid):
    return "/" in pid and os.path.exists(f"{LIB}/{pid}.flexiarg")


def tok(text, kind, note=None):
    t = {"t": text, "k": kind}
    if note:
        t["n"] = note
    return t


def derived_cascade(name, frags):
    """The analysis as s-expression tokens, each tagged with what it is.

    Not a translation -- nobody wrote this. It is the annotation restated in
    cascade form, tokenised so the reader can see at a glance which parts
    retrieved a design pattern and which are only textual markup: a resolving
    pattern id and a HOLE read differently, and an intent with neither is
    markup wearing a cascade's shape.
    """
    out = [tok("(cascade ", "punct"), tok(name, "name"),
           tok(" :derived-from analysis", "kw"), tok("\n", "punct")]
    for f in frags:
        if f.get("unresolved"):
            continue
        note = f["id"]
        target = (f.get("target") or "").replace('"', "'")
        out += [tok("  (", "punct", note),
                tok(f.get("intent", "?"), "intent", note)]
        for cue in f.get("display_cues", []):
            out.append(tok(f' "{cue["text"]}"', "cue", note))
        refs = f.get("pattern_refs", [])
        if refs:
            for r in refs:
                out += [tok("\n    ", "punct", note),
                        tok(r["id"], "pattern" if resolves(r["id"]) else "dangling", note)]
            out += [tok("\n    :target ", "kw", note),
                    tok(f'"{target}"', "str", note), tok(")", "punct", note)]
        else:
            out += [tok("\n    (", "punct", note), tok("HOLE", "hole", note),
                    tok(" :wanted ", "kw", note), tok(f'"{target}"', "str", note),
                    tok("))", "punct", note)]
        out.append(tok("\n", "punct"))
    out.append(tok(")", "punct"))
    return out


TOKEN = re.compile(r"""(?P<str>"[^"]*")|(?P<kw>:[\w-]+)|(?P<sym>[^\s()\[\]{}"]+)|(?P<punct>.)""",
                   re.S)


def tokenise_authored(text):
    """Lex someone's hand-written cascade well enough to colour it.

    No note ids: an authored cascade was not derived from the annotation, so
    its parts do not correspond to cues and clicking one cannot light it.
    """
    out = []
    for m in TOKEN.finditer(text):
        kind, value = m.lastgroup, m.group(0)
        if kind == "sym":
            kind = ("hole" if value.startswith("HOLE")
                    else "pattern" if resolves(value.strip("()[],"))
                    else "dangling" if "/" in value
                    else "sym")
        out.append(tok(value, kind))
    return out


CAND_DIR = "/home/joe/code/storage/operator-turns/candidates"


def compact_candidate(c):
    """A proposal shown as a lineage, not as a printed pattern.

    Joe: "we don't need to have the whole design pattern printed out in the
    3rd column, because in such cases we would have too many". What a reader
    needs at a glance is where the proposal hangs -- which known pattern it
    descends from, or that it claims to be a new root. The body stays in the
    file and on the turn's own page.
    """
    parent = c.get("parent")
    if not parent:
        # a candidate written as a flexiarg carries its ancestry in @why
        path = f"{CAND_DIR}/{c['id']}.flexiarg"
        if os.path.exists(path):
            m = re.search(r"^@why (.+)$", open(path, encoding="utf-8").read(), re.M)
            if m:
                parent = m.group(1).strip("[] ").split()[0]
    return {"id": c["id"], "title": c.get("title", ""),
            "fragment": c.get("fragment"),
            "parent": parent,
            "parent-known": bool(parent) and os.path.exists(
                f"{LIB}/{parent}.flexiarg") if parent else False,
            "why": (c.get("then") or c.get("because") or "")[:220]}


def feed_entry(name, record, analysis):
    """A turn with its annotations inline, for a feed that never navigates.

    The margin pages hand the reader a link per turn; this hands them the
    turn. Cues are pre-sliced into runs so the client renders spans without
    re-deriving offsets, and an un-interpreted turn still appears -- with its
    text and an empty note list, which is the honest shape of `requested'.
    """
    source = record.get("source_text", "")
    frags = fragments(analysis, prefix=f"{name}-") if analysis else []
    cuts = sorted((c["start"], c["end"], f["id"])
                  for f in frags for c in f.get("display_cues", []))
    runs, at = [], 0
    for start, end, note in cuts:
        if start < at:
            continue
        if start > at:
            runs.append({"t": source[at:start]})
        runs.append({"t": source[start:end], "n": note})
        at = end
    if at < len(source):
        runs.append({"t": source[at:]})

    # Put the quoted blocks back for DISPLAY only. The interpreter saw QUOTE in
    # their place, so no fragment can point into them; the reader should still
    # see what Joe pasted, and it is usually code.
    quotes = list(record.get("quotes") or [])
    if quotes:
        expanded = []
        for run in runs:
            parts = re.split(r"(?m)^QUOTE$", run["t"])
            for i, part in enumerate(parts):
                if i:
                    expanded.append({"t": quotes.pop(0) if quotes else "",
                                     "q": True})
                if part:
                    expanded.append(dict(run, t=part))
        runs = expanded

    notes = []
    for f in frags:
        notes.append({"id": f["id"],
                      "intent": "unresolved" if f.get("unresolved") else f.get("intent"),
                      "target": f.get("target", ""),
                      "rationale": f.get("rationale", ""),
                      "relations": f.get("relations", []),
                      "patterns": [{"id": r["id"], "why": r.get("rationale", "")}
                                   for r in f.get("pattern_refs", [])]})
    side = os.path.join(os.path.dirname(record.get("_path", "")) or "", "")
    authored = authored_cascade(name)
    return {"name": name, "at": record.get("created_at", ""),
            "agent": record.get("agent_id", ""),
            "surface": record.get("surface", "unrecorded"),
            "labeller": (analysis or {}).get("labeller"),
            "runs": runs, "notes": notes,
            "sexp": (tokenise_authored(authored["text"]) if authored
                     else derived_cascade(name, frags) if frags else None),
            "sexp-by": (authored or {}).get("by", "derived from the annotation"
                                            if frags else None),
            "candidates": [compact_candidate(c) for c in record.get("_candidates", [])]}


def summarise(name, record, analysis):
    head = record.get("source_text", "").strip().replace("\n", " ")
    return {"name": name, "at": record.get("created_at", ""),
            "head": html.escape(head[:140] + ("…" if len(head) > 140 else "")),
            "state": "interpreted by " + analysis["labeller"] if analysis
                     else "not yet interpreted"}


def write_session_log(a, written):
    """A log for one agent's turns that appends without a reload."""
    mine = [(n, r, x) for n, r, x in written if r.get("agent_id") == a.agent]
    mine.sort(key=lambda w: w[1].get("created_at", ""), reverse=True)
    rows = [summarise(n, r, x) for n, r, x in mine]

    json.dump([feed_entry(n, r, x) for n, r, x in mine],
              open(os.path.join(a.out, f"feed-{a.agent}.json"), "w"))
    manifest = f"session-{a.agent}.json"
    json.dump(rows, open(os.path.join(a.out, manifest), "w"), indent=1)

    body = "".join(
        f'<p class="note" data-turn="{r["name"]}" data-state="{r["state"]}">'
        f'<a href="{r["name"]}.html">{r["name"]}</a> — {r["head"]}<br>'
        f'<span class="rel">{r["at"]} · <span class="state">{r["state"]}</span></span></p>'
        for r in rows)
    page = os.path.join(a.out, f"session-{a.agent}.html")
    open(page, "w", encoding="utf-8").write(
        f"""<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{html.escape(a.agent)} — turn log</title><style>{CSS}
.fresh {{ animation: lit 2.5s ease-out; }}
@keyframes lit {{ from {{ background: #fbe6dd; }} to {{ background: transparent; }} }}
</style></head><body>
<h1>{html.escape(a.agent)} — turn log</h1>
<p class="meta">Newest first. The page asks for new turns every {a.poll}s; nothing to reload.
Interpretation lands later than the turn, so a row's state changes in place.</p>
<div class="notes" id="log">{body}</div>
<script>const MANIFEST = {json.dumps(manifest)}, POLL = {a.poll};{SESSION_JS}</script>
</body></html>""")
    print(page)
    print(os.path.join(a.out, manifest))


def agent_of(path):
    try:
        return json.load(open(path, encoding="utf-8")).get("agent_id")
    except (OSError, ValueError):
        return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("records", nargs="*")
    ap.add_argument("--latest", type=int, metavar="N")
    ap.add_argument("--out", default=DEFAULT_OUT)
    ap.add_argument("--records-dir", default=DEFAULT_RECORDS)
    ap.add_argument("--agent", metavar="AGENT_ID",
                    help="also write a live session log for this agent: "
                         "session-<agent>.html plus the manifest it polls")
    ap.add_argument("--poll", type=int, default=10, metavar="SECONDS",
                    help="how often the session log asks for the manifest")
    a = ap.parse_args()

    paths = list(a.records)
    if a.latest:
        every = [p for p in glob.glob(f"{a.records_dir}/turn-*.json")
                 if not p.endswith((".analysis.json", ".candidates.json"))]
        if a.agent:
            # The latest N of THIS agent's turns: counted across all agents,
            # one busy agent's turns push another's out of its own feed.
            every = [p for p in every if agent_of(p) == a.agent]
        paths += sorted(every, key=os.path.getmtime)[-a.latest:]
    if not paths:
        sys.exit("turn_margin_html: no records (pass paths or --latest N)")

    os.makedirs(a.out, exist_ok=True)
    written = []
    for path in paths:
        record, analysis = load(path)
        name = os.path.basename(path)[:-len(".json")]
        out = os.path.join(a.out, name + ".html")
        open(out, "w", encoding="utf-8").write(render(record, analysis, name))
        written.append((name, record, analysis))
        print(out)

    rows = []
    for name, record, analysis in sorted(written, key=lambda w: w[1].get("created_at", ""),
                                         reverse=True):
        head = (record.get("source_text", "")[:110] + "…") if record.get("source_text") else ""
        rows.append(f'<p class="note"><a href="{name}.html">{name}</a> — '
                    f'{html.escape(head)}<br>'
                    f'<span class="rel">{html.escape(record.get("created_at",""))} · '
                    f'{"interpreted" if analysis else "not yet interpreted"}</span></p>')
    if a.agent:
        write_session_log(a, written)

    index = os.path.join(a.out, "index.html")
    open(index, "w", encoding="utf-8").write(
        f"""<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Operator turns</title><style>{CSS}</style></head><body>
<h1>Operator turns</h1><p class="meta">rendered {datetime.now(timezone.utc).isoformat(timespec='seconds')}</p>
<div class="notes">{''.join(rows)}</div></body></html>""")
    print(index)


if __name__ == "__main__":
    main()
