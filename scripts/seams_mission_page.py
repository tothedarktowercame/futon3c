#!/usr/bin/env python3
"""seams_mission_page.py — M-futon-seams as a working page.

The mission text is the main column and accumulates as the mission is worked;
the margin carries two columns of annotations, (a) the pattern working and
(b) the PROOF-2a reading. The page is a view of two files, not a snapshot:
re-run it and it follows whatever the mission and the annotations now say.

  seams_mission_page.py OUT.html [--rev HEAD] [--worktree]

Deterministic for fixed inputs: the mission is read from a git revision, the
annotations are sorted by (anchor start, id), and nothing in the output is
derived from the clock.

Anchors are exact character spans of the mission at a recorded sha. At render
time each is re-checked against the revision being rendered; a note whose
quote is no longer at its offsets is drawn FLAGGED in place, never silently
moved. A moved anchor is a fact about the mission having changed under an
annotation, which is the thing a reader needs to see.
"""
import argparse, html, json, os, re, subprocess, sys, hashlib

REPO = "/home/joe/code/futon3c"
MISSION = "holes/missions/M-futon-seams.md"
ANNOTATIONS = "holes/labs/M-futon-seams/annotations.edn"
LIFECYCLE = "holes/labs/M-futon-seams/lifecycle.edn"
CASCADES = ["holes/labs/M-futon-seams/proto/instance-4.edn",
            "holes/labs/M-futon-seams/proto/instance-4b.edn",
            "holes/labs/M-futon-seams/proto/instance-5.edn",
            "holes/labs/M-futon-seams/proto/instance-6.edn",
            "holes/labs/M-futon-seams/proto/instance-7.edn"]
WIRINGS = ["holes/labs/M-futon-seams/wiring/instance-4-wiring.edn",
           "holes/labs/M-futon-seams/wiring/instance-4b-wiring.edn",
           "holes/labs/M-futon-seams/wiring/instance-5-wiring.edn",
           "holes/labs/M-futon-seams/wiring/instance-6-wiring.edn",
           "holes/labs/M-futon-seams/wiring/instance-7-wiring.edn"]

OPEN, CLOSE = "\x00", "\x01"          # anchor sentinels, absent from markdown

# The acceptance test Joe set: a figure's label text must render at the same
# size as the body text beside it. That fixes the figure's rendered width --
# rendered_label = css_font_px * (rendered_width / viewBox_width) -- so the
# width follows from the label size and the diagram's content, not from
# however wide the margin happens to be.
BODY_PX = 16.64          # 1.04rem at a 16px root; must track the body rule below
LABEL_PX = 12.5          # .nid and .wid, the primary label in both drawings


def sh(*args, **kw):
    return subprocess.run(args, capture_output=True, text=True, cwd=REPO, **kw)


def read_mission(rev, worktree):
    if worktree:
        return open(os.path.join(REPO, MISSION), encoding="utf-8").read(), "worktree"
    r = sh("git", "show", f"{rev}:{MISSION}")
    if r.returncode:
        sys.exit(f"seams_mission_page: cannot read {MISSION} at {rev}: {r.stderr.strip()}")
    return r.stdout, sh("git", "rev-parse", "--short", rev).stdout.strip()


def edn_to_json(path):
    r = sh("bb", "-e",
           '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
           f'(print (j/generate-string (edn/read-string (slurp "{path}"))))')
    if r.returncode:
        sys.exit(f"seams_mission_page: cannot read {path}: {r.stderr.strip()}")
    return json.loads(r.stdout)


# ---------------------------------------------------------------- markdown

def inline(s):
    """Escape, then the four inline forms the mission uses. Sentinels survive."""
    s = html.escape(s, quote=False)
    s = re.sub(r"`([^`]+)`", lambda m: f"<code>{m.group(1)}</code>", s)
    s = re.sub(r"\*\*([^*]+)\*\*", lambda m: f"<strong>{m.group(1)}</strong>", s)
    s = re.sub(r"(?<![*\w])\*([^*\n]+)\*(?!\*)", lambda m: f"<em>{m.group(1)}</em>", s)
    return s


def render_markdown(text):
    """A small renderer for the subset the mission uses. Deterministic."""
    out, lines, i = [], text.split("\n"), 0
    while i < len(lines):
        ln = lines[i]
        if ln.startswith("```"):
            j = i + 1
            buf = []
            while j < len(lines) and not lines[j].startswith("```"):
                buf.append(lines[j]); j += 1
            body = html.escape("\n".join(buf), quote=False)
            out.append(f"<pre class=\"code\">{body}</pre>")
            i = j + 1
            continue
        m = re.match(r"^(#{1,6})\s+(.*)$", ln)
        if m:
            lvl = len(m.group(1))
            slug = re.sub(r"[^a-z0-9]+", "-", m.group(2).lower()).strip("-")[:48]
            out.append(f'<h{lvl} id="s-{slug}">{inline(m.group(2))}</h{lvl}>')
            i += 1
            continue
        if re.match(r"^\s*[-*]\s+", ln):
            items = []
            while i < len(lines) and re.match(r"^\s*[-*]\s+", lines[i]):
                items.append(inline(re.sub(r"^\s*[-*]\s+", "", lines[i]))); i += 1
            out.append("<ul>" + "".join(f"<li>{x}</li>" for x in items) + "</ul>")
            continue
        if re.match(r"^\s*\d+\.\s+", ln):
            items = []
            while i < len(lines) and re.match(r"^\s*\d+\.\s+", lines[i]):
                items.append(inline(re.sub(r"^\s*\d+\.\s+", "", lines[i]))); i += 1
            out.append("<ol>" + "".join(f"<li>{x}</li>" for x in items) + "</ol>")
            continue
        if ln.startswith(">"):
            items = []
            while i < len(lines) and lines[i].startswith(">"):
                items.append(inline(lines[i].lstrip("> "))); i += 1
            out.append("<blockquote>" + " ".join(items) + "</blockquote>")
            continue
        if not ln.strip():
            i += 1
            continue
        para = []
        while i < len(lines) and lines[i].strip() and not lines[i].startswith(("#", "```", ">")) \
                and not re.match(r"^\s*([-*]|\d+\.)\s+", lines[i]):
            para.append(lines[i]); i += 1
        out.append(f"<p>{inline(' '.join(para))}</p>")
    return out


# ---------------------------------------------------------------- anchors

def place_anchors(text, notes):
    """Insert sentinels at each live anchor, outermost first, and report which
    anchors no longer hold. Offsets are applied from the end so earlier ones
    stay valid."""
    live, stale = [], []
    for n in notes:
        a = n["anchor"]
        s, e, q = a["start"], a["end"], a["quote"]
        if text[s:e] == q:
            live.append(n)
        else:
            found = text.find(q)
            n["_stale"] = ("quote found at %d, recorded %d" % (found, s) if found >= 0
                           else "quote no longer present in the mission")
            stale.append(n)
    marks = []
    for n in live:
        marks.append((n["anchor"]["start"], "open", n["id"]))
        marks.append((n["anchor"]["end"], "close", n["id"]))
    # apply right to left; at one position, closes before opens
    marks.sort(key=lambda m: (-m[0], 0 if m[1] == "close" else 1))
    for pos, kind, nid in marks:
        tag = f"{OPEN}{nid}{OPEN}" if kind == "open" else CLOSE
        text = text[:pos] + tag + text[pos:]
    return text, live, stale


def resolve_sentinels(rendered):
    rendered = re.sub(OPEN + r"([a-zA-Z0-9_-]+)" + OPEN,
                      lambda m: f'<span class="anchor" id="anc-{m.group(1)}" '
                                f'data-note="{m.group(1)}">', rendered)
    return rendered.replace(CLOSE, "</span>")


# ---------------------------------------------------------------- page

STATUS_LABEL = {"exit-met": "exit met", "in-progress": "in progress",
                "not-started": "not started"}


def toc_html(life):
    """One row per lifecycle phase, with the phase's own exit criterion and
    the evidence the status was read from. Status comes from the criterion
    against the evidence; where that disagrees with the mission's Status line
    the row says so."""
    rows = []
    for ph in life["phases"]:
        st = ph["status"]
        ev = "".join(
            f'<li><span class="evkind">{html.escape(e["kind"])}</span> '
            + (f'<code>{html.escape(e["ref"])}</code> — ' if e.get("ref") else "")
            + html.escape(e["what"]) + "</li>"
            for e in ph.get("evidence", []))
        arte = len(ph.get("artefacts", []))
        rows.append(
            f'<tr class="ph-{st}">'
            f'<td class="phname"><a href="#anc-phase-{ph["id"]}">{html.escape(ph["title"])}</a>'
            + ('' if ph.get("mission-anchor") else
               ' <span class="nosec">no section in the mission</span>') +
            f'</td>'
            f'<td class="phstat"><span class="dot d-{st}"></span>'
            f'{STATUS_LABEL[st]}</td>'
            f'<td class="phexit">“{html.escape(ph["exit"])}”'
            f'<span class="exitsrc">mission-lifecycle.md:{ph["exit-line"]}</span></td>'
            f'<td class="phwhy">{html.escape(ph["because"])}'
            + (f'<ul class="phev">{ev}</ul>' if ev else "")
            + (f'<p class="phart">{arte} artefact{"s" if arte != 1 else ""} '
               f'in the margin below</p>' if arte else "")
            + '</td></tr>')
    o = life["overall"]
    return (f'<section class="toc" id="toc">'
            f'<h2>Where this mission stands</h2>'
            f'<p class="tocnote">Status per phase is read from that phase\'s exit criterion in '
            f'<code>futon4/holes/mission-lifecycle.md</code> against the evidence, not from the '
            f'mission\'s Status line. The mission\'s Status line says '
            f'<b>{html.escape(life["mission"]["status-line"])}</b>. '
            f'Nothing here advances it: that is the owner\'s act, and the owner is unassigned.</p>'
            f'<div class="tocwrap"><table class="toctable"><thead><tr><th>phase</th><th>status</th>'
            f'<th>exit criterion</th><th>read from</th></tr></thead>'
            f'<tbody>{"".join(rows)}</tbody></table></div>'
            f'<p class="tocfind"><b>Overall.</b> {html.escape(o["finding"])}</p>'
            f'</section>')


def phase_section(ph, in_mission):
    """A heading in the main column for each phase, so its artefacts have a
    place to sit. Where the mission has written the phase, this points at it;
    where it has not, the placeholder says so and is drawn as a placeholder --
    it is not mission text and must not read as any."""
    st = ph["status"]
    if in_mission:
        body = (f'<p class="phbody">Written in the mission above, at '
                f'<code>{html.escape(ph["mission-anchor"]["quote"])}</code>. '
                f'{html.escape(ph["because"])}</p>')
        cls = "phase inmission"
    else:
        body = (f'<p class="phbody"><b>Not yet written in the mission.</b> '
                f'{html.escape(ph["because"])}</p>')
        cls = "phase placeholder"
    return (f'<section class="{cls}">'
            f'<h2 id="anc-phase-{ph["id"]}" class="phhead" data-note="phase-{ph["id"]}">'
            f'<span class="phn">{ph["n"]}</span> {html.escape(ph["title"])}'
            f'<span class="dot d-{st}"></span>'
            f'<span class="phstatword">{STATUS_LABEL[st]}</span></h2>'
            f'<p class="phexit2">Exit criterion: “{html.escape(ph["exit"])}”</p>'
            + body + '</section>')


def figure_width(svg):
    """The width at which this drawing's labels come out at body size."""
    m = re.search(r'viewBox="0 0 ([0-9.]+) ([0-9.]+)"', svg)
    return round(float(m.group(1)) * BODY_PX / LABEL_PX) if m else None


def figure_html(key, svg, cap, num, anchor_id):
    """A numbered figure sized so its labels match the body text."""
    marks = "".join(f'<li>{html.escape(m)}</li>' for m in cap.get("marks", []))
    w = figure_width(svg)
    style = f' style="width:{w}px"' if w else ""
    return (f'<figure class="marginfig" id="fig-{num}" data-anchor="{html.escape(anchor_id)}" '
            f'data-fig="{html.escape(key)}" data-natural-width="{w}">'
            f'<div class="figbody"{style} title="click to enlarge">{svg}'
            f'<span class="figopen">enlarge ⤢</span></div>'
            f'<figcaption><span class="fignum">Figure {num}</span> '
            f'{html.escape(cap.get("what", key))} — instance {cap.get("instance", "?")}. '
            f'{html.escape(cap.get("sub", ""))}'
            + (f' <span class="figwhy">{html.escape(cap["why"])}</span>'
               if cap.get("why") else "")
            + (f'<ul class="figmarks">{marks}</ul>' if marks else "")
            + '</figcaption></figure>')


def note_html(n, figures=None, anchor_override=None):
    col = n.get("column", "pattern")
    cls = "note col-" + ("a" if col == "pattern" else "b")
    if n.get("status") != "reviewed":
        cls += " draft"
    if n.get("_stale"):
        cls += " stale"
    verdict = n.get("verdict")
    bits = []
    head = [f'<span class="nid">{html.escape(n["id"])}</span>',
            f'<span class="nauth">{html.escape(n.get("author", "?"))}</span>']
    if n.get("status") != "reviewed":
        head.append('<span class="ndraft">draft</span>')
    if verdict:
        head.append(f'<span class="verdict v-{html.escape(verdict)}">'
                    f'{html.escape(verdict)}</span>')
    bits.append('<p class="nhead">' + " ".join(head) + "</p>")
    if n.get("_stale"):
        bits.append(f'<p class="nstale">anchor no longer holds — {html.escape(n["_stale"])}. '
                    f'Recorded quote: “{html.escape(n["anchor"]["quote"][:80])}”</p>')
    bits.append(f'<p class="nbody">{html.escape(n.get("body",""))}</p>')
    ref = n.get("_figref")
    if ref:
        bits.append(f'<p class="nseefig">See <a href="#fig-{ref}">Figure {ref}</a> '
                    f'{n.get("_figdir", "below")}.</p>')
    if n.get("refs"):
        bits.append('<p class="nrefs">' +
                    " · ".join(f"<code>{html.escape(r)}</code>" for r in n["refs"]) + "</p>")
    if n.get("_why"):
        bits.insert(1, f'<p class="nwhy">{html.escape(n["_why"])}</p>')
    if n.get("_at-phase"):
        bits.append(f'<p class="natphase">placed at {html.escape(n["_at-phase"])}; '
                    f'anchored in the mission at '
                    f'<a href="#anc-{html.escape(n["id"])}">its span</a></p>')
    return (f'<aside class="{cls}" id="note-{html.escape(n["id"])}" '
            f'data-anchor="{html.escape(anchor_override or n["id"])}">'
            + "".join(bits) + "</aside>")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--rev", default="HEAD")
    ap.add_argument("--worktree", action="store_true",
                    help="render the working copy instead of a committed revision")
    a = ap.parse_args()

    text, revlabel = read_mission(a.rev, a.worktree)
    sha = hashlib.sha256(text.encode()).hexdigest()
    notes = edn_to_json(ANNOTATIONS)
    notes.sort(key=lambda n: (n["anchor"]["start"], n["id"]))
    life = edn_to_json(LIFECYCLE)
    # Which phase claims each note and each figure. An artefact is placed
    # beside the phase that produced it, in phase order, rather than beside
    # whichever mission sentence happens to mention it.
    note_phase, fig_phase = {}, {}
    for ph in life["phases"]:
        for art in ph.get("artefacts", []):
            if art.get("note"):
                note_phase[art["note"]] = ph["id"]
            if art.get("figure"):
                fig_phase[art["figure"]] = ph["id"]

    figures, captions = {}, {}
    bundle = os.path.join("/tmp", "seams-bundle.json")
    r = sh("bb", "holes/labs/M-futon-seams/build_page.clj", bundle, *CASCADES)
    if r.returncode == 0 and os.path.exists(bundle):
        sys.path.insert(0, os.path.join(REPO, "scripts"))
        import seams_page
        for b in json.load(open(bundle)):
            cand = str(b.get("candidate") or "")
            key = f"instance-{b['instance']}{'b' if 'b-observe' in cand else ''}"
            figures[key] = seams_page.svg(b, vertical=True)
            marks = []
            miss = (b.get("meets") or {}).get("missing") or []
            if miss:
                pairs = {tuple(m["maximal-common"]) for m in miss}
                for mc in sorted(pairs):
                    marks.append("missing meet, amber dashed: the maximal common units are "
                                 + " and ".join(seams_page.short(x) for x in mc))
            confs = {tuple(sorted((a, bb))) for w in b.get("wide-states", [])
                     for a, bb, _ in w.get("conflicts", [])}
            for c in sorted(confs):
                marks.append("conflicting frontier, red dashed: "
                             + " and ".join(seams_page.short(x) for x in c)
                             + " each produce a token the other forbids")
            for h in (b.get("holes") or []):
                if h.get("status") != "closed":
                    marks.append("unproduced want: " + seams_page.short(h["token"]))
            captions[key] = {
                "what": "Pattern cascade" + (f", candidate {cand.split('/')[-1]}" if cand else ""),
                "instance": b["instance"],
                "sub": (f"{len(b['patterns'])} patterns, {len(b['above'])} edges, "
                        f"{b['linear-extensions']} linear extensions; depth downward, "
                        f"solid = differentiates, dashed = jointly with"),
                "marks": marks}

    # the wiring diagrams: the construction beside the argument
    import seams_wiring
    for path in WIRINGS:
        w = edn_to_json(path)
        # instance 4 has two candidates; the key distinguishes them by the
        # candidate id, not by "has one at all" -- candidate a has one too.
        cand = str(w.get("candidate") or "")
        suffix = "b" if "b-observe" in cand else ""
        key = f"wiring-{w['instance']}{suffix}"
        figures[key] = seams_wiring.svg(w)
        devs = [n for n in w["nodes"] if n.get("role") == "deviation"]
        marks = []
        for d in (w.get("dangling-outputs") or []):
            marks.append(f"dangling output, amber stub marked unused: "
                         f"{d['node']} produces {d['token']}, which no box needs and "
                         f"nothing wants")
        for u in (w.get("unfed-wants") or []):
            marks.append(f"unfed want, amber open port: nothing produces {u['want']}")
        if w.get("derived"):
            marks.append("derived from the cascade and plan-only: nothing here is built, "
                         "so every interior node is hungry")
        for d in devs:
            marks.append("deviation node, red and below the path: "
                         + d["form"][:90] + " — licensed by nothing in the cascade")
        captions[key] = {
            "what": "Wiring diagram" + (" (derived)" if w.get("derived") else ""),
            "instance": w["instance"],
            "sub": ("token flow: a box's input ports are the tokens it needs (top) and "
                    "its output ports the tokens it produces (bottom); an edge carries one "
                    "named token from a producer's port to a consumer's. A filled red port "
                    "is a token the box FORBIDS. Solid green carries a witness, amber dashed "
                    "carries what it owes, ⊢ names the licensing pattern."),
            "marks": marks}

    marked, live, stale = place_anchors(text, notes)
    blocks = [resolve_sentinels(b) for b in render_markdown(marked)]

    # Each note is emitted in the flow, directly after the block its anchor
    # sits in. On a wide screen the script lifts it into the margin and levels
    # it with the anchor; on a narrow one it stays where it is and folds under
    # the passage it belongs to, which is the behaviour a phone needs.
    # Figures are numbered by walking the phases in order, BEFORE the body is
    # assembled, so a note at a mission span knows whether its figure is above
    # it or below it. They all sit in phase sections after the mission text,
    # so a span-anchored note points down and a note beside its own figure
    # points up.
    fignums = {}
    for ph in sorted(life["phases"], key=lambda q: q["n"]):
        for art in ph.get("artefacts", []):
            fig = art.get("figure")
            if fig and fig in figures:
                fignums[fig] = len(fignums) + 1
    for n in notes:
        fig = n.get("figure")
        if fig in fignums:
            n["_figref"] = fignums[fig]
            n["_figdir"] = "above" if n["id"] in note_phase else "below"

    placed = set()
    body_parts = []
    for b in blocks:
        body_parts.append(b)
        for n in notes:
            if n["id"] in placed or n["id"] in note_phase:
                continue          # a claimed note waits for its phase
            if f'id="anc-{n["id"]}"' in b:
                body_parts.append(note_html(n))
                placed.add(n["id"])

    # Phase order after the mission text: the War Machine works a mission
    # phase by phase, so the artefacts appear where they would have been made.
    by_id = {n["id"]: n for n in notes}
    for ph in sorted(life["phases"], key=lambda p: p["n"]):
        body_parts.append(phase_section(ph, bool(ph.get("mission-anchor"))))
        for art in ph.get("artefacts", []):
            fig = art.get("figure")
            if fig and fig in figures:
                cap = dict(captions.get(fig, {}))
                cap["phase"] = ph["id"]
                cap["why"] = art.get("why", "")
                body_parts.append(figure_html(fig, figures[fig], cap,
                                              fignums[fig], f"phase-{ph['id']}"))
            nid = art.get("note")
            if nid and nid in by_id and nid not in placed:
                n = dict(by_id[nid])
                n["_at-phase"] = ph["id"]
                n["_why"] = art.get("why", "")
                body_parts.append(note_html(n, anchor_override=f"phase-{ph['id']}"))
                placed.add(nid)

    for n in notes:
        if n["id"] not in placed:
            body_parts.append(note_html(n))
    body = "\n".join(body_parts)

    cols = {"a": [n for n in notes if n.get("column") == "pattern"],
            "b": [n for n in notes if n.get("column") == "proof2a"]}

    counts = (f'{len(notes)} notes — {len(cols["a"])} pattern, {len(cols["b"])} PROOF-2a; '
              f'{sum(1 for n in notes if n.get("status") == "reviewed")} reviewed, '
              f'{len(stale)} with a stale anchor')

    open(a.out, "w", encoding="utf-8").write(PAGE.format(
        css=CSS, js=JS, body=body, toc=toc_html(life), rev=html.escape(revlabel),
        sha=sha[:16], counts=html.escape(counts),
        mission=html.escape(MISSION)))
    print(a.out)
    print(f"  rev {revlabel} sha {sha[:16]} · {counts}")
    for n in stale:
        print(f"  STALE {n['id']}: {n['_stale']}")


CSS = """
/* Mission left, a WIDE margin right: the margin is a working second column,
   not a gutter. The text keeps a reading measure and the margin takes all the
   rest, which is the arrangement the mark7 typeset previews use. */
:root { --measure: 33rem; --gutter: 2.4rem; --pad: 3vw;
        --margin-w: max(26rem, calc(100vw - 2*var(--pad) - var(--measure) - var(--gutter))); }
*,*::before,*::after { box-sizing:border-box; }
body { margin:0; padding:2.5rem var(--pad) 8rem; background:#fffff8; color:#111;
       font:1.04rem/1.65 et-book, Palatino, "Palatino Linotype", Georgia, serif; }
.page { display:grid; grid-template-columns: var(--measure) var(--margin-w);
        column-gap:var(--gutter); align-items:start; }
.main { grid-column:1; min-width:0; }
.margin { grid-column:2; position:relative; min-width:0; }
/* Not sticky: it is a one-line label and the columns are colour-coded, so
   keeping it pinned bought nothing and painted over the top of any figure
   that scrolled under it. */
.mhead { margin:0;
         padding:.15rem 0 .35rem; border-bottom:1px solid #eae6d8;
         font-size:.66rem; font-variant:small-caps; letter-spacing:.08em; color:#999;
         display:grid; grid-template-columns:1fr 1fr; column-gap:1.6rem; }
.mh-a { color:#1b6b3a; } .mh-b { color:#2a4d8f; }
h1 { font-size:1.6rem; font-weight:400; margin:0 0 .2rem; }
h2 { font-size:1.18rem; font-weight:400; margin:2.4rem 0 .5rem;
     border-bottom:1px solid #e6e2d4; padding-bottom:.25rem; }
h3 { font-size:1rem; font-weight:600; margin:1.7rem 0 .4rem; }
p { margin:0 0 .88rem; }
code { font-family:ui-monospace,Menlo,Consolas,monospace; font-size:.85em; }
pre.code { font-family:ui-monospace,Menlo,Consolas,monospace; font-size:.71rem;
           line-height:1.5; background:#f4f2ea; border-left:3px solid #c9c4b0;
           padding:.6rem .8rem; overflow-x:auto; white-space:pre-wrap; }
blockquote { margin:0 0 .88rem; padding-left:.9rem; border-left:3px solid #c9c4b0; color:#444; }
ul,ol { margin:0 0 .88rem; padding-left:1.2rem; }
li { margin:0 0 .3rem; }
.anchor { border-bottom:1.5px solid #b8431f; cursor:pointer; }
.anchor.lit { background:#fbe6dd; }

/* A note lives in the flow and is lifted into the margin by the script when
   there is room. Both states are readable; only one is Tufte. */
.note { font-size:.74rem; line-height:1.5; border-left:2px solid #ddd;
        padding:.2rem 0 .2rem .65rem; color:#333; margin:.7rem 0 1rem 1.2rem;
        background:#fffff8; }
.note.col-a { border-left-color:#1b6b3a; }
.note.col-b { border-left-color:#2a4d8f; }
.note.draft { background:repeating-linear-gradient(135deg,transparent,transparent 8px,
              rgba(0,0,0,.02) 8px, rgba(0,0,0,.02) 16px); }
.note.stale { border-left-color:#b8431f; background:#fdf1ec; }
.note.lit { background:#fbe6dd; }
.note.inmargin { position:absolute; margin:0; width:calc(50% - .8rem); }
.note.inmargin.col-b { left:calc(50% + .8rem); }

/* A figure gets the WHOLE margin, not half of it, and is legible there
   without enlarging: the cascades are laid out depth-downward for exactly
   this reason. Clicking still opens it larger for detail. */
.marginfig { margin:1.2rem 0 1.4rem; padding:0; }
.marginfig.inmargin { position:absolute; width:100%; margin:0; }
/* Width is set per figure so its labels render at body size; max-width keeps
   it inside the margin on a screen too narrow for that, which is a text-size
   miss and is reported by scripts/check_seams_layout.js. */
.figbody { position:relative; border:1px solid #eae6d8; background:#fff;
           padding:.4rem; cursor:zoom-in; overflow:hidden;
           box-sizing:content-box;
           /* content-box, so the stated width is the drawing's width and the
              label lands at body size. max-width has to subtract the padding
              and border it does NOT include, or a narrow screen gets 2px of
              horizontal scroll. */
           max-width:calc(100% - .8rem - 2px); }
.figbody svg { display:block; width:100%; height:auto; }
.marginfig figcaption { font-size:.68rem; line-height:1.45; color:#666;
                        padding:.35rem .1rem 0; }
.fignum { font-variant:small-caps; letter-spacing:.06em; color:#333; font-weight:600; }
.figmarks { margin:.25rem 0 0; padding-left:1rem; }
.figmarks li { margin:0 0 .15rem; color:#8a5a12; }
.nseefig { margin:.35rem 0 0; font-size:.7rem; }
.nseefig a { color:#b8431f; }
.nhead { margin:0 0 .25rem; font-size:.63rem; font-family:ui-monospace,Menlo,monospace; }
.nid { color:#555; } .nauth { color:#aaa; margin-left:.4rem; }
.ndraft { color:#a8791d; margin-left:.4rem; font-variant:small-caps; letter-spacing:.05em; }
.verdict { margin-left:.4rem; font-variant:small-caps; letter-spacing:.05em; }
.v-fits { color:#1b6b3a; } .v-breaks { color:#b8431f; font-weight:700; }
.v-missing-field { color:#a8791d; }
.nbody { margin:0; }
.nstale { margin:0 0 .3rem; color:#b8431f; font-size:.68rem; }
.nrefs { margin:.3rem 0 0; color:#999; font-size:.62rem; word-break:break-all; }
.nfig { margin:.4rem 0; overflow:hidden; cursor:zoom-in; position:relative;
        border:1px solid #eae6d8; background:#fff; padding:.2rem; }
.nfig svg { width:100%; height:auto; min-width:0; }
.wiring { max-width:100%; height:auto; }
.wid { font-size:12.5px; fill:#333; font-weight:600; font-family:ui-monospace,Menlo,monospace; }
.wform { font-size:8.5px; fill:#666; }
.wlic { font-size:8.5px; fill:#1b6b3a; font-family:ui-monospace,Menlo,monospace; }
.wdev { font-size:8.5px; fill:#b8431f; font-family:ui-monospace,Menlo,monospace; }
.wtok { font-size:7.5px; fill:#667; font-family:ui-monospace,Menlo,monospace; }
.wloose { font-size:7.5px; fill:#a8791d; font-family:ui-monospace,Menlo,monospace; }
.wnode { cursor:help; }
.nmeet{ font-size:9.5px; fill:#a8791d; font-family:ui-monospace,Menlo,monospace; }
.nconf{ font-size:9.5px; fill:#b8431f; font-weight:700; font-family:ui-monospace,Menlo,monospace; }
.nhole{ font-size:11px; fill:#a8791d; font-weight:700; font-family:ui-monospace,Menlo,monospace; }
.nfam { font-size:10px; fill:#999; font-family:ui-monospace,Menlo,monospace; }
.nid  { font-size:12.5px; fill:#1b6b3a; font-weight:600; font-family:ui-monospace,Menlo,monospace; }
.nprod{ font-size:10.5px; fill:#555; font-family:ui-monospace,Menlo,monospace; }
.node { cursor:pointer; }
.figopen { position:absolute; right:.25rem; bottom:.2rem; font-size:.58rem; color:#999;
           font-family:ui-monospace,Menlo,monospace; background:#fffff8; padding:0 .2rem; }
.figmodal { position:fixed; inset:0; background:rgba(255,255,248,.97); z-index:50;
            display:flex; align-items:center; justify-content:center; padding:3vw;
            cursor:zoom-out; }
.figmodal svg { width:100%; max-width:1600px; height:auto; }
.figmodal .figcap { position:absolute; top:1.2rem; left:3vw; font-size:.72rem;
                    color:#888; font-family:ui-monospace,Menlo,monospace; }
/* The table of contents: the mission's standing, read from the lifecycle's
   own exit criteria. It spans both columns because it is about the whole
   document, not about any span of it. */
.toc { max-width:none; margin:0 0 2.8rem; }
/* The table is four columns of prose. On a narrow screen it scrolls inside
   its own box rather than making the whole document scroll sideways. */
.tocwrap { overflow-x:auto; max-width:100%; }
.toctable { min-width:46rem; }
.toc h2 { margin:0 0 .3rem; border:0; }
.tocnote { font-size:.8rem; color:#666; max-width:48rem; }
.toctable { border-collapse:collapse; font-size:.74rem; width:100%; max-width:none; }
.toctable th { text-align:left; font-variant:small-caps; letter-spacing:.05em;
               color:#888; border-bottom:1px solid #ddd; padding:.3rem .8rem .3rem 0;
               font-weight:600; }
.toctable td { padding:.45rem .8rem .45rem 0; border-bottom:1px solid #f0ece0;
               vertical-align:top; }
.phname { width:15rem; } .phname a { color:#111; text-decoration:none;
          border-bottom:1px solid #ddd; }
.nosec { color:#a8791d; font-size:.66rem; display:block; }
.phstat { width:7rem; white-space:nowrap; }
.phexit { width:22rem; color:#444; font-style:italic; }
.exitsrc { display:block; font-style:normal; color:#aaa; font-size:.64rem;
           font-family:ui-monospace,Menlo,monospace; }
.phwhy { color:#333; }
.phev { margin:.3rem 0 0; padding-left:1rem; font-size:.68rem; color:#666; }
.evkind { font-variant:small-caps; letter-spacing:.04em; color:#999; }
.phart { margin:.3rem 0 0; font-size:.66rem; color:#1b6b3a; }
.dot { display:inline-block; width:.55rem; height:.55rem; border-radius:50%;
       margin-right:.35rem; vertical-align:baseline; }
.d-exit-met { background:#1b6b3a; }
.d-in-progress { background:#a8791d; }
.d-not-started { background:#ccc; }

/* A phase section. One that the mission has written points at it; one it has
   not is a PLACEHOLDER -- dashed and tinted, so it cannot be read as mission
   text that happens to be short. */
.phase { margin:2.2rem 0 1rem; }
.phase.placeholder { border-left:3px dashed #c9c4b0; padding:.5rem 0 .4rem .9rem;
                     background:#fbfaf3; }
.phase.inmission { border-left:3px solid #e6e2d4; padding:.5rem 0 .4rem .9rem; }
.phhead { font-size:1.05rem; margin:0 0 .3rem; border:0; padding:0; }
.phn { display:inline-block; min-width:1.4rem; color:#aaa;
       font-family:ui-monospace,Menlo,monospace; font-size:.8rem; }
.phstatword { font-size:.68rem; color:#888; font-variant:small-caps;
              letter-spacing:.05em; }
.phexit2 { font-size:.74rem; color:#666; font-style:italic; margin:0 0 .4rem; }
.phbody { font-size:.82rem; color:#444; margin:0; }
.nwhy { margin:0 0 .3rem; font-size:.7rem; color:#1b6b3a; }
.natphase { margin:.3rem 0 0; font-size:.63rem; color:#999; }
.natphase a { color:#999; }
.figwhy { color:#1b6b3a; }

.masthead { max-width:var(--measure); }
.sub { color:#666; font-size:.82rem; margin-bottom:.4rem; }
.prov { color:#999; font-size:.68rem; font-family:ui-monospace,Menlo,monospace;
        margin-bottom:2rem; }
.prov a { color:#777; }
footer { margin-top:5rem; padding-top:1rem; border-top:1px solid #e6e2d4;
         color:#999; font-size:.7rem; }
footer a { color:#999; }

/* Narrow: one column, and every note stays exactly where the flow put it --
   under the passage it annotates, never hidden. */
@media (max-width:81.99rem) {
  .page { grid-template-columns:1fr; }
  .margin { display:none; }
  .note { margin-left:.8rem; }
}
"""

JS = """
// Lift each note into the wide margin and level it with its anchor. Two
// sub-columns inside the one margin: the pattern working on the left, the
// PROOF-2a reading on the right. Notes that would collide stack downward.
// With the script off, or on a narrow screen, notes stay in the flow under
// the passage they annotate -- which is why they are emitted there.
function layout() {
  const margin = document.querySelector('.margin');
  const main = document.querySelector('.main');
  const wide = window.matchMedia('(min-width: 82rem)').matches;
  const notes = [...document.querySelectorAll('.note, .marginfig')];
  if (!wide) {
    notes.forEach(n => {
      n.classList.remove('inmargin');
      const anc = document.getElementById('anc-' + n.dataset.anchor);
      const host = anc ? anc.closest('.main > *') : null;
      if (host && host.nextSibling !== n) host.after(n);
      n.style.top = '';
    });
    return;
  }
  const mtop = margin.getBoundingClientRect().top + window.scrollY;
  const bottom = {a: 34, b: 34};   // clear the sticky column heads
  notes.forEach(n => {
    if (n.parentElement !== margin) margin.appendChild(n);
    n.classList.add('inmargin');
  });
  notes.forEach(n => {
    const fig = n.classList.contains('marginfig');
    const col = n.classList.contains('col-b') ? 'b' : 'a';
    const anc = document.getElementById('anc-' + n.dataset.anchor);
    const floor = fig ? Math.max(bottom.a, bottom.b) : bottom[col];
    const want = anc ? anc.getBoundingClientRect().top + window.scrollY - mtop : floor;
    const y = Math.max(want, floor + 12);
    n.style.top = y + 'px';
    if (fig) { bottom.a = bottom.b = y + n.offsetHeight; }
    else { bottom[col] = y + n.offsetHeight; }
  });
  margin.style.minHeight = Math.max(bottom.a, bottom.b, main.offsetHeight) + 'px';
}
document.addEventListener('click', e => {
  const open = e.target.closest('.figbody');
  if (open) {
    const m = document.createElement('div');
    m.className = 'figmodal';
    const host = open.closest('.marginfig');
    m.innerHTML = '<span class="figcap">' + (host ? host.id.replace('fig-', 'Figure ') : '')
                + ' — ' + (host ? host.dataset.fig : '') + '</span>'
                + open.querySelector('svg').outerHTML;
    m.addEventListener('click', () => m.remove());
    document.body.appendChild(m);
    return;
  }
  const t = e.target.closest('.anchor, .note');
  document.querySelectorAll('.lit').forEach(x => x.classList.remove('lit'));
  if (!t) return;
  const id = t.dataset.note || t.dataset.anchor;
  document.getElementById('anc-' + id)?.classList.add('lit');
  document.getElementById('note-' + id)?.classList.add('lit');
});
window.addEventListener('load', layout);
window.addEventListener('resize', layout);
"""

PAGE = """<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>M-futon-seams — worked against PROOF-2a</title>
<style>{css}</style></head><body>
<div class="masthead">
<h1>M-futon-seams, worked</h1>
<p class="sub">The mission is the main text and grows as it is worked. The margin carries
two columns: <b>the pattern working</b> — cascade nodes, work-state tokens, context→pattern
edges — and <b>the PROOF-2a reading</b> of the same span: which clause or data shape it
exercises, and whether this example fits the shape, breaks it, or needs a field that does
not exist. Breaks are the evidence worth most. Click a marked span or a note to pair them.</p>
<p class="prov">{mission} at {rev} · sha256 {sha}… · {counts}<br>
kernel tables, conflict states and the full-size lattices:
<a href="seams-kernels.html">seams-kernels.html</a></p>
</div>
{toc}
<div class="page">
  <article class="main">{body}</article>
  <div class="margin" aria-hidden="false">
    <p class="mhead"><span class="mh-a">the pattern working</span><span class="mh-b">the PROOF-2a reading</span></p>
  </div>
</div>
<footer>Generated by <code>scripts/seams_mission_page.py</code> from
<code>{mission}</code> and <code>holes/labs/M-futon-seams/annotations.edn</code>.
Re-runnable and byte-deterministic for a fixed revision and annotation set.
Anchors are checked against the revision rendered; a note whose quote has moved is
drawn flagged in place rather than relocated.</footer>
<script>{js}</script>
</body></html>
"""

if __name__ == "__main__":
    main()
