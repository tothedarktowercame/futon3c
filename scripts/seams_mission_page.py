#!/usr/bin/env python3
"""seams_mission_page.py — M-futon-seams as a working page.

The mission text is the main column and accumulates as the mission is worked;
the margin carries two columns of annotations, (a) the pattern working and
(b) the PROOF-2a reading. The page is a view of two files, not a snapshot:
re-run it and it follows whatever the mission and the annotations now say.

  seams_mission_page.py OUT.html [--rev HEAD] [--worktree]
                        [--mission PATH] [--lab DIR]

--mission and --lab default to M-futon-seams, so every existing invocation
renders exactly what it did before -- byte for byte, which is the control
test for this change. A lab is a directory: annotations.edn and lifecycle.edn
are read from under it, and so are the cascade prototypes the figures come
from. Either file may be absent. An absent file is drawn as a typed absence
on the page: no annotations means an empty margin saying that it is empty,
and no lifecycle means every phase reads "not read" with its exit criterion
quoted from futon4/holes/mission-lifecycle.md. A phase is never reported as
met by a page that has nothing to read a verdict from.

Deterministic for fixed inputs: the mission is read from a git revision, the
annotations are sorted by (anchor start, id), and nothing in the output is
derived from the clock.

Anchors are exact character spans of the mission at a recorded sha. At render
time each is re-checked against the revision being rendered; a note whose
quote is no longer at its offsets is drawn FLAGGED in place, never silently
moved. A moved anchor is a fact about the mission having changed under an
annotation, which is the thing a reader needs to see.
"""
import argparse, glob, html, json, os, re, subprocess, sys, hashlib

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from mission_anchors import heading_changed

REPO = "/home/joe/code/futon3c"
DEFAULT_MISSION = "holes/missions/M-futon-seams.md"
DEFAULT_LAB = "holes/labs/M-futon-seams"

# These were literals. They are still module-level and still repo-relative,
# because every reader of them expects that; only where they come from has
# changed. use_lab() sets them from --lab before anything reads them.
MISSION = DEFAULT_MISSION
LAB = DEFAULT_LAB
ANNOTATIONS = os.path.join(LAB, "annotations.edn")
LIFECYCLE = os.path.join(LAB, "lifecycle.edn")
CASCADES = []
WIRINGS = []


def use_lab(lab):
    """Point the module at a lab directory.

    The cascade and wiring lists were written out by hand. Globbing them
    gives the same five files in the same order for M-futon-seams -- which
    the byte-identity control test is what actually checks -- and gives a
    different lab its own, rather than silently drawing M-futon-seams's
    figures onto someone else's mission.
    """
    global LAB, ANNOTATIONS, LIFECYCLE, CASCADES, WIRINGS
    LAB = lab
    ANNOTATIONS = os.path.join(lab, "annotations.edn")
    LIFECYCLE = os.path.join(lab, "lifecycle.edn")

    def under(sub_dir, pattern):
        hits = sorted(glob.glob(os.path.join(REPO, lab, sub_dir, pattern)))
        return [os.path.relpath(h, REPO) for h in hits]

    CASCADES = under("proto", "instance-*.edn")
    WIRINGS = under("wiring", "instance-*-wiring.edn")

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


def mission_name(text, path):
    """The mission's own name, from its `# Mission: <name>` first heading.

    Refusing is the whole point of the check. This script is furniture built
    around a mission -- a phase table, a status line, a margin of anchored
    annotations -- and pointed at a file that is not a mission it would
    render all of that around arbitrary prose and look entirely convincing.
    """
    first = next((ln for ln in text.split("\n") if ln.startswith("# ")), None)
    m = re.match(r"# Mission:\s*(\S.*?)\s*$", first or "")
    if not m:
        sys.exit(f"seams_mission_page: {path} is not a mission — its first H1 is "
                 f"{first if first else '(no H1 at all)'!r}, and a mission's is "
                 f"'# Mission: <name>'. Nothing written.")
    return m.group(1)


def status_line_of(text):
    """The mission's own Status line, for the phase table to quote back."""
    m = re.search(r"^\*\*Status:\*\*\s*(.+?)\s*$", text, re.M)
    return m.group(1) if m else "(the mission carries no Status line)"


def lifecycle_doc():
    """futon4/holes/mission-lifecycle.md, which defines the eight phases.

    House convention is that every repo sits at its canonical path; a
    worktree of this one does not, so look beside this checkout first and
    fall back to canonical. Return None rather than exit: a missing
    lifecycle document is one more absence to state on the page, and this
    is the path taken only when the lab had no lifecycle.edn either.
    """
    for root in (os.path.dirname(REPO), "/home/joe/code"):
        doc = os.path.join(root, "futon4", "holes", "mission-lifecycle.md")
        if os.path.isfile(doc):
            return doc
    return None


def generic_lifecycle(text):
    """Every phase unread, each with its exit criterion quoted.

    A lab with no lifecycle.edn has recorded no verdict on any phase, so the
    page must not report one. It reports the absence instead, and quotes
    what each phase would have to satisfy -- which is the useful half, and
    is the same document every lifecycle.edn reads its criteria from.
    """
    doc = lifecycle_doc()
    if not doc:
        return {"phases": [], "read": False, "doc": None,
                "overall": {"finding": "No lifecycle.edn in the lab, and "
                            "futon4/holes/mission-lifecycle.md was not found "
                            "beside this checkout or at /home/joe/code, so not "
                            "even the exit criteria can be quoted."},
                "mission": {"status-line": status_line_of(text)}}
    lines = open(doc, encoding="utf-8").read().split("\n")
    phases, cur = [], None
    for i, ln in enumerate(lines, 1):
        h = re.match(r"^### (?:(\d+)\. )?([A-Z][A-Z]+)\b(.*)$", ln)
        if h:
            cur = {"id": h.group(2),
                   "n": int(h.group(1)) if h.group(1) else 0,
                   "title": h.group(2), "status": "not-started",
                   "label": "not read", "exit": "", "exit-line": i,
                   # Short, because it is identical on all eight rows and the
                   # note above the table carries the reason once.
                   "because": "Not read.",
                   "evidence": [], "artefacts": []}
            phases.append(cur)
            continue
        if cur is not None and not cur["exit"] and ln.startswith("**Exit criterion:**"):
            body = [ln.split("**Exit criterion:**", 1)[1].strip()]
            for nxt in lines[i:]:
                if not nxt.strip():
                    break
                body.append(nxt.strip())
            cur["exit"] = " ".join(x for x in body if x)
            cur["exit-line"] = i
    return {"phases": [q for q in phases if q["exit"]], "read": False,
            "doc": os.path.relpath(doc, os.path.dirname(REPO)),
            "overall": {"finding": "Not read. This lab has no lifecycle.edn: "
                        "no phase has been judged, and none is reported met. "
                        "The exit criteria below are quoted from the lifecycle "
                        "document so the table says what would have to hold."},
            "mission": {"status-line": status_line_of(text)}}


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


def slugify(s):
    """A heading's slug, with any anchor sentinel inside it removed: a phase
    heading now carries one, and letting it through renamed every phase id."""
    s = re.sub(OPEN + r"[a-zA-Z0-9_-]+" + OPEN, "", s).replace(CLOSE, "")
    return re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")[:48]


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
            slug = slugify(m.group(2))
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
            moved_section = heading_changed(text, a)
            if moved_section:
                # The quote is where it was and the section around it is not.
                # Drawn flagged rather than silently: a note read under the
                # wrong phase is worse than one that says it is unsure.
                n["_stale"] = (f"still at its offsets, but the section changed — "
                               f"recorded under “{moved_section[0]}”, now under "
                               f"“{moved_section[1]}”")
                stale.append(n)
            live.append(n)
        else:
            found = text.find(q)
            n["_stale"] = ("quote found at %d, recorded %d" % (found, s) if found >= 0
                           else "quote no longer present in the mission")
            stale.append(n)
    marks = []
    for n in live:
        s = n["anchor"]["start"]
        if n.get("_heading"):
            # A phase anchor quotes the whole heading line, hashes included.
            # Opening at the hash would stop the line matching as a heading,
            # so open just inside it; the span still covers the phase name.
            m = re.match(r"#{1,6}\s+", text[s:n["anchor"]["end"]])
            if m:
                s += m.end()
        marks.append((s, "open", n["id"]))
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
                "blocked": "blocked", "not-started": "not started"}


def status_label(ph):
    """A phase waiting on another says which, because 'in progress' on a phase
    nobody is working reads as a stall rather than as a dependency."""
    if ph.get("label"):
        return ph["label"]
    if ph["status"] == "blocked" and ph.get("blocked-on"):
        return f'blocked on {ph["blocked-on"].lstrip(":")}'
    return STATUS_LABEL[ph["status"]]


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
            + ('' if ph.get("mission-anchor") or not life.get("read", True) else
               ' <span class="nosec">no section in the mission</span>') +
            f'</td>'
            f'<td class="phstat"><span class="dot d-{st}"></span>'
            f'{status_label(ph)}</td>'
            f'<td class="phexit">“{html.escape(ph["exit"])}”'
            f'<span class="exitsrc">mission-lifecycle.md:{ph["exit-line"]}</span></td>'
            f'<td class="phwhy">{html.escape(ph["because"])}'
            + (f'<ul class="phev">{ev}</ul>' if ev else "")
            + (f'<p class="phart">{arte} artefact{"s" if arte != 1 else ""}, '
               f'each beside the passage it annotates</p>' if arte else "")
            + '</td></tr>')
    o = life["overall"]
    if life.get("read", True):
        note = (f'<p class="tocnote">Status per phase is read from that phase\'s exit criterion in '
                f'<code>futon4/holes/mission-lifecycle.md</code> against the evidence, not from the '
                f'mission\'s Status line. The mission\'s Status line says '
                f'<b>{html.escape(life["mission"]["status-line"])}</b>. '
                f'Nothing here advances it: that is the owner\'s act, and the owner is unassigned.</p>')
    else:
        # No lifecycle.edn in the lab. Every row says so, and none says met:
        # a page that has read no verdict must not report one.
        note = (f'<p class="tocnote"><b>No lifecycle.edn in this lab, so no phase has been '
                f'read.</b> Every row below is unread and the exit criteria are quoted from '
                + (f'<code>{html.escape(life["doc"])}</code>' if life.get("doc")
                   else 'the lifecycle document')
                + f'. The mission\'s own Status line says: '
                f'<b>{html.escape(life["mission"]["status-line"])}</b> Nothing here confirms '
                f'or advances it — reading the phases is the owner\'s act.</p>')
    return (f'<section class="toc" id="toc">'
            f'<h2>Where this mission stands</h2>'
            + note +
            f'<div class="tocwrap"><table class="toctable"><thead><tr><th>phase</th><th>status</th>'
            f'<th>exit criterion</th><th>read from</th></tr></thead>'
            f'<tbody>{"".join(rows)}</tbody></table></div>'
            f'<p class="tocfind"><b>Overall.</b> {html.escape(o["finding"])}</p>'
            f'</section>')


def phase_bar(ph):
    """The phase's standing, set directly under the mission's own heading for
    it. There used to be a second heading per phase at the foot of the page,
    restating what the mission already said, and every artefact was levelled
    with that restatement rather than with the passage it annotates. One
    heading per phase now: the mission's."""
    st = ph["status"]
    return (f'<p class="phasebar b-{st}">'
            f'<span class="dot d-{st}"></span>'
            f'<span class="phstatword">{status_label(ph)}</span>'
            f'<span class="phexit3">exit: “{html.escape(ph["exit"])}”</span>'
            f'<a class="phwhere" href="#toc">how this was read</a></p>')


def unwritten_phase(ph):
    """A phase the mission has not written has no heading of its own to sit
    under, so it gets a placeholder -- drawn as one, because it is not
    mission text and must not read as any."""
    st = ph["status"]
    return (f'<section class="phase placeholder">'
            f'<h2 id="anc-phase-{ph["id"]}" class="phhead" data-note="phase-{ph["id"]}">'
            f'<span class="phn">{ph["n"]}</span> {html.escape(ph["title"])}'
            f'<span class="dot d-{st}"></span>'
            f'<span class="phstatword">{status_label(ph)}</span></h2>'
            f'<p class="phbody"><b>Not yet written in the mission.</b> '
            f'{html.escape(ph["because"])}</p></section>')


def figure_width(svg):
    """The width at which this drawing's labels come out at body size."""
    m = re.search(r'viewBox="0 0 ([0-9.]+) ([0-9.]+)"', svg)
    return round(float(m.group(1)) * BODY_PX / LABEL_PX) if m else None


def figure_html(key, svg, cap, num, anchor_id, at_id=None, full_width=False):
    """A numbered figure sized so its labels match the body text."""
    marks = "".join(f'<li>{html.escape(m)}</li>' for m in cap.get("marks", []))
    w = figure_width(svg)
    # A full-width figure is not lifted into the margin: it sits below its
    # section across the whole page, because at margin width its edge labels
    # cannot be placed without overprinting something.
    # Both kinds are sized from the label target; a full-width figure simply
    # has more room to be that size in.
    style = f' style="width:{w}px"' if w else ""
    cls = "pagefig" if full_width else "marginfig"
    return (f'<figure class="{cls}" id="fig-{num}" data-anchor="{html.escape(anchor_id)}" '
            f'data-fig="{html.escape(key)}" data-natural-width="{w}">'
            f'<div class="figbody"{style} title="click to enlarge">{svg}'
            f'<span class="figopen">enlarge ⤢</span></div>'
            f'<figcaption><span class="fignum">Figure {num}</span> '
            f'{html.escape(cap.get("what", key))} — instance {cap.get("instance", "?")}. '
            f'{html.escape(cap.get("sub", ""))}'
            + (f' <span class="figwhy">— {html.escape(cap["why"])}.</span>'
               if cap.get("why") else "")
            + (f'<p class="figback">annotates '
               f'<a href="#anc-{html.escape(at_id or anchor_id)}">'
               f'\u201c{html.escape(cap["at"])}\u201d</a></p>' if cap.get("at") else "")
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
        # The direction word is set by the script from the laid-out positions,
        # because the figures stack in a lane of their own and a direction
        # asserted from the source order is a claim the layout can falsify --
        # which it did, the last time this page said "above" of a figure below.
        bits.append(f'<p class="nseefig" data-fig="{ref}">See '
                    f'<a href="#fig-{ref}">Figure {ref}</a> '
                    f'<span class="figdir">{n.get("_figdir", "below")}</span>.</p>')
    if n.get("refs"):
        bits.append('<p class="nrefs">' +
                    " · ".join(f"<code>{html.escape(r)}</code>" for r in n["refs"]) + "</p>")
    if n.get("_why"):
        bits.insert(1, f'<p class="nwhy">{html.escape(n["_why"])}</p>')
    if n.get("_at-phase"):
        # The note is set beside the passage it annotates, so the phase is no
        # longer its position -- it is where the artefact was made, which the
        # lifecycle records and the reader still needs.
        bits.append(f'<p class="natphase">made in '
                    f'<a href="#anc-phase-{html.escape(n["_at-phase"])}">'
                    f'{html.escape(n["_at-phase"])}</a></p>')
    return (f'<aside class="{cls}" id="note-{html.escape(n["id"])}" '
            f'data-anchor="{html.escape(anchor_override or n["id"])}">'
            + "".join(bits) + "</aside>")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out")
    ap.add_argument("--rev", default="HEAD")
    ap.add_argument("--worktree", action="store_true",
                    help="render the working copy instead of a committed revision")
    ap.add_argument("--mission", default=DEFAULT_MISSION,
                    help="repo-relative path to the mission (default M-futon-seams)")
    ap.add_argument("--lab", default=DEFAULT_LAB,
                    help="repo-relative lab dir holding annotations.edn and "
                         "lifecycle.edn (default holes/labs/M-futon-seams)")
    a = ap.parse_args()

    global MISSION
    MISSION = os.path.relpath(os.path.abspath(a.mission), REPO) \
        if os.path.isabs(a.mission) else a.mission
    use_lab(a.lab)
    default_lab = (MISSION == DEFAULT_MISSION and a.lab == DEFAULT_LAB)

    text, revlabel = read_mission(a.rev, a.worktree)
    name = mission_name(text, MISSION)
    sha = hashlib.sha256(text.encode()).hexdigest()

    # Either file may be absent. An absent file is stated on the page; it is
    # never filled in with a default, and it never becomes a crash.
    ann_path = os.path.join(REPO, ANNOTATIONS)
    life_path = os.path.join(REPO, LIFECYCLE)
    notes = edn_to_json(ANNOTATIONS) if os.path.isfile(ann_path) else []
    # The absence is written INTO the margin's head rather than after it. The
    # head is a two-column colour key, which keys nothing when there are no
    # annotations -- but the layout script sizes it to the lanes and would
    # throw on a null if it were dropped, so it stays and carries the absence
    # instead. Found by running the page, not by reading it.
    absent = ""
    if not os.path.isfile(ann_path):
        absent = (f'<span class="nosec" style="grid-column:1/-1;font-variant:none;'
                  f'letter-spacing:0">No <code>{html.escape(ANNOTATIONS)}</code>, so this '
                  f'margin is empty. The mission renders; nothing is annotated. '
                  f'Annotations are the mission owner\'s to write.</span>')
    notes.sort(key=lambda n: (n["anchor"]["start"], n["id"]))
    life = (edn_to_json(LIFECYCLE) if os.path.isfile(life_path)
            else generic_lifecycle(text))
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

    # Each phase heading in the mission becomes an anchor of its own, so the
    # table of contents lands on the phase as the mission writes it. A phase
    # the mission has not written has no heading to anchor to and is drawn as
    # a placeholder at the end.
    phase_anchor, unwritten = [], []
    for ph in life["phases"]:
        anc = ph.get("mission-anchor")
        (phase_anchor if anc else unwritten).append(ph)
    pseudo = [{"id": f'phase-{ph["id"]}', "anchor": ph["mission-anchor"],
               "_heading": True} for ph in phase_anchor]

    marked, live, stale = place_anchors(text, notes + pseudo)
    blocks = [resolve_sentinels(b) for b in render_markdown(marked)]

    # Which block each anchor landed in. An annotation is levelled with the
    # passage it is anchored to -- that is what an anchored note is for. It
    # used to be levelled with its phase instead, and because every phase was
    # restated below the mission text, every annotation ended up at the foot
    # of the page with nothing beside it.
    block_of = {}
    for i, b in enumerate(blocks):
        for aid in re.findall(r'id="anc-([a-zA-Z0-9_-]+)"', b):
            block_of.setdefault(aid, i)

    # A figure sits with the note that declares it; one no note declares
    # falls back to its phase's heading.
    fig_note = {n["figure"]: n["id"] for n in notes if n.get("figure") in figures}
    fig_why, note_why = {}, {}
    for ph in life["phases"]:
        for art in ph.get("artefacts", []):
            if art.get("figure"):
                fig_why[art["figure"]] = art.get("why", "")
            if art.get("note"):
                note_why[art["note"]] = art.get("why", "")

    # A figure is levelled with the mission's heading for the phase that made
    # it, and carries a link back to the passage it annotates. Levelling it
    # with that passage instead was tried and does not fit: ten figures need
    # about 13900px and the seven instances they annotate are 8500px of text,
    # so the last of them ended up 6700px past its anchor while the margin
    # beside every phase section stood empty.
    LAST = len(blocks)          # anything whose anchor no longer holds
    plan, seq = [], 0
    for key in figures:
        aid = f'phase-{fig_phase.get(key, "")}'
        if aid not in block_of:
            aid = fig_note.get(key, aid)
        plan.append((block_of.get(aid, LAST), 0, seq, "fig", key, aid)); seq += 1
    for n in notes:
        aid = (n["id"] if n["id"] in block_of
               else f'phase-{note_phase.get(n["id"], "")}')
        plan.append((block_of.get(aid, LAST), 1, seq, "note", n["id"], aid)); seq += 1
    plan.sort()

    # Figures are numbered in the order they are now read in, and a note
    # knows whether its figure is above it or below it from the same order.
    fignums = {key: i + 1 for i, (_, _, _, kind, key, _)
               in enumerate([q for q in plan if q[3] == "fig"])}
    where = {(kind, key): i for i, (_, _, _, kind, key, _) in enumerate(plan)}
    for n in notes:
        fig = n.get("figure")
        if fig in fignums:
            n["_figref"] = fignums[fig]
            n["_figdir"] = ("above" if where[("fig", fig)] < where[("note", n["id"])]
                            else "below")

    by_block, by_id = {}, {n["id"]: n for n in notes}
    for bi, _, _, kind, key, aid in plan:
        by_block.setdefault(bi, []).append((kind, key, aid))
    bar_at = {block_of[f'phase-{ph["id"]}']: ph for ph in phase_anchor
              if f'phase-{ph["id"]}' in block_of}

    # Ten figures totalling some 13900px are anchored into about 8500px of
    # mission text, so a figure cannot be levelled with its passage however
    # the lanes are cut. It carries the passage instead, as a back-link.
    quote_of = {n["id"]: n["anchor"]["quote"] for n in notes}
    quote_of.update({f'phase-{ph["id"]}': ph["mission-anchor"]["quote"]
                     for ph in phase_anchor})

    def artefacts(bi):
        out = []
        for kind, key, aid in by_block.get(bi, []):
            if kind == "fig":
                cap = dict(captions.get(key, {}))
                cap["phase"] = fig_phase.get(key)
                cap["why"] = fig_why.get(key, "")
                at = fig_note.get(key, aid)
                q = quote_of.get(at, "").lstrip("#").strip()
                cap["at"] = q if len(q) <= 64 else q[:63].rsplit(" ", 1)[0] + "\u2026"
                out.append(figure_html(key, figures[key], cap, fignums[key], aid,
                                       at_id=fig_note.get(key)))
            else:
                n = dict(by_id[key])
                n["_at-phase"] = note_phase.get(key)
                n["_why"] = note_why.get(key, "")
                out.append(note_html(n, anchor_override=aid))
        return out

    body_parts = []
    for i, b in enumerate(blocks):
        body_parts.append(b)
        if i in bar_at:
            body_parts.append(phase_bar(bar_at[i]))
        body_parts.extend(artefacts(i))
    # A placeholder section per phase is a statement that the mission has not
    # written that phase. With no lifecycle.edn nothing has been read, so that
    # is not something this page knows; the table carries the absence instead.
    if life.get("read", True):
        for ph in unwritten:
            body_parts.append(unwritten_phase(ph))
    if by_block.get(LAST):
        body_parts.append('<h2 class="orphanhead">Annotations with no live anchor</h2>'
                          '<p class="orphannote">The span each of these was written '
                          'against is no longer where it was recorded, so there is '
                          'nothing in the mission to set them beside.</p>')
        body_parts.extend(artefacts(LAST))
    body = "\n".join(body_parts)

    cols = {"a": [n for n in notes if n.get("column") == "pattern"],
            "b": [n for n in notes if n.get("column") == "proof2a"]}

    # Prose and data drifting apart is this mission's own defect, and it
    # happened here: DERIVE read :exit-met in lifecycle.edn while its section
    # still said otherwise, and the page rendered the contradiction until an
    # outside reader found it. Refuse rather than render it again.
    # verdict_check.py reads M-futon-seams' mission and lifecycle by fixed
    # path. Run against another mission it would check the wrong pair and
    # report a pass about a file nobody asked about, which is worse than not
    # running it -- so it guards the mission it is actually about.
    if default_lab:
        vc = subprocess.run([sys.executable,
                             os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                          "verdict_check.py")],
                            capture_output=True, text=True)
        if vc.returncode:
            sys.exit("seams_mission_page: " + vc.stdout.strip())

    counts = (f'{len(notes)} notes — {len(cols["a"])} pattern, {len(cols["b"])} PROOF-2a; '
              f'{sum(1 for n in notes if n.get("status") == "reviewed")} reviewed, '
              f'{len(stale)} with a stale anchor; '
              f'{sum(1 for p in life["phases"] if p["status"] == "exit-met")} of '
              f'{len(life["phases"])} phase exits met')

    mhead = (MHEAD if not absent
             else '<p class="mhead">' + absent + '</p>')
    f = furniture(name)
    open(a.out, "w", encoding="utf-8").write(PAGE.format(
        css=CSS, js=JS, body=body, toc=toc_html(life), rev=html.escape(revlabel),
        sha=sha[:16], counts=html.escape(counts),
        mission=html.escape(MISSION), annpath=html.escape(ANNOTATIONS),
        title=html.escape(f["title"]), h1=html.escape(f["h1"]),
        sub=f["sub"], kernels=f["kernels"], mhead=mhead))
    print(a.out)
    print(f"  rev {revlabel} sha {sha[:16]} · {counts}")
    for n in stale:
        print(f"  STALE {n['id']}: {n['_stale']}")


CSS = """
/* Mission left, a WIDE margin right: the margin is a working second column,
   not a gutter. The text keeps a reading measure and the margin takes all the
   rest, which is the arrangement the mark7 typeset previews use. */
/* The measure flexes between 26rem and 33rem. It was fixed at 33rem, which
   is a good measure at 1920 and leaves a 1600 screen only 224px of margin to
   annotate in -- the notes went narrow and therefore tall, and stopped
   levelling with what they annotate. 26rem is still about 55 characters. */
:root { --measure: clamp(26rem, 27vw, 33rem); --gutter: 2.4rem; --pad: 3vw;
        --margin-w: max(44rem, calc(100vw - 2*var(--pad) - var(--measure) - var(--gutter))); }
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
.k-a { color:#1b6b3a; } .k-b { color:#2a4d8f; }
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
.note.inmargin { position:absolute; margin:0; }

/* A figure gets the WHOLE margin, not half of it, and is legible there
   without enlarging: the cascades are laid out depth-downward for exactly
   this reason. Clicking still opens it larger for detail. */
.marginfig { margin:1.2rem 0 1.4rem; padding:0; }
/* Full-width figures: below the section, across the page. */
.pagefig { margin:1.4rem 0 2rem; padding:0; max-width:none; }
.pagefig .figbody { width:auto; max-width:100%; }
.pagefig figcaption { font-size:.72rem; line-height:1.5; color:#666;
                      padding:.4rem .1rem 0; max-width:60rem; }
.marginfig.inmargin { position:absolute; margin:0; }
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
.figback { margin:.25rem 0 0; font-size:.66rem; color:#888; }
.figback a { color:#b8431f; }
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
/* Secondary text in a figure. The node title (.nid/.wid) is held at body
   size; these carry annotations and are set smaller deliberately, with the
   measured ratio reported per class by check_seams_layout.js rather than
   left unstated. */
.wtok { font-size:10px; fill:#556; font-family:ui-monospace,Menlo,monospace; }
.winh { fill:#b8431f; }
.wtokloose { fill:#a8791d; }
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
.d-blocked { background:#7a6ca8; }
.d-not-started { background:#ccc; }

/* The phase's standing, under the mission's own heading for that phase.
   It is a rule across the measure rather than a box: the mission text is
   what the reader is reading, and this is a caption on it. */
.phasebar { margin:-.2rem 0 1rem; padding:.3rem 0 .35rem .8rem; font-size:.7rem;
            line-height:1.45; color:#777; border-left:3px solid #e6e2d4;
            background:#fbfaf3; }
.phasebar.b-exit-met { border-left-color:#1b6b3a; }
.phasebar.b-in-progress { border-left-color:#a8791d; }
.phasebar.b-blocked { border-left-color:#7a6ca8; }
.phexit3 { display:block; font-style:italic; color:#666; margin-top:.15rem; }
.phwhere { color:#aaa; font-size:.64rem; }
/* A phase the mission has not written: dashed and tinted, so it cannot be
   read as mission text that happens to be short. */
.phase { margin:2.2rem 0 1rem; }
.phase.placeholder { border-left:3px dashed #c9c4b0; padding:.5rem 0 .4rem .9rem;
                     background:#fbfaf3; }
.phhead { font-size:1.05rem; margin:0 0 .3rem; border:0; padding:0; }
.phn { display:inline-block; min-width:1.4rem; color:#aaa;
       font-family:ui-monospace,Menlo,monospace; font-size:.8rem; }
.phstatword { font-size:.68rem; color:#888; font-variant:small-caps;
              letter-spacing:.05em; }
.phbody { font-size:.82rem; color:#444; margin:0; }
.orphanhead { color:#b8431f; }
.orphannote { font-size:.78rem; color:#777; }
.nwhy { margin:0 0 .3rem; font-size:.7rem; color:#1b6b3a; }
.natphase { margin:.3rem 0 0; font-size:.63rem; color:#999;
            font-variant:small-caps; letter-spacing:.05em; }
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
@media (max-width:89.99rem) {
  .page { grid-template-columns:1fr; }
  .margin { display:none; }
  .note { margin-left:.8rem; }
}
"""

JS = """
// Three lanes in the margin, not two. The pattern working and the PROOF-2a
// reading each get a note lane and are levelled with the passage they are
// anchored to; the figures get a lane of their own at the outer edge.
//
// They used to share: a figure took the whole margin, so every figure pushed
// both note columns down by its full height. Ten figures totalling 13805px
// are anchored into about 4300px of mission text -- the seven instances --
// so the notes ended up as much as 12000px below the passages they annotate,
// which is the pile-up at the foot of the page. Separate lanes decouple
// them: the notes align, and the figures, which are numbered and referred to
// by number, stack.
//
// With the script off, or on a narrow screen, every one of them stays in the
// flow under the passage it annotates -- which is why they are emitted there.
const GAP = 24;          // between lanes
const MIN_NOTE = 220;    // a note lane narrower than this is not worth having
const MAX_NOTE = 640;    // and one wider than this stops reading as a note
const MIN_TWO  = 420;    // two note columns are worth having only above this

function layout() {
  const margin = document.querySelector('.margin');
  const main = document.querySelector('.main');
  const head = document.querySelector('.mhead');
  const all = [...document.querySelectorAll('.note, .marginfig')];  // document order
  if (!window.matchMedia('(min-width: 90rem)').matches) {
    all.forEach(n => {
      n.classList.remove('inmargin');
      const anc = document.getElementById('anc-' + n.dataset.anchor);
      const host = anc ? anc.closest('.main > *') : null;
      if (host && host.nextSibling !== n) host.after(n);
      n.style.top = n.style.left = n.style.width = '';
    });
    figDirections();
    return;
  }
  all.forEach(n => {
    if (n.parentElement !== margin) margin.appendChild(n);
    n.classList.add('inmargin');
  });
  const MW = margin.clientWidth;
  const mtop = margin.getBoundingClientRect().top + window.scrollY;
  const top = n => {
    const a = document.getElementById('anc-' + n.dataset.anchor);
    return a ? a.getBoundingClientRect().top + window.scrollY - mtop : null;
  };

  // Figures first, in a lane of their own at the outer edge, each at the
  // width its labels come out at body size. They are levelled with the
  // mission's heading for the phase that made them, so they occupy one band
  // of the page -- DERIVE and ARGUE -- and not the whole of it.
  const figs = all.filter(n => n.classList.contains('marginfig'));
  const natural = figs.reduce((m, f) => Math.max(m, +f.dataset.naturalWidth || 0), 0);
  const figLane = figs.length ? Math.min(natural, Math.max(MW - MIN_NOTE - GAP, 0)) : 0;
  let floor = 34, band = [Infinity, -Infinity];
  figs.forEach(f => {
    f.style.left = (MW - figLane) + 'px';
    f.style.width = figLane + 'px';
    const y = Math.max(top(f) ?? floor, floor + 12);
    f.style.top = y + 'px';
    floor = y + f.offsetHeight;
    band = [Math.min(band[0], y), Math.max(band[1], floor)];
  });
  const figBottom = floor;

  // Then the notes, levelled with the passage each is anchored to. Two
  // columns where there is room -- the pattern working and the PROOF-2a
  // reading -- and one where a figure is already using the outer edge.
  // Reserving the figure lane down the whole page was the wrong trade: the
  // figures occupy two sections of eight, and narrowing every note for them
  // made every note taller, which puts it further from what it annotates.
  const two = MW >= 2 * MIN_TWO + GAP;
  const colW = Math.min(MAX_NOTE, two ? (MW - GAP) / 2 : MW);
  const bandW = Math.min(MAX_NOTE, Math.max(MIN_NOTE, MW - figLane - GAP));
  head.style.width = (two ? colW * 2 + GAP : colW) + 'px';
  head.classList.toggle('onelane', !two);
  let fa = 34, fb = 34;
  all.filter(n => !n.classList.contains('marginfig')).forEach(n => {
    const b = two && n.classList.contains('col-b');
    const x = b ? colW + GAP : 0;
    let floor = b ? fb : fa;
    let y = Math.max(top(n) ?? floor, floor + 12);
    n.style.left = x + 'px';
    n.style.width = colW + 'px';
    let h = n.offsetHeight;
    if (x + colW > MW - figLane && y < band[1] && y + h > band[0]) {
      // This one would run into the figure lane. One narrow column instead,
      // sharing a floor, so the two columns cannot print over each other.
      floor = Math.max(fa, fb);
      y = Math.max(top(n) ?? floor, floor + 12);
      n.style.left = '0px';
      n.style.width = bandW + 'px';
      n.style.top = y + 'px';
      fa = fb = y + n.offsetHeight;
      return;
    }
    n.style.top = y + 'px';
    if (b) fb = y + h; else fa = y + h;
  });
  margin.style.minHeight = Math.max(fa, fb, figBottom, main.offsetHeight) + 'px';
  figDirections();
}

// "See Figure 7 above" is true or false depending on where the figure ended
// up, so it is read off the page rather than guessed from the source order.
function figDirections() {
  document.querySelectorAll('.nseefig').forEach(p => {
    const note = p.closest('.note'), fig = document.getElementById('fig-' + p.dataset.fig);
    const word = p.querySelector('.figdir');
    if (!note || !fig || !word) return;
    word.textContent = fig.getBoundingClientRect().top < note.getBoundingClientRect().top
                     ? 'above' : 'below';
  });
}
document.addEventListener('click', e => {
  const open = e.target.closest('.figbody');
  if (open) {
    const m = document.createElement('div');
    m.className = 'figmodal';
    const host = open.closest('figure');
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
# The masthead is furniture ABOUT a mission, not a template for missions in
# general: "worked against PROOF-2a" is true of M-futon-seams, the two-column
# legend describes annotations that only M-futon-seams has, and the kernels
# link points at a page built from its lattices. Kept with the mission it
# describes, so another mission gets a masthead about itself rather than
# inheriting claims that are not true of it.
FURNITURE = {
    "M-futon-seams": {
        "title": "M-futon-seams — worked against PROOF-2a",
        "h1": "M-futon-seams, worked",
        "sub": '<p class="sub">The mission is the main text and grows as it is worked. Each note is set beside\nthe passage it is anchored to, and is one of two readings of that passage: <b class="k-a">the\npattern working</b> — cascade nodes, work-state tokens, context→pattern edges — or <b\nclass="k-b">the PROOF-2a reading</b>, which clause or data shape the passage exercises and\nwhether this example fits the shape, breaks it, or needs a field that does not exist. Breaks\nare the evidence worth most. The figures sit in a lane of their own, beside the phase that\nmade them, each linking back to the passage it draws. Click a marked span or a note to pair\nthem.</p>',
        "kernels": '<br>\nkernel tables, conflict states and the full-size lattices:\n<a href="seams-kernels.html">seams-kernels.html</a>',
    },
}


def furniture(name):
    """The masthead for a mission, or a plain one that claims nothing."""
    if name in FURNITURE:
        return FURNITURE[name]
    return {
        "title": f"{name} — worked",
        "h1": f"{name}, worked",
        "sub": '<p class="sub">The mission is the main text and grows as it '
               'is worked. Annotations, where there are any, are set in the '
               'margin beside the passage each is anchored to.</p>',
        "kernels": "",
    }

MHEAD = ('<p class="mhead"><span class="mh-a">green: the pattern working</span>'
         '<span class="mh-b">blue: the PROOF-2a reading</span></p>')

PAGE = """<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
<style>{css}</style></head><body>
<div class="masthead">
<h1>{h1}</h1>
{sub}
<p class="prov">{mission} at {rev} · sha256 {sha}… · {counts}{kernels}</p>
</div>
{toc}
<div class="page">
  <article class="main">{body}</article>
  <div class="margin" aria-hidden="false">
    {mhead}
  </div>
</div>
<footer>Generated by <code>scripts/seams_mission_page.py</code> from
<code>{mission}</code> and <code>{annpath}</code>.
Re-runnable and byte-deterministic for a fixed revision and annotation set.
Anchors are checked against the revision rendered; a note whose quote has moved is
drawn flagged in place rather than relocated.</footer>
<script>{js}</script>
</body></html>
"""

if __name__ == "__main__":
    main()
