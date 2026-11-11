#!/usr/bin/env python3
"""Writes the zone blog post "Promises kept: McCarthy's Elephant 2000 drawn
as cascades and wants" (Joe, 2026-11-11).

Figure 1 is drawn from the recorded lab cascade (../cascade-2026-11-11-turn);
figures 2 and 3 draw McCarthy's own examples, quoted from his paper
(~/code/storage/references/mccarthy-elephant-2000/elephant.txt).

  make_post.py [--out FILE]
"""
import argparse
import html
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))
LAB = os.path.join(HERE, "..", "cascade-2026-11-11-turn")
OUT = "/var/www/zone.hyperreal.enterprises/2026-11-11-promises-kept-elephant-2000.html"

INK, DIM, GLUE, OK, NO, OPEN, PAT = "#1a1a1a", "#6a6a6a", "#c58a12", "#2f7d32", "#b23b3b", "#2a5a8a", "#2a6fdb"


def esc(s):
    return html.escape(str(s), quote=True)


def svg(w, h, body):
    return ('<svg viewBox="0 0 %d %d" width="%d" role="img" xmlns="http://www.w3.org/2000/svg" '
            'style="max-width:100%%;height:auto;font-family:inherit">'
            '<defs><marker id="ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" '
            'markerHeight="6" orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="%s"/></marker></defs>'
            '%s</svg>' % (w, h, w, INK, body))


def text(x, y, s, size=11, fill=INK, anchor="middle", weight="normal"):
    return '<text x="%d" y="%d" font-size="%d" fill="%s" text-anchor="%s" font-weight="%s">%s</text>' % (
        x, y, size, fill, anchor, weight, esc(s))


def pattern(x, y, label):
    """A pattern occurrence: a filled circle, as in Mojira's minimal view."""
    return '<circle cx="%d" cy="%d" r="11" fill="%s"/>' % (x, y, PAT) + text(x, y + 28, label, 11, INK, "middle", "bold")


def act(x, y, label, sub=None):
    """A speech act (McCarthy's examples have acts, not patterns): a hollow circle."""
    out = '<circle cx="%d" cy="%d" r="8" fill="#fff" stroke="%s" stroke-width="1.5"/>' % (x, y, INK)
    out += text(x + 16, y + 4, label, 11, INK, "start")
    if sub:
        out += text(x + 16, y + 18, sub, 10, DIM, "start")
    return out


def want(x, y, label, cls, state):
    """A want: a diamond; its locator class beside it; the observation inside."""
    col = {"observed": OK, "not-observed": NO, "open": OPEN}[state]
    mark = {"observed": "✓", "not-observed": "✗", "open": "…"}[state]
    d = '<path d="M%d,%d l12,12 l-12,12 l-12,-12 z" fill="#fff" stroke="%s" stroke-width="1.6"/>' % (x, y - 12, col)
    return d + text(x, y + 4, mark, 12, col, "middle", "bold") + text(x + 20, y - 2, label, 11, INK, "start") + \
        text(x + 20, y + 12, "%s · %s" % (cls, state), 10, col, "start")


def arrow(x1, y1, x2, y2, col=INK, dash=False):
    return '<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1.2" %s marker-end="url(#ah)"/>' % (
        x1, y1, x2, y2, col, 'stroke-dasharray="4 3"' if dash else "")


def figure1():
    """The 11-11 turn: two patterns glued, and the two THEN wants."""
    occ = json.load(open(os.path.join(LAB, "occurrences.json"), encoding="utf-8"))
    complete = [o for o in occ if all((o["maps"].get(r) or {}).get("element") for r in ("I1", "I2", "G", "B"))]
    a, b = complete
    ea = {r: a["maps"][r]["element"] for r in ("I1", "I2", "G", "B")}
    eb = {r: b["maps"][r]["element"] for r in ("I1", "I2", "G", "B")}
    glue = set(ea.values()) & set(eb.values())
    assert glue == {"voxterm-diagram-tab-start-incomplete"}, glue
    g = glue.pop()
    body = pattern(150, 60, a["pattern"].split("/")[1]) + pattern(470, 60, b["pattern"].split("/")[1])
    body += '<path d="M161,60 Q310,0 459,60" fill="none" stroke="%s" stroke-width="2"/>' % GLUE
    body += '<circle cx="310" cy="30" r="4" fill="%s"/>' % GLUE + text(310, 18, g, 10, GLUE)
    body += text(178, 48, "BECAUSE", 9, DIM) + text(438, 48, "HOWEVER", 9, DIM)
    body += arrow(150, 100, 150, 150) + text(160, 128, "THEN", 9, DIM, "start")
    body += arrow(470, 100, 470, 150) + text(480, 128, "THEN", 9, DIM, "start")
    body += want(150, 170, ea["B"], "T1", "observed")
    body += want(470, 170, eb["B"], "T1", "open")
    return svg(800, 210, body), (a["pattern"], b["pattern"], g, ea["B"], eb["B"])


def figure2():
    """McCarthy's George's telephone number: truthful but not responsive."""
    body = act(40, 30, "question: George's telephone number?")
    body += arrow(48, 40, 88, 100, OPEN, True) + text(76, 66, "creates", 9, DIM, "start")
    body += want(100, 115, "want: a responsive answer, i.e. a sequence of digits",
                 "locator: the shape of the answer", "not-observed")
    body += act(40, 200, "answer: the same as George's wife's husband's number",
                "truthful: yes · addressed to the question: yes · a sequence of digits: no")
    body += arrow(48, 190, 88, 130, INK) + text(76, 168, "offered", 9, DIM, "start")
    return svg(720, 235, body)


def figure3():
    """McCarthy's airline reservation program, drawn vertically (time runs down)."""
    rows = [
        ("request: reserve psgr on flt", "¬full flt → accept.request make commitment admit(psgr, flt)"),
        ("query: does psgr have a reservation?", "answer.query exists commitment admit(psgr, flt)"),
        ("now = time flt", "accept.request admit(psgr, flt)"),
    ]
    body = '<line x1="40" y1="20" x2="40" y2="300" stroke="%s" stroke-width="1"/>' % DIM
    y = 40
    for label, sub in rows:
        body += act(40, y, label, sub)
        y += 90
    # the commitment is a want whose horizon is the flight's time
    body += '<line x1="520" y1="40" x2="520" y2="220" stroke="%s" stroke-width="2" stroke-dasharray="5 4"/>' % OPEN
    body += want(520, 40, "admit(psgr, flt)", "commitment, horizon: time flt", "open")
    body += want(520, 220, "admit(psgr, flt)", "observed at time flt", "observed")
    body += arrow(400, 40, 505, 40, OPEN, True) + text(452, 32, "makes", 9, DIM)
    body += arrow(400, 220, 505, 220, OK) + text(452, 212, "fulfils", 9, DIM)
    body += text(40, 330, "cancel: accept.request cancel commitment admit(psgr, flt) withdraws the want before its horizon.", 11, DIM, "start")
    body += text(40, 348, "full flt ≡ card{psgr | exists commitment admit(psgr, flt)} = capacity flt: a refusal, not a 'no'.", 11, DIM, "start")
    return svg(760, 365, body)


def page():
    f1, (pa, pb, g, wa, wb) = figure1()
    return """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<meta name="robots" content="noindex">
<meta name="description" content="McCarthy's Elephant 2000 asks that answers be truthful and responsive and that promises be kept. Drawn in the cascade language we use for operator turns: a pattern's THEN aims at a want, and an observation says whether it now holds.">
<title>Promises kept: McCarthy's Elephant 2000, drawn as cascades and wants · Zone</title>
<link rel="stylesheet" href="style.css">
<style>
  figure {{ margin:1.2rem 0 1.8rem; }}
  figure .scroll {{ overflow-x:auto; }}
  figcaption {{ color:var(--dim); font-size:.85rem; margin-top:.3rem; }}
  .draft {{ border-left:3px solid #8a3a2a; padding:.4rem 1rem; background:#fff8e5; }}
  blockquote {{ border-left:3px solid #ccc; margin:.8rem 0; padding:.2rem 1rem; color:#333; }}
  body {{ max-width:52rem; }}
</style>
</head>
<body>
<header><a href="/">zone.hyperreal.enterprises</a><div class="sub">decisions, with the evidence attached</div></header>
<main><article>
<h1>Promises kept: McCarthy's Elephant 2000, drawn as cascades and wants</h1>
<p class="meta">11 November 2026 · Draft by Claude-13 for Joe, from work with Kimi-2 and Claude-CS</p>
<div class="draft"><p><strong>Status.</strong> A draft for Joe. Figure 1 is drawn from a real record: one of Joe's turns from today, read as a pattern cascade (futon3c <code>holes/labs/M-repl-circus/cascade-2026-11-11-turn</code>). Figures 2 and 3 draw McCarthy's own examples, quoted from his paper. The schema behind the drawings is a draft too (futon4 <code>E-gcc-schema.md</code> §16).</p></div>

<h2>What Elephant 2000 asks of a program</h2>
<p>John McCarthy's <em>Elephant 2000: a programming language based on speech acts</em> proposes programs whose inputs and outputs are speech acts: questions, answers, requests, permissions and promises. Their correctness is stated in those terms:</p>
<blockquote>Answers should be truthful and responsive, and promises should be kept.</blockquote>
<p>Our 象 (elephant) reads each operator turn as speech acts, and is named after McCarthy's language. This post puts his examples beside ours, in the picture language we are building for turns.</p>

<h2>The picture language</h2>
<p>A <strong>filled circle</strong> is a pattern that occurs in the situation: all four of its parts (IF, HOWEVER, BECAUSE, THEN) map to something the situation contains. An <strong>amber arc</strong> joins two patterns that hold the same element; that shared element is the glue, and patterns joined this way form one cascade. A <strong>diamond</strong> is a want: something asked for, with a check that can observe it (its locator class) and sometimes a horizon. Inside the diamond, the observation: ✓ observed, ✗ not observed, … still open. A <strong>hollow circle</strong> is a speech act, used where McCarthy's examples have acts rather than patterns.</p>

<h2>1. A turn from today</h2>
<figure><div class="scroll">{f1}</div>
<figcaption>Joe's turn of 11 November, 16:15 UTC, as read by Kimi-2: {pa} and {pb} glued on <code>{g}</code>. Each pattern's THEN aims at a want the turn states.</figcaption></figure>
<p>The turn asked for a graphical stepper for cascades. Two patterns occur in it completely, and they share one element: Joe's remark that the Voxterm "Diagram" tab was a start that was never finished. That element is the BECAUSE of one pattern (read what exists before building) and the HOWEVER of the other (the forward model is a pattern cascade). Each THEN aims at something the turn wants. <code>{wa}</code> has been observed: the Moran essay and figure were read and used for the drawing. <code>{wb}</code> is still open: only one turn has been read so far, so there is no session to draw.</p>
<p>The locator class here is <strong>T1</strong>, conversation text: a later act quotes the want's words and answers them. Our 馬 (horse) already checks this for every reply. A reply paragraph opens with a mark and quotes, in brackets, the words of Joe's that it answers. That is a responsiveness check of the weakest kind: it shows the answer is addressed to the question, not that it answers it. McCarthy's next example shows the difference.</p>

<h2>2. Truthful is not enough: George's telephone number</h2>
<figure><div class="scroll">{f2}</div>
<figcaption>McCarthy's example. The answer is true and addresses the question, and it fails the want.</figcaption></figure>
<blockquote>Suppose someone asks for George's telephone number. It is unresponsive to say that George's telephone number is the same as George's wife's husband's telephone number. It is necessary that the specification language be able to express a requirement that a responsive answer be a sequence of digits and not just an expression whose value is a sequence of digits.</blockquote>
<p>In our terms, the question creates a want whose locator checks the <em>shape</em> of the answer. A quote match would pass this answer: it is addressed to George's number. So the T1 check that 馬 runs today is not McCarthy's responsiveness; a want needs its own locator, saying what an answer has to look like.</p>

<h2>3. A promise is a want with a horizon: the reservation program</h2>
<figure><div class="scroll">{f3}</div>
<figcaption>McCarthy's airline reservation program, drawn with time running down the page. The commitment is a want that stays open until the flight's time.</figcaption></figure>
<p>McCarthy's program is four lines. A request makes a commitment, a query is answered from the commitments that exist, a cancellation removes one, and at the flight's time a commitment is honoured:</p>
<blockquote><code>if now = time flt ∧ exists commitment admit(psgr, flt) then accept.request admit(psgr, flt).</code></blockquote>
<p>Drawn as a want, the commitment has a horizon, the flight's time, and it is observed when the passenger is admitted. A cancellation withdraws it before the horizon; our reply marks have a withdraw act (🈡) for exactly this. A full flight refuses the request, and a refusal is a different answer from "no": we keep that distinction too ("absence is typed"). McCarthy also separates two readings of <code>admit</code>: telling the gate agent to let the passenger on (the program's own act) and the passenger actually boarding (a fact in the world). In our terms those are two locators for one want, one that reads the program's output and one that reads the world.</p>
<p>The vertical drawing is a first step towards the session view Joe asked for: each turn's cascade a row, with lines running down the page from a want to where it is observed.</p>

<h2>What this asks of us</h2>
<ul>
<li>A want record with its locator class, so "responsive" can mean more than "quotes the question".</li>
<li>Horizons on the wants a turn makes, so a promise that has not been kept shows up as open, and one past its horizon as unresolved.</li>
<li>One count, before and after a piece of work: how many of a situation's wants are now observed. The War Machine already makes this comparison for its tokens; 馬 makes it for replies. The draft schema puts them in one record (E-gcc-schema §16, items 11–13).</li>
</ul>
<p class="meta">Source of this page: futon3c <code>holes/labs/M-repl-circus/blog-elephant-2000/make_post.py</code>. McCarthy, J., <em>Elephant 2000: A Programming Language Based on Speech Acts</em>, Stanford University.</p>
</article></main>
</body>
</html>
""".format(f1=f1, f2=figure2(), f3=figure3(), pa=esc(pa), pb=esc(pb), g=esc(g), wa=esc(wa), wb=esc(wb))


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=OUT)
    a = ap.parse_args(argv)
    with open(a.out, "w", encoding="utf-8") as f:
        f.write(page())
    print(a.out)


if __name__ == "__main__":
    main()
