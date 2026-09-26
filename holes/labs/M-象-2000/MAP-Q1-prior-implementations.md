# MAP-Q1 — Was Elephant 2000 implemented before the LLM-agent era?

Mission M-象-2000, MAP phase. Researched 2026-09-26 (claude subagent). Every
claim below cites a URL that was opened (WebFetch, `gh api`, or `curl` +
`pdftotext`); where a claim rests only on a search-engine snippet it is marked
**(snippet only)**.

## Verdict

No implementation of Elephant 2000 as McCarthy specified it exists from before
the LLM-agent era, and McCarthy never wrote one himself. His paper describes an
interpreter (one history list of events, inputs matched against it on each
arrival, "the simple form of matching done by Prolog may be adequate") and a
compiler to Common Lisp or C as things "we contemplate", not things that exist
(local copy `elephant.txt` §7 "Implementation", l.437–464; the paper says "This
article is exploratory"). Jimmy Miller's 2024 review says "no one rushed to make
implementations of this language". What exists before about 2023 is partial
work that borrows pieces of the design, in three groups:
(1) **small hobby languages that implement parts of it**: Carin Meier's *Babar*
(Clojure, 2013; speech-act syntax, requests become commitments, cancelled
commitments stay queryable), HParker's *Horton* (Python, 2014; assertions and
requests, values with `was`/`has_been` history queries, promises never
finished), William Byrd's *elephantKanren* (Scheme/miniKanren, 2015; about 60
lines that follow McCarthy's §7 interpreter scheme: every event goes on a
list, and each goal is re-matched against the whole list on every event), and
Kerry Holmes's U. Akron honours project (Python/parglare, 2018; titled
"Elephant 2000" but it is a dialogue-act chatbot DSL with a seq2seq fallback;
no public code found);
(2) **research systems that take over one feature**: Shoham's AGENT0
(1990–91, Lisp) uses McCarthy's airline example and the speech-act I/O, but
Shoham himself notes that it replaces Elephant's reference to the history with
an explicit mental state; ElGolog (De Giacomo, Lespérance, Ternovska, AAAI
2020, SWI-Prolog) is named after Elephant and implements *tests over the
execution history*, with no speech acts;
(3) **works that cite it as inspiration only** (Edelman's Habitat 2018, Frank
McCabe's Star via Meier's 2014 airline port, Agent Factory commitment rules,
Singh's BSPL line, which does not cite it at all).
Of the pre-LLM candidates, **elephantKanren** comes closest to McCarthy's
*runtime* design (a history list as the only state, with rules
pattern-matched against it). **Babar** covers more of the *surface* features
(speech acts, commitments, queryable past including cancelled ones). Neither
does the airline example with reference to the past, and neither checks that
answers are truthful or that promises are kept. Two LLM-era projects (2026)
claim to implement it more fully: `dev-isaacmello/eleph` ("Implements and
extends Elephant 2000 … never implemented by him"; it has airline examples and
a checker for answer truth and promise keeping) and `anuna-research/elephant`
(Rust, signed speech acts plus defeasible logic). They fall outside the
question's "before LLMs" window but are the closest in intent.

Side finding: the mission brief says McCarthy cites Shoham's AGENT0. The local
copy of the paper (1989/1994 draft plus working notes) does **not** cite Shoham;
grep for `shoham` across `elephant.tex`, `.txt` and `.html` returns nothing. The
citation runs the other way: Shoham cites McCarthy (AAAI-91, AIJ 1993, and the
1997 overview chapter).

## Candidates

| Name | Year | What it implements | Code? | URL | Closeness to McCarthy's design |
|---|---|---|---|---|---|
| McCarthy's own paper, §7 "Implementation" | 1989–98 | Design only: an interpreter over a history list of events, matched Prolog-style on each input; a compiler to Common Lisp or C that removes the explicit history | No | https://www-formal.stanford.edu/jmc/elephant/node7.html ; local `storage/references/mccarthy-elephant-2000/elephant.txt` l.437–464 | This is the reference design. No code exists |
| **AGENT0** (Shoham; interpreter documented in Torrance 1991) | 1990–91 | Agent language with speech-act messages (INFORM/REQUEST/UNREQUEST), commitments, and time-stamped beliefs. The worked example is "a minor modification of one due to John McCarthy … Elephant". Shoham says "In contrast to AOP, Elephant2000 currently contains no explicit representation of state … Conditional statements therefore refer to the history of past communication rather than to the current mental state". "We are currently implementing AGENT0 in LISP for the X-windows environment. A Prolog implementation will start soon." | No public source found (GitHub search "agent0 shoham" returned nothing) | https://cdn.aaai.org/AAAI/1991/AAAI91-110.pdf ; https://www.infor.uva.es/~cllamas/MAS/AOP-Shoham.pdf | Close relative that deliberately diverges: speech acts, commitments and the airline example are shared, but it uses mental state instead of reference to the past. Cites Elephant; does not implement it |
| **Babar** (Carin Meier, `gigasquid/babar`) | 2013 (first commit 2013-05-14) | Clojure/Instaparse little language "inspired by Elephant 2000". Speech-act statements: `request`, `convince` (belief), `query`, `ask-query`, `cancel-request`. Requests become stored commitments that a 5 ms watcher tries to fulfil. "remembers all the commitments that it ever had and they can all be queried – even cancelled ones." The query forms include `request-completed`, `created` and `cancelled` | Yes; also on Clojars `babar/babar 0.1.0`. Clone: https://github.com/gigasquid/babar.git (833 KB) | https://github.com/gigasquid/babar ; https://gigasquidsoftware.com/blog/2013/06/04/babar-a-little-language-with-speech-acts-for-machines/ | Partial. Speech acts are statements and commitments are first-class; there is a queryable log of commitments but no general reference to past events, no airline example, no correctness conditions (truthful answers, kept promises). The blog says it was built "to try to implement parts of Elephant" |
| **Horton** (`HParker/horton`) | 2014 (Feb–Apr) | Python framework "based on Elephant 2000". Agents take assertions and requests. `Memorable` values keep every past value, with `was()`, `has_been()` and `same()` ("allows for direct reference to the past"). `goals.md`: assertions, requests and memory done; promises, own syntax and REPL not done | Yes. Clone: https://github.com/HParker/horton.git (196 KB) | https://github.com/HParker/horton | Partial and small. The history query is over sensed values, not over speech acts. Promises never implemented. Python API, not a language |
| **elephantKanren** (William E. Byrd, `webyrd/elephantKanren`) | 2015 (2015-02-10/11) | "Experiments with Searle's Speech Acts/McCarthy's Elephant 2000 … baby steps towards … an event processing kernel based on miniKanren". `oliphant.scm` (1990 bytes): `handle-event` conses each event onto `*loe*` (list of events) and re-runs every registered miniKanren goal against the *whole* history | Yes. Clone: https://github.com/webyrd/elephantKanren.git (156 KB) | https://github.com/webyrd/elephantKanren | Closest to McCarthy's §7 *interpreter* (the history list is the only state; relational matching over the past on each input), but a toy: the demo matches turtle moves, with no speech acts and no promises |
| **Star** (Frank McCabe) + Meier's airline port | 2014 blog | Star has Notify/Request/Query speech acts to actors. Meier ports McCarthy's airline example to it, but the port uses mutable state (`extend plist with pname`, `delete … in plist`) | Star: https://github.com/fmccabe/star (not inspected) | https://gigasquidsoftware.com/blog/2014/06/11/a-taste-of-the-star-programming-language/ | Speech acts only. The airline example is written in the style McCarthy argued against (a data structure instead of reference to the past). Star itself is not presented as an Elephant implementation |
| **Elephant 2000: A Programming Language for Remembering the Past and Building on It** (Kerry J. Holmes, U. Akron honours project; sponsor C.-C. Chan) | 2018 | Per abstract: speech-text I/O "based on Dialogue Act theory" (requests, questions, answers); Python 3.6 with the parglare parser; named-entity recognition, rule matching, English-like syntax for chatbots, seq2seq/LSTM fallback | No public code found (GitHub searches for parglare+elephant and for Holmes found nothing). PDF behind Cloudflare, not read | https://ideaexchange.uakron.edu/honors_research_projects/622/ | Uses the name and the speech-act I/O. The abstract gives no sign of history reference or promises; it is a chatbot DSL. Low-to-moderate closeness, unconfirmed because the PDF could not be read |
| **Habitat** (Joe Edelman, PX/18) | 2018 | Chatroom "social programming": rules posted as messages, live queries over posted and clicked events (e.g. a match rule over past `posted`/`clicked like` events). Footnote: "This all probably owes something to McCarthy's Elephant 2000" | Yes. Clone: https://github.com/jxe/habitat.git (JS/React, 2018-04) | https://nxhx.org/pdf/edelman-habitat.pdf | Inspiration only. It does, however, contain rules that query past events (the matching example) |
| **ElGolog** (De Giacomo, Lespérance, Ternovska, AAAI 2020) | 2020 | Golog extended with tests over the execution history (first-order two-way linear dynamic logic with converse). "drawing inspiration from McCarthy's Elephant 2000 … ElGolog is named after this." SWI-Prolog interpreter (`do/3`, `doLog/4`, `holds/3`) | Paper says code "available at https://www.eecs.yorku.ca/~lesperan/code/ElGolog". **That URL returned 404 on 2026-09-26** | https://www.cs.sfu.ca/~ter/my_web_page/Papers_files/2wGolog.pdf ; https://ojs.aaai.org/index.php/AAAI/article/view/5669 | Implements the *reference to the past* half rigorously, for robot programs, with Prolog as McCarthy suggested. No speech acts, no promises. The most serious academic implementation of any Elephant feature |
| Agent Factory / AFAPL commitment rules (Collier, O'Hare et al.) | c. 2003–2009 | Commitment-rule agent language. Cites Elephant 2000 (Semantic Scholar citation list). A search snippet says the chapter states "no implementation of Elephant 2000 exists" **(snippet only; academia.edu and ResearchGate returned 403)** | Agent Factory is open source (not inspected) | https://www.academia.edu/2884596/Modeling_and_programming_with_commitment_rules_in_agent_factory | Cites; does not implement |
| BSPL / information protocols (Singh, Chopra) | 2011–2017 | Declarative, information-based interaction protocols | Yes (not inspected) | https://www.csc2.ncsu.edu/faculty/mpsingh/papers/mas/AAMAS-12-BSPL.pdf ; …/AAMAS-17-Splee.pdf ; https://www.ijcai.org/proceedings/2017/0037.pdf | **Does not cite McCarthy.** Grep for "mccarthy\|elephant" in all three PDFs returned 0. A parallel line of work, not descended from Elephant |
| **eleph** (Isaac Mello) — *LLM era* | 2026-08 | "Implements and extends Elephant 2000 … never implemented by him". "a history that is the only state"; derives the obligations "answer must be true" and "promise must be kept" from the program text; `examples/airline_buggy.eleph` with `make_reservation`/`cancel_reservation`; Python API; on PyPI as `eleph` | Yes. Clone: https://github.com/dev-isaacmello/eleph.git | https://github.com/dev-isaacmello/eleph | The closest design match found, but post-LLM (created 2026-08-26), so outside the question's window |
| **anuna-research/elephant** — *LLM era* | 2026-09 | Rust CLI and daemon. Humans, LLMs and bots exchange signed speech acts into shared append-only theories; promise fulfilment is derived by defeasible reasoning | Yes. Clone: https://github.com/anuna-research/elephant.git (1.7 MB) | https://github.com/anuna-research/elephant | Descends from Elephant in spirit (speech acts, promises, append-only history). Post-LLM (created 2026-09-07) |

Not implementations, noted for completeness: the c2 wiki page "ElephantLanguage"
asks "Is it theoretical, or has it ever been implemented?" with no answer
(recovered text at `BekaValentine/c2_wiki_recovery`); PLDB lists Elephant 2000
as a 1989 language with no implementation link (`breck7/pldb concepts/elephant.scroll`);
Jimmy Miller, https://jimmyhmiller.com/advent-of-papers/2024/dec-1-elephant-2000
("no one rushed to make implementations of this language"); Windley's 2008
write-up of McCarthy's talk says nothing about implementation
(https://www.windley.com/archives/2008/03/john_mccarthy_on_the_elephant_programming_language.shtml);
`exp2exp/ob-servant` (2020) mentions Elephant only in a code comment.

## Searches run

Local:
- `grep -i "implement|interpret|compil|prototype|Shoham|Agent0|KQML|Genesereth|Hewitt"` over
  `storage/references/mccarthy-elephant-2000/elephant.{txt,tex,html}`. Result:
  §7 describes an implementation as contemplated only; no known implementation;
  no Shoham citation.

Web (WebSearch):
1. `"Elephant 2000" McCarthy implementation interpreter`
2. `"Elephant 2000" github`
3. `"Elephant 2000" speech acts programming language implemented prototype`
4. `Babar gigasquid Carin Meier speech acts Elephant 2000 language`
5. `McCarthy Elephant 2000 "speech acts" programming language implementation Prolog history "reference to the past"`
6. `ElGolog "memory of the execution history" De Giacomo Lespérance Ternovska Elephant`
7. `Joe Edelman "Social Programming Considered as a Habitat for Groups" Elephant 2000`
8. `"Elephant 2000" implementation site:news.ycombinator.com`
9. `"Elephant 2000" McCarthy event sourcing "refer to the past"`
10. `Shoham "Agent-oriented programming" AGENT0 McCarthy Elephant 2000 speech acts`
11. `Shoham 1993 "Agent-oriented programming" … "Elephant2000"`
12. `AGENT0 interpreter source code Lisp Torrance CMU AI repository`
13. `"Elephant" McCarthy speech acts language thesis implementation "airline reservation" history interpreter 1990s`
14. `Singh Chopra BSPL "information protocols" McCarthy "Elephant 2000"`
15. `"Elephant 2000" Havelund Peled runtime verification past time "McCarthy"`
16. `Collier O'Hare "commitment rules" Agent Factory "Elephant 2000"`
17. `"Elephant 2000" McCarthy "implemented" OR "implementation of" … Lisp Prolog Haskell Erlang`
18. `elephant2000 OR "elephant-2000" interpreter sourceforge OR gitlab OR codeberg speech acts`
19. `"Elephant" McCarthy "speech act" … blog implementation "airline" … Clojure OR Racket OR Scheme`

GitHub (`gh`):
- `gh search repos "elephant 2000"` → webyrd/elephantKanren, HParker/horton
- `gh search repos "elephant2000"`, `"parglare elephant"`, `"agent0 shoham"`, `"agent-0 shoham"`, `"elephant chatbot parglare"` → nothing
- `gh search code "Elephant 2000"` → babar, elephantKanren, eleph, pldb, c2 wiki recovery, ob-servant, jimmyhmiller/PlayGround, anuna-cooperative/agent-comms-wiki, logicmoo (copies of McCarthy's text)
- `gh api repos/...` metadata and README for each repo in the table

Citation graph:
- Semantic Scholar API, citations of paper `a060aa9128734a37a1fac0bad2c59866be4ba6e5`
  (26 citing papers; checked ElGolog, Edelman, Holmes and Collier from that list).

Pages that could not be read: Britannica (403), the U. Akron PDF (Cloudflare 403),
ACM DL for the OOPSLA 2007 abstract (403), academia.edu/ResearchGate (403),
Lambda the Ultimate node/3180 (socket closed twice), the InfoQ 2008 interview
(video only, no transcript), the ElGolog code URL (404).

---
Reviewer check (claude-14, 2026-09-26): every github.com URL in this report
returns 200 (repo pages; `.git` clone URLs redirect with 301) except
https://github.com/fmccabe/star, which returns 404 — Star's source is no
longer at that address.
