# Together: several people editing one Arxana document

Mission and history: `futon4/holes/missions/M-arxana-together.md`. This file is
the operating manual: where the code is, how to run and check it, and what to do
when it misbehaves. Peer-to-peer version (not started): `M-arxana-together-apart.md`.

## What it is

One hub in the futon3c JVM holds each document as an append-only log of edits.
Emacs and browser clients keep the same list of characters and apply the same
rule, so everyone sees the same text. Every character has an id, an author, and
optionally a face or a mark (its colour). Structure (which answers make up a
form) is a separate log of serialised acts.

    browser page  --HTTPS-->  Caddy /edit/  --loopback-->  hub :7096  (futon3c JVM)
    Emacs on zone ---------------------------loopback-->   hub :7096
                                                             |
                                             ~/storage/arxana-together/<KEY>.log.edn
                                             ~/storage/arxana-together/structure/<WS>.log.edn

## Where the code is

| part | repo, path |
|---|---|
| text engine (pure) | futon3c `src/futon3c/arxana/together.clj` |
| hub: HTTP routes, logs on disk | futon3c `src/futon3c/arxana/together_http.clj` |
| hub singleton started with the JVM | futon3c `src/futon3c/arxana/together_service.clj`, `dev/futon3c/dev/bootstrap.clj` (`start-together-service!`; `FUTON3C_TOGETHER_SERVICE_AUTOSTART=0` turns it off) |
| structure engine and routes | futon3c `together_structure.clj`, `together_structure_http.clj` |
| browser client | futon3c `resources/arxana/together/web/`: `together.js` (model), `binding.js` (text area to operations, colours), `editor.js` (one editor), `page.js` (list and single document), `form.js` + `form.html` (a shared form) |
| shared fixture all three models must pass | futon3c `resources/arxana/together/conformance.json` |
| Emacs model, binding, transport, live buffers | futon4a `lisp/futon4a-together.el`, `-buffer.el`, `-net.el`, `-live.el` |
| sharing the incubator form | futon4a `lisp/futon4a-incubator-together.el` |
| Demo A records for these features | futon3c `together_catalogue.clj`, `incubator_catalogue.clj`, `catalogue_document.clj` |

## Using it

- Browser: `https://zone.hyperreal.enterprises/edit/` lists documents and shared
  forms; `?doc=KEY` opens one document; `form.html?ws=incubator.<layer>` opens a form.
- Printing: the "Download PDF" link on a document's status line, or
  `GET doc/KEY/pdf`. The hub (`together_pdf.clj`) typesets the text as it is at
  that moment with LuaLaTeX: US letter, Libertinus, 1.45 line spacing, a 1.7 in
  right margin for notes, headings and provenance colours kept, struck text
  crossed out. It runs `/usr/local/texlive/<newest>/bin/*/lualatex`
  (`TOGETHER_LUALATEX` overrides), with shell escape off, outside the
  document's lock. A shared form (`form.html`) has no PDF link yet.
- Emacs: `M-x futon4a-together-open RET KEY` for a document. In the incubator
  form, `E` then `M-x futon4a-incubator-together-share-all` shares every answer
  and writes the form's index; `C-c C-s` detaches and saves as usual.
  The Emacs libraries are not loaded at startup: `(require 'futon4a-together-live)`
  and `(require 'futon4a-incubator-together)` with `futon4a/lisp` on the load path.
- Logins are in the `/edit/` block of `/etc/caddy/Caddyfile` (realm
  `arxana-together`). Adding a person is one `user hash` line there
  (`caddy hash-password`), then `sudo caddy reload --config /etc/caddy/Caddyfile
  --adapter caddyfile` (`systemctl reload caddy` fails on this host). Every login
  can read and edit every document; there are no per-document permissions.

## The one rule that must not be broken

The hub takes the editor's identity from the `X-Together-Editor` header and
believes it, which is safe only because it listens on loopback and Caddy sets
that header from the logged-in user, replacing whatever the client sent. Never
expose port 7096, and never proxy to it without that `request_header` line.
`start!` refuses a non-loopback host.

## Checking it

    scripts/together-check.sh          # every suite in both repos, about 3 minutes
    scripts/together-check.sh --fast   # unit suites only

The script never touches the live hub, `~/storage/arxana-together`, the
incubator's session, or a running Emacs; the end-to-end runs start their own hub
on a free port in a temporary directory. Tests must keep it that way. Four
incubator tests create and kill a buffer named `*Arxana Incubator*`: run them in
batch only, never inside the Emacs that has the real form open.

## Changing it while people are using it

- **Web files are served from this checkout as they are on disk.** An edit to
  `resources/arxana/together/web/` is live at once, half-finished or not (on
  2026-11-20 a JVM restart killed a job mid-edit and the unfinished page was
  served for a quarter of an hour). Do page work in a worktree
  (`~/worktrees/futon3c/<name>`) and bring it over when it passes.
- **Clojure changes** reach the running hub by reload from this checkout, never
  by restarting the JVM (that is Joe's call):
  `scripts/proof-eval.sh '(require (quote futon3c.arxana.together-http) :reload)'`.
  That is enough when only functions the handler calls have changed. If the
  handler function itself or `start!` changed, also
  `(futon3c.arxana.together-service/stop!)` then `(…/start!)`; open pages
  reconnect by themselves.
- After `stop!`/`start!` or a JVM restart, a document is read from its log on
  first use. Since packet 15 (608dd5f9f) a 5,000-entry log loads in well under a
  second; before it, the two long documents of 2026-11-20 took about a minute
  each, with every request for that document waiting.
- Check `curl -s -H 'X-Together-Editor: joe' localhost:7096/together/v1/health`.

## When something looks wrong

| symptom | what it was on 2026-11-20 | where to look |
|---|---|---|
| page stays blank or white | the hub was holding that document's lock for a long operation (a listing that replayed every log; a 19,000-character insert) | Caddy's access log for the `state` request's duration: `sudo tail /var/log/caddy/zone.access.log`; `GET docs` should answer in milliseconds |
| caret away from the text, or a selection showing different words | the coloured layer and the text area wrapped at different widths (scrollbar on one only; bold on one only) | `style.css` `.text-layer` and `.text-editor` must have the same box, font and overflow |
| "Posting failed; retrying" stays on the status line | one request dropped on a mobile link; the retry had succeeded | all posts 200 in the access log means nothing was lost |
| all text suddenly one author's colour | select-all, a keystroke, and the browser's undo re-inserted the text as that person | the log still has the originals as deleted entries: `~/storage/arxana-together-tools/restore-attrs.py KEY DELETE_REV` reports, and with `--go` restores |
| a document will not load ("Damaged document log") | a partial line at the end of its log after a failed write | the file under `~/storage/arxana-together/`; the last line must be a complete EDN map |

The logs are the record. One line per entry, `{:rev :op :author :parents :at}`;
never edit a line in place. `~/storage` is backed up periodically.

## Tools kept outside the repos

`~/storage/arxana-together-tools/`: `extract-incubator-buffer.el` and
`seed-incubator-form.py` (copy the open incubator buffer, with colours, into a
document), `restore-attrs.py` (above). `~/storage/arxana/design-catalogue/`:
`write-together.clj`, `write-incubator.clj` (the Demo A live writes, each made
on Joe's go) and `seed-demo-doc.clj` (the catalogue as the document `demo-a`).

## Scratch documents on the live hub

Made while building, safe to ignore: `proof-073745`, `caddy-proof-…`, workspaces
`proof-ws-1120` and `proof-form`. `demo-a` is the Demo A catalogue as text;
`incubator-form` is a copy of the incubator form that real users edited on
2026-11-20, not connected to the form's session.

## Known limits

See "Known weaknesses" in the mission file. In short: no undo of your own that
restores the same character ids; no per-document permissions; deleted characters
are never compacted; structure acts cannot change or remove a node; a document
copy does not follow its source.
