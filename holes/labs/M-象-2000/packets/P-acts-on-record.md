# Packet: persist the turn's acts on its record (HOLE 14 of minus40 SPEC-open-items)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Code with tests; no
reload, no live recompute (a later packet reloads 1–3 together).

## Why

`compute-ports!` (`src/futon3c/xiang/turn_service.clj`) runs `session-acts`
over the session and stores only `:ports`. The acts are thrown away, so the
40ºBelowDB follower runs 象's own `session-acts` to get them, which is
recomputation across a seam (Joe's rule, 2026-09-28). Store the acts and the
follower reads them.

## Behaviour

In `compute-ports!`, after `turn-ports` succeeds, also store on the record
`:acts`: a vector of this record's acts (those whose `:record` is this record
id, or whose `:turn` is this turn id when no `:record` is set), in session-acts
order, each as a map with exactly these keys when present: `:id :kind :author
:at :to :turn :record :port? :standing? :target :carries-out :about :option
:sha :repo :subject :start :end :text`. `:text` trimmed to 120 characters as
`trim-act-text` does for ports. Nothing else about `:ports` changes. Records
that refuse ("No acts for turn") store no `:acts`, as they store no `:ports`.

JSON: keywords become snake_case strings as the rest of the record does
(`port?` → `"port?"` is fine; keep whatever `update-record!` produces for the
existing keys, consistently).

## Tests (`test/futon3c/xiang/turn_service_test.clj`, the existing ports fixture)

1. After a settle, the record carries `:acts`; the set of their ids equals the
   set of ids `session-acts` produces for that record; each has `:port?` and
   `:standing?`.
2. A record whose turn id is reused in the session (two records, same
   `turn_id`) stores only its own acts (by `:record`), with qualified ids.
3. A commit act stores `:sha :repo :subject` and its `:carries-out` when set.
4. A refused record stores neither `:ports` nor `:acts`.

## Gates

clj-kondo, `futon4/dev/check-parens.el`, `clojure -M:test` for the xiang
namespaces; warrant if the script exists, else paste output. Touched files
only; never `git commit -a`. Bell claude-16 back with the sha and test output.
