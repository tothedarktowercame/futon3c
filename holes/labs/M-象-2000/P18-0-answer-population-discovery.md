# P18-0 — answer population discovery

Date: 2026-09-28. This is read-only discovery for BUILD-PLAN P18, whose test is
that an answer declares its population and both read-time axes and detects
“asked A, answered B” (`BUILD-PLAN-象2000.md:242-245`).

## 1. P9's question and actual population

The HTTP question is: for `agent=<id>`, at `T` (explicit `at`, or the route's
read-start time), which open promise and accepted-agreement obligations does the
agent owe or is owed, plus its unchecked waits? The route chooses `:current`
when `at` is absent and `:as-of` when present
(`src/futon3c/transport/http.clj:10232-10247`). The projection returns `:owes`,
`:owed`, `:unchecked`, global `:incomplete`, and closed rows internally as
`:ignored` (`agency/obligations.clj:132-210`); HTTP replaces the latter with only
`:ignored-count` (`transport/http.clj:10248-10253`). Thus the question's kinds
are `#{:promise :agreement}`, not every possible act that might imply a duty.

The reader's population is:

- all evidence tagged `promise-history`, repository-wide, because filtering by
  debtor would omit promises owed to the named agent
  (`agency/obligations_reader.clj:1-4,89`);
- all evidence tagged `promise-outcome`, then only types
  `promise/fulfilled`, `promise/lapsed`, and `promise/fulfilment-check` survive
  into the projection (`obligations_reader.clj:90-95`);
- `agreement/record` hyperedges at endpoint `agent:<id>` and the referenced
  `offer/record` hyperedge for each agreement (`obligations_reader.clj:96-117`).

Evidence pages have 1,000 rows. Cursors are followed to exhaustion, retaining
both temporal parameters in as-of mode, with a hard cap of 20 pages. A partial,
malformed, repeated, or over-cap cursor is refused as `:truncated-input`
(`obligations_reader.clj:14,41-62`). Hyperedges have no paging implementation:
a full page or cursor is refused (`obligations_reader.clj:64-73`). Therefore a
page cap cannot silently become an answer.

The projection applies event time `<= T` (`obligations.clj:20-24,140-142`). It
excludes broken promise chains and undecodable creations from debts and puts
issues in `:incomplete` (`obligations.clj:143-164,193-202`). Closed checkable
promises leave `:owes`/`:owed`; unreadable agreements, missing offers,
pre-repair history faults, missing beneficiaries, missing checks, and orphan
checks are reported as incomplete. `:incomplete` is intentionally unfiltered by
agent because a broken row may not reveal its parties (`obligations.clj:203-209`).
That means it describes the source population's defects, not “incomplete duties
of this agent.”

Today's basis is only `{:mode :t :pages :rows}`
(`obligations_reader.clj:118-131`). It does not declare:

- the question's agent or requested kinds;
- source tags, types, endpoints, outcome-type filtering, or page limits;
- excluded/failed rows and their counts;
- a `complete?` assertion per source;
- separate system-as-of and valid-as-of values;
- that current mode uses no store as-of parameters, so sequential source reads
  are not one pinned system snapshot;
- that `T` in current mode is a projection cutoff captured before those reads;
- that the returned `:incomplete` population is repository-wide.

## 2. Mechanical population declaration

A closed declaration can be:

```clojure
{:question {:agent "claude-17"
            :at "2026-09-28T22:05:53Z"
            :kinds [:promise :agreement]}
 :sources [{:kind :evidence
            :filter {:tags ["promise-history"]}
            :rows 2125 :pages 3 :page-limit 1000 :complete? true}
           {:kind :evidence
            :filter {:tags ["promise-outcome"]
                     :types [:promise/fulfilled :promise/lapsed
                             :promise/fulfilment-check]}
            :rows 3 :pages 1 :page-limit 1000 :complete? true}
           {:kind :hyperedge
            :filter {:type :agreement/record :end "agent:claude-17"}
            :rows 0 :pages 1 :page-limit 1000 :complete? true}
           {:kind :hyperedge
            :filter {:type :offer/record :ids []}
            :rows 0 :pages 0 :page-limit 1000 :complete? true}]
 :excluded [{:reason :unreadable-or-incomplete :rows 49
             :scope :repository-wide}]
 :read {:mode :current
        :system-as-of nil
        :valid-as-of "2026-09-28T22:05:53Z"
        :started-at "2026-09-28T22:05:53Z"}}
```

`same-population? [question population]` should return `{:status :same}` or
`{:status :mismatch :reasons [...]}` from a closed reason set:
`:agent-mismatch`, `:time-mismatch`, `:kinds-mismatch`, `:source-missing`,
`:filter-mismatch`, `:incomplete-source`, and `:read-axis-mismatch`. It compares
question fields exactly, requires the source descriptors for every requested
kind, refuses any `:complete? false`, and requires both axes to equal an
explicit as-of `T`. Current mode must explicitly declare an unpinned system
axis; it must never masquerade as an as-of answer.

## 3. Substitution cases and live reads

Concrete bad cases:

1. Asked `at=T`, read current sidecars: `:read-axis-mismatch` (and usually
   `:time-mismatch`). Current code selects mode from the presence of `at`, so
   this does not happen on the route today; tests already pin temporal query
   parameters. The response basis exposes `:mode`, but no checker joins it to
   the question.
2. Asked for agent X, projected/filter endpoint Y: `:agent-mismatch`. The route
   passes one local `agent` to both reader and projection, so the present code
   does not substitute Y internally. The response does not echo agent, so a
   downstream consumer cannot verify that fact from the answer alone.
3. Asked for all P9 obligations, source truncated at the page cap:
   `:incomplete-source`. Current code refuses this as 409 `truncated-input`
   rather than returning a partial 200 (`obligations_reader.clj:37-39,53-59`).

Live `GET /api/alpha/obligations?agent=claude-17` at
`2026-09-28T22:05:53.845005373Z` returned 200 in 1.324 s: current mode; 2,125
history rows/3 pages, 3 outcome rows/1 page, 0 agreements, 0 offers; `owes=1`,
`owed=0`, `unchecked=3`, `incomplete=49`. This confirms the basis counts the
repository-wide population but does not name its filters or the queried agent.

One bounded historical request used `at=2026-09-28T21:05:53.840671Z`. The
client received no bytes and timed out after 65.002 s (HTTP 000), consistent
with the already recorded temporal-tag problem: the earlier P9-2b run returned
a typed 504 at about 60 s (`holes/missions/M-象-2000.md:2184-2192`). I did not
retry. A typed server 504 is intended (`transport/http.clj:10254-10263`), but
this attempt shows the client can expire before receiving it.

## 4. Next adopters

After P9: P12-4 disclosure audit; P7b weak-activation evidence; P3 dispatch
reconstruction; P7 prompt-line retrieval answers. Each already has some basis
or descriptor, but none uses one shared population/question checker.

## 5. First implementation packet

Add a pure `futon3c.agency.answer-population/same-population?`, then have the
P9 reader emit the closed declaration above and the route join it to the parsed
question before returning 200. Tests: current and explicit-as-of envelopes;
agent, time, kind, filter, axis, and completeness mutations; the constructed
bad case asks `agent-a` as-of T but supplies a complete current-mode population
for `agent-b`, and must return both `:agent-mismatch` and
`:read-axis-mismatch`. Existing 409 truncation and 504 timeout behavior remains.
