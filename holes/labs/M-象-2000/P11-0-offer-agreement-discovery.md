# P11-0 — offer and agreement discovery

This packet is read-only discovery. It proposes a separate structured-act
subsystem; it does not make conversation text, a bell, a clock, or a notice into
an offer or an agreement.

## Binding decisions

Joe approved an agreement that joins a named offer to a named acceptance and
specifies how much was accepted. An ambiguous “yes” requires one short question,
not a provisional reading (`holes/missions/M-象-2000.md:981-989`). He separately
required offers and agreements to be structured records, comparable to clocking
in, with at most classical string matching and no LLM reading agent speech
(`holes/missions/M-象-2000.md:1020-1026`). The build plan adds two requirements:
the agreement enters P9 obligations, and an acceptance of a withdrawn offer is
refused and recorded (`holes/labs/M-象-2000/BUILD-PLAN-象2000.md:180-183`). A
grant may cite an accepted offer only after these records exist
(`holes/missions/M-象-2000.md:1866`).

## 1. Existing structured facilities

### Mission clock-in

Clocking in currently consists of a typed target, an exact seat, live state, and
a durable transition:

* Emacs parses `C-*`, `M-*`, `E-*`, and `T-*` targets, stores their components in
  buffer-local fields, updates the header, and invokes the clock callback
  (`emacs/agent-chat.el:1213-1235`; the interactive command is at :1251-1256).
* The JVM keys clock state by `[agent session]` and an explicit dispatch mission
  immediately replaces that seat's clock (`agency/clock_store.clj:199-224`). It
  exposes the exact-seat clock through `/api/alpha/agent-clock`
  (`transport/http.clj:6993-7016`).
* Durable lineage retracts the prior target and writes the new agent-to-target
  hyperedge with session and witness fields (`agency/clock_lineage.clj:138-165`).
* A bell may carry `mission-id`; `agency_send.py --mission` documents that it
  clocks the recipient (`scripts/agency_send.py:31-39,213-218`).

This is the useful analogy: an offer should be an explicit structured operation
with an exact author/seat and durable record. The clock record itself is not an
offer and has no options or acceptance.

### Parks, bells, notices, and the prompt

* A park records an exact agent/session/surface, dependency set, continuation
  payload, deadline, and bounded resume budget (`agency/parked_on.clj:368-395`).
  Its join releases once and records promise history (`:308-357`). It can wait for
  an offer-related job, but its opaque payload is not semantic authority.
* Bells already carry structured `caller`, typed performative and `ref`, mission,
  mode, warrants, and harness (`scripts/agency_send.py:21-49,190-225`;
  `transport/http.clj:5656-5707`). The server turns them into invoke jobs at
  `transport/http.clj:5775-5798`. A bell can transport an offer id, but a typed
  `agree` bell still records a dispatch performative rather than Joe's agreement.
* Turn notices are exact-seat, bounded, deduplicated, and atomically consumed
  once (`agency/turn_notice.clj:15-18,26-71`). Their text is currently closed to
  withdrawal outcomes (`:20-24`), so P11 should reuse the delivery pattern rather
  than overload those three meanings.
* Prompt-line providers are single-writer per segment id and render against an
  exact agent/session with bounded deadlines (`agency/prompt_line.clj:29-49,
  83-150`). This is suitable for a compact “pending offer” fact backed by an
  exact-seat cache. It is not the offer store.

## 2. Recording an offer when it is made

Use an immutable minted act, `:hx/type :offer/record`, written through a CLI or
an authenticated JVM route before or in the same operation that asks Joe. Its
minimal props should be:

```clojure
{:offer/schema 1
 :offer/author "claude-17"
 :offer/addressee "joe"
 :offer/seat {:agent "claude-17" :session "..."}
 :offer/at "..."
 :offer/until "..."                       ; optional, half-open
 :offer/options
 [{:option/id "1"
   :option/label "build the first packet"
   :option/scope {:description "..."
                  :act-kinds [...] :rule-ids [...]}}]
 :act/stamp {...}
 :act/harness {...}}
```

Option ids must be unique and stable. Scope is structured enough to copy into
the agreement and later grant; prose alone cannot silently authorize acts. The
act stamp is the existing closed executor/signer/authority record
(`agency/act_stamp.clj:29-65`). The minted act id is the offer id.

The route returns that id for inclusion in the agent's question. It also updates
an exact-seat active-offer cache. A `:offer` prompt segment may show, for example,
`offer act:… (2 options)`; a one-shot typed notice can put the same fact in Joe's
buffer. Both are projections. The durable hyperedge remains authoritative.

## 3. Classical acceptance and ambiguity

The Emacs `undo` path supplies the parsing precedent: it trims, case-folds, strips
trailing punctuation, recognizes one anchored grammar, and sends every near miss
normally (`emacs/agent-chat.el:2789-2795,2805-2852`). P11 can recognize only:

* `yes`: accept only when this exact seat has one visible offer with one option;
* `yes 2`: accept option `2` only when this seat has exactly one visible offer;
* `yes act:<offer-id>`: accept only if that visible offer has one option;
* `yes act:<offer-id> 2`: the fully explicit offer-and-option form.

The acceptance route re-reads the offer as of acceptance time, rejects expired or
withdrawn offers, and appends Joe's operator-turn evidence id. It never chooses by
recency when more than one candidate remains.

A bare `yes` that leaves multiple offers or options is ordinary input and is not
consumed as acceptance. Before invoking the agent, Emacs attaches a typed
exact-seat ambiguity fact to that turn (or queues a P11-specific header notice):
`agreement ambiguous: ask which offer/option`. The agent therefore sees the fact
in the same turn header and asks one short question. A later answer uses the
explicit grammar. Extending the existing withdrawal-only notice kinds would mix
unrelated protocols; use an offer/agreement notice namespace or a generalized
closed server-rendered notice type.

## 4. Agreement record and grant source

An accepted choice mints `:hx/type :agreement/record`:

```clojure
{:agreement/schema 1
 :agreement/offer "act:..."
 :agreement/acceptance-evidence "emacs-..."
 :agreement/option-id "2"
 :agreement/scope {...}                   ; exact copy of the chosen option
 :agreement/offeror "claude-17"
 :agreement/acceptor "joe"
 :agreement/at "..."
 :act/stamp {:executor "joe" :signer "joe"
             :authority {:operator true}
             :executor-basis :session-bound}
 :act/harness {...}}
```

Validation reads the offer and acceptance evidence, proves that the option exists
and is active, that the evidence is Joe's operator turn, and that its exact text
matches the accepted command. Endpoints include the offer id, both parties, and
the acceptance evidence id. Idempotency is keyed by offer, option, and acceptance
evidence.

Today `grant_record` requires `:grant/source` to contain exactly
`{:id :author :at :quote}`, then verifies the evidence author, time, quote, role,
and non-harness origin (`agency/grant_record.clj:42-68,108-130`). Therefore an
agreement source is a schema change, not an alternate spelling smuggled through
that map. A later grant schema should accept a closed tagged union, retaining the
legacy evidence source and adding, for example,
`{:kind :agreement :offer "act:..." :agreement "act:..."}`. Validation must read
both hyperedges and prove the agreement names that offer and chosen scope before
grant scope is checked. Until that validator exists, agreements are not grant
sources.

## 5. P9 obligations

An agreement is the immutable source for P9's projection of who owes what. The
chosen option should name structured deliverables/beneficiary/deadline where
applicable; P9 derives obligations whose source is the agreement id, debtor and
creditor are the named parties, and status begins open. Completion changes the
obligation record, not the agreement or offer. Free-form offer prose must not be
converted into additional duties, and an unaccepted or withdrawn offer produces
none.

## 6. Smallest first implementation packet

Build **P11-1: pure offer records**, with no route and no live writes:

1. `futon3c.agency.offer-record`: closed validator and record↔hyperedge mapping
   for one stamped `:offer/record`, including unique options, structured scope,
   exact seat, and optional half-open expiry.
2. `active-offers-as-of` over plain records, retaining withdrawn/expired records
   while excluding them from visible offers.
3. Tests for round-trip, duplicate option ids, description-only authority,
   expiry boundary, other-seat isolation, and the BUILD-PLAN bad case: an
   acceptance lookup against a withdrawn offer is refused rather than selected.

P11-2 can add the offer CLI/route and verified readback; P11-3 can add the pure
acceptance resolver and agreement record; P11-4 can add the exact Emacs command,
same-turn ambiguity fact, and visibility cache; P11-5 can add the grant-source
union; P11-6 can project accepted scope into P9.

## Spot checks

I re-read two claims after drafting:

1. The bell transport really does preserve structured caller/type/ref/mission
   fields: the client builds them at `scripts/agency_send.py:207-220`, and the
   handler reads caller, mission and reply reference at
   `transport/http.clj:5665-5685` before job creation at :5775-5798.
2. Grant sources really are closed evidence-quote maps today: the shape check is
   `agency/grant_record.clj:62-68`, and live validation requires exactly one
   matching evidence record plus author/time/quote/origin checks at :108-130.
