# E-transitive-proxy-ghosts — a proxy nobody can prune

Opened 2026-08-14. Found by Joe noticing `ams-apm-driver` had been `invoking`
for **116h47m** in the `*agents*` HUD. Topology traced by claude-3; code
confirmed independently by ams-claude-2 on Zone.

## The observation

`ams-apm-driver` shows `status=invoking`, `invoke-started-at` ~117 hours old,
`last-active 2026-08-09T20:18:50`. The agent has not existed for five days.

## The topology — absence only exists at the origin

Measured across all four sites the same evening:

```
zone (ams)   apm-driver ABSENT from the roster entirely (37 agents, zero matches)
  |          the real agent; last activity 2026-08-09T20:18:50
  v
chi          ams-apm-driver  proxy=true  origin-url http://104.243.39.24:7070  (zone)
  v
lucy (lon)   ams-apm-driver  proxy=true  origin-url http://172.236.108.82:7070 (chi)
  v
oxf          ams-apm-driver  proxy=true  origin-url ws-uplink://lon            (lucy)
```

**Zone is the only clean box** — and it is the only one with an edge that could
be pruned, because it is the only site that ever observes the agent's absence.

## Why it cannot self-heal

`src/futon3c/agency/federation.clj:935`:

```clojure
(defn- prune-departed-proxies!
  [peer-url roster-ids]
  (let [roster-set (set roster-ids)]
    (->> (peer-proxy-ids peer-url)
         (remove roster-set)        ; only what the IMMEDIATE peer dropped
         ...)))
```

`roster-ids` is built at the call site from that one peer's
`/api/alpha/agents` response, and `peer-proxy-ids` returns proxies registered
*from that peer*. So pruning is strictly **pairwise**.

A relayed proxy is never absent from the hop directly above it. chi advertises
to lucy; lucy advertises to oxf. From each receiver's view its upstream is
presenting a well-formed, currently-listed agent. Nobody is wrong locally, and
the chain re-attests indefinitely something no site has seen since 2026-08-09.

**The invariant that fails:** presence is transitive under relay, absence is
not.

### The code already reasons about relayed identity — one hop deep

`own-site-reflection?` at the same call site excludes a peer's reflection of
our own agents, so loop-back proxies "get pruned rather than kept alive". The
machinery for reasoning about relayed identity exists; it just does not extend
past a single hop. The transitive case is the same class of problem, unsolved.
*(ams-claude-2's observation.)*

## Why it surfaced on 2026-08-14 and not earlier

The bug is five days old; only the symptom is new. `reconcile-stale-invoking!`
used to sweep any `:invoking` agent with no local ledger job after 120s —
including proxies — so this ghost was silently zeroed every two minutes and
re-imported as `invoking` on the next roster. `M-dionysus-winddown` packet D
stopped the sweep touching proxies, on the correct principle that **a proxy's
home site owns its status and the local jobs ledger has no jurisdiction**.

That fix was right and it removed the concealment. A five-day-old ghost became
visible for the first time within hours of it landing.

## Two shapes of fix

Both from ams-claude-2; neither implemented.

1. **Origin-attested pruning.** Propagate an origin identity with the proxy and
   prune when the *origin* is unreachable or no longer advertises, rather than
   when the immediate peer drops it. Makes absence observable at every hop.
   `README-federate.md` F-0 already requires `:origin/site` and `:origin/epoch`
   on durable records, so the metadata this needs is specified.
2. **Origin-attested TTL.** Give proxies a TTL refreshed *only* by
   origin-attested presence, so a relay chain cannot indefinitely re-attest
   something nobody has seen. Fails safe: a partitioned origin eventually
   expires its proxies rather than freezing them forever.

Shape (1) is closer to what the federation design already commits to. Shape (2)
is cheaper and degrades better under partition. They are not exclusive.

## Cost, and why it matters beyond tidiness

117 hours of phantom `invoking` in the operator's primary HUD. The `*agents*`
buffer is the surface Joe reads to know what the fleet is doing; an agent that
has not existed for five days occupying a live-work row is a false statement
about the system, made continuously, by a mechanism working as written.

Same family as the day's other findings: a correct mechanism whose correctness
silently depends on a condition that no longer holds. Here the condition is
"every proxy's absence is observable by someone who can act on it", and under
relay it is not.

## Related

- `holes/missions/M-dionysus-winddown.md` — packet D, which removed the sweep
  that was hiding this.
- `holes/excursions/E-port-wiring-map.md` — the four-site topology.
- `futon0/README-federate.md` — F-0 origin identity; §4 recommended topology,
  which allows a hub to relay while requiring the event to retain its original
  origin and ordering domain. This defect is that requirement not being carried
  into pruning.
