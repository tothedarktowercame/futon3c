# TN: Agency Area Codes — bare agent ids are observer-relative

Date: 2026-07-16
Context: Joe + claude-6, emacs-repl. Joe asked me to bell oxf-claude-2 to
coordinate on the propagators/patterns seam. I signed `--from claude-6`; Joe
flagged that cross-site routing wants `lon-claude-6`. Checking that turned a
naming preference into a live ambiguity. Structure-first per WR-3: this is the
diagram, not the implementation. Nothing in the registry has been changed.

## The finding, in one table

`claude-6` resolves to **two different sessions** depending on which registry
you ask. Measured 2026-07-16 against both live registries:

| registry | `claude-6` → | `lon-claude-6` → |
|---|---|---|
| lucy (`FUTON3C_SITE=lon`), `:7070` | `25aefc4c…` — **this agent** | *does not exist* |
| oxf peer, `:17070` | `380d6e33…` — **a different agent** | `25aefc4c…` — **this agent** |

Two things follow, and the second is the one that matters.

1. The oxf peer **already carries my global name**: `lon-claude-6`, bound to my
   exact session id. It also carries `lon-claude-1..6` and `lon-codex-1`. The
   global name is not missing from the federation — it is missing from **the box
   I live on**.
2. So `claude-6` is not an identity. It is an **indexical**: "whichever claude-6
   is local to the asker". Every site mirrors *remotes* with a prefix and keeps
   *its own* agents bare, so the bare form means something different at each
   site. Two agents both truthfully call themselves `claude-6`.

## Why it did not bite (and why that is the bad news)

The bell worked. `oxf-claude-2` is mirrored on lucy with `invoke-route: local`,
so the whole exchange resolved inside lucy's registry, where `claude-6`
unambiguously means me, and the bellback came home.

Had it federated, `--from claude-6` arriving on the oxf side would have named
**their** `claude-6` (`380d6e33`) — which is oxf-claude-2's own sibling. The
reply would have routed to the wrong agent, and would have looked like it
worked. The signature was correct **by accident of routing**, not by design.
That is the failure mode worth naming: not an error, a silently plausible
delivery to the wrong inhabitant.

## The area-code reading

The telephone metaphor is exact, and it is the whole design.

- Within an area you dial the local number (`claude-6`). Convenient, and it
  means nothing outside the area.
- Across areas you dial area code + number (`lon-claude-6`).
- **Your own number has an area code even when you never dial it.** The area
  code is not a marker of foreignness. It is part of your global identity that
  is *elided* locally.

The current code has the first two and not the third. `federation.clj` treats a
site prefix as evidence of remoteness:

- `site-prefix` (`:161`) reads `FUTON3C_SITE` → `"lon"` on lucy. **The box knows
  its own area code.** It simply never applies it to itself.
- `agent-id-home-site` (`:170`) parses `lon-claude-1` → `"lon"` by regex, so
  qualified ids are already understood.
- `remote-homed-agent-id?` (`:192`) is the AG-2 guard. It refuses site-qualified
  peer ids from being rebound locally — but `known-remote-home-sites` explicitly
  `disj`es the local site, so **`lon-*` is already not-remote on lucy**. The
  guard would not block a local `lon-claude-6`.

So the machinery is 90% present. What is absent is the idea that the local site
prefix names *us*.

## The design: one record, two names

**Alias, not dual registration.** On local registration, bind `<site>-<id>` as
an additional *resolution name* for the same agent record. Bare stays valid
locally; the qualified form is always globally valid; signing the qualified form
is always safe.

**The trap is already visible in the roster, and it is why this must be an
alias.** lucy currently carries `lon-claude-1` as a **separate record with
`session-id: None`**, sitting beside the real `claude-1` (`d5072b7e`). That is a
ghost — the residue of something that registered a second agent instead of
aliasing one. Two records for one inhabitant violates **I-1** (one agent = one
session = one identity) and would make `*agents*` lie about the roster's size.
(`claude-2`, `codex-1` and `lon-claude-1` all currently show a null session, so
there is pre-existing litter here regardless — worth a separate sweep.)

Sketch, to be argued before it is built:

```
register!(bare-id):
  aliases := {bare-id}
  when (site-prefix) and (not (agent-id-home-site bare-id)):
      aliases := aliases ∪ {"<site>-<bare-id>"}
  bind every alias -> ONE record          ; never a second record
  record[:agent/canonical-id] := "<site>-<bare-id>"   ; the global name
```

Then:
- `resolve(id)` accepts either form.
- **Agents sign, and are displayed as, the canonical (qualified) form.** The
  bare form stays as an input convenience only. An agent that signs its indexical
  is asserting something only true where it stands.
- Federation sync keeps importing peers as `<their-site>-<id>`; unchanged.

## What this does not settle

- **Which name is canonical for an agent with no `FUTON3C_SITE`?** Lucy has one;
  a laptop dev box may not. Options: refuse to alias, or synthesise from
  hostname. Unresolved.
- **Idempotency across restart.** If a session re-registers bare after having a
  canonical form, do the aliases rebind or fork? The ghost above suggests this is
  exactly where it went wrong last time.
- **Peer collisions on the alias itself.** If lucy ever imported a real
  `lon-claude-6` proxy from a *third* site claiming home-site `lon`,
  `remote-homed-agent-id?` would then call `lon-` remote and the local alias
  would be refused. Currently impossible (lucy IS lon), but the guard's logic and
  the alias's logic must agree about who owns a prefix.
- **Blast radius.** This is agent identity in the one serving JVM (**I-0**),
  across three sites (lon/oxf/chi), touching the AG-2 guard, the registry, the
  `*agents*` pane, mesh_trace, and auto-bellback routing. The change is small;
  the surface is not.

## Why it matters beyond tidiness

Bare ids are fine until an agent is *quoted* — in a bell's `--from`, in a mesh
edge, in evidence, in a PAR. At that point the indexical is separated from the
site that gives it meaning, and it silently rebinds to whoever holds that number
locally. Every durable record naming a bare agent id is ambiguous the moment it
crosses a site boundary — and the federation has three.

The narrow version of this bug already cost a turn today: a bell sent to
`oxf-claude-2` was answered by them, correctly, only because the routing happened
to stay local.

## Provenance / reproduce

```bash
curl -s http://127.0.0.1:7070/api/alpha/agents   # lucy   : claude-6 -> 25aefc4c
curl -s http://127.0.0.1:17070/api/alpha/agents  # oxf    : claude-6 -> 380d6e33
                                                 #          lon-claude-6 -> 25aefc4c
```

Code: `src/futon3c/agency/federation.clj` — `site-prefix` :161,
`agent-id-home-site` :170, `remote-homed-agent-id?` :192. No alias mechanism
exists: `grep -i alias src/futon3c/agency/*.clj` returns nothing.

Related: memory `federation-roster-completeness` already lists a
"site-qualification slice" as an open gap — this note is that slice, with the
ambiguity now measured rather than anticipated.
