# The Interest-Network Scribe (`scribe-1`)

`scribe-1` is a persistent agency agent that turns **registered EOI essays** into
**Interest-Constellation data**. It is the downstream worker behind the Arxana
essays browser's opt-in **IC** ("Interest Constellation") ingest.

## What it does

Given one registered essay (an EOI), the scribe:

1. Reads the essay from the store (entity, sections, annotations, source/manifest).
2. Runs the **Track-B reduce** from
   `futon5a/holes/excursions/E-interest-mining.md` — extracts the essay's
   ~10-star **interest core** (not a raw theme×section index).
3. Writes to the store, all stamped with `:essay-id` provenance and candidate
   flags (`:speculative true :ratified? false :for-ratification true`):
   - **interest-star** entities `arxana/interest/<essay-slug>/<star-slug>`
   - **semantic-arc** hyperedges between stars
   - **section→star witness** hyperedges (evidence, never apex spokes)
   - a per-EOI **diagram** container
4. Bells a reviewer agent (e.g. `claude-6`) with a summary when done.

Cross-EOI gluing into the single **Interest Constellation** is a separate,
operator-ratified step — the scribe never glues across EOIs.

## How it's triggered (opt-in)

Ingest is **opt-in**, modelled on the media *stage-to-EP* operation — there is
**no** auto-ingest on essay registration:

- In the Arxana essays browser, the **IC column** shows `✓` for essays whose
  interest-core is already in the store.
- Put point on an essay and press **`i`**
  (`arxana-browser-essays-ingest-to-constellation`). It confirms, then POSTs a
  bell to the agency, which invokes `scribe-1` (a `claude` subprocess, async).
- The scribe runs in the background; press **`g`** to refresh the IC column once
  it has written the stars.

Bell path: Emacs `arxana-browser-essays--dispatch-interest-scribe-bell`
→ `POST http://localhost:7070/api/alpha/bell` `{agent-id "scribe-1" …}`.

## Registration (required for `i` to do anything)

`scribe-1` must be registered in the running agency or the ingest bell has no
processor (and you get **no checkmark, silently**).

- **At startup:** set `FUTON3C_REGISTER_SCRIBE=1` — `dev/futon3c/dev/agents.clj`
  then calls `peripheral-agents/register-scribe-agent!`.
- **Live (no restart):** via the drawbridge (see `README-drawbridge.md`):
  ```clojure
  ((resolve 'futon3c.dev.peripheral-agents/register-scribe-agent!)
   {:make-claude-invoke-fn @(resolve 'futon3c.dev/make-claude-invoke-fn)
    :read-session-id        @(resolve 'futon3c.dev/read-session-id)})
  ```
  (Use `resolve`/`@` rather than a bare namespaced call — the `/eval` analyzer
  rejects compile-time namespaced symbols.)

`scribe-1` runs `claude` with `bypassPermissions` and `:capabilities
[:write :observe]`, so it can write the interest-core to the store unattended.

## Troubleshooting: "I pressed `i` and got no checkmark"

1. **Is `scribe-1` registered?**
   ```clojure
   (some (fn [a] (= "scribe-1" (get-in a [:id :id/value])))
         (vals (:agents (futon3c.agency.registry/registry-status))))
   ```
   If `nil`, register it (above). This is the usual cause.
2. **Did stars get written?** Query `arxana/interest-star` and look for the
   essay's slug. If absent, the scribe job hasn't completed (it's an async LLM
   job — give it time) or failed (check the agency / reviewer bell).
3. **Refresh the column** with `g` (the IC status is cached per render).

## TODO — self-heal

The `i` press should *guarantee* the scribe is up: if `scribe-1` isn't
registered, ingest should register it (or fail loudly) rather than dropping the
bell silently. Cleanest options: register peripherals unconditionally at startup,
or have the bell endpoint lazily `ensure-scribe-registered!` (the deps are
reachable as `futon3c.dev/make-claude-invoke-fn` / `…/read-session-id`). Tracked
in `futon4/holes/TODO.md`.
