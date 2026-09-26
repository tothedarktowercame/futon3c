# R10 operator-lane acceptance run — commission spent

Operator: joe (authorization this turn: "Yes, you can reload, and you
can run the curl directly"). Executor: claude-15, 2026-09-14.

Procedure and observations:

1. Reload from master into the serving JVM via proof-eval.sh (policy:
   reload-from-master always allowed): r10-commission,
   r10-commission-binding, r10-click-adapter, transport.http. Verified
   in-JVM: binding sha 7c5018493c8b10987277231c15c67b9fc9a2102e2c358c
   47bd688a2a303217df, adapter var resolvable, GET /api/alpha/wm/click
   still serving.
2. Pre-check: runner idle (running? false).
3. POST /api/alpha/wm/click with exactly {"r10-commissioned": true}.
   Response: :ok true; commission r10-click-commission-2026-09-14-01
   (issuer joe) echoed with authority path + sha; receipt
   :dispatch/id = :click/id = wm-click-a1100750-0e8c-4a69-958c-ab4e1acb236a;
   :evidence/id e-af73b9b5-a164-4a18-9bb6-68eac432cf8c.
4. Durable reservation read back from
   data/r10-reservations/r10-click-commission-2026-09-14-01.edn:
   :state :recorded, with reserved-at < dispatched-at < recorded-at
   (15:45:16.039 -> .042 -> .229) and the dispatch/evidence ids joined.
5. Runner status: running? true with exactly that click-id — the click
   is real and in flight.
6. Live single-use probe: a second identical POST refused
   ("R10 commissioned click refused") and the runner click-id was
   unchanged — no second click was minted. (Pre-dispatch refusal:
   runner-busy or duplicate-commission; both correct.)
7. Evidence entry fetched from the serving durable store via
   GET /api/alpha/evidence/e-af73b9b5-...: tags
   [:coordination :scheduled-dispatch :R10], session-id = the click id,
   body joins the full commission and the linked dispatch receipt at
   :process/stage :dispatched.

This is r10-record evidence produced by the production HTTP caller
through run-scheduled-dispatch! — the credit bar (integration, not
mechanism-only evidence) is met. Commission
r10-click-commission-2026-09-14-01 is SPENT; any future commissioned
click requires a new operator-committed authority file and binding
rebind (cohort-rebind pattern).

Cosmetic note (not blocking, no live churn): the HTTP catch surfaces
adapter refusals as the generic "wm-click-start-failed" because
refuse! carries :error/code while the handler reads :error; status and
message still surface. Align if the commissioned path is ever
generalized.
