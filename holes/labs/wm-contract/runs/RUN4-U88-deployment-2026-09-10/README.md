# RUN4 U88 disabled deployment packet

This packet is configuration evidence, not activation. `server-config.disabled.edn`
pins the reviewed futon2 U88 manifest, allowlists, casting, existing serving route,
and every supported durable root. It intentionally contains no bearer value and
does not create any directory. The mission remains DRAFT outside discovery roots;
the preflight reports that eligibility refusal separately from wiring failures.

Read-only preflight:

```sh
clojure -M -m futon3c.wm.run4-deployment-preflight \
  holes/labs/wm-contract/runs/RUN4-U88-deployment-2026-09-10/server-config.disabled.edn
```

The operator must separately provision a 256-bit lowercase hexadecimal bearer,
create and permission the named roots, activate the reviewed mission, install
server-owned mission/admissibility functions, and explicitly enable the RUN4 and
series flags. None of those actions is authorized or performed here. Durable
acceptance reporting remains operator-invoked through
`futon3c.wm.run4-acceptance-report/report-durable`; there is no request-controlled
evidence/root route and `:accepted?` remains false.
