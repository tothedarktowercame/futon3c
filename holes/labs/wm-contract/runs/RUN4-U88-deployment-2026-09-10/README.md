# RUN4 U88 disabled deployment packet

This packet is disabled serving configuration for the activated mission. `server-config.disabled.edn`
pins the reviewed futon2 U88 manifest, allowlists, casting, existing serving route,
and every supported durable root. It intentionally contains no bearer value and
does not create any directory. The canonical U88 mission is OPEN in ordinary discovery and the source/task/series/template chain is exact. The template and automatic start remain false, so preflight reports mission activation separately from launch eligibility.

Read-only preflight:

```sh
clojure -M -m futon3c.wm.run4-deployment-preflight \
  holes/labs/wm-contract/runs/RUN4-U88-deployment-2026-09-10/server-config.disabled.edn
```

The operator must separately provision a 256-bit lowercase hexadecimal bearer,
explicitly enable the RUN4 and series flags before serving. The roots, private bearer, startup environment, loaded consumer attestation, mission activation, exact resolver, and exact action guardrail are already prepared; this change does not enable or invoke the route. Durable
acceptance reporting remains operator-invoked through
`futon3c.wm.run4-acceptance-report/report-durable`; there is no request-controlled
evidence/root route and `:accepted?` remains false.
