# RUN4 U88 successor deployment — disabled

This directory retains the disabled server template and the exact
server-owned `:execution-cohort` dependency for the successor series. Neither
file is a credential and neither enables serving.

The proposed cohort data root is `/home/joe/run4/U88-cohort-20260911`. The
successor serving stores are under `/home/joe/run4/U88-successor`. None is
created or activated by this packet. The old attempted packet and its durable
admission remain unchanged.

At materialization, the operator-owned caller must pass the parsed exact
four-key `execution-cohort.edn` value as `:execution-cohort` and
`futon2.aif.full-loop-cohort/execution-preflight` as
`:cohort-preflight!`. The normal materializer then validates the frozen
manifest, task/config/source allowlists, mission/action/casting, and cohort
before admission. No request may supply either dependency.
