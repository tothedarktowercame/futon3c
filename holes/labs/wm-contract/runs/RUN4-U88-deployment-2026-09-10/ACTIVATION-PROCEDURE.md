# RUN4 U88 activation procedure (NOT EXECUTED)

The boot integration is disabled unless `FUTON3C_RUN4_U88_ENABLED=1` is
present before the serving JVM starts. It reads only the fixed reviewed
template and fixed private credential path. The credential value must never be
printed or copied into shell history.

Before activation, the U88 mission is still DRAFT. An operator must first land
and independently review the intended OPEN mission change, then regenerate and
review the task pin, series pin, deployment-template manifest hash, and source
hashes. Starting with the current DRAFT bytes is expected to fail eligibility;
that refusal is not a wiring failure.

From a separate shell (not the shell or JVM being served), verify Agency can be
recovered and record the current status:

```sh
cd /home/joe/code/futon3c
curl --fail --silent http://127.0.0.1:7070/api/alpha/health
curl --fail --silent http://127.0.0.1:7070/api/alpha/agents
```

After the mission/pins are reviewed and Joe separately approves a restart,
stop the existing service using its normal supervisor. In the supervisor's
environment (not by mutating a running JVM), set:

```sh
FUTON_WM_FPI_DARK=1
FUTON_WM_BETA_DARK=1
FUTON_WM_TRACE_POLICY_DETAILS=1
FUTON_WM_RECORDING_CONTRACT=1
FUTON3C_RUN4_U88_ENABLED=1
```

Then start the normal futon3c service command used by that supervisor. Do not
source or echo the bearer file; bootstrap reads it directly and requires the
canonical fixed path, a regular file owned by the serving user, and mode 0600.
Startup materializes the reviewed template, validates all pinned bytes and
store roots, and constructs the existing HTTP handler. It does not submit a
RUN4 request.

From the separate shell, repeat the health and agent-list commands and verify
the recovered roster/queued work before any RUN4 request. Inspect startup logs
for a typed refusal only; the secret must not appear. Finally run the read-only
deployment preflight against the newly reviewed template. A series-step POST
remains a distinct operator action and is not authorized by this procedure.

## Post-review live handler installation (NOT EXECUTED)

The first restart must keep `FUTON3C_RUN4_U88_ENABLED=false`. After that JVM
passes effective-environment checks and the OPEN mission plus regenerated pins
have independent review, an authenticated private operator evaluation may
install the configuration without another restart:

```clojure
(let [fragment (futon3c.wm.run4-boot/materialize true)]
  (futon3c.transport.http/reconfigure-handler!
   #(assoc % :run4 (:run4 fragment))))
```

The expression returns only `:ok`, `:status`, and `:run4-configured?`; it must
not print `fragment`, handler metadata, or captured server configuration.
Materialization reads the fixed private credential internally and completes
template, source, store, mission, and effective-environment validation before
the existing handler is replaced. A refusal leaves the old handler installed.

This installs route configuration only. It neither invokes the route nor
creates an admission, click, series transition, run record, or acceptance.
