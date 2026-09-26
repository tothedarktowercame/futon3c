# C230 — reload → click → certificate rehearsal

Date: 2026-08-31. Owner: `wm-organization`.

No serving JVM was reloaded and no production click was sent. The rehearsal
runs in a throwaway JVM and temporary Git repository. It uses the real
`code-identity/load-file-recorded!`, real HTTP handler and runner-service click
boundary, and real operational-certificate implementation.

## Result: the chain is red at click closure

The rehearsal established, in order:

1. a clean canonical fixture runner reload records the exact fixture commit,
   clean state, and stable identity;
2. the real `POST /api/alpha/wm/click` handler accepts the click;
3. the loaded runner executes and writes the run record;
4. **the runner service remains `running? true` beyond its five-second close
   bound**, so the click lifecycle does not close;
5. the emitted run plus a matching clean resource produces a passing
   operational certificate;
6. a resource carrying a different run id produces a failing certificate.

The focused rehearsal exited 1: 1 test, 12 assertions, 2 failures. Both
failures describe the same seam: timeout waiting for click closure and absent
`:last-result`. The existing `futon3c.wm.runner-service-test` independently
reproduced the lifecycle failure in
`runner-resolution-failure-releases-single-flight` (2 failures). This is not a
fixture-only certificate issue and was not repaired inside the rehearsal.

The rehearsal is tagged `:slow`, so the ordinary suite does not acquire a
known-red diagnostic. Canonical explicit invocation:

```sh
clojure -M:test:test-all -i :slow -n futon3c.wm.chain-rehearsal-test
```

## What remains production-only

The rehearsal does not establish the serving JVM's current loaded namespace
set or `defonce` state, the authenticated Drawbridge reload transport, the
network listener at Agency `:7070`, live selector/author/reviewer dispatch, a
real bounded resource envelope around the click, or writes to the live trace
and evidence stores. Those remain properties of Joe's approved quiet-window
operation. In particular, downstream certificate success does not override
the unclosed click lifecycle; readiness must remain blocked until that seam is
understood.

## Bounded workspace gate

The required bounded run was
`bounded-1788217995689-c230-workspace-gate`. It completed rather than being
cancelled:

- inner exit: `1`;
- outer exit: `125`;
- resource status: `clean` (`pids.peak=64`, `pids.events:max delta=0`, no
  native-thread markers);
- outer reason: `repository-basis-changed` — Futon2 moved from
  `02a59bed…` to `baa9cac3…` during the run;
- gate summary: 78 checks, 77 executable, failures
  `q-interface-completeness` and `p4ng-referent-drift`.

Thus the bounded wrapper correctly declined to certify a gate whose repository
basis changed, independently of the gate's two red findings.
