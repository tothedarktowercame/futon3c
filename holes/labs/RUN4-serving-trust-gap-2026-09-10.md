# RUN4 serving trust boundary: concrete gap

Status: **BLOCKED — NO IMPLEMENTATION, NO LAUNCH**

## Finding

The Agency HTTP service on port 7070 has no authenticated operator context
that can securely authorize a RUN4 task pin. Consequently the accepted Futon2
callback contract cannot yet be supplied by `futon3c.wm.runner-service`.
Treating localhost, JSON fields, an Agency agent identity, or a constant
callback as Joe's authority would create the exact bypass the runner now
refuses.

## Existing paths inspected

- `futon3c.transport.http/handle-wm-click-start` parses arbitrary JSON and
  copies only author, reviewer, repair-reviewer, and trigger into `click!`.
  It receives no verified principal or authorization object
  (`src/futon3c/transport/http.clj:8221-8247`). The route is mounted directly
  at POST `/api/alpha/wm/click` (`src/futon3c/transport/http.clj:8491-8492`).
- `futon3c.wm.runner-service/run-click!` passes configured options to the
  Futon2 runner and adds only click, phase-log, and strategic-selection
  functions (`src/futon3c/wm/runner_service.clj:377-396`). Its CAS in
  `click!` remains the correct single-flight authority
  (`src/futon3c/wm/runner_service.clj:416-446`).
- `futon3c.social.authenticate/resolve-identity` converts an already verified
  PresenceRecord into an agent identity; it does not authenticate an HTTP
  request or establish operator authority
  (`src/futon3c/social/authenticate.clj:33-82`).
- Typed role-submission tokens are controller-minted capabilities for a
  preregistered APM job/role/agent envelope, not operator credentials
  (`src/futon3c/apm/typed_role_submission.clj:1-6,70-86`). Reusing one would
  authorize the wrong principal and scope.
- Drawbridge on port 6768 has a token plus IP allowlist wrapper
  (`src/repl/http.clj:37-55,270-284`). That authentication terminates in a
  separate development/admin server and does not place a principal or
  delegation claim into Agency HTTP requests. Merely copying its token into
  `/api/alpha/wm/click` would not provide scoped RUN4 authority or delegation.

## Smallest secure configuration contract

Before RUN4 serving integration, the 7070 server needs a server-owned operator
authentication boundary with all of these properties:

1. Startup configuration supplies a non-default secret credential and its
   fixed principal, `Joe`; startup refuses an absent or placeholder secret.
   The secret is never accepted from route JSON, logged, or persisted.
2. Middleware validates the credential in constant time and attaches an
   internal, non-serializable authentication context to the Ring request. The
   context includes principal, authentication scheme, authentication time,
   and a server-minted request nonce.
3. Optional delegation is an explicit server-side allowlist from the Joe
   principal to a named delegate and the single `wm/run4-click` capability.
   Agency registration or generic agent authentication alone grants nothing.
4. The click handler accepts only a task-pin document/reference from JSON. It
   obtains source-reading and mission/admissibility ports from server code,
   validates the exact pin, and compares the declared operator/delegate with
   the authenticated context.
5. The server derives the runner callback as a closure over that context and
   exact pin SHA-256. Its attestation is valid for only that digest and request
   nonce. A different digest, principal, delegation, or replayed nonce is
   rejected before `runner-service/click!` and before the single-flight worker
   is created.
6. Raw JSON keys resembling callbacks, ports, authentication flags, principal
   claims, or attestations are rejected. Legacy requests with no RUN4 pin
   remain byte/value-equivalent to the current click options.

The next implementation choice is therefore which established server
credential authority should issue the scoped 7070 operator context. No current
component does so. Once Joe selects/configures that authority, focused handler
tests must prove absent, forged, mismatched, and cross-pin replay refusals plus
legacy click identity and single-flight preservation. Until then there is no
honest callback to wire.

## Read-only verification

No endpoint, runner, lock, registry, data file, or serving JVM was invoked or
modified. This finding was derived by source inspection only.
