# T-matrix-federation-proof — cross-server proof handshake with Rob

**Status: DEFERRED / parked 2026-06-10.** Standalone follow-on — **NOT** part of
M-agency-hardening. Revisit when Joe is ready.

## Goal

Join Rob's private Matrix federation proof room, send one short test message,
Rob replies — confirming the first bounded cross-server exchange.

## Details (from Rob's email)

- Homeserver: `into-the-matrix.my-familiar.com`
- Proof room alias:
  `#private-federation-proof-20260422-191702:into-the-matrix.my-familiar.com`
- Rob: "Join that room from your own homeserver/account during our proof window.
  Once you're in, send one short test message and I'll reply so we can confirm
  the first bounded cross-server exchange."

## Why it couldn't be done from this host (probed 2026-06-10)

1. **No Matrix identity here.** No Matrix account, access token, client config,
   or tooling on this machine (no `MATRIX_*` env, no Element/gomuks/
   matrix-commander, no `matrix-nio`). Joining a private room + sending a
   message both require an authenticated account.
2. **Homeserver blocks this host.** `into-the-matrix.my-familiar.com` is behind
   Cloudflare, which returns "Sorry, you have been blocked" even for
   unauthenticated discovery endpoints (`.well-known/matrix/*`,
   `/_matrix/client/versions`) from this box's IP.

## Candidate paths to revisit (Joe's notes)

- Run the handshake from the **Linode** (different network/IP — may not be
  CF-blocked), or
- **Talk to Rob about permissions/access** (allowlisting, or he provisions an
  account for Joe on his homeserver), and/or
- Simplest: do the handshake from a **normal Matrix client (Element) signed into
  Joe's own account** on his usual network — sidesteps both the credential gap
  and the Cloudflare block. This is the tool Rob assumed; it's a manual step,
  not a futon task.

## If "agents on Matrix" is later wanted (bigger build)

Mirror the IRC bridge: a `matrix_bridge.py` analogous to
`scripts/ngircd_bridge.py`, plus `agency/federation.clj` patterns; env like
`MATRIX_HOMESERVER_URL` / bot creds / `MATRIX_ROOMS`. **First clarify what Matrix
buys over the existing `#zabuton` WS link** — Rob's agents (`zclaude-1`,
`zcodex-1`) already federate into Agency over WebSocket today (see
`README-zabuton.md`, `README-math.md`).
