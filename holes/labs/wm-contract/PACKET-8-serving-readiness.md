# Packet 8 serving META self-heal readiness

No-click serving composition: `click!` configures concrete repair ports before
Futon2 preflight.  The route is `production-readiness -> route! -> reset and
invoke wm-author -> reset and invoke wm-reviewer -> recorded source reload ->
close-click! -> queued attributable retry`.

Readiness is read-only and names the three-seat cast, canonical Futon2
repository, ledger/checkpoint paths, registry dispatch/reset, recorded reload,
and post-parent-release retry boundary.  Agency responses must be one typed
EDN map, with a real fresh session and job identity.  Reload accepts only a
reviewed patch already at clean canonical HEAD.  The retry id is minted before
enqueue and becomes both child click id and idempotency identity.

This does not authorize a click.  Live execution still needs ready seats, an
active durable injury, a reviewed canonical patch, and sanctioned reload.  The
post-release in-memory queue has no independent crash-recovery worker yet.
