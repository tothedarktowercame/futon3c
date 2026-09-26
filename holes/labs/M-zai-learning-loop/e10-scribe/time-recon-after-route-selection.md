# Memory: time reconnaissance after route selection

- Requested memory level: `process`
- Lane: desk-research
- Confidence: one paired phase-A/phase-B session (`n=1`)
- Problem: `a96J08`
- Git commit: `37192e165973a5280114a45ca2b66bf8429e4c37`
- Jobs: `invoke-1785772892476-949-1c6b0d34` (phase A),
  `invoke-1785773411377-951-bce95078` (phase B)
- Evidence IDs: `e-d149bd0c-10b4-4fd5-8759-dfb9fb84991f`,
  `e-249fac6b-f72a-467a-b816-9a0f6c2aae6b`,
  `e-5475c4e0-c87e-4ca4-8c4a-7905bc5eaedb`

## Memory

Reconnaissance performed before route selection tends to match the problem's
surface vocabulary, while the useful dependency vocabulary appears only
after a proof route meets the library.

In a96J08, phase A surfaced six honestly plausible hints from the problem's
prose: circle-pole separation, two Dirichlet/Abel tail patterns, monotone
antiderivative integrability, open-set interval decomposition, and exponential
domination. Phase B selected a rectangular-contour period-shift route and
correctly discarded all six: circular contours did not transfer to the
required rectangle; tail estimates were downstream of the residue blocker;
the remaining hints addressed different machinery. The event-anchored lookup
at the actual blocker used the terms `residue theorem`, `rectangle`, `pole`,
and `Cauchy-Goursat`, and located the exact zero-residue boundary.

## Application rule

Keep cheap pre-route recon for orientation, but repeat a bounded lookup after
choosing a route and whenever an API search first returns empty. Treat the
second lookup as load-bearing: its query should name the failed bridge, not
merely the informal theorem's topic. Record pre-route hints as discarded when
their proof objects do not match; do not force citations to justify recon.

## Honesty boundary

This is a timing lesson from one paired session (`n=1`), not evidence against
pre-route reconnaissance. Phase A was useful for coverage measurement and its
six discards were correct; the claim is only that event-anchored lookup carried
the route-specific consultation load in this instance.
