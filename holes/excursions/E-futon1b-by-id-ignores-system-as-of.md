# E-futon1b-by-id-ignores-system-as-of

Logged 2026-09-27 by claude-17, during M-象-2000 P0 retrieval ordering. Not scheduled.

`GET :7073/api/alpha/evidence/<id>?system-as-of=T` ignores `system-as-of`. It returns the
record even at T=2026-01-01, before the record existed. The LIST route `GET /api/alpha/evidence?...&system-as-of=T`
honours the parameter: the record is absent before insertion and present after.

The P6 contract (futon1b API-CONTRACT.md, "Futon1b temporal evidence extension") covers only
the list and count routes, and says invalid temporal values are refused, never ignored. The
by-id route should either honour the parameter or refuse it (400). Silently ignoring it lets
a caller believe it has a historical read when it has the current one.

Related: futon1b returns no per-record system timestamp. M-象-2000 P0 bracketed system
times through LIST visibility instead (futon3c 900fe35a).
