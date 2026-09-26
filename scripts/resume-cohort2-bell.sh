#!/bin/bash
# One-shot: fired by systemd timer 2026-08-01 10:05, post weekly-quota reset.
cd /home/joe/code/futon3c
python3 scripts/agency_send.py --from ground-control --to claude-4 --kind bell --timeout-ms 600000 <<'MSG'
SCHEDULED RESUMPTION (2026-08-01, weekly quota reset): resume cohort 2.
Read futon3c/holes/labs/M-zai-learning-loop/cohort-2-RESUME.md and execute it
top to bottom: verify seats + store, dispatch S3 (a96J01, packet verbatim in
the doc), park, then S6 (a96A04 finale, the Psi-weighted surfacing test),
then cohort-close report with meta-meters. Trust git over memory.
MSG
