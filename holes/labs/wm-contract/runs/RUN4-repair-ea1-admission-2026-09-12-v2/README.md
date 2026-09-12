# ea1 artifact-binding admission — v2

Second packet for the ea1 historical repair-verification admission. v1
(RUN4-repair-ea1-admission-2026-09-12) was started 2026-09-12 12:45 and its
single attempt closed :agent-unavailable / :not-reached-selection, because
the cast author zai-5 was mid-invoke while driving the install — the
selection window requires the cast actors idle. The v1 packet is retained
untouched as the record; this v2 packet is identical except ids/shas and the
fresh cohort at /home/joe/run4/ea1-admission-20260912-v2.

Verification consumed: repair-ea1-artifact-binding-revalidation-20260912-v1
(offline-verification, sha256 81f671b14dd21168bb38b6a4190302cc55e7bc5aa3875c5189071371cc770b65).

Casting: author zai-5 (qualification author), reviewer zai-1, repair-reviewer
zai-1. Historical admission performs no author/reviewer dispatch; a distinct
production successor remains required.

## Live trigger (must run while zai-5 AND zai-1 are idle)

```sh
TOKEN=$(cat /home/joe/.config/futon3c/run4/u88.bearer)
curl -s --max-time 280 -X POST http://127.0.0.1:7070/api/alpha/wm/run4/series/step \
  -H 'Content-Type: application/json' -H "Authorization: Bearer $TOKEN" \
  -d '{"run4-series-ref":"holes/labs/wm-contract/runs/RUN4-repair-ea1-admission-2026-09-12-v2/series-pin.edn"}'
```

Expected: trial-started, then after ~3-5 min a second identical POST returns
the terminal inspection (the click publishes binding/projection/run-record
under /home/joe/run4/ea1-admission-20260912-v2/). Verify with
GET /api/alpha/wm/click (outcome should be
:historical-verification-awaiting-validation, binding :verified).
