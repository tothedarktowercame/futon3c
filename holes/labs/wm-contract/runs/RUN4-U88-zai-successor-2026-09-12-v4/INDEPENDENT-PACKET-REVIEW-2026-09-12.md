# Independent packet audit

Codex-17 executed a local read-only audit on 2026-09-12. All captured files had identical hashes at the end of the audit. No POST, live configuration access, task dispatch, or access to the running successor data root occurred.

| Item | Result |
| --- | --- |
| 1. Cohort → task → series → deployment digest chain | PASS |
| 2. Every task/series path+SHA reference | PASS: allowlisted, canonical containment, existing file, exact digest |
| 3. Casting | PASS: zai-2 / zai-1 / zai-1 in all four artifacts |
| 4. Historical link | PASS for retained installer against finding, verifier, historical projection and cohort bytes; installed JVM value NOT independently captured |
| 5. Successor series/trial/attempt | PASS: exact equality with the single frozen trial |

Computed SHA-256:

- Cohort: `2794a8699c133e34a3a4c387a789cc625d353d498327bd5466b9982163a42e21`
- Task: `4c0feb4a72ef5488f34de8827af7cad23272906725d8b37eba3e9e44746152ce`
- Series: `bd78b6de0ef7d00c613bb2a9e3797a9914abfb053c6dd5d9df3c0df2f8c3006e`
- Historical cohort: `ef390f48fbdc834302fece98a16eb9d79d59741646c44cdc00dc789e32de90c5`
- Finding: `7e9a8d45743cf759161f70b1d2a709e5866057dcd6387eb6e590c8759ca91048`
- Verifier: `81f671b14dd21168bb38b6a4190302cc55e7bc5aa3875c5189071371cc770b65`
- Historical projection: `35ca7e0db3ef0eac5f734bdea6295aac568d0f7bf0e925cf8bbf7cca68cfb982`

The historical projection and transition agree with the retained installer on the exact qualified execution, repair and verification IDs. Historical verifier authorship remains zai-5/zai-1; successor casting is independently zai-2/zai-1. These are different operations, not relabelled evidence.

The full path/hash inventory and link are in INDEPENDENT-PACKET-AUDIT-2026-09-12.edn. This is a packet/data identity audit, not a rerun of strict production readers, activation certification, or evidence of successor success. The installer was parsed as data and never executed. Its absence from the frozen template means live link installation still rests on the coordinator's installation evidence.
