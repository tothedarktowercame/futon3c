# Live-session ports recompute after intent and verify fixes

Read and run on 2026-10-11 UTC. The live-session set was frozen from `GET /api/alpha/agents` before record writes. Agency exposed 124 distinct current session IDs; 81 contained settled records and are listed below. The immutable 2,115-record before-state ledger was committed as `61cad13f`.

## Reload receipt

The three namespaces were reloaded from master through `scripts/proof-eval.sh`: `futon3c.logic.xiang`, `futon3c.xiang.turn-acts`, and `futon3c.xiang.turn-service`. The receipt was `{:agree-kind :approve, :verify-kind :verify, :verify-port? false}`. No JVM restart occurred.

## Results

All 2,115 frozen settled records were submitted to live `compute-ports!` with `{:session-cap 10000}`, in per-session `created_at` order, with request starts at least one second apart. 2,107 produced ports maps. Eight declared agent-origin Matrix reply records produced no acts and therefore no ports: `turn-IPuc7i`, `turn-xy0WXk`, `turn-wDJHml`, `turn-iezmrd`, `turn-CG3sw4`, `turn-T8Kkuu`, `turn-XthaXw`, `turn-X9T8gs`.

All eight records that had formerly refused because of `agree`/`challenge` were in live sessions and now succeeded: `turn-5JBr6g`, `turn-JTuY1v`, `turn-968NVf`, `turn-qX81HV`, `turn-WtIZCU`, `turn-mVytXT`, `turn-VgUJ8K`, `turn-ABYrM4`.

At the 71 tails having a measurable before map, total open ports changed from 2,188 to 2,248. Ten other tails had no before map; nine now contain 296 open ports, making the populated-tail after total 2,544. Agent-authored open verify ports fell from 144 before to **0 after** at all 80 populated tails. The remaining `codex-9` tail is itself a refused Matrix record, so it still has no ports map.

## Live sessions and tails

| Session | Agent(s) | Records | Tail open before | Tail open after | Tail agent verify after |
|---|---|---:|---:|---:|---:|
| `00e653e3-28f7-4add-8304-47ca0475b5c2` | claude-16 | 8 | 10 | 9 | 0 |
| `01a0f19d-767c-7843-bce4-dd428b65de9d` | codex-8 | 19 | — | 27 | 0 |
| `01a0f4ae-af56-7191-b610-291a1bcfaeed` | codex-10 | 155 | 57 | 154 | 0 |
| `01a0f574-8fc9-7981-a991-b4dd5e0f34da` | codex-11 | 36 | — | 35 | 0 |
| `01a0f991-b404-7c23-9715-f1396e76d4db` | codex-15 | 6 | — | 4 | 0 |
| `01a0fa8e-cb5c-7361-b267-4be544a002a6` | codex-17 | 25 | — | 21 | 0 |
| `01a0fcec-2528-7953-bce5-37fedc767dee` | codex-1 | 80 | — | 123 | 0 |
| `01a0fd8b-af80-7b41-87e8-1560e5bb6732` | codex-3 | 9 | — | 10 | 0 |
| `01a0fe0a-ecff-7b70-8f8e-ee9d8b329ebe` | codex-5 | 12 | — | 14 | 0 |
| `01a0fe38-6bc7-7160-832e-66b6d0b3c58b` | codex-7 | 3 | — | 2 | 0 |
| `01a0fee7-d1b6-7f01-b832-267d0af445b1` | codex-9 | 13 | — | — | — |
| `01a10492-1dbe-72d3-9707-0d21e7419572` | codex-21 | 1 | 1 | 0 | 0 |
| `01a10734-c514-7e73-9973-2d40a41855c0` | codex-19 | 152 | 50 | 152 | 0 |
| `01a10771-aad6-7cb1-b417-c77aae5b92ae` | codex-23 | 38 | 38 | 38 | 0 |
| `01a107d3-b1ca-7890-9f7b-3f22f2e228fb` | codex-24 | 11 | 11 | 11 | 0 |
| `01a11eb0-9bc4-7a51-9f6b-d8ab6f524485` | codex-36 | 15 | 15 | 15 | 0 |
| `01a11eb0-be5f-7010-8038-693f0eed7d24` | codex-37 | 1 | 1 | 1 | 0 |
| `01a11ee9-067c-7b43-95d5-6df5bbcac017` | codex-38 | 1 | 1 | 1 | 0 |
| `01a11ee9-15b4-73e1-90ec-7316de45a73e` | codex-39 | 5 | 5 | 5 | 0 |
| `01a11f1a-f79c-76b2-8be1-74f7c68a0e9d` | codex-40 | 2 | 2 | 2 | 0 |
| `01a11f1d-88f0-71b1-9425-743aade74ed7` | codex-41 | 4 | 4 | 4 | 0 |
| `01a11f1d-8b15-7d73-9d60-d8453cdfbba6` | codex-42 | 3 | 3 | 3 | 0 |
| `01a11f1d-c18d-7710-998b-55cdc139732d` | codex-43 | 6 | 6 | 6 | 0 |
| `01a11f1e-69f9-76f3-b647-738da227753c` | codex-44 | 5 | 5 | 5 | 0 |
| `01a11f1e-9fb6-7eb2-8b33-7ac817ae7ee4` | codex-45 | 1 | 1 | 1 | 0 |
| `01a11f1e-b57d-73c2-b997-05441444042f` | codex-46 | 6 | 6 | 6 | 0 |
| `01a11f1e-ea5b-74b3-a98e-627c9d549f1f` | codex-47 | 1 | 1 | 1 | 0 |
| `01a11f1f-06cd-7541-8588-5bb425b555f7` | codex-48 | 3 | 3 | 3 | 0 |
| `01a11f21-a14e-7800-82ac-fa2a033e1165` | codex-49 | 1 | 1 | 1 | 0 |
| `01a11f21-c873-78f0-bfac-9a764975f000` | codex-50 | 2 | 2 | 2 | 0 |
| `01a11f21-df01-7b20-8a26-e3f4c97a7853` | codex-51 | 2 | 2 | 2 | 0 |
| `01a11f22-0aa2-7163-b302-86ca3eb56b89` | codex-52 | 2 | 2 | 2 | 0 |
| `01a11f22-25ce-7920-bde7-8f07d2e58e0e` | codex-53 | 2 | 2 | 2 | 0 |
| `01a11f22-3b6c-7022-8996-da0aa487a61c` | codex-54 | 2 | 2 | 2 | 0 |
| `01a11f29-93e8-74b2-b32d-495274542854` | codex-55 | 4 | 4 | 4 | 0 |
| `01a11f33-ab55-70e3-b4cd-193e93ee48d8` | codex-56 | 29 | 29 | 29 | 0 |
| `01a11f33-c242-7b60-97ec-e941fecb92bf` | codex-57 | 4 | 4 | 4 | 0 |
| `01a11f33-ebd0-78e3-a9db-3c7109f22230` | codex-58 | 67 | 50 | 67 | 0 |
| `01a11f33-eefa-7770-9ce5-4b49aa2ea24e` | codex-59 | 17 | 17 | 17 | 0 |
| `01a11f34-dc89-70d2-90e5-ac6cc2df7570` | codex-60 | 35 | 35 | 35 | 0 |
| `01a11f34-fb26-79b0-bba3-2f67cd0d959e` | codex-61 | 10 | 10 | 10 | 0 |
| `01a11f35-1c23-7061-b0c6-7f4f51f1ba84` | codex-62 | 9 | 9 | 9 | 0 |
| `01a11f35-23db-7ac1-9a7b-9ea5fad1be4d` | codex-63 | 15 | 15 | 15 | 0 |
| `01a11f35-4934-7e43-8040-7aee331d6613` | codex-64 | 25 | 25 | 25 | 0 |
| `01a11f35-52b8-71c3-a75e-3604c32f293b` | codex-65 | 11 | 11 | 11 | 0 |
| `01a11f3a-c730-7d91-91a5-a10e81250b93` | codex-66 | 30 | 30 | 30 | 0 |
| `01a120d9-303a-7fd0-bb1d-b983df65855b` | codex-67 | 3 | 3 | 3 | 0 |
| `01a120e8-7071-7083-8169-9a7b62617897` | codex-28 | 9 | 9 | 9 | 0 |
| `01a120e8-7a37-7aa0-899b-982a123b81e1` | codex-29 | 13 | 13 | 13 | 0 |
| `01a120e8-8139-7641-b87f-d145eba5aff6` | codex-32 | 7 | 22 | 4 | 0 |
| `01a120e8-826d-7441-8ed3-1b1d6d778de7` | codex-33 | 3 | 6 | 2 | 0 |
| `01a120e9-558e-7732-8948-5a5c067514cb` | codex-31 | 5 | 5 | 5 | 0 |
| `01a120e9-57b5-71f3-9907-9db45a26fefd` | codex-30 | 25 | 25 | 25 | 0 |
| `01a120f7-bee6-79a2-a8df-21eb5346af8a` | codex-34 | 11 | 11 | 11 | 0 |
| `01a12124-6e1e-7e93-b7c6-2e08790a7d75` | codex-35 | 7 | 7 | 7 | 0 |
| `01a123e4-5da0-7130-bcc2-d8be5a21d32c` | codex-68 | 48 | 43 | 51 | 0 |
| `01a126a8-c457-7660-bcf6-a5ffbb3c5460` | codex-69 | 29 | 35 | 25 | 0 |
| `01a128ee-8003-7791-8af2-32028cb9e369` | codex-70 | 11 | 11 | 11 | 0 |
| `01a1294b-dec3-78b0-9f4f-74dcc5f0c7d0` | codex-71 | 13 | 13 | 13 | 0 |
| `01a12a53-7e37-7cf3-95f1-1cf6b82b8338` | codex-72 | 2 | 5 | 5 | 0 |
| `01a12a7b-9e52-7460-8c43-f39ea3298903` | codex-73 | 1 | 1 | 1 | 0 |
| `1b5d271d-f4b6-4c74-891a-55d34db56ffe` | claude-7 | 3 | 1 | 1 | 0 |
| `40369bd2-11a6-47b9-a440-f319b68bbbf8` | claude-2 | 51 | 144 | 106 | 0 |
| `49d366f7-dd42-4437-8b80-72ce9baf2e39` | claude-3 | 8 | 11 | 11 | 0 |
| `564c8e50-c240-46fc-81ad-55afafbc63ee` | claude-17 | 371 | 660 | 612 | 0 |
| `5a109aed-1c56-4c99-9876-b5c2d6aac003` | claude-12 | 57 | 40 | 40 | 0 |
| `66c595ba-5098-4ba8-8ae4-def07343f76a` | claude-18 | 114 | 131 | 131 | 0 |
| `75b00893-43cc-4172-917a-06a591193b20` | claude-14 | 22 | 39 | 29 | 0 |
| `9ef8e479-e1b7-4755-894d-1264cacb6c3b` | claude-4 | 199 | 273 | 248 | 0 |
| `a89c3e64-2a65-4a00-ac82-3ec1ddfd5ba3` | claude-11 | 109 | 108 | 106 | 0 |
| `ae883f3a-6164-4404-834e-e89a0381ce6a` | claude-13 | 36 | — | 60 | 0 |
| `b52510f9-275c-44e1-a74f-243df3750115` | claude-5 | 3 | 1 | 1 | 0 |
| `d710876a-82cc-4301-b628-3896ab13516b` | claude-6 | 9 | 13 | 13 | 0 |
| `d9e4516f-e601-405f-a623-0dc31d6b7a73` | claude-9 | 1 | 3 | 3 | 0 |
| `ec3a9b31-6656-4530-90db-53a22762c17f` | claude-15 | 29 | 58 | 51 | 0 |
| `kimi-e545a8ea-412c-499e-b495-2a421d67bd2b` | kimi-1 | 1 | 0 | 0 | 0 |
| `zai-52828332-789d-4541-bed7-5e9bacb1f465` | zai-3 | 7 | 7 | 7 | 0 |
| `zai-72e43dfb-37ad-40a9-a3f0-3e6cc0a981da` | zai-5 | 3 | 3 | 3 | 0 |
| `zai-ab07af6b-0c40-4cd9-9504-6083477f0144` | zai-4 | 4 | 4 | 4 | 0 |
| `zai-c02b13ca-b1db-4357-8f76-b6f9382b7a1f` | zai-1 | 5 | 5 | 5 | 0 |
| `zai-d19c3168-7064-4b54-8279-e1bf18d646de` | zai-2 | 8 | 8 | 8 | 0 |

## Records whose open count grew

152 records grew. Each line below gives the observed delta. The common supported explanation is that this replay recomputed the complete session at cap 10,000 under the current act rules, whereas the stored before map was produced from an older or shorter session snapshot; the ledger does not retain enough act-level before detail to attribute individual added ports more narrowly.

- `turn-eBByja`: 56 → 147 (+91); full-session/current-rule replay exposed that many net additional open ports.
- `turn-5kDAdY`: 57 → 148 (+91); full-session/current-rule replay exposed that many net additional open ports.
- `turn-TV2AbS`: 57 → 149 (+92); full-session/current-rule replay exposed that many net additional open ports.
- `turn-RM0x44`: 57 → 150 (+93); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ybzr6x`: 57 → 151 (+94); full-session/current-rule replay exposed that many net additional open ports.
- `turn-dAaMvg`: 57 → 152 (+95); full-session/current-rule replay exposed that many net additional open ports.
- `turn-bWJ449`: 57 → 153 (+96); full-session/current-rule replay exposed that many net additional open ports.
- `turn-fEHcns`: 57 → 154 (+97); full-session/current-rule replay exposed that many net additional open ports.
- `turn-bYoKSE`: 50 → 51 (+1); full-session/current-rule replay exposed that many net additional open ports.
- `turn-kqgLLv`: 50 → 52 (+2); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ABZdYl`: 50 → 53 (+3); full-session/current-rule replay exposed that many net additional open ports.
- `turn-oKV8Yr`: 50 → 54 (+4); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ZO2M1L`: 50 → 55 (+5); full-session/current-rule replay exposed that many net additional open ports.
- `turn-UvEHhI`: 50 → 56 (+6); full-session/current-rule replay exposed that many net additional open ports.
- `turn-LTkvbI`: 50 → 57 (+7); full-session/current-rule replay exposed that many net additional open ports.
- `turn-sTHBxk`: 50 → 58 (+8); full-session/current-rule replay exposed that many net additional open ports.
- `turn-bFLF1Y`: 50 → 59 (+9); full-session/current-rule replay exposed that many net additional open ports.
- `turn-O5SOTs`: 50 → 60 (+10); full-session/current-rule replay exposed that many net additional open ports.
- `turn-7VleUe`: 50 → 61 (+11); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ZohfXW`: 50 → 62 (+12); full-session/current-rule replay exposed that many net additional open ports.
- `turn-feK1Q8`: 50 → 63 (+13); full-session/current-rule replay exposed that many net additional open ports.
- `turn-HnYbj5`: 50 → 64 (+14); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ik0j46`: 50 → 65 (+15); full-session/current-rule replay exposed that many net additional open ports.
- `turn-fN5MaG`: 50 → 66 (+16); full-session/current-rule replay exposed that many net additional open ports.
- `turn-zsKw9o`: 50 → 67 (+17); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Ks7gLZ`: 50 → 68 (+18); full-session/current-rule replay exposed that many net additional open ports.
- `turn-mSwTdo`: 50 → 69 (+19); full-session/current-rule replay exposed that many net additional open ports.
- `turn-zJ1zX4`: 50 → 70 (+20); full-session/current-rule replay exposed that many net additional open ports.
- `turn-5Pf10Y`: 50 → 71 (+21); full-session/current-rule replay exposed that many net additional open ports.
- `turn-BNPH9x`: 50 → 72 (+22); full-session/current-rule replay exposed that many net additional open ports.
- `turn-dnUyxz`: 50 → 73 (+23); full-session/current-rule replay exposed that many net additional open ports.
- `turn-OieNxV`: 50 → 74 (+24); full-session/current-rule replay exposed that many net additional open ports.
- `turn-fjsi0I`: 50 → 75 (+25); full-session/current-rule replay exposed that many net additional open ports.
- `turn-7Lxp4z`: 50 → 76 (+26); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Q4vEBZ`: 50 → 77 (+27); full-session/current-rule replay exposed that many net additional open ports.
- `turn-WsjFjU`: 50 → 78 (+28); full-session/current-rule replay exposed that many net additional open ports.
- `turn-GFJmMA`: 50 → 79 (+29); full-session/current-rule replay exposed that many net additional open ports.
- `turn-savVE8`: 50 → 80 (+30); full-session/current-rule replay exposed that many net additional open ports.
- `turn-aGEys3`: 50 → 81 (+31); full-session/current-rule replay exposed that many net additional open ports.
- `turn-H3wYFw`: 50 → 82 (+32); full-session/current-rule replay exposed that many net additional open ports.
- `turn-QsXMoF`: 50 → 83 (+33); full-session/current-rule replay exposed that many net additional open ports.
- `turn-sORj67`: 50 → 84 (+34); full-session/current-rule replay exposed that many net additional open ports.
- `turn-le85pZ`: 50 → 85 (+35); full-session/current-rule replay exposed that many net additional open ports.
- `turn-0OHNNw`: 50 → 86 (+36); full-session/current-rule replay exposed that many net additional open ports.
- `turn-05ZTvU`: 50 → 87 (+37); full-session/current-rule replay exposed that many net additional open ports.
- `turn-uceASm`: 50 → 88 (+38); full-session/current-rule replay exposed that many net additional open ports.
- `turn-siRUrw`: 50 → 89 (+39); full-session/current-rule replay exposed that many net additional open ports.
- `turn-afGFM7`: 50 → 90 (+40); full-session/current-rule replay exposed that many net additional open ports.
- `turn-5PxwLL`: 50 → 91 (+41); full-session/current-rule replay exposed that many net additional open ports.
- `turn-6AMtJ5`: 50 → 92 (+42); full-session/current-rule replay exposed that many net additional open ports.
- `turn-yR8VmV`: 50 → 93 (+43); full-session/current-rule replay exposed that many net additional open ports.
- `turn-NRZaZR`: 50 → 94 (+44); full-session/current-rule replay exposed that many net additional open ports.
- `turn-xwbJBc`: 50 → 95 (+45); full-session/current-rule replay exposed that many net additional open ports.
- `turn-7zfU3y`: 50 → 96 (+46); full-session/current-rule replay exposed that many net additional open ports.
- `turn-GoAf7W`: 50 → 97 (+47); full-session/current-rule replay exposed that many net additional open ports.
- `turn-AHE6wi`: 50 → 98 (+48); full-session/current-rule replay exposed that many net additional open ports.
- `turn-EzfGaN`: 50 → 99 (+49); full-session/current-rule replay exposed that many net additional open ports.
- `turn-kxZ9f2`: 50 → 100 (+50); full-session/current-rule replay exposed that many net additional open ports.
- `turn-VYPQCQ`: 50 → 101 (+51); full-session/current-rule replay exposed that many net additional open ports.
- `turn-OdCDi2`: 50 → 102 (+52); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Irewhf`: 50 → 103 (+53); full-session/current-rule replay exposed that many net additional open ports.
- `turn-rm7u6x`: 50 → 104 (+54); full-session/current-rule replay exposed that many net additional open ports.
- `turn-oNTlGD`: 50 → 105 (+55); full-session/current-rule replay exposed that many net additional open ports.
- `turn-48KdIB`: 50 → 106 (+56); full-session/current-rule replay exposed that many net additional open ports.
- `turn-egp0Y0`: 50 → 107 (+57); full-session/current-rule replay exposed that many net additional open ports.
- `turn-WVLqlv`: 50 → 108 (+58); full-session/current-rule replay exposed that many net additional open ports.
- `turn-dqtMEK`: 50 → 109 (+59); full-session/current-rule replay exposed that many net additional open ports.
- `turn-TTgld0`: 50 → 110 (+60); full-session/current-rule replay exposed that many net additional open ports.
- `turn-fs8POe`: 50 → 111 (+61); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Fh9CiQ`: 50 → 112 (+62); full-session/current-rule replay exposed that many net additional open ports.
- `turn-AdDkmy`: 50 → 113 (+63); full-session/current-rule replay exposed that many net additional open ports.
- `turn-OrLyzO`: 50 → 114 (+64); full-session/current-rule replay exposed that many net additional open ports.
- `turn-d7t9PH`: 50 → 115 (+65); full-session/current-rule replay exposed that many net additional open ports.
- `turn-hjgLOf`: 50 → 116 (+66); full-session/current-rule replay exposed that many net additional open ports.
- `turn-EDuAIl`: 50 → 117 (+67); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ZxYT0k`: 50 → 118 (+68); full-session/current-rule replay exposed that many net additional open ports.
- `turn-u9W64p`: 50 → 119 (+69); full-session/current-rule replay exposed that many net additional open ports.
- `turn-8TaxLJ`: 50 → 120 (+70); full-session/current-rule replay exposed that many net additional open ports.
- `turn-WKmNE9`: 50 → 121 (+71); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Yyr9zH`: 50 → 122 (+72); full-session/current-rule replay exposed that many net additional open ports.
- `turn-EKlTfn`: 50 → 123 (+73); full-session/current-rule replay exposed that many net additional open ports.
- `turn-wF1NpR`: 50 → 124 (+74); full-session/current-rule replay exposed that many net additional open ports.
- `turn-aj1296`: 50 → 125 (+75); full-session/current-rule replay exposed that many net additional open ports.
- `turn-JLInlB`: 50 → 126 (+76); full-session/current-rule replay exposed that many net additional open ports.
- `turn-oDPgCf`: 50 → 127 (+77); full-session/current-rule replay exposed that many net additional open ports.
- `turn-2WgrGo`: 50 → 128 (+78); full-session/current-rule replay exposed that many net additional open ports.
- `turn-c3cQOH`: 50 → 129 (+79); full-session/current-rule replay exposed that many net additional open ports.
- `turn-5F08JI`: 50 → 130 (+80); full-session/current-rule replay exposed that many net additional open ports.
- `turn-9fmVhI`: 50 → 131 (+81); full-session/current-rule replay exposed that many net additional open ports.
- `turn-yc2zLd`: 50 → 132 (+82); full-session/current-rule replay exposed that many net additional open ports.
- `turn-lNo0cp`: 50 → 133 (+83); full-session/current-rule replay exposed that many net additional open ports.
- `turn-iCgECc`: 50 → 134 (+84); full-session/current-rule replay exposed that many net additional open ports.
- `turn-f93fOo`: 50 → 135 (+85); full-session/current-rule replay exposed that many net additional open ports.
- `turn-nv9cBU`: 50 → 136 (+86); full-session/current-rule replay exposed that many net additional open ports.
- `turn-70le1O`: 50 → 137 (+87); full-session/current-rule replay exposed that many net additional open ports.
- `turn-RLy1V4`: 50 → 138 (+88); full-session/current-rule replay exposed that many net additional open ports.
- `turn-VGdWpd`: 50 → 139 (+89); full-session/current-rule replay exposed that many net additional open ports.
- `turn-RzuRns`: 50 → 140 (+90); full-session/current-rule replay exposed that many net additional open ports.
- `turn-dJBweH`: 50 → 141 (+91); full-session/current-rule replay exposed that many net additional open ports.
- `turn-MY9ixu`: 50 → 142 (+92); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Y1fMCg`: 50 → 143 (+93); full-session/current-rule replay exposed that many net additional open ports.
- `turn-0eIDrn`: 50 → 144 (+94); full-session/current-rule replay exposed that many net additional open ports.
- `turn-WRrOfr`: 50 → 145 (+95); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ksIGgq`: 50 → 146 (+96); full-session/current-rule replay exposed that many net additional open ports.
- `turn-tDKUCq`: 50 → 147 (+97); full-session/current-rule replay exposed that many net additional open ports.
- `turn-UJJo2e`: 50 → 148 (+98); full-session/current-rule replay exposed that many net additional open ports.
- `turn-8oBMbf`: 50 → 149 (+99); full-session/current-rule replay exposed that many net additional open ports.
- `turn-1NttAd`: 50 → 150 (+100); full-session/current-rule replay exposed that many net additional open ports.
- `turn-r6QWkX`: 50 → 151 (+101); full-session/current-rule replay exposed that many net additional open ports.
- `turn-5TsgZL`: 50 → 152 (+102); full-session/current-rule replay exposed that many net additional open ports.
- `turn-pKN6uX`: 50 → 51 (+1); full-session/current-rule replay exposed that many net additional open ports.
- `turn-a1dzGl`: 50 → 52 (+2); full-session/current-rule replay exposed that many net additional open ports.
- `turn-uJ9AxU`: 50 → 53 (+3); full-session/current-rule replay exposed that many net additional open ports.
- `turn-tsuhVa`: 50 → 54 (+4); full-session/current-rule replay exposed that many net additional open ports.
- `turn-DSFVMu`: 50 → 55 (+5); full-session/current-rule replay exposed that many net additional open ports.
- `turn-MztHui`: 50 → 56 (+6); full-session/current-rule replay exposed that many net additional open ports.
- `turn-hZoZuQ`: 50 → 57 (+7); full-session/current-rule replay exposed that many net additional open ports.
- `turn-yTvtDE`: 50 → 58 (+8); full-session/current-rule replay exposed that many net additional open ports.
- `turn-hd7BZk`: 50 → 59 (+9); full-session/current-rule replay exposed that many net additional open ports.
- `turn-0muIa2`: 50 → 60 (+10); full-session/current-rule replay exposed that many net additional open ports.
- `turn-noBl5Y`: 50 → 61 (+11); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ZcA0O6`: 50 → 62 (+12); full-session/current-rule replay exposed that many net additional open ports.
- `turn-vac7le`: 50 → 63 (+13); full-session/current-rule replay exposed that many net additional open ports.
- `turn-UaSjco`: 50 → 64 (+14); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Kdgutg`: 50 → 65 (+15); full-session/current-rule replay exposed that many net additional open ports.
- `turn-VhF5x2`: 50 → 66 (+16); full-session/current-rule replay exposed that many net additional open ports.
- `turn-l9JMgV`: 50 → 67 (+17); full-session/current-rule replay exposed that many net additional open ports.
- `turn-aHHE4j`: 46 → 51 (+5); full-session/current-rule replay exposed that many net additional open ports.
- `turn-E5TWrB`: 45 → 51 (+6); full-session/current-rule replay exposed that many net additional open ports.
- `turn-xZaq0J`: 43 → 51 (+8); full-session/current-rule replay exposed that many net additional open ports.
- `turn-cLvEst`: 43 → 51 (+8); full-session/current-rule replay exposed that many net additional open ports.
- `turn-IAHmhQ`: 52 → 68 (+16); full-session/current-rule replay exposed that many net additional open ports.
- `turn-RkES1B`: 52 → 69 (+17); full-session/current-rule replay exposed that many net additional open ports.
- `turn-HbPeHM`: 52 → 70 (+18); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Wy7JFb`: 53 → 71 (+18); full-session/current-rule replay exposed that many net additional open ports.
- `turn-dOy68q`: 54 → 72 (+18); full-session/current-rule replay exposed that many net additional open ports.
- `turn-X108xu`: 50 → 74 (+24); full-session/current-rule replay exposed that many net additional open ports.
- `turn-oHezvH`: 51 → 75 (+24); full-session/current-rule replay exposed that many net additional open ports.
- `turn-UnO2g1`: 49 → 79 (+30); full-session/current-rule replay exposed that many net additional open ports.
- `turn-cbDzdp`: 49 → 80 (+31); full-session/current-rule replay exposed that many net additional open ports.
- `turn-zdQBIp`: 42 → 82 (+40); full-session/current-rule replay exposed that many net additional open ports.
- `turn-pattxp`: 42 → 83 (+41); full-session/current-rule replay exposed that many net additional open ports.
- `turn-1B7b9Z`: 42 → 84 (+42); full-session/current-rule replay exposed that many net additional open ports.
- `turn-uUoVva`: 42 → 85 (+43); full-session/current-rule replay exposed that many net additional open ports.
- `turn-LKRLaB`: 43 → 86 (+43); full-session/current-rule replay exposed that many net additional open ports.
- `turn-pAQpIc`: 44 → 87 (+43); full-session/current-rule replay exposed that many net additional open ports.
- `turn-M028kE`: 44 → 88 (+44); full-session/current-rule replay exposed that many net additional open ports.
- `turn-3JJUTK`: 42 → 89 (+47); full-session/current-rule replay exposed that many net additional open ports.
- `turn-o8JkNa`: 42 → 93 (+51); full-session/current-rule replay exposed that many net additional open ports.
- `turn-Bn2jWE`: 41 → 94 (+53); full-session/current-rule replay exposed that many net additional open ports.
- `turn-ibPznT`: 41 → 95 (+54); full-session/current-rule replay exposed that many net additional open ports.
- `turn-EOuiBy`: 13 → 21 (+8); full-session/current-rule replay exposed that many net additional open ports.

## Refusals

`turn-IPuc7i`, `turn-xy0WXk`, `turn-wDJHml`, `turn-iezmrd`, `turn-CG3sw4`, `turn-T8Kkuu`, `turn-XthaXw`, and `turn-X9T8gs` are `declared`, `origin: agent` Matrix reply records with neither a reading nor `reply_text`. They therefore contribute no fragment or reply acts. This is separate from the repaired `agree`/`challenge` path.
