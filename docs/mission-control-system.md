# Mission Control System Architecture

Two layers: what the stack **wants to be** (aspirational/ideal) and what it
**is** (empirical/actual). Mission Control sits between them, comparing and
feeding back. The reflexivity loop closes when MC's own output becomes part
of what it observes.

Reference: futon3 P11 — System Self-Description. "This closes the reflexivity
loop — a semantic network that includes a model of itself."

Psychological parallel: Higgins' self-discrepancy theory. Ideal self (devmaps)
vs actual self (evidence). The discrepancy generates corrective action.

See [War Bulletin 3](../../futon3/holes/war-bulletin-3.md) for the full
strategic assessment.

```mermaid
flowchart TB

    %% =============================================
    %% ASPIRATIONAL LAYER — what we want to be
    %% =============================================
    subgraph Ideal["Aspirational Self (Ideal)"]
        direction TB

        DEVMAPS["futon3 Devmaps<br/>F0–F7 × P0–P16<br/>prototypes, maturity,<br/>dependencies, success criteria"]
        WIRINGS["futon5 Wiring Diagrams<br/>11 exotypes<br/>ports, components, edges<br/>abstract structure"]
        MISSIONS_SPEC["Mission Specs<br/>M-*.md across repos<br/>objectives, completion criteria,<br/>blocked-by, derivation phase"]
        NONSTARTER["Nonstarter Proposals<br/>pre-registered intent<br/>ask, milestones,<br/>voting, mana budget"]

        DEVMAPS ---|"prototypes define<br/>what components exist"| WIRINGS
        WIRINGS ---|"exotypes constrain<br/>what missions are valid"| MISSIONS_SPEC
        NONSTARTER ---|"pre-registers<br/>incremental work"| MISSIONS_SPEC
    end

    %% =============================================
    %% EMPIRICAL LAYER — what we actually are
    %% =============================================
    subgraph Actual["Actual Self (Empirical)"]
        direction TB

        CODE["Code Across Repos<br/>futon{1a,3a,3b,3c,4,5,6}<br/>files, namespaces, functions"]
        TESTS["Test Results<br/>774 tests (futon3c)<br/>pass/fail/count per ns"]
        EVIDENCE["Evidence Store (XTDB)<br/>observations, patterns,<br/>PSR/PUR/PAR, gate traversals,<br/>blackboard snapshots"]
        AGENTS["Live Agents<br/>registry, heartbeats,<br/>IRC presence, invoke history"]
        COMMITS["Commit History<br/>recency, authorship,<br/>files touched, issues closed"]
    end

    %% =============================================
    %% MISSION CONTROL — the comparator
    %% =============================================
    subgraph MC["Mission Control Peripheral"]
        direction TB

        MC_INV["mc-inventory<br/>scan M-*.md + *.edn<br/>across all repos"]
        MC_DEV["mc-devmaps<br/>read wiring diagrams +<br/>prototype status"]
        MC_COV["mc-coverage<br/>which prototypes have<br/>missions? which don't?"]
        MC_MANA["mc-mana<br/>nonstarter pool state<br/>proposals, votes, funding"]
        MC_REVIEW["mc-review<br/>full portfolio synthesis"]
        MC_TICKLE["tickle<br/>scan/page/cycle<br/>stall detection"]
        MC_BULLETIN["mc-bulletin<br/>war room communiqué"]

        MC_INV --> MC_COV
        MC_DEV --> MC_COV
        MC_COV --> MC_REVIEW
        MC_MANA --> MC_REVIEW
        MC_TICKLE --> MC_REVIEW
    end

    %% =============================================
    %% READS — ideal into MC
    %% =============================================
    DEVMAPS -->|"prototype maturity<br/>+ success criteria"| MC_DEV
    WIRINGS -->|"structural validation<br/>ports, edges, components"| MC_DEV
    MISSIONS_SPEC -->|"mission status<br/>+ completion criteria"| MC_INV
    NONSTARTER -->|"pool balance<br/>active proposals"| MC_MANA

    %% =============================================
    %% READS — actual into MC
    %% =============================================
    EVIDENCE -->|"evidence counts<br/>per mission/prototype"| MC_REVIEW
    AGENTS -->|"agent activity<br/>last-active times"| MC_TICKLE
    TESTS -->|"pass/fail per namespace"| MC_COV
    COMMITS -->|"recency, staleness"| MC_COV
    CODE -->|"file existence<br/>confirms implementation"| MC_COV

    %% =============================================
    %% OUTPUTS — MC produces
    %% =============================================
    MC_REVIEW -->|"portfolio review<br/>(evidence entry)"| EVIDENCE
    MC_BULLETIN -->|"war bulletin<br/>(evidence entry)"| EVIDENCE
    MC_TICKLE -->|"page/escalate<br/>(evidence entry)"| EVIDENCE

    %% =============================================
    %% CORRECTIVE ACTIONS — discrepancy → work
    %% =============================================
    MC_COV -->|"gaps → new missions<br/>or nonstarter proposals"| MISSIONS_SPEC
    MC_COV -->|"staleness → demote<br/>prototype maturity"| DEVMAPS
    MC_REVIEW -->|"priorities → vote<br/>on proposals"| NONSTARTER
    MC_TICKLE -->|"stall → page agent<br/>or reassign"| AGENTS

    %% =============================================
    %% REFLEXIVITY LOOP — MC observes its own output
    %% =============================================
    EVIDENCE -.->|"MC reviews include<br/>prior MC reviews<br/>(reflexivity)"| MC_REVIEW

    %% =============================================
    %% Styles
    %% =============================================
    classDef ideal fill:#e8eaf6,stroke:#3f51b5
    classDef actual fill:#e8f5e9,stroke:#2e7d32
    classDef mc fill:#fff3e0,stroke:#ef6c00
    classDef action fill:#fce4ec,stroke:#c62828

    class DEVMAPS,WIRINGS,MISSIONS_SPEC,NONSTARTER ideal
    class CODE,TESTS,EVIDENCE,AGENTS,COMMITS actual
    class MC_INV,MC_DEV,MC_COV,MC_MANA,MC_REVIEW,MC_TICKLE,MC_BULLETIN mc
```
