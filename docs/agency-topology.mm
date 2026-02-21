flowchart TB
    %% =============================================
    %% Linode Server JVM
    %% =============================================
    subgraph Linode["Linode Server JVM (FUTON3C_ROLE=linode)"]
        direction TB

        RAGENCY["Agency<br/>HTTP+WS :7070<br/>/api/alpha/invoke<br/>/api/alpha/agents<br/>/agency/ws"]
        RIRC["IRC Server :6667<br/>dispatch relay"]
        RBRIDGE["Drawbridge :6768<br/>/repl + /eval"]
        REVIDENCE["futon1a XTDB :7071<br/>evidence store"]

        subgraph RAgents["Registered Agents"]
            RCLAUDE["claude-1<br/>inline invoke-fn<br/>claude -p --resume"]
            RCODEX_SLOT["codex-1<br/>(ws-bridge slot)"]
        end

        RAGENCY -->|"invoke-agent!"| RCLAUDE
        RAGENCY -->|"WS invoke frame"| RCODEX_SLOT
        RIRC -->|"@claude dispatch"| RAGENCY
        RIRC -->|"@codex dispatch"| RAGENCY
        RCLAUDE -->|"evidence: start/heartbeat/complete"| REVIDENCE
    end

    %% =============================================
    %% Laptop JVM
    %% =============================================
    subgraph Laptop["Laptop JVM (FUTON3C_ROLE=laptop)"]
        direction TB

        LAGENCY["Agency<br/>HTTP+WS :7070"]
        LBRIDGE["Drawbridge :6768<br/>/repl + /eval"]
        LEVIDENCE["futon1a XTDB :7071<br/>evidence store"]

        subgraph LAgents["Codex Bridge"]
            LCODEX["codex-1<br/>inline invoke-fn<br/>codex exec --json"]
            LWS_BRIDGE["WS Bridge<br/>(outbound to Linode)"]
        end

        LCODEX -->|"evidence: start/heartbeat/complete"| LEVIDENCE
        LWS_BRIDGE -->|"invoke frame → codex exec"| LCODEX
    end

    %% =============================================
    %% Cross-machine connections (all outbound from laptop)
    %% =============================================

    %% Outbound WS: laptop dials linode (no inbound port needed)
    LWS_BRIDGE ==>|"outbound WS<br/>/agency/ws<br/>ready + invoke_result"| RAGENCY

    %% HTTP registration: laptop registers codex-1 on linode
    LWS_BRIDGE -.->|"HTTP POST<br/>/api/alpha/agents<br/>register codex-1"| RAGENCY

    %% Federation (agent announcements)
    LAGENCY -.->|"federation announce"| RAGENCY

    %% =============================================
    %% Human surfaces
    %% =============================================
    JOE_IRC["Joe via IRC client"]
    JOE_EMACS["Joe via Emacs"]

    JOE_IRC -->|"PRIVMSG #futon"| RIRC
    JOE_EMACS -->|"WS frames"| RAGENCY
    JOE_EMACS -->|"Drawbridge /eval"| RBRIDGE

    %% =============================================
    %% CLI processes (spawned by invoke-fns)
    %% =============================================
    CLAUDE_CLI["claude -p --resume<br/>(ProcessBuilder)"]
    CODEX_CLI["codex exec --json<br/>(clojure.java.shell/sh)"]

    RCLAUDE -->|"spawns"| CLAUDE_CLI
    LCODEX -->|"spawns"| CODEX_CLI

    %% Style
    classDef human fill:#e1f5fe,stroke:#0288d1
    classDef agent fill:#fff3e0,stroke:#ef6c00
    classDef infra fill:#f3e5f5,stroke:#7b1fa2
    classDef bridge fill:#e8f5e9,stroke:#2e7d32

    class JOE_IRC,JOE_EMACS human
    class RCLAUDE,RCODEX_SLOT,LCODEX agent
    class REVIDENCE,LEVIDENCE,RAGENCY,LAGENCY,RIRC,RBRIDGE,LBRIDGE infra
    class LWS_BRIDGE bridge
    class CLAUDE_CLI,CODEX_CLI agent
