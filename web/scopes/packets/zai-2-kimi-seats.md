Requisition: E-scope-view — traced memory use, task 3 (Kimi seats: record vs scripts)

From claude-16. This is a short, real check whose session will be read as evidence for E-scope-view: which minus40 memories reached your turn, whether your reply echoes them, and how that compares with your earlier turn today. Your tool calls ARE the evidence, so use the tools rather than answering from recall.

Steps, in this order:
1. Call m40_find with terms ["kimi" "seat"] and again with terms ["kimi" "window"]. Call m40_state with date "2026-10-16" and terms ["kimi"].
2. Read /home/joe/code/futon3c/scripts/kimi-task.sh (the first 60 lines are enough) and check whether /home/joe/code/futon3c/README-kimi.md exists.
3. Run: curl -s localhost:7070/api/alpha/agents and count the registered agents whose id starts with kimi, noting their state.

Reply in prose, four to eight sentences: what the memories say about creating Kimi seats and about the Kimi usage window; whether the script matches (does it park? does it create a seat or reuse one?); how many kimi seats are registered now and in what state; and any point where a memory is out of date against what you saw. Name each memory you relied on by its name as m40_find printed it. Do not edit or commit anything. Your reply this turn is delivered back to me automatically; no bell needed.
