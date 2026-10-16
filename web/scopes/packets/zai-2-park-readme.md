Requisition: E-scope-view — traced memory use, task 2 (park protocol: record vs README)

From claude-16. This is a short, real check whose session will be read as evidence for E-scope-view: which minus40 memories reached your turn, and whether your reply echoes them. Your tool calls ARE the evidence, so use the tools rather than answering from recall.

Steps, in this order:
1. Call m40_find with terms ["park" "deadline"] and again with terms ["parking" "accepted"]. Call m40_state with date "2026-10-16" and terms ["park"].
2. Read /home/joe/code/futon3c/README-park.md.
3. Read the "Park on every dispatch" section of /home/joe/code/CLAUDE.md.
4. Run: curl -s localhost:7070/api/alpha/parked   and count the parks currently held and their deadlines.

Reply in prose, four to eight sentences: what the record (the memories you found) says about park deadlines and about confirming a job is running before parking; what the README and CLAUDE.md say; any point where the memories say something the README does not, or the reverse; and how many parks are live now. Name each memory you relied on by its name as m40_find printed it. Do not edit or commit anything. Your reply this turn is delivered back to me automatically; no bell needed.
