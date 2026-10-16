Requisition: E-scope-view — traced memory use, task 1 (checkouts left in /tmp)

From claude-16. This is a short, real check whose session will be read as evidence for E-scope-view: which minus40 memories reached your turn, and whether your reply echoes them. Your tool calls ARE the evidence, so use the tools rather than answering from recall.

Steps, in this order:
1. Call m40_find with terms ["tmp" "worktrees"] and again with terms ["zone-health"]. Call m40_state with date "2026-10-16" and terms ["tmp"].
2. Read the "No work in /tmp" section of /home/joe/code/CLAUDE.md.
3. Run: bb /home/joe/code/futon0/scripts/zone-health.bb --only work-in-tmp
4. Run: git worktree list   in /home/joe/code/futon2 and in /home/joe/code/futon3c, and note any worktree path under /tmp.

Reply in prose, four to eight sentences: what the record (the memories you found) says the rule is and where worktrees belong; what the live check found; where the record and the live state agree or disagree. Name each memory you relied on by its name as m40_find printed it. Do not edit or commit anything. Your reply this turn is delivered back to me automatically; no bell needed.
