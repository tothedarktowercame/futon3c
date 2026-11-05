import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-426"
build("turn-Sdbuno", [
    A(f"{T}-f-s1-0", "redirect", "propose/redirect-scope",
      "the gap-per-N hours should already exist as part of xiao-xiang.py"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(what its gaps measure: three differences from the Minard panel)",
      answers=[(f"{T}-f-s1-0", "(what its gaps measure", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/in-flight",
      "(where it writes)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report-problem/observed",
      "Gist: This wake-up came early.",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "explain/mechanism",
      "(the park) A park with an empty `awaiting` list",
      conf="medium"),
])

T = "claude-2-turn-62"
build("turn-To5NXY", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "Yes, this sounds right"),
    A(f"{T}-f-s1-1", "qualify", "qualify/caveat",
      "but probably not all of them,",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s1-2", "approve", "approve/plain",
      "that's okay if we can specify r20",
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-0", "reply-mark", "report/in-flight",
      "(status) I read PROOF-2a's theorem",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what PROOF-2a already fixed)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      "(what R20 adds)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/design",
      "(precision, reviewed)",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/done",
      "(precision, limit)",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "report/in-flight",
      "(R20 job, running)",
      conf="medium"),
    A(f"{T}-r-6", "reply-mark", "report/in-flight",
      "(status) - **Job:** `invoke-1791174014053-32568-a25564cd`",
      conf="medium"),
])
