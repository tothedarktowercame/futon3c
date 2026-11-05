import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-360"
build("turn-dLz02V", [
    A(f"{T}-f-s1-0", "verify", "verify/confirmed",
      "All the characters show up nicely on my screen."),
    A(f"{T}-f-s2-0", "propose", "propose/document-key",
      "Let's save your key and a brief instruction about how to use it in CLAUDE.md and AGENTS.md"),
    A(f"{T}-f-s3-0", "approve", "approve/plain",
      "Regarding the 190 seat hours and research design, I would see that as acceptable."),
    A(f"{T}-f-s4-0", "propose", "propose/parallel-training",
      "we should do in parallel is continually train various editions of 小象"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "The key and a short usage note are now in `CLAUDE.md` and `AGENTS.md`.",
      answers=[(f"{T}-f-s2-0", "The key and a short usage note are now in `CLAUDE.md` and `AGENTS.md`.", "closes"),
               (f"{T}-f-s4-0", "小象 has its first logged edition", "closes")],
      opens "xiaoxiang editions" if False else None) if False else None,
])
