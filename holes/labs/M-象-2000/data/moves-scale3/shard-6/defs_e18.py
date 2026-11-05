import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-444"
build("turn-NuqqKL", [
    A(f"{T}-f-s2-0", "constrain", "constrain/standing-rule",
      "let's have each agent write to a unique file like R1-links.edn"),
    A(f"{T}-f-s2-1", "redirect", "propose/alternative",
      "rather than going via Agency, we could just use the CLI codex"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what's in place) Everything is in `futon0/analysis/audits/rnode-bids/`",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(R14 pilot) R14 (commitment temperature)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/in-flight",
      "(tracking) The run is `bg-1790894795239-28`",
      conf="medium"),
])

T = "claude-5-turn-3"
build("turn-O8L7or", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "Thanks."),
    A(f"{T}-f-s2-0", "constrain", "ask-action/test-script",
      "Now ask me one 🈸 question about whether to delete the file,",
      opens="ask then stop",
      note="a scripted test instruction with a stop condition"),
    A(f"{T}-f-s2-1", "constrain", "constrain/standing-rule",
      "and then stop.",
      within=f"{T}-f-s2-0"),
    A(f"{T}-r-0", "reply-mark", "ask-action/one-decision",
      '("whether to delete the file") Shall I delete elephant.txt',
      answers=[(f"{T}-f-s2-0", '("whether to delete the file")', "closes")],
      within=f"{T}-f-s2-0"),
])
