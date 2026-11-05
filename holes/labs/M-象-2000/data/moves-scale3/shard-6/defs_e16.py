import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-36"
build("turn-L2e942", [
    A(f"{T}-f-s1-0", "report", "report/context",
      "Details of the post-receive hook are below."),
    A(f"{T}-f-s2-0", "qualify", "qualify/caveat",
      "That may need to change..."),
    A(f"{T}-f-s3-0", "report", "report/context",
      "$ cat ls /home/git/hel.git/hooks/post-receive"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(the push) `staging` is now on hel.git",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(whether the hook needs to change) It only matters in two cases",
      answers=[(f"{T}-f-s2-0", "(whether the hook needs to change)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/done",
      "(records) I updated the site memory",
      conf="medium"),
])

T = "claude-2-turn-105"
build("turn-NEBxBC", [
    A(f"{T}-f-s1-0", "explain", "clarify/assertion",
      "It's a blend with emergent structure."),
    A(f"{T}-f-s2-0", "explain", "clarify/context",
      "the idea  was described formally with regard to Goguen 1-and-a-half categories"),
    A(f"{T}-f-s3-0", "explain", "clarify/definition",
      'a houseboat and a boathouse are two different blends of "boat" and "house".'),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      '("Goguen 1-and-a-half categories") The source is Goguen\'s',
      answers=[(f"{T}-f-s2-0", '("Goguen 1-and-a-half categories")', "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      '("a houseboat and a boathouse") His 2006 paper gives the full example',
      answers=[(f"{T}-f-s3-0", '("a houseboat and a boathouse")', "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      '("emergent structure") Two statements of his',
      answers=[(f"{T}-f-s1-0", '("emergent structure")', "closes")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/design",
      "(what the draft says, in brief)",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/evidence",
      "(one check in the library)",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "report/in-flight",
      "(in flight) claude-17 and codex-10",
      conf="medium"),
])
