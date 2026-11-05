import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-410"
build("turn-EMwTyb", [
    A(f"{T}-f-s1-0", "verify", "verify/status-request",
      "let's make sure the processing is actually queued and running"),
    A(f"{T}-f-s1-1", "constrain", "constrain/for-now",
      "ideally with counts that are tailable via a simple command"),
    A(f"{T}-f-s2-0", "defer", "defer/until",
      'Let\'s hold the "One job" until such time as we have the 象 dataset complete'),
    A(f"{T}-r-0", "reply-mark", "report/status",
      "(what's queued) The 68 pending turns",
      answers=[(f"{T}-f-s1-0", "(what's queued) The 68 pending turns", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(watching progress)",
      answers=[(f"{T}-f-s1-1", "(watching progress)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/diagnosis",
      "(pipes, your point from last turn) You were right",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(pipes fix) Shall I propose changing the author on park-resume",
      conf="medium"),
])

T = "claude-17-turn-465"
build("turn-GY9bI0", [
    A(f"{T}-f-s2-0", "explain", "explain/expectation",
      "The War Machine's agents should have self-talk or exchanges between them."),
    A(f"{T}-f-s3-0", "propose", "propose/tentative",
      "If we can find those transcripts, we should be able to figure out what they are doing"),
    A(f"{T}-r-0", "reply-mark", "report/status",
      "(the War Machine's own agents) The transcripts exist, but only four R-nodes have speakers.",
      answers=[(f"{T}-f-s2-0", "The transcripts exist, but only four R-nodes have speakers.", "closes")],
      opens="wm agent corpus"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what's retained) There are two sources:",
      answers=[(f"{T}-f-s3-0", "(what's retained) There are two sources:", "keeps")],
      within="wm agent corpus",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(limits) This covers 4 of the 23 nodes.",
      answers=[(f"{T}-f-s3-0", "(limits) This covers 4 of the 23 nodes.", "keeps")],
      within="wm agent corpus"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(go-ahead) Shall I build the corpus and run the contrast?",
      answers=[(f"{T}-f-s3-0", "(go-ahead) Shall I build the corpus", "keeps")],
      within="wm agent corpus"),
])

T = "claude-17-turn-50"
build("turn-GiuyRt", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "a no visible offer message in claude-4 repl is an interesting exercise"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "Your \"yes\" got \"no-visible-offer\"",
      answers=[(f"{T}-f-s1-0", 'Your "yes" got "no-visible-offer"', "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(why it's a good exercise) The one paragraph shows four gaps:",
      answers=[(f"{T}-f-s1-0", "(why it's a good exercise)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(a fix for 1 and 2, small) Treat a 🈳 paragraph",
      answers=[(f"{T}-f-s1-0", "(a fix for 1 and 2, small)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I make that change to the recorder",
      answers=[(f"{T}-f-s1-0", "(one decision) Shall I make that change", "keeps")],
      conf="medium"),
])
