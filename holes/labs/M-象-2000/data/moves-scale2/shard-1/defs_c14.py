import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-425"
build("turn-TTtCKM", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      "I have looked at the new Minard diagram."),
    A(f"{T}-f-s2-0", "agree", "approve/plain",
      "there hasn't been any autonomous work outside of 6-hour gaps, which is plausible"),
    A(f"{T}-f-s2-1", "explain", "explain/reason",
      "because I have done a lot of hands-on-the-keyboard work while here in Dallas."),
    A(f"{T}-f-s3-0", "verify", "verify/sanity-check-request",
      "as a sanity check perhaps we could make a second box-diagram for 3-hour gaps"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "Gist: The figure doesn't show that.",
      answers=[(f"{T}-f-s2-0", "Gist: The figure doesn't show that.", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(how the lower panel is built) It comes from `FORENSIC-autopilot-2026-09-21.md`",
      answers=[(f"{T}-f-s3-0", "(how the lower panel is built)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/with-options",
      "(other things a 3-hour version needs)",
      answers=[(f"{T}-f-s3-0", "(other things a 3-hour version needs)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I dispatch this to zai-2?",
      answers=[(f"{T}-f-s3-0", "(one decision) Shall I dispatch this to zai-2?", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-420"
build("turn-UzImn0", [
    A(f"{T}-f-s1-0", "agree", "approve/plain",
      "OK, mapping the 97 patterns to PBASE stages, got it."),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "maybe the Minard method can induce/infer a stage for them"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "Gist: The Minard method does have a built-in guess for this",
      answers=[(f"{T}-f-s2-0", "Gist: The Minard method does have a built-in guess for this", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what I measured just now)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/context",
      "(labelling is already a model reading)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/alternative",
      "(an inference that might work) The patterns already have embeddings",
      answers=[(f"{T}-f-s2-0", "(an inference that might work)", "keeps")],
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "ask-action/two-decisions",
      "(one decision) Shall I have the Kimi seats read the 97 patterns",
      answers=[(f"{T}-f-s2-0", "(one decision)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-8@turn-V3oRdx"
build("turn-V3oRdx", [
    A(f"{T}-f-s1-0", "delegate", "delegate/to-agent",
      "Yes please add it to E-agency-work-orders and bell it to kimi-1"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(HUD in the excursion, adapter to kimi-1) Both done.",
      answers=[(f"{T}-f-s1-0", "(HUD in the excursion, adapter to kimi-1) Both done.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(the excursion section) It records:",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/design",
      "(the adapter packet) One behaviour",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/acceptance-plan",
      "(review) When it returns, I'll test the claim in the other direction too",
      conf="medium"),
])
