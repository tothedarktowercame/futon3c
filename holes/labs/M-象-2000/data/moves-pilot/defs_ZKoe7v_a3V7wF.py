import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-17-turn-437"
build("turn-ZKoe7v", [
    A(f"{T}-f-s1-0", "verify", "verify/confirmed",
      "OK, I see it on the clickthrough."),
    A(f"{T}-f-s2-0", "propose", "propose/consolidate-ui",
      "it would be much nicer if this was shown on ONE interactive infographic."),
    A(f"{T}-f-s3-0", "delegate", "delegate/to-agent-with-tool",
      "Do you think Kimi could sort that out using Playwright to validate?"),
    A(f"{T}-r-0", "reply-mark", "delegate/to-agent",
      "Yes, I think Kimi can do this, so I've handed it to kimi-1 and it's working on it now.",
      answers=[(f"{T}-f-s2-0", "The plan is one page where clicking a stage band in the main stream, or its legend entry, opens that stage up into its pattern families in place", "closes"),
               (f"{T}-f-s3-0", "Yes, I think Kimi can do this, so I've handed it to kimi-1 and it's working on it now.", "closes"),
               ("claude-17-turn-435-r-4", "The plan is one page where clicking a stage band in the main stream", "closes")],
      opens="interactive infographic page",
      note="one page, as the earlier one-or-two question asked"),
    A(f"{T}-r-1", "reply-mark", "propose/acceptance-plan",
      "Kimi has to write a Playwright check that loads the page in headless Chromium and fails on any script error.",
      answers=[(f"{T}-f-s3-0", "It also checks the back button, hover values, and light and dark themes.", "keeps")],
      within="interactive infographic page"),
    A(f"{T}-r-2", "reply-mark", "report/process-note",
      "My first send was refused because Kimi seats need a requisition line naming a mission.",
      conf="medium",
      note="misfiled mission flagged for operator correction"),
    A(f"{T}-r-3", "reply-mark", "report/dispatched",
      "Job `invoke-1790893029289-29903-dcce93af`, park `park-38d969f0-7e9d-494e-80df-435caac0e5db`.",
      within="interactive infographic page"),
])

T = "claude-6-turn-5"
build("turn-a3V7wF", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/grievance",
      "I am feeling angry because when I got claude-2 to tell claude-4 about this I thought it was going to be used"),
    A(f"{T}-r-0", "reply-mark", "report/fact-check",
      "I checked the messages, and it went differently: claude-4 never received the route.",
      answers=[(f"{T}-f-s1-0", "So the agents carried out what reached them; the fast route was dropped before anything was sent.", "closes"),
               ("claude-6-turn-4-r-4", "claude-2 mentioned it to you once (\"so I only name it\") and left it out of the bell.", "keeps")],
      opens="elaborator route disambiguation",
      note="acknowledges the grievance while correcting the record"),
    A(f"{T}-r-1", "reply-mark", "report/evidence-chain",
      "claude-2 told you that letting native Lean elaborate and translating its output would change the picture",
      answers=[(f"{T}-f-s1-0", "That is what was sent, and what was built.", "keeps")],
      within="elaborator route disambiguation"),
    A(f"{T}-r-2", "reply-mark", "clarify/disambiguation",
      "\"Use the Lean elaborator\" can mean two things.",
      answers=[(f"{T}-f-s1-0", "You meant, or came to mean, the second. The agents heard the first", "closes")],
      within="elaborator route disambiguation"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "Shall I dispatch the test now: Lean's compiled output for two or three failing rows, translated to Python by a throwaway translator",
      answers=[(f"{T}-f-s1-0", "If it works, the same message to claude-4 would state the route as an action, quote your reading of gate 1, and include the result.", "keeps"),
               ("claude-6-turn-4-r-5", "checked against the preregistered expected output, outside mfuton-share, with a bell back to me?", "closes")],
      within="elaborator route disambiguation",
      note="concretizes the earlier two-or-three-row test offer"),
])
