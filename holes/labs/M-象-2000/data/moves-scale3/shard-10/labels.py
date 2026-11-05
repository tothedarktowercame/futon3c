# Batch-3 wave-4 labels: dialogue-act moves; scopes vs open_ports/in_force.

LABELS = {}

def L(rec, acts):
    LABELS[rec] = acts

# --- claude-17 (stepper/rewind & related) ---
L("turn-gXCZfv", [
 {"act": "claude-17-turn-35-f-s1-0", "kind": "propose", "move": "propose/directive",
  "quote": "we should make the \"staging\" site a git checkout like futon7a"}])
L("turn-iNKHir", [
 {"act": "claude-17-turn-280-f-s1-0", "kind": "continue", "move": "continue/resume-work",
  "quote": "OK so..."},
 {"act": "claude-17-turn-280-f-s2-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "if it is really a stepper with Elephant-2000 semantics we should be able to demonstrate things like rewinding.",
  "opens": "rewind-demos"},
 {"act": "claude-17-turn-280-f-s3-0", "kind": "propose", "move": "propose/alternative",
  "quote": "One way to do this would be via git pins.", "within": "rewind-demos"},
 {"act": "claude-17-turn-280-f-s4-0", "kind": "report", "move": "report/observation",
  "quote": "I have made various attempts at having the inbox-zero service actually work"},
 {"act": "claude-17-turn-280-f-s5-0", "kind": "propose", "move": "propose/alternative",
  "quote": "If we instead wanted to unwind aspects of an interpretation",
  "within": "rewind-demos"}])
L("turn-ip3KYO", [
 {"act": "claude-17-turn-349-f-s1-0", "kind": "propose", "move": "propose/tentative",
  "quote": "there's another mission, which isn't completed, called M-formal-patterns that could"}])
L("turn-jydyGP", [
 {"act": "claude-17-turn-347-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Sure we could do a \"TN-facade-considered-harmful.md\" if you just search on \"facade\"."},
 {"act": "claude-17-turn-347-f-s2-0", "kind": "clarify", "move": "clarify/definition",
  "quote": "I'm thinking that facade is a correct usage for the Facade design pattern",
  "within": "claude-17-turn-347-f-s1-0"}])
L("turn-kO4P9A", [
 {"act": "claude-17-turn-308-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "I did like your \"fork session\" idea"},
 {"act": "claude-17-turn-308-f-s1-1", "kind": "propose", "move": "propose/tentative",
  "quote": "maybe we should add that as an alternative configuration if it works"}])
L("turn-kXMi8w", [
 {"act": "claude-17-turn-283-f-s1-0", "kind": "report", "move": "report/observation",
  "quote": "Ah, I had forgotten about park."},
 {"act": "claude-17-turn-283-f-s2-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "in a highly orchestrated mode there may be one operator turn and 100 or 1000 automated turns."},
 {"act": "claude-17-turn-283-f-s3-0", "kind": "defer", "move": "defer/until",
  "quote": "for now, let's keep it simple and NOT do the part about continuations.",
  "closes": "claude-17-turn-283-f-s2-0"},
 {"act": "claude-17-turn-283-f-s4-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "We'll know what the agent did in its turn, and that's it."},
 {"act": "claude-17-turn-283-f-s5-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "we should at least be able to get back to where we were at the start of the turn."}])
L("turn-lKnuWk", [
 {"act": "claude-17-turn-270-f-s1-0", "kind": "redirect", "move": "redirect/back-to-plan",
  "quote": "Let's try to focus on the M-象-2000 issues.", "opens": "M-象-2000-focus"},
 {"act": "claude-17-turn-270-f-s2-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "the underlining of old joe: turns goes away"},
 {"act": "claude-17-turn-270-f-s2-1", "kind": "explain", "move": "explain/observation",
  "quote": "the underlining is gone so I can't cite it accurately.",
  "within": "claude-17-turn-270-f-s2-0"}])
L("turn-ll2u4P", [
 {"act": "claude-17-turn-274-f-s1-0", "kind": "clarify", "move": "clarify/check-understand",
  "quote": "Uh, we are reusing the IBOL verbs inside the web page,"},
 {"act": "claude-17-turn-274-f-s1-1", "kind": "disagree", "move": "disagree/correction",
  "quote": "I was pretty sure that \"look\" is one of the operators"}])
L("turn-nb32l7", [
 {"act": "claude-17-turn-335-f-s1-0", "kind": "extend", "move": "extend/possibility",
  "quote": "we need to understand how the graph grows / enriches / garbage collects"}])
L("turn-nsNSf2", [
 {"act": "claude-17-turn-333-f-s1-0", "kind": "clarify", "move": "clarify/assertion",
  "quote": "\"cascade file\" isn't quite my concept of how this should work"},
 {"act": "claude-17-turn-333-f-s1-1", "kind": "propose", "move": "propose/alternative",
  "quote": "I want a giant connected cascade of which individual cascades are just \"retractions\""}])
L("turn-pBW6st", [
 {"act": "claude-17-turn-341-f-s1-0", "kind": "continue", "move": "continue/resume-work",
  "quote": "Hm..."},
 {"act": "claude-17-turn-341-f-s2-0", "kind": "explore", "move": "explore/open-question",
  "quote": "the live-gap feature could be interesting"},
 {"act": "claude-17-turn-341-f-s3-0", "kind": "clarify", "move": "clarify/check-understand",
  "quote": "I was thinking that you were talking about gaps like the time between turns"},
 {"act": "claude-17-turn-341-f-s4-0", "kind": "propose", "move": "propose/directive",
  "quote": "Maybe we can look into both."},
 {"act": "claude-17-turn-341-f-s5-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "the 象 annotations are only on the operator turns"},
 {"act": "claude-17-turn-341-f-s6-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "we'd have to model it."},
 {"act": "claude-17-turn-341-f-s7-0", "kind": "report", "move": "report/observation",
  "quote": "Your remark about \"parks on a deadline\" is something we already model."},
 {"act": "claude-17-turn-341-f-s8-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "We don't need a special \"promise\" agent tool for that."},
 {"act": "claude-17-turn-341-f-s9-0", "kind": "propose", "move": "propose/tentative",
  "quote": "maybe we would *benefit* from interfacing with the custom harness used by Zai and Kimi"}])
L("turn-q7JPOG", [
 {"act": "claude-17-turn-343-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "So, I like the direct way you put it, assuming I understand correctly."},
 {"act": "claude-17-turn-343-f-s2-0", "kind": "collect", "move": "collect/quote-back",
  "quote": "The commitments that were broken are yours to the program, not the agent's to you"},
 {"act": "claude-17-turn-343-f-s3-0", "kind": "clarify", "move": "clarify/check-understand",
  "quote": "Here I don't think you're saying I was not living up to my own standard.",
  "within": "claude-17-turn-343-f-s2-0"},
 {"act": "claude-17-turn-343-f-s4-0", "kind": "clarify", "move": "clarify/definition",
  "quote": "I think what you're saying is that I had specified certain things",
  "within": "claude-17-turn-343-f-s2-0"},
 {"act": "claude-17-turn-343-f-s5-0", "kind": "explain", "move": "explain/observation",
  "quote": "this wasn't a case of agent malfeasance either"},
 {"act": "claude-17-turn-343-f-s6-0", "kind": "explain", "move": "explain/reasoning",
  "quote": "the rule was never really written down in a way that it could be followed."},
 {"act": "claude-17-turn-343-f-s7-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "I ended up feeling upset because I thought I'd be getting something else out of this process"},
 {"act": "claude-17-turn-343-f-s8-0", "kind": "explain", "move": "explain/reasoning",
  "quote": "the \"commitment to the program\" was broken not at the point of delivery, but really at the point of specification."},
 {"act": "claude-17-turn-343-f-s9-0", "kind": "report", "move": "report/observation",
  "quote": "I *thought* I was requisitioning something that was clearly specified"},
 {"act": "claude-17-turn-343-f-s10-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "the Lean model contained backdoors where it wasn't specified correctly."},
 {"act": "claude-17-turn-343-f-s11-0", "kind": "report", "move": "report/status",
  "quote": "NOTE-lean-backdoor-audit-2026-09-30.md"}])
L("turn-qBPytU", [
 {"act": "claude-17-turn-264-f-s1-0", "kind": "explore", "move": "explore/open-question",
  "quote": "So..."},
 {"act": "claude-17-turn-264-f-s2-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "the first demo would be for me, not for the website"},
 {"act": "claude-17-turn-264-f-s2-1", "kind": "propose", "move": "propose/directive",
  "quote": "can we walk through some of the Elephant-2000 interactions with this system?",
  "within": "claude-17-turn-264-f-s2-0"},
 {"act": "claude-17-turn-264-f-s3-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "we might need to build the alignment of what the agent actually did"},
 {"act": "claude-17-turn-264-f-s4-0", "kind": "propose", "move": "propose/design",
  "quote": "I'm pretty sure we could build a 2-up mode"},
 {"act": "claude-17-turn-264-f-s5-0", "kind": "explain", "move": "explain/analogy",
  "quote": "it's a little bit like building GDB but for operator turns."},
 {"act": "claude-17-turn-264-f-s6-0", "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "What did I say?"},
 {"act": "claude-17-turn-264-f-s7-0", "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "What was the parse?"},
 {"act": "claude-17-turn-264-f-s8-0", "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "Then, what happened?"}])
L("turn-qJLdm2", [
 {"act": "claude-17-turn-348-f-s1-0", "kind": "explore", "move": "explore/open-question",
  "quote": "maybe 象-2000 is useful as a \"specification language\"."},
 {"act": "claude-17-turn-348-f-s2-0", "kind": "propose", "move": "propose/tentative",
  "quote": "maybe it could be usefully combined with M-diagramprover to *validate* the specifications",
  "within": "claude-17-turn-348-f-s1-0"},
 {"act": "claude-17-turn-348-f-s3-0", "kind": "explain", "move": "explain/example",
  "quote": "if G-over-cascades was specifed but but tangibly a backdoor",
  "within": "claude-17-turn-348-f-s2-0"}])
L("turn-quBqCC", [
 {"act": "claude-17-turn-286-f-s1-0", "kind": "verify", "move": "verify/status-check",
  "quote": "I still see"},
 {"act": "claude-17-turn-286-f-s1-1", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "the 象 stepper window on frame 91 with \"no operator words\"",
  "within": "claude-17-turn-286-f-s1-0"}])
L("turn-u68Q5n", [
 {"act": "claude-17-turn-301-f-s1-0", "kind": "explain", "move": "explain/context",
  "quote": "OK this turn is here for me to practice rewinding with."},
 {"act": "claude-17-turn-301-f-s2-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "@claude-17: just say hi.", "within": "claude-17-turn-301-f-s1-0"}])
L("turn-vA4xCY", [
 {"act": "claude-17-turn-303-f-s1-0", "kind": "propose", "move": "propose/directive",
  "quote": "OK, let's set up another practice turn."},
 {"act": "claude-17-turn-303-f-s2-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "@claude-17 please make a signed commit in futon7a which I will subsequently rewind.",
  "within": "claude-17-turn-303-f-s1-0"}])
L("turn-wTyaZR", [
 {"act": "claude-17-turn-298-f-s1-0", "kind": "propose", "move": "propose/design",
  "quote": "the stepper should retry after a wait."},
 {"act": "claude-17-turn-298-f-s2-0", "kind": "propose", "move": "propose/design",
  "quote": "Possibly we should develop an \"r r\" pattern or \"r R\" pattern"},
 {"act": "claude-17-turn-298-f-s3-0", "kind": "propose", "move": "propose/design",
  "quote": "we might need to be *storing* the 象 data in futon1b so we can use XT tricks to rewind."},
 {"act": "claude-17-turn-298-f-s4-0", "kind": "extend", "move": "extend/possibility",
  "quote": "Another fun feature would be to rewind the REPL buffer"}])
L("turn-wkDFj5", [
 {"act": "claude-17-turn-279-f-s1-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "I don't know what the right solution is"},
 {"act": "claude-17-turn-279-f-s1-1", "kind": "propose", "move": "propose/alternative",
  "quote": "presumably it just shouldn't display anything until the results are ready",
  "within": "claude-17-turn-279-f-s1-0"}])
L("turn-zfiDGh", [
 {"act": "claude-17-turn-267-f-s1-0", "kind": "explain", "move": "explain/context",
  "quote": "The prompt changes each turn so I don't recall which ones came up"},
 {"act": "claude-17-turn-267-f-s1-1", "kind": "collect", "move": "collect/ask-option",
  "quote": "retrieve them from XTDB"},
 {"act": "claude-17-turn-267-f-s1-2", "kind": "defer", "move": "defer/until",
  "quote": "I could comment on which had a negative sound", "within": "claude-17-turn-267-f-s1-1"}])
L("turn-zjvQRQ", [
 {"act": "claude-17-turn-346-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "Sure, this is a good level of pilot/follow-up on the idea we were discussing, please do"},
 {"act": "claude-17-turn-346-f-s1-1", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "we could write it up as a TN-G-over-cascades-revisited.md",
  "within": "claude-17-turn-346-f-s1-0"}])
L("turn-zl4sFo", [
 {"act": "claude-17-turn-269-f-s1-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "Right now voxterm is saying \"agency: timed out\""}])
L("turn-zuxAgV", [
 {"act": "claude-17-turn-345-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "So, yes, we're on the same page."},
 {"act": "claude-17-turn-345-f-s2-0", "kind": "defer", "move": "defer/until",
  "quote": "Let's assume that Claude-1 manages to get the War Machine build itself under control."},
 {"act": "claude-17-turn-345-f-s3-0", "kind": "propose", "move": "propose/next-step",
  "quote": "So, we will specify \"critical parameters\" for example.",
  "within": "claude-17-turn-345-f-s2-0"},
 {"act": "claude-17-turn-345-f-s4-0", "kind": "prioritize", "move": "prioritize/focus",
  "quote": "I think what we can do that is most valuable is to try and understand the *build process*",
  "opens": "build-process-understanding"},
 {"act": "claude-17-turn-345-f-s5-0", "kind": "propose", "move": "propose/tentative",
  "quote": "let's consider that the deficiences of the War Machine are *mirrored* by the deficiencies of the overall build process",
  "within": "claude-17-turn-345-f-s4-0"},
 {"act": "claude-17-turn-345-f-s6-0", "kind": "explain", "move": "explain/analogy",
  "quote": "Just as the War Machine could not see most of its niche and had reduced its \"brain\" to 2 neurons",
  "within": "claude-17-turn-345-f-s5-0"}])
L("turn-zzNVem", [
 {"act": "claude-17-turn-276-f-s1-0", "kind": "verify", "move": "verify/checked-live",
  "quote": "OK, this works,"},
 {"act": "claude-17-turn-276-f-s1-1", "kind": "disagree", "move": "disagree/correction",
  "quote": "I think the \"象 marks with no operator word:\" line and dropdown just take up space",
  "within": "claude-17-turn-276-f-s1-0"},
 {"act": "claude-17-turn-276-f-s1-2", "kind": "propose", "move": "propose/alternative",
  "quote": "we could put those into the PARSE segment as its own underlined portions.",
  "within": "claude-17-turn-276-f-s1-1"}])

# --- claude-1 (PROOF-2 / War Machine build) ---
L("turn-hFwOuJ", [
 {"act": "claude-1-turn-153-f-s1-0", "kind": "clarify", "move": "clarify/assertion",
  "quote": "I have told you the small model in the last few chat messages"},
 {"act": "claude-1-turn-153-f-s2-0", "kind": "explain", "move": "explain/reasoning",
  "quote": "If there were any functions that provided it we would not be having this conversation."}])
L("turn-hlUHVb", [
 {"act": "claude-1-turn-156-f-s1-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "it should be no different from the regular machine being run in a debugger"}])
L("turn-i0neS5", [
 {"act": "claude-1-turn-169-f-s1-0", "kind": "disagree", "move": "disagree/correction",
  "quote": "whoever fed cascadePolicySet with the degenerate badly designed system certainly did not get it from me"},
 {"act": "claude-1-turn-169-f-s2-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "this machine should not be based on facades"},
 {"act": "claude-1-turn-169-f-s3-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "you tell me that there was 1 item in the policy space"}])
L("turn-j1RMWb", [
 {"act": "claude-1-turn-164-f-s1-0", "kind": "delegate", "move": "delegate/to-agent",
  "quote": "You can be granted 1 click under the new system"},
 {"act": "claude-1-turn-164-f-s1-1", "kind": "constrain", "move": "constrain/for-now",
  "quote": "it needs to be pushed through end to end", "within": "claude-1-turn-164-f-s1-0"}])
L("turn-jNu4PX", [
 {"act": "claude-1-turn-237-f-s1-0", "kind": "redirect", "move": "redirect/change-of-plan",
  "quote": "I have a better idea than summarising."},
 {"act": "claude-1-turn-237-f-s2-0", "kind": "propose", "move": "propose/directive",
  "quote": "Let's use the Evidence Landscape.", "within": "claude-1-turn-237-f-s1-0",
  "opens": "evidence-landscape-heads"},
 {"act": "claude-1-turn-237-f-s3-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "HEADs are supposed to be in my words.", "within": "claude-1-turn-237-f-s2-0"},
 {"act": "claude-1-turn-237-f-s4-0", "kind": "propose", "move": "propose/next-step",
  "quote": "go find when they were recorded and see what words of mine were associated",
  "within": "claude-1-turn-237-f-s2-0"},
 {"act": "claude-1-turn-237-f-s5-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "Two limitations are worth considering"},
 {"act": "claude-1-turn-237-f-s6-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "we wouldn't expect to get complete coverage using this technique.",
  "within": "claude-1-turn-237-f-s5-0"},
 {"act": "claude-1-turn-237-f-s7-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "if we got, say, 70 missions with real HEADs this way, then we could run the system without it being a facade",
  "within": "claude-1-turn-237-f-s2-0"},
 {"act": "claude-1-turn-237-f-s8-0", "kind": "extend", "move": "extend/possibility",
  "quote": "NB similar concepts apply to Excursions and Tickets."}])
L("turn-k1hnor", [
 {"act": "claude-1-turn-114-f-s1-0", "kind": "report", "move": "report/status",
  "quote": "I restarted both futon1b and futon3c"},
 {"act": "claude-1-turn-114-f-s1-1", "kind": "continue", "move": "continue/resume-work",
  "quote": "so we should have a clean setup to work with", "within": "claude-1-turn-114-f-s1-0"}])
L("turn-lOVt6C", [
 {"act": "claude-1-turn-138-f-s1-0", "kind": "delegate", "move": "delegate/handoff-spec",
  "quote": "Let's agree the plan with codex-6."},
 {"act": "claude-1-turn-138-f-s2-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "a plan that does not involve running \"clicks\" because you have wasted time and usage that way",
  "within": "claude-1-turn-138-f-s1-0"},
 {"act": "claude-1-turn-138-f-s3-0", "kind": "propose", "move": "propose/next-step",
  "quote": "You will need to agree another plan for verifying that the behaviour will work",
  "within": "claude-1-turn-138-f-s1-0"}])
L("turn-nOAm5F", [
 {"act": "claude-1-turn-140-f-s1-0", "kind": "qualify", "move": "qualify/scope",
  "quote": "I don't care HOW the system works initially."},
 {"act": "claude-1-turn-140-f-s2-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "You could fill all the text fields with \"XXXX\" and it should work, but it wouldn't.",
  "within": "claude-1-turn-140-f-s1-0"}])
L("turn-o4sSH6", [
 {"act": "claude-1-turn-154-f-s1-0", "kind": "clarify", "move": "clarify/check-understand",
  "quote": "I have no idea what you mean by \"project\""}])
L("turn-oCRVHo", [
 {"act": "claude-1-turn-168-f-s1-0", "kind": "verify", "move": "verify/check-understand",
  "quote": "Surel the system specified in Lean cannot have failed to have recorded this core design principle"},
 {"act": "claude-1-turn-168-f-s2-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "There is an order of operation problem."},
 {"act": "claude-1-turn-168-f-s3-0", "kind": "redirect", "move": "redirect/method",
  "quote": "Go ahead and interpret a pattern, after you select it."},
 {"act": "claude-1-turn-168-f-s4-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "Don't gate selection on there already being an interpretation.",
  "within": "claude-1-turn-168-f-s3-0"},
 {"act": "claude-1-turn-168-f-s5-0", "kind": "direct", "move": "ask-action/read-record",
  "quote": "Look at how M-象-cascade works.", "within": "claude-1-turn-168-f-s3-0"},
 {"act": "claude-1-turn-168-f-s6-0", "kind": "disagree", "move": "disagree/correction",
  "quote": "I have been told that the machine was ready to run, that it met the specification."},
 {"act": "claude-1-turn-168-f-s7-0", "kind": "explain", "move": "explain/reasoning",
  "quote": "the specification either did not write down what I said should be in there, or it was changed after the fact.",
  "within": "claude-1-turn-168-f-s6-0"}])
L("turn-oJSzE7", [
 {"act": "claude-1-turn-113-f-s1-0", "kind": "report", "move": "report/status",
  "quote": "Another agent is doing a heavy backfill job that adds another index"},
 {"act": "claude-1-turn-113-f-s1-1", "kind": "defer", "move": "defer/until",
  "quote": "so we will restart after that finishes", "within": "claude-1-turn-113-f-s1-0"}])
L("turn-oQQXu0", [
 {"act": "claude-1-turn-116-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "OK, please make the small change"},
 {"act": "claude-1-turn-116-f-s1-1", "kind": "constrain", "move": "constrain/for-now",
  "quote": "and hot-load it into futon1b with Drawbridge", "within": "claude-1-turn-116-f-s1-0"}])
L("turn-ogEaLG", [
 {"act": "claude-1-turn-108-f-s1-0", "kind": "verify", "move": "verify/status-check",
  "quote": "OK, show me it working via the next dispatch"}])
L("turn-on8hJV", [
 {"act": "claude-1-turn-93-f-s1-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "this business of \"five targets\" is reminiscent of the earlier white list facade"},
 {"act": "claude-1-turn-93-f-s2-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "The machine should have all unfinished Missions, Excursions, and Tickets to choose from."},
 {"act": "claude-1-turn-93-f-s3-0", "kind": "disagree", "move": "disagree/correction",
  "quote": "The \"five targets\" is at best a placeholder for a complete target",
  "within": "claude-1-turn-93-f-s1-0"},
 {"act": "claude-1-turn-93-f-s3-1", "kind": "explain", "move": "explain/reasoning",
  "quote": "we can't have a \"War Machine ranging over all available tasks in FUTON",
  "within": "claude-1-turn-93-f-s2-0"},
 {"act": "claude-1-turn-93-f-s4-0", "kind": "extend", "move": "extend/possibility",
  "quote": "Especially if, as a kind of picky eater, it won't even work on the five",
  "within": "claude-1-turn-93-f-s3-0"}])
L("turn-qazxCi", [
 {"act": "claude-1-turn-139-f-s1-0", "kind": "propose", "move": "propose/directive",
  "quote": "I would like to see \"exend the Lean proof to prove that bad behaviour cannot occur\"."},
 {"act": "claude-1-turn-139-f-s2-0", "kind": "explain", "move": "explain/context",
  "quote": "We spent days and days developing an AIF formalism."},
 {"act": "claude-1-turn-139-f-s3-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "you're running info failures purely on interface things"},
 {"act": "claude-1-turn-139-f-s4-0", "kind": "retract", "move": "retract/correction",
  "quote": "PROOF-2a clearly was not a proof of anything"}])
L("turn-szLhf8", [
 {"act": "claude-1-turn-105-f-s1-0", "kind": "disagree", "move": "disagree/correction",
  "quote": "I don't know if we need to wait on the 230 targets for an interpretation gate"},
 {"act": "claude-1-turn-105-f-s2-0", "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "So, how is interpretation *supposed* to work?", "within": "claude-1-turn-105-f-s1-0"}])
L("turn-u16ikv", [
 {"act": "claude-1-turn-124-f-s1-0", "kind": "report", "move": "report/status",
  "quote": "OK, you have kimi, zai, and codex available."},
 {"act": "claude-1-turn-124-f-s2-0", "kind": "continue", "move": "continue/resume-work",
  "quote": "I'd like you to keep working according to the plan we have discussed",
  "within": "claude-1-turn-124-f-s1-0"}])
L("turn-uBFrjQ", [
 {"act": "claude-1-turn-94-f-s1-0", "kind": "propose", "move": "propose/tentative",
  "quote": "we could admit Algorithms (A-)"},
 {"act": "claude-1-turn-94-f-s2-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "This business abstentions is not good at all."},
 {"act": "claude-1-turn-94-f-s3-0", "kind": "explain", "move": "explain/analogy",
  "quote": "Imagine if YOU decided to abstain from work when I asked",
  "within": "claude-1-turn-94-f-s2-0"}])
L("turn-uChO9h", [
 {"act": "claude-1-turn-155-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "OK, well, then, yes, each file is inside a repo"},
 {"act": "claude-1-turn-155-f-s2-0", "kind": "redirect", "move": "redirect/refocus",
  "quote": "But you don't need to worry about managing information at that level in order to GET THE MACHINE TO WORK."},
 {"act": "claude-1-turn-155-f-s3-0", "kind": "clarify", "move": "clarify/scope",
  "quote": "I don't care if you address futon0 issues", "within": "claude-1-turn-155-f-s2-0"},
 {"act": "claude-1-turn-155-f-s4-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "so far NONE have been addressed."},
 {"act": "claude-1-turn-155-f-s5-0", "kind": "redirect", "move": "redirect/change-of-plan",
  "quote": "just begin, and once we get the machine done, we will shortly have addressed ALL the issues",
  "closes": "claude-1-turn-155-f-s3-0"}])
L("turn-v5LFbO", [
 {"act": "claude-1-turn-230-f-s1-0", "kind": "defer", "move": "defer/until",
  "quote": "My sense is that \"Pooling over near-variant policies, or diverse retractions\" is a premature question"},
 {"act": "claude-1-turn-230-f-s1-1", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "longer a backdoor for \"whatever\" data structure we put in.",
  "within": "claude-1-turn-230-f-s1-0"},
 {"act": "claude-1-turn-230-f-s2-0", "kind": "delegate", "move": "delegate/to-agent",
  "quote": "You could work with Codex to clarify the definitions"}])
L("turn-vIxmo9", [
 {"act": "claude-1-turn-161-f-s1-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "codex-5 can build the debugger but then can be retired"},
 {"act": "claude-1-turn-161-f-s2-0", "kind": "report", "move": "report/observation",
  "quote": "We already have a whole system of \"refusals\" and other known types of failures."},
 {"act": "claude-1-turn-161-f-s3-0", "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "They need to be raised as errors and handled properly.".replace("properly","appropriately")},
 {"act": "claude-1-turn-161-f-s4-0", "kind": "redirect", "move": "redirect/method",
  "quote": "There are already tools for this in the Clojure ecosystem.",
  "within": "claude-1-turn-161-f-s3-0"}])
L("turn-vUUqAY", [
 {"act": "claude-1-turn-166-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "let's write this up as a critical incident so that it doesn't get lost in chat"},
 {"act": "claude-1-turn-166-f-s2-0", "kind": "disagree", "move": "disagree/premise",
  "quote": "\"The policy space had one member\"?"},
 {"act": "claude-1-turn-166-f-s3-0", "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "How is that possible when \"policies\" are cascades which are formed on the fly?",
  "within": "claude-1-turn-166-f-s2-0"}])
L("turn-vVYf2H", [
 {"act": "claude-1-turn-117-f-s1-0", "kind": "report", "move": "report/observation",
  "quote": "before several diversions we were working through PROOF-2b on a \"one demonstrated capability per dispatch\" basis"},
 {"act": "claude-1-turn-117-f-s1-1", "kind": "continue", "move": "continue/resume-work",
  "quote": "can we get back to that?", "within": "claude-1-turn-117-f-s1-0"}])
L("turn-w8IBDL", [
 {"act": "claude-1-turn-172-f-s1-0", "kind": "define", "move": "clarify/definition",
  "quote": "Well, there are certain critical parameters."},
 {"act": "claude-1-turn-172-f-s2-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "The number of missions being chosen from is one, that was the first facade that I noticed."},
 {"act": "claude-1-turn-172-f-s3-0", "kind": "verify", "move": "verify/check-understand",
  "quote": "If I understand correctly, there were 3 missions on a whitelist."},
 {"act": "claude-1-turn-172-f-s4-0", "kind": "report", "move": "report/observation",
  "quote": "Since then, there has been at least one other one."},
 {"act": "claude-1-turn-172-f-s5-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "Now the policy"}])
L("turn-wNu4eV", [
 {"act": "claude-1-turn-118-f-s1-0", "kind": "verify", "move": "verify/status-check",
  "quote": "I don't know if that ingest is still in progress"}])
L("turn-x3Bohj", [
 {"act": "claude-1-turn-90-f-s1-0", "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "OK, this has been going for a while can you report back on progress?"}])
L("turn-x6UTP5", [
 {"act": "claude-1-turn-106-f-s1-0", "kind": "disagree", "move": "disagree/correction",
  "quote": "I don't know if we need to wait on the 230 targets for an interpretation gate"},
 {"act": "claude-1-turn-106-f-s2-0", "kind": "clarify", "move": "clarify/ask-fact",
  "quote": "So, how is interpretation *supposed* to work?", "within": "claude-1-turn-106-f-s1-0"}])
L("turn-x92YqA", [
 {"act": "claude-1-turn-192-f-s1-0", "kind": "qualify", "move": "qualify/caveat",
  "quote": "They won't always have tagged/analysed turns because that's a new feature"},
 {"act": "claude-1-turn-192-f-s1-1", "kind": "propose", "move": "propose/next-step",
  "quote": "we could find a few examples and do the analysis on them"},
 {"act": "claude-1-turn-192-f-s1-2", "kind": "explain", "move": "explain/reasoning",
  "quote": "surely some missions will have been worked on since the time when the Evidence Landscape was turned on"}])

# --- codex-10 & claude-14 sessions ---
L("turn-jADdxR", [
 {"act": "codex-10-turn-9@turn-jADdxR-f-s1-0", "kind": "continue", "move": "continue/resume-work",
  "quote": "OK please carry on with the remaining checks"}])
L("turn-jyJU5y", [
 {"act": "codex-10-turn-177-f-s1-0", "kind": "report-problem", "move": "report-problem/observed",
  "quote": "I pressed RET on META but it is very slow to load and I am impatient;"},
 {"act": "codex-10-turn-177-f-s1-1", "kind": "constrain", "move": "constrain/for-now",
  "quote": "we shouldn't have it be so intensive.", "within": "codex-10-turn-177-f-s1-0"}])
L("turn-l92VRh", [
 {"act": "codex-10-turn-12@turn-l92VRh-f-s1-0", "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "OK shall we run another click?"},
 {"act": "codex-10-turn-12@turn-l92VRh-f-s2-0", "kind": "constrain", "move": "constrain/for-now",
  "quote": "Things to look for would be: it selects a different mission",
  "within": "codex-10-turn-12@turn-l92VRh-f-s1-0"},
 {"act": "codex-10-turn-12@turn-l92VRh-f-s3-0", "kind": "propose", "move": "propose/tentative",
  "quote": "we could make it a new walk-through running example in the PLoP-2026 paper."}])
L("turn-wS7piB", [
 {"act": "codex-10-turn-7@turn-wS7piB-f-s1-0", "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Digging into the timing issues should be straightforward separately"}])
L("turn-xSJMjf", [
 {"act": "codex-10-turn-13@turn-xSJMjf-f-s1-0", "kind": "approve", "move": "approve/plain",
  "quote": "1 is authorised"}])
L("turn-jaTrCG", [
 {"act": "claude-14-turn-61-f-s1-0", "kind": "ask-action", "move": "ask-action/read-record",
  "quote": "but what code regulates the loops so I can point Rob to that?"}])
L("turn-kCioyD", [
 {"act": "claude-14-turn-48-f-s1-0", "kind": "explain", "move": "explain/observation",
  "quote": "it is almost surely the topology loops as of now"},
 {"act": "claude-14-turn-48-f-s1-1", "kind": "ask-action", "move": "ask-action/one-decision",
  "quote": "I dont know if they can be made more effucuent??"}])
L("turn-yVubZK", [
 {"act": "claude-14-turn-60-f-s1-0", "kind": "disagree", "move": "disagree/premise",
  "quote": "well, the weeks to months stuff seems a bit crazy to me."},
 {"act": "claude-14-turn-60-f-s2-0", "kind": "verify", "move": "verify/check-understand",
  "quote": "are we not formally checking known results?"},
 {"act": "claude-14-turn-60-f-s3-0", "kind": "verify", "move": "verify/status-check",
  "quote": "anyway is apm-lean and the current topo loop running material xhecked in and pushed?"}])
