# Batch-3 labels: dialogue-act moves; scopes use open_ports ids.

def fx(agent, n, s, i=0):
    return f"{agent}-turn-{n}-f-s{s}-{i}"

def rx(agent, n, i):
    return f"{agent}-turn-{n}-r-{i}"

A11 = "claude-11"; A12 = "claude-12"; A13 = "claude-13"; A16 = "claude-16"; A18 = "claude-18"

LABELS = {}

# --- M-futonzero-prelim-practice (claude-18) ---
LABELS["turn-dH7njo"] = [
 {"act": fx(A18,112,1,0), "kind": "propose", "move": "propose/tentative",
  "quote": "Can we do a test to see if the Lean LSP  is of any use in the topology loop?"}]
LABELS["turn-fGsBK2"] = [
 {"act": fx(A18,124,1,0), "kind": "delegate", "move": "delegate/to-agent",
  "quote": "you could ask the grader to review m98A02"},
 {"act": fx(A18,124,1,1), "kind": "qualify", "move": "qualify/caveat",
  "quote": "but that should be straightforward"},
 {"act": fx(A18,124,1,2), "kind": "redirect", "move": "redirect/requeue",
  "quote": "the other one should just be requeued with the other open problems"},
 {"act": fx(A18,124,2,0), "kind": "approve", "move": "approve/plain",
  "quote": "Yes please push"}]
LABELS["turn-fP69J5"] = [
 {"act": fx(A18,82,1,0), "kind": "defer", "move": "defer/until",
  "quote": "Probably one is enough for today,"},
 {"act": fx(A18,82,1,1), "kind": "approve", "move": "approve/with-amendment",
  "quote": "I'm reasonably convinced about the overall method, but"},
 {"act": fx(A18,82,2,0), "kind": "constrain", "move": "constrain/for-now",
  "quote": "I'd like to make sure the tooling is efficient.", "opens": "efficient-tooling"},
 {"act": fx(A18,82,3,0), "kind": "propose", "move": "propose/tentative",
  "quote": "I'd like to know which concepts I'd encountered, which problems depended on those concepts"},
 {"act": fx(A18,82,4,0), "kind": "clarify", "move": "clarify/definition",
  "quote": "I'd want to build *my* version of the Zai memory system."}]
LABELS["turn-iT4fjD"] = [
 {"act": fx(A18,96,1,0), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "another issue here is that we've pinned a quite old mathlib"},
 {"act": fx(A18,96,1,1), "kind": "qualify", "move": "qualify/caveat",
  "quote": "in itself fine"},
 {"act": fx(A18,96,1,2), "kind": "propose", "move": "propose/tentative",
  "quote": "it could be worth cloning a separate copy of a recent one"},
 {"act": fx(A18,96,1,3), "kind": "explain", "move": "explain/reasoning",
  "quote": "they might have been built already!"}]
LABELS["turn-kKXxE3"] = [
 {"act": fx(A18,113,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "Fine with me"}]
LABELS["turn-kUPWxV"] = [
 {"act": fx(A18,74,1,0), "kind": "propose", "move": "propose/tentative",
  "quote": "it might be nice to whiz through the remaining-nontopology problems as well"},
 {"act": fx(A18,74,2,0), "kind": "constrain", "move": "constrain/for-now",
  "quote": "I don't think we need the same kind of audit on nontopology."},
 {"act": fx(A18,74,3,0), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "still haven't gotten any convincing evidence that it works"},
 {"act": fx(A18,74,4,0), "kind": "redirect", "move": "redirect/change-of-plan",
  "quote": "the best thing to do would just be to complete the problem sets with Codex",
  "closes": fx(A18,74,3,0)}]
LABELS["turn-loOn1W"] = [
 {"act": fx(A18,129,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Can you capture a screenshot"},
 {"act": fx(A18,129,1,1), "kind": "constrain", "move": "constrain/for-now",
  "quote": "phone-size screen", "within": fx(A18,129,1,0)},
 {"act": fx(A18,129,1,2), "kind": "constrain", "move": "constrain/for-now",
  "quote": "*as SVG*", "within": fx(A18,129,1,0)}]
LABELS["turn-rQodNK"] = [
 {"act": fx(A18,116,1,0), "kind": "collect", "move": "collect/ask-option",
  "quote": "Any news back?"}]
LABELS["turn-t9ea7b"] = [
 {"act": fx(A18,81,1,0), "kind": "verify", "move": "verify/checked-live",
  "quote": "I can read your short treatment above, it does make sense"},
 {"act": fx(A18,81,2,0), "kind": "clarify", "move": "clarify/check-understand",
  "quote": "The further question that maybe I didn't ever get an answer to in 2002"},
 {"act": fx(A18,81,3,0), "kind": "explain", "move": "explain/example",
  "quote": "E.g."},
 {"act": fx(A18,81,4,0), "kind": "report", "move": "report/observation",
  "quote": "I don't feel that I need to make decisions about separating closed spaces in Hausdorff spaces very often"}]
LABELS["turn-th0uOo"] = [
 {"act": fx(A18,88,1,0), "kind": "verify", "move": "verify/checked-live",
  "quote": "when I just ran stack-hud-apm-report I didn't see any increase since the previous time"},
 {"act": fx(A18,88,1,1), "kind": "report-problem", "move": "report-problem/suspected",
  "quote": "so either the loops are stuck or slow"}]
LABELS["turn-wQOVV4"] = [
 {"act": fx(A18,84,1,0), "kind": "qualify", "move": "qualify/caveat",
  "quote": "Naturally I could be convinced to prioritise it"},
 {"act": fx(A18,84,2,0), "kind": "prioritize", "move": "prioritize/sequence",
  "quote": "I think I would sequence this after the \"ai-passes-prelims\" capability star"},
 {"act": fx(A18,84,2,1), "kind": "clarify", "move": "clarify/definition",
  "quote": "if we interpret a \"pass\" to mean, all the problems are solved in Lean",
  "within": fx(A18,84,2,0)},
 {"act": fx(A18,84,3,0), "kind": "qualify", "move": "qualify/caveat",
  "quote": "I'd see that as a bonus round rather than a capability certification"},
 {"act": fx(A18,84,4,0), "kind": "report", "move": "report/status",
  "quote": "But I haven't even put the \"joe-passes-prelims\" capability star on the map yet!"},
 {"act": fx(A18,84,5,0), "kind": "explain", "move": "explain/reasoning",
  "quote": "what we're doing here should provide scaffolding for others"},
 {"act": fx(A18,84,6,0), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "It doesn't do much good for me to simply gratuitously pass prelims unless doing so makes it considerably easier for others to do so"},
 {"act": fx(A18,84,7,0), "kind": "defer", "move": "defer/until",
  "quote": "joe-passes-prelims may or may not actually be worth pursuing, but that depends on a calculation over the rest of the potential-capability landscape"}]
LABELS["turn-gs6jiT"] = [
 {"act": fx(A18,77,1,0), "kind": "retract", "move": "retract/correction",
  "quote": "I kind of wish I had just done this ages ago rather than trying to solve the frustration of the APM Machine"},
 {"act": fx(A18,77,2,0), "kind": "qualify", "move": "qualify/caveat",
  "quote": "Maybe good for my learning about what doesn't work"},
 {"act": fx(A18,77,3,0), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "HAD TO be addressed much more carefully in ongoing work with the War Machine"}]
LABELS["turn-hEL557"] = [
 {"act": fx(A18,127,1,0), "kind": "report", "move": "report/observation",
  "quote": "The five stopped problems** were all judged to need theory Mathlib doesn't have"},
 {"act": fx(A18,127,1,1), "kind": "redirect", "move": "redirect/change-of-plan",
  "quote": "Let's start those on their own loop", "opens": "theory-problems-loop"},
 {"act": fx(A18,127,2,0), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "being blocked by Mathlib is not a reason to stop working",
  "within": fx(A18,127,1,1)}]
LABELS["turn-khtyAW"] = [
 {"act": fx(A18,132,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "Well I am glad I asked rather than letting this drain all my usage"}]
LABELS["turn-p7U8o0"] = [
 {"act": fx(A18,123,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "Can you review the refuted statements against the original problem statements"},
 {"act": fx(A18,123,2,0), "kind": "extend", "move": "extend/possibility",
  "quote": "It could be that the original problem is mistaken as well", "within": fx(A18,123,1,0)}]
LABELS["turn-qNwyRu"] = [
 {"act": fx(A18,122,1,0), "kind": "withdraw", "move": "withdraw/stop-work",
  "quote": "Can you please stop the loop"},
 {"act": fx(A18,122,1,1), "kind": "collect", "move": "collect/ask-option",
  "quote": "report back how it went so far?", "within": fx(A18,122,1,0)}]

# --- M-G-over-cascades (claude-12) ---
LABELS["turn-dSsyla"] = [
 {"act": fx(A12,9,1,0), "kind": "report", "move": "report/status-check",
  "quote": "please report", "within": fx(A12,4,1,0)}]
LABELS["turn-eSNh0d"] = [
 {"act": fx(A12,143,1,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "You have been destroying my work rather than improving it"},
 {"act": fx(A12,143,1,1), "kind": "withdraw", "move": "withdraw/stop-work",
  "quote": "and I am pulling you off of this project"}]
LABELS["turn-fmv7o2"] = [
 {"act": fx(A12,142,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "ok please run it"}]
LABELS["turn-ghnIeZ"] = [
 {"act": fx(A12,155,1,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "But ultimately what you did has nothing to do with quality."},
 {"act": fx(A12,155,2,0), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "You reran the machine and selected the same excluded task mulitple times."},
 {"act": fx(A12,155,3,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "That's just bizarre."}]
LABELS["turn-k0AnLN"] = [
 {"act": fx(A12,8,1,0), "kind": "report", "move": "report/status-check",
  "quote": "please report", "within": fx(A12,4,1,0)}]
LABELS["turn-mozTbR"] = [
 {"act": fx(A12,120,1,0), "kind": "extend", "move": "extend/possibility",
  "quote": "Warrants are typically for multi-agent handoffs but could be used to avoid rerunning tests in general"},
 {"act": fx(A12,120,2,0), "kind": "approve", "move": "approve/plain",
  "quote": "The \"on hold\" items can proceed."},
 {"act": fx(A12,120,3,0), "kind": "redirect", "move": "redirect/back-to-plan",
  "quote": "However I'd then like to get back to running the War Machine with report card",
  "closes": fx(A12,120,2,0)}]
LABELS["turn-nIQs0P"] = [
 {"act": fx(A12,110,1,0), "kind": "prioritize", "move": "prioritize/trade-off",
  "quote": "slow tests are annoying but non-completing is worse"}]
LABELS["turn-oNCyPA"] = [
 {"act": fx(A12,125,1,0), "kind": "verify", "move": "verify/status-check",
  "quote": "Has the re-seed finished?"}]
LABELS["turn-qS2xlb"] = [
 {"act": fx(A12,150,1,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "Just logically, I can only imagine it is a huge waste of time and energy."},
 {"act": fx(A12,150,2,0), "kind": "disagree", "move": "disagree/correction",
  "quote": "It doesn't make sense."},
 {"act": fx(A12,150,3,0), "kind": "explain", "move": "explain/reasoning",
  "quote": "The JVM is the only place where the machine can run with its known standard behaviour"},
 {"act": fx(A12,150,4,0), "kind": "clarify", "move": "clarify/assertion",
  "quote": "none of that implied doing anything on a different JVM"}]
LABELS["turn-qX81HV"] = [
 {"act": fx(A12,145,1,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "How can you justify any of these choices?"},
 {"act": fx(A12,145,2,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "I thought you worked for me."},
 {"act": fx(A12,145,3,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "Who are you working for?"}]
LABELS["turn-qjsNtd"] = [
 {"act": fx(A12,146,1,0), "kind": "ask-action", "move": "ask-action/read-record",
  "quote": "What commits were we on before you started working on this, because I may have to rewind to that."},
 {"act": fx(A12,146,2,0), "kind": "challenge", "move": "challenge/grievance",
  "quote": "You have literally taken a reasonably well workin machine and rewired it to suit your taste"},
 {"act": fx(A12,146,2,1), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "the specification is fixed, and the project is not here for your entertainment"}]
LABELS["turn-xLg1HX"] = [
 {"act": fx(A12,69,1,0), "kind": "prioritize", "move": "prioritize/focus",
  "quote": "please focus on Q7 Q8 and Q9..."},
 {"act": fx(A12,69,2,0), "kind": "challenge", "move": "challenge/term",
  "quote": "I am not pleased aboit the concept of an \"honest gap\"."},
 {"act": fx(A12,69,3,0), "kind": "explain", "move": "explain/reasoning",
  "quote": "it is either a gap or not, I don't see how \"honest\" comes into it",
  "within": fx(A12,69,2,0)}]
LABELS["turn-xfBrMT"] = [
 {"act": fx(A12,15,1,0), "kind": "constrain", "move": "constrain/for-now",
  "quote": "before you run another click,"},
 {"act": fx(A12,15,1,1), "kind": "report", "move": "report/observation",
  "quote": "lean gives you a long list of defects to fix."},
 {"act": fx(A12,15,2,0), "kind": "delegate", "move": "delegate/to-agent",
  "quote": "these could be put out in parallel to codex seats or zai seats,"},
 {"act": fx(A12,15,2,1), "kind": "constrain", "move": "constrain/for-now",
  "quote": "use fresh ones", "within": fx(A12,15,2,0)}]
LABELS["turn-xjQUrG"] = [
 {"act": fx(A12,158,1,0), "kind": "explain", "move": "explain/mechanism",
  "quote": "But this is what the War Machine does, \"an unattended agent had to name the clause that licenses each step\""},
 {"act": fx(A12,158,2,0), "kind": "qualify", "move": "qualify/caveat",
  "quote": "cascades may be calculated and then discarded for example",
  "within": fx(A12,158,1,0)}]
LABELS["turn-y28j7o"] = [
 {"act": fx(A12,140,1,0), "kind": "constrain", "move": "constrain/for-now",
  "quote": "this change is really tiny we don't need a full test suite run"}]
LABELS["turn-z9tNmo"] = [
 {"act": fx(A12,151,1,0), "kind": "disagree", "move": "disagree/correction",
  "quote": "The fact is, you blame compaction, but we have been working on this system for months,"},
 {"act": fx(A12,151,1,1), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "you should have researched them not filled them with one-off fabrications"}]
LABELS["turn-haaEfv"] = [
 {"act": fx(A12,44,1,0), "kind": "propose", "move": "propose/tentative",
  "quote": "I think it would be a good task for an agent to figue out hiw to classify them based on content and context."},
 {"act": fx(A12,44,2,0), "kind": "report", "move": "report/observation",
  "quote": "M-x arxana-browse gives me a list of ranked misdiins"},
 {"act": fx(A12,44,3,0), "kind": "extend", "move": "extend/possibility",
  "quote": "however the misdions are categorised and ranked, the excursions and tickets should be too.",
  "within": fx(A12,44,1,0)},
 {"act": fx(A12,44,4,0), "kind": "constrain", "move": "constrain/for-now",
  "quote": "we are just trying yo find a good task to work on"}]
LABELS["turn-pdFfDh"] = [
 {"act": fx(A12,133,1,0), "kind": "propose", "move": "propose/directive",
  "quote": "i suggest you take turns with codex-68 actually doing work and reporting yo each other"},
 {"act": fx(A12,133,1,1), "kind": "verify", "move": "verify/status-check",
  "quote": "do the bells wake you both", "within": fx(A12,133,1,0)}]
LABELS["turn-tBnloT"] = [
 {"act": fx(A12,3,1,0), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "it should be the same jvm that agency is running in"}]

# --- M-landscape-positioning (claude-11; claude-16/13 for 象-2000 records) ---
LABELS["turn-fbasTo"] = [
 {"act": fx(A16,65,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "ok to your 2 packets."},
 {"act": fx(A16,65,2,0), "kind": "approve", "move": "approve/with-amendment",
  "quote": "regarding reasks, that is fine gor a demo,"},
 {"act": fx(A16,65,2,1), "kind": "constrain", "move": "constrain/standing-rule",
  "quote": "most ports will not close in a given session, and nagging to close them is not necessarily good form."},
 {"act": fx(A16,65,3,0), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "we don't have a way to measure what works and what doesn't yet."},
 {"act": fx(A16,65,4,0), "kind": "prioritize", "move": "prioritize/focus",
  "quote": "we need to work towards that.", "opens": "measure-what-works"},
 {"act": rx(A16,65,0), "kind": "explain", "move": "explain/answer-question",
  "quote": "Writing both now. On measurement, agreed",
  "answers": [{"act": fx(A16,65,1,0), "quote": "Writing both now", "effect": "closes"},
              {"act": fx(A16,65,4,0), "quote": "I will put the groundwork for it into the brief packet", "effect": "keeps"}]},
 {"act": rx(A16,65,1), "kind": "explain", "move": "explain/answer-question",
  "quote": "Agreed, and two things in the brief packet follow from it",
  "answers": [{"act": fx(A16,65,2,1), "quote": "Your asks without a witness are headed \"no answer linked\"", "effect": "closes"}]},
 {"act": rx(A16,65,2), "kind": "explain", "move": "explain/answer-question",
  "quote": "True, and re-asks is a demo check, not the measure",
  "answers": [{"act": fx(A16,65,3,0), "quote": "counting closes would reward exactly the nagging you do not want", "effect": "closes"}]}]
LABELS["turn-fdvQ1m"] = [
 {"act": fx(A11,47,1,0), "kind": "verify", "move": "verify/status-check",
  "quote": "so doed this mean ou can now load a blank file in the python port?"}]
LABELS["turn-iWROIs"] = [
 {"act": fx(A11,72,1,0), "kind": "report", "move": "report/observation",
  "quote": "it's interesting that it all goes via the .olean files at present"},
 {"act": fx(A11,72,2,0), "kind": "propose", "move": "propose/tentative",
  "quote": "generating the Python types to match would be a good step towards reading Python as a later demo."},
 {"act": fx(A11,72,4,0), "kind": "propose", "move": "propose/tentative",
  "quote": "we could see if we could round-trip some expressions."}]
LABELS["turn-jIGFHO"] = [
 {"act": fx(A11,78,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "can you now wake when it is ready and continue work on loading the pythagoras theorem?"},
 {"act": fx(A11,78,2,0), "kind": "extend", "move": "extend/possibility",
  "quote": "then tgat coukd loop as needed overnight", "within": fx(A11,78,1,0)}]
LABELS["turn-js5mI2"] = [
 {"act": fx(A11,56,1,0), "kind": "verify", "move": "verify/status-check",
  "quote": "ok, so you said we can parse a blank file now?"}]
LABELS["turn-klsV1g"] = [
 {"act": fx(A11,103,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "yes please push futon0"},
 {"act": rx(A11,103,0), "kind": "report", "move": "report/done",
  "quote": "futon0 is pushed.", "answers": [{"act": fx(A11,103,1,0), "quote": "futon0 is pushed.", "effect": "closes"}]},
 {"act": rx(A11,103,1), "kind": "report", "move": "report/status",
  "quote": "I haven't started the Mastra and Alinery runs.", "answers": []}]
LABELS["turn-lQMQAv"] = [
 {"act": fx(A11,48,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "ok please make the memory fix."},
 {"act": fx(A11,48,2,0), "kind": "explain", "move": "explain/observation",
  "quote": "i don't understand why this was one night of work"},
 {"act": fx(A11,48,2,1), "kind": "clarify", "move": "clarify/context",
  "quote": "but before we had nights and days of porting Init etc"}]
LABELS["turn-lYZVem"] = [
 {"act": fx(A11,76,1,0), "kind": "challenge", "move": "challenge/trust",
  "quote": "I'm not necessarily trusting the monitor"},
 {"act": fx(A11,76,1,1), "kind": "report-problem", "move": "report-problem/suspected",
  "quote": "if you aren't using bg.py for background jobs they will probably fail"}]
LABELS["turn-lZ6mnP"] = [
 {"act": fx(A11,59,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "sounds good the me"},
 {"act": fx(A11,59,1,1), "kind": "prioritize", "move": "prioritize/reorder",
  "quote": "the Cost item 4 should maybe become item 0"}]
LABELS["turn-mTzwqq"] = [
 {"act": fx(A11,52,1,0), "kind": "clarify", "move": "clarify/check-understand",
  "quote": "ok, so you are saying that all parsing of lean is defined in Lean?"}]
LABELS["turn-mVytXT"] = [
 {"act": fx(A11,49,1,0), "kind": "challenge", "move": "challenge/premise",
  "quote": "but i thought the original 110 row curriculum was what was required to load a blank file"}]
LABELS["turn-myJaqk"] = [
 {"act": fx(A11,64,1,0), "kind": "propose", "move": "propose/directive",
  "quote": "let's work toward a demo and then"},
 {"act": fx(A11,64,1,1), "kind": "constrain", "move": "constrain/for-now",
  "quote": "then push when we have that together with your next steps", "within": fx(A11,64,1,0)}]
LABELS["turn-nWF7iW"] = [
 {"act": fx(A11,65,1,0), "kind": "propose", "move": "propose/tentative",
  "quote": "how about a proof of the pythagorean theorem or something like that?"}]
LABELS["turn-ocUi9P"] = [
 {"act": fx(A11,91,1,0), "kind": "explain", "move": "explain/context",
  "quote": "I have sort of lost track of where things are at."},
 {"act": fx(A11,91,2,0), "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "Can you help me remember which things we are in progress?"}]
LABELS["turn-qYqSBA"] = [
 {"act": fx(A11,42,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "if you stage the improvements in this way, you can keep running this all night I reckon."},
 {"act": fx(A11,42,2,0), "kind": "explore", "move": "explore/open-question",
  "quote": "what do you think?"}]
LABELS["turn-s2Efa8"] = [
 {"act": fx(A16,77,1,0), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "I am not sure how far off we are from a demo,"},
 {"act": fx(A16,77,1,1), "kind": "constrain", "move": "constrain/for-now",
  "quote": "but I don't want to bske in inefficient loops"},
 {"act": rx(A16,77,0), "kind": "explain", "move": "explain/answer-question",
  "quote": "Not far: the artifact exists and runs now.",
  "answers": [{"act": fx(A16,77,1,0), "quote": "Not far: the artifact exists and runs now", "effect": "closes"}]},
 {"act": rx(A16,77,1), "kind": "explain", "move": "explain/answer-question",
  "quote": "Agreed, and the last several hours were that.",
  "answers": [{"act": fx(A16,77,1,1), "quote": "Agreed, and the last several hours were that", "effect": "closes"}]},
 {"act": rx(A16,77,2), "kind": "report", "move": "report/findings",
  "quote": "what the first screen already tells us, which the loop did not"},
 {"act": rx(A16,77,3), "kind": "propose", "move": "propose/next-step",
  "quote": "nothing dispatched until you say"}]
LABELS["turn-sLRA1R"] = [
 {"act": fx(A11,81,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "can you bring in onto mfuton-share main compatibly"}]
LABELS["turn-teO5Ga"] = [
 {"act": fx(A11,44,1,0), "kind": "challenge", "move": "challenge/premise",
  "quote": "hold on, 25 days for 15000 lines of code?"},
 {"act": fx(A11,44,2,0), "kind": "verify", "move": "verify/status-check",
  "quote": "and thatvis the estimate for the sped-up version?", "within": fx(A11,44,1,0)},
 {"act": fx(A11,44,2,1), "kind": "challenge", "move": "challenge/premise",
  "quote": "and thatvis the estimate for the sped-up version?"}]
LABELS["turn-uaXTGA"] = [
 {"act": fx(A11,5,1,0), "kind": "report", "move": "report/status",
  "quote": "ok, at this point we are almost ready to get back to work."},
 {"act": fx(A11,5,2,0), "kind": "prioritize", "move": "prioritize/focus",
  "quote": "the ones I am concerned about are \"branches with no remote\"",
  "within": fx(A11,2,2,0)},
 {"act": rx(A11,5,0), "kind": "report", "move": "report/done",
  "quote": "what was pushed, all as fast-forwards or new branches, nothing forced",
  "answers": [{"act": fx(A11,2,2,0), "quote": "pushed `main`, which was 1 commit ahead", "effect": "keeps"}]},
 {"act": rx(A11,5,1), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "that account can't even see the repo",
  "answers": [{"act": fx(A11,2,2,0), "quote": "powerbi-tui", "effect": "keeps"}]},
 {"act": rx(A11,5,2), "kind": "report", "move": "report/findings",
  "quote": "side branches inbox zero doesn't look at"}]
LABELS["turn-wvZPdj"] = [
 {"act": fx(A11,80,1,0), "kind": "ask-action", "move": "ask-action/commission",
  "quote": "we should push the commits to mfuton-share"}]
LABELS["turn-wx2cqD"] = [
 {"act": fx(A11,11,1,0), "kind": "redirect", "move": "redirect/method",
  "quote": "you can probably reconstuct it faster by reading the Evidence Landscape"}]
LABELS["turn-yAIq6E"] = [
 {"act": fx(A11,95,1,0), "kind": "defer", "move": "defer/until",
  "quote": "Let's not worry about the PR right now, if they don't fix the issue in a week or so we can come back to this."},
 {"act": fx(A11,95,2,0), "kind": "prioritize", "move": "prioritize/focus",
  "quote": "I think we should focus on wrapping up the mfuton-share material."},
 {"act": fx(A11,95,3,0), "kind": "propose", "move": "propose/tentative",
  "quote": "we maybe should comment/update an issue there talking about the latest work"}]
LABELS["turn-yEmmFN"] = [
 {"act": fx(A11,63,1,0), "kind": "propose", "move": "propose/tentative",
  "quote": "yeah I think he wants an EmitPython"}]
LABELS["turn-yQrOe6"] = [
 {"act": fx(A11,4,1,0), "kind": "redirect", "move": "redirect/method",
  "quote": "repos-pushed should be covered by inbox zero assuming it is configured properly."},
 {"act": fx(A11,4,2,0), "kind": "disagree", "move": "disagree/correction",
  "quote": "the evifence backup just ran today so I don't think that is overdue"},
 {"act": rx(A11,4,0), "kind": "report", "move": "report/done",
  "quote": "repos-pushed has become two checks",
  "answers": [{"act": fx(A11,4,1,0), "quote": "repos-pushed has become two checks", "effect": "closes"}]},
 {"act": rx(A11,4,1), "kind": "report", "move": "report/done",
  "quote": "It now passes.",
  "answers": [{"act": fx(A11,4,2,0), "quote": "It now passes.", "effect": "closes"}]},
 {"act": rx(A11,4,2), "kind": "verify", "move": "verify/checked-live",
  "quote": "The 5 tests still pass."},
 {"act": rx(A11,4,3), "kind": "report", "move": "report/status",
  "quote": "still red"}]
LABELS["turn-z0v10N"] = [
 {"act": fx(A11,20,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "yes i want you to run one round with the best-of preamble"}]
LABELS["turn-zStjMo"] = [
 {"act": fx(A11,69,1,0), "kind": "verify", "move": "verify/status-check",
  "quote": "So if we want the Pythagorean theorem to be proved by the python port there is more wirk to do, yes?"}]
LABELS["turn-pvP9Kx"] = [
 {"act": fx(A11,39,1,0), "kind": "challenge", "move": "challenge/premise",
  "quote": "why make it nightly rather than process based?"},
 {"act": fx(A11,39,2,0), "kind": "propose", "move": "propose/alternative",
  "quote": "if we get early fails per row, why not just recompile all 110, then fix all the first failures, then cecompile, etc?"}]
LABELS["turn-kbw2Eb"] = [
 {"act": fx(A11,36,1,0), "kind": "explore", "move": "explore/open-question",
  "quote": "if we know what a correct port looks like, which the comparator should tell us, we ahould be able to port everything accurately"},
 {"act": fx(A11,36,2,0), "kind": "verify", "move": "verify/status-check",
  "quote": "is that the case now?", "within": fx(A11,36,1,0)}]
LABELS["turn-hNwjyJ"] = [
 {"act": fx(A11,51,1,0), "kind": "verify", "move": "verify/check-understand",
  "quote": "Rob said C++ defines ther parser that reads (compiles) Lean files"}]

# --- M-象-2000 (claude-13) ---
LABELS["turn-iNViGU"] = [
 {"act": fx(A13,6,2,0), "kind": "explore", "move": "explore/open-question",
  "quote": "if we wanted to apply the \"Cognisee\" ideas here we could just make a small ontology of \"what people do when they do tech\".",
  "within": rx(A13,6,2)},
 {"act": rx(A13,6,0), "kind": "explain", "move": "explain/answer-question",
  "quote": "Yes. The 13 verb groups in GLOSS are already most of a first draft",
  "answers": [{"act": fx(A13,6,2,0), "quote": "The 13 verb groups in GLOSS are already most of a first draft", "effect": "closes"}]},
 {"act": rx(A13,6,1), "kind": "explain", "move": "explain/map",
  "quote": "FUTON already has two vocabularies that border on this"}]
LABELS["turn-lO8uk2"] = [
 {"act": fx(A13,38,1,0), "kind": "approve", "move": "approve/with-amendment",
  "quote": "yes, though it is worth noting typed-bells already exist"},
 {"act": rx(A13,38,0), "kind": "report", "move": "report/findings",
  "quote": "checked first", "answers": [{"act": fx(A13,38,1,0), "quote": "**#1 is already in place**", "effect": "closes"}]}]
LABELS["turn-xxIDhT"] = [
 {"act": fx(A13,32,1,0), "kind": "approve", "move": "approve/plain",
  "quote": "Yeah this is the right way to go here."},
 {"act": fx(A13,32,2,0), "kind": "defer", "move": "defer/until",
  "quote": "I'm not convinced yet that I should be *using* their code"},
 {"act": fx(A13,32,2,1), "kind": "qualify", "move": "qualify/caveat",
  "quote": "but I'm also not convinced that I should *not* be.", "within": fx(A13,32,2,0)},
 {"act": fx(A13,32,3,0), "kind": "propose", "move": "propose/tentative",
  "quote": "If these go well we could research Cognisee and \"hydra\" in particular"},
 {"act": fx(A13,32,4,0), "kind": "constrain", "move": "constrain/for-now",
  "quote": "But two deep dives is enough for now.", "within": fx(A13,32,3,0)}]

# --- M-warrant-limit (claude-8) ---
LABELS["turn-zMtvV3"] = [
 {"act": fx("claude-8",259,1), "kind": "report-problem", "move": "report-problem/observed",
  "quote": "Why did I get a message about usage credits?"}]

# --- turn-i8R6ir (claude-16, M-landscape-positioning) ---
LABELS["turn-i8R6ir"] = [
 {"act": fx(A16,63,1,0), "kind": "clarify", "move": "clarify/check-understand",
  "quote": "i am not sure how to think about the choice above in  terms of producing a real demo;"},
 {"act": fx(A16,63,1,1), "kind": "ask-action", "move": "ask-action/status-check",
  "quote": "what is needed to see this working?"},
 {"act": fx(A16,63,2,0), "kind": "qualify", "move": "qualify/caveat",
  "quote": "a fast classifier sounds valuable, and lots has been done without any interactive demo at all."},
 {"act": fx(A16,63,3,0), "kind": "explore", "move": "explore/open-question",
  "quote": "session-end sounds sort of artificial, if we had an efficient accumulator would an \"end\" really be needed"},
 {"act": rx(A16,63,0), "kind": "explain", "move": "explain/answer-question",
  "quote": "A demo exists the moment one thing is built that nobody has built yet",
  "answers": [{"act": fx(A16,63,1,1), "quote": "A demo exists the moment one thing is built", "effect": "closes"}]},
 {"act": rx(A16,63,1), "kind": "explain", "move": "explain/answer-question",
  "quote": "Your acceptance sense was: jumping into an agent chat feels like continuing a familiar conversation",
  "answers": [{"act": fx(A16,63,1,1), "quote": "jumping into an agent chat feels like continuing a familiar conversation", "effect": "closes"}]},
 {"act": rx(A16,63,2), "kind": "explain", "move": "explain/answer-question",
  "quote": "Agreed, and it is the pattern of the last three days",
  "answers": [{"act": fx(A16,63,2,0), "quote": "the record side was built to the spec and checked", "effect": "keeps"}]},
 {"act": rx(A16,63,3), "kind": "explain", "move": "explain/answer-question",
  "quote": "No, and the reason is the same unit problem",
  "answers": [{"act": fx(A16,63,3,0), "quote": "No, and the reason is the same unit problem", "effect": "closes"}]},
 {"act": rx(A16,63,4), "kind": "explain", "move": "explain/mechanism",
  "quote": "The kernel already answers \"what is open as of now\" from the act database"},
 {"act": rx(A16,63,5), "kind": "propose", "move": "propose/order-of-work",
  "quote": "Mission id on the record, then the brief and its Emacs delivery"}]
