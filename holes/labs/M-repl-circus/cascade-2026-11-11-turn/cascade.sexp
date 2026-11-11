;;; -*- mode: lisp-data -*-
;;; One turn read as a pattern cascade (CASCADE-SPEC-v3 §2-3).
;;; #N=(element ...) names a situation element where it first appears;
;;; #N# is the same element again.  Two complete occurrences that hold
;;; the same element are glued there.
(cascade
  (situation (turn "emacs-d1ceee43f003a912322bce0c0f951169" :at "2026-11-11T16:15:24.046120661Z")
    "OK, so our aim is to work through M-repl-circus completely, and this is scoped as \"multi-day\" though my guess is it will probably take us less than one day. The first criterion that I think we need to look at is producing an actual \"legal\"…")
  (reading :candidates 26 :complete 2 :partial 12 :void 12)
  (occurrence cascade-construction/read-what-exists-first
    (IF "A cascade is to be constructed for a problem."
        (element legal-cascade-per-turn-criterion
            "The first criterion that I think we need to look at is producing an actual \"legal\" cascade for each turn."))
    (HOWEVER "Reading takes time, and a plausible cascade can be written from the problem statement in minutes."
        #1=(element scoped-multiday-guess-less-than-one-day
            "this is scoped as \"multi-day\" though my guess is it will probably take us less than one day."))
    (THEN "Read the four sources first and record where they disagree."
        #2=(element moran-paper-reread-figure-1
            "./futon5a/essays/moran-1971-agent-cascades/reading-moran-1971.md is worth rereading and figure-1-cascade-of-64-patterns.png is, I think an example from the Moran paper that I found stylistically nice."))
    (BECAUSE "The disagreements decide which patterns the cascade needs; a cascade built without them restates the requirement."
        #3=(element voxterm-diagram-tab-start-incomplete
            "We had some start on something like this in the Voxterm \"Diagram\" tab but it wasn't complete yet.")))
  (occurrence forward-model/the-forward-model-is-a-pattern-cascade
    (IF "The stack has both a retrospective strategy layer, attested rulings composed into a semilattice, and mission-level forward models such as the backlog cascade, …"
        (element local-cascades-local-regions-inside-gcc
            "The relevance to the GCC is is that the local cascades would all be local regions inside the GCC."))
    (HOWEVER "Every existing forward model boxes the future as missions; none characterizes the future build through patterns, so the reusable form of how we build stays imp…"
        #3#)
    (THEN "Model the next half-million lines as a cascade whose boxes are patterns and whose holes are the patterns that would have to be authored to reach the terminals,…"
        (element wiring-diagram-session-cascades-hook-up
            "some kind of wiring diagram that would run through a session, probably showing how the individual turn cascades hook up into a bigger diagram"))
    (BECAUSE "Patterns are reusable where missions are one-off. A future modeled as patterns tells you not just what to build but the shape of building it, and it composes w…"
        #4=(element turn-potentially-cascade-null-if-none
            "each turn can be thought of as \"potentially\" a cascade (a null cascade if no patterns are found)")))
  (glues
    (#3# (cascade-construction/read-what-exists-first BECAUSE) (forward-model/the-forward-model-is-a-pattern-cascade HOWEVER)))
  (partial peeragogy/use-or-make 3/4
    (IF "The pattern operates on the axis: novelty production ↔ deliberate reuse. Irreducible: a single decision cannot simultaneously be a fresh production and a reuse…"
        #5=(element mosaic-referent-display-pictures
            "I want to lean into the \"Mosaic\" referent over there so that we can display pictures, even though Mojira is largely a text-based browser."))
    (HOWEVER "Real costs: producing new artefacts builds skill, identity, and visible output; reusing existing artefacts compounds prior work and avoids reinventing wheels. …"
        (element forty-years-clickable-graphics-conkeror
            "we have nearly 40 years or improvements in what \"graphics\" means so we can make graphics that are clickable or whatever we want (including navigable/interactable via Conkeror style text hints!)"))
    (THEN "Operate an explicit move on the named axis (see substructure below); steward the pattern against the declared discipline. + COMPOSITIONS: (1) Reuse search befo…"
        #2#)
    (BECAUSE ""
        (void "pattern-part-missing (BECAUSE is —)")))
  (partial math-strategy/construction-before-estimates 3/4
    (IF "The object exists as a total, well-defined thing before any estimate is attempted, then the estimate is a statement about a fixed object and can be proved by o…"
        #6=(element category-theory-model-underneath
            "Of course, underneath that we'll have our category theory model."))
    (HOWEVER "The tempting order is the opposite one, because the estimate is what the construction is *for*: define the first few pieces, check the bound, and let the remai…"
        #5#)
    (THEN "Write down the whole construction first, including the behaviour on every case and at the boundaries between them. Prove it well defined. Only then state and p…"
        (void "no match: the turn does not state the construction-first order, it only enacts it"))
    (BECAUSE "An estimate is a statement about an object, so it has no content until the object exists. Interleaving the two produces an argument in which neither half can b…"
        #7=(element graphical-representation-for-cascade-needed
            "Anyway, I think we need to work out a graphical representation for a cascade.")))
  (partial proof-search/typed-hole-as-frontier 2/4
    (IF "A piece of formal work is incomplete and must be recorded, handed off, or prioritized."
        #3#)
    (HOWEVER "\"Incomplete\" as prose is unqueryable: nobody can list the gaps, rank them, cluster them, or hand one to an agent — the knowledge of what's missing lives in hea…"
        (void "no match: the incompleteness is named, not complained-of as unqueryable"))
    (THEN "Write the hole with its type: WANT (the signature or codomain of what's missing — what would have to be true/constructed), HAVE (the endpoints in hand), and th…"
        #8=(element acceptance-condition-graphical-stepper
            "the acceptance condition for this criterion is to develop a *graphical* stepper."))
    (BECAUSE "A typed hole is a proof obligation with an interface — it can be matched against candidate fillers mechanically (does this lemma's conclusion unify with the WA…"
        (void "no match")))
  (partial cascade-construction/borrow-a-sibling-cascade 2/4
    (IF "A new problem resembles one an existing cascade addresses."
        #2#)
    (HOWEVER "Analogies suggest steps the new setting does not need and hide steps it does."
        (void "no match: no analogy-misleads tension voiced"))
    (THEN "Walk the sibling's rows one by one and record fit, counterpart or gap."
        #3#)
    (BECAUSE "Gaps between siblings are exactly where the settings differ, which is what the new cascade has to handle."
        (void "no match")))
  (partial translation/bind-to-the-source 2/4
    (IF "A translated unit is used later."
        #6#)
    (HOWEVER "Names and line numbers look like bindings and are not."
        (void "no match: no names-look-like-bindings tension voiced"))
    (THEN "Bind by content hash and named form, and refuse on drift."
        (void "no match: no content-hash binding demanded"))
    (BECAUSE "Everything else about the translation is checked relative to this link."
        #7#))
  (partial pattern-interpretation/declare-the-universe 1/4
    (IF "An interpretation introduces or relies on facts."
        #4#)
    (HOWEVER "Unknown is the safe-looking default and stalls cascades; agents invent near-synonyms; and grain chosen per item explodes the universe."
        (void "no second element: the safe-looking-default tension is voiced on the same element as I1"))
    (THEN "Apply the six admission rules before any interpretation using the fact is admitted."
        (void "no match: no admission-rule application in the turn"))
    (BECAUSE "The fact vocabulary is shared by every pattern in the cascade, so an error in it affects all of them at once."
        (void "no match: no shared-fact-vocabulary argument in the turn")))
  (partial data-mining/gates-as-code 1/4
    (IF "Your pipeline has acceptance criteria for mined records (a label distribution, a vocabulary, a provenance rule, an evidence requirement)."
        #8#)
    (HOWEVER "Criteria kept as prose are re-judged by hand each run, inconsistently, and tend toward rubber-stamping. And if the same agent both produces the data and judges…"
        (void "no match: no prose-criteria rubber-stamping tension voiced"))
    (THEN "Write one checker that takes a file of records, prints PASS/FAIL per band with the numbers, and exits nonzero on any hard-gate fail. Run it on (a) the smoke ou…"
        (void "no match: no checker is written or demanded"))
    (BECAUSE "A counted, independent gate is the difference between \"we reviewed it\" and \"we checked it.\" Cross-checking the golden against the gate is mutual validation: a …"
        (void "no match")))
  (partial agent/handoff-preserves-context 1/4
    (IF "Continuity matters: the work is incremental, context is expensive to rebuild, or progress would be lost."
        (element aim-work-through-mreplcircus-completely
            "OK, so our aim is to work through M-repl-circus completely, and this is scoped as \"multi-day\""))
    (HOWEVER "Without explicit handoff, successors face cold starts: they repeat discoveries, miss constraints, or contradict prior decisions."
        (void "no match: no cold-start/successor tension voiced"))
    (THEN "Define a handoff record that captures: current state hypothesis, decisions made and why, open questions, scope boundaries, next actions, evidence/trail anchors…"
        (void "no match: no handoff record defined"))
    (BECAUSE "Handoffs convert ephemeral session state into persistent project state. They enable distributed and asynchronous work without context collapse. + evidence: Ful…"
        (void "no match")))
  (partial process/built-but-not-wired-invisibility 1/4
    (IF "A component exists, elaborates or runs, and nothing on its intended path calls it."
        #3#)
    (HOWEVER "Binary accounting cannot represent inhabited-but-unconsumed, so it reads as present. + VIOLATION (WM): four confirmed instances, including the R14 dial and fix…"
        (void "no match"))
    (THEN "Treat consumption as a checked rung (the conventions note's rung grammar: constructed then wired, never skipping); wire the acceptance authority to failure pat…"
        (void "no match"))
    (BECAUSE "Each instance is individually correct and the defect lives only in the wiring — no per-component test can catch it; only a rung that refuses on unconsumed mach…"
        (void "no match")))
  (partial cascade-construction/add-a-pattern-when-an-item-fits-no-class 1/4
    (IF "Some items are acted on by no pattern."
        #4#)
    (HOWEVER "The nearest class usually looks close enough."
        (void "no match"))
    (THEN "Name the gap, ask about the distinction, census the real items, and add the pattern."
        (void "no match"))
    (BECAUSE "The distinction the requirement missed is often one the operator already makes without having said it."
        (void "no match")))
  (partial cascade-construction/run-it-on-a-real-case 1/4
    (IF "A real recorded case of the problem exists."
        #2#)
    (HOWEVER "The design already looks right, and running it takes an hour."
        (void "no match"))
    (THEN "Run it, compare, and measure the counterfactual difference."
        (void "no match"))
    (BECAUSE "The case shows reproduction, implementation divergence and actual benefit, none of which reading the cascade shows."
        (void "no match")))
  (partial writing-coherence/hedged-lift 1/4
    (IF "The pattern operates on the axis: claim strength ↔ authorial safety. Irreducible: a sentence cannot maximise both strength and safety; the hedge is the visible…"
        #1#)
    (HOWEVER "Real costs: every claim exposes the writer to challenge; the stronger the claim, the larger the exposure. Hedges reduce exposure but also reduce what the reade…"
        (void "no match"))
    (THEN "Operate an explicit move on the named axis (see substructure below); steward the pattern against the declared discipline. + COMPOSITIONS: (1) Commit and defend…"
        (void "no match"))
    (BECAUSE ""
        (void "pattern-part-missing (BECAUSE is —)")))
  (void problems/llm-over-corpus-mining-lessons social/ARGUMENT devmap-coherence/ifr-f1-dhammavicaya data-mining/saturate-the-accelerator-with-concurrency problems/devmap-self-consistency-standards futon-theory/single-source-of-truth peeragogy/creating-a-guide system-coherence/correct-effect-claims-with-relevant-denominators war-room/wr-18-war-machine-is-demonstrated-not-hypothesised cycle-machine/step-machine workshop/records-carry-warrant memory/verify-in-the-serving-process))
