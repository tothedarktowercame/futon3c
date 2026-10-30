# T2: Argumentation games and computational models of argument as game-like search

Verification legend. **[V]** means fetched or confirmed this session from the linked page, PDF or search result. **[K]** means a canonical reference given from domain knowledge with a DOI or arXiv link, but its page was not re-fetched this session. The report writer should treat [K] bibliographic details (pages, exact titles) as needing a spot check before final publication.

Abstract vs structured: **abstract** = Dung AFs, where arguments are atomic nodes and attack is a binary relation (ICCMA Main/Approximate/Dynamic tracks). **Structured** = ASPIC+, ABA, DeLP, where arguments are built from rules and premises and attacks are derived (undermine/rebut/undercut). The standard pipeline is structured -> induced Dung AF -> semantics, and Pease et al. 2017 follows it (ASPIC+ via TOAST -> Dung AF -> grounded extension). The ICCMA 2023 paper itself states the field's split: "abstract formalisms ... and structured formalisms" [V] ([Järvisalo, Lehtonen, Niskanen, ICCMA 2023 overview, AIJ 2025](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)).

## Q1. Which dialogue/proof-procedure games decide grounded, preferred, stable or admissible acceptance with a computed (not judged) win condition?

### Takeaway
There is a mature family of two-player Proponent/Opponent argument games. In each one, "P wins" is a purely structural property of the game tree (the opponent has no legal reply, or no winning strategy exists for O). Winning strategies are proven sound and complete for the target semantics. So the reward is computed, never judged. The canonical games are: Dung's grounded/admissible dispute trees (1995); Vreeswijk & Prakken's credulous and sceptical preferred games (2000); Dunne & Bench-Capon's TPI disputes; Modgil & Caminada's survey of proof theories (2009); Caminada's grounded discussion/persuasion games and Socratic preferred game; and Prakken's persuasion-dialogue systems, which generalise these to multi-move protocols with commitment stores.

### Cited Findings
- Dung 1995 introduced abstract AFs (A, R) and the admissible/preferred/stable/grounded/complete semantics. Grounded = least fixed point of the characteristic function. Venue: *Artificial Intelligence* 77(2):321–357. [K] — [Dung 1995, AIJ](https://doi.org/10.1016/0004-3702(94)00041-X)
- **Grounded game** (standard form, as presented in Modgil & Caminada 2009). P moves first with the query argument. Players alternate, each move attacking the previous one. P may not repeat an argument within a dispute line, and P's moves must *strictly* defeat (no counter-attack back on the same move). P wins a dispute if O cannot move. The argument is grounded-accepted iff P has a winning strategy, i.e. a finite dispute tree in which every O-branch ends with a P move. [K] — [Modgil & Caminada, "Proof theories and algorithms for abstract argumentation frameworks", in Rahwan & Simari (eds.) *Argumentation in Artificial Intelligence*, Springer 2009, pp. 105–129](https://doi.org/10.1007/978-0-387-98197-0_6)
- **Credulous preferred (= admissible) game.** O may not repeat its own arguments, and P wins if it can make the set of its arguments conflict-free and defend every O attack. **Sceptical preferred game** (Vreeswijk & Prakken 2000). P must additionally show that every attacker of the query is itself out in every preferred extension. This requires a two-phase game in which O can switch roles. [K] — [Vreeswijk & Prakken, "Credulous and sceptical argument games for preferred semantics", JELIA 2000, LNCS 1919, pp. 239–253](https://doi.org/10.1007/3-540-40006-0_17)
- **TPI (two-party immediate response) disputes.** These are games for credulous and sceptical preferred acceptance, with proofs of properties and efficiency (exponential length is unavoidable in the worst case, consistent with the complexity bounds). [K] — [Dunne & Bench-Capon, "Two party immediate response disputes: properties and efficiency", AIJ 149(2):221–250, 2003](https://doi.org/10.1016/S0004-3702(03)00076-6)
- Caminada and co-authors recast semantics as *discussion games* with natural-language-like moves. The grounded discussion game uses moves HTB ("has to be in"), CB ("can't be"), CONCEDE and RETRACT. It is polynomial in length, and P wins iff the argument is in the grounded labelling. Related titles: "Grounded semantics as persuasion dialogue" (Caminada & Podlaszewski, COMMA 2012); "A discussion game for grounded semantics" (Caminada, TAFA 2015); "Preferred semantics as Socratic discussion" (Caminada, Dvořák, Vesic, *J. Logic Comput.* 2016). [K, titles from domain knowledge; not fetched this session — see Gaps]
- Prakken's persuasion dialogue systems generalise the two-player games above. They allow multiple replies, backtracking and commitment stores. "Coherence and flexibility in dialogue games for argumentation" (*J. Logic Comput.* 15(6), 2005) proves that the protocol's "current winner" is determined by the grounded status of the dialogue's dialectical graph. This is a computed *turn-winner* rule. [K] — [Prakken 2006, "Formal systems for persuasion dialogue", *Knowledge Engineering Review* 21(2):163–188](https://doi.org/10.1017/S0269888906000865)
- **Applied instance with a computed "current state" (Pease et al.).** In the Lakatos game, the dialogue game "defines ordered operations on a shared information state represented in the Argument Interchange Format (AIF)". That state "is then interpreted by ... TOAST ... and passed on to DungOMatic ... to calculate the grounded extension, which in this case represents the currently accepted, collaboratively constructed, proof or theory under discussion" [V] — [Corneli, Martin, Murray-Rust, Nesin, Pease, "Argumentation theory for mathematical argument", *Argumentation* 2019 / arXiv:1803.06500, footnote 1](https://arxiv.org/pdf/1803.06500)

### Inferences
- For an "add moves until your argument is accepted" engine, grounded and credulous-preferred games give a *binary, exactly computable* terminal reward. A grounded-acceptance check after each move is a polynomial graph computation. These games are much closer to Go/chess-style self-play (deterministic, perfect information, rule-defined win) than to judged debate.
- The classical games assume a *fixed* AF that the players explore. A game where players *add* arguments/attacks to a growing AF corresponds to persuasion dialogues (Prakken) and to dynamic argumentation / enforcement. The ICCMA 2023 Dynamic track (incremental add/delete with re-queries) is the closest solver-side analogue [V] ([ICCMA 2023 overview](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)).

### Gaps
- I did not re-fetch Caminada's discussion-game papers or Modgil & Caminada's chapter this session. Move names (HTB/CB/CONCEDE/RETRACT) and the exact P/O repetition rules come from domain knowledge and should be spot-checked.
- I found no single source that tabulates all games by semantics with their proven move-length bounds. The Baroni et al. *Handbook of Formal Argumentation* (College Publications 2018) has a chapter on argument games, but I did not access it.

## Q2. Is there self-play, RL, MCTS or learned-policy work on building argument graphs? Is there neural approximation of extensions?

### Takeaway
Yes, but it is sparse. MCTS and RL have been applied (a) to *strategy selection in persuasion dialogues* over a given argument graph (Hadoux, Hunter and co-authors; Riveret et al. 2014 on "argumentative puzzles") and (b) to *solving* acceptance (Yonas: DRL+MCTS ICCMA 2019 entrant; Q-learning by Alahmari, Yuan, Kudenko 2017). Neural extension prediction is well established: GNNs (Craandijk & Bex IJCAI 2020; Kuhlmann & Thimm 2019; Malmqvist's AFGCN in ICCMA). I found **no** published work that does AlphaZero-style self-play in which agents *construct* an AF by picking attacks/supports from a menu until a computed semantics accepts their claim. That configuration appears to be open.

### Cited Findings
- **Hadoux, Hunter, Polberg** ("Strategic argumentation dialogues for persuasion: framework and experiments based on modelling the beliefs and concerns of the persuadee", *Argument & Computation* 2023; arXiv 2101.11870, 2021). The system builds decision trees over possible dialogues and uses **Monte Carlo Tree Search** because exhaustive search is impractical. It was evaluated in a user study against a baseline that ignores beliefs/concerns [V] — [arXiv:2101.11870](https://arxiv.org/pdf/2101.11870); [ORCA record](https://orca.cardiff.ac.uk/id/eprint/157846/)
- **Riveret et al., ARGMAS 2014** applied MCTS to "argumentative puzzles" over abstract AFs and compared it with a basic RL approach [V] — [ARGMAS 2014 paper w12-07](https://www.mit.edu/~irahwan/argmas/argmas14/w12-07.pdf)
- **Yonas (ICCMA 2019 entrant)** is a hybrid of deep RL and MCTS for abstract argumentation, in which "the trained model guides the tree search at runtime". It solves acceptance; it is not a dialogue policy [V] — [ICCMA19 paper 8](https://iccma2019.dmi.unipg.it/papers/ICCMA19_paper_8.pdf)
- **Alahmari, Yuan & Kudenko (2017)** used Q-learning for abstract argumentation. The Yonas authors cite it as the RL baseline [V, via search summary of the Yonas paper] — [ICCMA19 paper 8](https://iccma2019.dmi.unipg.it/papers/ICCMA19_paper_8.pdf)
- **Hadoux, Beynier, Maudet, Weng, Hunter**, "Optimization of probabilistic argumentation with Markov decision models", IJCAI 2015. It models probabilistic argumentation dialogues as (PO)MDPs to compute optimal policies [K] — [IJCAI 2015 proceedings](https://www.ijcai.org/Proceedings/15/Papers/406.pdf)
- **Craandijk & Bex, "Deep Learning for Abstract Argumentation Semantics", IJCAI 2020, pp. 1667–1673.** It proposes an argumentation GNN (AGNN) that "learns a message-passing algorithm to predict the likelihood of an argument being accepted". It can "almost perfectly predict the acceptability under different semantics and scale well for larger argumentation frameworks". Code: github.com/DennisCraandijk/DL-Abstract-Argumentation [V] — [IJCAI 2020](https://www.ijcai.org/proceedings/2020/231); [arXiv:2007.07629](https://arxiv.org/pdf/2007.07629)
- **Kuhlmann & Thimm**, "Using graph convolutional networks for approximate reasoning with abstract argumentation frameworks: a feasibility study", SUM 2019 (LNCS 11940). This was the first GCN for credulous acceptance [K; my search did not return it — see Gaps]
- **AFGCN v2 (Lars Malmqvist, York)** entered the ICCMA 2023 Approximate track: a Python solver that "employs graph convolutional" networks / "a neural network trained" on AFs and supports all approximate subtracks [V]. In the results it ranked **4th–5th** in every subtrack. For example, DS-PR was solved correctly on 228 instances against harper++'s 300 [V] — [ICCMA 2023 overview, Table 4](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)
- **harper++ (Thimm)** is a non-learned approximation. It answers YES to skeptical acceptance iff the query is in the grounded extension, and it ranked **first in all skeptical approximate subtracks** (DS-PR 300, DS-SST 274, DS-ST 196, DS-STG 275 correct). **fargo-limited** (bounded DPLL-style search) won DC-CO, DC-SST and DC-ST (283/277/271 correct). Time limit was 60 s [V] — [ICCMA 2023 overview, §8.2 and Table 4](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)
- **LLMs computing extensions** (Li et al., Zhejiang; arXiv 2412.16725, Dec 2024; v2 Nov 2025 retitled "Enhancing Conflict Resolution in Language Models via Abstract Argumentation"). Chain-of-thought LLMs "struggle to accurately compute semantics". SFT+RLHF on Llama-3/Qwen2.5 with labelling-algorithm *process explanations* generalises better than QA-only training (tasks: grounded and complete extensions). Reported failure modes are hallucination, labelling inconsistencies and early termination [V, abstract plus third-party summary] — [arXiv:2412.16725v1](https://arxiv.org/abs/2412.16725v1); [v2](https://arxiv.org/html/2412.16725v2)

### Inferences
- In a competitive setting, cheap exact or near-exact baselines (grounded approximation, bounded search) beat learned GNN approximators. A learned value network over AFs is therefore unlikely to be needed *for the reward*. It may still be useful as a *policy/value prior* for choosing which attack to add (cf. Yonas guiding MCTS).
- The closest analogue to "self-play building an argument graph" is the combination of strategic-argumentation games (Q4) with the MCTS persuasion work. None of these trains a policy by self-play against a symbolic acceptance reward.

### Gaps
- The Kuhlmann & Thimm 2019 SUM paper did not surface in search. Its details are from domain knowledge.
- I found no RL/MCTS work in which actions are "add argument/attack/support" and reward = σ-acceptance of one's claim (AlphaZero-style). This looks like a genuine gap, but absence of evidence after a limited search (~15 calls) is not proof.
- I did not check recent (2024–2026) GNN work, e.g. on extension enumeration or on bipolar/weighted AFs.

## Q3. Complexity of deciding acceptance, ICCMA solver speeds, and whether deciding the win is cheap enough to be a search reward

### Takeaway
Grounded acceptance is in P (polynomial, effectively linear-time via labelling). Credulous acceptance under complete, preferred and stable semantics is NP-complete. Skeptical stable is coNP-complete, skeptical preferred is Π2P-complete, and ideal semantics is Θ2P-complete. Despite this, ICCMA 2023 SAT-based solvers (µ-toksia, Crustabri) solve ~90%+ of hard benchmark AFs within a 1200 s CPU limit. Those benchmarks are far larger than any hand-built argument game. For game-sized AFs (tens to hundreds of arguments), deciding the win is effectively free, even for skeptical preferred.

### Cited Findings
- The complexity table, as stated by the ICCMA 2023 organisers [V] ([Järvisalo, Lehtonen, Niskanen, "ICCMA 2023: 5th International Competition on Computational Models of Argumentation", *Artificial Intelligence* 2025](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf), §2):
  - DS-CO (= grounded membership) is in **P**.
  - DC-CO, DC-PR and DC-ST are **NP-complete**.
  - DS-ST is **coNP-complete**.
  - DC-ID and DS-ID are **Θ2P-complete**.
  - DC-SST and DC-STG are **Σ2P-complete**.
  - DS-PR, DS-SST and DS-STG are **Π2P-complete**.
- Primary complexity sources [K]:
  - Dimopoulos & Torres 1996 (TCS 170) for NP-completeness of credulous preferred.
  - Dunne & Bench-Capon 2002 (AIJ 141:187–203, "Coherence in finite argument systems") for Π2P-completeness of skeptical preferred.
  - Dunne 2009 for ideal semantics.
  - The survey is Dvořák & Dunne, "Computational problems in formal argumentation and their complexity", *Handbook of Formal Argumentation* vol. 1, College Publications 2018. — [Dunne & Bench-Capon 2002](https://doi.org/10.1016/S0004-3702(02)00261-8)
- For structured argumentation (ABA), the same paper reports that credulous acceptance is NP-complete, skeptical complete acceptance is coNP-complete, and DS-PR is Π2P-complete for flat ABA [V] — [ICCMA 2023 overview, §2](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)
- **ICCMA 2023 setup.**
  - Four tracks: Main (+ No-limits), Approximate, Dynamic and ABA.
  - 329 benchmark AFs. Domains were sampled from the ICCMA 2015 generators (Grounded/SCC/Stable generators, 100 AFs each), Planning2AF (385), Traffic (600), Erdős–Rényi (500), Watts–Strogatz (400) and others.
  - Per-instance limit: **1200 s CPU** (Main), **60 s** (Approximate).
  - Scoring by PAR-2 (timeouts cost 2× the limit) [V] — [ICCMA 2023 overview, §§4–6](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)
- **ICCMA 2023 Main track winners** [V] — [ICCMA 2023 overview, §8.1, Table 3](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf); also the [official rankings page](https://argumentationcompetition.org/2023/rankings.html):
  - **Crustabri** (Lagniez, Lonca, Mailly; incremental SAT; successor to CoQuiAAS) ranked first in 9 subtracks: DC-ST, DC-STG, DS-SST, DS-ST, DS-STG, SE-PR, SE-SST, SE-ST, SE-STG.
  - **µ-toksia (Glucose)** (Niskanen & Järvisalo) ranked first in DC-CO, DC-SST, **DS-PR** and SE-ID.
  - Example PAR-2 scores for DS-PR: µ-toksia(glucose) 242.69, Crustabri 279.27, µ-toksia(cmsat) 325.07.
  - No-limits entries Fudge (Thimm, Cerutti, Vallati) and PORTSAT (parallel portfolio, Rust) "did not significantly outperform the Main track solvers overall".
- **Hardness in practice** [V] — [ICCMA 2023 overview, §9.2, Tables 8–9](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf):
  - The virtual best solver solved 314/329 (DC-CO) and 315/329 (DC-ST).
  - Second-level-complete tasks are "clearly harder": DC-SST, DC-STG, DS-SST, DS-STG, SE-ID, SE-SST and SE-STG had 287–297 out of 329 solved.
  - DS-PR: VBS solved 303/329; Crustabri alone solved 296.
  - µ-toksia(glucose) contributed most often to the VBS (160–219 instances per subtrack).
- **Dynamic track** (incremental changes plus re-queries via the IPAFAIR API, analogous to IPASIR): Crustabri won all three subtracks (DC-CO, DC-ST, DS-ST). On DS-ST its PAR-2 score was 367.82 against 684.92 for µ-toksia [V] — [ICCMA 2023 overview, §§5.4, 8.3](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)
- **ABA track:** Crustabri was disqualified in every ABA subtrack for erroneous output. Entrants included AcbAr, ASPforABA, ASTRA and flexABle [V] — [ICCMA 2023 overview, §8.4](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf); [solver list](https://argumentationcompetition.org/2023/solvers.html)
- Solver internals: µ-toksia and Crustabri are SAT-based with incremental SAT calls, and Fudge uses iterative SAT for preferred/ideal. ICCMA 2023 also introduced *witness verification* for YES/NO answers (proof checking) [V] — [ICCMA 2023 overview, §§5, 7](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)

### Inferences
- **Reward-cheapness.** If the game's terminal reward is grounded acceptance (as in Pease et al. 2017 and Prakken's "current winner"), it is a polynomial fixed-point computation. That is microseconds to milliseconds for AFs of the size an argument game produces, so it can be called at every node of an MCTS. Skeptical preferred is Π2P-complete in theory. In practice, a SAT-based solver such as µ-toksia solves most ICCMA AFs (hundreds to tens of thousands of arguments) within the time limit. On AFs of < ~1000 arguments it should be sub-second, but that figure is an inference: ICCMA does not publish per-size timing in what I read.
- **The Dynamic track's IPAFAIR interface** is precisely the API a search engine needs: add or remove arguments/attacks, then re-query acceptance incrementally.
- A game designer can choose the semantics to set reward cost. Grounded is cheap and skeptical. Preferred is credulous NP or skeptical Π2P. Stable has the risk that no extension exists, in which case everything is skeptically accepted. ICCMA 2023 notes that a stable extension existed in only 206/329 AFs [V] ([ICCMA 2023 overview, §9.1](https://www.cs.helsinki.fi/u/mjarvisa/papers/jln.aij25.pdf)). This is a degenerate-reward trap.

### Gaps
- ICCMA 2025 results (6th edition) were not checked.
- I found no per-instance runtimes split by AF size, so "sub-second on small AFs" is inference, not citation.

## Q4. Game-theoretic treatments of argumentation (strategic argumentation; Rahwan & Larson; Thimm; Governatori)

### Takeaway
A distinct line treats argumentation as a strategic game among self-interested agents with private arguments. Four strands stand out:
- **Mechanism design.** Rahwan & Larson ask when truthfully revealing all one's arguments is a dominant strategy under grounded semantics.
- **Extensive-form/incomplete-information games.** Procaccia & Rosenschein; Riveret et al.; Thimm & García.
- **Complexity of strategic argumentation.** Governatori et al. show that deciding a winning move in dialogue games over defeasible logic with incomplete information is NP-complete.
- **Probabilistic/opponent-modelling persuasion.** Hunter, Hadoux, Polberg (Q2).

These provide the game-theoretic vocabulary (strategy-proofness, equilibria, opponent models) for an argument-game engine.

### Cited Findings (all [K] unless noted; not re-fetched this session)
- Rahwan & Larson, "Mechanism design for abstract argumentation", AAMAS 2008. Defines *argumentation mechanism design* (ArgMD). Shows that under grounded semantics, truthful revelation is dominant-strategy incentive compatible only under restrictive graph conditions (e.g. agents' arguments do not indirectly defend each other). — [AAMAS 2008](https://dl.acm.org/doi/10.5555/1402298.1402308)
- Rahwan, Larson & Tohmé, "A characterisation of strategy-proofness for grounded argumentation semantics", IJCAI 2009. — [IJCAI 2009](https://www.ijcai.org/Proceedings/09/Papers/045.pdf)
- Rahwan & Larson, "Argumentation and game theory", chapter in Rahwan & Simari (eds.) *Argumentation in AI*, Springer 2009, pp. 321–339. — [Springer](https://doi.org/10.1007/978-0-387-98197-0_16)
- Procaccia & Rosenschein, "Extensive-form argumentation games", EUMAS 2005: argumentation as extensive-form games of perfect information over Dung AFs.
- Riveret, Prakken, Rotolo, Sartor, "Heuristics in argumentation: a game-theoretical investigation", COMMA 2008: game-theoretic analysis of the costs of revealing arguments.
- Thimm & García, "Classification and strategical issues of argumentation games on structured argumentation frameworks", AAMAS 2010 (on DeLP, structured). Thimm, "Strategic argumentation in multi-agent systems", *Künstliche Intelligenz* 28(3):159–168, 2014, a survey of strategic argumentation, opponent models and dialogue games.
- Governatori, Olivieri, Rotolo, Scannapieco, Maher, "Strategic argumentation is NP-complete", ECAI 2014. Dialogue games in which each player's knowledge is private and players choose which rules to reveal. Deciding whether a player has a winning move is NP-complete even with polynomial base reasoning (defeasible logic, structured).
- Hunter's line on probabilistic and epistemic persuasion with opponent models: *Argument & Computation* 2023 MCTS paper [V] ([arXiv:2101.11870](https://arxiv.org/pdf/2101.11870)).

### Inferences
- The Governatori result matters for an engine design. Even if *evaluating* a fixed AF is cheap (grounded), *choosing* which of one's private arguments to reveal is NP-hard. The search problem (move selection) is therefore where hardness lives, which is exactly what MCTS/learned policies would address.
- Rahwan & Larson imply that, under grounded semantics, agents have incentives to *hide* arguments. A "menu" game in which agents choose which attacks to add will in general exhibit strategic withholding. The engine needs to decide whether that is a feature or a bug.

### Gaps
- None of the Q4 sources were fetched this session (budget). Citations are canonical, but page numbers and exact theorem statements need verification.

## Q5. Does Pease et al. 2017 define moves and a win condition precisely enough to implement as a game engine? What were its moves?

### Takeaway
Partially. The paper is: Alison Pease, John Lawrence, Katarzyna Budzynska, Joseph Corneli, Chris Reed, "Lakatos-style collaborative mathematics through dialectical, structured and abstract argumentation", *Artificial Intelligence* 246:181–219, May 2017, DOI 10.1016/j.artint.2017.02.006, CC BY 4.0.
- **Implemented.** It specifies a dialogue game whose moves operate on an AIF information state. It was executed in the ARG-tech stack: dialogue-game execution, TOAST for ASPIC+ and induced Dung AF, and DungOMatic for semantics.
- **Status is computed.** The "status" of the proof is computed, not judged: the **grounded extension** = the currently accepted proof/theory.
- **No winner.** There is no winner/loser win condition in the competitive sense. The game is *collaborative*: the computed artefact is "the currently accepted proof", and the dialogue ends when participants stop.
- **Moves.** The moves are Lakatos's methods of proof and refutation: conjecture, proof, local/global counterexample, monster-barring, monster-adjusting, exception-barring (piecemeal exclusion / strategic withdrawal) and lemma-incorporation.
- **Engine gap.** For a competitive engine, one would need to add a terminal condition, e.g. "Proponent wins if the conjecture (in some revised form) is in the grounded extension when no legal Opponent move remains."

### Cited Findings
- Bibliographic: *Artificial Intelligence* vol. 246 (May 2017), pp. 181–219, DOI 10.1016/j.artint.2017.02.006, CC BY 4.0. Part of the EPSRC "Dialectical Argumentation Machines" project (EP/G060347/1) [V] — [Edinburgh Research Explorer](https://www.research.ed.ac.uk/en/publications/lakatos-style-collaborative-mathematics-through-dialectical-struc/); [ScienceDirect](http://www.sciencedirect.com/science/article/pii/S0004370217300267)
- The abstract's three arcs [V] — [Edinburgh Research Explorer](https://www.research.ed.ac.uk/en/publications/lakatos-style-collaborative-mathematics-through-dialectical-struc/):
  1. Lakatos's informal logic of mathematical discovery interpreted "through the lens of dialogue game theory ... as a dialogue game ranging over structures of argumentation".
  2. Building structured arguments, inducing abstract argumentation systems and computing semantics to label acceptability, yielding "a currently accepted proof artefact" viewable with its development history.
  3. An implemented computational model demonstrated on a real-world mathematical collaboration.
- Mechanism and win/state computation: the game "defines ordered operations on a shared information state represented in the Argument Interchange Format (AIF) (Lawrence et al., 2012)". That state is interpreted by TOAST (Snaith & Reed 2012) and passed to DungOMatic (Snaith et al. 2010) "to calculate the grounded extension, which in this case represents the currently accepted, collaboratively constructed, proof or theory under discussion" [V] — [Corneli et al., arXiv:1803.06500, fn. 1](https://arxiv.org/pdf/1803.06500)
- Corneli et al. characterise Pease et al. (2017) as formalising "the theory of informal mathematics developed in Lakatos's Proofs and Refutations (1976) as a set of rules for turn-taking in a dialogue game". They describe the work as showing "formally specified and fully implemented argumentation tools can be brought together" [V] — [arXiv:1803.06500](https://arxiv.org/pdf/1803.06500)
- Precursor: Pease, Budzynska, Lawrence, Reed, "Lakatos Games for Mathematical Argument", COMMA 2014, FAIA 266:59–66, DOI 10.3233/978-1-61499-436-7-59 [V] — [ResearchGate record](https://www.researchgate.net/publication/288662414_Lakatos_Games_for_Mathematical_Argument). Search snippets indicate that TOAST conversion to ASPIC+ and DungOMatic semantics are used "to determine the most up-to-date status of a proof in a Lakatos Game" [V, snippet] — [ARG-tech publications](https://www.arg.tech/index.php/publications/)
- Lakatos-derived move set as described in secondary literature on dialogue-game readings of Lakatos: "raising counterexamples, piecemeal exclusion, monster barring, monster adjusting", plus the three main methods monster-barring, exception-barring and lemma-incorporation [V, search snippets; the primary AIJ PDF was 403-blocked] — [Başkent, "Lakatos nonclassically"](https://www.canbaskent.net/logic/lakatos-nonclassically.pdf); [Pease thesis summary, "A Computational Model of Lakatos-style Reasoning"](https://www.exeter.ac.uk/research/groups/education/pmej/pome27/Pease%20%20A%20Computational%20Model%20of%20Lakatos-style%20Reasoning.pdf)
- Data context: the MiniPolymath projects (Tao's blog-based collaborative olympiad solving). MiniPolymath 3, analysed by Pease & Martin 2012, had 147 comments in 27 threads between 8pm and 9.50pm UTC on 19 July 2011 [V] — [Corneli et al., arXiv:1803.06500](https://arxiv.org/pdf/1803.06500). Whether the AIJ 2017 worked example is MiniPolymath was **not confirmed**.
- Follow-on: IATC (Inference Anchoring Theory + Content) for annotating mathematical dialogue. It builds on IAT (Budzynska & Reed 2011), whose roots are in AIF dialogue representation [V] — [Corneli et al., *Argumentation* 2019 / arXiv:1803.06500](https://arxiv.org/pdf/1803.06500); [Dundee published version](https://discovery.dundee.ac.uk/ws/portalfiles/portal/30132828/Final_Published_Version.pdf)

### Inferences
- **What is engine-ready.** Moves are typed operations on an AIF graph. The state evaluation is deterministic (ASPIC+ induction followed by the grounded extension). This is a computed evaluation function.
- **What is missing for a competitive/self-play engine.** First, an explicit terminal rule and winner. Second, a bounded move menu: Lakatos moves such as "monster-adjusting" or "lemma-incorporation" require *generating new content* (a revised conjecture or definition), not merely selecting an edge. In an LLM-era engine, that content-generation step is where an LLM would sit. The symbolic part (attack/support topology plus grounded evaluation) supplies the reward.
- The protocol is executable in principle: the ARG-tech stack (DGDL / dialogue-game execution, TOAST, DungOMatic, AIF DB) was designed to run such games, and the same group built Arvina for mixed-initiative dialogue games on the Argument Web. That last point is from domain knowledge.

### Gaps
- **The full AIJ PDF was 403-blocked** (both research.ed.ac.uk files and ScienceDirect). I could not quote the paper's exact locution list, pre/post-conditions or any termination clause. The move list above is reconstructed from secondary sources and the Lakatos methods. **Before relying on the exact move inventory, fetch the CC BY PDF directly**: https://doi.org/10.1016/j.artint.2017.02.006 or https://www.research.ed.ac.uk/files/121574294/Lakatos_style_collaborative_PEASE_DOA20112019_VOR_CC_BY.pdf.
- I did not confirm whether Pease et al. specify a DGDL description of the game, though the ARG-tech context makes it likely.

## Q6. AIF, AIF+ and "AIF²"

### Takeaway
AIF is the ARG-tech/Dundee-led ontology for interchanging argument data:
- **2006 draft specification.** I-nodes carry information/propositions. S-nodes are schemes: RA = inference, CA = conflict, PA = preference.
- **AIF+ (COMMA 2008)** adds dialogue: L-nodes are locutions (a subclass of I-nodes), and TA-nodes are transitions between locutions.
- **"AIF²" does not appear** in the literature I found. It is a **local coinage** in the futon stack: futon5 `README-aif+.md` §12. There it names an "AIF² contest" of paired AIF+ "lifeforms" used to adjudicate close calls.

### Cited Findings
- Chesñevar, McGinnis, Modgil, Rahwan, Reed, Simari, South, Vreeswijk, Willmott, "Towards an argument interchange format", *Knowledge Engineering Review* 21(4):293–316, 2006. It proposed a shared interchange format "for representation and exchange of data between various argumentation tools and agent-based applications" [V] — [Dundee record](https://discovery.dundee.ac.uk/en/publications/towards-an-argument-interchange-format/); [Vidal's library](https://jmvidal.cse.sc.edu/lib/chesnevar06a.html)
- Reed, Wells, Devereux, Rowe, "AIF+: Dialogue in the Argument Interchange Format", COMMA 2008, FAIA pp. 311–323. Utterances are analogous to I-nodes and transitions between locutions to S-nodes, letting dialogue protocols and histories sit alongside monological argument. Later literature names these L-nodes (a subclass of I-nodes) and TA-nodes [V] — [PDF, arg.tech](https://www.arg.tech/people/chris/publications/2008/comma2008-aif+.pdf); [Dundee record](https://discovery.dundee.ac.uk/en/publications/aif-dialogue-in-the-argument-interchange-format/)
- The ARG-tech "Argument Web" ecosystem includes AIFdb, OVA, TOAST and DungOMatic. It is the deployment substrate for AIF-based dialogue games [V, title] — [Bex, Lawrence, Snaith, Reed et al., "The Argument Web: an online ecosystem of tools, systems and services for argumentation", *Philosophy & Technology* 30:137–160, 2017](https://link.springer.com/article/10.1007/s13347-017-0260-8)
- Local usage of "AIF²". In this repository, `/home/user/futon3c/holes/specs/repl.spec.edn` instructs: "when warrants are close or directly contested, run the AIF² contest (futon5/README-aif+.md §11 contest pattern + §12 AIF²): treat the two options as paired AIF+ lifeforms, find the fixed-point (mutual-eradication vs scope-restricted coalescence)". `/home/user/futon3c/holes/missions/M-pilot-appearance.md` §8.4.4 reports "AIF² contest evidence" [V, local file grep].

### Inferences
- "AIF²" should be presented in the report as a futon-local extension of AIF+, not as literature. The canonical external terms are AIF (2006) and AIF+ (2008).
- IAT (Budzynska & Reed) is the bridge from AIF+ dialogue structure to illocutionary connections, and IATC is its mathematical variant.

### Gaps
- I did not run a dedicated web search for "AIF²/AIF2". The conclusion that it is local rests on absence from the AIF/AIF+ results plus its presence in local docs. `~/code/futon5/README-aif+.md` was not available in this checkout to quote.

## Q7. 2023–2026 LLMs plus formal argumentation, and the debate/verifier line: what does it say about a judged reward being gamed?

### Takeaway
Two literatures are converging:
- **(a) Neuro-symbolic argumentation.** LLMs generate arguments and attacks or mine relations, and a *formal* semantics computes the verdict. Examples are ArgLLMs (Freedman, Toni et al., AAAI 2025) and the extension-computation benchmark (Li et al. 2024/25).
- **(b) AI-safety debate.** The verdict is *judged* by a human or LLM.

The debate empirics show judged rewards are partially gameable. Khan et al. (ICML 2024 best paper) find that *debate* improves judge accuracy as debaters get more persuasive (human judges 88%, LLM judges 76%, against naive baselines of 60%/48%). Under *consultancy*, a single optimised arguer, judge accuracy *decreases* as persuasiveness rises: optimisation against a judge rewards persuasion over truth unless an adversary is present. Kenton et al. (NeurIPS 2024) find debate's advantage over consultancy is real, but gains over direct QA are modest and task-dependent. Together this is the core motivation for replacing a judge with a *computed* argumentation semantics.

### Cited Findings
- Irving, Christiano, Amodei, "AI safety via debate", arXiv 1805.00899, 2018. Two agents alternate statements and a human judges. The analogy is that debate with optimal play can let a polynomial-time judge decide PSPACE questions (versus NP for direct judging) [K] — [arXiv:1805.00899](https://arxiv.org/abs/1805.00899)
- Khan, Hughes, Valentine, Ruis, Sachan, Radhakrishnan, Grefenstette, Bowman, Rocktäschel, Perez, "Debating with More Persuasive LLMs Leads to More Truthful Answers", ICML 2024 (arXiv 2402.06782). Run on QuALITY with information asymmetry. Findings [V] — [arXiv:2402.06782](https://arxiv.org/pdf/2402.06782); [OpenReview](https://openreview.net/pdf/8d5a193853ab6de2da5e1297019a89bf933e8811.pdf):
  - With the most persuasive debaters, non-expert human judges reach 88% and LLM judges 76%, against naive baselines of 60% and 48%.
  - Optimising debaters for persuasiveness *raises* judge accuracy in debate.
  - In *consultancy*, judge accuracy *decreases* as consultants become more persuasive.
  - Humans outperform LLM judges across protocols.
- Kenton, Siegel, Kramár, Brown-Cohen, Albanie, Bulian, Agarwal, Lindner, Tang, Goodman, Shah, "On scalable oversight with weak LLMs judging strong LLMs", NeurIPS 2024 (arXiv 2407.04622). Weak LLM judges with stronger debaters. Findings [V for the findings; author list K] — [summary](https://www.greaterwrong.com/posts/Qn3ZDf9WAqGuAjWQe/on-scalable-oversight-with-weak-llms-judging-strong-llms); [arXiv:2407.04622](https://arxiv.org/abs/2407.04622):
  - Debate beats consultancy, and stronger debaters increase judge accuracy "more modestly than in previous studies".
  - On closed QA (information asymmetry), debate is about equal to direct QA.
  - On other tasks there is "either small or no advantage to debate over QA without article".
- Michael, Mahdi, Rein, Petty, Dirani, Padmakumar, Bowman, "Debate Helps Supervise Unreliable Experts", arXiv 2311.08702, 2023. Human debaters on QuALITY: debate judges reached 84% against 74% for consultancy [K] — [arXiv:2311.08702](https://arxiv.org/abs/2311.08702)
- Brown-Cohen, Irving, Piliouras, "Scalable AI safety via doubly-efficient debate", arXiv 2311.14125 (ICML 2024). It addresses the obfuscated-argument problem by requiring the honest strategy to win with polynomial simulation against an unbounded dishonest one [K] — [arXiv:2311.14125](https://arxiv.org/abs/2311.14125)
- Freedman, Dejl, Gorur, Yin, Rago, Toni, "Argumentative Large Language Models for Explainable and Contestable Claim Verification", AAAI 2025 (arXiv 2405.02079). LLMs generate pro/con arguments with confidence scores, forming a quantitative bipolar argumentation framework (QBAF). The verdict is computed by gradual semantics (e.g. DF-QuAD), so it is explainable and contestable. Structured/bipolar, not Dung [K] — [arXiv:2405.02079](https://arxiv.org/abs/2405.02079)
- Li et al. 2024/2025 (arXiv 2412.16725): LLMs computing grounded/complete extensions are poor under CoT and improve with process-supervised fine-tuning [V] — [arXiv](https://arxiv.org/abs/2412.16725v1)

### Inferences
- **Design implication.** A judged reward (human or LLM judge) is gameable by persuasion in *non-adversarial* settings (Khan et al.'s consultancy result), and debate's protective effect is modest and task-dependent (Kenton et al.). An argument game whose terminal reward is a *computed* Dung/ASPIC+ semantics removes the persuasion channel for the *evaluation* step.
- **It moves the attack surface rather than removing it.** Gaming relocates to (i) the *content* of arguments and whether an attack "really" holds, i.e. who certifies an LLM-proposed attack edge, and (ii) degenerate topologies such as odd cycles, self-attack or exploiting stable semantics' non-existence. This mirrors the obfuscated-arguments concern in Brown-Cohen et al.
- A verifier per edge (e.g. a proof checker for mathematical lemmas, a test suite for code claims) combined with a computed semantics is the natural hybrid. It is the gap between the debate literature (judged) and the argumentation literature (computed over given edges).

### Gaps
- I did not verify 2025–2026 follow-ups on debate training (e.g. RL-trained debaters with LLM judges, or "debate vs. consultancy" replications). Search budget was exhausted.
- I did not verify 2023–2026 argument-mining-to-AF pipelines in detail: Gorur/Rago/Toni on LLM relation-based argument mining (COLING 2025), and surveys such as Castagna et al. on argumentation-based chatbots (JAIR 2024). These are [K] mentions only.
- I found no paper that uses a *formal argumentation semantics as the RL reward* for LLM debaters. This appears to be an open combination, but that is unconfirmed.
