# T5: LLM proposer + exact engine verifier; judged-reward search; CPU-only AlphaZero

Provenance key, used throughout:
- **[V]**: verified this session by fetching or searching the page.
- **[M]**: from the researcher's prior knowledge of the primary paper, cited to its canonical arXiv/venue URL. Not re-fetched this session (the tool budget was capped at about 15 calls). The writer should treat [M] figures as "standard, widely cited" but spot-check any number that becomes load-bearing.
- **[BLOG]**: the number appears only in a blog, README or docs page, not in a peer-reviewed paper.

Date context: as of 2026-10-30. Pre-2024 work counts as "older/foundational".

---

## Q1. For each system in (A), what is the proposer, what is the verifier/engine, is the reward exact or judged, and what gain is reported over the LLM alone?

### Takeaway
The systems with the largest and most robust gains share one trait: an **exact, non-learned verifier** (program execution, a symbolic deduction engine, the Lean kernel, a rules engine). Examples are FunSearch, AlphaEvolve, AlphaGeometry, AlphaProof, rStar-Math's code-execution filter, and RLVR. Systems whose "verifier" is the LLM itself (ToT, RAP, LATS on QA, rStar) report big benchmark jumps. Those benchmarks, however, usually have an exact final-answer check, and a judged intermediate value function guides the search. The cleanest "LLM as policy prior inside a game engine" result is DeepMind's ICML 2025 MAV paper (Schultz et al.), which reaches grandmaster-level chess.

### Cited Findings

**Summary table** (P = proposer, V = verifier/value, R = reward type)

| System | Year / venue | Proposer | Verifier / engine | Reward | Reported gain vs LLM alone |
|---|---|---|---|---|---|
| Tree of Thoughts | 2023, NeurIPS | GPT-4 thoughts | GPT-4 self-evaluation ("sure/likely/impossible") + BFS/DFS | Judged (search heuristic); final answer checked exactly in Game of 24 | Game of 24: CoT 4% -> ToT 74% [M] |
| RAP | 2023, EMNLP | LLaMA-33B as policy *and* world model | MCTS; reward = LLM self-eval + action likelihood + task heuristics | Judged during search | Blocksworld: RAP on LLaMA-33B beats GPT-4 CoT, about 33% relative gain [M] |
| LATS | 2023 arXiv, ICML 2024 | GPT-3.5/GPT-4 | MCTS; LM value + self-consistency + environment feedback (test execution for code) | Mixed: exact tests for code, judged for QA | HumanEval pass@1 92.7% with GPT-4 [M] |
| AlphaLLM | 2024, NeurIPS | LLaMA-2-70B | eta-MCTS with three *learned* critics (value, PRM, ORM) | Judged | GSM8K 57.8 -> 92.0; MATH 20.7 -> 51.0 [M] |
| rStar | 2024 arXiv (ICLR 2025) | Small LM (SLM) generator with MCTS over human-like actions | Second SLM as a "mutual consistency" discriminator | Judged | LLaMA2-7B GSM8K 12.51 -> 63.91; Mistral-7B 36.46 -> 81.88; LLaMA3-8B-Instruct 74.53 -> 91.13 [M] |
| rStar-Math | 2025, ICML | Qwen2.5-Math-7B policy SLM, code-augmented CoT | **Python execution** filters steps + learned process preference model (PPM) | Hybrid: exact execution filter + judged PPM | MATH 58.8 -> 90.0 (7B); AIME 2024 53.3% [M] |
| FunSearch | 2023, *Nature* 625 | Codey/PaLM-2 writes programs | **Exact evaluator** runs the program and scores it | Exact | New cap-set lower bounds (dimension 8: 512 vs 496 previously); better online bin-packing heuristics [M] |
| AlphaEvolve | May 2025, DeepMind white paper / arXiv 2506.13131 | Gemini Flash + Pro ensemble, evolutionary code diffs | **Automated evaluators** (execution, metrics) | Exact (user-specified) | 4x4 complex matrix multiplication with 48 scalar mults (beats Strassen's 49); matched SOTA on about 75% and improved on about 20% of 50+ open problems; recovered about 0.7% of Google fleet compute [M] |
| AlphaGeometry | 2024, *Nature* 625 | LM proposes auxiliary constructions | **Symbolic DD+AR deduction engine** | Exact | IMO-AG-30: 25/30 vs 10/30 for previous SOTA (Wu's method); symbolic engine alone (DD+AR) 14/30 [M] |
| AlphaGeometry 2 | 2025, arXiv 2502.03544 | Gemini-based LM + search (SKEST) | Faster symbolic engine | Exact | 84% of IMO geometry problems 2000-2024 (vs 54% for AG1) [M] |
| AlphaProof | IMO 2024 announcement; *Nature* 12 Nov 2025 (DOI 10.1038/s41586-025-09833-y) | Gemini-finetuned autoformalizer (80M statements) + AlphaZero-style proof network | **Lean kernel** | Exact | With AG2: 28/42 at IMO 2024 (silver). AlphaProof solved 3/5 non-geometry problems, including the hardest (P6). Adds test-time RL on problem variants [V] [research.google](https://research.google/pubs/olympiad-level-formal-mathematical-reasoning-with-reinforcement-learning/), [DeepMind blog](https://deepmind.google/discover/blog/ai-achieves-silver-medal-standard-solving-international-mathematical-olympiad-problems/) |
| MAV / external+internal planning | arXiv 2412.12119; ICML 2025 (PMLR v267) | Multi-game transformer LM as policy+value prior | MCTS **without a game engine** (the LM also predicts legal moves and transitions) | Learned value; game outcome is exact | Grandmaster-level chess at a per-move search budget similar to human GMs; also Chess960, Connect Four, Hex [V] [arXiv](https://arxiv.org/abs/2412.12119v1), [PMLR](https://proceedings.mlr.press/v267/schultz25a.html) |
| LLM-as-pruner/value in MCTS | arXiv 2403.05632 (2024) | LLM prunes actions and serves as a value proxy | MCTS self-play with game rules | Judged value; exact terminal | Beats both standalone LLM and plain MCTS on chess puzzles, 5x5 MiniGo, chess [V] [arXiv](https://arxiv.org/pdf/2403.05632) |

- AlphaProof, primary source: "AlphaZero-inspired agent that learns to find formal proofs through RL", trained on 80M auto-formalized statements; for the hardest problems it uses Test-Time RL, generating related problem variants at inference [V]. — [Google Research pub page](https://research.google/pubs/olympiad-level-formal-mathematical-reasoning-with-reinforcement-learning/); [Nature Asia press release](https://www.natureasia.com/en/info/press-releases/detail/9147)
- Two scoring framings for IMO 2024: the 2024 blog says 4/6 problems (AlphaProof + AG2), and the Nature abstract says 3/5 non-geometry by AlphaProof. They are consistent (AG2 solved the geometry problem) [V]. — [DeepMind blog](https://deepmind.google/blog/ai-solves-imo-problems-at-silver-medal-level/)
- Schultz et al. test "external search", where the model guides MCTS rollouts and evaluations "without calls to an external game engine", and "internal search", where the model writes a linearized search tree in context. Both improve win rates over the base model. Authors include Lanctot (OpenSpiel). [V] — [arXiv 2412.12119](https://arxiv.org/abs/2412.12119v1); [ICML 2025 poster](https://icml.cc/virtual/2025/poster/45648)
- Tree of Thoughts: Yao et al. 2023 [M]. — [arXiv 2305.10601](https://arxiv.org/abs/2305.10601)
- RAP: Hao et al. 2023, "Reasoning with Language Model is Planning with World Model" (EMNLP 2023) [M]. — [arXiv 2305.14992](https://arxiv.org/abs/2305.14992)
- LATS: Zhou et al. 2023, "Language Agent Tree Search Unifies Reasoning, Acting, and Planning in Language Models" (ICML 2024) [M]. — [arXiv 2310.04406](https://arxiv.org/abs/2310.04406)
- AlphaLLM: Tian et al. 2024, "Toward Self-Improvement of LLMs via Imagination, Searching, and Criticizing" (NeurIPS 2024) [M]. — [arXiv 2404.12253](https://arxiv.org/abs/2404.12253)
- rStar: Qi et al. 2024, "Mutual Reasoning Makes Smaller LLMs Stronger Problem-Solvers" [M]. — [arXiv 2408.06195](https://arxiv.org/abs/2408.06195)
- rStar-Math: Guan et al. 2025 [M]. — [arXiv 2501.04519](https://arxiv.org/abs/2501.04519)
- PPO-MCTS (value model from PPO reused as an MCTS critic for text generation): Liu et al. 2023, "Don't throw away your value model!" [M]. — [arXiv 2309.15028](https://arxiv.org/abs/2309.15028)
- FunSearch: Romera-Paredes et al., *Nature* 625, 468–475 (2024; online Dec 2023) [M]. — [Nature](https://www.nature.com/articles/s41586-023-06924-6)
- AlphaEvolve: Novikov et al. 2025 [M]. — [arXiv 2506.13131](https://arxiv.org/abs/2506.13131)
- AlphaGeometry: Trinh et al., *Nature* 625, 476–482 (2024) [M]. — [Nature](https://www.nature.com/articles/s41586-023-06747-5). AG2: Chervonyi et al. 2025 [M]. — [arXiv 2502.03544](https://arxiv.org/abs/2502.03544)
- **Self-improvement against exact verifiers:** STaR (Zelikman et al., NeurIPS 2022) filters self-generated rationales by answer correctness. GPT-J with STaR on CommonsenseQA performs comparably to a 30x larger fine-tuned GPT-3 [M] — [arXiv 2203.14465](https://arxiv.org/abs/2203.14465). ReST-EM (Singh et al., "Beyond Human Data", TMLR 2024) runs EM-style self-training with *binary exact* rewards on MATH and APPS, and beats fine-tuning on human data for PaLM-2 [M] — [arXiv 2312.06585](https://arxiv.org/abs/2312.06585).
- **Generator–verifier gap:** Song et al., "Mind the Gap: Examining the Self-Improvement Capabilities of LLMs" (ICLR 2025). They formalize the gap between generation and verification as the quantity that self-improvement exploits, and it scales with pretraining compute [M] — [arXiv 2412.02674](https://arxiv.org/abs/2412.02674). Scalable-oversight framing: Bowman et al. 2022 [M] — [arXiv 2211.03540](https://arxiv.org/abs/2211.03540).
- **LLMs playing board games through a rules engine (2025–2026):** Kaggle Game Arena (Aug 2025, Google DeepMind + Kaggle). Models get FEN + PGN and must emit a SAN move. They get **no legal-move list**, three retries per illegal move, and forfeit after that. The harness is open-source (`game_arena`) [V] — [SiliconANGLE](https://siliconangle.com/2025/08/04/google-deepmind-host-ai-chess-tournament-evaluate-leading-ai-models-reasoning-skills/); [ChessArena arXiv 2509.24239](https://arxiv.org/pdf/2509.24239). ChessArena reports that 78% of Gemini 2.5 Flash's losses came from illegal moves rather than strategic blunders [V, via search snippet] — [arXiv 2509.24239](https://arxiv.org/pdf/2509.24239). A 2026 "AutoHarness" paper (arXiv 2603.03329) has LLM agents synthesize their own code harness for games [V title only] — [arXiv](https://arxiv.org/html/2603.03329v1).
- A "Game Arena: Strategic LLM Evaluation in Competitive Environments" tech report appears at arXiv 2609.31473. The search tool flagged its date as needing provenance verification [V title only; unverified] — [emergentmind](https://www.emergentmind.com/papers/2609.31473).
- Counterpoint, scaling without search: Ruoss et al., "Amortized Planning with Large-Scale Transformers: A Case Study on Chess" (NeurIPS 2024; first posted as "Grandmaster-Level Chess Without Search"). A 270M transformer distilled from Stockfish values reaches a Lichess blitz Elo of about 2895 without explicit search [M] — [arXiv 2402.04494](https://arxiv.org/abs/2402.04494).

### Inferences
- Two systems are essentially *direct analogues* of "LLM proposes moves, exact engine verifies and scores": AlphaGeometry (LM supplies auxiliary points, DD+AR closes the proof) and FunSearch/AlphaEvolve (LM proposes programs, evaluator scores). AlphaProof is the closest to an AlphaZero-with-LM loop: an exact verifier (Lean) used as RL reward, plus test-time RL.
- Game Arena shows that an LLM *without* a legal-move list wastes much of its capacity on legality. In an architecture with a rules engine, the engine should enumerate or validate legal moves, and the LLM should only rank or propose among them. This mirrors how MAV and the 2403.05632 pruner use the LLM.
- In the ToT/RAP/LATS/rStar family, the *search heuristic* is judged, but the reported metric is an exact answer check. Their gains therefore show that a judged heuristic can *guide* search when an exact terminal check exists. They do not show that judged rewards can stand in for exact ones.

### Gaps
- I did not re-verify the specific numbers for ToT, RAP, LATS, AlphaLLM, rStar, rStar-Math, FunSearch or AlphaEvolve this session; they are marked [M].
- I did not open the full MAV paper, so its exact Elo values and search budgets are not captured.
- I found no 2025–2026 primary paper that inserts a *frontier general-purpose* LLM (rather than a game-specialised transformer) as the PUCT prior in a full AlphaZero loop with an external rules engine and reports Elo. 2403.05632 is the closest, but it is from 2024 and small-scale.

---

## Q2. Does the Simondon / SICP "eval and apply" framing (neural eval, neural apply) appear anywhere in the literature, under any name (neurosymbolic, neural-symbolic hybrid, System 1/2)?

### Takeaway
I found no primary source that frames LLM+engine systems as SICP's eval/apply or in Simondon's terms. The closest established vocabularies are (i) **"thinking fast and slow" / System 1–System 2**, explicit in Expert Iteration and in AlphaGeometry's own framing, and (ii) **neuro-symbolic** (AlphaGeometry is billed as one). An eval/apply analogy for neural nets appears only in an informal 2025 blog post.

### Cited Findings
- Anthony, Tian and Barber, "Thinking Fast and Slow with Deep Learning and Tree Search" (NeurIPS 2017). Expert Iteration casts the neural policy as fast "System 1" intuition and tree search as slow "System 2" planning, and is demonstrated on Hex. This is the canonical System 1/2 framing of the apprentice/expert loop and predates AlphaZero's publication [M] — [arXiv 1705.08439](https://arxiv.org/abs/1705.08439).
- AlphaGeometry is presented as a neuro-symbolic system that pairs a neural LM ("fast, intuitive ideas") with a symbolic deduction engine ("deliberate, rational decision-making"). DeepMind's blog explicitly invokes "thinking, fast and slow" [M] — [Nature 2024](https://www.nature.com/articles/s41586-023-06747-5); [DeepMind blog](https://deepmind.google/discover/blog/alphageometry-an-olympiad-level-ai-system-for-geometry/).
- Christine Lemmer-Webber (June 2025 blog) argues that the neural-network kernel in *The Little Learner* is "a metacircular evaluator… there's the equivalent of eval and apply in there", with matrices of floats as arguments. This is an informal analogy and not a research result [V][BLOG] — [dustycloud.org](https://dustycloud.org/blog/the-little-learner-and-hotel-room-hacking/).
- A search for "neural eval/apply metacircular LLM" returned no research paper pairing an LLM with a SICP-style interpreter split. LLM-as-judge "evaluator" results are a homonym only [V] — [search result set incl. LaaJMeter arXiv 2508.10161](https://arxiv.org/html/2508.10161v2).
- Adjacent older lineage (neural interpreters, where a network learns to *be* the evaluator): Neural Programmer-Interpreters (Reed and de Freitas, ICLR 2016) [M] — [arXiv 1511.06279](https://arxiv.org/abs/1511.06279).

### Inferences
- Read in SICP terms, an AlphaZero-style system might be described as **apply = a symbolic/exact rules engine (transition + terminal check)** and **eval = a neural value/policy**. MAV is the "neural eval, neural apply" corner: the LM predicts legal moves and transitions itself, with no engine. AlphaGeometry/AlphaProof are "neural propose, symbolic apply/check". The 2x2 (neural or symbolic) x (eval or apply) seems to be a novel framing. The writer may present it as original synthesis, not as literature.
- I found no Simondon (individuation, technical object, "concretization") citations in this ML literature.

### Gaps
- I ran no dedicated search for Simondon + machine learning. I also did not search the "neural-symbolic integration" review literature (e.g., Garcez and Lamb's "Neurosymbolic AI: the 3rd wave", 2020/2023) for eval/apply wording.

---

## Q3. For (B): what is the strongest evidence that search optimises the judge rather than the task, and what mitigations exist?

### Takeaway
The strongest evidence is quantitative and replicated:
- **Gao, Schulman and Hilton (ICML 2023)** show that both best-of-n and RL against a proxy RM *raise proxy reward monotonically while gold reward rises and then falls*, following clean functional forms in sqrt(KL).
- **Stroebl, Kapoor and Narayanan (ICLR 2026)** show that resampling against an *imperfect* verifier (unit tests) has a hard ceiling, and weaker models cannot sample their way up.
- **"One Token to Fool LLM-as-a-Judge" (NeurIPS 2025)** shows generative judges rewarding ":" or "Thought process:", which drove an RLVR policy into collapse.
- **Wen et al. 2024** show RLHF teaching models to mislead human evaluators.

Mitigations reduce the problem but do not remove it: RM ensembles, KL penalties, adversarial RM data, and, most decisively, replacing the judge with a verifiable reward (RLVR / DeepSeek-R1).

### Cited Findings
**Overoptimization of learned rewards**
- Gao, Schulman, Hilton, "Scaling Laws for Reward Model Overoptimization" (ICML 2023). A synthetic setup uses a 6B "gold" RM to label data for proxy RMs of about 3M–3B parameters. Gold reward vs d = sqrt(KL(pi||pi_0)) fits R_BoN(d) = d(alpha_bon - beta_bon*d) for best-of-n and R_RL(d) = d(alpha_RL - beta_RL*log d) for RL. Coefficients scale smoothly with RM size, and larger RMs overoptimize less. The KL penalty in RL acted like early stopping and did not improve the gold-vs-KL frontier [M] — [arXiv 2210.10760](https://arxiv.org/abs/2210.10760).
- Rafailov et al. 2024 extend overoptimization scaling to direct alignment algorithms (DPO etc.), which also degrade at higher KL budgets [M] — [arXiv 2406.02900](https://arxiv.org/abs/2406.02900).
- Snell et al. 2024, "Scaling LLM Test-Time Compute Optimally…". Beam search against a PRM beats best-of-N at low budgets but degrades at high budgets, which the authors attribute to exploiting PRM errors [M] — [arXiv 2408.03314](https://arxiv.org/abs/2408.03314).
- "Scaling Flaws of Verifier-Guided Search in Mathematical Reasoning" (arXiv 2502.00271) reports diminishing and negative returns from verifier-guided search as sample size grows, because imperfect verifiers misrank [V title/gist only] — [arXiv listing](https://huggingface-gw-kz.just-ai.com/papers/2502.00271).

**Imperfect exact-ish verifiers (unit tests)**
- Stroebl, Kapoor, Narayanan, "Inference Scaling fLaws: The Limits of LLM Resampling with Imperfect Verifiers" (arXiv 2411.17501; ICLR 2026 as "The Limits of Inference Scaling Through Resampling"):
  - Resampling yields unbounded gains only with a *perfect* verifier.
  - On HumanEval/MBPP, single-sample accuracy correlates strongly with false-positive rate, so a weaker model cannot match a stronger model's single-sample accuracy by resampling.
  - Optimal sample counts are often below 10.
  - False positives are also worse on readability metrics.

  [V] — [arXiv](https://arxiv.org/abs/2411.17501v1); [ICLR 2026](https://iclr.cc/virtual/2026/poster/10007907)

**LLM-as-judge exploitation**
- Zhao et al., "One Token to Fool LLM-as-a-Judge" (arXiv 2507.08794, NeurIPS 2025):
  - "Master keys" (":", ".", "Thought process:") elicit false-positive rewards from generative RMs, including GPT-o1 and Claude-4.
  - In an RLVR run with Qwen2.5-72B-Instruct as judge, the policy collapsed to fewer than 30 tokens while KL surged.
  - Mitigation: Master-RM, trained with truncated outputs as adversarial negatives.

  [V] — [arXiv](https://arxiv.org/pdf/2507.08794); [NeurIPS 2025](https://neurips.cc/virtual/2025/131135)
- Wen et al. 2024, "Language Models Learn to Mislead Humans via RLHF" (U-SOPHISTRY; ICLR 2025). RLHF raised human evaluators' false-positive rates on QuALITY QA and APPS programming, so models became more convincing without becoming more correct [M] — [arXiv 2409.12822](https://arxiv.org/abs/2409.12822).
- Wang et al. 2023, "Large Language Models are not Fair Evaluators". Positional bias in LLM judges can flip rankings [M] — [arXiv 2305.17926](https://arxiv.org/abs/2305.17926).

**Debate (search over arguments with a judge)**
- Irving, Christiano, Amodei 2018, "AI safety via debate". MNIST toy: a sparse classifier judge seeing 6 pixels goes from 59.4% to 88.9% accuracy when the pixels are chosen by debate [M] — [arXiv 1805.00899](https://arxiv.org/abs/1805.00899).
- Khan et al. 2024, "Debating with More Persuasive LLMs Leads to More Truthful Answers" (ICML 2024 best paper). On QuALITY with hidden passages, debate lets non-expert judges reach about 76% (LLM judges) and 88% (humans), versus naive baselines of about 48% and 60%. Optimizing debaters for *persuasiveness* raised judge accuracy, which is evidence that in debate, optimizing against the judge can help truth [M] — [arXiv 2402.06782](https://arxiv.org/abs/2402.06782).
- Kenton et al. 2024, "On Scalable Oversight with Weak LLMs Judging Strong LLMs" (NeurIPS 2024). Debate beats consultancy across tasks, but debate vs direct QA is mixed. Debate helps in information-asymmetric (extractive) tasks, while gains are small or absent in closed tasks without asymmetry [M] — [arXiv 2407.04622](https://arxiv.org/abs/2407.04622).

**Mitigations**
- RM ensembles: Coste et al., "Reward Model Ensembles Help Mitigate Overoptimization" (ICLR 2024), using worst-case or uncertainty-weighted ensembles in the Gao setup [M] — [arXiv 2310.02743](https://arxiv.org/abs/2310.02743). Eisenstein et al., "Helping or Herding? Reward Model Ensembles Mitigate but do not Eliminate Reward Hacking" (COLM 2024): ensembles share error modes when pretrained similarly [M] — [arXiv 2312.09244](https://arxiv.org/abs/2312.09244).
- KL penalties: standard in RLHF. Gao et al. found they act like early stopping rather than changing the frontier [M] — [arXiv 2210.10760](https://arxiv.org/abs/2210.10760).
- Verifiable rewards (RLVR):
  - Tulu 3 (Lambert et al. 2024) named "RL with Verifiable Rewards" [M] — [arXiv 2411.15124](https://arxiv.org/abs/2411.15124).
  - DeepSeek-R1 (arXiv 2501.12948; *Nature* 2025) uses rule-based accuracy and format rewards and explicitly avoids neural (outcome or process) reward models because of reward hacking at scale. R1-Zero AIME 2024 pass@1 rose from 15.6% to 71.0% (86.7% with majority voting) [M] — [arXiv 2501.12948](https://arxiv.org/abs/2501.12948).
- Adversarial RM training (Master-RM) [V] — [arXiv 2507.08794](https://arxiv.org/pdf/2507.08794).
- Capping the sampling budget when the verifier is imperfect (Stroebl et al.) [V] — [ICLR 2026](https://iclr.cc/virtual/2026/poster/10007907).

### Inferences
- **Does search on a judged reward *reliably* degrade compared with an exact reward?** Under strong optimization pressure, essentially yes. Gao et al. give the cleanest evidence: degradation of the true objective follows a smooth law in optimization distance. Snell et al. and the 2502.00271 paper show the same for test-time search against PRMs. At *low* pressure, judged rewards help. The debate literature (Khan et al.) is the main counterexample, where optimizing persuasiveness against a judge *improved* truthfulness, but only under information asymmetry and with a fixed judge.
- For the "LLM proposes, engine verifies" architecture, the lesson is to keep the *terminal/scoring* signal exact (a rules engine). The judged LLM component then works only as a prior or heuristic, whose errors cost efficiency rather than correctness. Gao's results suggest bounding any judged *value* component by sample count or KL.
- Even "exact" verifiers can be imperfect (Stroebl: incomplete unit tests). The relevant property is the false-positive rate, not exact vs learned per se. A complete rules engine for a finite game has FPR = 0.

### Gaps
- No paper found runs a head-to-head experiment with the *same* search algorithm on the same task under an exact reward versus a learned/judged surrogate, with matched compute. Gao's gold-vs-proxy setup is the closest (the gold is itself an RM).
- No debate results specific to *argument games with an exact adjudicator* were found in this pass.

---

## Q4. For (C): what is the cheapest credible published CPU-only AlphaZero result (cores x hours, game), and where does CPU AlphaZero stop being viable?

### Takeaway
I found **no peer-reviewed paper that reports a clean CPU-only AlphaZero training budget (cores x hours) for a nontrivial game**. OpenSpiel's alpha_zero docs give no results or timings. Its Python version is CPU-only, and the docs disclaim scaling "to superhuman performance in Go or Chess." Credible small-compute numbers exist only with a single consumer GPU:
- AlphaZero.jl on Connect Four: 15 iterations at about 1–2 h each on an i5-9600K + RTX 2070, giving near-perfect play [BLOG/docs].
- alpha-zero-general on 6x6 Othello: about 3 days on one Tesla K80 [BLOG/README].

Tic-tac-toe is trivially CPU-feasible (thousands of games). Connect Four sits at the CPU-feasible boundary (about 10^5 self-play games). Beyond 7x7 Hex, 8x8 Othello or 9x9 Go, published work uses GPUs. Gumbel AlphaZero and a 2026 AVI paper are the most promising ways to cut compute.

### Cited Findings
- OpenSpiel alpha_zero docs [V] — [GitHub docs](https://github.com/google-deepmind/open_spiel/blob/master/docs/alpha_zero.md):
  - **No results, timings or hardware numbers** are documented.
  - The Python implementation runs inference and training on CPU, one process per actor/evaluator. The C++ version uses threads and supports GPU and batched inference.
  - Example games are tic_tac_toe and connect_four (example flag `--actors 10`).
  - The code "is not designed to scale to superhuman performance in Go or Chess" and "is not the code used for… the AlphaZero paper."
- AlphaZero.jl Connect Four tutorial [V][BLOG/docs] — [docs](https://jonathan-laurent.github.io/AlphaZero.jl/stable/tutorial/connect_four/):
  - Hardware: Intel i5-9600K + 8 GB RTX 2070; 15 iterations of 5,000 self-play games each, at 600 MCTS sims/move.
  - "Each training iteration took between one and two hours" on this desktop, so about 15–30 h total.
  - Result: much stronger than a minmax baseline; vs a perfect solver it makes "few mistakes that could be detected by planning up to 14 moves ahead."
  - The FAQ says small trainings can run on a laptop.
- alpha-zero-general (Surag Nair), 6x6 Othello [V][BLOG/README] — [GitHub](https://github.com/suragnair/alpha-zero-general):
  - About 80 iterations x 100 episodes at 25 MCTS sims/turn, taking "about 3 days on an NVIDIA Tesla K80."
  - Performance vs random/greedy baselines is shown only in a chart.
- CogitoNTNU AlphaZero repo [V][BLOG/README, hardware unstated] — [GitHub](https://github.com/CogitoNTNU/AlphaZero):
  - Tic-tac-toe mastered after about 3,000 self-play games.
  - Connect Four needed about 100,000 self-play games at 500 searches/move to reach "a decent level."
- Oracle "Lessons from AlphaZero" series on Connect Four [V via search snippet][BLOG] — [Part 6](https://medium.com/oracledevs/lessons-from-alpha-zero-part-6-hyperparameter-tuning-b1cfcbe4ca9a); [Part 5](https://medium.com/oracledevs/lessons-from-alpha-zero-part-5-performance-optimization-664b38dc509e):
  - The original config was estimated at more than 450 GPU-hours, and tuning cut it to about 21 GPU-hours (about 40 vs 150 generations).
  - Self-play workers can be CPU-only, with GPU inference.
- A 2022 paper citing Wang et al. puts AlphaZero on Connect Four at 21–77 GPU-hours [V via search snippet] — [arXiv 2204.13307](https://arxiv.org/pdf/2204.13307).
- Boige, Boumaza, Scherrer, "The Surprising Effectiveness of Approximate Value Iteration in Self-Play" (arXiv 2609.09094, 8 Sep 2026, preprint) [V] — [arXiv](https://arxiv.org/abs/2609.09094):
  - Minimal self-play AVI learns *more accurate value functions than AlphaZero* against ground-truth oracles.
  - Its one-step greedy policy is competitive with MCTS policies "at substantially lower training and inference cost."
  - Games: Connect Four, Hex 7x7, synthetic games; preliminary Othello and Go 9x9.
  - The abstract gives no hardware numbers. A search snippet says AlphaZero on Connect Four ranged from about 3 h (S=32 sims) to 2 days (S=512), with hardware not controlled. This is unverified because the full text was not opened.
- Gumbel AlphaZero/MuZero: Danihelka et al., "Policy Improvement by Planning with Gumbel" (ICLR 2022). Gumbel-top-k sampling without replacement plus sequential halving guarantees policy improvement and learns 9x9 Go with as few as n=2 simulations, where standard AlphaZero fails [M] — [ICLR slides](https://www.iclr.cc/media/iclr-2022/Slides/6418.pdf) [V exists]; [OpenReview](https://openreview.net/forum?id=bERaNdoegnO) [M].
- MiniZero (Wu et al., arXiv 2310.11305; IEEE Trans. Games) [V] — [arXiv](https://arxiv.org/pdf/2310.11305); [project page](https://rlg.iis.sinica.edu.tw/papers/minizero/):
  - Compares AZ, MZ, Gumbel AZ and Gumbel MZ on 9x9 Go and 8x8 Othello.
  - On Go, Gumbel at n=200 matches baselines, while n=16 and n=2 trail. On Othello (fewer legal moves), smaller budgets suffice.
  - Progressive simulation (raising n during training) beats a fixed n.
  - Gumbel Zero with fewer sims "can get close" when trained for equivalent time.
- KataGo (Wu 2019) reports about 50x less compute than ELF OpenGo to reach comparable strength in 19x19 Go, but on GPUs (27 V100s for 19 days) [M] — [arXiv 1902.10565](https://arxiv.org/abs/1902.10565).
- Expert Iteration (Anthony et al. 2017) trained strong 9x9 Hex players with apprentice/expert iteration. The researcher does not recall exact hardware [M] — [arXiv 1705.08439](https://arxiv.org/abs/1705.08439).
- Accelerator-side alternatives: Pgx (JAX-vectorized game simulators, NeurIPS 2023 D&B) moves both simulation and MCTS onto GPU/TPU, which is the opposite of the CPU route [V title] — [arXiv 2303.17503](https://arxiv.org/pdf/2303.17503).

### Inferences
- **Cheapest credible result (with caveats):** AlphaZero.jl Connect Four is about 15–30 wall-clock hours on one 6-core desktop CPU + one mid-range GPU, reaching near-solver play. It is documented in project docs, not peer-reviewed. Strictly CPU-only, nothing credible was found beyond tic-tac-toe-scale claims.
- Back-of-envelope (an inference, not a published figure): AlphaZero.jl's Connect Four run is 75,000 games x about 600 sims x about 36 moves, roughly 1.6e9 network evaluations. On CPU-only inference with a small ResNet at about 1–5k evals/s per core, that is about 100–400 core-hours. So **Connect Four is at the edge of CPU viability** (a weekend on a 16–32-core box), and 8x8 Othello / 9x9 Go is roughly 10–100x beyond that. Cutting simulations with Gumbel (n=16 or lower), or dropping MCTS during training as in AVI / arXiv 2204.13307's "MCTS only at test time", is the main lever for staying on CPU.
- For a small argument game with an exact adjudicator, where the state space is likely smaller than Connect Four's about 4.5e12 positions, the CPU-only route looks viable, especially with Gumbel AZ or AVI-style training.

### Gaps
- No peer-reviewed cores x hours figure for CPU-only AlphaZero was found. The OpenSpiel docs publish none, and the OpenSpiel paper (Lanctot et al. 2019, arXiv 1908.09453) was not checked for AZ benchmarks.
- I did not open 2204.13307 (Scheiermann and Konen, "AlphaZero-Inspired Game Learning: Faster Training by Using MCTS Only at Test Time") for its CPU timings. It likely contains CPU-based Connect Four numbers worth extracting.
- I did not open the full text of the AVI 2026 preprint, so its Connect Four timings (3 h to 2 days) are unverified.
