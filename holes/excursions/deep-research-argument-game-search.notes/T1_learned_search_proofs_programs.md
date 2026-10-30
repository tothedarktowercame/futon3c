# Learned tree search (AlphaZero-style: policy/value + search + self-play) for theorem proving and program synthesis

Provenance legend: [V] = verified this session from the primary source (full text or abstract read on 2026-10-30). [K] = from the cited primary paper as I know it from prior reading, not re-fetched this session; the writer should treat [K] numbers as "likely correct, spot-check before quoting". Press-only claims are flagged explicitly.

## Q1. Per-system profile: search, reward, state/move representation, compute, headline results

### Takeaway
The prior art splits into three eras. (a) 2016-2022: step-level (tactic-level) tree search with a learned policy, and sometimes a learned value: Holophrasm, rlCoP, TacticToe, GPT-f, Polu 2022, HTPS. (b) 2023-2024: LLM tactic generators with best-first search (ReProver) or light MCTS (DeepSeek-Prover-V1.5 RMaxTS). (c) 2025-2026: two branches. The dominant one is whole-proof generation by large LLMs trained with RL against the Lean checker plus iterative refinement (Kimina, Goedel-V2, DeepSeek-Prover-V2, Seed-Prover). The other is the one true AlphaZero-scale system, AlphaProof (Nature, Nov 2025), with MCTS over AND-OR trees and an ~80,000-TPU-day RL loop. Every serious system uses an exact checker (Lean, Metamath or HOL4) as the reward. None uses a learned judge for the final reward.

### Cited Findings

**HyperTree Proof Search / Evariste (Lample, Lachaux, Lavril, Martinet, Hayat, Ebner, Rodriguez, Lacroix; Meta AI; NeurIPS 2022) [V]**
- Search: AlphaZero-inspired search over a *hypergraph*, because a tactic maps one goal to a set of subgoals that must *all* be proved. Each simulation expands a partial proof tree. The value backed up to a tactic is the product of the children's values. The selection policy is PUCT (or a variant) with virtual counts for batching. — [HTPS paper (arXiv 2205.11491)](https://arxiv.org/abs/2205.11491)
- Explicit design argument against plain MCTS. Theorem proving maps to a two-player game, with one player choosing tactics and an adversary choosing the hardest goal (the framing of Holophrasm). The authors cite that MCTS is "suboptimal" in this setting because it ignores "unlikely but critical moves from the opponent (in our case, a subgoal that looks easy but is impossible to solve)". HTPS therefore back-propagates over *all* opponent moves, which is cheap because "most tactics lead to one or two goals". — [HTPS App. A.4](https://arxiv.org/pdf/2205.11491)
- Models: a single encoder-decoder transformer with shared weights serves as both policy (goal to tactic) and critic. The critic is decoded as P(PROVABLE | goal) via a special token. Critic targets: 1 for solved nodes, 0 for invalid nodes, and the HTPS action-value estimate W/N for internal nodes ("soft critic"). — [HTPS §5](https://arxiv.org/pdf/2205.11491)
- Environments: Metamath (set.mm), Lean 3 (mathlib, miniF2F) and *Equations*, a purpose-built synthetic environment for trigonometric and real identities with elementary rewrite rules. A cheap synthetic environment is the closest analogue to an "s-expression evaluation game". — [HTPS §6, App. B](https://arxiv.org/pdf/2205.11491)
- Training: *online* training. Provers run search, trainers update, and models are refreshed on the provers every 5 minutes, unlike batch expert iteration. Lean: 32 trainers and 200 provers on A100s, "Each prover runs our Lean API on 48 CPU cores", i.e. (200+32) A100-days per day of training. Metamath: 128 trainers and 256 provers on V100s, with ablations on 16+32 GPUs. Equations ablations used 16 trainers and 32 provers, 48 V100s in total. — [HTPS §7.1](https://arxiv.org/pdf/2205.11491)
- Results: Metamath held-out set 65.4% with the supervised model under HTPS (GPT-f 56.5%). Online training raises this to 82.6% cumulative on valid. Lean miniF2F-test is 41.0% pass@64 vs 36.6% for GPT-f/Polu 2022, and miniF2F-curriculum goes from 31% to 42%. Train time is quoted as roughly 10x less than GPT-f to beat it on miniF2F-test, since Table 2 lists 230 A100-days for Evariste-1d vs 2000 for GPT-f. — [HTPS abstract and Table 2](https://arxiv.org/abs/2205.11491)
- Code: the official repo is facebookresearch/Evariste. — [GitHub](https://github.com/facebookresearch/Evariste) [K]

**AlphaProof (Hubert, Mehta, Sartran, et al., Silver; Google DeepMind; Nature, published online 12 Nov 2025, print Vol 651, 19 Mar 2026) [V]**
- MDP: the state is the Lean 4 tactic state (pretty-printed hypotheses and goals) and an action is a tactic as a text string. Reward is r_t = -1 per tactic. An episode ends at a Lean-verified proof or a timeout. At multi-goal states the return is the *minimum* over subgoals (the longest branch), so the value is about −(number of remaining steps on the hardest branch). — [AlphaProof Nature paper](https://www.nature.com/articles/s41586-025-09833-y) ([PDF](https://www.nature.com/articles/s41586-025-09833-y.pdf))
- Network: a 3B-parameter encoder-decoder. The encoder reads the tactic state. The decoder is the policy and samples K=6 tactics in parallel. A value head on the encoder outputs a categorical distribution over 64 bins. — [Supplementary Tables 2-3](https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41586-025-09833-y/MediaObjects/41586_2025_9833_MOESM1_ESM.pdf)
- Search: adapted from AlphaZero and Sampled MuZero, with three changes. (i) Progressive sampling adds K more tactics when n(s) ≤ C·N(s)^α, with C=0.01 and α=0.6. (ii) AND nodes for multi-goal states: PUCT at an AND node uses (1−Q), which prioritises "the unproven subgoal perceived as most difficult", and backup takes the minimum over children. This is in effect the adversary of the prover/adversary framing. (iii) A single search per attempt with no move commitment, "analogous to a single-player game in a perfectly simulated environment". Lean states that are identical up to renaming are merged. — [Nature paper, Methods "Tree search algorithm"](https://www.nature.com/articles/s41586-025-09833-y.pdf)
- Disproofs: a custom tactic turns a goal into its negation, so the agent also learns from *disproofs* of mis-formalized statements, and the matchmaker records each outcome as proved, disproved or timeout. — [Nature paper, Methods](https://www.nature.com/articles/s41586-025-09833-y.pdf)
- Data and compute: pretraining on ~300B tokens and Mathlib SFT on 300k state-tactic pairs (~10 TPU-days). Autoformalization with a Gemini-based model turned ~1M natural-language problems into ~80M formal statements at ~100,000 TPU-days. Main RL took ~80,000 TPU-days ("4,000 TPUs utilized for 20 days", TPU v6e). The learner mixes 10% Mathlib SFT with 90% replay of proofs and disproofs, and failed attempts are discarded. — [Nature paper, "Computational budget summary"](https://www.nature.com/articles/s41586-025-09833-y.pdf)
- Test-time RL (TTRL): the system generates "millions of related problem variants" around a hard target and runs RL on that bespoke curriculum, at 50-500 TPU-days per problem. — [Nature paper](https://www.nature.com/articles/s41586-025-09833-y.pdf)
- Results (Table 1):

  | Configuration | miniF2F-test | formal-IMO | PutnamBench-test |
  |---|---|---|---|
  | 2 TPU-min search | 96.3% | 33.2% | 27.9% |
  | 12 TPU-h search | 97.7% | 43.7% | 39.4% |
  | TTRL, 50 TPU-days | 97.5% | 53.9% | 45.5% |
  | TTRL, 500 TPU-days | 99.6% | 58.3% | 56.1% |

  — [Nature paper Table 1](https://www.nature.com/articles/s41586-025-09833-y.pdf)
- IMO 2024: AlphaProof solved P1, P2 and P6, each needing "2-3 days of TTRL". Experts manually formalized the problems. Candidate answers came from Gemini 1.5 Pro, and AlphaProof "rapidly refut[ed]" the wrong ones. Combinatorics problems P3 and P5 went unsolved. With AlphaGeometry 2 the total was 28/42 (silver). — [Nature paper](https://www.nature.com/articles/s41586-025-09833-y.pdf); see also the July 2024 [DeepMind blog](https://deepmind.google/discover/blog/ai-achieves-silver-medal-standard-solving-international-mathematical-olympiad-problems/) (press, pre-paper).
- Subject breakdown: combinatorics stays weak. Formal-IMO combinatorics is 11.2% at 2 TPU-min and 20.3% at 500 TPU-days of TTRL, against 75.7% for number theory and 72.6% for algebra. — [Supplementary Table 1](https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41586-025-09833-y/MediaObjects/41586_2025_9833_MOESM1_ESM.pdf)

**GPT-f (Polu & Sutskever, 2020; OpenAI) [K]**
- Metamath. A decoder-only transformer generates proof steps (goal to substitution/theorem), and the search is *best-first* over goals, scored by cumulative log-prob or a learned value. The value function ("outcome" objective) was trained on proof-search attempts and iterated, i.e. an early form of expert iteration. It reached 56.22% on the held-out set (HTPS quotes 56.5%) vs ~21% for the earlier MetaGen-IL. 23 GPT-f-shortened proofs were accepted into set.mm. — [arXiv 2009.03393](https://arxiv.org/abs/2009.03393)

**Polu, Han, Zheng, Baksys, Babuschkin, Sutskever, "Formal Mathematics Statement Curriculum Learning" (Feb 2022; ICLR 2023) [V abstract / K details]**
- Lean 3. The core loop is expert iteration: proof search with best-first search over tactic states, then training on the proofs found, repeated. "At same compute budget, expert iteration … dramatically outperforms proof search only" [V]. Starting from statements of varied difficulty, without ground-truth proofs, it "finds and solves a curriculum of increasingly difficult problems" [V]. — [arXiv 2202.01344](https://arxiv.org/abs/2202.01344)
- miniF2F-test 36.6% pass@64, confirmed as the GPT-f baseline in HTPS Table 2 [V via HTPS]. It solved some adapted IMO problems [K]. Training cost is about 2000 A100-days per HTPS's comparison table [V via HTPS](https://arxiv.org/pdf/2205.11491).

**Holophrasm (Whalen, 2016) [K]**
- Metamath. UCT-style tree search with three networks: a relevance net (which theorem to apply), a generative net (substitutions) and a payoff/value net. The game is explicitly two-player: a prover picks an action, and an adversary picks the subgoal most likely to fail, which the HTPS appendix describes. It proved ~14% of held-out set.mm theorems [K, verify]. — [arXiv 1608.02644](https://arxiv.org/abs/1608.02644); framing confirmed in [HTPS App. A.4](https://arxiv.org/pdf/2205.11491) [V]

**rlCoP (Kaliszyk, Urban, Michalewski, Olšák; NeurIPS 2018) [V abstract / K details]**
- Monte Carlo simulations guided by RL over connection-tableau (leanCoP) proof states on Mizar40 problems. The strongest version "solves over 40% more problems than a baseline prover" at the same number of inferences [V]. The policy and value were gradient-boosted trees (XGBoost) on hand-built features and it ran on CPUs, a key small-compute data point [K]. — [arXiv 1805.07563](https://arxiv.org/abs/1805.07563)

**TacticToe (Gauthier, Kaliszyk, Urban, Kumar, Norrish; JAR 2021, first version 2017) and TacticZero (Wu, Jiang, Ba, Grosse; NeurIPS 2021) [K]**
- TacticToe: HOL4 with k-NN / learned tactic and argument prediction from human proofs. Later versions use MCTS with a learned value, all on CPU. It proved about 66% of the 7,164 HOL4 standard-library theorems at 60 s, vs about 35% for E prover [K, verify]. — [arXiv 1804.00596](https://arxiv.org/abs/1804.00596)
- TacticZero: HOL4 deep RL *from scratch* with no human proofs. Goal-selection, tactic and argument policies are trained with policy gradient, and the search is a learned "proof search" over a fringe of goals, not MCTS. It outperformed TacticToe-like baselines on its benchmark [K]. — [arXiv 2102.09756](https://arxiv.org/abs/2102.09756)

**LeanDojo / ReProver (Yang et al.; NeurIPS 2023 Datasets & Benchmarks) and Lean Copilot (Song, Yang, Anandkumar; 2024) [K]**
- ReProver: a retrieval-augmented ByT5 (~300M) tactic generator with *best-first search* over Lean tactic states. Pass@1 was ~51% on the LeanDojo Benchmark random split vs ~29% for GPT-4 zero-shot [K]. Explicitly low compute: "one GPU week" of training [K]. LeanDojo also provides the gym-like Lean interaction toolkit. — [arXiv 2306.15626](https://arxiv.org/abs/2306.15626)
- Lean Copilot runs LLM inference natively inside Lean for suggest_tactics, search_proof (LLM-guided aesop best-first search) and premise selection, and is a human-in-the-loop tool. It reported automating ~74% of proof steps vs ~40% for aesop [K, verify]. — [arXiv 2404.12534](https://arxiv.org/abs/2404.12534)

**DeepSeek-Prover V1 / V1.5 / V2 (DeepSeek; May 2024, Aug 2024, Apr 2025)**
- V1 [K]: whole-proof generation trained on ~8M synthetic formal statements created by autoformalization and filtered by Lean (expert-iteration style). miniF2F-test ~46% at 64 samples. — [arXiv 2405.14333](https://arxiv.org/abs/2405.14333)
- V1.5 [V]: a 7B model with SFT plus RLPAF, i.e. RL from proof-assistant feedback with a binary Lean reward. Its search is **RMaxTS**, an MCTS over *tactic-level* nodes in which each expansion resumes whole-proof generation from a prefix and the output is truncated at the first Lean error. The intrinsic reward is 1 if an expansion adds a new node, and the selection rule is discounted UCB with γ=0.99. Scale: 256 MCTS runners per node, one LM per GPU at batch size 512 and 32 thread workers per tree, with Lean REPL checks on "thousands of CPU cores", each sandboxed with a 300 s limit. Results: miniF2F-test 63.5% and ProofNet 25.3%. — [arXiv 2408.08152 (HTML)](https://arxiv.org/html/2408.08152)
- V2 [V abstract]: a 671B model (plus a 7B per the repo [K]). DeepSeek-V3 decomposes problems into subgoals, the subgoals are proved recursively, and the proofs are combined into a CoT cold start for RL. Results: 88.9% miniF2F-test and 49/658 PutnamBench. On the 15 ProverBench AIME 24-25 problems it solved 6, vs 8 for informal DeepSeek-V3 with majority voting. The abstract mentions no tree search. — [arXiv 2504.21801](https://arxiv.org/abs/2504.21801)

**Goedel-Prover, Kimina-Prover (2025)**
- Kimina-Prover Preview (Moonshot/Numina, Apr 2025) [V abstract]: RL from Qwen2.5-72B with a "formal reasoning pattern" that interleaves thinking and Lean. It reports 80.7% miniF2F at pass@8192 and is explicitly positioned against "traditional search algorithms". Distilled 1.5B and 7B versions are open. — [arXiv 2504.11354](https://arxiv.org/abs/2504.11354)
- Goedel-Prover (Princeton, Feb 2025) [K]: whole-proof generation with expert iteration on ~1.64M autoformalized statements, 57.6% miniF2F pass@32 at 7B. — [arXiv 2502.07640](https://arxiv.org/abs/2502.07640). Goedel-Prover-V2 (Aug 2025) [K]: scaffolded data synthesis, verifier-guided self-correction and model averaging. The 32B model gets ~88% miniF2F pass@32 (~90% with self-correction), and the 8B is comparable to DeepSeek-Prover-V2-671B on miniF2F. — [arXiv 2508.03613](https://arxiv.org/abs/2508.03613)

**BFS-Prover (ByteDance, Feb 2025; ACL 2025) [V via search snippet of the paper]**
- Argues "simpler methods like Best-First Search" are underexplored relative to "value functions and MCTS". It is a step-level BFS with expert iteration, DPO on tactic pairs using compiler errors as negatives, and length normalization. It reaches 72.95% miniF2F-test (cumulative) with no value function. — [arXiv 2502.03438](https://arxiv.org/html/2502.03438v2)

**Seed-Prover (ByteDance Seed, 31 Jul 2025) [V abstract]**
- A "lemma-style whole-proof reasoning model" that "iteratively refine[s] its proof based on Lean feedback, proved lemmas, and self-summarization", with three test-time inference strategies for "deep and broad reasoning". Results: 78.1% of formalized past IMO problems, "saturates MiniF2F", >50% PutnamBench, and 5/6 IMO 2025 problems (with Seed-Geometry). The abstract mentions no MCTS. — [arXiv 2507.23726](https://arxiv.org/abs/2507.23726)

**Other 2025-2026 successors**
- AxiomProver (Axiom Math): no system paper found. Axiom's own materials say it is "an autonomous multi-agent ensemble theorem prover for Lean 4". It solved 8/12 Putnam 2025 problems within the exam window and the remaining 4 afterwards. The Lean repo gives per-problem compute, e.g. A1 at "110 minutes, 7M tokens", 652 lines. Arxiv papers exist whose *proofs* AxiomProver generated, e.g. arXiv 2602.05095. This is company material, not a peer-reviewed method description. — [Axiom blog, 8 Jan 2026](https://axiommath.ai/research/from-seeing-why-to-checking-everything); [Lean Reservoir repo](https://reservoir.lean-lang.org/@AxiomMath/Putnam2025); [arXiv 2602.05095](https://arxiv.org/pdf/2602.05095)
- Harmonic "Aristotle" (Oct 2025) [K, verify]: reported IMO-2025 gold-level formal solutions using Lean search (described as a Monte-Carlo-graph-search-style prover) plus an informal lemma pipeline. — [arXiv 2510.01346](https://arxiv.org/abs/2510.01346)

**Program synthesis**
- AlphaDev (Mankowitz et al., Nature 618, 2023) [K]: sorting and hashing framed as single-player "AssemblyGame". The state is the program so far plus the CPU/memory state after executing it on test inputs, the actions are x86 assembly instructions, and the reward combines correctness (exact, by running test cases) and latency (measured or proxied by length). Search is AlphaZero MCTS with a transformer program encoder and a CPU-state encoder. Results: sort3 with one fewer instruction than the human baseline, sort5 shorter, and libc++ sort up to ~70% faster on short sequences and ~1.7% on >250k elements, merged into LLVM libc++. A stochastic-superoptimization baseline (AlphaDev-S) was competitive on short programs but needed far more compute or failed on longer ones [K, verify]. — [Nature paper](https://www.nature.com/articles/s41586-023-06004-9); [DeepMind blog (press)](https://deepmind.google/discover/blog/alphadev-discovers-faster-sorting-algorithms/)
- AlphaCode (Li et al., Science 2022) [K]: **no tree search**. It samples up to ~1M programs per problem, filters on the example tests (an exact checker) and clusters the survivors down to 10 submissions. Average ranking was top 54.3% in simulated Codeforces contests, and the solve rate grows roughly log-linearly with sample count. — [arXiv 2203.07814](https://arxiv.org/abs/2203.07814). AlphaCode 2 (Dec 2023, ~85th percentile) appears only in a tech report and press.
- DreamCoder (Ellis et al., PLDI 2021) [K]: wake-sleep library learning. A neural "recognition model" predicts a distribution over DSL primitives and guides *enumerative* (best-first by probability) search, not MCTS. The sleep phase compresses solutions into new library abstractions ("abstraction sleep") and trains the recognition model on replays and "dreams". Ablations show both library learning and the recognition model contribute. It ran on CPUs. — [arXiv 2006.08381](https://arxiv.org/abs/2006.08381)
- Neural-Guided Deductive Search, NGDS (Kalyan, Mohta, Polozov, Batra, Jain, Gulwani; ICLR 2018) [K]: inside Microsoft PROSE's top-down deductive synthesizer, a learned model scores branches and the search prunes or branch-and-bounds on those scores. It reported large speed-ups (order of 10x average) over PROSE at comparable accuracy on FlashFill-style string tasks [K, verify]. — [arXiv 1804.01186](https://arxiv.org/abs/1804.01186)
- "Learning to search" programs [K]:
  - DeepCoder (Balog et al., ICLR 2017): a neural net predicts which DSL functions appear and orders enumerative/SMT search, with order-of-magnitude speed-ups. — [arXiv 1611.01989](https://arxiv.org/abs/1611.01989)
  - Write-Execute-Assess (Ellis, Nye, Pu, Sosa, Tenenbaum, Solar-Lezama; NeurIPS 2019): a policy plus a value over *executed partial programs* in a REPL with SMC/beam search, essentially an AlphaZero-like loop where the "state" is the evaluated s-expression. This is the closest analogue to an s-expression evaluation game. — [arXiv 1906.04604](https://arxiv.org/abs/1906.04604)
  - Execution-guided decoding (Chen, Liu, Song; ICLR 2019). — [arXiv 1807.03100](https://openreview.net/forum?id=H1gfOiAqYm)

### Inferences
- Every system pairs search with an exact symbolic checker as the reward: Lean, Metamath, HOL4, or test execution for programs. A learned judge is used only for *search guidance* (the value/critic) or ranking, never as the final reward. A small argument/s-expression game whose outcome is computed exactly is therefore squarely in the mainstream design.
- The adversarial "prover picks tactic, opponent picks hardest subgoal" structure is already standard: Holophrasm makes it explicit, AlphaProof's AND-node rule uses 1−Q to pick the hardest unproven subgoal and min-backup, and HTPS uses full product backup over all subgoals. An "argument game" maps directly onto it. HTPS's warning that vanilla MCTS on AND nodes ignores "unlikely but critical" opponent moves is the most relevant design caution.
- Cheap synthetic environments are where the ablations happen. HTPS ran most ablations on its Equations rewriting environment and on Metamath, not Lean.

### Gaps
- I did not re-fetch the full texts of GPT-f, Holophrasm, TacticToe, TacticZero, ReProver, Lean Copilot, Goedel-Prover(-V2), AlphaDev, AlphaCode, DreamCoder or NGDS. Numbers marked [K] should be spot-checked.
- I found no system paper for AxiomProver, only company blog and repo material.
- I did not check Seed-Prover 1.5 or later ByteDance successors, or any AlphaProof follow-up beyond the Nature paper. AlphaProof's code is not public.

## Q2. Evidence that the learned value/policy actually saved search (ablations)

### Takeaway
There are explicit, quotable ablations. In HTPS, removing the critic costs 3.8-12.5 points depending on environment and split. AlphaProof's trained agent solves ~30% of historical IMO problems with 300 simulations, a level earlier checkpoints never reach "even with vastly more search". In DeepSeek-Prover-V1.5, MCTS beats plain sampling by only 1.2-2.5 points at matched budget, and plain UCT without its intrinsic-exploration reward gives no gain at all. Expert iteration beats search-only at equal compute (Polu 2022). The evidence that a *learned value* in particular helps is strong for AlphaZero-style systems with their own critic (HTPS, AlphaProof) and weak to absent for the 2025 LLM whole-proof systems, which mostly dropped step-level value functions.

### Cited Findings
- HTPS critic ablation (Table 5; pass@8 on Metamath, cumulative pass rate on Equations). "No critic" means search guided only by the policy, with leaf value fixed at 0.5. — [HTPS §7.2.2](https://arxiv.org/pdf/2205.11491) [V]

  | Setting | Metamath valid | Metamath test | Equations |
  |---|---|---|---|
  | Evariste (soft critic) | 68.6 | 57.4 | 78.1 |
  | No critic | 64.8 | 52.2 | 65.6 |
  | Hard critic | 67.6 | 57.4 | 63.1 |
  | Fixed search params | 69.8 | 56.2 | 73.8 |

  The text's own numbers: the critic "improved the performance significantly, by 5.2% and 12.5% on Metamath and Equations respectively", where the Metamath 5.2 is the test split. Hard 0/1 critic targets in the style of Polu & Sutskever do *worse than no critic* in Equations: "setting all internal nodes to zero is too pessimistic."
- HTPS policy-target ablation (Table 4): AlphaZero-style training on the filtered visit-count search policy reached only 51.6% (Metamath valid pass@8) and 37.5% (Equations). Training on *minimal proofs* reached 68.6% and 78.1% respectively. Learning from all proofs of solved nodes gave 40.6% on Equations, vs 78.1% for minimal proofs. Naive AlphaZero policy distillation underperformed imitation of minimal found proofs. — [HTPS §7.2.1](https://arxiv.org/pdf/2205.11491) [V]
- HTPS online vs expert iteration: refreshing models every 5 min vs 1 h, 6 h or never gives monotonically better cumulative pass rate over training hours, "confirming the benefits of online training over expert iteration". Removing the synthetic co-training task drops Lean miniF2F-valid from 58.6% to 54.9% and test from 41.0% to 38.5%. — [HTPS §7.1.1, §7.2.4](https://arxiv.org/pdf/2205.11491) [V]
- HTPS vs supervised at matched attempts: on Metamath "the supervised model plateaus at 66% while Evariste keeps improving beyond 74% after 7 days", and "this improvement cannot solely be explained by the high number of attempts". — [HTPS §7.1.2](https://arxiv.org/pdf/2205.11491) [V]
- AlphaProof search efficiency from learning (Fig. 3c): "later-stage agents are not only stronger but also more efficient, requiring substantially less search to achieve the same solve rate … the final agent solves approximately 30% of problems with only 300 simulations, a level of performance that earlier agents could not reach even with vastly more search." — [AlphaProof Nature paper](https://www.nature.com/articles/s41586-025-09833-y.pdf) [V]
- AlphaProof search scaling: going from 2 TPU-min to 12 TPU-h of search "boosted solve rates by over 10 absolute percentage points" (formal-IMO 33.2 to 43.7, Putnam 27.9 to 39.4). TTRL adds ~15 more points over 12 TPU-h search. The paper reports no "no-value-head" or "no-search" ablation in the main text. — [AlphaProof Nature paper](https://www.nature.com/articles/s41586-025-09833-y.pdf) [V]
- DeepSeek-Prover-V1.5 matched-budget ablation (miniF2F-test, RL model, CoT). — [arXiv 2408.08152](https://arxiv.org/html/2408.08152) [V]

  | Method | 4×6400 | 16×6400 |
  |---|---|---|
  | Single-pass sampling | 58.4% | 60.2% |
  | UCT without intrinsic reward | 58.2% | 61.1% |
  | RMaxTS with UCB1 instead of discounted UCB | 58.6% | 60.7% |
  | RMaxTS without tactic-state comments | 58.4% | 61.1% |
  | Full RMaxTS | 59.6% | 62.7% |

  The gain is real but small, and the paper attributes it to exploration diversity, not to a learned value: V1.5 has no value network.
- Polu et al. 2022: "At same compute budget, expert iteration, by which we mean proof search interleaved with learning, dramatically outperforms proof search only." — [arXiv 2202.01344](https://arxiv.org/abs/2202.01344) [V]
- rlCoP: learned guidance solves "over 40% more problems than a baseline prover" at the same inference budget. — [arXiv 1805.07563](https://arxiv.org/abs/1805.07563) [V]
- Counter-evidence: BFS-Prover reaches 72.95% miniF2F with *best-first search and no value function*, explicitly challenging the need for value/MCTS at the LLM scale. — [arXiv 2502.03438](https://arxiv.org/html/2502.03438v2) [V snippet]. Kimina reaches 80.7% at pass@8192 with RL-trained whole-proof generation and no tree search. — [arXiv 2504.11354](https://arxiv.org/abs/2504.11354) [V]

### Inferences
- In the regime a small team would build (a small model and a cheap exact environment), the HTPS Equations numbers are the best guide. A learned critic was worth +12.5 points, and it matters *how* critic targets are built: soft, search-backed values work, while hard 0/1 values hurt.
- Policy targets taken from raw visit counts underperformed imitation of minimal successful proofs in HTPS. A naive AlphaZero-visit-distillation loop on a proof-like game may underperform a simpler "expert iteration on shortest solutions" loop.
- When the policy is a large pretrained LLM, the marginal value of step-level search shrinks to 1-3 points (DeepSeek V1.5), and the 2025 frontier moved its compute into RL-trained long-CoT sampling instead.

### Gaps
- None of these papers reports "nodes expanded to first proof" with vs without value. The available metrics are pass rate at a fixed budget, or simulations needed for a given solve rate (AlphaProof Fig. 3c, shown only as a plot).
- I found no AlphaProof ablation of the value head or of AND-node handling. AlphaDev's MCTS-vs-stochastic-search comparison was not re-verified this session.

## Q3. Is theorem proving slow or expensive for self-play because of verifier latency?

### Takeaway
Yes, for Lean. Every large Lean system reports CPU-bound verification as a primary engineering cost: HTPS gave every prover 48 CPU cores, DeepSeek used "thousands of CPU cores" with 300 s timeouts, and AlphaProof compiled tactics to C to get 6x speed-ups. A warm Lean REPL with Mathlib preloaded checks a typical whole proof in roughly 0.75-1 s on one machine, but a cold Lean process carries a Mathlib-loading cost. Metamath and synthetic rewrite environments are far cheaper, which is why HTPS ran its ablations there.

### Cited Findings
- HTPS: "Since Lean experiments are CPU intensive, we run most of our ablations on the Equations and Metamath environments." Each Lean prover "runs our Lean API on 48 CPU cores". — [HTPS §7.1.1, §7.2](https://arxiv.org/pdf/2205.11491) [V]
- DeepSeek-Prover-V1.5: Lean REPL verification "on a cluster with thousands of CPU cores", each check in a sandboxed process with a 300-second limit. — [arXiv 2408.08152](https://arxiv.org/html/2408.08152) [V]
- AlphaProof's environment engineering "to operate efficiently and robustly under the high throughput demands of RL":
  - key Mathlib tactics such as `linarith` were compiled to C, so they run "six-times faster";
  - wall-clock limits on each tactic, alongside Lean heartbeats;
  - more frequent `checkSystem` calls;
  - a hard cap on numeral size;
  - save/restore of tactic states so search can resume from any node;
  - multi-threaded concurrent proof states.

  A separate final check runs the standard Lean CLI on a .lean file and verifies that only the three standard axioms are used. — [AlphaProof Nature paper, Methods "Interfacing with Lean"](https://www.nature.com/articles/s41586-025-09833-y.pdf) [V]
- Per-proof check latency: on a 5,000-proof Goedel-workbook workload on one machine, Kimina Lean Server measured ~0.75 s latency per proof check and ~2.13 requests/s, while AXLE measured ~1.05 s and ~2.09 req/s. Kimina's edge comes from dispatching into a warm REPL pool with Mathlib preloaded, which avoids raw-Lean startup cost. These are third-party measurements by the AXLE authors (June 2026). — [AXLE, arXiv 2606.26442](https://arxiv.org/pdf/2606.26442) [V via search summary; not full-text read]
- Inference-time cost at the frontier:
  - AlphaProof: 2 TPU-min to 12 TPU-h per problem for search, and 50-500 TPU-days per problem for TTRL; IMO 2024 solutions took 2-3 days each. — [AlphaProof Nature paper](https://www.nature.com/articles/s41586-025-09833-y.pdf) [V]
  - AxiomProver Putnam 2025 A1: "110 minutes, 7M tokens". — [Axiom Lean repo](https://reservoir.lean-lang.org/@AxiomMath/Putnam2025) [V via search snippet]

### Inferences
- With ~1 s per whole-proof check, or a few ms to hundreds of ms per tactic step (not directly measured in my sources), a single-CPU Lean self-play loop manages on the order of 10^4-10^5 checks per day. That is orders of magnitude below what AlphaZero-style training consumed (AlphaProof: thousands of TPUs for 20 days). A purpose-built s-expression/argument evaluator with microsecond steps would remove the bottleneck that dominates Lean work.
- Metamath (substitution checking) and HTPS-style Equations sit between the two and are the precedent for "cheap exact verifier, lots of self-play".

### Gaps
- I found no published per-*tactic* latency distribution for Lean 4 + Mathlib. AlphaProof gives only the relative 6x for compiled tactics. The Kimina Lean Server's own technical report was not retrieved.
- I found no Isabelle per-step cost numbers in this pass (PISA, Thor and Baldur are not covered).

## Q4. What failed or was abandoned, and why

### Takeaway
Recurring failure modes are reward sparsity, which forces curricula, synthetic statements or autoformalization; exhaustion of real data ("proof-search data on available corpora will likely be insufficient"); verifier cost; weak combinatorics; and a field-level drift away from step-level MCTS toward whole-proof LLM RL once models got large. Specific negative results: naive AlphaZero visit-count targets, hard 0/1 critic targets, and vanilla UCT without exploration bonuses.

### Cited Findings
- HTPS conclusion: "Despite large number of attempts per theorem, proving the entirety of datasets like miniF2F remains elusive, and generating data from proof-search on the currently available corpora will likely be insufficient in the long term." The AlphaZero-style visit-count policy target underperformed (51.6 vs 68.6), and hard critic targets were worse than no critic in Equations (63.1 vs 65.6). — [HTPS §7.2, §8](https://arxiv.org/pdf/2205.11491) [V]
- AlphaProof limitations, in the authors' words:
  - "the bespoke learning phase required by AlphaProof represents a scale of domain-specific training that is likely beyond the reach of most academic research groups";
  - "the multi-day inference time required by TTRL … highlights the need for more efficient inference-time strategies";
  - the move to research mathematics "requires moving beyond pure problem solving to theory building".

  Combinatorics stays at 20.3% even with 500 TPU-days of TTRL, and IMO 2024 P3 and P5 were unsolved. — [AlphaProof Nature paper](https://www.nature.com/articles/s41586-025-09833-y.pdf); [Supp. Table 1](https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41586-025-09833-y/MediaObjects/41586_2025_9833_MOESM1_ESM.pdf) [V]
- Reward sparsity is handled by curricula rather than shaping:
  - Polu 2022 uses a manually curated statement curriculum of varied difficulty [V abstract](https://arxiv.org/abs/2202.01344);
  - AlphaProof uses ~80M autoformalized statements with an adaptive "matchmaker" and learns from disproofs too, with failed attempts discarded [V](https://www.nature.com/articles/s41586-025-09833-y.pdf);
  - HTPS co-trains on synthetic data, worth +2.5 to +3.7 points [V](https://arxiv.org/pdf/2205.11491).
- DeepSeek moved from tree search in V1.5 (RMaxTS gave +1.2 to +2.5 points) to subgoal decomposition plus RL in V2, with no tree search mentioned in the V2 abstract. — [V1.5](https://arxiv.org/html/2408.08152), [V2](https://arxiv.org/abs/2504.21801) [V]. Whether DeepSeek *abandoned* MCTS for stated reasons is not documented in what I read. The move is inferred from the method change.
- Plain UCT without the intrinsic novelty reward gave no gain over plain sampling in DeepSeek-Prover-V1.5 (58.2% vs 58.4% at 4×6400). — [arXiv 2408.08152](https://arxiv.org/html/2408.08152) [V]
- AlphaCode skipped search entirely in favour of massive sampling plus filtering, and AlphaDev's results were limited to very short assembly programs such as sort3-sort5 and small hashing routines. — [AlphaCode](https://arxiv.org/abs/2203.07814), [AlphaDev](https://www.nature.com/articles/s41586-023-06004-9) [K]

### Inferences
- For a small self-play game, the main risks inherited from this literature are:
  1. sparse terminal reward without a curriculum or problem generator;
  2. value targets that are too pessimistic;
  3. policy targets from raw visit counts rather than minimal successful lines;
  4. a verifier that dominates wall-clock time.

  A game whose generator produces graded-difficulty positions, with a cheap exact evaluator, addresses (1) and (4) by construction.

### Gaps
- Few papers publish outright abandoned approaches. Rationales for the 2025 shift away from step-level search appear only implicitly. I found no explicit post-mortem from DeepSeek or ByteDance.

## Q5. CPU-only or small-compute replications

### Takeaway
Real small-compute precedents exist, but they are mostly pre-LLM or use small models: rlCoP and TacticToe (CPU, gradient-boosted or k-NN learners with MCTS), DreamCoder (CPU, enumerative search plus a small recognition net), ReProver (one GPU-week), and HTPS's Equations and Metamath ablation setups (48 V100s, small by HTPS standards). I found no CPU-only replication of AlphaProof or HTPS on Lean.

### Cited Findings
- rlCoP: MCTS-style guided connection-tableau search with learned policy/value, +40% problems over baseline at a matched inference budget. CPU with XGBoost [K]. — [arXiv 1805.07563](https://arxiv.org/abs/1805.07563) [V abstract]
- TacticToe: HOL4, CPU, MCTS with learned tactic selection. — [arXiv 1804.00596](https://arxiv.org/abs/1804.00596) [K]
- ReProver: trainable on "one GPU week". — [arXiv 2306.15626](https://arxiv.org/abs/2306.15626) [K]
- HTPS Equations/Metamath ablations ran on 16 trainers and 32 provers. — [HTPS §7](https://arxiv.org/pdf/2205.11491) [V]
- DreamCoder ran on CPUs with enumerative search guided by a small neural recognition model. — [arXiv 2006.08381](https://arxiv.org/abs/2006.08381) [K]
- Self-play conjecture-and-prove on a small custom logic: Poesia, Broman, Haber, Goodman, "Learning Formal Mathematics From Intrinsic Motivation" (minimo, NeurIPS 2024). Agents learn to *conjecture* and prove from axioms alone in a small dependent-type language (Peano) with MCTS guided by a small transformer, using a hindsight-relabelling curriculum. This is a strong precedent for a small self-play "argument game" [K, verify]. — [arXiv 2407.00695](https://arxiv.org/abs/2407.00695)

### Inferences
- The most transferable recipe at small scale:
  - a cheap custom exact environment (HTPS Equations, minimo/Peano, DreamCoder DSLs, rlCoP's leanCoP);
  - a small policy/value model;
  - AND-node-aware search;
  - online or expert-iteration training on minimal solutions.

  Lean is the wrong substrate for a first CPU-only experiment.

### Gaps
- I found no published CPU-only AlphaZero replication on Lean or Isabelle. The minimo and DreamCoder compute figures were not re-verified this session.
