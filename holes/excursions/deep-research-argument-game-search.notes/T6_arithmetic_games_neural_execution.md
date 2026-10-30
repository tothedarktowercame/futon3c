# T6: Arithmetic-expression construction games as learned-search benchmarks, and the limits of neural program execution

Provenance key. [F] = fetched and read in this session (abstract, HTML or full PDF text). [K] = from the researcher's prior knowledge of the paper; the arXiv/venue link is given, but the page was NOT re-fetched this session, so check any number marked [K] before it is quoted. "Blog/repo-only" means there is no peer-reviewed paper.

## Q1. Is the "arithmetic s-expression game" (partial tree with holes + target number; a move fills the leftmost hole with + - * or a small constant; reward 1 iff eval hits the target) already a published benchmark?

### Takeaway
Not under that exact specification. I found no published benchmark whose state is a prefix-order partial arithmetic tree with holes, filled leftmost-first, with a 0/1 reward for hitting a numeric target. Its parts all exist in three neighbouring families:
- **Countdown / Game of 24.** Numeric target, exact-hit reward. The state is a multiset of remaining numbers, not a tree with holes. Examples: Stream of Search, TinyZero, Tree of Thoughts.
- **Symbolic regression.** The state is a partial expression tree expanded leftmost-first or in pre-order, as in DSR and SPL. The reward is a continuous fit to data, not an exact single number.
- **Program synthesis from I/O examples.** Learned priors guide enumerative search over expression DSLs. Examples: DeepCoder, NGDS, DreamCoder.

The proposed game is closest to "DSR/SPL-style leftmost-hole expansion with a Countdown-style exact target". As far as I found, that combination is unpublished as a named benchmark.

### Cited Findings
**Countdown as a learned-search benchmark: Stream of Search (Gandhi et al., arXiv Apr 2024; published at COLM 2024 [K])**
- Setup: 4 input numbers, targets from 10 to 100, and 10% of targets held out for out-of-distribution (OOD) evaluation. 5-input problems give traces of up to 60,000 tokens. [F] — [arXiv 2404.03683 HTML](https://arxiv.org/html/2404.03683)
- Trace generation: 12 symbolic strategies built on BFS and DFS, guided by two heuristics.
  - Sum heuristic: |sum of remaining numbers − target|.
  - Multiply heuristic: distance between the sum of inputs and the target's factors.
  - DFS threshold = the target. BFS breadth b ∈ {1..5}.
  - 500,000 trajectories, of which 285,501 (57.1%) reach a solution. [F] — [SoS HTML](https://arxiv.org/html/2404.03683)
- State/move: the search state is the set of remaining numbers, and a move combines two numbers with an operation. This is the classic Countdown state, not a tree with holes. [F, inferred from the trace description] — [SoS HTML](https://arxiv.org/html/2404.03683)
- Solve rates on held-out inputs:
  - Model trained only on optimal paths (OP): 25.73%.
  - Stream of Search model (SoS): 51.27% on seen targets and 49.00% on unseen targets.
  - SoS + STaR: 55.59% / 53.34%.
  - SoS + APA (Advantage-Induced Policy Alignment): 56.52% / 54.23%.
  - The abstract also says SoS pretraining raises search accuracy by 25% over optimal-path training. [F] — [SoS HTML](https://arxiv.org/html/2404.03683); [abstract](https://arxiv.org/abs/2404.03683)
- Search-effort metric: average number of states explored in correct trajectories (seen / unseen targets).
  - SoS: 75.04 / 81.75.
  - STaR: 66.18 / 72.25.
  - APA: 64.73 / 70.85.
  - So policy improvement cuts explored states by about 12–14%. Arithmetic errors per trajectory (seen targets): SoS 2.247, STaR 1.927, APA 0.668. [F] — [SoS HTML](https://arxiv.org/html/2404.03683)
- The finetuned models solve 36% of problems that no symbolic strategy solved. About 4% of problems are unsolvable by every symbolic strategy. The authors call the evidence for "new strategies" only "some evidence". [F] — [SoS HTML](https://arxiv.org/html/2404.03683)
- The paper does not compare the models' explored-state count directly against the symbolic training traces. Per-strategy solve rates for the symbolic solvers appear only in Fig. 3a, and SoS is described as "slightly lower" than their average. [F] — [SoS HTML](https://arxiv.org/html/2404.03683)

**Countdown as an RL-from-verifier task: TinyZero (Pan et al., Jan 2025; blog/repo-only)**
- A reproduction of DeepSeek-R1-Zero on Countdown and multiplication, built on veRL. The README claims a 3B base model "develops self-verification and search abilities" through RL, at a cost under $30. There is no paper; the numbers are only in a W&B log. The repo is marked deprecated in favour of veRL. [F, via search snippet of the repo] — [TinyZero GitHub mirror](https://github.com/vra/TinyZero); secondary write-up (Substack, treat as blog): [xyzlabs](https://xyzlabs.substack.com/p/berkeley-researchers-replicate-deepseek)
- The same secondary source says 0.5B models only guess, while models from 1.5B up show problem-solving. [F, secondary only] — [xyzlabs Substack](https://xyzlabs.substack.com/p/berkeley-researchers-replicate-deepseek)
- Follow-up paper [K]: Gandhi et al. 2025, "Cognitive behaviors that enable self-improving reasoners", ran RL on Countdown with Qwen-2.5-3B versus Llama-3.2-3B. Qwen improves far more under RL, which the authors attribute to verification and backtracking behaviours already present in the base model. Not fetched; verify the numbers — [arXiv 2503.01307](https://arxiv.org/abs/2503.01307)

**Game of 24 in Tree of Thoughts (Yao et al., NeurIPS 2023)**
- Test set: the 100 hard games at indices 901–1000 of the 1,362 puzzles on 4nums.com. Success means a valid equation equal to 24 that uses each input exactly once. [F, via search summary] — [Emergent Mind topic page (aggregator)](https://www.emergentmind.com/topics/game-of-24)
- ToT reaches 74% success. A later comparison puts its cost at $75.02, against Fleet of Agents at 76% for $62.93. [F, via search summary] — [Fleet of Agents, arXiv 2405.06691](https://arxiv.org/pdf/2405.06691)
- GPT-4 baselines [K]: IO prompting 7.3%, CoT 4.0%, CoT-SC (k=100) 9.0%, ToT with b=1 45%, ToT with b=5 74%. Each "thought" is one intermediate equation over 3 steps, and the LLM scores states as sure/likely/impossible. — [arXiv 2305.10601](https://arxiv.org/abs/2305.10601)
- RLSF (2024) fine-tunes an LLM with PPO on Game of 24, using a computer algebra system as the symbolic verifier for reward. [F, via search summary] — [arXiv 2405.16661](https://arxiv.org/pdf/2405.16661)
- The search found no AlphaZero/MCTS-with-learned-policy work on Game of 24 or Countdown, and no paper benchmarking plain MCTS against random search on Countdown. [F, negative search result]

**Symbolic regression as partial-expression-tree search (closest state representation)**
- **Deep Symbolic Regression** (Petersen et al., ICLR 2021) [K]:
  - An RNN emits the expression token by token in pre-order. This is exactly "fill the next hole", with parent/sibling context fed as input and constraints applied in-loop.
  - Training uses a risk-seeking policy gradient. Reward is fit to data, 1/(1+NRMSE), not an exact hit.
  - Evaluated on the Nguyen benchmarks. — [arXiv 1912.04871](https://arxiv.org/abs/1912.04871)
- **SPL** (Sun et al., ICLR 2023) [F, abstract only]: an MCTS agent traverses expression trees under a parsimony-enforcing reward from measurement data, and claims superiority over state-of-the-art baselines. The abstract has no numbers. [K]: the actions are context-free-grammar production rules applied to the leftmost nonterminal, which is the same "leftmost hole" discipline, and no learned prior is used (plain UCT). — [arXiv 2205.13134](https://arxiv.org/abs/2205.13134)
- **DGSR-MCTS** (Kamienny et al., ICML 2023, PMLR v202):
  - State: an expression tree.
  - Action: a mutation ⟨A, op, B⟩. Mutations only grow the tree, up to fewer than 60 operators.
  - Reward: 1 when R² ≥ 0.99. Other nodes take a learned critic value, because raw accuracy "usually induces deceptive rewards".
  - A transformer mutation policy, pretrained on synthetic expressions, is the PUCT prior. Policy and critic keep training during search. [F] — [arXiv 2302.11223v2 HTML](https://arxiv.org/html/2302.11223v2); [PMLR PDF](https://proceedings.mlr.press/v202/kamienny23a/kamienny23a.pdf)
  - SRBench black-box: median test R² 0.846 at expression size 41, versus E2E at 0.797 and size 61.
  - Feynman: about 80% of datasets reach R² ≥ 0.99 at size 33, versus E2E at about 87% but size 121.
  - The conclusion calls it "on par with" GP-GOMEA while using fewer evaluations. [F] — [arXiv 2302.11223v2](https://arxiv.org/html/2302.11223v2)
  - Synthetic solved fraction (in-domain / OOD): mutation size 10 gives 74.8% / 44.0%; no search gives 72.4% / 16.8%. Without search, the learned prior collapses OOD. [F] — [arXiv 2302.11223v2](https://arxiv.org/html/2302.11223v2)
- Other MCTS variants:
  - RSRM: double Q-learning plus MCTS plus a sub-tree discovery block for symmetric and long equations. [F, snippet] — [arXiv 2305.14656](https://arxiv.org/pdf/2305.14656)
  - Improved MCTS for SR (Huang et al., 2025): extreme-bandit allocation and state-jumping actions, run with a budget of 2M expression evaluations on Nguyen, Jin, Livermore and SRBench. [F, snippet] — [arXiv 2509.15929](https://arxiv.org/pdf/2509.15929)
- "AlphaSymbol": I did not locate a primary source in this session. See Gaps.

**Expression rewriting and term-rewriting games**
- **Lample & Charton** (ICLR 2020) [K]: seq2seq over prefix-notation expression trees for integration and ODEs. Training data is generated synthetically by several generators (FWD, BWD, IBP), and the paper reports beating Mathematica/Maple on its test sets. — [arXiv 1912.01412](https://arxiv.org/abs/1912.01412)
- **Chen & Tian**, "Learning to Perform Local Rewriting for Combinatorial Optimization" (NeurIPS 2019) [K]: a learned region-picker plus rule-picker for rewriting, applied to Halide expression simplification and beating the hand-written Halide rewriter. — [arXiv 1810.00337](https://arxiv.org/abs/1810.00337)
- **AlphaDev** (Mankowitz et al., Nature 2023) [K] is the closest "program-construction single-player game": AssemblyGame, an AlphaZero agent appends instructions, and the reward combines correctness and latency. — [Nature](https://www.nature.com/articles/s41586-023-06004-9)

### Inferences
- SoS's "states explored" is the closest published analogue to a "nodes expanded / search steps saved" metric on a numeric-target arithmetic game. It shows a learned policy improved by self-training cuts explored states by about 12–14% and raises solve rate by about 5 points. That is a benchmark-shaped precedent for the proposed metric.
- The proposed game is a distinct and publishable recombination: Countdown's exact-hit reward on the representation of DSR/SPL. One novelty to state explicitly: leftmost-hole filling with free small constants (rather than "use these given numbers") makes the space unbounded and the reward very sparse. That pushes it towards SPL-style MCTS, not Countdown-style enumeration.

### Gaps
- No primary source found for "AlphaSymbol". It may refer to a 2023–24 arXiv preprint coupling a GPT-style prior with MCTS for symbolic regression. Unverified.
- TinyZero accuracy numbers exist only in a W&B log, not retrieved.
- SoS per-strategy symbolic solve rates (Fig. 3a) were not readable as text.
- No published "random vs plain MCTS vs learned-prior MCTS" table on Countdown or Game of 24 was found.

## Q1b. Does any work use a natural distribution of expressions (from real code corpora) rather than random targets, and does it find a difference?

### Takeaway
I found no expression-construction or Countdown-style work that draws targets or chunks from a real code corpus. Indirect evidence says the training distribution matters a great deal:
- In symbolic regression, a prior pretrained on random synthetic formulas degrades sharply OOD without search.
- Program-synthesis systems that learn from a corpus of solved tasks (DreamCoder, Stitch) search much faster than unguided enumeration.

### Cited Findings
- DGSR-MCTS pretrains on random synthetic expressions with 5–25 internal nodes, simplified with SymPy, then evaluates on real-world SRBench black-box and Feynman equations, which are outside the generator's distribution. Without search, the solved fraction falls from 72.4% in-domain to 16.8% OOD. With MCTS search it is 74.8% / 44.0%. [F] — [arXiv 2302.11223v2](https://arxiv.org/html/2302.11223v2)
- Lample & Charton report that models trained on one data generator (e.g. BWD) transfer poorly to test sets from another (e.g. FWD). The expression distribution, not just the task, determines accuracy. [K, verify table] — [arXiv 1912.01412](https://arxiv.org/abs/1912.01412)
- Zaremba & Sutskever say outright that their train and test distributions are identical. They "do not know how heavily our model relies on memorization", and they defer testing on "very different new examples". [F] — [Learning to Execute, arXiv 1410.4615](https://arxiv.org/abs/1410.4615)

### Inferences
- A natural-corpus versus random-target comparison on an arithmetic s-expression game would be a genuine empirical contribution. The closest existing design is library learning over real task corpora (DreamCoder/Stitch, see Q4), but those corpora are curated tasks, not mined code.

### Gaps
- No study found that mines arithmetic sub-expressions from real code, such as constant-folding sites or index arithmetic, and uses them as a search prior or a target distribution.

## Q2. What is the best current evidence about whether a fixed-size network can evaluate arbitrarily deep expressions, and at what depth does accuracy fall?

### Takeaway
Theory and experiment agree: a fixed-depth network evaluating an expression in one forward pass cannot handle arbitrary nesting depth.
- **Theory:** log-precision fixed-depth transformers sit in TC0, while arithmetic or Boolean formula evaluation is NC1-complete. So unless TC0 = NC1, depth-unbounded evaluation needs either super-polynomial size or step-by-step computation such as chain-of-thought, an external stack, or an executor.
- **Experiment:** accuracy degrades steadily with nesting and length (LSTMs in 2014; GPT-3/4 in 2023). Fine-tuned models are near-perfect in-distribution and fall off sharply OOD in depth and width.
- **Exception:** architectures with structured or extensible memory (Neural GPU, stack/tape-augmented RNNs, NPI with traces) can length-generalise on specific algorithms.

### Cited Findings
**Learning to Execute** (Zaremba & Sutskever, arXiv 2014; ICLR 2015 submission per the PDF header) [F, full text]
- Program class: Python-like programs that can be evaluated in a single left-to-right pass with constant memory. They have two parameters:
  - **length:** the number of digits in the integers, drawn uniformly from [1, 10^length].
  - **nesting:** "the number of times we are allowed to combine the operations with each other", giving deeper parse trees.
  - Multiplication operands and for-loop ranges are capped to [1, 4·length], because "generic integer multiplication requires superlinear time." — [arXiv 1410.4615](https://arxiv.org/abs/1410.4615) (PDF text)
- Authors: "Nesting makes the task much harder for the LSTMs, because they do not have a natural way of dealing with compositionality… It is surprising that the LSTMs can handle nested expressions at all." Figure 4: "Deeper nesting and longer integers make the task more difficult." — [arXiv 1410.4615](https://arxiv.org/abs/1410.4615)
- Curricula compared:
  - baseline (no curriculum);
  - naive (length up, then nesting), which "sometime perform[s] worse than baseline";
  - mix (random length and nesting per sample);
  - combined (naive + mix), which "outperforms all other strategies in every configuration on program evaluation".
  - Combined gave 99% accuracy on 9-digit addition, "a massive improvement over the naive curriculum." — [arXiv 1410.4615](https://arxiv.org/abs/1410.4615)
- Important caveat: all accuracies use teacher forcing. The correct previous target digits are supplied, and the authors note that free generation "would almost surely result in lower numerical accuracies." Random-guess accuracy is about 8.3% (12 output symbols). The per-cell numbers in the length × nesting grid are in an image (Fig. 4) and were not extractable as text. — [arXiv 1410.4615](https://arxiv.org/abs/1410.4615)
- Discussion: they cannot evaluate programs running in more than O(n) time, which "cannot be achieved with conventional RNNs or LSTMs due to their runtime restrictions." — [arXiv 1410.4615](https://arxiv.org/abs/1410.4615)

**Faith and Fate** (Dziri et al., arXiv May 2023, v3 Oct 2023; NeurIPS 2023 per [K]) [F]
- Tasks: multi-digit multiplication, logic grid puzzles and a dynamic-programming problem, each cast as computation graphs. — [arXiv 2305.18654 HTML](https://arxiv.org/html/2305.18654)
- Off-the-shelf ChatGPT / GPT-4 reach only 55% / 59% on 3×3-digit multiplication. Zero-shot accuracy falls to near zero as digit count grows. — [arXiv 2305.18654](https://arxiv.org/html/2305.18654)
- Fine-tuned GPT-3, trained on about 1.8M multiplication pairs:
  - Near-perfect in-distribution, with sharp OOD drops for deeper or wider graphs.
  - Extended training (420K steps) did not fix OOD generalisation, at reported costs of $50K and $40K.
  - 82.3% of correct answers on unseen 4×2-digit problems still had at least one error in the computation graph. — [arXiv 2305.18654](https://arxiv.org/html/2305.18654)
- Mechanism: correct predictions coincide with frequent matching training subgraphs ("linearized subgraph matching"), not systematic reasoning. — [arXiv 2305.18654](https://arxiv.org/abs/2305.18654)
- Theory, Prop. 4.2 (iterated/depth): P(wrong) ≥ 1 − (1−ε−c)^(n−1)·(1−ε−c/(c+ε)). The probability of a correct answer decays exponentially with composition depth n. Prop. 4.1 gives the analogous result for width. — [arXiv 2305.18654](https://arxiv.org/html/2305.18654)

**Circuit-complexity results**
- **Feng et al.**, "Towards Revealing the Mystery behind Chain of Thought" (NeurIPS 2023, oral) [F, abstract]:
  - Bounded-depth transformers cannot directly output correct answers to basic arithmetic/equation tasks unless model size grows super-polynomially with input length.
  - Constant-size autoregressive transformers can solve them by generating chain-of-thought (CoT) derivations.
  - Empirically, "Transformers always fail to directly predict the answers" but learn CoT solutions. [K]: the task is Arithmetic(n,p), evaluating expressions mod p with parentheses, and the lower bound assumes TC0 ≠ NC1. — [arXiv 2305.15408](https://arxiv.org/abs/2305.15408)
- **Merrill & Sabharwal**, "The Parallelism Tradeoff" (TACL 2023) [K]: log-precision transformers are in uniform TC0, so they cannot solve NC1-hard or P-complete problems unless the classes collapse. — [arXiv 2207.00729](https://arxiv.org/abs/2207.00729)
- **Merrill & Sabharwal**, "The Expressive Power of Transformers with Chain of Thought" (ICLR 2024) [K]: a linear number of CoT steps extends power to regular languages, and a polynomial number of steps reaches P. Intermediate computation, not width, buys depth. — [arXiv 2310.07923](https://arxiv.org/abs/2310.07923)
- **Liu et al.**, "Transformers Learn Shortcuts to Automata" (ICLR 2023) [K]: O(log T)-layer transformers can simulate any automaton over T steps, and O(1) layers suffice for solvable automata (Krohn–Rhodes). The learned shortcuts are brittle OOD. — [arXiv 2210.10749](https://arxiv.org/abs/2210.10749)

**Architectures and length generalisation**
- **Delétang et al.**, "Neural Networks and the Chomsky Hierarchy" (arXiv 2022, v3 Feb 2023; ICLR 2023 per [K]) [F]:
  - 20,910 models across 15 tasks.
  - RNNs and Transformers fail on non-regular tasks OOD; LSTMs handle regular and counter tasks.
  - Only networks with structured memory (stack/tape) generalise on context-free and context-sensitive tasks.
  - Arithmetic-expression evaluation with brackets is context-free, so this lands squarely on it. — [arXiv 2207.02098](https://arxiv.org/abs/2207.02098)
- **Anil et al.** (NeurIPS 2022) [K]: naive fine-tuning of LLMs shows length-generalisation failures regardless of scale; scratchpad plus in-context learning helps. — [arXiv 2207.04901](https://arxiv.org/abs/2207.04901)
- **Zhou et al.**, "What Algorithms Can Transformers Learn?" (ICLR 2024) [K]: the RASP-L conjecture says transformers length-generalise when the task has a short RASP-L program. Plain addition and parity are hard without format changes. — [arXiv 2310.16028](https://arxiv.org/abs/2310.16028)
- **Neural GPU** (Kaiser & Sutskever, ICLR 2016) [K]: trained on 20-bit binary addition and multiplication, it generalises to 2000-bit inputs. Its size grows with the input (a convolutional grid), so it is not fixed-size in the relevant sense. — [arXiv 1511.08228](https://arxiv.org/abs/1511.08228)
- **NTM** (Graves et al., arXiv 2014) and **DNC** (Graves et al., Nature 2016) [K]: external differentiable memory. Copy, sort and graph tasks generalise somewhat beyond training length. — [arXiv 1410.5401](https://arxiv.org/abs/1410.5401); [Nature 538:471](https://www.nature.com/articles/nature20101)
- **NPI** (Reed & de Freitas, ICLR 2016) [K]: learns to compose sub-programs from full execution traces (strong supervision), with a call stack. Multi-digit addition generalises to much longer inputs. — [arXiv 1511.06279](https://arxiv.org/abs/1511.06279)
- **Neural algorithmic reasoning** (Veličković & Blundell, Patterns 2021) [K] and **CLRS** (Veličković et al., ICML 2022): 30 classical algorithms with hint traces, trained on small graphs (n=16) and tested OOD at n=64. OOD gaps remain substantial for many algorithms. — [arXiv 2105.02761](https://arxiv.org/abs/2105.02761); [arXiv 2205.15659](https://arxiv.org/abs/2205.15659)
- **Hupkes, Veldhoen & Zuidema** (JAIR 2018) [K]: GRUs trained on a nested-bracket arithmetic language partly generalise to longer and deeper expressions using a "cumulative" strategy. **ListOps** (Nangia & Bowman, 2018) [K] is a standard nested-expression benchmark. — [arXiv 1711.10203](https://arxiv.org/abs/1711.10203); [arXiv 1804.06028](https://arxiv.org/abs/1804.06028)

### Inferences
- **"Arbitrarily deep":** the best evidence is negative for any fixed-depth, single-pass network: TC0 vs NC1 (Feng 2023, Merrill & Sabharwal 2023) plus exponential error compounding (Dziri 2023, Prop. 4.2). Positive results need either unbounded intermediate computation (CoT, scratchpad, NPI traces) or memory and compute that grow with the input (Neural GPU, stack/tape RNNs).
- **"At what depth does accuracy fall?"** There is no universal number; it depends on how far depth or length goes past the training distribution. The robust pattern: near-perfect inside the training range, sharp decline just beyond it (Faith and Fate Fig. 5, CLRS n=16→64, Delétang). For GPT-4 with no CoT, multiplication is already at 59% for 3×3 digits.
- **This supports "learn to recognise, not execute"**: a learned component should propose or recognise chunks and promising holes, and evaluation should go to an exact evaluator, which is cheap and exact for arithmetic.

### Gaps
- Exact per-cell Learning to Execute accuracies by nesting/length (Fig. 4) are only in an image.
- No source found giving a clean "accuracy vs nesting depth" curve for transformers on pure s-expression evaluation without CoT. Feng et al.'s experiments are the closest, and the numbers were not retrieved.

## Q3. Which works argue for a hybrid of a learned recogniser plus an exact evaluator/executor? And has anyone shown that a learned prior over expression chunks reduces search on expression-construction tasks?

### Takeaway
Yes, in program synthesis, though not on an arithmetic numeric-target game specifically.
- Learned priors guiding exact enumerative/deductive search give roughly 10× speedups: DeepCoder ("order of magnitude"), NGDS (up to about 12× [K]).
- Learned libraries of reusable sub-expression "chunks" (DreamCoder, Stitch) let search solve more tasks within a fixed budget.
- In LLMs, the hybrid "model writes the expression, interpreter evaluates it" (PAL, Toolformer's calculator) beats in-network arithmetic.
- In Countdown, a learned search policy cuts explored states by about 12–14% relative to its own base model (SoS).

### Cited Findings
- **DeepCoder** (Balog et al., ICLR 2017 submission; accepted [K]): a network predicts which DSL functions appear in the program, from I/O examples. Those predictions steer exact enumerative and SMT search, giving an "order of magnitude speedup" over strong non-augmented baselines and over an RNN approach. [F] — [arXiv 1611.01989](https://arxiv.org/abs/1611.01989)
- **SoS Countdown** (above): STaR/APA policy improvement lowers average explored states from 75.04 to 64.73 on seen targets, and raises solve rate from 51.27% to 56.52%. [F] — [arXiv 2404.03683](https://arxiv.org/html/2404.03683)
- **DGSR-MCTS** (above): a pretrained neural mutation prior inside MCTS reaches high R² in fewer evaluations than GP-GOMEA. There is no direct with/without-prior ablation of search budget. Search without the prior's guidance and the prior without search are both weaker. [F] — [arXiv 2302.11223v2](https://arxiv.org/html/2302.11223v2)
- **NGDS**, Neural-Guided Deductive Search (Kalyan et al., ICLR 2018) [K]: neural scoring of branches inside the exact PROSE deductive synthesiser, with speedups of up to about 12× at matching accuracy. — [arXiv 1804.01186](https://arxiv.org/abs/1804.01186)
- **DreamCoder** (Ellis et al., PLDI 2021) [K]: wake-sleep library learning. Recurring sub-programs are compressed into new primitives ("chunks"), and a neural recognition model is trained on replays and dreams. Together they let enumerative search solve more tasks per budget across domains, including list processing and symbolic regression. — [arXiv 2006.08381](https://arxiv.org/abs/2006.08381)
- **Stitch** (Bowers et al., POPL 2023) [K]: corpus-guided top-down abstraction learning, orders of magnitude faster than DreamCoder's compressor. It is the scalable "learned memoisation of subtrees" component. — [arXiv 2211.16605](https://arxiv.org/abs/2211.16605)
- **PAL** (Gao et al., ICML 2023) [K]: the LLM writes Python and the interpreter executes it. It beats CoT, which computes inside the model; the abstract reports PaLM-540B CoT beaten by 15% absolute top-1 on GSM8K. — [arXiv 2211.10435](https://arxiv.org/abs/2211.10435)
- **Toolformer** (Schick et al., NeurIPS 2023) [K]: self-supervised learning of when to call tools, including a calculator, offloading exact arithmetic. — [arXiv 2302.04761](https://arxiv.org/abs/2302.04761)
- **Feng et al.** (NeurIPS 2023) [F]: a constructive proof that constant-size transformers can do arithmetic via externalised step-by-step derivations. This is the theoretical justification for moving execution out of the forward pass. — [arXiv 2305.15408](https://arxiv.org/abs/2305.15408)
- **Kamienny et al.** [F]: they reward with a learned critic instead of raw fit because raw accuracy "usually induces deceptive rewards". This argues for a learned recogniser of promising partial states on top of exact evaluation of complete ones. — [arXiv 2302.11223v2](https://arxiv.org/html/2302.11223v2)

### Inferences
- The literature supports the split "learned recogniser/prior for proposing and ranking chunks and holes + exact evaluator for scoring complete trees". Evidence comes from:
  - theory (TC0/NC1; error compounding);
  - LLM tool use (PAL, Toolformer);
  - program synthesis (DeepCoder, NGDS, DreamCoder).
- The specific claim "a learned prior over expression chunks reduces nodes expanded on an arithmetic numeric-target construction game, versus plain MCTS and random search" is not directly measured anywhere I found. SoS measures states explored but compares learned against learned, not against plain MCTS. DreamCoder measures tasks solved per budget, not nodes saved on arithmetic targets. That is the open empirical slot.

### Gaps
- Exact DreamCoder and NGDS speedup numbers were not re-verified this session.
- No work found comparing learned-chunk priors from natural code corpora against priors from random expressions on any expression-construction benchmark.
- "Learned memoisation / chunk caching of subtrees" as a named technique, separate from library learning, was not located.
