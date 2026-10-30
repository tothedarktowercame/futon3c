# T4: How active inference avoids enumerating policies when minimising G, and its relation to AlphaZero-style amortised search

Method note: full texts were read directly (pdftotext) for Sophisticated Inference (arXiv 2006.04120), Fountas et al. 2020 (arXiv 2006.04176), Grill et al. 2020 (arXiv 2007.12509), Millidge 2020 (arXiv 1907.03876), Da Costa et al. 2020 (arXiv 2001.07203) and Tschantz et al. 2020 (arXiv 2002.12636). The reference implementations SPM `spm_MDP_VB_X.m` and pymdp `control.py` were also read. Friston et al. 2016 was read via PMC, but its equations are images and were not extracted. Items marked "[not re-fetched]" rest on prior knowledge and should be checked before they are quoted.

## Claim 1: Hierarchy (deep temporal models; Pezzulo, Rigoli & Friston 2018)

### Takeaway
Partly confirmed. "Deep temporal models and active inference" exists with the stated authors, but the original article is 2017 (Neurosci. Biobehav. Rev. 77:388–402), not 2018. 2018 (vol. 90:486–501) is a corrigendum/re-issue. In these hierarchical models the higher level supplies an empirical prior E over a small number of lower-level policies. The Pezzulo, Rigoli & Friston paper was not re-fetched.

### Cited Findings
- Friston, Rosch, Parr, Price, Bowman, "Deep temporal models and active inference", *Neuroscience & Biobehavioral Reviews* **77** (June 2017) 388–402, DOI 10.1016/j.neubiorev.2017.04.009, PMID 28416414. It simulates reading with hierarchical, deep temporal generative models. — [PMC5461873](https://pmc.ncbi.nlm.nih.gov/articles/PMC5461873)
- A 2018 item is listed as vol. 90, pp. 486–501, DOI 10.1016/j.neubiorev.2018.04.004 (corrigendum), with a separate PMC record. — [UCL Discovery](https://discovery-pp.ucl.ac.uk/id/eprint/10057887); [PMC5998386](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5998386/). Da Costa et al. 2020 cite it as "Neuroscience & Biobehavioral Reviews, 90:486–501, July 2018". — [arXiv 2001.07203](https://arxiv.org/abs/2001.07203)
- The Sophisticated Inference paper states the hierarchical role of E directly: "E(π) represents an empirical prior that is usually conditioned upon hidden states at a higher level in deep (i.e., hierarchical) generative models". — [Friston et al., arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- The same paper notes that classic policy-based models "can be regarded as placing priors over actions that stipulate a small number of allowable action sequences". This is the "few short policies per level" mechanism. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)

### Inferences
- Hierarchy avoids enumeration in two ways. Each level considers only a handful of short, hand-specified policies, and the level above conditions E (and initial states D) for the level below. The combinatorics are factored across timescales rather than searched.

### Gaps
- Pezzulo, Rigoli & Friston, "Hierarchical active inference: a theory of motivated control", *Trends in Cognitive Sciences* 22(4):294–306, 2018 [not re-fetched]. This citation is from memory and its exact content was not verified this session.

## Claim 2: Pruning in Sophisticated Inference (threshold, horizon, tasks)

### Takeaway
Confirmed, with precise details. Sophisticated inference is a recursive tree search over beliefs and actions. Branches are dropped when the predictive probability of an outcome, or the prior probability of an action, falls below 1/16. The toy tasks were a three-arm T-maze (horizon 2 vs 1) and an 8×8 grid maze (horizons 1, 3, 4, and 6 discussed). Most paths are never evaluated.

### Cited Findings
- Authors: Friston, Da Costa, Hafner, Hesp, Parr. arXiv 7 June 2020. Published in *Neural Computation* 33(3):713–763, 2021 [venue/pages not re-fetched]. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- The recursion, eq. (1.11), as written (σ = softmax, u_τ = action at τ):
  `G(o_τ,u_τ) = E_{P(o_{τ+1}|s_{τ+1})Q(s_{τ+1}|u_{<τ+1})}[ln Q(s_{τ+1}|u_{<τ+1}) − ln P(s_{τ+1}) − ln P(o_{τ+1}|s_{τ+1})]` (risk + ambiguity, "expected free energy of next action")
  `+ E_{Q(u_{τ+1}|o_{τ+1})Q(o_{τ+1}|u_{≤τ})}[G(o_{τ+1},u_{τ+1})]` ("expected free energy of subsequent actions")
  `Q(u_τ|o_τ) = σ[−G(o_τ,u_τ)]`.
  The paper calls this formally similar to the Bellman equation, but "sophisticated inference … deals with functionals (functions of belief distributions over states)". — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- Pruning, as stated: "we can restrict the search over future outcomes … that have a non-trivial posterior probability (e.g., greater than 1/16). Similarly, we only need to evaluate those policies that are likely to be pursued; namely, those with an expected free energy that renders their prior probability nontrivial (e.g., greater than 1/16)". In the simulations: "searches over paths were terminated when the predictive probability fell below 1/16". The search is "terminating the search down any given branch when the prior probability of an action (and the predictive posterior probability of its subsequent outcome) reach a suitably small threshold", citing Keramati et al. 2016 and Solway & Botvinick 2015. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- The search evaluates distributions over outcomes conditioned on plausible hidden states, not sampled outcomes: "it is sufficient to evaluate distributions over outcomes, conditioned upon plausible hidden states". It also states: "There is no need for sampling or random searches". This contrasts with MCTS. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- Task 1, T-maze: a rat in a three-arm T-maze, 32 trials of two moves each. A sophisticated agent with planning horizon 2 is compared against horizon 1 and against unsophisticated agents. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- Task 2, navigation: an 8×8 grid maze with 5 actions and 64 hidden states. Deterministic solutions are shown for horizons 1, 3 and 4; horizon ≥4 avoids local minima. With 8 moves per trial, the novelty/exploration run used 64 moves. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- Efficiency claim: the maze has "1.0486·10^10 [potential paths] for a planning horizon of four and 1.0737·10^15 for a planning horizon of six … only a tiny fraction of these paths is actually evaluated – usually several hundred, which takes a few hundred milliseconds on a personal computer". The authors concede the scheme is shown "on rather trivial problems". — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- The paper mentions amortisation as the route to scaling ("amortized perception and action by learning a parametrised approximation to expected free energy") and notes that "amortization precludes online" inference. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)

### Inferences
- Pruning relies on precise beliefs: "Given reasonably precise beliefs about current states and state transitions, only a small number of paths are eligible". The savings disappear when beliefs are diffuse, as in an argument game against an unknown opponent.
- Unlike AlphaZero there is no learned prior steering the tree. The action prior inside the recursion is σ(−G) computed from the children, and the 1/16 cutoff is a fixed heuristic.

### Gaps
- No ablation over the threshold value was found; 1/16 is given only as an example and setting.

## Claim 3: The habit prior E (Friston et al. 2016) and the policy posterior σ(ln E − F − γG)

### Takeaway
Confirmed, with a correction. The policy posterior σ(ln E − F − γG) is correct in sign convention (F and G as free energies), and SPM implements exactly that. Habits are learned by Dirichlet accumulation, but the SPM update adds the **posterior expectation over policies** (e ← e + η·π at the end of the trial), not a count of the sampled or chosen policy. The paper's prose says "counting the number of times they occur", so "counts" is a loose paraphrase. Habits are described as "learned by observing one's own goal directed behaviour".

### Cited Findings
- Friston, FitzGerald, Rigoli, Schwartenbeck, O'Doherty, Pezzulo, "Active inference and learning", *Neurosci. Biobehav. Rev.* 68:862–879 (2016), DOI 10.1016/j.neubiorev.2016.06.022. The prior over policies is given as π₀ = σ(ln E − γ·G): "Here E corresponds to prior beliefs about policies, while G is the free energy expected under each policy". — [PMC5167251](https://pmc.ncbi.nlm.nih.gov/articles/PMC5167251/)
- Posterior in words, from the same paper: "The policy updates are just a softmax function of their log probability, which has three components: a prior based on previous experience, the (posterior) free energy based on past outcomes and the expected (prior) free energy based on preferences about future outcomes." — [PMC5167251](https://pmc.ncbi.nlm.nih.gov/articles/PMC5167251/)
- On habit learning, the same paper says: "habits … are learned by observing one's own goal directed behaviour". The concentration parameters reflect "the number of times each state, transition or policy has been encountered in previous trials". Habits "accumulate evidence by simply counting the number of times they occur". "The arbitration among habits and (sequential) policies rests on their posterior probability". In a devaluation simulation, "the habit persists", which yields "resistance of habitual policies to devaluation". — [PMC5167251](https://pmc.ncbi.nlm.nih.gov/articles/PMC5167251/)
- SPM reference implementation (`spm_MDP_VB_X.m`). The posterior is `qu = spm_softmax(qE{m}(p{m}) + w{m}(t)*Q(p{m}) + F(p{m}))` and the prior is `pu = spm_softmax(qE{m}(p{m}) + w{m}(t)*Q(p{m}))`, where `qE = spm_log(spm_norm(e))`, Q is "(negative) expected free energy", F is negative variational free energy and w = γ. In free-energy sign convention this is exactly π = σ(ln E − F − γG). The habit update is `MDP(m).e = MDP(m).e + eta*u{m}(:,T)`, where `u{m}(p{m},t) = qu` is the policy posterior. — [SPM spm_MDP_VB_X.m](https://github.com/spm/spm/blob/main/toolbox/DEM/spm_MDP_VB_X.m)
- Sophisticated Inference writes the same structure as the generalised free energy: `F[Q(s,π)] = E_{Q(π)}[ln Q(π) + E(π) + F(π) + G(π)]`, with `−ln P(π) = E(π) + G(π)`. There E is a negative log prior, so ln P(π) = −E − G. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)
- Da Costa et al. 2020 write the full posterior as "σ(− log E − F − G)". The extra minus on log E comes from their footnote convention and is likely a sign typo or E-as-energy convention; it conflicts with the σ(ln E …) of Friston 2016 and SPM. Their main text uses Q(π) = σ(−G(π)), extended to σ(−γG(π)). — [arXiv 2001.07203](https://arxiv.org/abs/2001.07203)
- pymdp (current JAX `control.py`, `update_posterior_policies`) returns `nn.softmax(gamma * neg_efe_all_policies + log_stable(E))`. This is σ(ln E − γG) with **no F term**; the F term appears only in SPM-style full schemes. — [pymdp control.py](https://github.com/infer-actively/pymdp/blob/master/pymdp/control.py)

### Inferences
- Because e accumulates the posterior π, which already includes −γG, E converges toward the time-averaged goal-directed policy distribution in the contexts visited. Mechanically, this is "distilling G into a prior", the same move AlphaZero makes when it trains π_θ toward MCTS visit counts. In SPM, though, E is a context-free vector over a fixed policy set, not a state-conditioned function, so it does not generalise across states the way a policy network does.
- E does not prune anything in the tabular schemes. Every policy is still scored by G, and E only reweights. Real savings appear only in the amortised and deep versions (Claim 4).

### Gaps
- The typeset equation 7 and the e-update equation of the 2016 paper could not be extracted (they are images on PMC). Whether the paper prints σ(ln E − F·π − γ·G) or σ(ln E − F − γ·G) is unverified. SPM confirms the semantics.
- No measure in Friston 2016 of how closely E tracks the G-driven choice over trials was found.

## Claim 4: Amortisation (Fountas 2020, Millidge 2020, Tschantz, Çatal, Mazzaglia, surveys)

### Takeaway
Mostly confirmed. One correction on Fountas: the habit network does **not** prune or replace MCTS. It is the PUCT-style prior in the UCB term and the sampler at unexpanded nodes; its KL to the planner is the training loss and sets a state-precision ω. The MCTS stopping rule is a separate confidence threshold T_dec. Millidge's G-network is explicitly a bootstrapped, TD-style "EFE-value" analogous to a Q-function.

### Cited Findings
- **Fountas, Sajid, Mediano, Friston, "Deep active inference agents using Monte-Carlo methods", NeurIPS 2020.** — [arXiv 2006.04176](https://arxiv.org/abs/2006.04176); [NeurIPS paper](https://papers.neurips.cc/paper_files/paper/2020/file/865dfbde8a344b44095495f3591f7407-Paper.pdf)
  - Motivation: "computing G across all policies is costly since it involves making an exponentially-increasing number of predictions for T-steps into the future … To solve this problem, we employ two methods operating in tandem": MCTS and "amortized inference through a habitual neural network that directly approximates the distribution over actions … Q_φa(a_t) acts as a variational posterior that approximates P(a_t|s_t), with a prior P(a_t), calculated by MCTS".
  - Selection rule, eq. (9): `U(s_t,a_t) = G̃(s_t,a_t) + c_explore · Q_φa(a_t|s_t) · 1/(1+N(a_t,s_t))`. Explored nodes sample from σ(U), new nodes sample from Q_φa. Final action prior: `P(a_t) = N(a_t,s_t)/Σ_j N(a_j,t,s_t)`. Planning stops when `max P(a_t) − 1/|A| > T_dec` or at the maximum number of loops. Settings: c_explore = 1, T_dec = 0.8, rollout depth 3, maximum 300 loops (dSprites) and 100 (Animal-AI).
  - EFE decomposition, eq. (7), as written: `G(π,τ) = −E_Q̃[log P(o_τ|π)] + E_Q̃[log Q(s_τ|π) − log P(s_τ|o_τ,π)] + E_Q̃[log Q(θ|s_τ,π) − log P(θ|s_τ,o_τ,π)]`. These terms are reward-like, state information gain, and parameter information gain (novelty). The computable eq. (8) uses entropies, with MC dropout for θ.
  - Habit training: the main text says "this network is trained to reproduce the last executed action a_{t−1} (selected by sampling P(a_t)) using the last state s_{t−1}". Algorithm 1 instead says "Compute D_t = D_KL[Q_φa(a_t) ‖ P(a_t)]. Apply a gradient step on φa using D_t as loss". The KL also sets precision, `ω_t = α/(1+e^{−(b−D_{t−1})/c}) + d`: "if D_KL … is high, then a habit has not yet been formed … As training progresses and the habitual network becomes a better approximation of P(a_t), ω_t is gradually increased".
  - Environments: dynamic dSprites (object sorting, |A| = 4) and Animal-AI "preferences" (|A| = 3). Runtime: about 10 h per agent with the 1-step and habitual strategies versus about 4 days with full MCTS.
  - A NeurIPS reviewer objected to the "habitual" label because the network does not itself select actions but initialises aspects of the plan. — [NeurIPS 2020 review](https://papers.nips.cc/paper/2020/file/865dfbde8a344b44095495f3591f7407-Review.html)
- **Millidge, "Deep active inference as variational policy gradients"** (arXiv 2019; *J. Math. Psych.* 2020 [venue not re-fetched]). Recursive EFE, eq. (20): `G(s,a) = −r(o) + Q(s)[log Q(s) − log Q(s|o)] + E_{Q(s_{t+1},a_{t+1})}[G(s_{t+1},a_{t+1})]`. Bootstrap target, eq. (21): `Ĝ(s,a) = −r(o) + Q(s)[log Q(s) − log Q(s|o)] + G_φ(s,a)`. Loss, eq. (22): `L = ‖G_φ(s,a) − Ĝ(s,a)‖²`. "This procedure is analogous to bootstrapped value or Q function estimation procedures in reinforcement learning". A separate policy network Q(a|s) is fitted variationally to p(a|s) = σ(−G_φ). Tested on CartPole, Acrobot and LunarLander against Q-learning and actor-critic baselines, with a replay buffer. — [arXiv 1907.03876](https://arxiv.org/abs/1907.03876)
- **Tschantz, Millidge, Seth, Buckley, "Reinforcement learning through active inference"**, ICLR 2020 workshop "Bridging AI and Cognitive Science". It uses ensemble dynamics models and CEM to optimise q(π) under EFE (model-predictive, no habit network): "amortising inference with neural networks, deep ensembles and sophisticated algorithms for planning (CEM)". — [arXiv 2002.12636](https://arxiv.org/abs/2002.12636)
- **Çatal, Nauta, Verbelen, Simoens, Dhoedt, "Bayesian policy selection using active inference"** (arXiv 2019, ICLR 2019 workshop [venue not re-fetched]). "The cumbersome sampling according to expected free energy can be avoided by using amortized inference … we also instantiate a 'habit' policy P(a_t|s_t) as a neural network that maps states to actions", trained by minimising G at each timestep. It generalises across Mountain Car start positions. — [arXiv 1904.08149](https://arxiv.org/html/1904.08149)
- **Mazzaglia, Verbelen, Çatal, Dhoedt, "The Free Energy Principle for Perception and Action: A Deep Learning Perspective"**, *Entropy* 2022 (review): "amortized inference is applied for the choice of variational parameters, in the model learning process, and for the formation of habits, in the action selection process". — [PMC8871280](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8871280/)
- **Lanillos et al., "Active Inference in Robotics and Artificial Agents: Survey and Challenges"** (arXiv Dec 2021). It frames planning as optimising the plan density q(π) with respect to EFE. — [arXiv 2112.01871](https://arxiv.org/abs/2112.01871)
- **Da Costa, Parr, Sajid, Veselic, Neacsu, Friston, "Active inference on discrete state-spaces: a synthesis"** (arXiv 2020; *J. Math. Psych.* 2020 [venue not re-fetched]). Decision-making is "Q(π) = σ(−G(π))", extended to σ(−γG(π)). Habit learning and arbitration appear in its table of neurobiological applications. — [arXiv 2001.07203](https://arxiv.org/abs/2001.07203)
- Fountas et al. summarise the state of the field: "many of which use amortization to scale-up active inference … Millidge introduced an approximation of the agent's expected free energy … based on bootstrap samples, while Tschantz et al. employed a reduced version of EFE". — [arXiv 2006.04176](https://arxiv.org/abs/2006.04176)

### Inferences
- Fountas et al. is structurally the closest active-inference analogue of AlphaZero. A learned prior Q_φa sits in a PUCT-like bonus, the visit-count distribution is the improved policy, and the network is distilled toward it by KL. Unlike AlphaZero, it has no learned value bootstrap at the leaves; leaves are scored by explicit EFE terms over depth-3 rollouts. Millidge supplies that missing bootstrapped "EFE-value". A full "AlphaZero for G" would combine the two, and no paper found does so explicitly.

### Gaps
- Mazzaglia et al., "Contrastive active inference" (NeurIPS 2021, arXiv 2110.10083 [not fetched]) was not verified. The arXiv ID tried (2107.12979) was a different paper.
- Paul, Sajid et al. 2021 ("Active inference for stochastic control", IWAI 2021 [not fetched]) was not verified.
- No 2023–2026 deep-active-inference planning survey was found in searches. Treat the coverage as up to 2022.

## Claim 5: Planning as inference and MCTS/AlphaZero (Grill 2020, Levine 2018, explicit AlphaZero ↔ habit mappings)

### Takeaway
Grill et al. is confirmed. AlphaZero's empirical visit distribution approximately tracks the solution of a KL-regularised policy optimisation toward the prior policy π_θ. Note the reverse-KL direction and λ_N ∝ √N/(|A|+N). No published paper was found that explicitly maps the AlphaZero policy network onto the active-inference habit prior E, or calls AlphaZero "amortised G". That mapping is an inference, not a literature claim.

### Cited Findings
- Grill, Altché, Tang, Hubert, Valko, Antonoglou, Munos, "Monte-Carlo tree search as regularized policy optimization", *Proceedings of the 37th ICML* (2020). — [arXiv 2007.12509](https://arxiv.org/abs/2007.12509)
  - AlphaZero selection, eq. (1)/(5): `a*(x) = argmax_a [Q(x,a) + λ_N · π_θ(a|x)/π̂(a|x)]`, with `π̂(a|x) = (1+n(x,a))/(|A| + Σ_b n(x,b))` and `λ_N(x) = c · √(Σ_b n_b)/(|A| + Σ_b n_b)` = Õ(1/√N).
  - Definition 1: `π̄ = argmax_{y∈S} [qᵀy − λ_N KL[π_θ, y]]`. The regulariser is "a reversed KL-divergence", and the solution is `π̄ = λ_N π_θ/(α − q)`. This is "slightly different from the softmax distribution obtained with KL[y, π_θ]", which is `∝ π_θ exp(q/λ_N)`.
  - Propositions 1–2: π̂ approximately follows the gradient of that objective, with `π̂(a*|x) ≤ π̄(a*|x)`. "Therefore, the influence of the regularization term decreases as the number of simulation increases, which makes π̄ rely increasingly more on search Q-values q and less on the policy prior π_θ".
  - Footnote 1 cautions that "terminologies such as prior follow Silver et al. (2017a) and do not relate to concepts in Bayesian statistics". They cite Levine 2018 for regularised policy optimisation.
- Levine, "Reinforcement learning and control as probabilistic inference: Tutorial and review", arXiv 2018 (1805.00909). It is in Grill et al.'s bibliography; the paper itself was not re-fetched. — [arXiv 2007.12509](https://arxiv.org/abs/2007.12509)
- Search for an explicit AlphaZero ↔ habit-prior mapping found none. The nearest are Fountas et al. 2020 (MCTS + habitual network, Claim 4) and a 2026 preprint that frames a learned policy network as an explicit habit prior acting in one forward pass. — [arXiv 2607.13612](https://arxiv.org/pdf/2607.13612) (preprint, not verified in detail)

### Inferences
- Formal correspondence (my synthesis, not a published claim). Active inference's posterior σ(ln E − γG) is the solution of `max_q −γ·qᵀG − KL[q ‖ E]`, i.e. a forward-KL-regularised softmax. Grill's π̄ is the same shape with a reverse KL, search Q-values in place of −G, and π_θ in place of E. So "π_θ ≈ E" and "MCTS ≈ an anytime approximation of the G-softmax" is a defensible mapping. The differences:
  - The KL direction differs.
  - λ_N shrinks with search effort, whereas the habit weight is fixed (precision γ is the analogue).
  - E is tabular and context-free in SPM, whereas π_θ is state-conditioned.
- In both frameworks the distilled prior is trained toward the search or posterior output (SPM: e += π; AlphaZero: π_θ → π̂; Fountas: KL(Q_φa ‖ P_MCTS)). This is the shared "amortise the search into a prior" loop.

### Gaps
- No paper was found arguing explicitly that "AlphaZero is amortised expected free energy". A targeted search ("AlphaZero active inference") returned nothing.

## Claim 6: Published measures of habit/amortised policy vs full G over training

### Takeaway
Weak support. The only direct agreement signal found is Fountas et al.'s D_t = KL[Q_φa(a_t) ‖ P(a_t)] between habit network and MCTS planner. It is used as the training loss and the precision driver, but no explicit curve of it was found. Their Fig. 3 compares task performance with planner-driven versus habit-driven action. Friston 2016 reports qualitative habit acquisition and devaluation insensitivity, with no agreement metric.

### Cited Findings
- Fountas et al. 2020: D_t = D_KL[Q_φa(a_t) ‖ P(a_t)] is the habit loss (Algorithm 1) and drives ω_t. Fig. 3A compares "different action selection strategies" (full MCTS, 1-step, habitual) during on-policy training. In Fig. 3C, "violin plots represent behavior driven by P(a_t) (the planner) and the gray box plots driven by the habitual network Q_φa(a_t)". — [arXiv 2006.04176](https://arxiv.org/abs/2006.04176)
- Fountas et al.: the T_dec threshold trades simulated states against performance, with the best performance near T_dec ≈ 0.8 (Fig. 4Aii–iii). This is a compute-versus-quality measure for the planner, not habit agreement. — [arXiv 2006.04176](https://arxiv.org/abs/2006.04176)
- Friston et al. 2016: habits form from repeated goal-directed choice and persist after devaluation ("the habit persists"). This is a behavioural, qualitative demonstration only. — [PMC5167251](https://pmc.ncbi.nlm.nih.gov/articles/PMC5167251/)
- Grill et al. 2020 give theoretical, not empirical-over-training, convergence of π̂ to π̄ as N grows (Proposition 2). — [arXiv 2007.12509](https://arxiv.org/abs/2007.12509)

### Inferences
- Candidate metrics an evaluation could adopt, by analogy with these papers:
  - KL(habit ‖ planner) per decision over training (Fountas's D_t).
  - Top-1 agreement rate between the habit argmax and the planner argmax.
  - Performance gap, habit-only vs planner (Fountas Fig. 3).
  - Search effort needed to reach a confidence threshold (nodes until T_dec), which should fall as the prior improves.
  None of these is reported as a time series of "amortisation progress" in the sources found.

### Gaps
- No published learning curve of habit–planner agreement in active inference was found. The Fountas figure contents (whether D_t is plotted) were inferred from captions; the figure itself was not inspected.

## Open question: where the training data for amortising G comes from

### Takeaway
In every source the data is the agent's **own on-policy experience**: its own posterior over policies, its own executed actions, or its own planner outputs. This is the analogue of self-play against the environment. Fountas et al. also mention off-policy and offline training from a pre-recorded dataset. No source uses recorded expert or other-agent behaviour as the primary habit signal, and none uses multi-agent self-play.

### Cited Findings
- Friston 2016: habits are "learned by observing one's own goal directed behaviour". SPM adds the agent's own end-of-trial policy posterior to e. — [PMC5167251](https://pmc.ncbi.nlm.nih.gov/articles/PMC5167251/); [SPM](https://github.com/spm/spm/blob/main/toolbox/DEM/spm_MDP_VB_X.m)
- Fountas 2020: the habit is trained on-policy against the MCTS output at visited states. "For either off-policy or off-line training, the action applied to the environment (line 9) is drawn from a different policy or loaded from a pre-recorded data-set respectively". — [arXiv 2006.04176](https://arxiv.org/abs/2006.04176)
- Millidge 2020: the G-network is TD-bootstrapped from a replay buffer of the agent's own transitions. — [arXiv 1907.03876](https://arxiv.org/abs/1907.03876)
- Çatal 2019: the habit network is trained by minimising G at each timestep, using the learned model (simulated/imagined evaluation). — [arXiv 1904.08149](https://arxiv.org/html/1904.08149)
- Sophisticated inference itself is fully online with no amortised data source, and notes that amortisation "precludes online" inference. — [arXiv 2006.04120](https://arxiv.org/abs/2006.04120)

### Inferences
- For an argument-game setting the open design choice remains open in this literature: amortising G from (a) self-play search outputs (AlphaZero/Fountas style), (b) recorded human/agent debate transcripts (offline, the Fountas "pre-recorded data-set" option), or (c) model-based simulation (Çatal style). In active inference the habit is defined relative to the agent's own goal-directed posterior, so recorded behaviour of others would make E an imitation prior rather than an amortisation of the agent's own G. That changes its meaning.

### Gaps
- No active-inference paper was found on multi-agent or adversarial self-play to amortise G.
- No paper compares the data sources (self-play vs recorded vs simulated) for habit learning.
