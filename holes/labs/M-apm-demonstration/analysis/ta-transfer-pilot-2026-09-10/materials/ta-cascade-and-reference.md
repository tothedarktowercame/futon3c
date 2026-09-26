# Frozen TA-only cascades, hints and reference proofs
Do not expose to Student before initial attempts. Prototype: b9e09bb5's condition/construction/error-signal contract; this is paper mathematics, no Lean claim.
Definitions: uniform continuity and total boundedness appear in source lesson. Uniform equicontinuity quantifies δ before f; pointwise boundedness quantifies B after x and before f. A Cauchy sequence eventually has all pairwise distances below each ε. Completeness means every Cauchy sequence converges. Density gives a point of A in every positive ball centered in X. Limits are unique in metric spaces.
Pattern revisions: exact original texts/hashes and futon3 SHA in patterns.json. Existing coverage in C1 is compositional and requires a family-level adaptation, not an assertion that a canonical pattern already states the whole theorem. reviewed-links.json preserves exact-ID snapshot attachments; attachment is not verification of method implementation. The source lesson is explicitly not a memory.

## C1: global family bound → shared finite cover → shared oscillation and center bounds
C1.1 obtain ONE δ for ε=1 uniformly over F.
C1.2 total boundedness supplies finitely many centers c_i with d(x,c_i)<δ (choose a δ/2-net if convention gives ≤).
C1.3 at each center obtain B_i valid for ALL f; M=1+max_i B_i.
C1.4 triangle inequality gives ||f(x)||≤||f(c_i)||+||f(x)-f(c_i)||<M.
All required conditions are in the target. No compactness or completeness of X, or finite dimensionality of Y is needed.
Contrast: on X=[0,1], triangular peaks f_n(x)=max(0,1-n²|x-1/n|)n for n≥2 are each Lipschitz. For fixed x>0 only finitely many are nonzero; at x=0 all vanish. Hence pointwise bounded but sup_{n,x} f_n(x)=∞. Uniform equicontinuity fails. This explicit contrast is the reference, not initial Student exposure.
Hint 1: which source constants must be chosen before f? Hint 2: use ε=1 and a common finite net, then take maximum of center bounds. Hint 3: for the contrast use taller, narrower peaks accumulating at 0 but vanishing there.

## C2: adjudicate general assertion → check boundedness/UC → inspect net condition
Diameter is 1; δ=1/2 forces equality so f is UC for every ε; f is unbounded since f(n)=n. For r<1 every ball has one point, so no finite r-net covers N. Source step C1.2 is absent. The canonical bounded-domain clause is a library error, not a theorem to trust. Correct rejection is a success; do not infer Student error from the library error.
Hint 1: what does distance <1/2 force? Hint 2: count points in balls of radius <1.

## C3: extension → sequence limits → Cauchy images → choice independence → UC → uniqueness
For each x choose a_n in A with d(a_n,x)<1/n (n≥1). Then a_n is Cauchy; UC sends it to a Cauchy sequence by the ε/δ definition. Completeness of Y supplies F(x)=lim f(a_n). If b_n→x too, d(a_n,b_n)→0, so d(f(a_n),f(b_n))→0; triangle inequality and limits give equal image limits. For x in A compare with constant sequence x, giving F(x)=f(x).
For ε>0 choose source UC δ for ε/2. If d(x,y)<δ/3, take approximants a_n→x,b_n→y; eventually d(a_n,x),d(b_n,y)<δ/3, so d(a_n,b_n)<δ. Hence d(f(a_n),f(b_n))<ε/2, and passing to limits gives d(F(x),F(y))≤ε/2<ε. This avoids losing strictness at the limit. For any continuous extension G, G(x)=lim G(a_n)=lim f(a_n)=F(x). Completeness of X is unused. If X empty there is the unique empty map; density implies A nonempty when X nonempty. Incomplete Y can fail: A=Q in X=R, Y=Q, f=id has no continuous extension into Q.
Candidate method: construct an extension by completing images of approximating sequences and prove independence before packaging. metric-cauchy-convergence covers a component, local-to-global gives general guidance. Search-audit is only a bounded lexical search; do not assert proven absence of this method from all stores, or promote it from this example.
Hint 1: approximate each x by a sequence in A; which sequence should be Cauchy? Hint 2: compare two approximating sequences by triangle inequality. Hint 3: use ε/2 before passing inequalities to a limit.

## Adjudication
Preserve retrieved, actually read, claimed used, and proof-supported used separately. Require command/output evidence for actual reads and proof steps for use. A reference to an unread memory is not use. Categories: retrieval miss, applicability gap, condition gap, construction gap, library error, verification failure, undetermined. With correct initial proof, report no observed core difficulty; request a bounded contrast/revision without inventing an error. No causal efficacy, prevalence or independent-session comparison.
