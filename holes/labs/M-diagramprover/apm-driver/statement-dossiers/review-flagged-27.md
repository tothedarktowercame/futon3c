# Statement dossier — review-flagged-27

Review each: source TeX vs Lean statement. Verdict per problem:
`approve` / `flag <reason>` (edit statements-manifest.jsonl or use
the campaign `review` subcommand).

## m01J06 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:lam']

### Source TeX
```tex
Consider the functional $E:S\rightarrow \R$ where $S$ is a subset of
$C^2([0,1],\R)$ and
$$E(\varphi)=\int_{0}^1\mathcal{E}(\varphi(s),\varphi'(s))\ ds,$$
where $\mathcal{E}(q,p)=\lambda\cos q + \frac{1}{2}K p^2$ where
$\lambda>0$ and $K>0$ are constants. This functional represents the
potential energy of a thin planar rod of bending stiffness $K$ that is
subject to an external load $\lambda$.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Let $S$ be the subset of functions that satisfy the essential
boundary condition $\varphi(0)=0$. Develop the first-order necessary
conditions for $\varphi(s)$ to be an extremal of $E$. Hint: What
boundary condition on $\varphi$ arises at $s=1$?
%------------------------------------------------------------------*
\item Verify that $\varphi_0(s)\equiv 0$ is an extremal for any
$\lambda>0$. For the nonlinear system of part (a), consider a
perturbation expansion of the form
$\varphi_{\epsilon}=\epsilon\varphi_1+\epsilon^2\varphi_2+\ldots $ ,
where $|\epsilon|<<1$. Develop a linear boundary value problem for
$\varphi_1$ and find values of $\lambda$ for which this problem admits
nontrivial solutions (if any such values exist). Based no these
results, do you expect the system of part (a) to admit nontrivial
solutions?
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m01j06
    (lam K : ℝ) (hlam : 0 < lam) (hK : 0 < K) :
    (∀ φ : ℝ → ℝ,
      apm_m01J06_stationary lam K φ →
        apm_m01J06_nonlinearSolution lam K φ) ∧
    apm_m01J06_stationary lam K (fun _ ↦ 0) ∧
    ((∃ φ : ℝ → ℝ,
        apm_m01J06_linearMode lam K φ ∧ apm_m01J06_nontrivial φ) ↔
      ∃ m : ℕ, lam = apm_m01J06_criticalLoad K m) ∧
    (∀ m : ℕ,
      apm_m01J06_isBifurcationLoad K (apm_m01J06_criticalLoad K m)) := by
```

### Declared repairs
The phrase "Based no these results" is read as "Based on these results". No
mathematical repair is made.

## m02J01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-trivially-short']

### Source TeX
```tex
Let $D'(\R)$ denote the space of distributions on $\R$. Prove
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item If $\varphi\in D(\R)$ is a test function, then
$\int_{\R}\varphi=0$ if and only if there is a $\psi\in D(\R)$ for
which $\varphi=\psi'$.
%------------------------------------------------------------------*
\item For every $u\in D'(\R)$ there is a primative, that is, a $v\in
D'(\R)$ for which $v'=u$.
%------------------------------------------------------------------*
\item Any two primatives of $u$ must differ by a constant.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m02j01 :
    (∀ φ : TestFunction (⊤ : Opens ℝ) ℝ ⊤,
      (∫ x : ℝ, φ x) = 0 ↔
        ∃ ψ : TestFunction (⊤ : Opens ℝ) ℝ ⊤,
          ∀ x : ℝ, φ x = deriv (ψ : ℝ → ℝ) x) ∧
    (∀ u : Distribution (⊤ : Opens ℝ) ℝ ⊤,
      ∃ v : Distribution (⊤ : Opens ℝ) ℝ ⊤,
        apm_m02j01_IsDistributionalDerivative v u) ∧
    (∀ u v₁ v₂ : Distribution (⊤ : Opens ℝ) ℝ ⊤,
      apm_m02j01_IsDistributionalDerivative v₁ u →
      apm_m02j01_IsDistributionalDerivative v₂ u →
      ∃ c : ℝ, ∀ φ : TestFunction (⊤ : Opens ℝ) ℝ ⊤,
        (v₁ - v₂) φ = c * ∫ x : ℝ, φ x) := by
```

### Declared repairs
The spelling `primative` in the source is read as `primitive`; no mathematical
change is made.

## m02J05 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:S']

### Source TeX
```tex
Suppose that $\alpha: [0,\infty) \rightarrow \R$ is continuous and
strictly increasing from $\alpha(0)=0$ to $\lim_{t\rightarrow
\infty} \alpha(t)=\infty$. Let $A(t)=\int_{0}^t\alpha(\tau)d\tau$ and
$\tilde{A}(t)=\int_{0}^t\alpha^{-1}(\tau)d\tau$, where $\alpha^{-1}$
is the inverse of $\alpha$. Let $$K_A=\{u: u\ \mathrm{measurable},
\int_{0}^1A(|u(x)|)dx<\infty\}.$$ 
Define the Orlicz space $L_A$ to be the set of $u$'s that are
measurable and $\exists \lambda\neq 0,
\lambda u \in K_A$.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Prove that $K_A$ is convex;
%------------------------------------------------------------------*
\item Prove that $L_A$ is a linear space (\ie\ a vector space);
%------------------------------------------------------------------*
\item Define $\|u\|_A=\inf \{\lambda>0: \int_{0}^1 A
(\frac{|u(x)|}{\lambda}) dx \leq 1\}$ and show that $\|u\|_A$ is a
well-defined norm on $L_A$.
%------------------------------------------------------------------*
\item Prove the following variant of H\"older's inequality: 
$\int_{0}^1 u(x)v(x)dx \leq 2\|u\|_A \|v\|_{\tilde{A}}$. Hint: first
show that for any $s,t\in [0,\infty)$ that $st\leq A(s)+\tilde{A}(t)$.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m02j05 :
    ∀ (α β : ℝ → ℝ),
      ContinuousOn α (Ici 0) →
      StrictMonoOn α (Ici 0) →
      α 0 = 0 →
      Tendsto α atTop atTop →
      ContinuousOn β (Ici 0) →
      (∀ t, 0 ≤ t → 0 ≤ α t ∧ 0 ≤ β t) →
      (∀ t, 0 ≤ t → β (α t) = t) →
      (∀ t, 0 ≤ t → α (β t) = t) →
      Convex ℝ (apm_m02J05_K α) ∧
      (∃ S : Submodule ℝ (ℝ →ₘ[volume] ℝ),
        (S : Set (ℝ →ₘ[volume] ℝ)) = apm_m02J05_L α) ∧
      ((∀ u ∈ apm_m02J05_L α, 0 ≤ apm_m02J05_lux α u) ∧
        (∀ u ∈ apm_m02J05_L α, apm_m02J05_lux α u = 0 ↔ u = 0) ∧
        (∀ u ∈ apm_m02J05_L α, ∀ c : ℝ,
          apm_m02J05_lux α (c • u) = |c| * apm_m02J05_lux α u) ∧
        (∀ u ∈ apm_m02J05_L α, ∀ v ∈ apm_m02J05_L α,
          apm_m02J05_lux α (u + v) ≤
            apm_m02J05_lux α u + apm_m02J05_lux α v)) ∧
      (∀ s t : ℝ, 0 ≤ s → 0 ≤ t →
        s * t ≤ apm_m02J05_young α s + apm_m02J05_young β t) ∧
      (∀ u ∈ apm_m02J05_L α, ∀ v ∈ apm_m02J05_L β,
        ∫ x in Icc (0 : ℝ) 1, u x * v x ∂volume ≤
          2 * apm_m02J05_lux α u * apm_m02J05_lux β v) := by
```

### Declared repairs
Measurable functions are taken modulo almost-everywhere equality. Without this
standard identification, the displayed Luxemburg functional is only a
seminorm, since it cannot distinguish functions differing on a null set.

## m03J03 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:A,N,n,fun']

### Source TeX
```tex
Let $\mathcal{D}'(\Omega)$ denote the space of distributions on
$\Omega$.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Show that $T(\varphi)=\sum_{n=0}^{\infty}\varphi(n)$  defines
an element of $\mathcal{D}'(\R)$.
%------------------------------------------------------------------*
\item Show that the series $T=\sum_{n=0}^{\infty}a_ne^{nix}$ converges
and defines $T\in \mathcal{D}'(\R)$ whenever there is an $A\geq 0$ and
an $N\geq 0$ such that $\forall n: |a_n|\leq A|n|^N$.
%------------------------------------------------------------------*
\item Evaluate the derivatives $D(H(\cdot )\sin(\cdot ))$ and 
$D^2(H(\cdot )\sin(\cdot ))$ in $\mathcal{D}'(\R)$ and show your
answer is correct.
%------------------------------------------------------------------*
\item Show that $T(\varphi)=|\varphi(0)|$ does not define an element
of $\mathcal{D}'(\R)$.
%------------------------------------------------------------------*
 \end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\soln{This seems like a badly typed problem. Part (a) and part (b) ask
(in part) the same question. Will have to take a look at the original
file on Monday... or at the web.}
```

### Lean statement
```lean
theorem apm_m03j03 :
    (∃ T : Distribution (⊤ : Opens ℝ) ℂ ⊤,
      ∀ φ : TestFunction (⊤ : Opens ℝ) ℝ ⊤,
        T φ = ∑' n : ℕ, (φ n : ℂ)) ∧
    (∀ a : ℕ → ℂ, (∃ (A : ℝ) (N : ℕ), 0 ≤ A ∧
        ∀ n : ℕ, ‖a n‖ ≤ A * (n : ℝ) ^ N) →
      ∃ T : Distribution (⊤ : Opens ℝ) ℂ ⊤,
        ∀ φ : TestFunction (⊤ : Opens ℝ) ℝ ⊤,
          HasSum (fun n : ℕ =>
            a n * (∫ x : ℝ,
              Complex.exp ((n : ℂ) * Complex.I * (x : ℂ)) * φ x)) (T φ)) ∧
    (∃ T DT D2T : Distribution (⊤ : Opens ℝ) ℝ ⊤,
      ∀ φ : TestFunction (⊤ : Opens ℝ) ℝ ⊤,
        T φ = ∫ x : ℝ, m03J03Heaviside x * Real.sin x * φ x ∧
        DT φ = -∫ x : ℝ,
          m03J03Heaviside x * Real.sin x * deriv φ x ∧
        DT φ = ∫ x : ℝ, m03J03Heaviside x * Real.cos x * φ x ∧
        D2T φ = ∫ x : ℝ,
          m03J03Heaviside x * Real.sin x * iteratedDeriv 2 φ x ∧
        D2T φ = φ 0 - T φ) ∧
    (¬ ∃ T : Distribution (⊤ : Opens ℝ) ℝ ⊤,
      ∀ φ : TestFunction (⊤ : Opens ℝ) ℝ ⊤, T φ = |φ 0|) := by
```

### Declared repairs
None.  Mathlib's distribution convention uses real-valued test functions and
allows complex-valued distributions, which faithfully accommodates every part.
-/

open MeasureTheory Set TopologicalSpace
open scoped Distributions ContDiff

noncomputable section

/-- The Heaviside function with the irrelevant value at zero chosen to be zero. -/
def m03J03Heaviside (x : ℝ) : ℝ := if 0 < x then 1 else 0

/-- The one-sided Dirac comb, polynomial-growth Fourier series, the two
distributional derivatives of `H sin`, and the absolute-value non-example. -/
theorem apm_m03j03 :
    (∃ T : Distribution (⊤ :

## m93A01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:n,n,n']

### Source TeX
```tex
Let $H^k(M)$ denote the Sobolev space of functions on $M$ with norm
$\|f\|^2=\sum_{i=0}^k\|D^if\|^2_{L^2}$, and let $\Son$ be the unit
circle.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Show in detail that the embedding $H^1(\Son)\rightarrow
L^2(\Son)$ is compact.
%------------------------------------------------------------------*
\item Which of the following maps are bounded and which are compact?
Justify your answer.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{rlist}
%------------------------------------------------------------------*
\item The inclusion of $H^1(\R)$ in $L^2(\R)$.
%------------------------------------------------------------------*
\item The derivative map from $H^1(\Son)$ to $L^2(\Son)$.
%------------------------------------------------------------------*
\item The derivative map from $H^2(\Son)$ to $L^2(\Son)$.
%------------------------------------------------------------------*
\end{rlist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m93a01
    (ιS1 D1 D2 : apm_m93A01_l2Z →L[ℂ] apm_m93A01_l2Z)
    (ιR : apm_m93A01_l2R →L[ℂ] apm_m93A01_l2R)
    (hιS1 : ∀ (f : apm_m93A01_l2Z) (n : ℤ),
      ιS1 f n = apm_m93A01_circleInclCoeff n * f n)
    (hD1 : ∀ (f : apm_m93A01_l2Z) (n : ℤ),
      D1 f n = apm_m93A01_circleDerivH1Coeff n * f n)
    (hD2 : ∀ (f : apm_m93A01_l2Z) (n : ℤ),
      D2 f n = apm_m93A01_circleDerivH2Coeff n * f n)
    (hιR : ∀ f : apm_m93A01_l2R,
      (fun ξ : ℝ => ιR f ξ) =ᵐ[volume]
        fun ξ => apm_m93A01_realInclCoeff ξ * f ξ) :
    IsCompactOperator ιS1 ∧
    ¬ IsCompactOperator ιR ∧
    ¬ IsCompactOperator D1 ∧
    IsCompactOperator D2 := by
```

### Declared repairs
None.

## m93A02 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:H,L,Q']

### Source TeX
```tex
Let $H$ and $K$ be Banach spaces. Let $L:H\rightarrow K$ be a
surjective linear map that such that $\exists \delta>0:\forall h\in
H:\|Lh\|\geq \delta \|h\|$. Let $Q$ be a bilinear map, $Q:H\times
H\rightarrow K$ such that $\|Q(h_1,h_2)\|\leq M\|h_1\|\|h_2\|$ for
some $M$.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Find constants $\epsilon,B>0$ such that $\forall k\in
K:\|k\|<\epsilon\Rightarrow$ the equation $L(H)+Q(h,h)=k$ has a
solution $h$ with $\|h\|<B\|k\|$.
%------------------------------------------------------------------*
\item Suppose $L$ is not surjective but meets all other conditions in
(a). Can a constant $\epsilon>0$ be found for which equation (1) has a
solution for all $k\in K$ with $\|k\|<\epsilon$? Prove it can or find
a counterexample.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m93a02
    (H K : Type*)
    [NormedAddCommGroup H] [NormedSpace ℝ H] [CompleteSpace H]
    [NormedAddCommGroup K] [NormedSpace ℝ K] [CompleteSpace K]
    (L : H →ₗ[ℝ] K) (Q : H →ₗ[ℝ] H →ₗ[ℝ] K)
    (δ M : ℝ) (hδ : 0 < δ) (hM : 0 ≤ M)
    (hL_surj : Function.Surjective L)
    (hL_lower : ∀ h : H, δ * ‖h‖ ≤ ‖L h‖)
    (hQ : ∀ h₁ h₂ : H, ‖Q h₁ h₂‖ ≤ M * ‖h₁‖ * ‖h₂‖) :
    (∃ ε B : ℝ, 0 < ε ∧ 0 < B ∧
      ∀ k : K, ‖k‖ < ε →
        ∃ h : H, L h + Q h h = k ∧ ‖h‖ ≤ B * ‖k‖) ∧
    (∀ ε : ℝ, 0 < ε →
      ∃ k : ℝ × ℝ, ‖k‖ < ε ∧
        ¬ ∃ h : ℝ, (h, 0) + (0, h * h) = k) := by
```

### Declared repairs
* In part (a), `L(H) + Q(h,h) = k` is ill-typed because `L(H)` denotes the
  range of `L`; it is minimally repaired to `L h + Q(h,h) = k`.
* The printed strict estimate `‖h‖ < B ‖k‖` cannot hold at `k = 0`.
  It is minimally repaired to `‖h‖ ≤ B ‖k‖`, preserving the requested
  neighbourhood of zero and its quantitative linear bound.

## m93J01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:y,fun,fun,fun', 'conclusion-trivially-short']

### Source TeX
```tex
The differential operator $Ly=-\dbydk{x}{2}y+V(x)y$ is called the
Schr\"odinger operator and plays an important role in physics. We are
going to study some distrete methods to solve it numerically.

Pick any $h>0$ -- we will assume it is small. Let $y_n$ denote $y(nh)$
and similarly for any other function.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Find a number $\kappa$ such that
$y_{n+1}+y_{n-1}-2y_n=h^2y''_n+\kappa h^4 y_n^{(\mathrm{iv})}+o(h^6)$.
%------------------------------------------------------------------*
\item Show that then we have
$(Ly)_n=-y_{n+1}-y_{n-1}+y_n(2+h^2v_n)-\kappa h^4
y_n^{(\mathrm{iv})}+o(h^6)$.
%------------------------------------------------------------------*
\item Prove that $y''_{n+1}+y''_{n-1}-2y''_n=h^2
y_n^{(\mathrm{iv})}+o(h^4)$.
%------------------------------------------------------------------*
\item Use the result in (c) to devise a scheme of order $6$ to solve
the equation $Ly=0$.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m93j01
    (y V : ℝ → ℝ) (hy : ContDiff ℝ 8 y) :
    apm_m93J01_kappa = (1 / 12 : ℝ) ∧
    (∀ x : ℝ,
      (fun h : ℝ ↦
        y (x + h) + y (x - h) - 2 * y x -
          (h ^ 2 * iteratedDeriv 2 y x +
            apm_m93J01_kappa * h ^ 4 * iteratedDeriv 4 y x))
        =O[nhds 0] (fun h : ℝ ↦ h ^ 6)) ∧
    (∀ x : ℝ,
      (fun h : ℝ ↦
        h ^ 2 * apm_m93J01_schrodinger V y x -
          (-y (x + h) - y (x - h) +
            (2 + h ^ 2 * V x) * y x +
            apm_m93J01_kappa * h ^ 4 * iteratedDeriv 4 y x))
        =O[nhds 0] (fun h : ℝ ↦ h ^ 6)) ∧
    (∀ x : ℝ,
      (fun h : ℝ ↦
        iteratedDeriv 2 y (x + h) + iteratedDeriv 2 y (x - h) -
          2 * iteratedDeriv 2 y x - h ^ 2 * iteratedDeriv 4 y x)
        =O[nhds 0] (fun h : ℝ ↦ h ^ 4)) ∧
    ((∀ x : ℝ, apm_m93J01_schrodinger V y x = 0) →
      ∀ x : ℝ,
        (fun h : ℝ ↦
          (1 - h ^ 2 / 12 * V (x + h)) * y (x + h) -
          (2 + 5 * h ^ 2 / 6 * V x) * y x +
          (1 - h ^ 2 / 12 * V (x - h)) * y (x - h))
          =O[nhds 0] (fun h : ℝ ↦ h ^ 6)) := by
```

### Declared repairs
1. The remainders printed as `o(h⁶)` in (a),(b) and `o(h⁴)` in (c)
   must be `O(h⁶)` and `O(h⁴)`: the next Taylor terms generally have
   exactly those orders.
2. Part (b) is dimensionally missing the factor `h²` on `(Ly)ₙ`, and the
   fourth-derivative correction must have sign `+`. We state the corrected
   identity confirmed by direct substitution and by the supplied solution.
3. The lowercase `vₙ` in part (b) is interpreted as the sampled potential
   `V(nh)` defined in the preamble.

## m93J07 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:let']

### Source TeX
```tex
Are there examples of these things?  In each case, if there is an
example, describe it.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item A nonseparable Banach space.
%------------------------------------------------------------------*
\item A separable nonreflexive Banach space.
%------------------------------------------------------------------*
\item A nonseparable, reflexive Banach space.
%------------------------------------------------------------------*
\item A nonseparable, nonreflexive Banach space.
%------------------------------------------------------------------*
\item A separable Banach space whose dual is nonseparable.
%------------------------------------------------------------------*
\item A nonseparable Banach space whose dual is separable.
%------------------------------------------------------------------*
\item A linear map $\Phi:L^2([0,1])\rightarrow L^2([0,1])$ whose image
is dense in $L^2([0,1])$ but not the whole of $L^2([0,1])$.
%------------------------------------------------------------------*
\item A bounded linear map $\Phi:L^2([0,1])\rightarrow L^2([0,1])$
whose image is dense in $L^2([0,1])$ but not the whole of $L^2([0,1])$.
%------------------------------------------------------------------*
\item A function $f\in L^2(\R)$ such that $f\notin L^4(\R)$.
%------------------------------------------------------------------*
\item A function $f\in L^4(\R)$ such that $f\notin L^2(\R)$.
%------------------------------------------------------------------*
\item A function $f\in L^2([0,1])$ such that $f\notin L^4([0,1])$.
%------------------------------------------------------------------*
\item A function $f\in L^4([0,1])$ such that $f\notin L^2([0,1])$.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\soln{this shouldn't be too bad, at least, not the second part}
```

### Lean statement
```lean
theorem apm_m93j07 :
    (∃ X : apm_m93j07_Banach, ¬ apm_m93j07_Separable X) ∧
    (∃ X : apm_m93j07_Banach,
      apm_m93j07_Separable X ∧ ¬ apm_m93j07_Reflexive X) ∧
    (∃ X : apm_m93j07_Banach,
      ¬ apm_m93j07_Separable X ∧ apm_m93j07_Reflexive X) ∧
    (∃ X : apm_m93j07_Banach,
      ¬ apm_m93j07_Separable X ∧ ¬ apm_m93j07_Reflexive X) ∧
    (∃ X : apm_m93j07_Banach,
      apm_m93j07_Separable X ∧ ¬ apm_m93j07_DualSeparable X) ∧
    (¬ ∃ X : apm_m93j07_Banach,
      ¬ apm_m93j07_Separable X ∧ apm_m93j07_DualSeparable X) ∧
    (let H := Lp ℝ 2 (volume.restrict (Set.Icc (0 : ℝ) 1));
      (∃ Φ : H →ₗ[ℝ] H, DenseRange Φ ∧ ¬ Function.Surjective Φ) ∧
      (∃ Φ : H →L[ℝ] H, DenseRange Φ ∧ ¬ Function.Surjective Φ)) ∧
    (∃ f : ℝ → ℝ, MemLp f 2 volume ∧ ¬ MemLp f 4 volume) ∧
    (∃ f : ℝ → ℝ, MemLp f 4 volume ∧ ¬ MemLp f 2 volume) ∧
    (∃ f : ℝ → ℝ,
      MemLp f 2 (volume.restrict (Set.Icc 0 1)) ∧
      ¬ MemLp f 4 (volume.restrict (Set.Icc 0 1))) ∧
    (¬ ∃ f : ℝ → ℝ,
      MemLp f 4 (volume.restrict (Set.Icc 0 1)) ∧
      ¬ MemLp f 2 (volume.restrict (Set.Icc 0 1))) := by
```

### Declared repairs
None.  The two impossible requests are represented as nonexistence claims.

## m94A04 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:volume,volume,volume,volume']

### Source TeX
```tex
Let $B_1=L^8([0,1])$, $B_2=L^2([0,1])$. Define the operator
$T:B_1\rightarrow B_2$ by $(Tf)(t)=\sin t+[f(t)]^3$. Find the
Fr\'echet derivative if and where it exists.
```

### Lean statement
```lean
theorem apm_m94a04 :
    ∃ T : Lp ℝ 8 (volume.restrict (Icc (0 : ℝ) 1)) →
        Lp ℝ 2 (volume.restrict (Icc (0 : ℝ) 1)),
      apm_m94a04_IsOperator T ∧
      ∀ f : Lp ℝ 8 (volume.restrict (Icc (0 : ℝ) 1)),
        ∃ L : Lp ℝ 8 (volume.restrict (Icc (0 : ℝ) 1)) →ₗ[ℝ]
            Lp ℝ 2 (volume.restrict (Icc (0 : ℝ) 1)),
          apm_m94a04_IsFrechetDerivative T f L ∧
          ∀ h : Lp ℝ 8 (volume.restrict (Icc (0 : ℝ) 1)),
            ∀ᵐ t ∂(volume.restrict (Icc (0 : ℝ) 1)),
              L h t = 3 * (f t) ^ 2 * h t := by
```

### Declared repairs
None.

## m94J01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:fun,n,fun,n']

### Source TeX
```tex
Let $\mathcal{D}'$ and $\mathcal{S}'$ be the usual Schwartz spaces of
distributions on $\R$. Given any $k,n\in \N$, denote by
$\delta_n^{(k)}$ the $k$th derivative of the delta function centered
at $n$, \ie\ $\delta_n^{(k)}(f)=(-1)^kf^{(k)}(n)$. Does the sequence
$n\mapsto \delta_n^{(n)}$ have a limit in $\mathcal{D}'$ and if it
does, what is the limit? Answer the same question for the space
$\mathcal{S}'$.
```

### Lean statement
```lean
theorem apm_m94j01 :
    (∀ φ : TestFunction (⊤ : TopologicalSpace.Opens ℝ) ℝ ⊤,
      Tendsto
        (fun n : ℕ ↦ (-1 : ℝ) ^ n * iteratedDeriv n φ (n : ℝ))
        atTop (nhds 0)) ∧
    ¬∃ T : TemperedDistribution ℝ ℂ,
      ∀ φ : 𝓢(ℝ, ℂ),
        Tendsto
          (fun n : ℕ ↦ (-1 : ℂ) ^ n * iteratedDeriv n φ (n : ℝ))
          atTop (nhds (T φ)) := by
```

### Declared repairs
None.

## m94J02 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:fun,n,fun,n', 'conclusion-trivially-short']

### Source TeX
```tex
Let $D$ be the closed unit disk in $\C$ and denote by $X$ the vector
space of all functions $f:D\rightarrow C$ that can be represented as a
power series $f(z)=\sum_{n=0}^{\infty}f_nz^n$ with
$\|f\|=\sum_{n=0}^{\infty}|f_n|(n^2+1)<\infty$.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Show that $X$ is a Banach space with the norm $\|\cdot\|$.
%------------------------------------------------------------------*
\item Show that all functions in $X$ are continuous on $D$.
%------------------------------------------------------------------*
\item Show that $X$ is a Banach algebra under pointwise multiplication. 
\Ie\ $\|fg\|\leq \|f\|\|g\|$ for all $f,g\in X$.
%------------------------------------------------------------------*
\item Show that $\|f^n\|^{1/n}$ converges as $n\rightarrow \infty$ for 
every $f\in X$.
%------------------------------------------------------------------*
\item Show that the operator $T:X\rightarrow X$ defined by 
$(Tf)(z)=f(z/2)$ is compact.
%------------------------------------------------------------------*
\item Finally, determine the spectrum of $T$.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m94j02 :
    (∀ a : ℕ → ℕ → ℂ,
        (∀ k, apm_m94J02_mem (a k)) →
        (∀ ε > 0, ∃ N, ∀ m ≥ N, ∀ n ≥ N,
          apm_m94J02_norm (fun k ↦ a m k - a n k) < ε) →
        ∃ b, apm_m94J02_mem b ∧
          ∀ ε > 0, ∃ N, ∀ n ≥ N,
            apm_m94J02_norm (fun k ↦ a n k - b k) < ε) ∧
      (∀ a, apm_m94J02_mem a →
        ContinuousOn (apm_m94J02_eval a) (Metric.closedBall 0 1)) ∧
      (∀ a b, apm_m94J02_mem a → apm_m94J02_mem b →
        apm_m94J02_mem (apm_m94J02_mul a b) ∧
        apm_m94J02_norm (apm_m94J02_mul a b) ≤
          (5 / 4 : ℝ) * apm_m94J02_norm a * apm_m94J02_norm b) ∧
      (∀ a, apm_m94J02_mem a →
        ∃ L : ℝ, Tendsto
          (fun n : ℕ ↦ Real.rpow (apm_m94J02_norm (apm_m94J02_pow a n))
            ((n : ℝ)⁻¹)) atTop (nhds L)) ∧
      (∀ a, apm_m94J02_mem a → apm_m94J02_mem (apm_m94J02_T a)) ∧
      (∀ a : ℕ → ℕ → ℂ,
        (∀ k, apm_m94J02_mem (a k) ∧ apm_m94J02_norm (a k) ≤ 1) →
        ∃ σ : ℕ → ℕ, StrictMono σ ∧ ∃ b, apm_m94J02_mem b ∧
          Tendsto (fun k ↦ apm_m94J02_norm
            (fun n ↦ apm_m94J02_T (a (σ k)) n - b n)) atTop (nhds 0)) ∧
      apm_m94J02_spectrum =
        {0} ∪ Set.range (fun n : ℕ ↦ (2 : ℂ) ^ (-(n : ℤ))) := by
```

### Declared repairs
The claimed bound `‖fg‖ ≤ ‖f‖‖g‖` is false for the printed weight
`n² + 1`: for `f = g = z`, the two sides are `5` and `4`. It is
minimally repaired to the sharp bound `‖fg‖ ≤ (5/4)‖f‖‖g‖`, which still
makes the complete normed space a Banach algebra.

## m95A05 — gate: pending-review
**Lint flags (advisory):** ['conclusion-trivially-short']

### Source TeX
```tex
Let $A\in L(V,V')$, where $V$ is a reflexive Banach space and $V'$ is
its dual. Suppose there is a $c>0$ such that $\forall v\in V:\langle
Av,v \rangle\geq c\|v\|^2$. Let $f\in V'$ and consider the abstract
linear problem of finding $u\in V$ such that $Au=f$. Show that a
solution to this problem exists and is unique.
```

### Lean statement
```lean
theorem apm_m95a05
    (V : Type*) [NormedAddCommGroup V] [NormedSpace ℝ V] [CompleteSpace V]
    (hrefl : Function.Surjective (NormedSpace.inclusionInDoubleDual ℝ V))
    (A : V →L[ℝ] StrongDual ℝ V)
    (c : ℝ) (hc : 0 < c)
    (hcoercive : ∀ v : V, c * ‖v‖ ^ 2 ≤ A v v)
    (f : StrongDual ℝ V) :
    ∃! u : V, A u = f := by
```

### Declared repairs
None.

## m96J03 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:n,n', 'conclusion-trivially-short']

### Source TeX
```tex
Let $f:\Rn\rightarrow \R$ be given by $f(x)=1$ if $\|x\|\leq 1$ and
$f(x)=0$ if $\|x\|>0$, where $\|\cdot\|$ denotes the standard
Euclidean norm in $\Rn$. Find $\triangle f$ in the sense of
distributions.
```

### Lean statement
```lean
theorem apm_m96j03
    (n : ℕ) (φ : apm_m96j03_E n → ℝ)
    (hφ : apm_m96j03_IsTestFunction φ) :
    (∫ x : apm_m96j03_E n,
        apm_m96j03_ballIndicator x * apm_m96j03_laplacian φ x) =
      ∫ x : apm_m96j03_E n in Metric.sphere 0 1,
        fderiv ℝ φ x x ∂μH[(n - 1 : ℕ)] := by
```

### Declared repairs
The condition `f(x)=0` when `‖x‖>0` is corrected to `‖x‖>1`.
As printed, the two clauses overlap throughout `0 < ‖x‖ ≤ 1`; the correction
makes `f` the unit-ball indicator used by the supplied solution.

## m96J06 — gate: pending-review
**Lint flags (advisory):** ['conclusion-trivially-short']

### Source TeX
```tex
Show that the equation $x(t)-2x(0)+x(t/2+1)x(t/2-1)=y(t)$ ($|t|\leq
2$) has a solution $x\in C([-2,2])$ for any given function $y$ in some
small open neighborhood of zero in $C([-2,2])$.
```

### Lean statement
```lean
theorem apm_m96j06 :
    ∀ y : ℝ → ℝ,
      apm_m96J06_inNeighborhood y →
        ∃ x : ℝ → ℝ,
          apm_m96J06_isSolution x y ∧
          ∀ t ∈ Icc (-2 : ℝ) 2, |x t| ≤ (1 / 12 : ℝ) := by
```

### Declared repairs
None.

## m97A06 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:deriv']

### Source TeX
```tex
On the space of all continuously differentiable functions
$\varphi:[0,T]\rightarrow \R$ satisfying $\varphi(0)=\alpha$ and
$\varphi(T)=\beta$, define the integral
$I(\varphi)=\int_0^T(\varphi'(t)^2/2-\cos(2\varphi(t))/4)dt$.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Find the Euler-Lagrange equations of $I$.
%------------------------------------------------------------------*
\item Prove that $\varphi'^2/2+\cos(2\varphi)/4$ is constant for every
solution of the Euler-Lagrange equation.
%------------------------------------------------------------------*
\item Find a solution of the Euler-Lagrange equation with
$\varphi(0)=\varphi(T)=0$.
%------------------------------------------------------------------*
\item Compute the second variation or Hessian around the solution you
found in (c).
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m97a06 :
    ∀ T : ℝ, 0 < T →
      (∀ φ : ℝ → ℝ, apm_m97a06_EL φ T ↔
        ∀ t ∈ Set.Ioo (0 : ℝ) T,
          deriv (deriv φ) t = Real.sin (2 * φ t) / 2) ∧
      (∀ φ, apm_m97a06_EL φ T →
        ∃ E, ∀ t ∈ Set.Icc (0 : ℝ) T, apm_m97a06_energy φ t = E) ∧
      apm_m97a06_EL (fun _ => 0) T ∧
      (∀ η : ℝ → ℝ, η 0 = 0 → η T = 0 →
        deriv (deriv (fun ε : ℝ => apm_m97a06_I T (ε • η))) 0 =
          apm_m97a06_hessian T η) := by
```

### Declared repairs
None.
-/

noncomputable def apm_m97a06_I (T : ℝ) (φ : ℝ → ℝ) : ℝ :=
  ∫ t in Set.Icc (0 : ℝ) T,
    deriv φ t ^ 2 / 2 - Real.cos (2 * φ t) / 4

def apm_m97a06_EL (φ : ℝ → ℝ) (T : ℝ) : Prop :=
  ∀ t ∈ Set.Ioo (0 : ℝ) T,
    deriv (deriv φ) t = Real.sin (2 * φ t) / 2

noncomputable def apm_m97a06_energy (φ : ℝ → ℝ) (t : ℝ) : ℝ :=
  deriv φ t ^ 2 / 2 + Real.cos (2 * φ t) / 4

noncomputable def apm_m97a06_hessian (T : ℝ) (η : ℝ → ℝ) : ℝ :=
  ∫ t in Set.Icc (0 : ℝ) T, deriv η t ^ 2 + η t ^ 2

theorem apm_m97a06 :
    ∀ T : ℝ, 0 < T →
      (∀ φ : ℝ → ℝ, apm_m97a06_EL φ T ↔
        ∀ t ∈ Set.Ioo (0 

## m98A04 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:n,T,DT,d']

### Source TeX
```tex
Let $\mathcal{D}'$ denote the space of distributions and $\mathcal{D}$
the space of test functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item If $T\in \mathcal{D}'$ and $\varphi\in \mathcal{D}$, define the
convolution $T\ast \varphi$. Show that for each multiindex $\alpha$,
$D^{\alpha}(T\ast \varphi)=\partial^{\alpha}\ast\varphi=T\ast
D^{\alpha}\varphi$.
%------------------------------------------------------------------*
\item If $T\in \mathcal{D}'$ and $\varphi\in \mathcal{D}$, show that
$T\ast\varphi\in C^{\infty}(\Rn)$.
%------------------------------------------------------------------*
\item The convolution of $2\pi$-periodic functions $f$ and $g$ is
$\int_0^{2\pi}f(y)g(x-y)dy$. Compute the convolution of $f(x)=\sin x$ 
and $g(x)=\cos x$. 
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m98a04 :
    (∀ (n : ℕ)
      (T : Distribution (⊤ : Opens (EuclideanSpace ℝ (Fin n))) ℝ ⊤)
      (φ : TestFunction (⊤ : Opens (EuclideanSpace ℝ (Fin n))) ℝ ⊤),
      ∃ h : EuclideanSpace ℝ (Fin n) → ℝ,
        apm_m98a04_IsConvolution T φ h ∧ ContDiff ℝ ∞ h ∧
        ∀ indices : List (Fin n),
          ∃ (DT : Distribution (⊤ : Opens (EuclideanSpace ℝ (Fin n))) ℝ ⊤)
            (dφ : TestFunction (⊤ : Opens (EuclideanSpace ℝ (Fin n))) ℝ ⊤)
            (hDT hφ : EuclideanSpace ℝ (Fin n) → ℝ),
            apm_m98a04_IsDistributionalPartial indices T DT ∧
            (∀ x, dφ x = apm_m98a04_partialList indices (φ : _ → ℝ) x) ∧
            apm_m98a04_IsConvolution DT φ hDT ∧
            apm_m98a04_IsConvolution T dφ hφ ∧
            apm_m98a04_partialList indices h = hDT ∧ hDT = hφ) ∧
    (∀ x : ℝ,
      ∫ y in (0 : ℝ)..(2 * Real.pi),
        Real.sin y * Real.cos (x - y) = Real.pi * Real.sin x) := by
```

### Declared repairs
The middle expression in part (a), printed as `∂ᵅ * φ`, is missing its
distribution.  It is repaired to the standard identity
`Dᵅ(T * φ) = (Dᵅ T) * φ = T * (Dᵅ φ)`, matching the surrounding
text and the informal solution.

## m99A03 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:H,A,A,A']

### Source TeX
```tex
Let $H$ be a Hilbert space and $A:H\rightarrow H$ a bounded linear
operator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item If $A$ is compact and normal, $A^3=0$, prove that $A=0$.
%------------------------------------------------------------------*
\item Construct an example of a bounded linear operator
$A:H\rightarrow H$ such that $A$ and $A^2$ are not compact but $A^3$
is compact.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m99a03 :
    (∀ (H : Type*) [NormedAddCommGroup H] [InnerProductSpace ℂ H]
      [CompleteSpace H] (A : H →L[ℂ] H),
      IsCompactOperator A → IsStarNormal A → A ^ 3 = 0 → A = 0) ∧
    (∃ A : m99A03ExampleSpace →L[ℂ] m99A03ExampleSpace,
      ¬IsCompactOperator A ∧
      ¬IsCompactOperator
        ((A ^ 2 : m99A03ExampleSpace →L[ℂ] m99A03ExampleSpace) :
          m99A03ExampleSpace → m99A03ExampleSpace) ∧
      IsCompactOperator
        ((A ^ 3 : m99A03ExampleSpace →L[ℂ] m99A03ExampleSpace) :
          m99A03ExampleSpace → m99A03ExampleSpace)) := by
```

### Declared repairs
Part (b) cannot hold on every Hilbert space (for example, not on a
finite-dimensional one).  Its instruction to “construct an example” is encoded
as an existential example on the explicit infinite-dimensional Hilbert space
`m99A03ExampleSpace`.
-/

noncomputable section

abbrev m99A03L2 := lp (fun _ : ℕ => ℂ) 2
abbrev m99A03ExampleSpace := m99A03L2 × m99A03L2 × m99A03L2

theorem apm_m99a03 :
    (∀ (H : Type*) [NormedAddCommGroup H] [InnerProductSpace ℂ H]
      [CompleteSpace H] (A : H →L[ℂ] H),
      IsCompactOperator A → IsStarNormal A → A ^ 3 = 0 → A = 0) ∧
    (∃ A : m99A03ExampleSpace →L

## m99J03 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:fun,fun,fun,nhds', 'conclusion-trivially-short']

### Source TeX
```tex
Each of the given functions $g_n(x)$ below is identified with the
linear functional $\ell_{g_n}\in \mathcal{D}'(\R)$ where
$\ell_{g_n}(\varphi)=\int_{-\infty}^{\infty}g_n(x)\varphi(x)dx$.
Determine which sequences $\ell_{g_n}$ have limits in
$\mathcal{D}'(\R)$ and compute the limit when the sequence converges.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item $g_n(x)=0$ for $|x|>1/n$ and $g_n(x)=n(1-n|x|)$ for $x\leq 1/n$.
%------------------------------------------------------------------*
\item $g_n(x)=n^2$ for $-1/n<x<0$,  $g_n(x)=-n^2$ for $0\leq x\leq 1/n$,
and $g_n(x)=0$ elsewhere.
%------------------------------------------------------------------*
\item $g_n(x)=\psi(x/n)$ where $\psi\in C^{\infty}(\R)$.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_m99j03
    (ψ : ℝ → ℝ) (hψ : ContDiff ℝ (⊤ : ℕ∞) ψ) :
    (∀ φ : ℝ → ℝ, apm_m99j03_IsTestFunction φ →
      Filter.Tendsto
        (fun n : ℕ => ∫ x : ℝ, apm_m99j03_triangle n x * φ x)
        Filter.atTop (nhds (φ 0))) ∧
    (∀ φ : ℝ → ℝ, apm_m99j03_IsTestFunction φ →
      Filter.Tendsto
        (fun n : ℕ => ∫ x : ℝ, apm_m99j03_dipole n x * φ x)
        Filter.atTop (nhds (-deriv φ 0))) ∧
    ∀ φ : ℝ → ℝ, apm_m99j03_IsTestFunction φ →
      Filter.Tendsto
        (fun n : ℕ => ∫ x : ℝ, ψ (x / (n : ℝ)) * φ x)
        Filter.atTop (nhds (ψ 0 * ∫ x : ℝ, φ x)) := by
```

### Declared repairs
In part (a), the second condition `x ≤ 1/n` is repaired to `|x| ≤ 1/n`.
As printed, it overlaps the first condition whenever `x < -1/n`; the symmetric
absolute-value condition is the unique reading consistent with the stated piecewise
function and the supplied solution.

## t00A02 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:F,p,p']

### Source TeX
```tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Let $F$ be a closed orientable surface of genus $5$. Prove that
all $10$-fold coverings of $F$ are homeomorphic.
%------------------------------------------------------------------*
\item Let $\RP^2$ denote the projective plane.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{rlist}
%------------------------------------------------------------------*
\item Find a double covering of $\RP^2$.
%------------------------------------------------------------------*
\item Let $X=\RP^2\vee \RP^2$, \ie\ $X$ is the union of two projective
planes that meet at exactly one point. Draw both a regular and
irregular $4$-fold covering of $X$ clearly indicating why your diagrams
are coverings.
%------------------------------------------------------------------*
\end{rlist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_t00a02 :
    (∀ (F E₁ E₂ : Type)
        [TopologicalSpace F] [T2Space F] [CompactSpace F]
        [PathConnectedSpace F] [ChartedSpace apm_t00A02_E2 F]
        [IsManifold apm_t00A02_I2 ⊤ F]
        [TopologicalSpace E₁] [PathConnectedSpace E₁]
        [TopologicalSpace E₂] [PathConnectedSpace E₂]
        (hF : apm_t00A02_hasGenusFiveHomology F)
        (p₁ : E₁ → F) (p₂ : E₂ → F),
      apm_t00A02_isNfoldCover 10 p₁ →
      apm_t00A02_isNfoldCover 10 p₂ → Nonempty (E₁ ≃ₜ E₂)) ∧
    apm_t00A02_isNfoldCover 2 apm_t00A02_antipodalCover ∧
    (∀ r₀ : apm_t00A02_RP2,
      Nonempty (apm_t00A02_coverWitness (apm_t00A02_wedge r₀) 4 true) ∧
      Nonempty (apm_t00A02_coverWitness (apm_t00A02_wedge r₀) 4 false)) := by
```

### Declared repairs
Part (a) must concern connected `10`-fold coverings. Without connectedness,
disjoint unions with total degree `10` give nonhomeomorphic covering spaces.
We add the standard connectedness hypothesis, as does the supplied solution.

## t00J08 — gate: pending-review
**Lint flags (advisory):** ['conclusion-trivially-short']

### Source TeX
```tex
Let $F$ be a free group on $n$ letters. Suppose $F$ contains a
subgroup $G$ of finite index where $G$ contains $7$ free generators.
What integers can $n$ be? Prove your answer. Do not use any facts
about free groups without justification.
```

### Lean statement
```lean
theorem apm_t00j08 :
    ∀ n : ℕ,
      (∃ G : Subgroup (FreeGroup (Fin n)),
        G.FiniteIndex ∧ Nonempty (G ≃* FreeGroup (Fin 7))) ↔
      n ∈ ({2, 3, 4, 7} : Set ℕ) := by
```

### Declared repairs
None.

## t01A01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:x']

### Source TeX
```tex
Let $f:\RP^2\rightarrow \RP^2$ be a continuous map between real
projective planes which is not surjective.  Show that
$f_*:\pi_1(\RP^2)\rightarrow \pi_1(\RP^2)$ is the map with image
$\{1\}$. (Hint: What does $\RP \setminus \mathrm{point}$ look like?)
Show that $f$ is homotopic to a constant map, \ie\ to a map whose
image is a single point.
```

### Lean statement
```lean
theorem apm_t01a01 :
    ∀ f : C(apm_t01A01_RP2, apm_t01A01_RP2),
      ¬Function.Surjective f →
      (∀ (x : apm_t01A01_RP2) (γ : FundamentalGroup apm_t01A01_RP2 x),
        FundamentalGroup.map f x γ = 1) ∧
      ∃ p : apm_t01A01_RP2,
        ContinuousMap.Homotopic f (ContinuousMap.const _ p) := by
```

### Declared repairs
None.

## t01A02 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:G']

### Source TeX
```tex
Let $T$ be the $2$-torus and $f:T\rightarrow T$ a continuous map which
is not surjective.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Show that the induced map $f_*:H_2(T)\rightarrow H_2(T)$ is the
zero map.
%------------------------------------------------------------------*
\item Show that $T$ has a covering space $S\rightarrow T$ 
with covering group $\Z/2\Z\oplus\Z/2\Z$.
%------------------------------------------------------------------*
\item Let $f$ be as in (a) and in addition assume that $f$ factors  
as a composition of maps $T\rightarrow S\rightarrow T$. Show that $f$
has a fixed point.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_t01a02 :
    ∀ f : C(apm_t01A02_Torus, apm_t01A02_Torus),
      ¬Function.Surjective f →
      (∀ a : apm_t01A02_H2 apm_t01A02_Torus,
        apm_t01A02_H2map f a = 0) ∧
      apm_t01A02_hasCoveringGroup
        (G := apm_t01A02_Deck) apm_t01A02_cover ∧
      ((∃ g : C(apm_t01A02_Torus, apm_t01A02_Torus),
          ∀ x, f x = apm_t01A02_cover (g x)) →
        ∃ x, f x = x) := by
```

### Declared repairs
None.

## t01J03 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:z,e,e,c']

### Source TeX
```tex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Let $\Ttw$ be a torus. Compute $\pi_1(\Ttw)$ and $H_i(\Ttw),
i\geq 0$.
%------------------------------------------------------------------*
\item Let $X_1$ and $X_2$ be copies of $\Son\times D^2$ and let
$h:\partial X_1\rightarrow \partial X_2$ be a homeomorphism such that
for some $x_0\in \Son$ we have that $h|{x_0\times \partial D^2}$ is a
curve that spirals twice around the close face of the torus, then
loops around behind the back and rejoins itself again. Let $M=X_1
\cup_h X_2$.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{rlist}
%------------------------------------------------------------------*
\item Use van Kampen's Theorem to compute $\pi_1(M)$.
%------------------------------------------------------------------*
\item Use the Mayer-Vietoris sequence to compute $H_i(M)$ for $i\geq
0$.
%------------------------------------------------------------------*
\end{rlist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

### Lean statement
```lean
theorem apm_t01j03 :
    Nonempty
      (FundamentalGroup apm_t01J03_Torus (1, 1) ≃*
        Multiplicative (ℤ × ℤ)) ∧
    Nonempty (apm_t01J03_H 0 apm_t01J03_Torus ≅ ModuleCat.of ℤ ℤ) ∧
    Nonempty (apm_t01J03_H 1 apm_t01J03_Torus ≅
      ModuleCat.of ℤ (Fin 2 → ℤ)) ∧
    Nonempty (apm_t01J03_H 2 apm_t01J03_Torus ≅ ModuleCat.of ℤ ℤ) ∧
    (∀ k : ℕ, 3 ≤ k → Limits.IsZero (apm_t01J03_H k apm_t01J03_Torus)) ∧
    (∀ (h : apm_t01J03_BoundaryTorus ≃ₜ apm_t01J03_BoundaryTorus)
        (z₀ : apm_t01J03_BoundaryTorus)
        (e₁ : FundamentalGroup apm_t01J03_BoundaryTorus z₀ ≃*
          Multiplicative (ℤ × ℤ))
        (e₂ : FundamentalGroup apm_t01J03_BoundaryTorus (h z₀) ≃*
          Multiplicative (ℤ × ℤ))
        (c₁ : FundamentalGroup apm_t01J03_SolidTorus
          (apm_t01J03_boundaryInclusion z₀) ≃* Multiplicative ℤ)
        (c₂ : FundamentalGroup apm_t01J03_SolidTorus
          (apm_t01J03_boundaryInclusion (h z₀)) ≃* Multiplicative ℤ),
      apm_t01J03_hasTwiceSpiral h z₀ e₁ e₂ c₁ c₂ →
      let M := apm_t01J03_gluedSpace h
      let m₀ := apm_t01J03_gluedBasepoint h z₀
      Nonempty (FundamentalGroup M m₀ ≃* Multiplicative (ZMod 2)) ∧
      Nonempty (apm_t01J03_H 0 M ≅ ModuleCat.of ℤ ℤ) ∧
      Nonempty (apm_t01J03_H 1 M ≅ ModuleCat.of ℤ (ZMod 2)) ∧
      Limits.IsZero (apm_t01J03_H 2 M) ∧
      Nonempty (apm_t01J03_H 3 M ≅ ModuleCat.of ℤ ℤ) ∧
      ∀ k : ℕ, 4 ≤ k → Limits.IsZero (apm_t01J03_H k M)) := by
```

### Declared repairs
None.

## t03J01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:X,J']

### Source TeX
```tex
Let $X$ be a double torus. Let $J_1$ be a curve that cuts $X$ into a
twice-punctured sphere and let $J_2$ be a curve that cuts $X$ into two
once-punctured tori.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Does $X$ retract to $J_1$? Prove your answer.
%------------------------------------------------------------------*
\item Does $X$ retract to $J_2$? Prove your answer.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\soln{
(retract: fact: homomorphism induced by inclusion of a retract is injective)  \defn{
\item $(X,t)$ topological space
\item $Y\subset X$ subspace
\item $r:X\rightarrow Y$ retraction
\item $j_*:\pi_1(Y)\rightarrow \pi_1(X)$ homomorphism induced by inclusion
\item $j_*$ injective}
\proof{
\item $r|_Y\circ j$ identity map & retraction:4
\item $y\in Y$ & let
\item $(r|_Y)_*\circ j_*$ identity map of $\pi_1((Y,y))$ & homomorphism induced by identity map is identity map
\item $j_*$ injective & if the identity map factors into two maps, both must be injective}
I don't know if that fact is particularly useful in this case, however, because it seems like each homotopy class in $\pi_1(\Son)$
is going to be mapped to some distinct homotopy class in $\Ttw\#\Ttw$. Well this should be at least true for the curve that cuts $\Ttw\#\Ttw$ into a twice-punctured sphere, since that curve has got to be a generator for the fundamental group of $\Ttw\#\Ttw$. I guess I should work out what exactly the fundamental group of $\Ttw\#\Ttw$ is, that could make things go a bit more easily
(induced homomorphism of fundamental groups) \defn{ % this can be trivially rephrased in terms of pointed topological spaces; they are used anyway.  We can sum up what the induced homomorphism of fundamental groups does as follows: take a representative of the homology class in the first fundamental group, and look at the homology class of the image under the given map of that representative in the second fundamental group.
\item $X$ topological manifold
\item $Y$ topological manifold
\item $h:X\rightarrow Y$ continuous
\item $x_0\in X$
\item $h(x_0)=y_0$
\item $h_*:\pi_1((X,x_0))\rightarrow \pi_1((Y,y_0))$
\item $\forall [f]\in\pi_1((X,x_0)): h_*([f])=[h\circ f]$}
}
```

### Lean statement
```lean
theorem apm_t03j01
    (X : Type*) [TopologicalSpace X] [T2Space X] [CompactSpace X]
    (hX : apm_t03j01_IsDoubleTorus X)
    (J₁ J₂ : Set X)
    (hcurve₁ : apm_t03j01_IsCurve J₁)
    (hcurve₂ : apm_t03j01_IsCurve J₂)
    (hcut₁ : apm_t03j01_CutsIntoTwicePuncturedSphere J₁)
    (hcut₂ : apm_t03j01_CutsIntoTwoOncePuncturedTori J₂) :
    apm_t03j01_RetractsOnto J₁ ∧
      ¬apm_t03j01_RetractsOnto J₂ := by
```

### Declared repairs
None.

## t91J06 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:f,univ']

### Source TeX
```tex
Let $I$ denote the unit interval and let $X$ be a topological
space. Suppose there is a sequence of embeddings $f_n:I\rightarrow X$
such that
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item $f_n(I)\subset f_{n+1}(I^{\circ})$ for each $n$;
%------------------------------------------------------------------*
\item $X=\bigcup f_n(I)$;
%------------------------------------------------------------------*
\item A subset of $X$ is closed whenever its intersection with each
$f_n(I)$ is closed.
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Prove that $X$ is homeomorphic to a certain familiar space.

\soln{ Maybe this has something to do with $\R$. I can imagine a sequence of
embeddings in $\R$ that have the first and second properties. Of
course, I can also imagine a sequence of embeddings in $\Son$ with
the first and second properties. The real interesting property is the
third property. Which sounds like a property that has some official
name or other.}
```

### Lean statement
```lean
theorem apm_t91j06
    {X : Type*} [TopologicalSpace X]
    (f : ℕ → apm_t91J06_I → X)
    (hf : ∀ n, Topology.IsEmbedding (f n))
    (hnested : ∀ n,
      f n '' (univ : Set apm_t91J06_I) ⊆ f (n + 1) '' apm_t91J06_Iinterior)
    (hcover : (⋃ n, f n '' (univ : Set apm_t91J06_I)) = (univ : Set X))
    (hweak : ∀ C : Set X, (∀ n, IsClosed (f n ⁻¹' C)) → IsClosed C) :
    Nonempty (X ≃ₜ ℝ) := by
```

### Declared repairs
None.

## t95J01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-trivially-short']

### Source TeX
```tex
Let $\pi:X\rightarrow Y$ be a continuous surjection. Suppose that
$\pi^{-1}$ is compact for all $y\in Y$ and that $\pi$ is closed.
Show that if $X$ is Hausdorff, then so is $Y$.
```

### Lean statement
```lean
theorem apm_t95j01
    {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y] [T2Space X]
    (π : X → Y) (hcont : Continuous π) (hsurj : Function.Surjective π)
    (hclosed : IsClosedMap π)
    (hcompact : ∀ y : Y, IsCompact (π ⁻¹' ({y} : Set Y))) :
    T2Space Y := by
```

### Declared repairs
The phrase “`π⁻¹` is compact for all `y`” is read as “the fiber
`π⁻¹({y})` is compact for every `y`”; an inverse map need not exist.
-/

open Set

theorem apm_t95j01
    {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y] [T2Space X]
    (π : X → Y) (hcont : Continuous π) (hsurj : Function.Surjective π)
    (hclosed : IsClosedMap π)
    (hcompact : ∀ y : Y, IsCompact (π ⁻¹' ({y} : Set Y))) :
    T2Space Y := by
  sorry

## t97A01 — gate: pending-review
**Lint flags (advisory):** ['conclusion-ignores-objects:apm_t97a01_piOne']

### Source TeX
```tex
Let $X$ be the connected sum of three real projective planes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{alist}
%------------------------------------------------------------------*
\item Describe a CW decomposition of $X$.
%------------------------------------------------------------------*
\item Use that CW decomposition to compute the homology groups in all
dimensions.
%------------------------------------------------------------------*
\item Now consider $X$ to be the connected sum of the torus and the
projective plane.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{rlist}
%------------------------------------------------------------------*
\item Using this decomposition, compute all the homology groups of $X$
using the Mayer-Vietoris theorem.
%------------------------------------------------------------------*
\item Using this decomposition, give a presentation of the fundamental
group of $X$ using Van Kampen's theorem.
%------------------------------------------------------------------*
\end{rlist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------------------------------------------*
\end{alist}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\soln{jac -- Mon Aug 4 15:36:43 2003 -- A CW decomposition: look at
two projective planes $P_1$, $P_2$, each with one hole in them, and
one projective plane $P_3$ with two holes in it. The single-holed
planes are decomposed into two faces with four sides each. The
two-holed plane has two faces with six sides each.  These sides have
some identifications, described as follows: if $P_{11}$, $P_{12}$,
$P_{21}$, $P_{22}$, $P_{31}$, and $P_{32}$ are the distinct faces,
then $P_{11a}$, $P_{11b}$, $P_{11c}$, $P_{11d}$ are the edges of the
first face, then $P_{12a}$, $P_{12b}$, $P_{11c}$, $P_{11e}$ are the
edges of the second face.}
```

### Lean statement
```lean
theorem apm_t97a01 :
    (Nat.card (Fin 1) = 1 ∧ Nat.card (Fin 3) = 3 ∧ Nat.card (Fin 1) = 1) ∧
    Function.Injective apm_t97a01_boundary₂ ∧
    Nonempty (((Fin 3 → ℤ) ⧸ AddSubgroup.closure {apm_t97a01_boundary₂ 1}) ≃+
      (ℤ × ℤ × ZMod 2)) ∧
    Nonempty (apm_t97a01_piOne ≃*
      PresentedGroup ({apm_t97a01_relator} : Set (FreeGroup (Fin 3)))) := by
```

### Declared repairs
None.