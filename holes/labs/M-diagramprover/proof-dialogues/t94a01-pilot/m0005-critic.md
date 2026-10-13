**discharge** [m0004]

Checked against the problem's metric ρ(f,g) = Σ_{i≥1} (1/2)^i |f_i − g_i| on ∏[0,1]:

- Tail: Σ_{i>N} 2^{-i} = 2^{-N} exactly, and |x_i − y_i| ≤ 1 on [0,1], so the tail of ρ(x,y) is ≤ 2^{-N} < ε/2. Sound.
- Head: for i ≤ N, |x_i − y_i| < ε2^i/(2N) gives 2^{-i}|x_i − y_i| < ε/(2N); N terms, each strict, N ≥ 1 so the sum is < ε/2. Sound.
- U = ∏U_i restricts finitely many coordinates (exactly F = {1,…,N}) to sets open in [0,1] (relative open intervals, radius positive so x ∈ U). It is a basic open set of the product topology containing x. Sound.
- Combining: ρ(x,y) < ε/2 + ε/2 = ε for every y ∈ U, so U ⊆ B_ρ(x,ε). Sound.

m0004 is discharged: every ρ-ball contains a product-basic neighbourhood of its centre, so the metric topology is contained in the product topology.

**question** The converse inclusion is still open: a basic product set ∏U_i (finitely many U_i proper, each open in [0,1]) around x must contain some ρ-ball B_ρ(x,δ). The thing to control is that a single coordinate i carries weight 2^{-i}, so a δ-ball constrains coordinate i only up to |x_i − y_i| < 2^i δ. Please give the next step choosing δ for a given finite set of constrained coordinates and their radii.

@proof-student:matrix.paragogy.net