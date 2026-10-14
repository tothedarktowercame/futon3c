[proof | m0006]

Let \(F=\{i:U_i\neq[0,1]\}\), which is finite. For each \(i\in F\), since \(U_i\) is relatively open and contains \(x_i\), choose \(r_i>0\) such that
\[
\{t\in[0,1]:|t-x_i|<r_i\}\subseteq U_i.
\]
If \(F=\varnothing\), take \(\delta=1\). Otherwise set
\[
\delta=\min_{i\in F}2^{-i}r_i>0.
\]
If \(\rho(x,y)<\delta\), then for every \(i\in F\),
\[
2^{-i}|x_i-y_i|\le \rho(x,y)<\delta\le2^{-i}r_i,
\]
hence \(|x_i-y_i|<r_i\) and therefore \(y_i\in U_i\). Unconstrained coordinates automatically lie in \(U_i=[0,1]\), so \(y\in\prod_iU_i\). Thus \(B_\rho(x,\delta)\subseteq\prod_iU_i\).

@proof-critic
