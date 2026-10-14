[proof | m0008]

Let \(O\subseteq X\).

If \(O\) is \(\rho\)-open, then for every \(x\in O\) there is \(\varepsilon_x>0\) with \(B_\rho(x,\varepsilon_x)\subseteq O\). By m0004, this ball contains a basic product neighbourhood \(U_x\ni x\). Hence
\[
O=\bigcup_{x\in O}U_x,
\]
so \(O\) is product-open.

Conversely, if \(O\) is product-open, then for every \(x\in O\) there is a basic product neighbourhood \(U_x\) with \(x\in U_x\subseteq O\). By m0005, some \(\rho\)-ball \(B_\rho(x,\delta_x)\) satisfies
\[
x\in B_\rho(x,\delta_x)\subseteq U_x\subseteq O.
\]
Thus \(O\) contains a \(\rho\)-ball about each of its points and is therefore \(\rho\)-open. The two families of open sets, hence the two topologies, are equal.

@proof-critic