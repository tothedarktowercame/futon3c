#!/usr/bin/env python3
"""FM-001 T3: General witness generation for R(B_{n-1}, B_n) = 4n-1.

Given n, constructs the Wesley 2-block-circulant graph on 4n-2 vertices
when q = 2n-1 is a prime power with q ≡ 1 (mod 4).

Construction: G = Γ_{F_q}(Q, Q, N) where Q = quadratic residues, N = non-residues.
- V1, V2 are two copies of F_q (vertices 0..q-1 and q..2q-1)
- Within V1: edge iff difference ∈ Q
- Within V2: edge iff difference ∈ N
- Between V1-V2: edge iff difference ∈ Q

Output: binary adjacency string (column-major, lower triangle) as required by
the FrontierMath specification.

Usage:
    python generate_witness.py <n>
    python generate_witness.py --all          # all n <= 100 where q is prime power
    python generate_witness.py --check <n>    # generate and verify
"""

import sys
import math
import time


def is_prime(n):
    """Miller-Rabin primality test for n < 3.3e24 (deterministic for small n)."""
    if n < 2:
        return False
    if n < 4:
        return True
    if n % 2 == 0:
        return False
    # Deterministic witnesses for n < 3.3e24
    d, r = n - 1, 0
    while d % 2 == 0:
        d //= 2
        r += 1
    for a in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]:
        if a >= n:
            continue
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            continue
        for _ in range(r - 1):
            x = pow(x, 2, n)
            if x == n - 1:
                break
        else:
            return False
    return True


def prime_power_decompose(q):
    """If q is a prime power p^k, return (p, k). Otherwise return None."""
    if q < 2:
        return None
    if is_prime(q):
        return (q, 1)
    # Check small primes
    for p in range(2, int(math.isqrt(q)) + 1):
        if not is_prime(p):
            continue
        if q % p != 0:
            continue
        k = 0
        val = q
        while val % p == 0:
            val //= p
            k += 1
        if val == 1:
            return (p, k)
    return None


class GF:
    """Finite field GF(p^k) arithmetic.

    For k=1: integers mod p.
    For k>1: polynomials over F_p modulo an irreducible polynomial of degree k.
    Elements are represented as tuples of coefficients (a0, a1, ..., a_{k-1}).
    """

    def __init__(self, p, k):
        self.p = p
        self.k = k
        self.q = p ** k
        if k == 1:
            self.elements = list(range(p))
            self.zero = 0
            self.one = 1
        else:
            self.irr = self._find_irreducible(p, k)
            self.elements = self._enumerate_elements()
            self.zero = tuple([0] * k)
            self.one = tuple([1] + [0] * (k - 1))

    def _find_irreducible(self, p, k):
        """Find an irreducible polynomial of degree k over F_p.
        Returns coefficients [a0, a1, ..., a_k] where poly = sum a_i x^i."""
        from itertools import product as cartprod
        for coeffs in cartprod(range(p), repeat=k):
            poly = list(coeffs) + [1]  # monic
            if self._is_irreducible(poly, p):
                return poly
        raise ValueError(f"No irreducible polynomial of degree {k} over F_{p}")

    def _is_irreducible(self, poly, p):
        """Test if polynomial is irreducible over F_p."""
        k = len(poly) - 1
        # poly must be degree k (monic)
        if poly[k] != 1:
            return False
        # x^{p^i} mod poly for i = 1..k/2; gcd with x^{p^i} - x must be 1
        # Simplified: just check no roots and no factors
        # For small fields, check no roots first
        for a in range(p):
            val = 0
            for i in range(len(poly)):
                val = (val + poly[i] * pow(a, i, p)) % p
            if val == 0:
                return False
        if k <= 1:
            return True
        # For degree > 1, use the formal criterion: x^{p^k} ≡ x mod f(x)
        # and gcd(x^{p^i} - x, f(x)) = 1 for 1 <= i < k
        # Implement polynomial arithmetic mod p
        def poly_mod(a, b):
            """a mod b, polynomials as lists, coeffs mod p."""
            a = list(a)
            while len(a) >= len(b):
                if a[-1] != 0:
                    c = a[-1] * pow(b[-1], p - 2, p) % p
                    for i in range(len(b)):
                        a[len(a) - len(b) + i] = (a[len(a) - len(b) + i] - c * b[i]) % p
                a.pop()
            while a and a[-1] == 0:
                a.pop()
            return a if a else [0]

        def poly_mul_mod(a, b, m):
            """(a * b) mod m, coeffs mod p."""
            result = [0] * (len(a) + len(b) - 1)
            for i, ai in enumerate(a):
                for j, bj in enumerate(b):
                    result[i + j] = (result[i + j] + ai * bj) % p
            return poly_mod(result, m)

        def poly_pow_mod(base, exp, m):
            """base^exp mod m."""
            result = [1]
            base = poly_mod(base, m)
            while exp > 0:
                if exp % 2 == 1:
                    result = poly_mul_mod(result, base, m)
                base = poly_mul_mod(base, base, m)
                exp //= 2
            return result

        def poly_gcd(a, b):
            """gcd of polynomials mod p."""
            while b and b != [0]:
                a, b = b, poly_mod(a, b)
            # normalize
            if a and a[-1] != 0:
                inv = pow(a[-1], p - 2, p)
                a = [(c * inv) % p for c in a]
            return a

        x = [0, 1]  # the polynomial x
        for i in range(1, k):
            xpi = poly_pow_mod(x, p ** i, poly)
            diff = list(xpi)
            while len(diff) < 2:
                diff.append(0)
            diff[1] = (diff[1] - 1) % p
            g = poly_gcd(diff, poly)
            if len(g) > 1:  # gcd has degree >= 1
                return False
        # Final check: x^{p^k} ≡ x mod poly
        xpk = poly_pow_mod(x, p ** k, poly)
        diff = list(xpk)
        while len(diff) < 2:
            diff.append(0)
        diff[1] = (diff[1] - 1) % p
        remainder = poly_mod(diff, poly)
        return all(c == 0 for c in remainder)

    def _enumerate_elements(self):
        """Enumerate all elements of GF(p^k) as tuples."""
        from itertools import product as cartprod
        return [tuple(c) for c in cartprod(range(self.p), repeat=self.k)]

    def add(self, a, b):
        if self.k == 1:
            return (a + b) % self.p
        return tuple((ai + bi) % self.p for ai, bi in zip(a, b))

    def neg(self, a):
        if self.k == 1:
            return (-a) % self.p
        return tuple((-ai) % self.p for ai in a)

    def sub(self, a, b):
        return self.add(a, self.neg(b))

    def mul(self, a, b):
        if self.k == 1:
            return (a * b) % self.p
        p, k = self.p, self.k
        result = [0] * (2 * k - 1)
        for i in range(k):
            for j in range(k):
                result[i + j] = (result[i + j] + a[i] * b[j]) % p
        # Reduce mod irreducible polynomial
        for i in range(2 * k - 2, k - 1, -1):
            if result[i] != 0:
                c = result[i]
                for j in range(k + 1):
                    result[i - k + j] = (result[i - k + j] - c * self.irr[j]) % p
        return tuple(result[:k])

    def is_zero(self, a):
        if self.k == 1:
            return a == 0
        return all(c == 0 for c in a)

    def is_qr(self, a):
        """Test if a is a nonzero quadratic residue in F_q*."""
        if self.is_zero(a):
            return False
        # a is QR iff a^{(q-1)/2} = 1
        exp = (self.q - 1) // 2
        result = self._pow(a, exp)
        return result == self.one

    def is_nr(self, a):
        """Test if a is a non-residue in F_q*."""
        if self.is_zero(a):
            return False
        return not self.is_qr(a)

    def _pow(self, base, exp):
        result = self.one
        while exp > 0:
            if exp % 2 == 1:
                result = self.mul(result, base)
            base = self.mul(base, base)
            exp //= 2
        return result


def generate_witness(n, verify=False):
    """Generate the Wesley 2-block-circulant witness for R(B_{n-1}, B_n) = 4n-1.

    Returns (adj_string, stats) or (None, reason) if q is not a valid prime power.

    Requires q ≡ 1 (mod 4) so that -1 ∈ Q and Q is symmetric (Q = -Q),
    giving undirected within-block adjacency. Uses Γ(Q, Q, N).

    For q ≡ 3 (mod 4), Q is antisymmetric (Q = -N) and within-block
    adjacency using Q symmetrizes to the complete graph. These cases
    require a different construction (SAT/IP search or non-circulant
    methods — see FM-001b).
    """
    q = 2 * n - 1

    # Check q ≡ 1 (mod 4)
    if q % 4 != 1:
        return None, f"q={q} ≡ {q%4} (mod 4), need q ≡ 1 (mod 4) for Wesley construction"

    # Check q is prime power
    decomp = prime_power_decompose(q)
    if decomp is None:
        return None, f"q={q} is not a prime power"

    p, k = decomp
    num_vertices = 2 * q  # = 4n - 2

    t0 = time.time()
    field = GF(p, k)

    # Precompute QR/NR lookup
    qr_set = set()
    nr_set = set()
    elem_list = field.elements
    for a in elem_list:
        if field.is_zero(a):
            continue
        if field.is_qr(a):
            qr_set.add(a)
        else:
            nr_set.add(a)

    neg_one = field.neg(field.one)
    q_mod4 = q % 4

    # -1 ∈ Q for q ≡ 1 mod 4, ensuring Q is symmetric (Q = -Q)
    assert neg_one in qr_set, f"-1 is not a QR in GF({q})"
    d11, d12, d22 = qr_set, qr_set, nr_set
    variant = "Q,Q,N"

    # Build adjacency
    # Vertices: 0..q-1 are V1, q..2q-1 are V2
    def has_edge(i, j):
        if i == j:
            return False
        block_i = 0 if i < q else 1
        block_j = 0 if j < q else 1
        ei = elem_list[i % q]
        ej = elem_list[j % q]
        diff = field.sub(ej, ei)
        if field.is_zero(diff):
            return False
        if block_i == 0 and block_j == 0:
            return diff in d11
        elif block_i == 1 and block_j == 1:
            return diff in d22
        else:
            return diff in d12

    t1 = time.time()

    # Generate adjacency string: column-major, lower triangle
    # For column j, rows i > j: bit = adj[i][j]
    bits = []
    max_cn_g = 0
    max_cn_comp = 0

    if verify:
        # Build full neighbor lists for verification
        neighbors = [set() for _ in range(num_vertices)]
        for i in range(num_vertices):
            for j in range(i + 1, num_vertices):
                if has_edge(i, j):
                    neighbors[i].add(j)
                    neighbors[j].add(i)

        # Check max common neighbors
        for i in range(num_vertices):
            for j in neighbors[i]:
                if j > i:
                    cn = len(neighbors[i] & neighbors[j])
                    max_cn_g = max(max_cn_g, cn)

        # Check complement
        for i in range(num_vertices):
            comp_i = set(range(num_vertices)) - neighbors[i] - {i}
            for j in comp_i:
                if j > i:
                    comp_j = set(range(num_vertices)) - neighbors[j] - {j}
                    cn = len(comp_i & comp_j)
                    max_cn_comp = max(max_cn_comp, cn)

    # Column-major lower triangle
    for j in range(num_vertices):
        for i in range(j + 1, num_vertices):
            bits.append('1' if has_edge(i, j) else '0')

    t2 = time.time()
    adj_string = ''.join(bits)

    stats = {
        'n': n,
        'q': q,
        'p': p,
        'k': k,
        'q_mod4': q_mod4,
        'variant': variant,
        'vertices': num_vertices,
        'edges': adj_string.count('1'),
        'bits': len(adj_string),
        'field_time': round(t1 - t0, 3),
        'adj_time': round(t2 - t1, 3),
        'total_time': round(t2 - t0, 3),
    }

    if verify:
        stats['max_cn_G'] = max_cn_g
        stats['max_cn_comp'] = max_cn_comp
        stats['B_{n-1}_free'] = max_cn_g < n - 1
        stats['comp_B_n_free'] = max_cn_comp < n
        stats['valid'] = stats['B_{n-1}_free'] and stats['comp_B_n_free']

    return adj_string, stats


def eligible_n_values(max_n=100):
    """Return all n <= max_n where q=2n-1 is prime power with q ≡ 1 (mod 4)."""
    results = []
    for n in range(2, max_n + 1):
        q = 2 * n - 1
        if q % 4 != 1:
            continue
        if prime_power_decompose(q) is not None:
            results.append(n)
    return results


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    if sys.argv[1] == '--all':
        max_n = int(sys.argv[2]) if len(sys.argv) > 2 else 100
        eligible = eligible_n_values(max_n)
        print(f"Eligible n values (q=2n-1 prime power, q≡1 mod 4) for n <= {max_n}:")
        print(f"  {eligible}")
        print(f"  Count: {len(eligible)} / {max_n - 1}")
        print()
        for n_val in eligible:
            adj, stats = generate_witness(n_val, verify=(n_val <= 30))
            status = ""
            if 'valid' in stats:
                status = " ✓" if stats['valid'] else " ✗ INVALID"
                status += f" (max_CN_G={stats['max_cn_G']}<{n_val-1}, max_CN_comp={stats['max_cn_comp']}<{n_val})"
            print(f"  n={n_val:3d}  q={stats['q']:3d}  GF({stats['p']}^{stats['k']})  "
                  f"{stats['vertices']} vertices  {stats['edges']} edges  "
                  f"{stats['total_time']:.3f}s{status}")
        return

    if sys.argv[1] == '--check':
        n_val = int(sys.argv[2])
        adj, stats = generate_witness(n_val, verify=True)
        if adj is None:
            print(f"n={n_val}: {stats}")
            sys.exit(1)
        print(f"n={n_val}, q={stats['q']}, GF({stats['p']}^{stats['k']})")
        print(f"  {stats['vertices']} vertices, {stats['edges']} edges")
        print(f"  max common neighbors (G): {stats['max_cn_G']} (need < {n_val-1})")
        print(f"  max common neighbors (comp): {stats['max_cn_comp']} (need < {n_val})")
        print(f"  B_{{{n_val-1}}}-free: {stats['B_{n-1}_free']}")
        print(f"  comp B_{{{n_val}}}-free: {stats['comp_B_n_free']}")
        print(f"  VALID: {stats['valid']}")
        print(f"  Time: {stats['total_time']:.3f}s")
        if '--adj' in sys.argv:
            print(f"  Adjacency string ({len(adj)} bits): {adj[:100]}...")
        sys.exit(0 if stats['valid'] else 1)

    # Single n
    n_val = int(sys.argv[1])
    adj, stats = generate_witness(n_val)
    if adj is None:
        print(f"n={n_val}: {stats}")
        sys.exit(1)
    print(f"n={n_val}, q={stats['q']}, GF({stats['p']}^{stats['k']}), "
          f"{stats['vertices']} vertices, {stats['edges']} edges, {stats['total_time']:.3f}s")
    print(adj)


if __name__ == '__main__':
    main()
