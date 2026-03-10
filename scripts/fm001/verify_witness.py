#!/usr/bin/env python3
"""Verify a Ramsey book graph witness.

Given n and an adjacency upper-triangle bitstring, verify that the graph on
4n-2 vertices is B_{n-1}-free and its complement is B_n-free.

Usage:
    python verify_witness.py <n> <bitstring>
    python verify_witness.py <n> --file <path>

Example (n=4, Wesley QR/NR on F_7):
    python verify_witness.py 4 01001011001001010110100101100100110011010010011

Exit code 0 = valid witness, 1 = invalid.
"""

import sys
import numpy as np


def parse_args():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(2)
    n = int(sys.argv[1])
    if sys.argv[2] == "--file":
        with open(sys.argv[3]) as f:
            bits = f.read().strip()
    else:
        bits = sys.argv[2].strip()
    return n, bits


def bitstring_to_adj(V, bits):
    """Convert upper-triangle bitstring to adjacency matrix."""
    expected = V * (V - 1) // 2
    if len(bits) != expected:
        print(f"FAIL: expected {expected} bits for {V} vertices, got {len(bits)}")
        sys.exit(1)
    A = np.zeros((V, V), dtype=np.int8)
    idx = 0
    for i in range(V):
        for j in range(i + 1, V):
            if bits[idx] == "1":
                A[i][j] = 1
                A[j][i] = 1
            idx += 1
    return A


def max_book(A):
    """Find the largest k such that B_k (= K_2 + K_k) exists in the graph.

    B_k means an edge (u,v) sharing k common neighbors.
    Uses matrix multiply: (A @ A)[i][j] = common neighbors of i and j.
    """
    CN = A @ A
    # Only count over edges
    return int((CN * A).max())


def find_worst_edge(A):
    """Return the edge (u,v) with the most common neighbors."""
    CN = A @ A
    edge_cn = CN * A
    idx = np.unravel_index(edge_cn.argmax(), edge_cn.shape)
    return idx[0], idx[1], int(edge_cn[idx])


def verify(n, bits):
    V = 4 * n - 2
    print(f"n={n}, V={V}, R(B_{{{n-1}}},B_{{{n}}}) conjectured = {4*n-1}")
    print(f"Bitstring length: {len(bits)}")

    A = bitstring_to_adj(V, bits)

    deg = A.sum(axis=1)
    print(f"Degree range: {int(deg.min())}-{int(deg.max())}, mean={deg.mean():.1f}")

    # Check B_{n-1}-free
    bg = max_book(A)
    print(f"Graph: max book = B_{bg} (need < {n-1} for B_{{{n-1}}}-free)")
    if bg >= n - 1:
        u, v, cn = find_worst_edge(A)
        print(f"  FAIL: edge ({u},{v}) has {cn} common neighbors (limit {n-2})")
        return False

    # Check complement B_n-free
    comp = 1 - A
    np.fill_diagonal(comp, 0)
    bc = max_book(comp)
    print(f"Complement: max book = B_{bc} (need < {n} for B_{{{n}}}-free)")
    if bc >= n:
        comp_cn = comp @ comp
        edge_cn = comp_cn * comp
        idx = np.unravel_index(edge_cn.argmax(), edge_cn.shape)
        u, v = idx[0], idx[1]
        cn = int(edge_cn[idx])
        print(f"  FAIL: complement edge ({u},{v}) has {cn} common neighbors (limit {n-1})")
        return False

    print(f"PASS: valid witness for R(B_{{{n-1}}},B_{{{n}}}) >= {V+1}")
    return True


if __name__ == "__main__":
    n, bits = parse_args()
    ok = verify(n, bits)
    sys.exit(0 if ok else 1)
