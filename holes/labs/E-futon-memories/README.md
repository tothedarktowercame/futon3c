# S1 git-history topology

This is the preregistered S1 experiment from `E-futon-memories.md`, against
futon3c history pinned at `d722f772ede949719948aec76839d4d5e83586b0`.

The corpus signal is `git-history-topology`.  The outputs are ordinary JSON,
not memory-use or outcome receipts, so they cannot pool with APM or WM memory
evidence.  Each file, subsystem, temporal window, reference target, or
qualifying co-change pair is one incidence hyperedge.  No clique expansion is
performed.

Run:

```sh
python3 -m pip install -r holes/labs/E-futon-memories/requirements.txt
python3 -m unittest holes/labs/E-futon-memories/test_s1_topology.py
python3 holes/labs/E-futon-memories/s1_topology.py --nulls 200
```

The configuration-model null uses bipartite double-edge swaps, preserving both
commit degrees and hyperedge degrees exactly.  Python 3, NumPy and SciPy
versions are recorded in `s1-results.json`.
