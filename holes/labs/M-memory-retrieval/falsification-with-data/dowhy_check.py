#!/usr/bin/env python3
import json
from pathlib import Path

import networkx as nx
import numpy as np
import pandas as pd
from dowhy.gcm.falsify import falsify_graph

HERE = Path("holes/labs/M-memory-retrieval/falsification-with-data")
engine = json.loads((HERE / "engine.json").read_text())
data = pd.read_csv(HERE / "data.csv").drop(columns=["job-id"])

graph = nx.DiGraph()
graph.add_nodes_from(engine["projection"]["nodes"])
graph.add_edges_from(engine["projection"]["directed"])
if engine["projection"]["bidirected"]:
    raise RuntimeError("DoWhy GCM cannot honestly encode projected bidirected arcs")

constant = sorted(c for c in data.columns if data[c].nunique(dropna=True) < 2)
missing = sorted(c for c in data.columns if data[c].isna().any())
complete = data.dropna()
np.random.seed(20260803)
evaluation = falsify_graph(graph, complete, n_permutations=20,
                           show_progress_bar=False, n_jobs=1)
result = {
    "dowhy_version": __import__("dowhy").__version__,
    "permutations_requested": 20,
    "random_seed": 20260803,
    "status": "tested-complete-case",
    "complete_case_n": len(complete),
    "constant_columns": constant,
    "missing_columns": missing,
    "summary": str(evaluation),
}

(HERE / "dowhy-results.json").write_text(
    json.dumps(result, indent=2, sort_keys=True) + "\n"
)
