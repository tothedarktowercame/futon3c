# Mission dependency graph

`build_mission_graph.py` builds a standalone SVG/HTML view of one mission's
acts. It uses the mission's existing `brief/resume_brief.py:load_records`
joiner, reads labelled `moves/1` sidecars, and reads kernel port closes from
the session-turn records. It has no third-party or graphviz dependency.

```sh
python3 -I graph/build_mission_graph.py \
  --mission M-象-2000 --join seats --out graph/out/M-象-2000.html
```

`--reduce` removes only transitively implied `scope/within` edges. Answer and
kernel edges remain because they are evidence for review. The graph marks
multi-node Tarjan SCC edges red and merges a labelled close with a kernel close
when the record and target agree.

The HTML review table defaults every edge to `unsure`. Authors can select
`right`, `wrong`, or `unsure`, add an edge by source/target/type, and download:

```json
{
  "mission": "M-象-2000",
  "generated_at": "2026-10-12T00:00:00Z",
  "graph_built_at": "2026-10-12T00:00:00Z",
  "verdicts": [
    {"edge_id":"abc123", "type":"answers/closes", "source":"act-a",
     "target":"act-b", "provenance":"labelled", "verdict":"right"}
  ],
  "added": [{"source":"act-a", "target":"act-b", "type":"scope/within"}]
}
```

`graph_built_at` is the build timestamp, while `generated_at` is the graph
generation timestamp retained in the page. A later scorer can compare kernel
and labelled provenance using the stable edge id (a hash of type/source/target)
and the verdicts without mutating the source records or sidecars.

Run tests with:

```sh
python3 -I graph/test_build_mission_graph.py
```
