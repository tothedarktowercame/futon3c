#!/usr/bin/env python3
"""Build a self-contained dependency graph for one 象 mission.

The input readers are intentionally stdlib-only.  Record selection is delegated
to the mission's ``brief/resume_brief.py`` loader so this view has the same
mission/seat join semantics as the existing brief.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import html
import importlib.util
import json
import re
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Iterable


HERE = Path(__file__).resolve().parent
MISSION_ROOT = HERE.parent
REPO_ROOT = MISSION_ROOT.parents[2]
RECORDS = Path.home() / ".emacs-graph/session-turn-analysis"


def _brief_module():
    path = MISSION_ROOT / "brief/resume_brief.py"
    spec = importlib.util.spec_from_file_location("mission_resume_brief", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


brief = _brief_module()


def instant(value: str | None) -> dt.datetime:
    return brief.instant(value)


def compact(text: Any, limit: int = 60) -> str:
    value = " ".join(str(text or "").split())
    return value if len(value) <= limit else value[: limit - 1] + "…"


def act_id(value: Any) -> str:
    if isinstance(value, str):
        return value
    if isinstance(value, dict):
        return str(value.get("act") or value.get("id") or "")
    return ""


@dataclass
class Node:
    id: str
    kind: str = "unknown"
    text: str = ""
    author: str = "unknown"
    record: str = ""
    session: str = ""
    time: str = ""
    synthetic: bool = False


@dataclass
class Edge:
    type: str
    source: str
    target: str
    provenance: str
    record: str = ""
    quote: str = ""
    labeller: str = ""
    at: str = ""
    rules_version: str = ""
    close: bool = False
    agreed: bool = False
    red: bool = False
    id: str = ""

    def __post_init__(self) -> None:
        self.id = hashlib.sha256(
            f"{self.type}\0{self.source}\0{self.target}".encode("utf-8")
        ).hexdigest()[:12]


@dataclass
class Graph:
    mission: str
    nodes: dict[str, Node] = field(default_factory=dict)
    edges: list[Edge] = field(default_factory=list)
    records: list[dict[str, Any]] = field(default_factory=list)
    joined_by: str = ""
    generated_at: str = ""
    sccs: list[list[str]] = field(default_factory=list)

    def add_node(self, node: Node) -> None:
        old = self.nodes.get(node.id)
        if old is None or (not old.text and node.text):
            self.nodes[node.id] = node

    def add_edge(self, edge: Edge) -> None:
        self.edges.append(edge)


def _node_from_act(act: dict[str, Any], record: dict[str, Any]) -> Node:
    aid = str(act.get("id") or "")
    author = act.get("author") or record.get("agent_id") or "unknown"
    return Node(aid, str(act.get("kind") or "unknown"), str(act.get("text") or ""),
                str(author), str(record.get("_record_id") or record.get("turn_id") or ""),
                str(record.get("session_id") or ""), str(act.get("at") or record.get("created_at") or ""))


def _record_id(record: dict[str, Any]) -> str:
    return str(record.get("_record_id") or record.get("turn_id") or "")


def _record_reply_node(record: dict[str, Any], graph: Graph) -> str:
    rid = _record_id(record)
    replies = [a for a in record.get("acts") or [] if str(a.get("id", "")).find("-r-") >= 0]
    if replies:
        return str(replies[0].get("id"))
    synthetic = f"{rid}/reply"
    graph.add_node(Node(synthetic, "reply", "reply for " + rid, str(record.get("agent_id") or "unknown"),
                        rid, str(record.get("session_id") or ""), str(record.get("created_at") or ""), True))
    return synthetic


def _sidecars(root: Path) -> Iterable[tuple[Path, dict[str, Any]]]:
    for path in sorted(root.glob("data/**/*.moves.json")):
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        if isinstance(payload, dict):
            yield path, payload


def _sidecars_by_record(root: Path) -> dict[str, list[dict[str, Any]]]:
    result: dict[str, list[dict[str, Any]]] = {}
    for path, payload in _sidecars(root):
        key = str(payload.get("record") or path.name.split(".moves.json", 1)[0])
        result.setdefault(key, []).append(payload)
    return result


def _join_records(mission: str, join: str, since: str | None) -> tuple[list[dict[str, Any]], str]:
    all_rows: list[dict[str, Any]] = []
    for path in sorted(RECORDS.glob("turn-*.json")):
        try:
            row = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        if not isinstance(row, dict) or not row.get("turn_id") or not row.get("session_id"):
            continue
        if since and instant(row.get("created_at")) < instant(since):
            continue
        row["_record_id"] = path.name.split(".", 1)[0]
        all_rows.append(row)
    mission_rows = [r for r in all_rows if r.get("mission_id") == mission]
    if join == "mission":
        return mission_rows, "mission"
    if join == "seats":
        seats = {str(r.get("agent_id")) for r in mission_rows if r.get("agent_id")}
        return [r for r in all_rows if r.get("agent_id") in seats], "seat"
    excursions = set(brief.mission_excursions(MISSION_ROOT / "../../missions" / f"{mission}.md"))
    matched = [r for r in all_rows if r.get("mission_id") == mission or r.get("excursion_id") in excursions]
    if matched:
        return matched, "mission-excursion"
    seats = {str(r.get("agent_id")) for r in mission_rows if r.get("agent_id")}
    return [r for r in all_rows if r.get("agent_id") in seats], "seat"


def _load_input_acts(root: Path, record_id: str) -> list[dict[str, Any]]:
    for path in root.glob(f"data/**/input/{record_id}.input.json"):
        try:
            value = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        acts: list[dict[str, Any]] = []
        for key, author in (("fragments", "operator"), ("reply_acts", "agent"), ("previous_reply_acts", "agent")):
            for item in value.get(key) or []:
                if isinstance(item, dict) and item.get("id"):
                    row = dict(item)
                    row.setdefault("author", author)
                    acts.append(row)
        return acts
    return []


def build_graph(mission: str, records: list[dict[str, Any]], root: Path = MISSION_ROOT,
                join: str = "", reduce: bool = False, generated_at: str | None = None) -> Graph:
    graph = Graph(mission, records=records, joined_by=join,
                  generated_at=generated_at or dt.datetime.now(dt.timezone.utc).isoformat().replace("+00:00", "Z"))
    acts_by_id: dict[str, dict[str, Any]] = {}
    record_by_id: dict[str, dict[str, Any]] = {}
    for record in records:
        rid = _record_id(record)
        record_by_id[rid] = record
        acts = list(record.get("acts") or [])
        if not acts:
            acts = _load_input_acts(root, rid)
        for act in acts:
            if act.get("id"):
                acts_by_id[str(act["id"])] = act
                graph.add_node(_node_from_act(act, record))

    sidecars = _sidecars_by_record(root)
    labelled: list[Edge] = []
    for rid, payloads in sidecars.items():
        if rid not in record_by_id:
            continue
        for payload in payloads:
            for item in payload.get("acts") or []:
                source = act_id(item)
                if not source:
                    continue
                source_record = record_by_id[rid]
                if source not in graph.nodes:
                    graph.add_node(_node_from_act({"id": source, "text": item.get("quote", ""), "kind": item.get("kind", "unknown")}, source_record))
                answers = item.get("answers") or []
                for answer in answers:
                    target = act_id(answer)
                    effect = str(answer.get("effect") or "unclear")
                    if not target:
                        continue
                    if target not in graph.nodes:
                        graph.add_node(_node_from_act({"id": target, "text": "(referenced opener)"}, source_record))
                    edge_type = f"answers/{effect}" if effect in {"closes", "keeps", "unclear"} else "answers/unclear"
                    labelled.append(Edge(edge_type, source, target, "labelled", rid,
                                         str(answer.get("quote") or item.get("quote") or ""),
                                         str(payload.get("labeller") or ""), str(payload.get("at") or ""),
                                         close=effect == "closes"))
                scope = item.get("scope") or {}
                for key, edge_type, close in (("within", "scope/within", False), ("closes", "scope/closes", True)):
                    target = act_id(scope.get(key))
                    if target:
                        if target not in graph.nodes:
                            graph.add_node(_node_from_act({"id": target, "text": "(referenced opener)"}, source_record))
                        labelled.append(Edge(edge_type, source, target, "labelled", rid,
                                             str(item.get("quote") or ""), str(payload.get("labeller") or ""),
                                             str(payload.get("at") or ""), close=close))

    kernels: list[Edge] = []
    for record in records:
        rid = _record_id(record)
        source = _record_reply_node(record, graph)
        version = str((record.get("ports_status") or {}).get("rules_version") or "unknown")
        for closed in (record.get("ports") or {}).get("closed_this_turn") or []:
            target = act_id(closed)
            if not target:
                continue
            if target not in graph.nodes:
                graph.add_node(Node(target, str(closed.get("kind") or "unknown"), str(closed.get("text") or ""),
                                    "unknown", rid, str(record.get("session_id") or ""), str(record.get("created_at") or "")))
            kernels.append(Edge("port/close", source, target, "kernel", rid,
                                str(closed.get("text") or ""), rules_version=version, close=True))

    # A close from the same record and target is one reviewed edge, with both
    # sources retained in its provenance rather than two drawn lines.
    merged: list[Edge] = []
    used_kernel: set[int] = set()
    for edge in labelled:
        match = next((i for i, kernel in enumerate(kernels)
                      if i not in used_kernel and edge.close and kernel.close
                      and edge.record == kernel.record and edge.target == kernel.target), None)
        if match is None:
            merged.append(edge)
            continue
        kernel = kernels[match]
        used_kernel.add(match)
        edge.agreed = True
        edge.provenance = f"labelled+kernel ({edge.labeller or 'unknown'}; {kernel.rules_version})"
        merged.append(edge)
    merged.extend(kernel for i, kernel in enumerate(kernels) if i not in used_kernel)
    graph.edges = merged
    if reduce:
        graph.edges = transitive_reduce(graph.edges)
    graph.sccs = tarjan(graph.nodes, graph.edges)
    cycle_nodes = {node for scc in graph.sccs if len(scc) > 1 for node in scc}
    for edge in graph.edges:
        edge.red = edge.source in cycle_nodes and edge.target in cycle_nodes and _same_scc(edge.source, edge.target, graph.sccs)
    return graph


def _same_scc(a: str, b: str, sccs: list[list[str]]) -> bool:
    return any(a in scc and b in scc for scc in sccs)


def tarjan(nodes: dict[str, Node] | Iterable[str], edges: Iterable[Edge]) -> list[list[str]]:
    names = list(nodes if isinstance(nodes, dict) else nodes)
    outgoing: dict[str, list[str]] = {name: [] for name in names}
    for edge in edges:
        outgoing.setdefault(edge.source, []).append(edge.target)
        outgoing.setdefault(edge.target, [])
    index = 0
    indices: dict[str, int] = {}
    low: dict[str, int] = {}
    stack: list[str] = []
    on_stack: set[str] = set()
    result: list[list[str]] = []

    def visit(node: str) -> None:
        nonlocal index
        indices[node] = low[node] = index
        index += 1
        stack.append(node); on_stack.add(node)
        for target in outgoing.get(node, []):
            if target not in indices:
                visit(target); low[node] = min(low[node], low[target])
            elif target in on_stack:
                low[node] = min(low[node], indices[target])
        if low[node] == indices[node]:
            component = []
            while True:
                item = stack.pop(); on_stack.remove(item); component.append(item)
                if item == node: break
            result.append(component)

    for node in names:
        if node not in indices:
            visit(node)
    return result


def transitive_reduce(edges: list[Edge]) -> list[Edge]:
    scope = [e for e in edges if e.type == "scope/within"]
    adjacency: dict[str, set[str]] = {}
    for edge in scope:
        adjacency.setdefault(edge.source, set()).add(edge.target)
    removed: set[int] = set()
    for i, edge in enumerate(edges):
        if edge.type != "scope/within":
            continue
        seen: set[str] = set()
        todo = [x for x in adjacency.get(edge.source, set()) if x != edge.target]
        while todo:
            current = todo.pop()
            if current in seen: continue
            seen.add(current)
            todo.extend(adjacency.get(current, set()))
        if edge.target in seen:
            removed.add(i)
    return [edge for i, edge in enumerate(edges) if i not in removed]


def layout(graph: Graph) -> tuple[dict[str, tuple[float, float]], float, float]:
    sessions: dict[str, list[Node]] = {}
    for node in graph.nodes.values():
        sessions.setdefault(node.session or "unknown", []).append(node)
    ordered = sorted(sessions.items(), key=lambda p: min((instant(n.time) for n in p[1]), default=dt.datetime.max))
    positions: dict[str, tuple[float, float]] = {}
    col_width, row_height = 330.0, 86.0
    for col, (_session, nodes) in enumerate(ordered):
        nodes.sort(key=lambda n: (instant(n.time), n.id))
        for row, node in enumerate(nodes):
            indent = 28.0 if node.author != "operator" else 0.0
            positions[node.id] = (40 + col * col_width + indent, 40 + row * row_height)
    return positions, max(720.0, len(ordered) * col_width + 80), max(320.0, max((y for _, y in positions.values()), default=0) + 90)


def _edge_style(edge: Edge) -> tuple[str, str]:
    if edge.red: return "#d22", "red"
    if edge.type == "answers/keeps": return "#557", "dashed"
    if edge.type == "answers/unclear": return "#888", "dotted"
    if edge.type.startswith("scope/"): return "#666", "dotted"
    if edge.type == "port/close": return "#087f9c", "solid"
    return "#333", "solid"


def _svg(graph: Graph, positions: dict[str, tuple[float, float]], width: float, height: float) -> str:
    parts = [f'<svg id="graph" viewBox="0 0 {width:g} {height:g}" width="100%" height="{min(height, 760):g}" role="img">',
             '<defs><marker id="arrow" markerWidth="8" markerHeight="8" refX="7" refY="3" orient="auto"><path d="M0,0 L0,6 L7,3 z" fill="context-stroke"/></marker></defs>']
    for edge in graph.edges:
        if edge.source not in positions or edge.target not in positions: continue
        x1, y1 = positions[edge.source]; x2, y2 = positions[edge.target]
        color, dash = _edge_style(edge)
        path = f"M{x1+120:g},{y1:g} C{x1+180:g},{y1:g} {x2-60:g},{y2:g} {x2:g},{y2:g}"
        title = html.escape(f"{edge.provenance}; record {edge.record}; {edge.quote}")
        dash_attr = " stroke-dasharray=\"7 5\"" if dash == "dashed" else " stroke-dasharray=\"2 5\"" if dash == "dotted" else ""
        parts.append(f'<path class="edge" data-edge="{edge.id}" d="{path}" fill="none" stroke="{color}" stroke-width="{2.5 if edge.red else 1.7:g}"{dash_attr} marker-end="url(#arrow)"><title>{title}</title></path>')
    for node_id, node in graph.nodes.items():
        if node_id not in positions: continue
        x, y = positions[node_id]
        fill = "#fff4d6" if node.author == "operator" else "#e7f2f7"
        parts.append(f'<g class="node" data-node="{html.escape(node_id)}" transform="translate({x:g},{y:g})"><rect x="0" y="-22" width="120" height="44" rx="7" fill="{fill}" stroke="#555"/><title>{html.escape(node.text)}</title><text x="6" y="-4" font-size="10">{html.escape(node.kind)}</text><text x="6" y="11" font-size="9">{html.escape(compact(node.text, 22))}</text></g>')
    parts.append("</svg>")
    return "".join(parts)


def render_html(graph: Graph, reduce: bool = False) -> str:
    positions, width, height = layout(graph)
    svg = _svg(graph, positions, width, height)
    rows = []
    for edge in graph.edges:
        source = graph.nodes.get(edge.source, Node(edge.source))
        target = graph.nodes.get(edge.target, Node(edge.target))
        provenance = edge.provenance + ("; agreed" if edge.agreed else "")
        rows.append(f'<tr class="edge-row" data-edge-id="{edge.id}"><td>{edge.id}</td><td>{html.escape(edge.type)}</td><td><code>{html.escape(source.id)}</code><br>{html.escape(compact(source.text))}</td><td><code>{html.escape(target.id)}</code><br>{html.escape(compact(target.text))}</td><td>{html.escape(provenance)}</td><td>{html.escape(edge.record)}</td><td>{html.escape(edge.quote)}</td><td><select class="verdict"><option value="unsure" selected>unsure</option><option value="right">right</option><option value="wrong">wrong</option></select></td></tr>')
    now = html.escape(graph.generated_at)
    return f'''<!doctype html><html lang="en"><meta charset="utf-8"><title>{html.escape(graph.mission)} dependency graph</title>
<style>body{{font:14px system-ui,sans-serif;margin:1rem;color:#222}}#viewport{{border:1px solid #bbb;overflow:hidden;cursor:grab}}#viewport.dragging{{cursor:grabbing}}table{{border-collapse:collapse;width:100%;margin-top:1rem;font-size:12px}}th,td{{border:1px solid #ccc;padding:.3rem;text-align:left;vertical-align:top}}th{{background:#eee}}code{{font-size:11px}}.legend span{{display:inline-block;margin-right:1.2rem}}.swatch{{display:inline-block;width:2rem;border-top:2px solid currentColor;margin-right:.3rem;vertical-align:middle}}.dashed{{border-top-style:dashed}}.dotted{{border-top-style:dotted}}</style>
<h1>Mission {html.escape(graph.mission)} dependency graph</h1><p>Generated {now}; join {html.escape(graph.joined_by)}; {len(graph.nodes)} nodes, {len(graph.edges)} edges, {len([s for s in graph.sccs if len(s)>1])} cyclic SCCs.</p>
<p class="legend"><span><i class="swatch"></i>answers / kernel close</span><span><i class="swatch dashed"></i>keeps</span><span><i class="swatch dotted"></i>scope / unclear</span><span style="color:#d22"><i class="swatch"></i>cycle</span></p>
<div id="viewport">{svg}</div><p><button id="export">Export decisions</button></p>
<form id="add"><label>Source <input name="source" required></label> <label>Target <input name="target" required></label> <label>Type <select name="type"><option>answers/closes</option><option>answers/keeps</option><option>answers/unclear</option><option>scope/within</option><option>scope/closes</option><option>port/close</option></select></label> <button>Add edge</button></form>
<table><thead><tr><th>id</th><th>type</th><th>source</th><th>target</th><th>provenance</th><th>record</th><th>quote</th><th>verdict</th></tr></thead><tbody>{''.join(rows)}</tbody></table>
<script>
const mission={json.dumps(graph.mission)}, generated={json.dumps(graph.generated_at)}, graphBuilt={json.dumps(graph.generated_at)}, added=[];
const vp=document.getElementById('viewport'), svg=document.getElementById('graph'); let drag=false,last=null;
vp.addEventListener('pointerdown',e=>{{drag=true;last=e;vp.classList.add('dragging')}}); window.addEventListener('pointerup',()=>{{drag=false;vp.classList.remove('dragging')}}); window.addEventListener('pointermove',e=>{{if(!drag)return; const a=svg.viewBox.baseVal; const sx=a.width/vp.clientWidth,sy=a.height/vp.clientHeight; a.x-=(e.clientX-last.clientX)*sx;a.y-=(e.clientY-last.clientY)*sy;last=e}});
vp.addEventListener('wheel',e=>{{e.preventDefault();const a=svg.viewBox.baseVal;const k=e.deltaY<0?.9:1.1;const mx=a.x+e.offsetX/vp.clientWidth*a.width,my=a.y+e.offsetY/vp.clientHeight*a.height;a.x=mx-(mx-a.x)*k;a.y=my-(my-a.y)*k;a.width*=k;a.height*=k}},{{passive:false}});
document.getElementById('add').addEventListener('submit',e=>{{e.preventDefault();const f=new FormData(e.target);added.push({{source:f.get('source'),target:f.get('target'),type:f.get('type')}});e.target.reset()}});
document.getElementById('export').addEventListener('click',()=>{{const verdicts=[...document.querySelectorAll('.edge-row')].map(r=>{{const e=r.dataset.edgeId;const cells=r.children;return {{edge_id:e,type:cells[1].innerText,source:cells[2].querySelector('code').innerText,target:cells[3].querySelector('code').innerText,provenance:cells[4].innerText,verdict:r.querySelector('.verdict').value}}}});const blob=new Blob([JSON.stringify({{mission,generated_at:generated,graph_built_at:graphBuilt,verdicts,added}},null,2)],{{type:'application/json'}});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download='decisions-'+mission+'-'+generated.replace(/[:.]/g,'-')+'.json';a.click()}});
</script></html>'''


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mission", required=True)
    parser.add_argument("--join", choices=("auto", "mission", "seats"), default="auto")
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--reduce", action="store_true")
    parser.add_argument("--since")
    args = parser.parse_args(argv)
    started = time.monotonic()
    records, joined = _join_records(args.mission, args.join, args.since)
    graph = build_graph(args.mission, records, MISSION_ROOT, joined, args.reduce)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(render_html(graph, args.reduce), encoding="utf-8")
    by_type: dict[str, int] = {}; by_prov: dict[str, int] = {}
    for edge in graph.edges:
        by_type[edge.type] = by_type.get(edge.type, 0) + 1
        by_prov[edge.provenance] = by_prov.get(edge.provenance, 0) + 1
    print(json.dumps({"out": str(args.out), "records": len(records), "nodes": len(graph.nodes),
                      "edges": len(graph.edges), "by_type": by_type, "by_provenance": by_prov,
                      "agreed": sum(e.agreed for e in graph.edges),
                      "scc_count": sum(len(s) > 1 for s in graph.sccs),
                      "seconds": round(time.monotonic() - started, 3)}, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
