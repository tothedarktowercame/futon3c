export type Ink = "c" | "m" | "y";

export interface ScopeSegment {
  id: string;
  role: string;
  label: string;
  text: string;
}

export interface ScopeLayer {
  id: string;
  name: string;
  ink: Ink;
}

export interface ScopeSpan {
  id: string;
  seg: string;
  start: number;
  end: number;
  layer: string;
  kind: string;
  mark?: string;
  weight: number;
  note?: string;
}

export interface ScopeEdge {
  from: string;
  to: string;
  relation: string;
  label: string;
}

export interface ScopeData {
  segments: ScopeSegment[];
  layers: ScopeLayer[];
  spans: ScopeSpan[];
  edges: ScopeEdge[];
  legend: Record<string, string>;
  [key: string]: unknown;
}

export interface ScopePiece {
  start: number;
  end: number;
  text: string;
  spanIds: string[];
}

export interface Unit {
  e: HTMLElement;
  a: number;
  b: number;
}

export interface ScopeInk {
  color: string;
  spanIds: string[];
}

export interface LegendAuditEntry {
  value: string;
  categories: Array<"kind" | "relation" | "label">;
  text: string;
  defined: boolean;
}

export interface ScopeAudit {
  invalidSpans: Array<{ id: string; reason: string }>;
  stubs: string[];
  legend: LegendAuditEntry[];
}

/** The span with the smallest extent, ties to the earliest start; undefined when empty. */
export function smallestSpan(spans: ScopeSpan[]): ScopeSpan | undefined {
  let best: ScopeSpan | undefined;
  for (const span of spans) {
    if (!best) { best = span; continue; }
    const size = span.end - span.start;
    const bestSize = best.end - best.start;
    if (size < bestSize || (size === bestSize && span.start < best.start)) best = span;
  }
  return best;
}

/** Split text at all active span boundaries, using Unicode code-point offsets. */
export function splitPieces(text: string, spans: ScopeSpan[]): ScopePiece[] {
  const points = Array.from(text);
  const boundaries = new Set<number>([0, points.length]);
  for (const span of spans) {
    boundaries.add(Math.max(0, Math.min(points.length, span.start)));
    boundaries.add(Math.max(0, Math.min(points.length, span.end)));
  }
  const ordered = Array.from(boundaries).sort((a, b) => a - b);
  const pieces: ScopePiece[] = [];
  for (let i = 0; i + 1 < ordered.length; i += 1) {
    const start = ordered[i];
    const end = ordered[i + 1];
    if (start === end) continue;
    pieces.push({
      start,
      end,
      text: points.slice(start, end).join(""),
      spanIds: spans
        .filter((span) => span.start <= start && span.end >= end)
        .map((span) => span.id),
    });
  }
  return pieces;
}

/** Mix weighted cyan, magenta and yellow as subtractive ink. */
export function mixInk(
  spans: ScopeSpan[],
  layers: ScopeLayer[],
): string {
  const layerById = new Map(layers.map((layer) => [layer.id, layer]));
  const strength: Record<Ink, number> = { c: 0, m: 0, y: 0 };
  for (const span of spans) {
    const ink = layerById.get(span.layer)?.ink;
    if (ink) strength[ink] = Math.max(strength[ink], clamp(span.weight));
  }
  if (strength.c === 0 && strength.m === 0 && strength.y === 0) return "transparent";
  const red = Math.round(255 * (1 - strength.c));
  const green = Math.round(255 * (1 - strength.m));
  const blue = Math.round(255 * (1 - strength.y));
  return `rgb(${red}, ${green}, ${blue})`;
}

/** Compute the ink for host-positioned units without requiring a DOM. */
export function inkUnits(
  units: Array<{ a: number; b: number }>,
  spans: ScopeSpan[],
  layers: ScopeLayer[],
  active: Set<string>,
): ScopeInk[] {
  return units.map((unit) => {
    const covering = spans.filter((span) =>
      active.has(span.layer) && span.start < unit.b && unit.a < span.end);
    return {
      color: covering.length === 0 ? "" : mixInk(covering, layers),
      spanIds: covering.map((span) => span.id),
    };
  });
}

/** Report invalid ranges, unresolved edge destinations, and legend coverage. */
export function auditScopeData(data: ScopeData): ScopeAudit {
  const segments = new Map(data.segments.map((segment) => [segment.id, segment]));
  const spans = new Map(data.spans.map((span) => [span.id, span]));
  const invalidSpans: ScopeAudit["invalidSpans"] = [];
  for (const span of data.spans) {
    const segment = segments.get(span.seg);
    const length = segment ? Array.from(segment.text).length : -1;
    if (!segment) invalidSpans.push({ id: span.id, reason: `unknown segment ${span.seg}` });
    else if (span.start < 0 || span.end < span.start || span.end > length) {
      invalidSpans.push({ id: span.id, reason: `[${span.start},${span.end}) outside 0..${length}` });
    }
  }
  const stubs = Array.from(new Set(data.edges
    .filter((edge) => !spans.has(edge.to))
    .map((edge) => edge.to)));
  const categories = new Map<string, Set<"kind" | "relation" | "label">>();
  const add = (value: string, category: "kind" | "relation" | "label") => {
    const set = categories.get(value) ?? new Set();
    set.add(category);
    categories.set(value, set);
  };
  data.spans.forEach((span) => add(span.kind, "kind"));
  data.edges.forEach((edge) => { add(edge.relation, "relation"); add(edge.label, "label"); });
  const legend = Array.from(categories.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([value, kinds]) => ({
      value,
      categories: Array.from(kinds).sort(),
      text: data.legend[value] ?? "undefined",
      defined: Object.prototype.hasOwnProperty.call(data.legend, value),
    }));
  return { invalidSpans, stubs, legend };
}

function clamp(value: number): number {
  return Math.max(0, Math.min(1, value));
}

function element<K extends keyof HTMLElementTagNameMap>(
  tag: K, className?: string, text?: string,
): HTMLElementTagNameMap[K] {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

export function renderScopes(
  data: ScopeData,
  host: HTMLElement,
  opts: { layers?: string[]; anchored?: Record<string, Unit[]> } = {},
): { setLayers(ids: string[]): void } {
  const knownLayers = new Set(data.layers.map((layer) => layer.id));
  let active = new Set((opts.layers ?? data.layers.map((layer) => layer.id))
    .filter((id) => knownLayers.has(id)));
  const spanById = new Map(data.spans.map((span) => [span.id, span]));
  const anchored = opts.anchored ?? {};
  const edgesByFrom = new Map<string, ScopeEdge[]>();
  for (const edge of data.edges) {
    const edges = edgesByFrom.get(edge.from) ?? [];
    edges.push(edge);
    edgesByFrom.set(edge.from, edges);
  }

  const root = element("div", "scope-view");
  const controls = element("div", "scope-controls");
  const layerControls = element("fieldset", "scope-layers");
  const layerLegend = element("legend", "", "Layers");
  layerControls.append(layerLegend);
  controls.append(layerControls);
  root.append(controls);
  const columns = element("div", "scope-columns");
  const main = element("main", "scope-main");
  const side = element("aside", "scope-side");
  columns.append(main, side);
  root.append(columns);
  if (Object.keys(anchored).length === 0) host.replaceChildren(root);
  else host.append(root);

  const checkboxes = new Map<string, HTMLInputElement>();
  for (const layer of data.layers) {
    const label = element("label", "scope-layer-toggle");
    const checkbox = element("input") as HTMLInputElement;
    checkbox.type = "checkbox";
    checkbox.checked = active.has(layer.id);
    checkbox.dataset.layer = layer.id;
    const swatch = element("span", `scope-swatch ink-${layer.ink}`);
    label.append(checkbox, swatch, document.createTextNode(layer.name));
    layerControls.append(label);
    checkboxes.set(layer.id, checkbox);
    checkbox.addEventListener("change", () => {
      setLayers(data.layers.filter((candidate) => checkboxes.get(candidate.id)?.checked)
        .map((candidate) => candidate.id));
    });
  }

  const audit = auditScopeData(data);
  const glossary = element("details", "scope-legend");
  glossary.open = true;
  glossary.append(element("summary", "", "Legend"));
  const glossaryList = element("dl");
  for (const entry of audit.legend) {
    const term = element("dt", entry.defined ? "" : "undefined", entry.value);
    term.title = entry.categories.join(", ");
    glossaryList.append(term, element("dd", "", entry.text));
  }
  glossary.append(glossaryList);
  controls.append(glossary);

  function setLit(spanId: string, lit: boolean): void {
    root.querySelectorAll<HTMLElement>(`[data-span~="${CSS.escape(spanId)}"]`)
      .forEach((node) => node.classList.toggle("lit", lit));
    Object.values(anchored).flat().forEach((unit) => {
      const ids = (unit.e.dataset.spans ?? "").split(" ");
      if (ids.includes(spanId)) unit.e.classList.toggle("lit", lit);
    });
  }

  function jumpTo(spanId: string): void {
    const target = root.querySelector<HTMLElement>(`[data-span~="${CSS.escape(spanId)}"]`)
      ?? Object.values(anchored).flat()
        .find((unit) => (unit.e.dataset.spans ?? "").split(" ").includes(spanId))?.e;
    if (!target) return;
    target.scrollIntoView({ behavior: "smooth", block: "center" });
    setLit(spanId, true);
    window.setTimeout(() => setLit(spanId, false), 1200);
  }

  function readout(spans: ScopeSpan[]): string {
    return spans.map((span) => {
      const layer = data.layers.find((candidate) => candidate.id === span.layer);
      return [layer?.name ?? span.layer, span.kind, span.mark, span.note]
        .filter(Boolean).join(" · ");
    }).join("\n");
  }

  function bindSpanInteraction(node: HTMLElement, spanIds: string[]): void {
    const spans = spanIds.map((id) => spanById.get(id))
      .filter((span): span is ScopeSpan => span !== undefined);
    node.title = readout(spans);
    // Hovering text lights only the smallest span around it: the clause or
    // binder, whose extent the page does not otherwise show. The enclosing
    // spans stay reachable from their notes and the readout.
    const hovered = smallestSpan(spans);
    node.onmouseenter = () => { if (hovered) setLit(hovered.id, true); };
    node.onmouseleave = () => { if (hovered) setLit(hovered.id, false); };
    node.onclick = () => spanIds.forEach((id) => setLit(id, true));
  }

  function paintAnchored(segment: ScopeSegment, visibleSpans: ScopeSpan[]): void {
    const units = anchored[segment.id] ?? [];
    const inks = inkUnits(units, visibleSpans, data.layers, active);
    units.forEach((unit, index) => {
      const ink = inks[index];
      unit.e.style.backgroundColor = ink.color;
      unit.e.dataset.spans = ink.spanIds.join(" ");
      unit.e.dataset.span = ink.spanIds.join(" ");
      bindSpanInteraction(unit.e, ink.spanIds);
    });
  }

  function paint(): void {
    main.replaceChildren();
    side.replaceChildren();
    for (const segment of data.segments) {
      const segmentSpans = data.spans.filter((span) => span.seg === segment.id);
      const visibleSpans = segmentSpans.filter((span) => active.has(span.layer));
      const primary = segment.role === "operator" || segment.role === "agent";
      const anchoredSegment = Object.prototype.hasOwnProperty.call(anchored, segment.id);
      if (anchoredSegment) paintAnchored(segment, visibleSpans);
      if (!primary && segmentSpans.length > 0 && visibleSpans.length === 0) continue;
      const article = element("article", `scope-segment role-${segment.role}`);
      article.dataset.segment = segment.id;
      article.append(element("h2", "scope-segment-label", segment.label));
      const body = element("div", "scope-segment-body");
      const text = element("div", "scope-text");
      const notes = element("ol", "scope-notes");
      const marksAt = new Map<number, ScopeSpan[]>();
      visibleSpans.filter((span) => span.mark).forEach((span) => {
        const marks = marksAt.get(span.start) ?? [];
        marks.push(span);
        marksAt.set(span.start, marks);
      });
      for (const piece of anchoredSegment ? [] : splitPieces(segment.text, visibleSpans)) {
        for (const marked of marksAt.get(piece.start) ?? []) {
          const mark = element("span", "scope-mark", marked.mark);
          mark.dataset.span = marked.id;
          text.append(mark);
        }
        const pieceSpans = piece.spanIds.map((id) => spanById.get(id))
          .filter((span): span is ScopeSpan => span !== undefined);
        const pieceNode = element("span", "scope-piece", piece.text);
        pieceNode.dataset.span = piece.spanIds.join(" ");
        pieceNode.style.backgroundColor = mixInk(pieceSpans, data.layers);
        bindSpanInteraction(pieceNode, piece.spanIds);
        text.append(pieceNode);
      }
      visibleSpans.filter((span) => span.note).sort((a, b) => a.start - b.start)
        .forEach((span) => {
          const note = element("li", "scope-note");
          note.dataset.span = span.id;
          note.append(element("p", "", span.note));
          for (const edge of edgesByFrom.get(span.id) ?? []) {
            const link = element("button", "scope-edge",
              `${edge.relation} ${edge.label} → ${edge.to}`);
            if (spanById.has(edge.to)) link.addEventListener("click", () => jumpTo(edge.to));
            else { link.classList.add("stub"); link.disabled = true; }
            note.append(link);
          }
          note.addEventListener("mouseenter", () => setLit(span.id, true));
          note.addEventListener("mouseleave", () => setLit(span.id, false));
          note.addEventListener("click", () => setLit(span.id, true));
          notes.append(note);
        });
      body.append(text, notes);
      article.append(body);
      (primary ? main : side).append(article);
    }
  }

  function setLayers(ids: string[]): void {
    active = new Set(ids.filter((id) => knownLayers.has(id)));
    checkboxes.forEach((checkbox, id) => { checkbox.checked = active.has(id); });
    paint();
  }

  paint();
  return { setLayers };
}
