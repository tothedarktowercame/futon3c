/** Split text at all active span boundaries, using Unicode code-point offsets. */
export function splitPieces(text, spans) {
    const points = Array.from(text);
    const boundaries = new Set([0, points.length]);
    for (const span of spans) {
        boundaries.add(Math.max(0, Math.min(points.length, span.start)));
        boundaries.add(Math.max(0, Math.min(points.length, span.end)));
    }
    const ordered = Array.from(boundaries).sort((a, b) => a - b);
    const pieces = [];
    for (let i = 0; i + 1 < ordered.length; i += 1) {
        const start = ordered[i];
        const end = ordered[i + 1];
        if (start === end)
            continue;
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
export function mixInk(spans, layers) {
    const layerById = new Map(layers.map((layer) => [layer.id, layer]));
    const strength = { c: 0, m: 0, y: 0 };
    for (const span of spans) {
        const ink = layerById.get(span.layer)?.ink;
        if (ink)
            strength[ink] = Math.max(strength[ink], clamp(span.weight));
    }
    if (strength.c === 0 && strength.m === 0 && strength.y === 0)
        return "transparent";
    const red = Math.round(255 * (1 - strength.c));
    const green = Math.round(255 * (1 - strength.m));
    const blue = Math.round(255 * (1 - strength.y));
    return `rgb(${red}, ${green}, ${blue})`;
}
/** Report invalid ranges, unresolved edge destinations, and legend coverage. */
export function auditScopeData(data) {
    const segments = new Map(data.segments.map((segment) => [segment.id, segment]));
    const spans = new Map(data.spans.map((span) => [span.id, span]));
    const invalidSpans = [];
    for (const span of data.spans) {
        const segment = segments.get(span.seg);
        const length = segment ? Array.from(segment.text).length : -1;
        if (!segment)
            invalidSpans.push({ id: span.id, reason: `unknown segment ${span.seg}` });
        else if (span.start < 0 || span.end < span.start || span.end > length) {
            invalidSpans.push({ id: span.id, reason: `[${span.start},${span.end}) outside 0..${length}` });
        }
    }
    const stubs = Array.from(new Set(data.edges
        .filter((edge) => !spans.has(edge.to))
        .map((edge) => edge.to)));
    const categories = new Map();
    const add = (value, category) => {
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
function clamp(value) {
    return Math.max(0, Math.min(1, value));
}
function element(tag, className, text) {
    const node = document.createElement(tag);
    if (className)
        node.className = className;
    if (text !== undefined)
        node.textContent = text;
    return node;
}
export function renderScopes(data, host, opts = {}) {
    const knownLayers = new Set(data.layers.map((layer) => layer.id));
    let active = new Set((opts.layers ?? data.layers.map((layer) => layer.id))
        .filter((id) => knownLayers.has(id)));
    const spanById = new Map(data.spans.map((span) => [span.id, span]));
    const edgesByFrom = new Map();
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
    host.replaceChildren(root);
    const checkboxes = new Map();
    for (const layer of data.layers) {
        const label = element("label", "scope-layer-toggle");
        const checkbox = element("input");
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
    function setLit(spanId, lit) {
        root.querySelectorAll(`[data-span~="${CSS.escape(spanId)}"]`)
            .forEach((node) => node.classList.toggle("lit", lit));
    }
    function jumpTo(spanId) {
        const target = root.querySelector(`[data-span~="${CSS.escape(spanId)}"]`);
        if (!target)
            return;
        target.scrollIntoView({ behavior: "smooth", block: "center" });
        setLit(spanId, true);
        window.setTimeout(() => setLit(spanId, false), 1200);
    }
    function paint() {
        main.replaceChildren();
        side.replaceChildren();
        for (const segment of data.segments) {
            const segmentSpans = data.spans.filter((span) => span.seg === segment.id);
            const visibleSpans = segmentSpans.filter((span) => active.has(span.layer));
            const primary = segment.role === "operator" || segment.role === "agent";
            if (!primary && segmentSpans.length > 0 && visibleSpans.length === 0)
                continue;
            const article = element("article", `scope-segment role-${segment.role}`);
            article.dataset.segment = segment.id;
            article.append(element("h2", "scope-segment-label", segment.label));
            const body = element("div", "scope-segment-body");
            const text = element("div", "scope-text");
            const notes = element("ol", "scope-notes");
            const marksAt = new Map();
            visibleSpans.filter((span) => span.mark).forEach((span) => {
                const marks = marksAt.get(span.start) ?? [];
                marks.push(span);
                marksAt.set(span.start, marks);
            });
            for (const piece of splitPieces(segment.text, visibleSpans)) {
                for (const marked of marksAt.get(piece.start) ?? []) {
                    const mark = element("span", "scope-mark", marked.mark);
                    mark.dataset.span = marked.id;
                    text.append(mark);
                }
                const pieceSpans = piece.spanIds.map((id) => spanById.get(id))
                    .filter((span) => span !== undefined);
                const pieceNode = element("span", "scope-piece", piece.text);
                pieceNode.dataset.span = piece.spanIds.join(" ");
                pieceNode.style.backgroundColor = mixInk(pieceSpans, data.layers);
                pieceNode.addEventListener("mouseenter", () => piece.spanIds.forEach((id) => setLit(id, true)));
                pieceNode.addEventListener("mouseleave", () => piece.spanIds.forEach((id) => setLit(id, false)));
                pieceNode.addEventListener("click", () => piece.spanIds.forEach((id) => setLit(id, true)));
                text.append(pieceNode);
            }
            visibleSpans.filter((span) => span.note).sort((a, b) => a.start - b.start)
                .forEach((span) => {
                const note = element("li", "scope-note");
                note.dataset.span = span.id;
                note.append(element("p", "", span.note));
                for (const edge of edgesByFrom.get(span.id) ?? []) {
                    const link = element("button", "scope-edge", `${edge.relation} ${edge.label} → ${edge.to}`);
                    if (spanById.has(edge.to))
                        link.addEventListener("click", () => jumpTo(edge.to));
                    else {
                        link.classList.add("stub");
                        link.disabled = true;
                    }
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
    function setLayers(ids) {
        active = new Set(ids.filter((id) => knownLayers.has(id)));
        checkboxes.forEach((checkbox, id) => { checkbox.checked = active.has(id); });
        paint();
    }
    paint();
    return { setLayers };
}
