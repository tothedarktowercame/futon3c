const MEMORY_ABSENT = "no recorded memory call for this turn (nothing reached it that the record can show)";
export function readyFrames(frames) {
    let end = frames.length;
    while (end > 0 && !["analyzed", "drafted-provisional"].includes(frames[end - 1].parse?.status || ""))
        end--;
    return frames.slice(0, end);
}
const short = (value, n = 8) => typeof value === "string" ? value.slice(0, n) : "?";
const shown = (value, fallback = "?") => value === undefined || value === null ? fallback : String(value);
export function cuePieces(text, cues) {
    const chars = Array.from(text);
    const covered = new Array(chars.length).fill(false);
    for (const cue of cues || []) {
        const needle = Array.from(cue);
        if (!needle.length)
            continue;
        outer: for (let i = 0; i + needle.length <= chars.length; i++) {
            for (let j = 0; j < needle.length; j++)
                if (chars[i + j] !== needle[j])
                    continue outer;
            for (let j = 0; j < needle.length; j++)
                covered[i + j] = true;
            break;
        }
    }
    if (!chars.length)
        return [{ text: "", cue: false }];
    const pieces = [];
    let start = 0;
    for (let i = 1; i <= chars.length; i++) {
        if (i === chars.length || covered[i] !== covered[start]) {
            pieces.push({ text: chars.slice(start, i).join(""), cue: covered[start] });
            start = i;
        }
    }
    return pieces;
}
export function happenedDetail(row) {
    const summary = row.summary;
    if (!summary || typeof summary !== "object")
        return "";
    if (summary.event === "turn-commits") {
        const commits = Array.isArray(summary.commits) ? summary.commits : [];
        return `${commits.length} commit(s) in window: ${commits.map(c => `${shown(c.repo)} ${short(c.sha, 7)} ${shown(c.author)}: ${shown(c.subject)}`).join(" | ")}`;
    }
    if (summary.intent)
        return `intent=${shown(summary.intent)} fragment=${shown(summary["fragment-id"])} target=${shown(summary.target)}`;
    if (typeof summary.text === "string") {
        const chars = Array.from(summary.text);
        return chars.length > 80 ? chars.slice(0, 79).join("") + "…" : summary.text;
    }
    return shown(summary.event, "");
}
export function portsAge(since, at) {
    if (!since || !at)
        return undefined;
    const delta = Date.parse(at) - Date.parse(since);
    if (!Number.isFinite(delta))
        return undefined;
    const minutes = Math.floor(delta / 60000);
    return minutes < 1 ? "<1m" : `${minutes}m`;
}
function operatorLines(frame) {
    const hits = frame.operators?.hits || [];
    if (!hits.length)
        return ["(no operator words)"];
    return hits.map(h => {
        const cue = h.cue_intent;
        let meeting = "outside 象's marks";
        if (cue && h.agree === true)
            meeting = `in 象 ${cue} "${shown(h.cue_text, "")}" (agrees)`;
        else if (cue)
            meeting = `in 象 ${cue} "${shown(h.cue_text, "")}"; chip says ${shown(h.chip_intent)}`;
        return `${shown(h.ibol).padEnd(12)} "${shown(h.text, "")}" ${meeting}`;
    });
}
function patternLines(frame) {
    const p = frame.patterns || {};
    const matched = (p.matched || []).map(x => typeof x === "string" ? x : shown(x.id));
    const out = [`matched: ${matched.length ? matched.join(", ") : "(none)"}`];
    const proposed = Object.entries(p.proposed_by_parent || {});
    if (!proposed.length)
        out.push("proposed: (none)");
    for (const [parent, children] of proposed) {
        out.push(`proposed under ${parent}:`);
        for (const child of children)
            out.push(`  - ${shown(child.id)} — ${shown(child.title)} (fragment ${shown(child.fragment)})`);
    }
    out.push(`rejected: ${(p.rejected || []).length}`);
    return out;
}
function portsText(frame) {
    if (!frame.ports)
        return "";
    const lines = [];
    const closed = frame.ports.closed_this_turn || [];
    const open = frame.ports.still_open || [];
    if (closed.length) {
        lines.push("closed:");
        closed.forEach(a => lines.push(`  - ${shown(a.kind)}: ${shown(a.text, "")}`));
    }
    if (open.length) {
        lines.push("still open:");
        open.forEach(a => lines.push(`  - ${shown(a.kind)} (${portsAge(a.since, frame.turn.at) || "?"}): ${shown(a.text, "")}`));
    }
    return lines.join("\n");
}
function memoryLines(frame) {
    if (!frame.memory)
        return [MEMORY_ABSENT];
    const out = [];
    for (const call of frame.memory.calls || []) {
        const args = typeof call.args === "string" ? call.args : JSON.stringify(call.args ?? {});
        out.push(`${shown(call.round)}  ${shown(call.tool)}  ${args}`);
        out.push(`  reached: ${(call.reached || []).length ? (call.reached || []).join(", ") : "(none)"}`);
    }
    if ((frame.memory.injected || []).length)
        out.push(`injected before round 1: ${frame.memory.injected.join(", ")}`);
    return out.length ? out : ["(no memory events recorded)"];
}
export function frameModel(frame) {
    const provisional = frame.parse?.status === "drafted-provisional";
    const parse = (frame.parse?.fragments || []).map(f => {
        const labels = f.labels || [];
        const intents = labels.map(l => l.intent).filter((x) => !!x);
        const head = f.disagree && intents.length > 1 ? `disagree: ${intents.join(" / ")}` : (f.combined || "(unlabelled)");
        const sources = labels.map(l => `${shown(l.source)}→${shown(l.intent)}${l.weak ? "?" : ""}`).join(", ");
        return { head, pieces: cuePieces(f.text || "", f.cues || []), sources, provisional };
    });
    const happened = (frame.happened || []).map(row => `${shown(row.at)}  ${shown(row.type).padEnd(28)} ${happenedDetail(row)}`);
    return {
        status: frame.parse?.status || "missing", at: frame.turn?.at || "?",
        said: frame.turn?.text || "(no text)",
        saidHref: frame.record?.turn_id ? `../scopes/index.html?turn=${encodeURIComponent(frame.record.turn_id)}` : undefined,
        operators: operatorLines(frame),
        parseNote: provisional ? "小象, provisional — 象's reading pending" : "",
        parse, patterns: patternLines(frame),
        happened: happened.length ? happened : ["(nothing between this turn and the next)"],
        ports: portsText(frame), memory: memoryLines(frame),
        session: frame.record?.session_id || "?",
    };
}
function add(parent, tag, cls, text) {
    const node = document.createElement(tag);
    if (cls)
        node.className = cls;
    if (text !== undefined)
        node.textContent = text;
    parent.append(node);
    return node;
}
export function renderStepper(initial, host, opts = {}) {
    let frames = readyFrames(initial);
    let current = Math.max(0, Math.min((opts.frame || 1) - 1, Math.max(0, frames.length - 1)));
    const status = add(host, "div", "stepper-status");
    const body = add(host, "main", "stepper-frame");
    const footer = add(host, "footer", "stepper-footer", "rewind: Emacs only");
    void footer;
    const section = (title, lines, pre = true) => {
        const sectionNode = add(body, "section");
        add(sectionNode, "h2", "", title);
        const content = add(sectionNode, pre ? "pre" : "div", "stepper-section");
        content.textContent = lines.join("\n");
        return content;
    };
    const updateUrl = () => {
        if (typeof history !== "undefined" && typeof location !== "undefined") {
            const url = new URL(location.href);
            url.searchParams.set("frame", String(current + 1));
            history.replaceState(null, "", url);
        }
    };
    const draw = () => {
        body.textContent = "";
        if (!frames.length) {
            status.textContent = "frame 0/0";
            add(body, "p", "", "No operator-turn frames.");
            return;
        }
        const model = frameModel(frames[current]);
        status.textContent = `frame ${current + 1}/${frames.length} · ${short(model.session)} · ${model.status} · ${opts.live ? "live" : "fixture"}`;
        const said = add(body, "section");
        const saidHeading = add(said, "h2");
        const heading = model.saidHref ? add(saidHeading, "a", "said-link") : add(saidHeading, "span");
        if (model.saidHref)
            heading.href = model.saidHref;
        heading.textContent = `SAID — ${model.at}`;
        add(said, "p", "said-text", model.said);
        section("OPERATORS", model.operators);
        const parseSection = add(body, "section");
        add(parseSection, "h2", "", "PARSE");
        if (model.parseNote)
            add(parseSection, "p", "provisional", model.parseNote);
        if (!model.parse.length)
            add(parseSection, "pre", "stepper-section", "(no fragments)");
        for (const line of model.parse) {
            const row = add(parseSection, "div", line.provisional ? "parse-line provisional" : "parse-line");
            add(row, "span", "parse-head", `[${line.head}] `);
            for (const piece of line.pieces)
                add(row, "span", piece.cue ? "parse-cue" : "", piece.text);
            add(row, "div", "parse-sources", `(${line.sources})`);
        }
        section("PATTERNS", model.patterns);
        section("HAPPENED", model.happened);
        if (model.ports)
            section("PORTS", model.ports.split("\n"));
        section("MEMORY", model.memory);
        updateUrl();
    };
    const goTo = (i) => { current = Math.max(0, Math.min(i, Math.max(0, frames.length - 1))); draw(); };
    const next = () => goTo(current + 1);
    const prev = () => goTo(current - 1);
    window.addEventListener("keydown", async (event) => {
        if (["INPUT", "TEXTAREA", "SELECT"].includes(event.target?.tagName))
            return;
        if (event.key === "n" || event.key === "ArrowRight")
            next();
        else if (event.key === "p" || event.key === "ArrowLeft")
            prev();
        else if (event.key === "Home")
            goTo(0);
        else if (event.key === "End")
            goTo(frames.length - 1);
        else if (event.key === "g") {
            if (!opts.live || !opts.refetch) {
                status.textContent += " · fixture mode: no refetch";
                return;
            }
            try {
                frames = readyFrames(await opts.refetch());
                current = Math.min(current, Math.max(0, frames.length - 1));
                draw();
            }
            catch (error) {
                status.textContent += ` · refresh failed: ${String(error)}`;
            }
        }
    });
    draw();
    return { goTo, next, prev, index: () => current, count: () => frames.length };
}
