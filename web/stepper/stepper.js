const MEMORY_ABSENT = "no recorded memory call for this turn (nothing reached it that the record can show)";
export const PARSE_PENDING = "no reading yet — 象 has not read this turn; what has happened so far is under HAPPENED";
export const READ_STATUSES = ["analyzed", "drafted-provisional"];
/** Frames to show. By default the newest turns that 象 has not read yet are
 * left out, as the Emacs stepper does. In live mode (keepPending) they stay:
 * the current turn's SAID and HAPPENED are what is going on right now, and
 * the reading arrives on the next refresh. An older unread turn always stays,
 * shown as missing. */
export function readyFrames(frames, keepPending = false) {
    if (keepPending)
        return frames;
    let end = frames.length;
    while (end > 0 && !READ_STATUSES.includes(frames[end - 1].parse?.status || ""))
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
    const status = frame.parse?.status || "missing";
    const provisional = status === "drafted-provisional";
    const parse = (frame.parse?.fragments || []).map(f => {
        const labels = f.labels || [];
        const intents = labels.map(l => l.intent).filter((x) => !!x);
        const head = f.disagree && intents.length > 1 ? `disagree: ${intents.join(" / ")}` : (f.combined || "(unlabelled)");
        const sources = labels.map(l => `${shown(l.source)}→${shown(l.intent)}${l.weak ? "?" : ""}`).join(", ");
        return { head, pieces: cuePieces(f.text || "", f.cues || []), sources, provisional };
    });
    const happened = (frame.happened || []).map(row => `${shown(row.at)}  ${shown(row.type).padEnd(28)} ${happenedDetail(row)}`);
    return {
        status, at: frame.turn?.at || "?",
        said: frame.turn?.text || "(no text)",
        saidHref: frame.record?.turn_id ? `../scopes/index.html?turn=${encodeURIComponent(frame.record.turn_id)}` : undefined,
        operators: operatorLines(frame),
        parseNote: provisional ? "小象, provisional — 象's reading pending" : READ_STATUSES.includes(status) ? "" : PARSE_PENDING,
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
/** opts.frame: 1-based frame to open on; absent means the newest frame in
 * live mode and the first in fixture mode. opts.pollMs: in live mode, refetch
 * this often while the page is visible (0 or absent: only on `g`). A reader on
 * the newest frame follows the newest after a refresh; one reading back stays
 * put, as the Emacs stepper does after a reload. */
export function renderStepper(initial, host, opts = {}) {
    const clamp = (i) => Math.max(0, Math.min(i, Math.max(0, frames.length - 1)));
    let frames = readyFrames(initial, !!opts.live);
    let current = clamp(opts.frame ? opts.frame - 1 : opts.live ? frames.length - 1 : 0);
    let refreshed = "";
    let busy = false;
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
        status.textContent = `frame ${current + 1}/${frames.length} · ${short(model.session)} · ${model.status} · ${opts.live ? "live" : "fixture"}${refreshed}`;
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
    const goTo = (i) => { current = clamp(i); draw(); };
    const next = () => goTo(current + 1);
    const prev = () => goTo(current - 1);
    const refresh = async () => {
        if (!opts.live || !opts.refetch) {
            status.textContent += " · fixture mode: no refetch";
            return;
        }
        if (busy)
            return;
        busy = true;
        try {
            const onNewest = current >= frames.length - 1;
            frames = readyFrames(await opts.refetch(), true);
            current = onNewest ? frames.length - 1 : clamp(current);
            refreshed = ` · refreshed ${new Date().toISOString().slice(11, 19)}Z`;
            draw();
        }
        catch (error) {
            status.textContent += ` · refresh failed: ${String(error)}`;
        }
        finally {
            busy = false;
        }
    };
    if (opts.live && opts.refetch && (opts.pollMs || 0) > 0) {
        setInterval(() => { if (typeof document === "undefined" || !document.hidden)
            void refresh(); }, opts.pollMs);
    }
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
        else if (event.key === "g")
            await refresh();
    });
    draw();
    return { goTo, next, prev, refresh, index: () => current, count: () => frames.length };
}
