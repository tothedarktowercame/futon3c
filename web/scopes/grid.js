/** The memory layer over a session: one row per turn, one column per memory
 * any recorded call named, one mark per cell for the strongest thing the
 * record shows (retrieved-only, reached, echoed). Reads the fixtures that
 * scripts/scope_view_over_time.py writes. */

const SYMBOL = { "": "·", "retrieved-only": "○", "reached": "●", "echoed": "◉" };
const LEVELS = ["retrieved-only", "reached", "echoed"];
const STATUSES = ["in-force", "retracted-since", "not-yet-said", "not-in-force"];

/** Report cells that name no row or column, levels and statuses the legend
 * lacks, and columns no cell uses. */
export function auditGridData(data) {
    const rows = new Set(data.rows.map((row) => row.turn));
    const columns = new Set(data.columns.map((column) => column.memory));
    const orphanCells = [];
    const undefinedValues = new Set();
    const usedColumns = new Set();
    for (const [key, cell] of Object.entries(data.cells)) {
        const bar = key.indexOf("|");
        const turn = key.slice(0, bar);
        const memory = key.slice(bar + 1);
        if (!rows.has(turn)) orphanCells.push({ key, reason: `unknown turn ${turn}` });
        if (!columns.has(memory)) orphanCells.push({ key, reason: `unknown memory ${memory}` });
        usedColumns.add(memory);
        if (!Object.prototype.hasOwnProperty.call(data.legend, cell.level))
            undefinedValues.add(cell.level);
        if (!Object.prototype.hasOwnProperty.call(data.legend, cell.status))
            undefinedValues.add(cell.status);
    }
    const emptyColumns = data.columns
        .filter((column) => !usedColumns.has(column.memory))
        .map((column) => column.memory);
    return {
        orphanCells,
        undefinedValues: Array.from(undefinedValues).sort(),
        emptyColumns,
    };
}

/** Counts per row and for the whole grid, by level. */
export function tallyGrid(data) {
    const perRow = new Map(data.rows.map((row) => [row.turn, { "retrieved-only": 0, reached: 0, echoed: 0 }]));
    const total = { "retrieved-only": 0, reached: 0, echoed: 0 };
    for (const [key, cell] of Object.entries(data.cells)) {
        const turn = key.slice(0, key.indexOf("|"));
        const tally = perRow.get(turn);
        if (tally && cell.level in tally) {
            tally[cell.level] += 1;
            total[cell.level] += 1;
        }
    }
    return { perRow, total, withCalls: data.rows.filter((row) => row.calls.length > 0).length };
}

/** Draw the grid into host. opts.turnHref(row) gives the per-turn page for a
 * row, when there is one. */
export function renderGrid(data, host, opts = {}) {
    host.textContent = "";
    const tally = tallyGrid(data);
    const head = element("div", "grid-head");
    head.append(element("h1", "", `${data.agent} · session ${data.session}`));
    head.append(element("p", "grid-counts",
        `${data.rows.length} turns, ${tally.withCalls} with minus40 calls, `
        + `${data.columns.length} memories ever named: `
        + `${tally.total.echoed} echoed, ${tally.total.reached} reached, `
        + `${tally.total["retrieved-only"]} retrieved-only.`));
    host.append(head);

    const legend = element("dl", "grid-legend");
    for (const level of LEVELS) {
        legend.append(element("dt", "", `${SYMBOL[level]} ${level}`));
        legend.append(element("dd", "", data.legend[level] ?? "undefined"));
    }
    for (const status of STATUSES) {
        legend.append(element("dt", "", status === "in-force" ? status : `${status} †`));
        legend.append(element("dd", "", data.legend[status] ?? "undefined"));
    }
    host.append(legend);

    const columns = element("div", "grid-columns");
    const table = element("table", "grid-table");
    const thead = document.createElement("thead");
    const headRow = document.createElement("tr");
    headRow.append(element("th", "grid-turn", "turn (created)"));
    headRow.append(element("th", "grid-calls", "calls"));
    data.columns.forEach((column, index) => {
        const th = element("th", "grid-memory");
        const button = element("button", "grid-memory-name", `c${index + 1}`);
        button.title = `${column.memory}: ${column.facts} facts, first said ${firstSaid(column)}`;
        button.addEventListener("click", () => showColumn(column, index));
        th.append(button);
        headRow.append(th);
    });
    thead.append(headRow);
    table.append(thead);

    const tbody = document.createElement("tbody");
    for (const row of data.rows) {
        const tr = document.createElement("tr");
        if (row.calls.length === 0) tr.className = "grid-row-quiet";
        const turnCell = element("td", "grid-turn");
        const label = `${row.turn.slice(-8)} ${row.at.slice(5, 16)}`;
        const href = opts.turnHref?.(row);
        if (href && row.calls.length > 0) {
            const a = element("a", "", label);
            a.href = href;
            turnCell.append(a);
        } else {
            turnCell.textContent = label;
        }
        turnCell.title = `${row.record}: ${row.memories} memories, reply ${row.reply_chars} chars`;
        tr.append(turnCell);
        const callsCell = element("td", "grid-calls", String(row.calls.length));
        callsCell.title = row.calls.map((call) => `${call.tool} ${call.args}`).join("\n");
        tr.append(callsCell);
        for (const column of data.columns) {
            const cell = data.cells[`${row.turn}|${column.memory}`];
            const td = element("td", "grid-cell");
            if (cell) {
                td.classList.add(`level-${cell.level}`, `status-${cell.status}`);
                td.textContent = SYMBOL[cell.level] + (cell.status === "in-force" ? "" : "†");
                td.title = [column.name, `${cell.level}, ${cell.status}`,
                    ...(cell.calls ?? []), ...(cell.echoes ?? []).map((echo) => `echo: ${echo}`)].join("\n");
            } else {
                td.textContent = SYMBOL[""];
            }
            tr.append(td);
        }
        tbody.append(tr);
    }
    table.append(tbody);
    columns.append(table);

    const side = element("aside", "grid-side");
    const key = element("ol", "grid-key");
    data.columns.forEach((column, index) => {
        const li = element("li", "");
        const button = element("button", "grid-edge", `c${index + 1} ${column.name}`);
        button.title = column.project;
        button.addEventListener("click", () => showColumn(column, index));
        li.append(button);
        key.append(li);
    });
    side.append(key);
    const detail = element("div", "grid-detail");
    side.append(detail);
    columns.append(side);
    host.append(columns);

    function showColumn(column, index) {
        detail.textContent = "";
        detail.append(element("h2", "", `c${index + 1} ${column.name}`));
        detail.append(element("p", "", `${column.project}, ${column.facts} facts`));
        const events = element("ul", "grid-events");
        for (const event of column.events ?? []) {
            const li = element("li", `event-${event.event}`);
            li.append(element("span", "grid-event-when", `${event.at.slice(0, 10)} ${event.event}`));
            li.append(element("span", "grid-event-text", ` ${event.text}`));
            events.append(li);
        }
        detail.append(events);
        for (const th of table.querySelectorAll("th.grid-memory")) th.classList.remove("lit");
        table.querySelectorAll("th.grid-memory")[index]?.classList.add("lit");
    }
}

function firstSaid(column) {
    const said = (column.events ?? []).filter((event) => event.event === "said");
    return said.length ? said[0].at.slice(0, 10) : "?";
}

function element(tag, className, text) {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined) node.textContent = text;
    return node;
}
