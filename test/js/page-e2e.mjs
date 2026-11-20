import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createRequire } from "node:module";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const checkout = resolve(here, "../..");
const playwrightPath = resolve(
  checkout,
  process.env.TOGETHER_PLAYWRIGHT ?? "../futon4/dev/web/webarxana/node_modules/playwright",
);
const require = createRequire(import.meta.url);

let chromium;
try {
  ({ chromium } = require(playwrightPath));
} catch (error) {
  console.log(`SKIP page-e2e: cannot load Playwright from ${playwrightPath}: ${error.message}`);
  process.exit(2);
}

function waitForPort(child) {
  return new Promise((resolvePort, reject) => {
    let output = "";
    const timer = setTimeout(() => reject(new Error(`hub did not print a port\n${output}`)), 30000);
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => {
      output += chunk;
      const match = output.match(/(?:^|\n)PORT (\d+)(?:\n|$)/);
      if (match) {
        clearTimeout(timer);
        resolvePort(Number(match[1]));
      }
    });
    child.stderr.on("data", (chunk) => { output += chunk; });
    child.once("exit", (code) => {
      clearTimeout(timer);
      reject(new Error(`hub exited ${code}\n${output}`));
    });
  });
}

async function waitLive(page) {
  await page.waitForSelector("textarea", { state: "visible" });
  await page.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("live"));
}

async function setValue(page, transform, caret = null) {
  await page.locator("textarea").evaluate((element, args) => {
    const next = args.mode === "prefix" ? args.value + element.value
      : args.mode === "suffix" ? element.value + args.value
        : args.value;
    element.value = next;
    const at = args.caret === null ? next.length : args.caret;
    element.setSelectionRange(at, at);
    element.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
  }, transform(caret));
}

const replaceWith = (value) => (caret) => ({ mode: "replace", value, caret });
const prefixWith = (value) => (caret) => ({ mode: "prefix", value, caret });
const suffixWith = (value) => (caret) => ({ mode: "suffix", value, caret });

async function expectedColour(page, author) {
  return page.evaluate(async (name) => {
    const { authorColour } = await import("/together/v1/web/binding.js");
    const probe = document.createElement("span");
    probe.style.color = authorColour(name);
    document.body.append(probe);
    const colour = getComputedStyle(probe).color;
    probe.remove();
    return colour;
  }, author);
}

async function installPostHold(page) {
  await page.evaluate(() => {
    const original = window.fetch.bind(window);
    const held = [];
    window.fetch = (url, options = {}) => {
      if ((options.method ?? "GET").toUpperCase() !== "POST") return original(url, options);
      return new Promise((resolveRequest, rejectRequest) => {
        held.push(() => original(url, options).then(resolveRequest, rejectRequest));
      });
    };
    window.__releaseTogetherPosts = () => {
      window.fetch = original;
      held.splice(0).forEach((release) => release());
    };
  });
}

async function postJSON(url, body, identity = "joe") {
  const response = await fetch(url, {
    method: "POST",
    headers: { "X-Together-Editor": identity, "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  assert.equal(response.status, 200, `${url}: ${await response.text()}`);
}

async function seedDocument(origin, key, value) {
  if (value === "") {
    await postJSON(`${origin}/together/v1/doc/${key}/ops`, {
      parents: { rev: 0 },
      op: { op: "insert", id: ["joe.seed", 1], after: null, text: "x", attrs: { author: "joe" } },
    });
    await postJSON(`${origin}/together/v1/doc/${key}/ops`, {
      parents: { rev: 1 }, op: { op: "delete", ids: [["joe.seed", 1]] },
    });
  } else {
    await postJSON(`${origin}/together/v1/doc/${key}/ops`, {
      parents: { rev: 0 },
      op: { op: "insert", id: ["joe.seed", 1], after: null, text: value, attrs: { author: "joe" } },
    });
  }
}

async function main() {
  const directory = await mkdtemp(resolve(tmpdir(), "together-page-"));
  const expression = `(require 'futon3c.arxana.together-http) (let [h (futon3c.arxana.together-http/start! {:port 0 :dir ${JSON.stringify(directory)}})] (println "PORT" (:port h)) (flush) @(promise))`;
  const hub = spawn("clojure", ["-M", "-e", expression], {
    cwd: checkout,
    stdio: ["ignore", "pipe", "pipe"],
  });
  let browser;
  try {
    const port = await waitForPort(hub);
    const origin = `http://127.0.0.1:${port}`;
    const documentKey = `page-e2e-${process.pid}`;
    const url = `${origin}/together/v1/web/index.html?doc=${documentKey}`;
    browser = await chromium.launch({ headless: true });
    const joeContext = await browser.newContext({ extraHTTPHeaders: { "X-Together-Editor": "joe" } });
    const murchContext = await browser.newContext({ extraHTTPHeaders: { "X-Together-Editor": "murch" } });
    const joe = await joeContext.newPage();
    const murch = await murchContext.newPage();
    await Promise.all([
      joe.goto(url, { waitUntil: "domcontentloaded" }),
      murch.goto(url, { waitUntil: "domcontentloaded" }),
    ]);
    await Promise.all([waitLive(joe), waitLive(murch)]);

    await joe.locator("textarea").fill("hello");
    await murch.waitForFunction(() => document.querySelector("textarea")?.value === "hello");
    const joeColour = await expectedColour(murch, "joe");
    const shownColour = await murch.locator('[data-author="joe"]').evaluate((element) => getComputedStyle(element).color);
    assert.equal(shownColour, joeColour);
    console.log("CHECK 1 joe text and colour propagated");

    await Promise.all([installPostHold(joe), installPostHold(murch)]);
    await Promise.all([
      setValue(joe, suffixWith("JOE")),
      setValue(murch, suffixWith("MURCH")),
    ]);
    await Promise.all([
      joe.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("1 unsent")),
      murch.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("1 unsent")),
    ]);
    await Promise.all([
      joe.evaluate(() => window.__releaseTogetherPosts()),
      murch.evaluate(() => window.__releaseTogetherPosts()),
    ]);
    await joe.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("0 unsent"));
    await murch.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("0 unsent"));
    await murch.waitForFunction(() => document.querySelector("textarea")?.value.includes("JOE"));
    await joe.waitForFunction(() => document.querySelector("textarea")?.value.includes("MURCH"));
    const concurrent = await Promise.all([joe.locator("textarea").inputValue(), murch.locator("textarea").inputValue()]);
    assert.equal(concurrent[0], concurrent[1]);
    assert.ok(concurrent[0].includes("JOE") && concurrent[0].includes("MURCH"));
    console.log("CHECK 2 held concurrent words converged without interleaving");

    await murch.locator("textarea").evaluate((element) => {
      const at = element.value.indexOf("hello") + "hello".length;
      element.setSelectionRange(at, at);
    });
    const oldCaret = await murch.locator("textarea").evaluate((element) => element.selectionStart);
    await setValue(joe, prefixWith("START"), 5);
    await murch.waitForFunction(() => document.querySelector("textarea")?.value.startsWith("START"));
    await murch.waitForFunction((expected) => document.querySelector("textarea")?.selectionStart === expected, oldCaret + 5);
    console.log("CHECK 3 remote prefix preserved and shifted the caret");

    await setValue(joe, suffixWith("😀"));
    await murch.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("😀"));
    await setValue(joe, suffixWith("Z"));
    await murch.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("😀Z"));
    console.log("CHECK 4 emoji and following insertion kept code-point order");

    const longText = `${"wrapped words ".repeat(35)}\n\tTabbed line ${"more ".repeat(20)}\n`;
    await setValue(joe, replaceWith(longText));
    await murch.waitForFunction((expected) => document.querySelector("textarea")?.value === expected, longText);
    for (const width of [360, 1200]) {
      await joe.setViewportSize({ width, height: 500 });
      const heights = await joe.evaluate(() => ({
        editor: document.querySelector("textarea").scrollHeight,
        layer: document.querySelector(".text-layer").scrollHeight,
      }));
      assert.equal(heights.layer, heights.editor, `layer alignment at ${width}px`);
    }
    console.log("CHECK 5 layer alignment held at 360px and 1200px");

    const phoneContext = await browser.newContext({ extraHTTPHeaders: { "X-Together-Editor": "joe" } });
    const phone = await phoneContext.newPage();
    await phone.goto(`${url}&surface=phone`, { waitUntil: "domcontentloaded" });
    await waitLive(phone);
    await setValue(phone, suffixWith("PHONE"));
    await joe.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("PHONE"));
    await setValue(joe, suffixWith("WEB"));
    await phone.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("PHONEWEB"));
    const joeColours = await phone.locator('[data-author="joe"]').evaluateAll(
      (elements) => [...new Set(elements.map((element) => getComputedStyle(element).color))],
    );
    assert.deepEqual(joeColours, [await expectedColour(phone, "joe")]);
    console.log("CHECK 6 joe.web and joe.phone were accepted with one author colour");

    await murchContext.setOffline(true);
    await murch.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("reconnecting"));
    await murchContext.setOffline(false);
    await murch.waitForFunction(() => document.querySelector(".status-main")?.textContent.includes("live"));
    await setValue(joe, suffixWith("RECONNECT_ONCE"));
    await murch.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("RECONNECT_ONCE"));
    const reconnectValue = await murch.locator("textarea").inputValue();
    assert.equal(reconnectValue.split("RECONNECT_ONCE").length - 1, 1);
    console.log("CHECK 7 interrupted event stream reconnected and delivered once");

    // One dropped POST while the event stream stays open: the retry goes through,
    // and the failure message and "reconnecting" must then leave the status line.
    let dropped = 0;
    await phone.route("**/ops", (route) => {
      if (dropped === 0) { dropped += 1; return route.abort("connectionreset"); }
      return route.continue();
    });
    await phone.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("RECONNECT_ONCE"));
    await setValue(phone, suffixWith("DROPPED_ONCE"));
    await phone.waitForFunction(() => document.querySelector(".status")?.textContent.includes("Posting failed"));
    await joe.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("DROPPED_ONCE"));
    await phone.waitForFunction(() => {
      const text = document.querySelector(".status")?.textContent ?? "";
      return text.includes("live") && text.includes("0 unsent") && !text.includes("Posting failed") && !text.includes("reconnecting");
    });
    await phone.unroute("**/ops");
    assert.equal(dropped, 1);
    assert.equal((await joe.locator("textarea").inputValue()).split("DROPPED_ONCE").length - 1, 1);
    console.log("CHECK 15 a dropped post is retried once and its message is cleared");

    await phone.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("DROPPED_ONCE"));
    const response = await fetch(`${origin}/together/v1/doc/${documentKey}/state`, {
      headers: { "X-Together-Editor": "joe" },
    });
    assert.equal(response.status, 200);
    const snapshot = await response.json();
    const hubText = snapshot.runs.filter((run) => !run.deleted).map((run) => run.text).join("");
    const values = await Promise.all([joe, murch, phone].map((page) => page.locator("textarea").inputValue()));
    values.forEach((value) => assert.equal(value, hubText));
    console.log("CHECK 8 hub and all three browser surfaces ended equal");
    // Listing discovers durable documents and preserves a requested surface.
    const secondKey = `listing-second-${process.pid}`;
    const seeded = await fetch(`${origin}/together/v1/doc/${secondKey}/ops`, {
      method: "POST",
      headers: { "X-Together-Editor": "joe", "Content-Type": "application/json" },
      body: JSON.stringify({ parents: { rev: 0 }, op: {
        op: "insert", id: ["joe.seed", 1], after: null, text: "A discoverable document 😀",
        attrs: { author: "joe" },
      } }),
    });
    assert.equal(seeded.status, 200);
    const listing = await joeContext.newPage();
    await listing.goto(`${origin}/together/v1/web/index.html?surface=listing`, { waitUntil: "domcontentloaded" });
    await listing.waitForSelector("#document-list a");
    assert.equal(await listing.locator("#list-identity").textContent(), "Signed in as joe");
    const keys = await listing.locator("#document-list a").allTextContents();
    assert.ok(keys.includes(documentKey) && keys.includes(secondKey));
    const originalURL = listing.url();
    await listing.locator('input[name="document-key"]').fill("invalid key");
    await listing.getByRole("button", { name: "Open", exact: true }).click();
    assert.equal(listing.url(), originalURL);
    assert.match(await listing.locator("#key-validation").textContent(), /Invalid document key/);
    await listing.getByRole("link", { name: secondKey, exact: true }).click();
    await waitLive(listing);
    assert.equal(await listing.locator("textarea").inputValue(), "A discoverable document 😀");
    assert.equal(new URL(listing.url()).searchParams.get("surface"), "listing");
    assert.equal(new URL(listing.url()).pathname, "/together/v1/web/index.html");
    await listing.close();
    console.log("CHECK 9 document list discovers both keys and opens the selected editor");

    const layerName = `packet14-${process.pid}`;
    const workspace = `incubator.${layerName}`;
    const docs = {
      first: `incubator.first.${layerName}`,
      second: `incubator.second.${layerName}`,
      third: `incubator.third.${layerName}`,
    };
    await Promise.all([
      seedDocument(origin, docs.first, "First answer"),
      seedDocument(origin, docs.second, ""),
      seedDocument(origin, docs.third, "Third answer"),
    ]);
    for (const op of [
      { op: "add-node", id: "third", kind: "question", attrs: { question: "Third question", doc: docs.third, order: 2 } },
      { op: "add-node", id: "form", kind: "form", attrs: { title: "Packet 14 form", layer: layerName } },
      { op: "add-node", id: "second", kind: "question", attrs: { question: "Second question", doc: docs.second, order: 1 } },
      { op: "add-node", id: "bad", kind: "question", attrs: { question: "Broken question", doc: "bad key", order: 3 } },
      { op: "add-node", id: "first", kind: "question", attrs: { question: "First question", doc: docs.first, order: 0 } },
    ]) {
      await postJSON(`${origin}/together/v1/structure/${workspace}/acts`, { op });
    }

    const formA = await joeContext.newPage();
    const formB = await murchContext.newPage();
    const formURL = `${origin}/together/v1/web/form.html?ws=${workspace}&surface=form`;
    await Promise.all([
      formA.goto(formURL, { waitUntil: "domcontentloaded" }),
      formB.goto(formURL, { waitUntil: "domcontentloaded" }),
    ]);
    await Promise.all([
      formA.waitForFunction(() => document.querySelector("#form-status")?.textContent.includes("3 live")),
      formB.waitForFunction(() => document.querySelector("#form-status")?.textContent.includes("3 live")),
    ]);
    assert.deepEqual(await formA.locator(".form-question h2").allTextContents(),
      ["First question", "Second question", "Third question", "Broken question"]);
    assert.deepEqual(await formA.locator(".form-question textarea").evaluateAll(
      (elements) => elements.map((element) => element.value)),
      ["First answer", "", "Third answer"]);
    assert.equal(await formA.locator('[data-node-id="bad"] .editor-error').textContent(),
      "This question has no valid shared document.");
    console.log("CHECK 10 form questions and answers render in structure order");

    const secondA = formA.locator('[data-node-id="second"] textarea');
    await secondA.fill("Second answer shared");
    await formB.waitForFunction(() => document.querySelector('[data-node-id="second"] textarea')?.value === "Second answer shared");
    const single = await joeContext.newPage();
    await single.goto(`${origin}/together/v1/web/index.html?doc=${docs.second}&surface=single`, { waitUntil: "domcontentloaded" });
    await waitLive(single);
    assert.equal(await single.locator("textarea").inputValue(), "Second answer shared");
    console.log("CHECK 11 an answer edit reaches the other form and single-document page");

    const firstB = formB.locator('[data-node-id="first"] .editor-stack');
    const beforeHeight = (await firstB.boundingBox()).height;
    const firstA = formA.locator('[data-node-id="first"] textarea');
    await firstA.fill("First answer\nwith remote lines\nthat make it taller\nand keep growing\none more line\nlast line");
    await formB.waitForFunction(() => document.querySelector('[data-node-id="first"] textarea')?.value.includes("last line"));
    await formB.waitForFunction((height) => document.querySelector('[data-node-id="first"] .editor-stack').getBoundingClientRect().height > height, beforeHeight);
    const grown = await firstB.boundingBox();
    assert.ok(grown.height > beforeHeight);
    assert.equal(await formB.locator('[data-node-id="bad"] .editor-error').count(), 1);
    assert.match(await formB.locator('[data-node-id="third"] textarea').inputValue(), /Third answer/);
    console.log("CHECK 12 remote multiline edits grow one editor without breaking its siblings");

    const formListing = await joeContext.newPage();
    console.log("TRACE opening form listing");
    await formListing.goto(`${origin}/together/v1/web/index.html?surface=forms`, { waitUntil: "domcontentloaded" });
    console.log("TRACE form listing loaded");
    await formListing.waitForSelector("#form-list a", { timeout: 10000 });
    console.log("TRACE form listing populated");
    const formLink = formListing.getByRole("link", { name: workspace, exact: true });
    assert.equal(await formLink.count(), 1);
    const linked = new URL(await formLink.getAttribute("href"));
    assert.equal(linked.pathname, "/together/v1/web/form.html");
    assert.equal(linked.searchParams.get("ws"), workspace);
    assert.equal(linked.searchParams.get("surface"), "forms");
    console.log("CHECK 13 document listing derives and links the shared form workspace");

    assert.equal(await formA.locator("#form-title").textContent(), "Packet 14 form");
    assert.match(await formA.locator("#form-layer").textContent(), new RegExp(layerName));
    assert.match(await formA.locator("#form-status").textContent(), /joe · 4 answers · 3 live · 0 unsent/);
    console.log("CHECK 14 form title, layer and aggregate status are visible");

    // The throwaway hub speaks HTTP/1.1, where a browser allows six connections
    // per host; the pages above hold that many event streams between them, so a
    // further request from a new page would wait for ever. (Caddy serves HTTP/2
    // to real browsers, which has no such limit.)
    await Promise.all([single.close(), formA.close(), formB.close(), formListing.close()]);

    const notesKey = `notes-${process.pid}`;
    const notesText = "Alpha beta gamma delta";
    await seedDocument(origin, notesKey, notesText);
    const sourceKey = `source-${process.pid}`;
    await seedDocument(origin, sourceKey, `${"Context line.\n".repeat(80)}beta\n   gamma${"\nFollowing context.".repeat(80)}`);
    const notesWorkspace = `${notesKey}.notes`;
    for (const op of [
      { op: "add-node", id: "magenta-note", kind: "span", attrs: {
        doc: notesKey, start: ["joe.seed", 7], end: ["joe.seed", 10], by: "joe",
        colour: "magenta", title: "For the project", text: "Keep this precise.",
      } },
      { op: "add-node", id: "yellow-note", kind: "span", attrs: {
        doc: notesKey, start: ["joe.seed", 7], end: ["joe.seed", 16], by: "murch",
        colour: "yellow", title: "Source", text: "Compare the wording.",
        quote_title: "Earlier draft", quote: "beta gamma", where: "Draft 2",
        source: { doc: sourceKey, words: "beta gamma" },
      } },
      { op: "add-node", id: "lost-note", kind: "span", attrs: {
        doc: notesKey, start: ["missing", 1], end: ["missing", 2], by: "joe",
        colour: "red", title: "Lost",
      } },
    ]) await postJSON(`${origin}/together/v1/structure/${notesWorkspace}/acts`, { op });

    const notesPage = await joeContext.newPage();
    await notesPage.goto(`${origin}/together/v1/web/index.html?doc=${notesKey}`, { waitUntil: "domcontentloaded" });
    await waitLive(notesPage);
    await notesPage.waitForSelector("#notes-pane");
    const underlined = await notesPage.locator(".text-layer span").evaluateAll((spans) => spans
      .filter((span) => getComputedStyle(span).textDecorationLine.includes("underline"))
      .map((span) => [span.textContent, getComputedStyle(span).textDecorationColor]));
    assert.deepEqual(underlined.map(([value]) => value), ["beta", " gamma"]);
    assert.notEqual(underlined[0][1], underlined[1][1]);
    await notesPage.locator("textarea").evaluate((element) => {
      element.setSelectionRange(8, 8); element.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    });
    await notesPage.waitForSelector('[data-note-id="magenta-note"]');
    const cards = await notesPage.locator(".note-card").evaluateAll((items) => items.map((item) => item.dataset.noteId));
    assert.deepEqual(cards, ["magenta-note", "yellow-note"]);
    assert.match(await notesPage.locator('[data-note-id="magenta-note"]').textContent(), /For the project.*Keep this precise.*note by joe/s);
    assert.match(await notesPage.locator('[data-note-id="yellow-note"]').textContent(), /Earlier draft.*“beta gamma”.*Draft 2.*note by murch/s);
    assert.match(await notesPage.locator("#notes-pane").textContent(), /Notes that no longer resolve.*lost-note/s);
    console.log("CHECK 16 note underlines, nested cards, quote and unresolved list render");

    await notesPage.locator("textarea").evaluate((element) => {
      element.value = `${element.value.slice(0, 8)}X${element.value.slice(8)}`;
      element.setSelectionRange(9, 9);
      element.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
    });
    // Wait on the underline itself: the layer is repainted more than once
    // (the local edit, then its echo), and a read between repaints sees none.
    await notesPage.waitForFunction(() => [...document.querySelectorAll(".text-layer span")]
      .filter((span) => getComputedStyle(span).textDecorationLine.includes("underline"))
      .map((span) => span.textContent).join("").includes("beXta"));
    await notesPage.locator("textarea").evaluate((element) => {
      // After "gamma": past the end of the outer (yellow) note as well.
      const at = element.value.indexOf("gamma") + 5;
      element.value = `${element.value.slice(0, at)}Y${element.value.slice(at)}`;
      element.setSelectionRange(at + 1, at + 1);
      element.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
    });
    await notesPage.waitForFunction(() => document.querySelector(".text-layer")?.textContent.includes("gammaY"));
    await notesPage.waitForFunction(() => {
      const spans = [...document.querySelectorAll(".text-layer span")];
      const y = spans.find((span) => span.textContent.includes("Y"));
      const gamma = spans.find((span) => span.textContent.includes("gamma"));
      const underlined = (span) => getComputedStyle(span).textDecorationLine.includes("underline");
      return y && gamma && y !== gamma && !underlined(y) && underlined(gamma);
    });
    console.log("CHECK 17 edits inside a note extend its underline; edits after it do not");

    await postJSON(`${origin}/together/v1/structure/${notesWorkspace}/acts`, { op: {
      op: "add-node", id: "blue-note", kind: "span", attrs: { doc: notesKey,
        start: ["joe.seed", 18], end: ["joe.seed", 22], by: "rob", colour: "blue",
        title: "Late note", text: "Added after the page opened.",
        source: { url: "https://example.test/source", words: "A quotation" } },
    } });
    await notesPage.evaluate(() => window.dispatchEvent(new Event("focus")));
    await notesPage.locator("textarea").evaluate((element) => {
      const at = element.value.indexOf("delta"); element.setSelectionRange(at, at);
      element.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    });
    await notesPage.waitForSelector('[data-note-id="blue-note"]');
    const layoutHeights = await notesPage.evaluate(() => ({
      editor: document.querySelector("textarea").scrollHeight,
      layer: document.querySelector(".text-layer").scrollHeight,
    }));
    assert.equal(layoutHeights.editor, layoutHeights.layer);
    console.log("CHECK 18 focus refresh adds a new note and preserves editor/layer alignment");

    const bareKey = `bare-${process.pid}`;
    await seedDocument(origin, bareKey, "No notes here");
    const bare = await joeContext.newPage();
    await bare.goto(`${origin}/together/v1/web/index.html?doc=${bareKey}`, { waitUntil: "domcontentloaded" });
    await waitLive(bare);
    await bare.waitForTimeout(100);
    assert.equal(await bare.locator("#notes-pane").count(), 0);
    console.log("CHECK 19 a document without notes has no pane");
    assert.equal(await bare.locator(".notes-strip, .note-navigation, .lineup-mobile-nav").count(), 0);
    // Lineup retains the existing textarea and its live connection.
    const caret = async (index) => {
      await notesPage.locator("textarea").evaluate((el, at) => {
        el.setSelectionRange(at, at); el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      }, index);
    };
    const right = notesPage.locator('.note-navigation button[title="Show the source in context"]');
    const left = notesPage.locator('.note-navigation button[title="Back to the document"]');
    await caret(0);
    await notesPage.waitForFunction(() => document.querySelector('.note-navigation button:last-child')?.disabled);
    assert.equal(await right.isDisabled(), true);
    await caret(8);
    await notesPage.waitForFunction(() => !document.querySelector('.note-navigation button:last-child')?.disabled);
    const preserved = await notesPage.locator("textarea").evaluate((el) => {
      window.__originalEditor = el;
      return { caret: el.selectionStart, scroll: el.scrollTop, text: el.value };
    });
    const visible = async (selectors, hidden) => notesPage.waitForFunction(({ selectors, hidden }) => {
      const inside = (selector) => {
        const box = document.querySelector(selector).getBoundingClientRect();
        return box.left >= -1 && box.right <= innerWidth + 1 && box.width > 0;
      };
      const box = document.querySelector(hidden).getBoundingClientRect();
      return selectors.every(inside) && (box.right <= 1 || box.left >= innerWidth - 1);
    }, { selectors, hidden });
    await right.click();
    await visible(["#notes-pane", "#source-pane"], ".document-editor");
    await notesPage.waitForFunction(() => {
      const mark = document.querySelector("#source-pane mark");
      if (!mark) return false;
      const box = mark.getBoundingClientRect(); const pane = document.querySelector("#source-pane").getBoundingClientRect();
      return box.top >= pane.top && box.bottom <= pane.bottom;
    });
    assert.equal((await notesPage.locator("#source-pane mark").textContent()).replace(/\s+/g, " "), "beta gamma");
    assert.deepEqual(await notesPage.locator(".note-card").evaluateAll((items) => items.map((item) => item.dataset.noteId)), ["magenta-note", "yellow-note"]);
    await left.click();
    await visible([".document-editor", "#notes-pane"], "#source-pane");
    assert.deepEqual(await notesPage.locator("textarea").evaluate((el) => ({ caret: el.selectionStart, scroll: el.scrollTop, text: el.value })), preserved);
    assert.equal(await notesPage.locator("textarea").evaluate((el) => el === window.__originalEditor), true);
    assert.match(await notesPage.locator(".status-main").textContent(), /live/);
    await notesPage.locator("textarea").focus(); await notesPage.keyboard.type(">");
    assert.equal(await notesPage.locator(".document-layout").getAttribute("data-position"), "0");
    assert.ok((await notesPage.locator("textarea").inputValue()).includes(">"));
    console.log("CHECK 20 desktop lineup, source match, preserved editor and literal >");

    await caret((await notesPage.locator("textarea").inputValue()).indexOf("delta"));
    await notesPage.waitForSelector('[data-note-id="blue-note"] button');
    await notesPage.locator('[data-note-id="blue-note"] button').click();
    const sourceLink = notesPage.locator("#source-pane a");
    assert.equal(await sourceLink.getAttribute("href"), "https://example.test/source");
    assert.equal(await sourceLink.getAttribute("rel"), "noopener noreferrer");
    assert.equal(await sourceLink.getAttribute("target"), "_blank");
    await left.click();
    await postJSON(`${origin}/together/v1/structure/${notesWorkspace}/acts`, { op: {
      op: "add-node", id: "missing-source", kind: "span", attrs: { doc: notesKey,
        start: ["joe.seed", 1], end: ["joe.seed", 2], colour: "red",
        source: { doc: `absent-${process.pid}`, words: "absent" } },
    } });
    await notesPage.evaluate(() => window.dispatchEvent(new Event("focus")));
    await caret(0);
    await notesPage.waitForSelector('[data-note-id="missing-source"] button');
    await notesPage.locator('[data-note-id="missing-source"] button').click();
    await notesPage.waitForFunction(() => document.querySelector("#source-pane")?.textContent.includes("missing or unreadable"));
    await left.click();
    console.log("CHECK 21 URL sources and missing source documents");

    await notesPage.setViewportSize({ width: 390, height: 844 });
    await caret(9);
    await notesPage.locator('.lineup-mobile-nav button', { hasText: "Notes" }).click();
    await visible(["#notes-pane"], ".document-editor").catch(async (error) => {
      console.log("DEBUG", JSON.stringify(await notesPage.evaluate(() => {
        const box = (s) => { const b = document.querySelector(s)?.getBoundingClientRect(); return b && [Math.round(b.left), Math.round(b.right), Math.round(b.top), Math.round(b.bottom)]; };
        const strip = document.querySelector(".notes-strip");
        return { pos: document.querySelector(".document-layout").dataset.position, w: innerWidth, h: innerHeight,
          strip: box(".notes-strip"), transform: strip.style.transform, cols: strip.style.gridTemplateColumns,
          editor: box(".document-editor"), notes: box("#notes-pane"), source: box("#source-pane"), layout: box(".document-layout"), nav: box(".lineup-mobile-nav") };
      })));
      throw error;
    });
    assert.equal(await notesPage.locator(".document-layout").getAttribute("data-position"), "1");
    await right.click();
    await visible(["#source-pane"], "#notes-pane");
    assert.equal(await notesPage.locator(".document-layout").getAttribute("data-position"), "2");
    await notesPage.locator('.lineup-mobile-nav button', { hasText: "Document" }).click();
    await visible([".document-editor"], "#notes-pane");
    assert.equal(await notesPage.locator(".document-layout").getAttribute("data-position"), "0");
    console.log("CHECK 22 narrow lineup has three single-pane positions");
    console.log("SUMMARY 22 checks passed");

    await Promise.all([notesPage.close(), bare.close()]);

    await phoneContext.close();
    await murchContext.close();
    await joeContext.close();
  } finally {
    await browser?.close().catch(() => {});
    hub.kill("SIGTERM");
    await new Promise((resolveExit) => {
      if (hub.exitCode !== null) resolveExit();
      else {
        hub.once("exit", resolveExit);
        setTimeout(() => { hub.kill("SIGKILL"); resolveExit(); }, 3000).unref();
      }
    });
    await rm(directory, { recursive: true, force: true });
  }
}

await main();
