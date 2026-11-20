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

    await phone.waitForFunction(() => document.querySelector("textarea")?.value.endsWith("RECONNECT_ONCE"));
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
    console.log("SUMMARY 9 checks passed");

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
