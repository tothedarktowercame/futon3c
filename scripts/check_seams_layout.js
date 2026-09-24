#!/usr/bin/env node
// check_seams_layout.js — the acceptance test for seams.html's figures.
//
//   node scripts/check_seams_layout.js [url] [screens-dir]
//
// Joe's measure: a figure's label text must render at the same size as the
// body text beside it. That is checked here rather than eyeballed, at four
// viewport widths, together with two things a reader notices immediately --
// a figure wider than its column, and a figure sitting on top of a note.
//
// Prints a table and exits nonzero on any overflow, overlap, or text-size
// miss outside the tolerance.
// Playwright is borrowed, not installed: nothing is added to futon3c. The
// default is where it lives on Zone; PLAYWRIGHT_PATH overrides it, because a
// host path baked into a script is instance 1 of the mission this very page
// is about, and writing one without an override would be poor form.
const PW = process.env.PLAYWRIGHT_PATH
  || '/home/joe/code/excitement-to-evidence/node_modules/playwright';
const { chromium } = require(PW);
const path = require('path');

const URL = process.argv[2] || 'file:///var/www/zone.hyperreal.enterprises/wip/seams.html';
const SHOTS = process.argv[3] || '/home/joe/code/futon3c/holes/labs/M-futon-seams/screens';
const WIDTHS = [420, 1280, 1600, 1920];
const TOLERANCE = 0.10;

(async () => {
  const browser = await chromium.launch({ headless: true });
  const rows = [];
  let fails = 0;

  for (const width of WIDTHS) {
    const page = await browser.newPage({ viewport: { width, height: 1100 } });
    await page.goto(URL, { waitUntil: 'load' });
    await page.waitForTimeout(350);          // let layout() run and settle
    await page.screenshot({
      path: path.join(SHOTS, `seams-${width}.png`), fullPage: false });

    const measured = await page.evaluate(() => {
      const bodyPx = parseFloat(getComputedStyle(
        document.querySelector('.main p')).fontSize);
      const margin = document.querySelector('.margin');
      const mBox = margin.getBoundingClientRect();
      const wide = getComputedStyle(margin).display !== 'none';
      const notes = [...document.querySelectorAll('.note')].map(n => {
        const r = n.getBoundingClientRect();
        return { top: r.top + scrollY, bottom: r.bottom + scrollY,
                 left: r.left, right: r.right, id: n.id };
      });
      return {
        bodyPx, wide,
        docOverflow: document.documentElement.scrollWidth
                     - document.documentElement.clientWidth,
        figures: [...document.querySelectorAll('.marginfig')].map(f => {
          const svg = f.querySelector('svg');
          const vb = svg.getAttribute('viewBox').split(' ').map(Number);
          const r = svg.getBoundingClientRect();
          const label = svg.querySelector('.nid, .wid');
          const labelCss = label ? parseFloat(getComputedStyle(label).fontSize) : null;
          const scale = r.width / vb[2];
          const fr = f.getBoundingClientRect();
          const overlaps = notes.filter(n =>
            !(fr.bottom + scrollY <= n.top || fr.top + scrollY >= n.bottom
              || fr.right <= n.left || fr.left >= n.right)).map(n => n.id);
          return {
            id: f.id, natural: Number(f.dataset.naturalWidth),
            rendered: Math.round(r.width), labelCss,
            labelPx: labelCss ? +(labelCss * scale).toFixed(2) : null,
            pastMargin: Math.round(fr.right - mBox.right),
            overlaps
          };
        })
      };
    });

    for (const f of measured.figures) {
      const ratio = f.labelPx / measured.bodyPx;
      const miss = measured.wide && Math.abs(ratio - 1) > TOLERANCE;
      const over = measured.wide && f.pastMargin > 1;
      const lap = f.overlaps.length > 0;
      if (miss || over || lap) fails++;
      rows.push({
        width, figure: f.id, rendered: f.rendered, natural: f.natural,
        labelPx: f.labelPx, bodyPx: measured.bodyPx,
        ratio: +ratio.toFixed(3),
        verdict: !measured.wide ? 'folded' :
                 [miss ? 'TEXT-SIZE' : null, over ? 'OVERFLOW' : null,
                  lap ? 'OVERLAP' : null].filter(Boolean).join('+') || 'ok'
      });
    }
    if (measured.docOverflow > 1) {
      fails++;
      rows.push({ width, figure: '(document)', rendered: '', natural: '',
                  labelPx: '', bodyPx: '', ratio: '',
                  verdict: `H-SCROLL ${measured.docOverflow}px` });
    }
    await page.close();
  }

  await browser.close();
  console.log('width  figure    rendered  natural  label px  body px  ratio  verdict');
  for (const r of rows) {
    console.log(
      String(r.width).padEnd(7) + String(r.figure).padEnd(10) +
      String(r.rendered).padEnd(10) + String(r.natural).padEnd(9) +
      String(r.labelPx).padEnd(10) + String(r.bodyPx).padEnd(9) +
      String(r.ratio).padEnd(7) + r.verdict);
  }
  console.log(`\n${rows.length} measurements, ${fails} failing (tolerance ±${TOLERANCE * 100}%)`);
  process.exit(fails ? 1 : 0);
})();
