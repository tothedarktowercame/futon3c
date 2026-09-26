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
    // Element shots too: a viewport shot at 1600 shows the mission text and no
    // figure, so it cannot support a visual check of the figures themselves.
    if (width === 1600) {
      for (const f of await page.$$('.marginfig, .pagefig')) {
        const id = await f.getAttribute('id');
        await f.screenshot({ path: path.join(SHOTS, `${id}.png`) });
      }
    }

    const measured = await page.evaluate(() => {
      // A MISSION paragraph: a direct child of .main. Phase and table-of-
      // contents paragraphs are nested and set smaller, and measuring one of
      // those compares a figure against the wrong text.
      const bodyPx = parseFloat(getComputedStyle(
        document.querySelector('.main > p')).fontSize);
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
        // Every <text> in every figure, by class: Joe's measure is about the
        // text in the image, and measuring only the node titles let ports at
        // 10px pass a check the titles passed at 16.6px.
        textClasses: (() => {
          const byClass = {};
          for (const svg of document.querySelectorAll('.marginfig svg, .pagefig svg')) {
            const vb = svg.getAttribute('viewBox').split(' ').map(Number);
            const scale = svg.getBoundingClientRect().width / vb[2];
            for (const t of svg.querySelectorAll('text')) {
              const cls = t.getAttribute('class') || '(none)';
              const px = +(parseFloat(getComputedStyle(t).fontSize) * scale).toFixed(2);
              (byClass[cls] ||= { px, n: 0 }).n++;
            }
          }
          return byClass;
        })(),
        // Text that runs past the box it labels, and text that overprints
        // other text inside one figure.
        clipped: [...document.querySelectorAll('.marginfig svg g, .pagefig svg g')].flatMap(g => {
          const rect = g.querySelector('rect');
          if (!rect) return [];
          const rb = rect.getBBox();
          return [...g.querySelectorAll('text')].filter(t => {
            const tb = t.getBBox();
            return tb.x + tb.width > rb.x + rb.width + 0.5 || tb.x < rb.x - 0.5;
          }).map(t => ({
            fig: g.closest('figure').id,
            text: t.textContent.slice(0, 28),
            over: +(t.getBBox().x + t.getBBox().width - rb.x - rb.width).toFixed(1)
          }));
        }),
        overlaps: (() => {
          const bad = [];
          for (const svg of document.querySelectorAll('.marginfig svg, .pagefig svg')) {
            const ts = [...svg.querySelectorAll('text')].map(t => ({
              el: t, b: t.getBBox(), s: t.textContent }));
            for (let i = 0; i < ts.length; i++)
              for (let j = i + 1; j < ts.length; j++) {
                const a = ts[i].b, b = ts[j].b;
                const ox = Math.min(a.x + a.width, b.x + b.width) - Math.max(a.x, b.x);
                const oy = Math.min(a.y + a.height, b.y + b.height) - Math.max(a.y, b.y);
                if (ox > 1 && oy > 1)
                  bad.push({ fig: svg.closest('figure').id,
                             a: ts[i].s.slice(0, 20), b: ts[j].s.slice(0, 20) });
              }
          }
          return bad;
        })(),
        figures: [...document.querySelectorAll('.marginfig, .pagefig')].map(f => {
          const svg = f.querySelector('svg');
          const vb = svg.getAttribute('viewBox').split(' ').map(Number);
          const r = svg.getBoundingClientRect();
          const label = svg.querySelector('.nid, .wid');
          const labelCss = label ? parseFloat(getComputedStyle(label).fontSize) : null;
          const scale = r.width / vb[2];
          const fr = f.getBoundingClientRect();
          const inMargin = f.classList.contains('marginfig');
          const overlaps = notes.filter(n =>
            !(fr.bottom + scrollY <= n.top || fr.top + scrollY >= n.bottom
              || fr.right <= n.left || fr.left >= n.right)).map(n => n.id);
          return {
            id: f.id, natural: Number(f.dataset.naturalWidth),
            rendered: Math.round(r.width), labelCss,
            labelPx: labelCss ? +(labelCss * scale).toFixed(2) : null,
            inMargin,
            pastMargin: inMargin ? Math.round(fr.right - mBox.right)
                                 : Math.round(fr.right - document.documentElement.clientWidth),
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
    if (width === 1600) {
      console.log('\ntext classes at 1600px (body is ' + measured.bodyPx + 'px):');
      for (const [cls, v] of Object.entries(measured.textClasses).sort())
        console.log('  ' + cls.padEnd(10) + String(v.px).padEnd(8)
                    + 'x' + String(v.n).padEnd(5)
                    + (Math.abs(v.px / measured.bodyPx - 1) <= TOLERANCE
                       ? 'body size' : 'secondary'));
      console.log('');
    }
    for (const o of measured.overlaps) {
      fails++;
      rows.push({ width, figure: o.fig, rendered: '', natural: '', labelPx: '',
                  bodyPx: '', ratio: '',
                  verdict: `TEXT-OVERLAP "${o.a}" / "${o.b}"` });
    }
    for (const c of measured.clipped) {
      fails++;
      rows.push({ width, figure: c.fig, rendered: '', natural: '', labelPx: '',
                  bodyPx: '', ratio: '',
                  verdict: `TEXT-CLIPPED "${c.text}" by ${c.over}px` });
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
