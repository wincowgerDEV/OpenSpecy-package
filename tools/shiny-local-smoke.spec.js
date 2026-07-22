const { test, expect } = require("@playwright/test");
const { spawn } = require("child_process");
const http = require("http");
const path = require("path");

// Run with the action-cached Playwright installation:
// $env:NODE_PATH = (Resolve-Path '_wasm/action-sim/node/node_modules').Path
// & '_wasm/action-sim/node/node_modules/.bin/playwright.cmd' test \
//   tools/shiny-local-smoke.spec.js --workers=1

const repo = path.resolve(__dirname, "..");
const port = Number(process.env.OPENSPECY_LOCAL_SMOKE_PORT || 4542);
let app;
let stderr = "";

function waitForApp(timeout = 60000) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    const poll = () => {
      const request = http.get(`http://127.0.0.1:${port}`, (response) => {
        response.resume();
        if (response.statusCode === 200) return resolve();
        setTimeout(poll, 250);
      });
      request.on("error", () => {
        if (Date.now() - started > timeout) {
          reject(new Error(`Local Shiny app did not become ready:\n${stderr}`));
        } else {
          setTimeout(poll, 250);
        }
      });
    };
    poll();
  });
}

function nonemptyTraces(page) {
  return page.locator("#MyPlotC").evaluate((plot) =>
    (plot.data || []).filter((trace) =>
      Array.isArray(trace.x) && Array.isArray(trace.y) &&
      trace.x.length > 100 && trace.y.length > 100
    ).map((trace) => ({
      points: trace.x.length,
      dash: trace.line && trace.line.dash,
    }))
  );
}

test.beforeAll(async () => {
  const expression = [
    `devtools::load_all(${JSON.stringify(repo.replace(/\\/g, "/"))}, quiet=TRUE)`,
    `shiny::runApp(${JSON.stringify(path.join(repo, "inst", "shiny").replace(/\\/g, "/"))}, host='127.0.0.1', port=${port}, launch.browser=FALSE)`,
  ].join("; ");
  app = spawn("C:/Program Files/R/R-4.3.3/bin/Rscript.exe", ["-e", expression], {
    cwd: repo,
    env: {
      ...process.env,
      OPENSPECY_SHINY_LIBRARY_PATH:
        process.env.OPENSPECY_SHINY_LIBRARY_PATH ||
        "C:/Users/winco/AppData/Local/R/cache/R/OpenSpecy/reference_libraries",
    },
    stdio: ["ignore", "pipe", "pipe"],
    windowsHide: true,
  });
  app.stderr.on("data", (chunk) => { stderr += chunk.toString(); });
  await waitForApp();
});

test.afterAll(() => {
  if (app && !app.killed) app.kill();
});

test("local app renders spectra, matches, and one informative progress overlay", async ({ page }, testInfo) => {
  test.setTimeout(300000);
  const severeErrors = [];
  page.on("console", (message) => {
    if (message.type() === "error" &&
        /Error in|package .* not found|there is no package|pinned build requires/i.test(message.text())) {
      severeErrors.push(message.text());
    }
  });
  page.on("pageerror", (error) => severeErrors.push(error.message));

  await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("#file")).toBeAttached({ timeout: 60000 });
  await expect(page.locator("#placeholder1")).toBeVisible();
  await expect(page.locator("#MyPlotC.js-plotly-plot .main-svg").first()).toBeVisible({ timeout: 60000 });
  await expect(page.locator("#MyPlotC .xaxislayer-above")).toBeAttached();
  await expect(page.locator("#MyPlotC .yaxislayer-above")).toBeAttached();
  expect(await nonemptyTraces(page)).toEqual([]);
  await expect(page.locator("#openspecy_busy_overlay")).toBeHidden();
  await page.screenshot({ path: testInfo.outputPath("local-app-empty-spectrum.png"), fullPage: true });

  await page.evaluate(() => {
    window.__openspecySmoke = {
      phases: [], elapsed: [], eta: [], visible: [], progressNodes: 0,
    };
    const record = () => {
      const state = window.__openspecySmoke;
      const html = document.documentElement;
      const phase = document.getElementById("openspecy_busy_message")?.textContent || "";
      const elapsed = document.getElementById("openspecy_busy_elapsed")?.textContent || "";
      const eta = document.getElementById("openspecy_busy_eta")?.textContent || "";
      if (phase && state.phases[state.phases.length - 1] !== phase) state.phases.push(phase);
      if (elapsed && state.elapsed[state.elapsed.length - 1] !== elapsed) state.elapsed.push(elapsed);
      if (eta && state.eta[state.eta.length - 1] !== eta) state.eta.push(eta);
      const showing = html.classList.contains("openspecy-busy-visible");
      if (state.visible[state.visible.length - 1] !== showing) state.visible.push(showing);
    };
    window.__openspecySmokeObserver = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        for (const node of mutation.addedNodes || []) {
          if (node.nodeType === 1 &&
              (node.matches?.(".shiny-progress-container, .shiny-progress-notification") ||
               node.querySelector?.(".shiny-progress-container, .shiny-progress-notification"))) {
            window.__openspecySmoke.progressNodes += 1;
          }
        }
      }
      record();
    });
    window.__openspecySmokeObserver.observe(document.documentElement, {
      attributes: true, childList: true, subtree: true, characterData: true,
    });
    record();
  });

  await page.locator("#file").setInputFiles(
    path.join(repo, "inst", "extdata", "raman_hdpe.csv")
  );
  const overlay = page.locator("#openspecy_busy_overlay");
  await expect(overlay).toBeVisible({ timeout: 30000 });
  const elapsedBefore = await page.locator("#openspecy_busy_elapsed").textContent();
  await page.waitForTimeout(1200);
  const elapsedAfter = await page.locator("#openspecy_busy_elapsed").textContent();
  expect(elapsedAfter).not.toEqual(elapsedBefore);
  await expect(page.locator("#openspecy_busy_eta")).not.toHaveText("");
  await page.screenshot({ path: testInfo.outputPath("local-app-analysis-progress.png"), fullPage: true });

  const firstMatch = page.locator("#event table tbody tr").first();
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect(page.locator("#MyPlotC.js-plotly-plot .main-svg").first()).toBeVisible();
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(2);
  const traces = await nonemptyTraces(page);
  expect(traces.every((trace) => trace.points > 100)).toBe(true);
  expect(traces.some((trace) => trace.dash === "dot")).toBe(true);
  await expect(page.locator("#eventmetadata table")).toBeVisible();
  await expect(page.locator(".shiny-output-error:visible")).toHaveCount(0);
  await expect(overlay).toBeHidden({ timeout: 30000 });
  await page.waitForTimeout(2200);
  await expect(overlay).toBeHidden();

  const progressState = await page.evaluate(() => window.__openspecySmoke);
  expect(progressState.progressNodes).toBe(0);
  expect(progressState.phases.length).toBeGreaterThanOrEqual(3);
  expect(progressState.phases.join(" ")).toMatch(/Preprocessing|reference library|Identifying|Rendering/i);
  expect(progressState.elapsed.length).toBeGreaterThanOrEqual(2);
  expect(progressState.eta.some((text) => /Estimated remaining|still working/i.test(text))).toBe(true);
  await testInfo.attach("progress-state", {
    body: JSON.stringify(progressState, null, 2),
    contentType: "application/json",
  });
  await expect(page.locator("#download_selection")).toHaveValue("Top Matches");
  await page.screenshot({ path: testInfo.outputPath("local-app-analysis-result.png"), fullPage: true });

  await page.locator("#active_identification").uncheck({ force: true });
  await expect.poll(() => nonemptyTraces(page), { timeout: 60000 }).toHaveLength(1);
  await expect(page.locator("#event")).toBeHidden();
  await expect(page.locator("#download_selection")).toHaveValue("Processed Spectra");
  await page.locator("#active_identification").check({ force: true });
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(2);

  await page.setViewportSize({ width: 390, height: 844 });
  await page.screenshot({ path: testInfo.outputPath("local-app-mobile.png"), fullPage: true });
  expect(severeErrors).toEqual([]);
  expect(stderr).not.toMatch(/Warning: Error in|Execution halted/);
});
