const { test, expect } = require("@playwright/test");
const { spawn } = require("child_process");
const fs = require("fs");
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
      request.setTimeout(5000, () => request.destroy());
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

async function consumeDownload(page) {
  const link = page.locator("#download_data");
  await expect(link).toBeVisible();
  await expect.poll(async () => link.getAttribute("href"), {
    timeout: 30000,
  }).toMatch(/(?:^|\/)session\/[^/]+\/download\/download_data/);
  const [download] = await Promise.all([
    page.waitForEvent("download"),
    link.click(),
  ]);
  expect(await download.failure()).toBeNull();
  const downloadPath = await download.path();
  expect(downloadPath).not.toBeNull();
  const content = fs.readFileSync(downloadPath);
  expect(content.length).toBeGreaterThan(20);
  return { content, filename: download.suggestedFilename() };
}

async function selectizeOption(page, id, value) {
  const control = page.locator(`#${id} + .selectize-control`);
  await control.locator(".selectize-input").click();
  await control.locator(
    `.selectize-dropdown-content [data-value="${value}"]`
  ).click();
  await expect(page.locator(`#${id}`)).toHaveValue(value);
}

test.beforeAll(async () => {
  test.setTimeout(90000);
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
  const popups = [];
  page.on("console", (message) => {
    if (message.type() === "error" &&
        /Error in|package .* not found|there is no package|pinned build requires/i.test(message.text())) {
      severeErrors.push(message.text());
    }
  });
  page.on("pageerror", (error) => severeErrors.push(error.message));
  page.on("popup", (popup) => popups.push(popup.url()));

  await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("#file")).toBeAttached({ timeout: 60000 });
  await expect(page.getByText("Min-Max Normalize", { exact: true })).toBeVisible();
  await expect(page.locator("#placeholder1")).toBeVisible();
  await expect(page.locator("#heatmap_frame")).toBeHidden();
  await expect(page.locator("#MyPlotC.js-plotly-plot .main-svg").first()).toBeVisible({ timeout: 60000 });
  await expect(page.locator("#MyPlotC .xaxislayer-above")).toBeAttached();
  await expect(page.locator("#MyPlotC .yaxislayer-above")).toBeAttached();
  expect(await nonemptyTraces(page)).toEqual([]);
  await expect(page.locator("#openspecy_busy_overlay")).toBeHidden();
  await expect(page.locator("#download_selection")).toHaveValue("Test Data");
  const testDataDownload = await consumeDownload(page);
  expect(testDataDownload.filename).toMatch(/^Test-Data-.*\.csv$/i);
  const testDataText = testDataDownload.content.toString("utf8");
  expect(testDataText.split(/\r?\n/)[0]).toMatch(/wavenumber,intensity/i);
  expect(testDataText.split(/\r?\n/).filter(Boolean).length).toBeGreaterThan(100);

  await selectizeOption(page, "download_selection", "Test Map");
  const testMapDownload = await consumeDownload(page);
  expect(testMapDownload.filename).toMatch(/^Test-Map-.*\.zip$/i);
  expect(testMapDownload.content.subarray(0, 2).toString("ascii")).toBe("PK");
  await page.screenshot({ path: testInfo.outputPath("local-app-empty-spectrum.png"), fullPage: true });

  await page.evaluate(() => {
    window.__openspecySmoke = {
      phases: [], elapsed: [], progress: [], visible: [], progressNodes: 0,
    };
    const record = () => {
      const state = window.__openspecySmoke;
      const html = document.documentElement;
      const phase = document.getElementById("openspecy_busy_message")?.textContent || "";
      const elapsed = document.getElementById("openspecy_busy_elapsed")?.textContent || "";
      if (phase && state.phases[state.phases.length - 1] !== phase) state.phases.push(phase);
      if (elapsed && state.elapsed[state.elapsed.length - 1] !== elapsed) state.elapsed.push(elapsed);
      const showing = html.classList.contains("openspecy-busy-visible");
      const progress = Number(document.getElementById("openspecy_busy_progress")
        ?.getAttribute("aria-valuenow"));
      if (showing && Number.isFinite(progress) &&
          state.progress[state.progress.length - 1] !== progress) {
        state.progress.push(progress);
      }
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
  const progressBar = page.locator("#openspecy_busy_progress");
  await expect(progressBar).toHaveAttribute("role", "progressbar");
  await expect.poll(async () => Number(await progressBar.getAttribute("aria-valuenow")))
    .toBeGreaterThan(0);
  await page.screenshot({ path: testInfo.outputPath("local-app-analysis-progress.png"), fullPage: true });

  const firstMatch = page.locator("#event table tbody tr").first();
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect(page.locator("#MyPlotC.js-plotly-plot .main-svg").first()).toBeVisible();
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(2);
  const traces = await nonemptyTraces(page);
  expect(traces.every((trace) => trace.points > 100)).toBe(true);
  expect(traces.some((trace) => trace.dash === "dot")).toBe(true);
  await expect(page.locator("#eventmetadata table")).toBeVisible();
  await expect(page.locator("#heatmap_frame")).toBeHidden();
  await expect(page.locator(".shiny-output-error:visible")).toHaveCount(0);
  await expect(overlay).toBeHidden({ timeout: 30000 });
  await page.waitForTimeout(2200);
  await expect(overlay).toBeHidden();

  const progressState = await page.evaluate(() => window.__openspecySmoke);
  expect(progressState.progressNodes).toBe(0);
  expect(progressState.phases.length).toBeGreaterThanOrEqual(3);
  expect(progressState.phases.join(" ")).toMatch(/Preprocessing|reference library|Identifying|Rendering/i);
  expect(progressState.elapsed.length).toBeGreaterThanOrEqual(2);
  expect(progressState.progress.length).toBeGreaterThanOrEqual(2);
  expect(Math.max(...progressState.progress)).toBeGreaterThanOrEqual(76);
  expect(progressState.progress.every((value) => value >= 0 && value <= 100)).toBe(true);
  await testInfo.attach("progress-state", {
    body: JSON.stringify(progressState, null, 2),
    contentType: "application/json",
  });
  await expect(page.locator("#download_selection")).toHaveValue("Top Matches");
  const topMatchDetails = page.locator("details.openspecy-download-details");
  await expect(topMatchDetails).toBeVisible();
  await expect(topMatchDetails).not.toHaveAttribute("open", "");
  await topMatchDetails.locator("summary").click();
  await page.locator("#top_n_input").fill("3");
  await selectizeOption(page, "columns_selected", "Simple");
  const topMatchesDownload = await consumeDownload(page);
  expect(topMatchesDownload.filename).toMatch(/^Top-Matches-.*\.csv$/i);
  const topMatchesText = topMatchesDownload.content.toString("utf8");
  const topMatchLines = topMatchesText.split(/\r?\n/).filter(Boolean);
  expect(topMatchLines[0]).toMatch(/file_name.*col_id.*material_class.*match_val.*signal_to_noise/i);
  expect(topMatchLines.length).toBe(4);
  expect(topMatchesText).toMatch(/poly\(ethylene\)/i);
  await page.screenshot({ path: testInfo.outputPath("local-app-analysis-result.png"), fullPage: true });

  await page.getByRole("link", { name: "Identification", exact: true }).click();
  await page.locator("#active_identification").uncheck({ force: true });
  await expect.poll(() => nonemptyTraces(page), { timeout: 60000 }).toHaveLength(1);
  await expect(page.locator("#event")).toBeHidden();
  await expect(page.locator("#download_selection")).toHaveValue("Processed Spectra");
  const processedDownload = await consumeDownload(page);
  expect(processedDownload.filename).toMatch(/^Processed-Spectra-.*\.csv$/i);
  const processedText = processedDownload.content.toString("utf8");
  expect(processedText).toMatch(/signal_to_noise/i);
  expect(processedText).toMatch(/raman_hdpe/i);
  await page.locator("#active_identification").check({ force: true });
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(2);

  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.waitForTimeout(750);
  await page.screenshot({ path: testInfo.outputPath("local-app-mobile.png"), fullPage: true });
  expect(popups).toEqual([]);
  expect(severeErrors).toEqual([]);
  expect(stderr).not.toMatch(/Warning: Error in|Execution halted/);
});
