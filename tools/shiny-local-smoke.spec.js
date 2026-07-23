const { test, expect } = require("@playwright/test");
const { spawn } = require("child_process");
const fs = require("fs");
const http = require("http");
const net = require("net");
const path = require("path");

// Run with the action-cached Playwright installation:
// $env:NODE_PATH = (Resolve-Path '_wasm/action-sim/node/node_modules').Path
// & '_wasm/action-sim/node/node_modules/.bin/playwright.cmd' test \
//   tools/shiny-local-smoke.spec.js --workers=1

const repo = path.resolve(__dirname, "..");
let port = Number(process.env.OPENSPECY_LOCAL_SMOKE_PORT || 0);
let app;
let stderr = "";

function findFreePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      server.close(() => resolve(address.port));
    });
  });
}

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
      color: trace.line && trace.line.color,
    }))
  );
}

async function expectEnabledSwitchColors(page, inputId) {
  const input = page.locator(`#${inputId}`);
  await expect(input).toBeChecked();
  const readColors = () => input.evaluate((element) => {
    const state = element.parentElement.querySelector(".state");
    const label = state && state.querySelector("label");
    return {
      track: state ? getComputedStyle(state, "::before").backgroundColor : "",
      knob: label ? getComputedStyle(label, "::after").backgroundColor : "",
    };
  });
  await expect.poll(async () => {
    const track = rgbChannels((await readColors()).track);
    return track ? track[1] - Math.max(track[0], track[2]) : -1000;
  }).toBeGreaterThan(40);
  await expect.poll(async () => {
    const knob = rgbChannels((await readColors()).knob);
    return knob ? Math.min(...knob) : -1000;
  }).toBeGreaterThanOrEqual(245);
}

async function expectInformationalDetails(page) {
  const disclosures = page.locator("details.openspecy-info-details");
  expect(await disclosures.count()).toBeGreaterThanOrEqual(16);
  const results = await disclosures.evaluateAll((items) => items.map((item) => {
    item.open = true;
    const summary = item.querySelector("summary");
    const body = item.querySelector(".openspecy-info-details-body");
    const result = {
      summary: (summary?.textContent || "").trim(),
      body: (body?.textContent || "").trim(),
      open: item.open,
    };
    item.open = false;
    return result;
  }));
  expect(results.every((item) => item.open && item.summary && item.body.length >= 20))
    .toBe(true);
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

async function expectCardCollapsed(card, collapsed = true) {
  await expect(card).toBeVisible();
  if (collapsed) {
    await expect(card).toHaveClass(/collapsed-card/);
    await expect(card.locator(":scope > .card-body")).toBeHidden();
  } else {
    await expect(card).not.toHaveClass(/collapsed-card/);
    await expect(card.locator(":scope > .card-body")).toBeVisible();
  }
}

async function toggleCard(card) {
  const toggle = card.locator(
    ':scope > .card-header [data-card-widget="collapse"]'
  );
  await expect(toggle).toBeVisible();
  await toggle.click();
}

async function expectEqualWidthAndTop(first, second, tolerance = 2) {
  const [firstBox, secondBox] = await Promise.all([
    first.boundingBox(),
    second.boundingBox(),
  ]);
  expect(firstBox).not.toBeNull();
  expect(secondBox).not.toBeNull();
  expect(Math.abs(firstBox.width - secondBox.width)).toBeLessThanOrEqual(tolerance);
  expect(Math.abs(firstBox.y - secondBox.y)).toBeLessThanOrEqual(tolerance);
}

async function expectSummaryRowsFilled(page, mobile = false) {
  const layout = await page.locator("#analysis_summary_box").evaluate((summary) => {
    const grids = Array.from(summary.querySelectorAll(".openspecy-summary-grid"));
    return grids.map((grid) => {
      const gridBox = grid.getBoundingClientRect();
      const panels = Array.from(grid.querySelectorAll(".openspecy-summary-panel"))
        .filter((panel) => {
          const style = getComputedStyle(panel);
          const box = panel.getBoundingClientRect();
          return style.display !== "none" && style.visibility !== "hidden" &&
            box.width > 0 && box.height > 0;
        })
        .map((panel) => {
          const box = panel.getBoundingClientRect();
          return { left: box.left, right: box.right, top: box.top, width: box.width };
        });
      const rows = [];
      for (const panel of panels.sort((a, b) => a.top - b.top || a.left - b.left)) {
        let row = rows.find((candidate) => Math.abs(candidate.top - panel.top) <= 4);
        if (!row) {
          row = { top: panel.top, panels: [] };
          rows.push(row);
        }
        row.panels.push(panel);
      }
      return {
        width: gridBox.width,
        rows: rows.map((row) => ({
          coverage: (
            Math.max(...row.panels.map((panel) => panel.right)) -
            Math.min(...row.panels.map((panel) => panel.left))
          ) / gridBox.width,
          panelRatios: row.panels.map((panel) => panel.width / gridBox.width),
        })),
      };
    });
  });
  expect(layout.length).toBeGreaterThan(0);
  expect(layout.every((grid) => grid.width > 100 && grid.rows.length > 0)).toBe(true);
  expect(layout.flatMap((grid) => grid.rows)
    .every((row) => row.coverage >= 0.9)).toBe(true);
  if (mobile) {
    expect(layout.flatMap((grid) => grid.rows)
      .flatMap((row) => row.panelRatios)
      .every((ratio) => ratio >= 0.9)).toBe(true);
  }
}

async function resetProgressProbe(page) {
  await page.evaluate(() => {
    if (!window.__openspecySmoke) return;
    const phase = document.getElementById("openspecy_busy_message")?.textContent || "";
    window.__openspecySmoke.phases = phase ? [phase] : [];
    window.__openspecySmoke.elapsed = [];
    window.__openspecySmoke.progress = [];
    window.__openspecySmoke.visible = [
      document.documentElement.classList.contains("openspecy-busy-visible")
    ];
  });
}

function rgbChannels(value) {
  const match = String(value).match(/rgba?\(\s*(\d+)[, ]+\s*(\d+)[, ]+\s*(\d+)/i);
  return match ? match.slice(1, 4).map(Number) : null;
}

async function expectDarkBlueSurface(locator) {
  const colors = await locator.evaluate((element) => {
    let node = element;
    let background = null;
    while (node && !background) {
      const value = getComputedStyle(node).backgroundColor;
      if (value && value !== "transparent" && !/rgba\([^)]*,\s*0\s*\)$/.test(value)) {
        background = value;
      }
      node = node.parentElement;
    }
    const style = getComputedStyle(element);
    return {
      background,
      border: style.borderTopColor,
      color: style.color,
    };
  });
  const background = rgbChannels(colors.background);
  expect(background, `No opaque background found for ${await locator.evaluate((el) => el.outerHTML.slice(0, 120))}`)
    .not.toBeNull();
  expect(Math.max(...background)).toBeLessThan(130);
  expect(background[2] - background[0]).toBeGreaterThanOrEqual(15);
}

async function expectBlueBorder(locator) {
  const value = await locator.evaluate((element) =>
    getComputedStyle(element).borderTopColor
  );
  const border = rgbChannels(value);
  expect(border).not.toBeNull();
  expect(border[2] - border[0]).toBeGreaterThanOrEqual(25);
  expect(border[2] - border[1]).toBeGreaterThanOrEqual(20);
}

test.beforeAll(async () => {
  test.setTimeout(90000);
  if (!Number.isInteger(port) || port < 1) port = await findFreePort();
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
  const minMaxControl = page.getByText("Min-Max Normalize", { exact: true });
  await expect(minMaxControl).toBeHidden();
  await expect(page.locator("#placeholder1")).toBeVisible();
  await expect(page.locator("#heatmap_frame")).toBeHidden();
  await expect(page.locator("#MyPlotC.js-plotly-plot .main-svg").first()).toBeVisible({ timeout: 60000 });
  await expect(page.locator("#MyPlotC .xaxislayer-above")).toBeAttached();
  await expect(page.locator("#MyPlotC .yaxislayer-above")).toBeAttached();
  expect(await nonemptyTraces(page)).toEqual([]);
  await expect(page.locator("#openspecy_busy_overlay")).toBeHidden();
  await expect(page.locator("#help_switch, #theme_switch")).toHaveCount(0);

  const settingsCard = page.locator("#analysis_settings_box");
  const downloadCard = page.locator("#download_panel_box");
  await expectCardCollapsed(settingsCard);
  await expectCardCollapsed(downloadCard);
  await expectEqualWidthAndTop(settingsCard, downloadCard);
  await expect(page.locator(
    '[data-toggle="popover"], [data-bs-toggle="popover"], .popover'
  )).toHaveCount(0);

  for (const tabName of ["Preprocessing", "Identification", "Advanced", "Quantification"]) {
    const tab = page.getByRole("link", { name: tabName, exact: true });
    await expect(tab).toBeVisible();
    await tab.click();
    await expectCardCollapsed(settingsCard, false);
    await expect(tab).toHaveClass(/active/);
    if (tabName === "Preprocessing") await expect(minMaxControl).toBeVisible();
    await toggleCard(settingsCard);
    await expectCardCollapsed(settingsCard);
  }

  await page.getByRole("link", { name: "Quantification", exact: true }).click();
  await expectCardCollapsed(settingsCard, false);
  await expect(page.locator("#active_quantification")).not.toBeChecked();
  await expect(page.locator("#quant_ratio_name")).toBeVisible();
  await expect(page.locator("#quant_ratio_add")).toBeVisible();
  await expect(page.locator("#quant_ratio_bounds")).toContainText(/Upload spectra/i);
  await expectInformationalDetails(page);
  await expectEnabledSwitchColors(page, "active_preprocessing");
  await toggleCard(settingsCard);
  await expectCardCollapsed(settingsCard);

  const downloadButtonStyle = await page.locator("#download_data").evaluate((button) => {
    const box = button.getBoundingClientRect();
    return {
      width: box.width,
      whiteSpace: getComputedStyle(button).whiteSpace,
      gap: parseFloat(getComputedStyle(button).columnGap || getComputedStyle(button).gap),
    };
  });
  expect(downloadButtonStyle.width).toBeGreaterThanOrEqual(270);
  expect(downloadButtonStyle.whiteSpace).toBe("nowrap");
  expect(downloadButtonStyle.gap).toBeGreaterThanOrEqual(8);

  const supportButton = page.locator("#support_openspecy");
  await expect(supportButton).toBeVisible();
  await expect(page.locator(".main-header .navbar-right #support_openspecy"))
    .toHaveCount(1);
  await expect(supportButton).toContainText("Support Open Source Software");
  await supportButton.click();
  const donationModal = page.locator(".modal-content");
  await expect(donationModal).toBeVisible();
  await expect(donationModal).toContainText("Help Support Us!");
  await expect(donationModal.locator("a.openspecy-donation-link")).toHaveCount(6);
  await expect(donationModal.locator("a.openspecy-donation-link").first())
    .toHaveAttribute("href", /^https:\/\/www\.paypal\.com\/donate\//);
  await donationModal.getByRole("button", { name: "Close", exact: true }).click();
  await expect(donationModal).toBeHidden();

  await expect(page.locator("#download_selection")).toHaveValue("Test Data");
  await expect(page.locator("#download_data")).toHaveText("Download Test Data");
  const testDataDownload = await consumeDownload(page);
  expect(testDataDownload.filename).toMatch(/^Test-Data-.*\.csv$/i);
  const testDataText = testDataDownload.content.toString("utf8");
  expect(testDataText.split(/\r?\n/)[0]).toMatch(/wavenumber,intensity/i);
  expect(testDataText.split(/\r?\n/).filter(Boolean).length).toBeGreaterThan(100);

  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard, false);
  await selectizeOption(page, "download_selection", "Test Map");
  await expect(page.locator("#download_data")).toHaveText("Download Test Map");
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard);
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

  const ramanText = fs.readFileSync(
    path.join(repo, "inst", "extdata", "raman_hdpe.csv"),
    "utf8"
  );
  const ramanBatch = ramanText.split(/\r?\n/).map((line, index) => {
    if (!line) return line;
    if (index === 0) return `${line},duplicate_intensity`;
    return `${line},${line.split(",")[1]}`;
  }).join("\n");
  await page.locator("#file").setInputFiles({
    name: "raman_hdpe_batch.csv",
    mimeType: "text/csv",
    buffer: Buffer.from(ramanBatch, "utf8"),
  });
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
  const processedTrace = traces.find((trace) => trace.dash !== "dot");
  expect(processedTrace).toBeDefined();
  expect(String(processedTrace.color).toUpperCase()).toMatch(/#FFF(?:FFF)?|RGB\(255[, ]+255[, ]+255\)/);
  await expect(page.locator("#eventmetadata table")).toBeVisible();
  await expect(page.locator("#heatmap_frame")).toBeVisible();
  await expect(page.locator("#collapse_decision")).not.toBeChecked();
  const spectraCard = page.locator("#spectra_box");
  const summaryCard = page.locator("#analysis_summary_box");
  await expect(summaryCard).toBeVisible();
  const [spectraBox, summaryBox, viewport] = await Promise.all([
    spectraCard.boundingBox(),
    summaryCard.boundingBox(),
    page.evaluate(() => ({ width: window.innerWidth })),
  ]);
  expect(spectraBox).not.toBeNull();
  expect(summaryBox).not.toBeNull();
  expect(Math.abs(spectraBox.width - summaryBox.width)).toBeLessThanOrEqual(2);
  expect(Math.abs(spectraBox.x - summaryBox.x)).toBeLessThanOrEqual(2);
  expect(spectraBox.width).toBeGreaterThanOrEqual(viewport.width * 0.85);
  await expectSummaryRowsFilled(page);

  const themeVariables = await page.evaluate(() => {
    const style = getComputedStyle(document.documentElement);
    return {
      panel: style.getPropertyValue("--openspecy-panel").trim(),
      panel2: style.getPropertyValue("--openspecy-panel-2").trim(),
      border: style.getPropertyValue("--openspecy-border").trim(),
      accent: style.getPropertyValue("--openspecy-accent").trim(),
    };
  });
  expect(Object.values(themeVariables).every(Boolean)).toBe(true);
  await expectDarkBlueSurface(page.locator(".main-header"));
  await expectDarkBlueSurface(page.locator(".main-footer"));
  await expectDarkBlueSurface(settingsCard);
  await expectDarkBlueSurface(downloadCard);
  await expectDarkBlueSurface(spectraCard);
  await expectDarkBlueSurface(summaryCard);
  await expectBlueBorder(settingsCard);
  await expectBlueBorder(downloadCard);
  await expectBlueBorder(spectraCard);
  await expectBlueBorder(summaryCard);
  await expectBlueBorder(page.locator("#MyPlotC").locator(
    "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
  ));
  await expectDarkBlueSurface(page.locator("#download_selection + .selectize-control .selectize-input"));
  await expectDarkBlueSurface(page.locator("#event table tbody td").first());
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

  // Child settings are configuration-only until their owner switch is on.
  await page.getByRole("link", { name: "Preprocessing", exact: true }).click();
  await expectCardCollapsed(settingsCard, false);
  const baselineSwitch = page.locator("#baseline_decision");
  await expect(baselineSwitch).not.toBeChecked();
  const baselineCard = baselineSwitch.locator(
    "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
  );
  if (await baselineCard.evaluate((card) => card.classList.contains("collapsed-card"))) {
    await toggleCard(baselineCard);
  }
  const tracesBeforeMutedChange = await nonemptyTraces(page);
  const matchBeforeMutedChange = await firstMatch.textContent();
  await resetProgressProbe(page);
  await selectizeOption(page, "baseline_method", "fill_peaks");
  await page.waitForTimeout(1300);
  await expect(overlay).toBeHidden();
  expect(await nonemptyTraces(page)).toEqual(tracesBeforeMutedChange);
  expect(await firstMatch.textContent()).toEqual(matchBeforeMutedChange);
  const mutedBaselineState = await page.evaluate(() => window.__openspecySmoke);
  expect(mutedBaselineState.phases).toEqual(["Preparing analysis..."]);
  expect(mutedBaselineState.visible).toEqual([false]);

  await resetProgressProbe(page);
  await baselineSwitch.check({ force: true });
  await expect.poll(async () => (
    await page.evaluate(() => window.__openspecySmoke.phases.join(" "))
  ), { timeout: 120000 }).toMatch(/Preprocessing spectra/i);
  await expect.poll(async () => (
    await page.evaluate(() => window.__openspecySmoke.phases.join(" "))
  ), { timeout: 240000 }).toMatch(/Identifying spectra|Rendering results/i);
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect(overlay).toBeHidden({ timeout: 120000 });

  // Draft ratios and Add are quiet while Quantification is off. Turning the
  // owner on calculates only the saved definitions.
  await page.getByRole("link", { name: "Quantification", exact: true }).click();
  await expect(page.locator("#active_quantification")).not.toBeChecked();
  await resetProgressProbe(page);
  await selectizeOption(page, "quant_treatment", "raw");
  await page.locator("#quant_ratio_name").fill("Custom Carbonyl");
  await page.waitForTimeout(350);
  await page.locator("#quant_ratio_add").click();
  await expect(page.locator("#quant_saved_ratios")).toContainText("Custom Carbonyl");
  await page.locator('input[name="quant_ratio_type"][value="peak"]')
    .check({ force: true });
  await expect(page.locator("#quant_numerator_peak")).toBeAttached();
  await page.locator("#quant_ratio_name").fill("Custom Peak");
  await page.waitForTimeout(350);
  await page.locator("#quant_ratio_add").click();
  await page.waitForTimeout(500);
  const ratioBuilderState = await page.evaluate(() => {
    const savedSelect = document.getElementById("quant_remove_id");
    const visibleDialog = Array.from(document.querySelectorAll(
      '[role="dialog"], .swal2-popup, .sweet-alert'
    )).find((element) => {
      const style = getComputedStyle(element);
      return style.display !== "none" && style.visibility !== "hidden";
    });
    return {
      type: document.querySelector(
        'input[name="quant_ratio_type"]:checked'
      )?.value || "",
      name: document.getElementById("quant_ratio_name")?.value || "",
      numerator: document.getElementById("quant_numerator_peak")?.value || "",
      denominator: document.getElementById("quant_denominator_peak")?.value || "",
      savedCount: savedSelect?.selectize ?
        Object.keys(savedSelect.selectize.options).length :
        (savedSelect?.options?.length || 0),
      savedText: document.getElementById("quant_saved_ratios")?.textContent || "",
      warning: visibleDialog?.textContent?.trim() || "",
    };
  });
  expect(ratioBuilderState).toMatchObject({
    type: "peak",
    name: "",
    savedCount: 2,
    warning: "",
  });
  await page.waitForTimeout(1300);
  await expect(overlay).toBeHidden();
  const mutedQuantState = await page.evaluate(() => window.__openspecySmoke);
  expect(mutedQuantState.phases).toEqual(["Preparing analysis..."]);
  expect(mutedQuantState.visible).toEqual([false]);

  await resetProgressProbe(page);
  await page.locator("#active_quantification").check({ force: true });
  await expectEnabledSwitchColors(page, "active_quantification");
  await expect.poll(async () => (
    await page.evaluate(() => window.__openspecySmoke.phases.join(" "))
  ), { timeout: 120000 }).toMatch(/Calculating saved ratios/i);
  await expect(page.locator("#eventmetadata table")).toContainText(
    /area_ratio_custom_carbonyl/i,
    { timeout: 120000 }
  );
  await expect(page.locator("#eventmetadata table")).toContainText(
    /peak_ratio_custom_peak/i
  );
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i);
  await expect(overlay).toBeHidden({ timeout: 120000 });

  await expect(page.locator("#download_selection")).toHaveValue("Top Matches");
  await expect(page.locator("#download_data")).toHaveText("Download Top Matches");
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard, false);
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
  expect(topMatchLines[0]).toMatch(/quantification_treatment/i);
  expect(topMatchLines[0]).toMatch(/quantification_definitions/i);
  expect(topMatchLines[0]).toMatch(/area_ratio_custom_carbonyl/i);
  expect(topMatchLines[0]).toMatch(/peak_ratio_custom_peak/i);
  expect(topMatchLines.length).toBe(7);
  expect(topMatchesText).toMatch(/poly\(ethylene\)/i);
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard);
  await page.screenshot({ path: testInfo.outputPath("local-app-analysis-result.png"), fullPage: true });

  await page.getByRole("link", { name: "Identification", exact: true }).click();
  await page.locator("#active_identification").uncheck({ force: true });
  await expect.poll(() => nonemptyTraces(page), { timeout: 60000 }).toHaveLength(1);
  await expect(page.locator("#event")).toBeHidden();
  await expect(page.locator("#download_selection")).toHaveValue("Processed Spectra");
  await expect(page.locator("#download_data")).toHaveText("Download Processed Spectra");
  const processedDownload = await consumeDownload(page);
  expect(processedDownload.filename).toMatch(/^Processed-Spectra-.*\.csv$/i);
  const processedText = processedDownload.content.toString("utf8");
  expect(processedText).toMatch(/signal_to_noise/i);
  expect(processedText).toMatch(/quantification_treatment/i);
  expect(processedText).toMatch(/quantification_definitions/i);
  expect(processedText).toMatch(/area_ratio_custom_carbonyl/i);
  expect(processedText).toMatch(/peak_ratio_custom_peak/i);
  expect(processedText).toMatch(/raman_hdpe/i);
  await page.locator("#active_identification").check({ force: true });
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(2);
  await expect(overlay).toBeHidden({ timeout: 30000 });

  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.waitForTimeout(750);
  await expectSummaryRowsFilled(page, true);
  const [mobileSpectra, mobileSummary] = await Promise.all([
    spectraCard.boundingBox(), summaryCard.boundingBox(),
  ]);
  expect(Math.abs(mobileSpectra.width - mobileSummary.width)).toBeLessThanOrEqual(2);
  expect(Math.abs(mobileSpectra.x - mobileSummary.x)).toBeLessThanOrEqual(2);
  expect(mobileSpectra.width).toBeGreaterThanOrEqual(390 * 0.85);
  await expect(page.locator(
    '[data-toggle="popover"], [data-bs-toggle="popover"], .popover'
  )).toHaveCount(0);
  await page.screenshot({ path: testInfo.outputPath("local-app-mobile.png"), fullPage: true });
  expect(popups).toEqual([]);
  expect(severeErrors).toEqual([]);
  expect(stderr).not.toMatch(/Warning: Error in|Execution halted/);
});
