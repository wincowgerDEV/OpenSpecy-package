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
const rPidFile = path.join(repo, "test-results", ".shiny-local-smoke-r.pid");
const fileSpecsSmokePath = process.env.OPENSPECY_LOCAL_FILE_SPECS_PATH || "";
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
      name: trace.name,
      points: trace.x.length,
      dash: trace.line && trace.line.dash,
      color: trace.line && trace.line.color,
      opacity: trace.opacity,
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
  expect(await disclosures.count()).toBeGreaterThanOrEqual(15);
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

async function consumeDownload(
  page,
  { readyTimeout = 30000, eventTimeout = 30000 } = {}
) {
  const link = page.locator("#download_data");
  await expect(link).toBeVisible();
  await expect.poll(async () => link.getAttribute("href"), {
    timeout: readyTimeout,
  }).toMatch(/(?:^|\/)session\/[^/]+\/download\/download_data/);
  await expect(link).not.toHaveClass(/\bdisabled\b/, {
    timeout: readyTimeout,
  });
  const started = Date.now();
  const [download] = await Promise.all([
    page.waitForEvent("download", { timeout: eventTimeout }),
    link.click(),
  ]);
  const elapsed = Date.now() - started;
  expect(await download.failure()).toBeNull();
  const downloadPath = await download.path();
  expect(downloadPath).not.toBeNull();
  const content = fs.readFileSync(downloadPath);
  expect(content.length).toBeGreaterThan(20);
  return { content, elapsed, filename: download.suggestedFilename() };
}

async function fetchDownload(link, { readyTimeout = 30000 } = {}) {
  await expect(link).toBeVisible();
  await expect.poll(async () => link.getAttribute("href"), {
    timeout: readyTimeout,
  }).toMatch(/(?:^|\/)session\/[^/]+\/download\/download_data/);
  await expect(link).not.toHaveClass(/\bdisabled\b/, {
    timeout: readyTimeout,
  });
  const started = Date.now();
  const response = await link.evaluate(async (element) => {
    const result = await fetch(element.href, { cache: "no-store" });
    return {
      status: result.status,
      contentType: result.headers.get("content-type") || "",
      disposition: result.headers.get("content-disposition") || "",
      content: await result.text(),
    };
  });
  return { ...response, elapsed: Date.now() - started };
}

async function expectBuilderItem(page, outputSelector, label) {
  await page.waitForFunction(({ selector, expected }) => {
    const output = document.querySelector(selector);
    const alert = document.querySelector(
      ".swal2-popup.swal2-show, .sweet-alert.showSweetAlert.visible"
    );
    return Boolean(alert || output?.textContent?.includes(expected));
  }, { selector: outputSelector, expected: label }, { timeout: 15000 });
  const alert = page.locator(
    ".swal2-popup.swal2-show, .sweet-alert.showSweetAlert.visible"
  );
  if (await alert.isVisible()) {
    throw new Error(`Failed to add ${label}: ${(await alert.innerText()).trim()}`);
  }
  await expect(page.locator(outputSelector)).toContainText(label);
}

async function dismissVisibleAlert(page) {
  const alert = page.locator(
    ".swal2-popup.swal2-show, .sweet-alert.showSweetAlert.visible"
  );
  if (!await alert.isVisible()) return false;
  const alertText = (await alert.innerText()).trim();
  const expectedNonfatalAlert = /Best practice not followed!|No regions passing threshold|No or all regions passing threshold/i;
  if (!expectedNonfatalAlert.test(alertText)) {
    throw new Error(`Unexpected alert while exercising the app: ${alertText}`);
  }
  const confirm = alert.locator(
    "button.swal2-confirm, button.confirm, button:has-text('OK')"
  ).first();
  if (await confirm.count()) {
    await confirm.click({ force: true });
  } else {
    await page.keyboard.press("Escape");
  }
  await page.waitForTimeout(100);
  return true;
}

async function dismissQueuedAlerts(page) {
  let quietChecks = 0;
  for (let attempt = 0; attempt < 16 && quietChecks < 4; attempt += 1) {
    await page.waitForTimeout(200);
    if (await dismissVisibleAlert(page)) {
      quietChecks = 0;
    } else {
      quietChecks += 1;
    }
  }
}

async function clickPastQueuedAlerts(page, locator) {
  let lastError;
  for (let attempt = 0; attempt < 6; attempt += 1) {
    await dismissQueuedAlerts(page);
    try {
      await locator.click({ timeout: 2500 });
      return;
    } catch (error) {
      lastError = error;
      const alert = page.locator(
        ".swal2-popup.swal2-show, .sweet-alert.showSweetAlert.visible"
      );
      if (!await alert.isVisible()) throw error;
    }
  }
  throw lastError;
}

async function selectizeOption(page, id, value) {
  const select = page.locator(`#${id}`);
  if (await select.inputValue() === value) return;
  const control = page.locator(`#${id} + .selectize-control`);
  await control.locator(".selectize-input").click({ timeout: 10000 });
  if (await select.inputValue() === value) return;
  await control.locator(
    `.selectize-dropdown-content [data-value="${value}"]`
  ).click({ timeout: 10000 });
  await expect(select).toHaveValue(value);
}

async function waitForStableSelectizeGeneration(
  select,
  requiredOption,
  { timeout = 120000, stableFor = 2000 } = {}
) {
  await expect.poll(async () => select.evaluate((element, expected) => {
    const options = Object.keys(
      element.selectize ? element.selectize.options : {}
    );
    if (!options.includes(expected)) return -1;
    const signature = options.join("\u001f");
    const now = performance.now();
    const previous = window.__openspecyLocalDownloadGeneration;
    if (!previous || previous.node !== element ||
        previous.signature !== signature) {
      window.__openspecyLocalDownloadGeneration = {
        node: element,
        signature,
        since: now,
      };
      return 0;
    }
    return now - previous.since;
  }, requiredOption), { timeout }).toBeGreaterThanOrEqual(stableFor);
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
  await toggle.click({ timeout: 10000 });
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
  fs.mkdirSync(path.dirname(rPidFile), { recursive: true });
  fs.rmSync(rPidFile, { force: true });
  const expression = [
    `writeLines(as.character(Sys.getpid()), ${JSON.stringify(rPidFile.replace(/\\/g, "/"))})`,
    "options(openspecy.shiny.local_files=TRUE)",
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
      OPENSPECY_FILE_SPECS_CACHE:
        process.env.OPENSPECY_FILE_SPECS_CACHE ||
        path.join(repo, "_wasm", "feature009-app-cache"),
    },
    stdio: ["ignore", "pipe", "pipe"],
    windowsHide: true,
  });
  app.stderr.on("data", (chunk) => { stderr += chunk.toString(); });
  await waitForApp();
});

test.afterAll(async () => {
  let rPid = app?.pid;
  try {
    const recordedPid = Number(fs.readFileSync(rPidFile, "utf8").trim());
    if (Number.isInteger(recordedPid) && recordedPid > 0) rPid = recordedPid;
  } catch (_) {
    // Fall back to the child-process PID when R did not reach PID-file setup.
  }
  if (process.platform === "win32") {
    if (Number.isInteger(rPid) && rPid > 0) {
      await new Promise((resolve) => {
        const killer = spawn(
          "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe",
          [
            "-NoProfile", "-NonInteractive", "-Command",
            `Stop-Process -Id ${rPid} -Force -ErrorAction SilentlyContinue`,
          ],
          { windowsHide: true, stdio: "ignore" }
        );
        killer.once("exit", resolve);
        killer.once("error", resolve);
      });
    }
  } else if (app && app.exitCode === null) {
    app.kill("SIGTERM");
  }
  app?.stdout?.destroy();
  app?.stderr?.destroy();
  app?.unref();
  fs.rmSync(rPidFile, { force: true });
});

test.afterEach(async ({}, testInfo) => {
  if (testInfo.status !== testInfo.expectedStatus && stderr.trim()) {
    await testInfo.attach("local-shiny-stderr", {
      body: stderr,
      contentType: "text/plain",
    });
    console.error(`Local Shiny stderr:\n${stderr}`);
  }
});

test("map-scale Top Matches download stays fast and leaves the session healthy", async ({ page }) => {
  test.setTimeout(900000);
  const stderrStart = stderr.length;
  const severeErrors = [];
  page.on("console", (message) => {
    if (message.type() === "error" &&
        /Error in|cannot allocate vector|package .* not found|there is no package/i.test(message.text())) {
      severeErrors.push(message.text());
    }
  });
  page.on("pageerror", (error) => severeErrors.push(error.message));

  await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("#file")).toBeAttached({ timeout: 60000 });
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  await page.locator("#active_identification").evaluate((input) => {
    if (!input.checked) input.click();
  });
  await page.locator("#active_preprocessing").evaluate((input) => {
    if (input.checked) input.click();
  });
  await page.locator("#collapse_decision").evaluate((input) => {
    if (input.checked) input.click();
  });
  await page.locator("#threshold_decision").evaluate((input) => {
    if (input.checked) input.click();
  });
  await expect(page.locator("#active_identification")).toBeChecked();
  await expect(page.locator("#active_preprocessing")).not.toBeChecked();
  await expect(page.locator("#collapse_decision")).not.toBeChecked();
  await expect(page.locator("#threshold_decision")).not.toBeChecked();
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });

  const mapUploadPath = path.join(repo, "inst", "extdata", "CA_tiny_map.zip");
  await page.locator("#file").setInputFiles(mapUploadPath);
  await expect.poll(async () => page.locator("#file").evaluate((input) =>
    input.files?.[0]?.name || ""
  )).toBe("CA_tiny_map.zip");
  await expect(page.locator("#heatmap_frame")).toBeVisible({ timeout: 180000 });
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((candidate) =>
      candidate.type === "heatmap" && Array.isArray(candidate.z)
    );
    return trace ? trace.z.flat(Infinity).filter(Number.isFinite).length : 0;
  }), { timeout: 180000 }).toBeGreaterThan(1);
  await expect(page.locator("#event table tbody tr").first()).toBeVisible({
    timeout: 180000,
  });
  await expect(page.locator("#eventmetadata")).toContainText("CA small UF.dat", {
    timeout: 600000,
  });
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 180000,
  });
  await waitForStableSelectizeGeneration(
    page.locator("#download_selection"),
    "Top Matches",
    { timeout: 180000 }
  );
  await page.locator("#download_selection").evaluate((select) => {
    select.selectize.setValue("Top Matches");
  });
  await expect(page.locator("#download_selection")).toHaveValue("Top Matches");
  await expect(page.locator("#download_data")).toHaveText("Download Top Matches");
  await expect(page.locator("#top_n_input")).toHaveValue("1");

  const topMatches = await fetchDownload(page.locator("#download_data"), {
    readyTimeout: 600000,
  });
  expect(topMatches.elapsed).toBeLessThan(15000);
  expect(topMatches.status).toBe(200);
  expect(topMatches.contentType).toMatch(/^text\/(?:csv|plain)/i);
  expect(topMatches.disposition).toMatch(/filename="?Top-Matches-.*\.csv/i);
  const rows = topMatches.content.split(/\r?\n/).filter(Boolean);
  expect(rows).toHaveLength(209);
  expect(rows[0]).toMatch(
    /file_name.*col_id.*material_class.*match_val.*signal_to_noise/i
  );

  await page.locator("#download_selection").evaluate((select) => {
    select.selectize.setValue("Processed Spectra");
  });
  await expect(page.locator("#download_selection")).toHaveValue("Processed Spectra");
  await expect(page.locator("#download_data")).toHaveText("Download Processed Spectra");
  const postExport = await fetchDownload(page.locator("#download_data"));
  expect(postExport.status).toBe(200);
  expect(postExport.disposition).toMatch(/filename="?Processed-Spectra-.*\.csv/i);
  expect(postExport.content).toMatch(/wavenumber/i);

  await expect.poll(() => stderr.slice(stderrStart), { timeout: 10000 })
    .toMatch(/completed 'Top Matches' download/i);
  const diagnostics = stderr.slice(stderrStart);
  expect(diagnostics).not.toMatch(/cannot allocate vector/i);
  expect(severeErrors).toEqual([]);
});

test("Test Map metadata sidebar selects a non-first spectrum", async ({ page }, testInfo) => {
  test.setTimeout(300000);
  const severeErrors = [];
  page.on("console", (message) => {
    if (message.type() === "error" &&
        /Error in|package .* not found|there is no package/i.test(message.text())) {
      severeErrors.push(message.text());
    }
  });
  page.on("pageerror", (error) => severeErrors.push(error.message));

  await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("#file")).toBeAttached({ timeout: 60000 });
  for (const inputId of [
    "active_identification", "active_preprocessing", "collapse_decision",
    "threshold_decision",
  ]) {
    await page.locator(`#${inputId}`).evaluate((input) => {
      if (input.checked) input.click();
    });
  }
  const mapUploadPath = path.join(repo, "inst", "extdata", "CA_tiny_map.zip");
  await page.locator("#file").setInputFiles(mapUploadPath);
  await expect(page.locator("#heatmap_frame")).toBeVisible({ timeout: 180000 });
  await expect(page.locator("#eventmetadata")).toContainText("CA small UF.dat", {
    timeout: 180000,
  });
  await dismissQueuedAlerts(page);

  const spectraCard = page.locator("#spectra_box");
  const sidebarToggle = page.locator("#mycardsidebar");
  await sidebarToggle.click();
  await expect(spectraCard).toHaveClass(/direct-chat-contacts-open/);
  const sidebar = spectraCard.locator(".direct-chat-contacts");
  await sidebar.getByRole("link", {
    name: "Uploaded Metadata", exact: true,
  }).click();
  const metadataTable = sidebar.locator(
    "#sidebar_metadata .dataTables_scrollBody table"
  );
  await expect(metadataTable).toBeVisible({
    timeout: 60000,
  });
  const nonFirstMetadataRow = metadataTable.locator("tbody tr").nth(1);
  await expect(nonFirstMetadataRow).toContainText("0_1", { timeout: 60000 });
  await nonFirstMetadataRow.click();
  await expect(page.locator("#eventmetadata table")).toContainText("0_1", {
    timeout: 60000,
  });
  await page.screenshot({
    path: testInfo.outputPath("test-map-uploaded-metadata.png"),
    fullPage: true,
  });
  await sidebarToggle.click();
  await expect(spectraCard).not.toHaveClass(/direct-chat-contacts-open/);
  await expect(page.locator(".shiny-output-error:visible")).toHaveCount(0);
  expect(severeErrors).toEqual([]);
});

test("local app renders spectra, matches, and one informative progress overlay", async ({ page }, testInfo) => {
  test.setTimeout(900000);
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
    if (tabName === "Preprocessing") {
      await expect(minMaxControl).toBeVisible();
      await expect(page.locator("#spike_decision")).toBeChecked();
      const saturationSwitch = page.locator("#saturation_decision");
      await expect(saturationSwitch).not.toBeChecked();
      await expect(page.locator("#spike_direction")).toHaveValue("both");
      await expect(page.locator("#spike_residual_threshold")).toHaveValue("8");
      await expect(page.locator("#saturation_mode")).toHaveValue("auto");
      const preprocessingPane = page.locator("#spike_decision").locator(
        "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' tab-pane ')][1]"
      );
      const preprocessingTitles = await preprocessingPane.locator(
        ".card > .card-header .card-title"
      ).allTextContents();
      expect(preprocessingTitles.slice(-2).map((text) => text.trim())).toEqual([
        "Remove Isolated Spikes", "Remove Saturated Ranges",
      ]);
      const spikeCard = page.locator("#spike_decision").locator(
        "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
      );
      if (await spikeCard.evaluate((card) => card.classList.contains("collapsed-card"))) {
        await toggleCard(spikeCard);
      }
      await expect(spikeCard).toContainText(
        "Robust Residual Threshold is the prediction error"
      );
      await toggleCard(spikeCard);
      const saturationCard = saturationSwitch.locator(
        "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
      );
      if (await saturationCard.evaluate((card) => card.classList.contains("collapsed-card"))) {
        await toggleCard(saturationCard);
      }
      await expect(saturationCard).toContainText(
        "Detector Ceiling is expressed in the uploaded intensity units"
      );
      await toggleCard(saturationCard);
      await saturationSwitch.check({ force: true });
      await expectEnabledSwitchColors(page, "saturation_decision");
      const rangeSwitch = page.locator("#range_automate");
      const rangeCard = rangeSwitch.locator(
        "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
      );
      if (await rangeCard.evaluate((card) => card.classList.contains("collapsed-card"))) {
        await toggleCard(rangeCard);
      }
      const manualBounds = page.locator("#manual_range_bounds");
      const minRange = page.locator("#MinRange");
      const maxRange = page.locator("#MaxRange");
      await expect(minRange).toBeDisabled();
      await expect(maxRange).toBeDisabled();
      await expect(manualBounds).toHaveClass(/openspecy-inputs-disabled/);
      expect(await manualBounds.evaluate((element) =>
        Number(getComputedStyle(element).opacity)
      )).toBeLessThan(0.7);
      await rangeCard.screenshot({
        path: testInfo.outputPath("local-app-automatic-range-disabled.png"),
      });
      await rangeSwitch.uncheck({ force: true });
      await expect(minRange).toBeEnabled();
      await expect(maxRange).toBeEnabled();
      await expect(manualBounds).not.toHaveClass(/openspecy-inputs-disabled/);
      await rangeSwitch.check({ force: true });
      await expect(minRange).toBeDisabled();
      await expect(maxRange).toBeDisabled();
      await expect(page.locator("#openspecy_busy_overlay")).toBeHidden();
      await toggleCard(rangeCard);
    }
    await toggleCard(settingsCard);
    await expectCardCollapsed(settingsCard);
  }

  await page.getByRole("link", { name: "Quantification", exact: true }).click();
  await expectCardCollapsed(settingsCard, false);
  await expect(page.locator("#active_quantification")).not.toBeChecked();
  await expect(page.locator("#quant_ratio_name")).toBeVisible();
  await expect(page.locator("#quant_ratio_add")).toBeVisible();
  const ratioNumericIds = [
    "quant_numerator_area_min", "quant_numerator_area_max",
    "quant_denominator_area_min", "quant_denominator_area_max",
    "quant_numerator_peak", "quant_denominator_peak",
  ];
  for (const id of ratioNumericIds) {
    await expect(page.locator(`#${id}`)).toBeAttached();
    await expect(page.locator(`#${id}`)).toHaveAttribute("type", "number");
  }
  await expect(page.locator("#quant_measurement_enabled")).toHaveCount(0);
  const measurementCard = page.locator("#quant_measurement_name").locator(
    "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
  );
  await expect(measurementCard.locator(":scope > .card-header"))
    .toContainText("Single Measurements");
  await expect(page.locator("#quant_measurement_name")).toBeVisible();
  await expect(page.locator("#quant_measurement_area_min")).toHaveAttribute(
    "type", "number"
  );
  await expect(page.locator("#quant_measurement_area_max")).toHaveAttribute(
    "type", "number"
  );
  await expect(page.locator("#quant_measurement_wavenumber")).toHaveAttribute(
    "type", "number"
  );
  await expectInformationalDetails(page);
  await expectEnabledSwitchColors(page, "active_preprocessing");
  await toggleCard(settingsCard);
  await expectCardCollapsed(settingsCard);

  const downloadButtonStyle = await page.locator("#download_data").evaluate((button) => {
    const box = button.getBoundingClientRect();
    const titleBox = button.closest(".card-title")?.getBoundingClientRect();
    return {
      width: box.width,
      titleWidth: titleBox?.width || 0,
      whiteSpace: getComputedStyle(button).whiteSpace,
      gap: parseFloat(getComputedStyle(button).columnGap || getComputedStyle(button).gap),
    };
  });
  expect(downloadButtonStyle.width).toBeGreaterThanOrEqual(270);
  expect(Math.abs(
    downloadButtonStyle.width - downloadButtonStyle.titleWidth
  )).toBeLessThanOrEqual(2);
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

  // Capture every initial nonempty heatmap render. The server-side default
  // must settle directly on categorical Match Name without a transient
  // numeric Match Value colorbar while the dynamic selector binds.
  await page.evaluate(() => {
    const state = {
      samples: [], observer: null, plot: null, afterPlot: null,
    };
    const record = () => {
      const plot = document.getElementById("heatmapA");
      const trace = (plot?.data || []).find((item) => item.type === "heatmap");
      const finiteCount = (trace?.z || []).flat?.(Infinity)
        .filter(Number.isFinite).length || 0;
      if (!trace || finiteCount === 0) return;
      const sample = {
        showscale: Boolean(trace.showscale),
        title: trace.colorbar?.title?.text || trace.colorbar?.title || "",
        mapColor: document.getElementById("map_color")?.value || "",
      };
      const key = JSON.stringify(sample);
      if (state.samples.at(-1)?.key !== key) {
        state.samples.push({ ...sample, key });
      }
    };
    const attach = () => {
      const plot = document.getElementById("heatmapA");
      if (!plot || plot === state.plot || typeof plot.on !== "function") {
        record();
        return;
      }
      if (state.plot && state.afterPlot &&
          typeof state.plot.removeListener === "function") {
        state.plot.removeListener("plotly_afterplot", state.afterPlot);
      }
      state.plot = plot;
      state.afterPlot = record;
      plot.on("plotly_afterplot", state.afterPlot);
      record();
    };
    state.observer = new MutationObserver(attach);
    state.observer.observe(document.documentElement, {
      childList: true, subtree: true, attributes: true,
    });
    window.__openspecyInitialHeatmapProbe = state;
    attach();
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
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(3);
  const traces = await nonemptyTraces(page);
  expect(traces.every((trace) => trace.points > 100)).toBe(true);
  expect(traces.map((trace) => trace.name)).toEqual([
    "Raw spectrum", "Active spectrum", "Identification match",
  ]);
  const rawTrace = traces.find((trace) => trace.name === "Raw spectrum");
  const processedTrace = traces.find((trace) => trace.name === "Active spectrum");
  const referenceTrace = traces.find((trace) => trace.name === "Identification match");
  expect(rawTrace).toBeDefined();
  expect(processedTrace).toBeDefined();
  expect(referenceTrace).toBeDefined();
  expect(String(rawTrace.color)).toMatch(/rgba\(203[, ]+213[, ]+225[, ]+0\.24\)/i);
  expect(String(processedTrace.color).toUpperCase()).toMatch(/#FFF(?:FFF)?|RGB\(255[, ]+255[, ]+255\)/);
  expect(referenceTrace.dash).toBe("dot");
  expect(String(referenceTrace.color).toUpperCase()).toMatch(/#FB7185|RGB\(251[, ]+113[, ]+133\)/);
  await expect(page.locator("#MyPlotC .legendtext")).toHaveText([
    "Raw spectrum", "Active spectrum", "Identification match",
  ]);
  const desktopLegend = await page.locator("#MyPlotC").evaluate((plot) => ({
    orientation: plot.layout.legend.orientation,
    x: plot.layout.legend.x,
    rightMargin: plot.layout.margin.r,
  }));
  expect(desktopLegend).toMatchObject({ orientation: "v" });
  expect(desktopLegend.x).toBeGreaterThan(1);
  expect(desktopLegend.rightMargin).toBeGreaterThanOrEqual(180);

  const qualityControls = [
    { status: "automatic", label: /Automatic Corrections Made/i },
    { status: "warning", label: /Warnings/i },
    { status: "success", label: /Successes/i },
  ];
  for (const { status, label } of qualityControls) {
    const button = page.locator(`#quality_${status}_details`);
    await expect(button).toBeVisible();
    await expect(button).toContainText(label);
    await expect(page.locator(`#quality_${status}_count`)).toHaveText(/^\d+$/);
  }
  const qualityControlGroup = page.locator(".openspecy-quality-controls");
  await expect(qualityControlGroup.locator("button.openspecy-quality-button"))
    .toHaveCount(3);
  await expect(qualityControlGroup).not.toContainText(/\bErrors?\b/i);
  await expect(page.locator("#correlation_head")).toHaveCount(0);
  const successColors = await page.locator("#quality_success_details")
    .evaluate((button) => {
      const icon = button.querySelector(".openspecy-quality-icon-success");
      return {
        border: getComputedStyle(button).borderColor,
        icon: icon ? getComputedStyle(icon).color : "",
      };
    });
  expect(successColors).toEqual({
    border: "rgb(34, 197, 94)",
    icon: "rgb(34, 197, 94)",
  });

  const automaticCount = Number(
    await page.locator("#quality_automatic_count").textContent()
  );
  expect(await page.locator("#quality_automatic_details").evaluate((button) =>
    button.classList.contains("openspecy-automatic-applied")
  )).toBe(automaticCount > 0);
  await page.locator("#quality_automatic_details").click();
  let qualityModal = page.locator(".modal-content:visible");
  await expect(qualityModal).toContainText("Automatic corrections made");
  await expect(qualityModal.locator(".openspecy-quality-finding-automatic"))
    .toHaveCount(4);
  for (const label of [
    "Spike correction", "Saturation restriction", "CO2 flattening",
    "High-tail range restriction",
  ]) {
    await expect(qualityModal).toContainText(label);
  }
  const saturationFinding = qualityModal.locator(
    ".openspecy-quality-finding-automatic",
    { hasText: "Saturation restriction" }
  );
  await expect(saturationFinding).not.toContainText(/disabled/i);
  const spikeFinding = qualityModal.locator(
    ".openspecy-quality-finding-automatic",
    { hasText: "Spike correction" }
  );
  await expect(spikeFinding).not.toContainText(/correctable spikes remain/i);
  if (/Status:\s*applied/i.test(await spikeFinding.textContent())) {
    await expect(spikeFinding).toContainText(/Corrected \d+ spike region/i);
    await expect(spikeFinding).toContainText(/cm\^-1/i);
  }
  const co2Finding = qualityModal.locator(
    ".openspecy-quality-finding-automatic",
    { hasText: "CO2 flattening" }
  );
  if (/Status:\s*applied/i.test(await co2Finding.textContent())) {
    await expect(co2Finding).toContainText(/Flattened 2200-2400 cm\^-1/i);
  }
  const tailFinding = qualityModal.locator(
    ".openspecy-quality-finding-automatic",
    { hasText: "High-tail range restriction" }
  );
  if (/Status:\s*applied/i.test(await tailFinding.textContent())) {
    await expect(tailFinding).toContainText(
      /Restricted the shared wavenumber axis from .* to .* cm\^-1/i
    );
  }
  await qualityModal.getByRole("button", { name: "Close", exact: true }).click();
  await expect(qualityModal).toBeHidden();

  const modalTestIds = {};
  for (const status of ["warning", "success"]) {
    const expectedCount = Number(
      await page.locator(`#quality_${status}_count`).textContent()
    );
    await page.locator(`#quality_${status}_details`).click();
    qualityModal = page.locator(".modal-content:visible");
    await expect(qualityModal).toContainText(
      status === "warning" ? "Spectral quality warnings" :
        "Successful spectral checks"
    );
    const findings = qualityModal.locator(".openspecy-quality-finding");
    await expect(findings).toHaveCount(expectedCount);
    modalTestIds[status] = await findings.evaluateAll(
      (items, expectedStatus) => items.map((finding) => ({
        status: finding.dataset.qualityStatus,
        testId: finding.dataset.qualityTestId,
        hasExpectedClass: finding.classList.contains(
          `openspecy-quality-finding-${expectedStatus}`
        ),
        border: getComputedStyle(finding).borderColor,
      })),
      status
    );
    expect(modalTestIds[status].every((finding) =>
      finding.status === status && finding.hasExpectedClass
    )).toBe(true);
    const expectedBorder = status === "warning" ?
      "rgb(250, 204, 21)" : "rgb(34, 197, 94)";
    expect(modalTestIds[status].every((finding) =>
      finding.border === expectedBorder
    )).toBe(true);
    if (expectedCount > 0 && status === "warning") {
      await expect(qualityModal).toContainText("Interpretation:");
      await expect(qualityModal).toContainText("Action:");
    }
    if (expectedCount > 0 && status === "success") {
      await expect(qualityModal).not.toContainText("Interpretation:");
      await expect(qualityModal).not.toContainText("Action:");
      await expect(qualityModal).not.toContainText(/check passed/i);
    }
    await qualityModal.getByRole("button", { name: "Close", exact: true }).click();
    await expect(qualityModal).toBeHidden();
  }
  expect(new Set([
    ...modalTestIds.warning.map((finding) => finding.testId),
    ...modalTestIds.success.map((finding) => finding.testId),
  ]).size).toBe(
    modalTestIds.warning.length + modalTestIds.success.length
  );
  expect([
    ...modalTestIds.warning,
    ...modalTestIds.success,
  ].filter((finding) =>
    finding.testId.endsWith(":correlation_threshold")
  )).toHaveLength(1);

  await expect(page.locator("#map_color")).toBeAttached();
  await expect(page.locator("#map_color")).toHaveValue("Match Name");
  const initialHeatmapStates = await page.evaluate(() => {
    const state = window.__openspecyInitialHeatmapProbe;
    state?.observer?.disconnect();
    if (state?.plot && state?.afterPlot &&
        typeof state.plot.removeListener === "function") {
      state.plot.removeListener("plotly_afterplot", state.afterPlot);
    }
    return (state?.samples || []).map(({ key, ...sample }) => sample);
  });
  expect(initialHeatmapStates.length).toBeGreaterThan(0);
  expect(initialHeatmapStates.every((state) =>
    !state.showscale && !/Match Value/i.test(String(state.title))
  )).toBe(true);
  await selectizeOption(page, "map_color", "Match Value");
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return Boolean(trace?.showscale);
  }), { timeout: 30000 }).toBe(true);
  const heatmapColors = await page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return (trace && trace.colorscale ? trace.colorscale : []).map((entry) =>
      String(entry[1]).toUpperCase()
    );
  });
  expect(heatmapColors).toEqual([
    "#56B4E9", "#44B9A8", "#009E73", "#F0E442", "#E69F00", "#CC79A7",
  ]);
  expect(heatmapColors.every((hex) => {
    const channels = hex.match(/[0-9A-F]{2}/g).map((value) => parseInt(value, 16));
    const luminance = 0.2126 * channels[0] + 0.7152 * channels[1] +
      0.0722 * channels[2];
    return luminance >= 90;
  })).toBe(true);
  const numericHeatmapLegend = await page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return {
      showscale: trace?.showscale,
      title: trace?.colorbar?.title?.text || trace?.colorbar?.title || "",
      orientation: trace?.colorbar?.orientation,
      x: trace?.colorbar?.x,
      y: trace?.colorbar?.y,
      topMargin: plot.layout?.margin?.t,
      rightMargin: plot.layout?.margin?.r,
    };
  });
  expect(numericHeatmapLegend.showscale).toBe(true);
  expect(numericHeatmapLegend.title).toMatch(/Match Value|Signal\/Noise|Value/i);
  expect(numericHeatmapLegend.orientation).toBe("h");
  expect(numericHeatmapLegend.x).toBeCloseTo(0.5, 4);
  expect(numericHeatmapLegend.y).toBeGreaterThan(1);
  expect(numericHeatmapLegend.topMargin).toBeGreaterThanOrEqual(100);
  expect(numericHeatmapLegend.rightMargin).toBeLessThan(100);
  const desktopHeatmapLegendBounds = await page.locator("#heatmapA").evaluate((plot) => {
    const legend = plot.querySelector(".colorbar");
    const plotArea = plot.querySelector(".nsewdrag");
    if (!legend || !plotArea) return null;
    const legendBox = legend.getBoundingClientRect();
    const plotAreaBox = plotArea.getBoundingClientRect();
    return {
      legendBottom: legendBox.bottom,
      legendWidth: legendBox.width,
      plotTop: plotAreaBox.top,
      plotWidth: plotAreaBox.width,
    };
  });
  expect(desktopHeatmapLegendBounds).not.toBeNull();
  expect(desktopHeatmapLegendBounds.legendBottom)
    .toBeLessThanOrEqual(desktopHeatmapLegendBounds.plotTop + 8);
  expect(desktopHeatmapLegendBounds.legendWidth)
    .toBeLessThan(desktopHeatmapLegendBounds.plotWidth);
  await page.screenshot({
    path: testInfo.outputPath("local-app-numeric-heatmap-legend.png"),
    fullPage: true,
  });
  await selectizeOption(page, "map_color", "Match Name");
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return Boolean(trace?.showscale);
  }), { timeout: 30000 }).toBe(false);
  const categoricalHeatmap = await page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return {
      showscale: trace?.showscale,
      layoutLegend: plot.layout?.showlegend,
    };
  });
  expect(categoricalHeatmap).toMatchObject({
    showscale: false,
    layoutLegend: false,
  });
  await expect(page.locator("#eventmetadata table")).toBeVisible();
  await expect(page.locator("#heatmap_frame")).toBeVisible();
  await expect(page.locator("#collapse_decision")).not.toBeChecked();
  await expect(overlay).toBeHidden({ timeout: 30000 });

  // Click the rendered Plotly interaction surface as a user would. Selection
  // should update through the marker proxy without restarting analysis or
  // replacing the heatmap graph (the visible double-render regression).
  const heatmapMarkerBefore = await page.locator("#heatmapA").evaluate((plot) => {
    const marker = (plot.data || []).find((trace) =>
      String(trace.name).toLowerCase() === "selected spectrum"
    );
    return JSON.stringify({ x: marker?.x || [], y: marker?.y || [] });
  });
  await page.locator("#heatmapA").evaluate((plot) => {
    const baseTrace = (plot.data || []).find((trace) => trace.type === "heatmap");
    const state = {
      afterPlots: 0,
      busyReappearances: 0,
      fullReplacements: 0,
      wasBusy: document.documentElement.classList.contains("openspecy-busy-visible"),
      baseTrace,
      plotContainer: plot.querySelector(".plot-container"),
      clickedPoint: null,
    };
    state.afterPlotHandler = () => { state.afterPlots += 1; };
    state.clickHandler = (event) => {
      const point = event?.points?.[0];
      if (point) {
        state.clickedPoint = { x: point.x, y: point.y };
      }
    };
    plot.on("plotly_afterplot", state.afterPlotHandler);
    plot.on("plotly_click", state.clickHandler);
    state.busyObserver = new MutationObserver(() => {
      const busy = document.documentElement.classList.contains(
        "openspecy-busy-visible"
      );
      if (busy && !state.wasBusy) state.busyReappearances += 1;
      state.wasBusy = busy;
    });
    state.busyObserver.observe(document.documentElement, {
      attributes: true, attributeFilter: ["class"],
    });
    state.plotObserver = new MutationObserver((mutations) => {
      const containsFullPlot = (node) => node.nodeType === 1 && (
        node.matches?.(".plot-container, .svg-container") ||
        node.querySelector?.(".plot-container, .svg-container")
      );
      for (const mutation of mutations) {
        if (Array.from(mutation.removedNodes || []).some(containsFullPlot)) {
          state.fullReplacements += 1;
        }
      }
    });
    state.plotObserver.observe(plot, { childList: true, subtree: true });
    window.__openspecyHeatmapClickProbe = state;
  });
  const heatmapInteractionSurface = page.locator("#heatmapA .nsewdrag");
  await expect(heatmapInteractionSurface).toBeVisible();
  const interactionBox = await heatmapInteractionSurface.boundingBox();
  expect(interactionBox).not.toBeNull();
  await page.mouse.click(
    interactionBox.x + interactionBox.width * 0.75,
    interactionBox.y + interactionBox.height * 0.25
  );
  await expect.poll(async () => page.locator("#heatmapA").evaluate(() =>
    window.__openspecyHeatmapClickProbe?.clickedPoint || null
  ), { timeout: 60000 }).not.toBeNull();
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const marker = (plot.data || []).find((trace) =>
      String(trace.name).toLowerCase() === "selected spectrum"
    );
    return JSON.stringify({ x: marker?.x || [], y: marker?.y || [] });
  }), { timeout: 60000 }).not.toBe(heatmapMarkerBefore);
  const selectedMarker = await page.locator("#heatmapA").evaluate((plot) => {
    const marker = (plot.data || []).find((trace) =>
      String(trace.name).toLowerCase() === "selected spectrum"
    );
    const points = Array.from(plot.querySelectorAll(".scatterlayer path.point"));
    const visiblePoint = points.find((point) => {
      const box = point.getBoundingClientRect();
      return box.width > 0 && box.height > 0 &&
        getComputedStyle(point).opacity !== "0";
    });
    return {
      x: marker?.x,
      y: marker?.y,
      color: marker?.marker?.color,
      size: marker?.marker?.size,
      fill: visiblePoint ? getComputedStyle(visiblePoint).fill : "",
      visible: Boolean(visiblePoint),
      clickedPoint: window.__openspecyHeatmapClickProbe?.clickedPoint,
    };
  });
  expect(Array.isArray(selectedMarker.x)).toBe(true);
  expect(Array.isArray(selectedMarker.y)).toBe(true);
  expect(selectedMarker.x).toHaveLength(1);
  expect(selectedMarker.y).toHaveLength(1);
  expect(String(selectedMarker.color).toUpperCase()).toBe("#F59E0B");
  expect(selectedMarker.size).toBe(14);
  expect(selectedMarker.fill).toBe("rgb(245, 158, 11)");
  expect(selectedMarker.visible).toBe(true);
  expect(Number(selectedMarker.x[0])).toBeCloseTo(
    Number(selectedMarker.clickedPoint.x), 8
  );
  expect(Number(selectedMarker.y[0])).toBeCloseTo(
    Number(selectedMarker.clickedPoint.y), 8
  );
  await page.waitForTimeout(750);
  const heatmapClickState = await page.locator("#heatmapA").evaluate((plot) => {
    const state = window.__openspecyHeatmapClickProbe;
    const marker = (plot.data || []).find((trace) =>
      trace.name === "Selected Spectrum"
    );
    const markerPoints = Array.from(
      plot.querySelectorAll(".scatterlayer path.point")
    );
    state.busyObserver.disconnect();
    state.plotObserver.disconnect();
    if (typeof plot.removeListener === "function") {
      plot.removeListener("plotly_afterplot", state.afterPlotHandler);
      plot.removeListener("plotly_click", state.clickHandler);
    }
    return {
      afterPlots: state.afterPlots,
      busyReappearances: state.busyReappearances,
      fullReplacements: state.fullReplacements,
      sameBaseTrace: (plot.data || []).find((trace) => trace.type === "heatmap") ===
        state.baseTrace,
      samePlotContainer: plot.querySelector(".plot-container") ===
        state.plotContainer,
      markerVisible: Array.isArray(marker?.x) && marker.x.length === 1 &&
        Array.isArray(marker?.y) && marker.y.length === 1 &&
        markerPoints.some((point) => {
          const box = point.getBoundingClientRect();
          return box.width > 0 && box.height > 0 &&
            getComputedStyle(point).opacity !== "0";
        }),
    };
  });
  expect(heatmapClickState.busyReappearances).toBe(0);
  expect(heatmapClickState.fullReplacements).toBe(0);
  expect(heatmapClickState.afterPlots).toBeLessThanOrEqual(1);
  expect(heatmapClickState.sameBaseTrace).toBe(true);
  expect(heatmapClickState.samePlotContainer).toBe(true);
  expect(heatmapClickState.markerVisible).toBe(true);
  await expect(overlay).toBeHidden();
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
  const sidebarToggle = page.locator("#mycardsidebar");
  await expect(sidebarToggle).toBeVisible();
  await sidebarToggle.click();
  await expect(spectraCard).toHaveClass(/direct-chat-contacts-open/);
  const matchSidebar = spectraCard.locator(".direct-chat-contacts");
  await expect(matchSidebar).toBeVisible();
  await expectDarkBlueSurface(matchSidebar);
  await expect(matchSidebar.locator("#sidebar_tables")).toBeVisible();
  await expect(matchSidebar).toContainText("Library Matches");
  await expect(matchSidebar).toContainText("Uploaded Metadata");
  await sidebarToggle.click();
  await expect(spectraCard).not.toHaveClass(/direct-chat-contacts-open/);
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

  // Draft ratios and single measurements are quiet while Quantification is
  // off. Every wavenumber control is a fine-grained numeric input, not a
  // range slider; turning the owner on calculates both saved definition sets.
  await page.getByRole("link", { name: "Quantification", exact: true }).click();
  await expect(page.locator("#active_quantification")).not.toBeChecked();
  await resetProgressProbe(page);
  const quantificationInputState = await page.evaluate((ids) => ids.map((id) => {
    const input = document.getElementById(id);
    return {
      id,
      type: input?.type || "",
      step: Number(input?.step),
      hasRangeSlider: Boolean(
        window.jQuery && window.jQuery(input).data("ionRangeSlider")
      ),
    };
  }), [
    ...ratioNumericIds,
    "quant_measurement_area_min", "quant_measurement_area_max",
    "quant_measurement_wavenumber",
  ]);
  expect(quantificationInputState.every((input) =>
    input.type === "number" && Number.isFinite(input.step) && input.step > 0 &&
    !input.hasRangeSlider
  )).toBe(true);
  await page.locator("#quant_numerator_area_min").fill("1650.5");
  await page.locator("#quant_numerator_area_max").fill("1849.5");
  await page.locator("#quant_denominator_area_min").fill("1420.5");
  await page.locator("#quant_denominator_area_max").fill("1499.5");
  await expect(page.locator("#quant_numerator_area_min")).toHaveValue("1650.5");
  await page.locator("#quant_ratio_name").fill("Custom Carbonyl");
  await page.waitForTimeout(350);
  await page.locator("#quant_ratio_add").click();
  await expectBuilderItem(page, "#quant_saved_ratios", "Custom Carbonyl");
  await expect(page.locator("#quant_ratio_name")).toHaveValue("");
  await page.locator('input[name="quant_ratio_type"][value="peak"]')
    .check({ force: true });
  await expect(page.locator("#quant_numerator_peak")).toBeVisible();
  await page.waitForTimeout(350);
  await page.locator("#quant_numerator_peak").fill("1715.25");
  await page.locator("#quant_denominator_peak").fill("1460.25");
  await page.locator("#quant_ratio_name").fill("Custom Peak");
  await page.waitForTimeout(350);
  await page.locator("#quant_ratio_add").click();
  await expectBuilderItem(page, "#quant_saved_ratios", "Custom Peak");
  await expect(page.locator("#quant_ratio_name")).toHaveValue("");

  await expect(page.locator("#quant_measurement_area_min")).toBeVisible();
  await page.locator("#quant_measurement_area_min").fill("1650.5");
  await page.locator("#quant_measurement_area_max").fill("1849.5");
  await page.locator("#quant_measurement_name").fill("Custom Area");
  await page.locator("#quant_measurement_add").click();
  await expectBuilderItem(
    page, "#quant_measurement_definitions", "Custom Area"
  );
  await expect(page.locator("#quant_measurement_name")).toHaveValue("");
  await page.locator('input[name="quant_measurement_type"][value="intensity"]')
    .check({ force: true });
  await expect(page.locator("#quant_measurement_wavenumber")).toBeVisible();
  await page.waitForTimeout(350);
  await page.locator("#quant_measurement_wavenumber").fill("1715.25");
  await page.locator("#quant_measurement_name").fill("Custom Intensity");
  await page.locator("#quant_measurement_add").click();
  await expectBuilderItem(
    page, "#quant_measurement_definitions", "Custom Intensity"
  );
  await expect(page.locator("#quant_measurement_name")).toHaveValue("");
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
  const measurementBuilderState = await page.evaluate(() => {
    const savedSelect = document.getElementById("quant_measurement_remove_id");
    const visibleDialog = Array.from(document.querySelectorAll(
      '[role="dialog"], .swal2-popup, .sweet-alert'
    )).find((element) => {
      const style = getComputedStyle(element);
      return style.display !== "none" && style.visibility !== "hidden";
    });
    return {
      type: document.querySelector(
        'input[name="quant_measurement_type"]:checked'
      )?.value || "",
      name: document.getElementById("quant_measurement_name")?.value || "",
      savedCount: savedSelect?.selectize ?
        Object.keys(savedSelect.selectize.options).length :
        (savedSelect?.options?.length || 0),
      savedText: document.getElementById("quant_measurement_definitions")
        ?.textContent || "",
      warning: visibleDialog?.textContent?.trim() || "",
    };
  });
  expect(measurementBuilderState).toMatchObject({
    type: "intensity",
    name: "",
    savedCount: 2,
    warning: "",
  });
  expect(measurementBuilderState.savedText).toMatch(/Custom Area/);
  expect(measurementBuilderState.savedText).toMatch(/Custom Intensity/);
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
  ), { timeout: 120000 }).toMatch(/Calculating saved quantification/i);
  await expect(page.locator("#eventmetadata table")).toContainText(
    /area_ratio_custom_carbonyl/i,
    { timeout: 120000 }
  );
  await expect(page.locator("#eventmetadata table")).toContainText(
    /peak_ratio_custom_peak/i
  );
  await expect(page.locator("#eventmetadata table")).toContainText(
    /area_under_band_custom_area/i
  );
  await expect(page.locator("#eventmetadata table")).toContainText(
    /point_intensity_custom_intensity/i
  );
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i);
  await expect(overlay).toBeHidden({ timeout: 120000 });

  await expect(page.locator("#download_selection")).toHaveValue("Top Matches");
  await expect(page.locator("#download_data")).toHaveText("Download Top Matches");
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard, false);
  await selectizeOption(page, "download_selection", "User Metadata");
  await expect(page.locator("#download_data")).toHaveText("Download User Metadata");
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard);
  const metadataDownload = await consumeDownload(page);
  expect(metadataDownload.filename).toMatch(
    /^os_metadata_\d{8}-\d{6}(?:\.\d+)?\.csv$/i
  );
  const metadataText = metadataDownload.content.toString("utf8");
  const metadataLines = metadataText.split(/\r?\n/).filter(Boolean);
  expect(metadataLines).toHaveLength(2);
  expect(metadataLines[0]).toMatch(
    /recorded_at.*app_version.*data_digest_md5.*active_preprocessing/i
  );
  expect(metadataLines[0]).toMatch(
    /range_automate.*MinRange.*MaxRange.*active_identification.*id_strategy.*lib_type/i
  );
  expect(metadataLines[0]).toMatch(
    /active_quantification.*quant_saved_ratio_definitions.*quant_saved_measurement_definitions/i
  );
  expect(metadataText).toMatch(/Custom Carbonyl/i);
  expect(metadataText).toMatch(/Custom Peak/i);
  expect(metadataText).toMatch(/Custom Area/i);
  expect(metadataText).toMatch(/Custom Intensity/i);
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard, false);
  await selectizeOption(page, "download_selection", "Top Matches");
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
  expect(topMatchLines[0]).toMatch(/quantification_source/i);
  expect(topMatchesText).toMatch(/displayed_processed_spectra/i);
  expect(topMatchLines[0]).toMatch(/quantification_definitions/i);
  expect(topMatchLines[0]).toMatch(/area_ratio_custom_carbonyl/i);
  expect(topMatchLines[0]).toMatch(/peak_ratio_custom_peak/i);
  expect(topMatchLines[0]).toMatch(/area_under_band_custom_area/i);
  expect(topMatchLines[0]).toMatch(/point_intensity_custom_intensity/i);
  expect(topMatchLines.length).toBe(7);
  expect(topMatchesText).toMatch(/poly\(ethylene\)/i);
  await toggleCard(downloadCard);
  await expectCardCollapsed(downloadCard);
  await page.screenshot({ path: testInfo.outputPath("local-app-analysis-result.png"), fullPage: true });

  await page.getByRole("link", { name: "Identification", exact: true }).click();
  await page.locator("#active_identification").uncheck({ force: true });
  await expect.poll(() => nonemptyTraces(page), { timeout: 60000 }).toHaveLength(2);
  expect((await nonemptyTraces(page)).map((trace) => trace.name)).toEqual([
    "Raw spectrum", "Active spectrum",
  ]);
  await expect(page.locator("#event")).toBeHidden();
  await expect(page.locator("#download_selection")).toHaveValue("Processed Spectra");
  await expect(page.locator("#download_data")).toHaveText("Download Processed Spectra");
  const processedDownload = await consumeDownload(page);
  expect(processedDownload.filename).toMatch(/^Processed-Spectra-.*\.csv$/i);
  const processedText = processedDownload.content.toString("utf8");
  expect(processedText).toMatch(/signal_to_noise/i);
  expect(processedText).toMatch(/quantification_source/i);
  expect(processedText).toMatch(/displayed_processed_spectra/i);
  expect(processedText).toMatch(/quantification_definitions/i);
  expect(processedText).toMatch(/area_ratio_custom_carbonyl/i);
  expect(processedText).toMatch(/peak_ratio_custom_peak/i);
  expect(processedText).toMatch(/area_under_band_custom_area/i);
  expect(processedText).toMatch(/point_intensity_custom_intensity/i);
  expect(processedText).toMatch(/raman_hdpe/i);
  await page.locator("#active_identification").check({ force: true });
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, { timeout: 240000 });
  await expect.poll(() => nonemptyTraces(page), { timeout: 240000 }).toHaveLength(3);
  await expect(overlay).toBeHidden({ timeout: 30000 });

  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await expect.poll(async () => page.locator("#MyPlotC").evaluate((plot) =>
    plot.layout.legend.orientation
  )).toBe("h");
  const mobileLegend = await page.locator("#MyPlotC").evaluate((plot) => ({
    orientation: plot.layout.legend.orientation,
    y: plot.layout.legend.y,
    bottomMargin: plot.layout.margin.b,
  }));
  expect(mobileLegend.y).toBeLessThan(0);
  expect(mobileLegend.bottomMargin).toBeGreaterThanOrEqual(100);
  await selectizeOption(page, "map_color", "Match Value");
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return Boolean(trace?.showscale) && trace?.colorbar?.orientation === "h";
  }), { timeout: 30000 }).toBe(true);
  const mobileHeatmapLegendBounds = await page.locator("#heatmapA").evaluate((plot) => {
    const legend = plot.querySelector(".colorbar");
    const plotArea = plot.querySelector(".nsewdrag");
    if (!legend || !plotArea) return null;
    const legendBox = legend.getBoundingClientRect();
    const plotAreaBox = plotArea.getBoundingClientRect();
    return {
      legendBottom: legendBox.bottom,
      legendWidth: legendBox.width,
      plotTop: plotAreaBox.top,
      plotWidth: plotAreaBox.width,
    };
  });
  expect(mobileHeatmapLegendBounds).not.toBeNull();
  expect(mobileHeatmapLegendBounds.legendBottom)
    .toBeLessThanOrEqual(mobileHeatmapLegendBounds.plotTop + 8);
  expect(mobileHeatmapLegendBounds.legendWidth)
    .toBeLessThanOrEqual(mobileHeatmapLegendBounds.plotWidth);
  const mobileDownloadWidth = await page.locator("#download_data").evaluate((button) => {
    const buttonBox = button.getBoundingClientRect();
    const titleBox = button.closest(".card-title")?.getBoundingClientRect();
    return { button: buttonBox.width, available: titleBox?.width || 0 };
  });
  expect(mobileDownloadWidth.button).toBeGreaterThanOrEqual(250);
  expect(Math.abs(mobileDownloadWidth.button - mobileDownloadWidth.available))
    .toBeLessThanOrEqual(2);
  await page.screenshot({
    path: testInfo.outputPath("local-app-mobile-numeric-heatmap.png"),
    fullPage: true,
  });
  await selectizeOption(page, "map_color", "Match Name");
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((item) => item.type === "heatmap");
    return Boolean(trace?.showscale);
  }), { timeout: 30000 }).toBe(false);
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

  // Exercise the contextual map export natively too. Wait for a real heatmap
  // from the new upload before inspecting options; collapse=true can leave a
  // stale Thresholded Particles option behind while the replacement file is
  // still being processed.
  await page.locator("#active_identification").evaluate((input) => {
    if (input.checked) input.click();
  });
  await page.locator("#active_preprocessing").evaluate((input) => {
    if (input.checked) input.click();
  });
  await page.locator("#collapse_decision").evaluate((input) => {
    if (!input.checked) input.click();
  });
  await page.locator("#threshold_decision").evaluate((input) => {
    if (input.checked) input.click();
  });
  await expect(page.locator("#active_identification")).not.toBeChecked();
  await expect(page.locator("#active_preprocessing")).not.toBeChecked();
  await expect(page.locator("#collapse_decision")).toBeChecked();
  await expect(page.locator("#threshold_decision")).not.toBeChecked();
  const mapUploadPath = path.join(repo, "inst", "extdata", "CA_tiny_map.zip");
  await page.locator("#file").setInputFiles(mapUploadPath);
  await expect.poll(async () => page.locator("#file").evaluate((input) =>
    input.files?.[0]?.name || ""
  )).toBe("CA_tiny_map.zip");
  await expect(page.locator("#heatmap_frame")).toBeVisible({ timeout: 120000 });
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((candidate) =>
      candidate.type === "heatmap" && Array.isArray(candidate.z)
    );
    return trace ? trace.z.flat(Infinity).filter(Number.isFinite).length : 0;
  }), { timeout: 120000 }).toBeGreaterThan(1);
  await expect(page.locator("#map_color")).toBeAttached();
  await selectizeOption(page, "map_color", "Signal/Noise");
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((candidate) =>
      candidate.type === "heatmap" && Array.isArray(candidate.z)
    );
    const title = trace?.colorbar?.title?.text || trace?.colorbar?.title || "";
    return Boolean(trace?.showscale) && /Signal\/Noise/i.test(String(title));
  }), { timeout: 30000 }).toBe(true);
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  await expect(overlay).toBeHidden({ timeout: 120000 });
  await toggleCard(settingsCard);
  await expectCardCollapsed(settingsCard, false);
  await page.waitForTimeout(600);
  const advancedTab = page.getByRole("link", {
    name: "Advanced", exact: true,
  });
  await advancedTab.click();
  await expect(advancedTab).toHaveClass(/active/);
  await page.waitForTimeout(600);
  const signalThresholdCard = page.locator("#threshold_decision").locator(
    "xpath=ancestor::div[contains(concat(' ', normalize-space(@class), ' '), ' card ')][1]"
  );
  await toggleCard(signalThresholdCard);
  await expectCardCollapsed(signalThresholdCard, false);
  await expect(page.locator("#signal_selection + .selectize-control"))
    .toBeVisible();
  await page.locator("#collapse_log_type").evaluate((input) => {
    input.value = "Thresholds";
    if (window.jQuery && window.jQuery(input).data("selectpicker")) {
      window.jQuery(input).selectpicker("val", "Thresholds");
    }
    input.dispatchEvent(new Event("input", { bubbles: true }));
    input.dispatchEvent(new Event("change", { bubbles: true }));
    window.Shiny?.setInputValue(
      "collapse_log_type", "Thresholds", { priority: "event" }
    );
  });
  await expect(page.locator("#collapse_log_type")).toHaveValue("Thresholds");
  await selectizeOption(page, "signal_selection", "log_tot_sig");
  await page.locator("#MinSNR").fill("-10000");
  await page.locator("#MinSNR").evaluate(() => {
    window.Shiny?.setInputValue("MinSNR", -10000, { priority: "event" });
  });
  await page.waitForTimeout(250);
  await page.locator("#threshold_decision").evaluate((input) => input.click());
  await expect(page.locator("#threshold_decision")).toBeChecked();
  await expect.poll(async () => page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((candidate) =>
      candidate.type === "heatmap" && Array.isArray(candidate.z)
    );
    const values = (trace?.text || []).flat(Infinity).map((text) => {
      const match = String(text).match(/<br>snr:\s*([^<]+)/i);
      return match ? Number.parseFloat(match[1]) : NaN;
    }).filter(Number.isFinite).sort((left, right) => left - right);
    const spread = values.length > 1 ? values.at(-1) - values[0] : 0;
    return values.length > 1 && values[0] >= 0 && spread > 0 ? {
      min: values[0], max: values.at(-1), spread,
    } : null;
  }), { timeout: 120000 }).toMatchObject({
    min: expect.any(Number),
    max: expect.any(Number),
    spread: expect.any(Number),
  });
  const mapSnrRange = await page.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((candidate) =>
      candidate.type === "heatmap" && Array.isArray(candidate.z)
    );
    const values = (trace?.text || []).flat(Infinity).map((text) => {
      const match = String(text).match(/<br>snr:\s*([^<]+)/i);
      return match ? Number.parseFloat(match[1]) : NaN;
    }).filter(Number.isFinite).sort((left, right) => left - right);
    return values.length ? { min: values[0], max: values.at(-1) } : null;
  });
  expect(mapSnrRange?.min).toBeGreaterThanOrEqual(0);
  expect(mapSnrRange?.max).toBeGreaterThan(mapSnrRange?.min);
  const mapSnrThreshold = (mapSnrRange.min + mapSnrRange.max) / 2;
  await page.locator("#MinSNR").fill(String(mapSnrThreshold));
  await page.locator("#MinSNR").evaluate((input) => {
    window.Shiny?.setInputValue(
      "MinSNR", Number(input.value), { priority: "event" }
    );
  });
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  await expect(overlay).toBeHidden({ timeout: 120000 });
  const thresholdState = await page.evaluate(() => ({
    collapse: document.getElementById("collapse_decision")?.checked,
    logic: document.getElementById("collapse_log_type")?.value,
    metric: document.getElementById("signal_selection")?.value,
    threshold: document.getElementById("MinSNR")?.value,
    thresholdEnabled: document.getElementById("threshold_decision")?.checked,
  }));
  await testInfo.attach("final-map-threshold-state", {
    body: JSON.stringify(thresholdState, null, 2),
    contentType: "application/json",
  });
  expect(thresholdState).toMatchObject({
    collapse: true,
    logic: "Thresholds",
    metric: "log_tot_sig",
    thresholdEnabled: true,
  });
  expect(Number(thresholdState.threshold)).toBeCloseTo(mapSnrThreshold, 6);
  await expect.poll(async () => page.locator("#map_color").evaluate(
    (select) => Object.keys(select.selectize ? select.selectize.options : {})
  ), { timeout: 120000 }).toContain("Feature ID");
  await toggleCard(signalThresholdCard);
  await expectCardCollapsed(signalThresholdCard);
  await toggleCard(settingsCard);
  await expectCardCollapsed(settingsCard);
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  await expect(overlay).toBeHidden({ timeout: 120000 });
  await dismissQueuedAlerts(page);
  const snrThresholdBuckets = {};
  for (const status of ["warning", "success"]) {
    await clickPastQueuedAlerts(
      page, page.locator(`#quality_${status}_details`)
    );
    qualityModal = page.locator(".modal-content:visible");
    await expect(qualityModal).toContainText(
      status === "warning" ? "Spectral quality warnings" :
        "Successful spectral checks"
    );
    const findingCount = await qualityModal.locator(
      ".openspecy-quality-finding"
    ).count();
    await expect.poll(async () => Number(
      await page.locator(`#quality_${status}_count`).textContent()
    ), { timeout: 30000 }).toBe(findingCount);
    const thresholdFinding = qualityModal.locator(
      '[data-quality-test-id$=":snr_threshold"]'
    );
    snrThresholdBuckets[status] = await thresholdFinding.count();
    if (snrThresholdBuckets[status]) {
      await expect(thresholdFinding).toHaveAttribute(
        "data-quality-status", status
      );
    }
    await clickPastQueuedAlerts(
      page,
      qualityModal.getByRole("button", { name: "Close", exact: true })
    );
    await expect(qualityModal).toBeHidden();
  }
  expect(snrThresholdBuckets.warning + snrThresholdBuckets.success).toBe(1);
  await expect.poll(async () => page.locator("#download_selection").evaluate(
    (select) => Object.keys(select.selectize ? select.selectize.options : {})
  ), { timeout: 120000 }).toContain("Thresholded Particles");
  await waitForStableSelectizeGeneration(
    page.locator("#download_selection"),
    "Thresholded Particles"
  );
  await page.locator("#download_selection").evaluate((select) => {
    select.selectize.setValue("Thresholded Particles");
  });
  await expect(page.locator("#download_selection")).toHaveValue(
    "Thresholded Particles", { timeout: 120000 }
  );
  await expect(page.locator("#download_data")).toHaveText(
    "Download Thresholded Particles", { timeout: 120000 }
  );
  await page.waitForTimeout(750);
  await expect(page.locator("#download_selection")).toHaveValue(
    "Thresholded Particles"
  );
  const thresholdedDownload = await consumeDownload(page);
  expect(thresholdedDownload.filename).toMatch(
    /^Thresholded-Particles-.*\.csv$/i
  );
  expect(thresholdedDownload.content.toString("utf8")).toMatch(
    /wavenumber|feature_id/i
  );
  expect(popups).toEqual([]);
  expect(severeErrors).toEqual([]);
  expect(stderr).not.toMatch(/Warning: Error in|Execution halted/);
});

test("local FileSpecs path opens regions and materializes only the selected spectrum", async ({ page }, testInfo) => {
  test.skip(
    !fileSpecsSmokePath || !fs.existsSync(fileSpecsSmokePath),
    "Set OPENSPECY_LOCAL_FILE_SPECS_PATH to exercise a genuine local H5/ENVI source."
  );
  test.setTimeout(900000);
  const severeErrors = [];
  page.on("console", (message) => {
    if (message.type() === "error" &&
        /Error in|cannot allocate vector|package .* not found|there is no package/i.test(message.text())) {
      severeErrors.push(message.text());
    }
  });
  page.on("pageerror", (error) => severeErrors.push(error.message));

  await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("#file")).toBeAttached({ timeout: 60000 });
  await expect(page.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  for (const inputId of [
    "active_identification", "active_preprocessing", "collapse_decision",
    "threshold_decision",
  ]) {
    await page.locator(`#${inputId}`).evaluate((input) => {
      if (input.checked) input.click();
    });
  }

  const sourceCard = page.locator("#filespec_source_box");
  await expectCardCollapsed(sourceCard);
  await toggleCard(sourceCard);
  await expectCardCollapsed(sourceCard, false);
  await page.locator("#filespec_path").fill(fileSpecsSmokePath);
  await page.locator("#filespec_open").click();
  await expect(page.locator("#filespec_status")).toContainText(
    "Open read-only:", { timeout: 180000 }
  );
  await expect(page.locator("#filespec_status")).toContainText("indexed spectra");
  await expect(page.locator("#filespec_map")).toBeVisible({ timeout: 60000 });

  if (path.basename(fileSpecsSmokePath).toLowerCase() === "drop.h5") {
    await expect.poll(
      async () => page.locator("#filespec_region").evaluate((select) =>
        Object.keys(select.selectize ? select.selectize.options : {})
      ),
      { timeout: 60000 }
    ).toEqual(["Region1", "Region2", "Region3"]);
    await selectizeOption(page, "filespec_region", "Region3");
    await expect(page.locator("#filespec_status")).toContainText("Region3", {
      timeout: 60000,
    });
  } else {
    const regions = await page.locator("#filespec_region").evaluate((select) =>
      Object.keys(select.selectize ? select.selectize.options : {})
    );
    expect(regions.length).toBeGreaterThanOrEqual(1);
  }

  const fullViewport = await page.locator("#filespec_view_status").innerText();
  expect(fullViewport).toMatch(/pixels visible/i);
  const previewImage = page.locator("#filespec_map img");
  await expect(previewImage).toBeVisible();
  const viewportMatch = fullViewport.match(
    /X\s+([-+\d.e]+)\s+to\s+([-+\d.e]+),\s+Y\s+([-+\d.e]+)\s+to\s+([-+\d.e]+)/i
  );
  expect(viewportMatch).not.toBeNull();
  const [xmin, xmax, ymin, ymax] = viewportMatch.slice(1).map(Number);
  await page.evaluate(({ xmin, xmax, ymin, ymax }) => {
    window.Shiny.setInputValue("filespec_map_brush", {
      xmin: xmin + 0.15 * (xmax - xmin),
      xmax: xmin + 0.48 * (xmax - xmin),
      ymin: ymin + 0.20 * (ymax - ymin),
      ymax: ymin + 0.58 * (ymax - ymin),
    }, { priority: "event" });
  }, { xmin, xmax, ymin, ymax });
  await expect.poll(
    async () => page.locator("#filespec_view_status").innerText(),
    { timeout: 60000 }
  ).not.toBe(fullViewport);
  const brushedViewport = await page.locator("#filespec_view_status").innerText();
  await page.locator("#filespec_view_right").click();
  await expect.poll(
    async () => page.locator("#filespec_view_status").innerText(),
    { timeout: 60000 }
  ).not.toBe(brushedViewport);
  await page.locator("#filespec_view_reset").click();
  await expect(page.locator("#filespec_view_status")).toHaveText(fullViewport, {
    timeout: 60000,
  });

  await expect.poll(async () => page.locator("#MyPlotC").evaluate((plot) =>
    Math.max(0, ...(plot.data || []).map((trace) =>
      Array.isArray(trace.x) ? trace.x.length : 0
    ))
  ), { timeout: 120000 }).toBeGreaterThan(100);

  const statusBeforeClick = await page.locator("#filespec_status").innerText();
  const previewBox = await previewImage.boundingBox();
  expect(previewBox).not.toBeNull();
  await previewImage.click({
    position: {
      x: Math.max(1, Math.floor(previewBox.width * 0.8)),
      y: Math.max(1, Math.floor(previewBox.height * 0.7)),
    },
  });
  await expect.poll(
    async () => page.locator("#filespec_status").innerText(),
    { timeout: 60000 }
  ).not.toBe(statusBeforeClick);

  await waitForStableSelectizeGeneration(
    page.locator("#download_selection"), "Processed Spectra",
    { timeout: 120000 }
  );
  await page.locator("#download_selection").evaluate((select) => {
    select.selectize.setValue("Processed Spectra");
  });
  const selectedDownload = await fetchDownload(page.locator("#download_data"), {
    readyTimeout: 120000,
  });
  expect(selectedDownload.status).toBe(200);
  const selectedHeader = selectedDownload.content.split(/\r?\n/, 1)[0];
  expect(selectedHeader.split(",").length).toBeGreaterThan(100);
  expect(selectedHeader).toMatch(/col_id|file_name/i);
  expect(selectedDownload.content.length).toBeLessThan(2 * 1024 * 1024);
  await page.screenshot({
    path: testInfo.outputPath("local-filespec-selection.png"),
    fullPage: true,
  });
  await page.locator("#filespec_close").click();
  await expect(page.locator("#filespec_status")).toContainText(
    "Closed the file-backed source", { timeout: 60000 }
  );
  await expect(page.locator("#filespec_map")).toBeHidden();
  expect(severeErrors).toEqual([]);
});
