const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");

const offlineUrl = process.env.OPENSPECY_OFFLINE_URL;
const expectedVersion = process.env.OPENSPECY_EXPECTED_VERSION;

if (!offlineUrl || !expectedVersion) {
  throw new Error(
    "OPENSPECY_OFFLINE_URL and OPENSPECY_EXPECTED_VERSION are required."
  );
}

// Shinylive itself needs its service worker. The deliberately unreachable
// proxy blocks every non-loopback request at Chromium's network layer,
// including requests initiated by a service worker; 127.0.0.1 bypasses it.
test.use({
  serviceWorkers: "allow",
  proxy: { server: "http://127.0.0.1:9", bypass: "127.0.0.1" },
});
test.setTimeout(900000);

async function setCheckbox(root, id, checked) {
  const input = root.locator(`#${id}`);
  await expect(input).toBeAttached();
  await input.evaluate((element, next) => {
    if (Boolean(element.checked) !== next) element.click();
  }, checked);
  if (checked) await expect(input).toBeChecked();
  else await expect(input).not.toBeChecked();
}

async function setShinyValues(root, values) {
  await root.locator("html").evaluate((_, nextValues) => {
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
      throw new Error("Shiny input bridge is not ready");
    }
    Object.entries(nextValues).forEach(([id, value]) => {
      window.Shiny.setInputValue(id, value, { priority: "event" });
    });
  }, values);
}

async function selectDownload(root, value) {
  const selection = root.locator("#download_selection");
  await expect(selection).toBeAttached({ timeout: 120000 });
  await selection.evaluate((element, next) => {
    if (!element.selectize) throw new Error("Download Selectize is unavailable");
    element.selectize.setValue(next);
  }, value);
  await expect(selection).toHaveValue(value, { timeout: 120000 });
  return root.locator("#download_data");
}

async function captureDownload(page, link, filenamePattern, contentPattern) {
  await expect(link).toBeVisible();
  await expect(link).not.toHaveClass(/\bdisabled\b/, { timeout: 120000 });
  await expect.poll(() => link.getAttribute("href"), { timeout: 120000 })
    .toMatch(/(?:^|\/)session\/[^/]+\/download\/download_data/);
  const [download] = await Promise.all([
    page.waitForEvent("download", { timeout: 180000 }),
    link.click({ force: true }),
  ]);
  expect(await download.failure()).toBeNull();
  expect(download.suggestedFilename()).toMatch(filenamePattern);
  const savedPath = await download.path();
  expect(savedPath).toBeTruthy();
  const content = fs.readFileSync(savedPath);
  expect(content.length).toBeGreaterThan(0);
  expect(content.toString("utf8")).toMatch(contentPattern);
  return content;
}

test("extracted launcher serves the landing page and runs the full app flow without internet", async ({ page, context }, testInfo) => {
  const entry = new URL(offlineUrl);
  expect(entry.hostname).toBe("127.0.0.1");
  expect(entry.pathname).toBe("/app/");
  const localOrigin = entry.origin;
  const externalAttempts = new Set();
  const externalWebSockets = new Set();
  const localFailures = [];
  const localHTTPFailures = [];
  const consoleErrors = [];

  context.on("request", (request) => {
    const requestUrl = new URL(request.url());
    if (["http:", "https:"].includes(requestUrl.protocol) &&
        requestUrl.origin !== localOrigin) {
      externalAttempts.add(`${request.method()} ${requestUrl.href}`);
    }
  });
  await context.route("**/*", async (route) => {
    const requestUrl = new URL(route.request().url());
    if (["http:", "https:"].includes(requestUrl.protocol) &&
        requestUrl.origin !== localOrigin) {
      externalAttempts.add(`${route.request().method()} ${requestUrl.href}`);
      await route.abort("blockedbyclient");
      return;
    }
    await route.continue();
  });
  page.on("requestfailed", (request) => {
    const requestUrl = new URL(request.url());
    if (requestUrl.origin === localOrigin) {
      localFailures.push(
        `${request.method()} ${requestUrl.href}: ` +
        `${request.failure()?.errorText || "failed"}`
      );
    }
  });
  page.on("response", (response) => {
    const responseUrl = new URL(response.url());
    if (responseUrl.origin === localOrigin && response.status() >= 400) {
      localHTTPFailures.push(`${response.status()} ${responseUrl.href}`);
    }
  });
  page.on("console", (message) => {
    if (message.type() === "error") consoleErrors.push(message.text());
  });
  page.on("pageerror", (error) => consoleErrors.push(error.message));
  page.on("websocket", (socket) => {
    const socketUrl = new URL(socket.url());
    if (["ws:", "wss:"].includes(socketUrl.protocol) &&
        socketUrl.host !== entry.host) {
      externalWebSockets.add(socketUrl.href);
    }
  });

  // Exercise the launcher's direct /app/ entry first in a fresh context. The
  // landing page embeds /app/ eagerly, so visiting / before this point would
  // pre-warm the service worker and mask a first-load-only offline failure.
  const response = await page.goto(offlineUrl, { waitUntil: "domcontentloaded" });
  expect(response && response.ok()).toBeTruthy();
  await expect(page.locator("iframe.app-frame")).toBeAttached({ timeout: 180000 });
  const app = page.frameLocator("iframe.app-frame");
  await expect(app.locator("body")).toContainText(`OpenSpecy ${expectedVersion}`, {
    timeout: 300000,
  });
  const mountedInput = app.locator("#openspecy_workerfs_files");
  await expect(mountedInput).toBeVisible({ timeout: 300000 });
  await expect(mountedInput).toBeEnabled({ timeout: 300000 });

  // Obtain the Raman fixture from the archive itself. This proves the bundle
  // needs no companion data download before the normal upload/Run path works.
  let downloadLink = await selectDownload(app, "Test Data");
  const bundledRaman = await captureDownload(
    page,
    downloadLink,
    /^Test-Data-.*\.csv$/i,
    /wavenumber[\s,]+intensity/i
  );
  const uploadPath = testInfo.outputPath("raman_hdpe-offline.csv");
  fs.writeFileSync(uploadPath, bundledRaman);

  for (const [id, checked] of [
    ["make_rel_decision", true],
    ["smooth_decision", true],
    ["derivative_abs", true],
    ["conform_decision", true],
    ["baseline_decision", false],
    ["identification_active", true],
    ["filter_lib", false],
    ["threshold_decision", false],
    ["cor_threshold_decision", false],
    ["spatial_decision", false],
    ["xy_grid", false],
    ["collapse_decision", false],
  ]) {
    await setCheckbox(app, id, checked);
  }
  await setShinyValues(app, {
    derivative_order: 1,
    id_spec_type: "raman",
    id_strategy: "deriv",
    lib_type: "medoid",
    top_n_input: 10,
  });

  expect(fs.existsSync(uploadPath)).toBe(true);
  await mountedInput.setInputFiles(path.resolve(uploadPath));
  await expect(app.locator("html")).toHaveAttribute(
    "data-openspecy-run-ready", "accepted", { timeout: 120000 }
  );
  const runButton = app.locator("#run_analysis").first();
  await expect(runButton).toBeEnabled({ timeout: 120000 });
  await runButton.click();

  const matches = app.locator("#event table tbody tr");
  await expect(matches.first()).toContainText(/poly\(ethylene\)/i, {
    timeout: 600000,
  });
  await expect.poll(() => matches.count(), { timeout: 120000 })
    .toBeGreaterThan(1);
  await expect(app.locator("#top_n_input")).toHaveValue("10");
  const selectionMetadata = app.locator("#eventmetadata table");
  await expect(selectionMetadata).toBeVisible({ timeout: 120000 });
  await expect(selectionMetadata).toContainText(
    /raman_hdpe|poly\(ethylene\)|material.class/i,
    { timeout: 120000 }
  );

  // DT's visual row class is a theme/client implementation detail and is not
  // reliable in the headless WebAssembly runtime. Verify the actual app
  // contract instead: each click must drive the server-owned Selection
  // Metadata table, and returning to rank one must restore its first result.
  const firstSelectionMetadata = await selectionMetadata.innerText();
  await matches.nth(1).click();
  await expect.poll(() => selectionMetadata.innerText(), { timeout: 60000 })
    .not.toBe(firstSelectionMetadata);
  await matches.first().click();
  await expect.poll(() => selectionMetadata.innerText(), { timeout: 60000 })
    .toBe(firstSelectionMetadata);

  downloadLink = await selectDownload(app, "Top Matches");
  await captureDownload(
    page,
    downloadLink,
    /^Top-Matches-.*\.csv$/i,
    /material_class.*match_val|match_val.*material_class/i
  );

  await setShinyValues(app, {
    quant_ratio_name: "Offline carbonyl",
    quant_ratio_type: "area",
    quant_numerator_area_min: 1650,
    quant_numerator_area_max: 1850,
    quant_denominator_area_min: 1420,
    quant_denominator_area_max: 1500,
  });
  await page.waitForTimeout(250);
  await app.locator("#quant_ratio_add").evaluate((button) => button.click());
  await expect(app.locator("#quant_saved_ratios")).toContainText(
    "Offline carbonyl", { timeout: 120000 }
  );
  await runButton.click();
  await expect(selectionMetadata).toContainText(
    /area_ratio_offline_carbonyl/i, { timeout: 300000 }
  );

  downloadLink = await selectDownload(app, "Processed Spectra");
  await captureDownload(
    page,
    downloadLink,
    /^Processed-Spectra-.*\.csv$/i,
    /area_ratio_offline_carbonyl/i
  );

  // Audit the complete landing route only after the first-load app flow. Its
  // two optional external videos must remain click-to-load placeholders.
  const landingResponse = await page.goto(new URL("/", entry).href, {
    waitUntil: "domcontentloaded",
  });
  expect(landingResponse && landingResponse.ok()).toBeTruthy();
  await expect(page.locator("[data-video-embed]")).toHaveCount(2);
  await expect(page.locator("[data-video-embed] .video-load")).toHaveCount(2);
  await expect(page.locator("[data-video-embed] iframe")).toHaveCount(0);
  expect([...externalAttempts]).toEqual([]);
  expect([...externalWebSockets]).toEqual([]);

  const severe = consoleErrors.filter((message) =>
    /Error in|package .* not found|there is no package|pinned build requires/i.test(message)
  );
  const unexpectedLocalFailures = localFailures.filter((message) =>
    !/net::ERR_ABORTED|NS_BINDING_ABORTED/i.test(message)
  );
  await testInfo.attach("offline-network-audit", {
    body: JSON.stringify({
      externalAttempts: [...externalAttempts],
      externalWebSockets: [...externalWebSockets], localFailures,
      unexpectedLocalFailures, localHTTPFailures, consoleErrors,
    }, null, 2),
    contentType: "application/json",
  });
  expect([...externalAttempts]).toEqual([]);
  expect([...externalWebSockets]).toEqual([]);
  expect(unexpectedLocalFailures).toEqual([]);
  expect(localHTTPFailures).toEqual([]);
  expect(severe).toEqual([]);
});
