const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");

test("pkgdown embeds a working OpenSpecy Shinylive app", async ({ page }, testInfo) => {
  const url = process.env.SHINYLIVE_SMOKE_URL || "http://127.0.0.1:8080/";
  const expectedVersion = process.env.OPENSPECY_EXPECTED_VERSION;
  const consoleErrors = [];
  const runtimeDiagnostics = [];

  test.setTimeout(420000);
  expect(expectedVersion).toBeTruthy();

  page.on("console", (message) => {
    const text = `[console:${message.type()}] ${message.text()}`;
    runtimeDiagnostics.push(text);
    if (message.type() === "error") consoleErrors.push(message.text());
  });
  page.on("pageerror", (error) => {
    runtimeDiagnostics.push(`[pageerror] ${error.message}`);
    consoleErrors.push(error.message);
  });
  page.on("requestfailed", (request) => {
    runtimeDiagnostics.push(
      `[requestfailed] ${request.method()} ${request.url()} ` +
        `${request.failure()?.errorText || "unknown error"}`
    );
  });
  page.on("response", (response) => {
    if (response.status() >= 400) {
      runtimeDiagnostics.push(
        `[response:${response.status()}] ${response.request().method()} ${response.url()}`
      );
    }
  });

  await page.goto(url, { waitUntil: "domcontentloaded" });
  const embed = page.locator("[data-openspecy-embed]");
  await expect(embed).toBeAttached();
  await expect(page.locator("[data-openspecy-loading]")).toBeVisible();
  await expect(page.getByRole("progressbar", {
    name: "Loading the OpenSpecy web application",
  })).toBeVisible();
  await expect(page.locator("#openspecy-fullscreen")).toBeDisabled();
  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("pkgdown-app-loading.png"),
  });

  const shinyliveFrame = page.frameLocator("#openspecy-app-frame");
  const appFrame = shinyliveFrame.frameLocator("iframe.app-frame");
  try {
    await expect(appFrame.locator("body")).toContainText(`OpenSpecy ${expectedVersion}`, {
      timeout: 180000,
    });
  } catch (error) {
    const rootHtml = await page.locator("#root").innerHTML().catch(() => "<unavailable>");
    runtimeDiagnostics.push(`[root] ${rootHtml}`);
    const shellHtml = await shinyliveFrame
      .locator("#root")
      .innerHTML()
      .catch(() => "<unavailable>");
    runtimeDiagnostics.push(`[shinylive-shell] ${shellHtml}`);
    const appBody = await appFrame.locator("body").innerHTML().catch(() => "<unavailable>");
    runtimeDiagnostics.push(`[app-frame-body] ${appBody}`);
    const diagnostics = runtimeDiagnostics.join("\n");
    await testInfo.attach("shinylive-runtime-diagnostics", {
      body: diagnostics,
      contentType: "text/plain",
    });
    console.error(`Shinylive runtime diagnostics:\n${diagnostics}`);
    throw error;
  }
  await expect(embed).toHaveClass(/\bis-ready\b/, { timeout: 120000 });
  await expect(page.locator("[data-openspecy-loading]")).toBeHidden();
  await expect(page.locator("#openspecy-app-status")).toHaveText("Ready");
  await expect(page.locator("#openspecy-fullscreen")).toBeEnabled();
  const fileInput = appFrame.locator("#file, input[type='file']").first();
  await expect(fileInput).toBeAttached({ timeout: 180000 });

  const uploadPath =
    process.env.OPENSPECY_SMOKE_UPLOAD ||
    path.resolve("inst", "extdata", "raman_hdpe.csv");
  if (fs.existsSync(uploadPath)) {
    await fileInput.setInputFiles(uploadPath);
    await expect(appFrame.locator("body")).toContainText("Upload complete", {
      timeout: 180000,
    });
    await expect(appFrame.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
      timeout: 240000,
    });
  }

  const identificationSwitch = appFrame.locator("#active_identification").first();
  if (await identificationSwitch.count()) {
    await identificationSwitch.check({ force: true });
    const firstMatch = appFrame.locator("#event table tbody tr").first();
    await expect(firstMatch).toContainText(/poly\(ethylene\)/i, {
      timeout: 240000,
    });

    const downloadSelection = appFrame.locator("#download_selection");
    const downloadSelectize = downloadSelection.locator(
      "xpath=following-sibling::div[contains(@class, 'selectize-control')]"
    );
    await downloadSelectize.locator(".selectize-input").click();
    await downloadSelectize
      .locator(".selectize-dropdown-content .option")
      .filter({ hasText: /^Top Matches$/ })
      .click();
    await expect(downloadSelection).toHaveValue("Top Matches");
    await expect(appFrame.locator("#top_n_input")).toBeAttached({
      timeout: 120000,
    });
    const downloadPromise = page.waitForEvent("download");
    await appFrame.locator("#download_data").click({ force: true });
    const download = await downloadPromise;
    const downloadPath = await download.path();
    expect(download.suggestedFilename()).toMatch(/^Top Matches.*\.csv$/);
    expect(fs.statSync(downloadPath).size).toBeGreaterThan(0);
    expect(fs.readFileSync(downloadPath, "utf8")).toMatch(/poly\(ethylene\)/i);
  }

  const severeErrors = consoleErrors.filter((text) =>
    /Error in|package .* not found|there is no package|pinned build requires/i.test(text)
  );
  expect(severeErrors).toEqual([]);

  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("pkgdown-embedded-app-desktop.png"),
  });

  const fullscreenButton = page.locator("#openspecy-fullscreen");
  await fullscreenButton.click();
  await expect.poll(() => page.evaluate(() =>
    document.fullscreenElement && document.fullscreenElement.id
  )).toBe("openspecy-app-shell");
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  await page.screenshot({
    path: testInfo.outputPath("openspecy-app-fullscreen.png"),
  });
  await fullscreenButton.click();
  await expect.poll(() => page.evaluate(() => document.fullscreenElement)).toBeNull();
  await expect(embed).not.toHaveClass(/\bis-fullscreen\b/);
  await expect(fullscreenButton).toHaveText("Full screen");

  await page.setViewportSize({ width: 390, height: 844 });
  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("pkgdown-embedded-app-mobile.png"),
  });
});
