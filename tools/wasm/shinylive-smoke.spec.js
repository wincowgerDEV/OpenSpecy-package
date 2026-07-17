const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");

test("OpenSpecy Shinylive app starts and exposes upload UI", async ({ page }, testInfo) => {
  const url = process.env.SHINYLIVE_SMOKE_URL || "http://127.0.0.1:8080/openspecy/";
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
  const appFrame = page.frameLocator("iframe.app-frame");
  try {
    await expect(appFrame.locator("body")).toContainText(`OpenSpecy ${expectedVersion}`, {
      timeout: 180000,
    });
  } catch (error) {
    const rootHtml = await page.locator("#root").innerHTML().catch(() => "<unavailable>");
    runtimeDiagnostics.push(`[root] ${rootHtml}`);
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
});
