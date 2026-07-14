const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");

test("OpenSpecy Shinylive app starts and exposes upload UI", async ({ page }) => {
  const url = process.env.SHINYLIVE_SMOKE_URL || "http://127.0.0.1:8080/openspecy/";
  const expectedVersion = process.env.OPENSPECY_EXPECTED_VERSION;
  const consoleErrors = [];

  test.setTimeout(420000);
  expect(expectedVersion).toBeTruthy();

  page.on("console", (message) => {
    if (message.type() === "error") consoleErrors.push(message.text());
  });
  page.on("pageerror", (error) => consoleErrors.push(error.message));

  await page.goto(url, { waitUntil: "domcontentloaded" });
  await expect(page.locator("body")).toContainText(`OpenSpecy ${expectedVersion}`, {
    timeout: 180000,
  });
  const fileInput = page.locator("#file, input[type='file']").first();
  await expect(fileInput).toBeAttached({ timeout: 180000 });

  const uploadPath =
    process.env.OPENSPECY_SMOKE_UPLOAD ||
    path.resolve("inst", "extdata", "raman_hdpe.csv");
  if (fs.existsSync(uploadPath)) {
    await fileInput.setInputFiles(uploadPath);
    await expect(page.locator("body")).toContainText("Upload complete", {
      timeout: 180000,
    });
  }

  const identificationSwitch = page.locator("#active_identification").first();
  if (await identificationSwitch.count()) {
    await identificationSwitch.check({ force: true });
    const firstMatch = page.locator("#event table tbody tr").first();
    await expect(firstMatch).toContainText(/poly\(ethylene\)/i, {
      timeout: 240000,
    });

    const downloadSelection = page.locator("#download_selection");
    await downloadSelection.selectOption({ label: "Top Matches" });
    const downloadPromise = page.waitForEvent("download");
    await page.locator("#download_data").click({ force: true });
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
