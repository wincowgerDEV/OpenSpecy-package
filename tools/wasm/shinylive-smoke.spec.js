const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");

test("OpenSpecy Shinylive app starts and exposes upload UI", async ({ page }) => {
  const url = process.env.SHINYLIVE_SMOKE_URL || "http://127.0.0.1:8080/openspecy/";
  const consoleErrors = [];

  page.on("console", (message) => {
    if (message.type() === "error") consoleErrors.push(message.text());
  });
  page.on("pageerror", (error) => consoleErrors.push(error.message));

  await page.goto(url, { waitUntil: "domcontentloaded" });
  await expect(page.locator("body")).toContainText(/Open Specy|OpenSpecy/, {
    timeout: 180000,
  });
  const fileInput = page.locator("#file, input[type='file']").first();
  await expect(fileInput).toBeAttached({ timeout: 180000 });

  const uploadPath =
    process.env.OPENSPECY_SMOKE_UPLOAD ||
    path.resolve("inst", "extdata", "raman_hdpe.csv");
  if (fs.existsSync(uploadPath)) {
    await fileInput.setInputFiles(uploadPath);
    await page.waitForTimeout(5000);
  }

  const identificationSwitch = page.locator("#active_identification").first();
  if (await identificationSwitch.count()) {
    await identificationSwitch.check({ force: true });
    await page.waitForTimeout(15000);
  }

  const severeErrors = consoleErrors.filter((text) =>
    /Error in|package .* not found|there is no package/i.test(text)
  );
  expect(severeErrors).toEqual([]);
});
