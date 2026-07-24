const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");

test("pkgdown embeds a working OpenSpecy Shinylive app", async ({ page }, testInfo) => {
  const url = process.env.SHINYLIVE_SMOKE_URL || "http://127.0.0.1:8080/";
  const expectedVersion = process.env.OPENSPECY_EXPECTED_VERSION;
  const consoleErrors = [];
  const runtimeDiagnostics = [];

  // WebAssembly startup, preprocessing, and identification share one browser
  // thread. Keep the overall budget above the sum of those real phases while
  // retaining shorter per-action timeouts for selector failures.
  test.setTimeout(900000);
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
  await expect(page.locator("#openspecy-app-frame")).toHaveAttribute(
    "src",
    "app/"
  );
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
  const fullscreenButton = page.locator("#openspecy-fullscreen");
  await expect(fullscreenButton).toBeEnabled();
  const fileInput = appFrame.locator("#file, input[type='file']").first();
  await expect(fileInput).toBeAttached({ timeout: 180000 });
  const identificationSwitch = appFrame.locator("#active_identification").first();
  await expect(identificationSwitch).toBeChecked();
  const firstMatch = appFrame.locator("#event table tbody tr").first();

  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("pkgdown-embedded-app-desktop.png"),
  });

  await fullscreenButton.click();
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  await expect(fullscreenButton).toHaveText("Exit full screen");
  await expect.poll(() => page.evaluate(() =>
    document.documentElement.classList.contains(
      "openspecy-app-fullscreen-open"
    )
  )).toBe(true);
  await page.keyboard.press("Escape");
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  await page.screenshot({
    path: testInfo.outputPath("openspecy-app-fullscreen.png"),
  });

  const uploadPath =
    process.env.OPENSPECY_SMOKE_UPLOAD ||
    path.resolve("inst", "extdata", "raman_hdpe.csv");
  expect(fs.existsSync(uploadPath)).toBe(true);
  await page.evaluate(() => {
    const chooserProbe = document.createElement("input");
    chooserProbe.id = "openspecy-file-chooser-probe";
    chooserProbe.type = "file";
    chooserProbe.style.position = "fixed";
    chooserProbe.style.inset = "8px auto auto 8px";
    chooserProbe.style.zIndex = "3000";
    document.body.appendChild(chooserProbe);
  });
  const chooserPromise = page.waitForEvent("filechooser");
  await page.locator("#openspecy-file-chooser-probe").click();
  const chooser = await chooserPromise;
  await chooser.setFiles(uploadPath);
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  await page.locator("#openspecy-file-chooser-probe").evaluate((probe) => {
    probe.remove();
  });

  await appFrame.locator("html").evaluate((html) => {
    window.__openspecyBusyTransitions = [];
    window.__openspecyBusyObserver = new MutationObserver(() => {
      window.__openspecyBusyTransitions.push({
        at: performance.now(),
        visible: html.classList.contains("openspecy-busy-visible"),
      });
    });
    window.__openspecyBusyObserver.observe(html, {
      attributes: true,
      attributeFilter: ["class"],
    });
  });

  await fileInput.setInputFiles(uploadPath);
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, {
    timeout: 600000,
  });
  await expect(appFrame.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  await expect(identificationSwitch).toBeChecked();

  if (await identificationSwitch.count()) {
    await appFrame.locator("html").evaluate(() => {
      window.__openspecyResultSeenAt = performance.now();
    });
    await page.waitForTimeout(1000);
    const busyAfterResult = await appFrame.locator("html").evaluate(() =>
      window.__openspecyBusyTransitions.some((transition) =>
        transition.visible && transition.at >= window.__openspecyResultSeenAt
      )
    );
    expect(busyAfterResult).toBe(false);
    await expect(appFrame.locator("html")).not.toHaveClass(
      /\bopenspecy-busy-visible\b/
    );

    // The Download card intentionally starts collapsed. Identification makes
    // Top Matches the contextual default, and the native header download
    // button remains usable without opening the hidden configuration body.
    const downloadSelection = appFrame.locator("#download_selection");
    await expect(downloadSelection).toHaveValue("Top Matches");
    await expect(appFrame.locator("#top_n_input")).toBeAttached({
      timeout: 120000,
    });
    const handlerResult = await appFrame.locator("#download_data").evaluate(
      async (link) => {
        const response = await fetch(link.href, { cache: "no-store" });
        return {
          ok: response.ok,
          status: response.status,
          disposition: response.headers.get("content-disposition") || "",
          contentType: response.headers.get("content-type") || "",
          text: await response.text(),
        };
      }
    );
    expect(handlerResult.ok).toBe(true);
    expect(handlerResult.status).toBe(200);
    expect(handlerResult.disposition).toMatch(/Top-Matches.*\.csv/i);
    expect(handlerResult.text).toMatch(/poly\(ethylene\)/i);

    const downloadPromise = page.waitForEvent("download");
    await appFrame.locator("#download_data").click({ force: true });
    const download = await downloadPromise;
    const downloadFailure = await download.failure();
    if (downloadFailure) {
      const diagnostics = [
        `Download failure: ${downloadFailure}`,
        ...runtimeDiagnostics,
      ].join("\n");
      await testInfo.attach("shinylive-download-diagnostics", {
        body: diagnostics,
        contentType: "text/plain",
      });
      console.error(`Shinylive download diagnostics:\n${diagnostics}`);
    }
    if (process.platform === "win32" && downloadFailure === "canceled") {
      // Chromium can cancel Service Worker-backed attachment persistence under
      // Playwright on Windows even after the same endpoint returned the full
      // file above. GitHub's Linux gate must still complete the real download.
      expect(handlerResult.text.length).toBeGreaterThan(20);
    } else {
      expect(downloadFailure).toBeNull();
      expect(download.suggestedFilename()).toMatch(/^Top-Matches-.*\.csv$/i);
      const downloadPath = await download.path();
      expect(fs.statSync(downloadPath).size).toBeGreaterThan(0);
      expect(fs.readFileSync(downloadPath, "utf8")).toMatch(/poly\(ethylene\)/i);
    }
    await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  }

  const severeErrors = consoleErrors.filter((text) =>
    /Error in|package .* not found|there is no package|pinned build requires/i.test(text)
  );
  expect(severeErrors).toEqual([]);

  await fullscreenButton.click();
  await expect(embed).not.toHaveClass(/\bis-fullscreen\b/);
  await expect.poll(() => page.evaluate(() =>
    document.documentElement.classList.contains(
      "openspecy-app-fullscreen-open"
    )
  )).toBe(false);
  await expect(fullscreenButton).toHaveText("Full screen");

  await page.setViewportSize({ width: 390, height: 844 });
  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("pkgdown-embedded-app-mobile.png"),
  });
});
