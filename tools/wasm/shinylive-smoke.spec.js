const { test, expect } = require("@playwright/test");
const fs = require("fs");
const path = require("path");
const { readZipEntryNames } = require("./zip-entries");

function bytePreview(content, limit = 32) {
  const bytes = content.subarray(0, limit);
  return {
    hex: Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join(" "),
    text: bytes.toString("utf8").replace(/[^\x20-\x7e]/g, "."),
  };
}

function dispositionFilename(disposition) {
  const encoded = disposition.match(/filename\*=UTF-8''([^;]+)/i);
  if (encoded) {
    try {
      return decodeURIComponent(encoded[1]);
    } catch (_error) {
      return encoded[1];
    }
  }
  const plain = disposition.match(/filename="?([^";]+)"?/i);
  return plain ? plain[1] : "";
}

async function summarizeResponse(response) {
  const headers = response.headers();
  let content = null;
  let bodyError = null;
  try {
    content = await response.body();
  } catch (error) {
    bodyError = error.message;
  }
  const preview = content ? bytePreview(content) : null;
  return {
    requestUrl: response.url(),
    requestMethod: response.request().method(),
    status: response.status(),
    contentType: headers["content-type"] || "",
    disposition: headers["content-disposition"] || "",
    length: content ? content.length : null,
    firstBytesHex: preview ? preview.hex : "",
    firstBytesText: preview ? preview.text : "",
    bodyError,
  };
}

async function probeDownloadEndpoint(link) {
  try {
    return await link.evaluate(async (element) => {
      const response = await fetch(element.href, { cache: "no-store" });
      const bytes = new Uint8Array(await response.arrayBuffer());
      const firstBytes = Array.from(bytes.slice(0, 32));
      return {
        requestUrl: element.href,
        ok: response.ok,
        status: response.status,
        contentType: response.headers.get("content-type") || "",
        disposition: response.headers.get("content-disposition") || "",
        length: bytes.length,
        firstBytes,
        firstBytesHex: firstBytes
          .map((byte) => byte.toString(16).padStart(2, "0"))
          .join(" "),
        firstBytesText: String.fromCharCode(...firstBytes)
          .replace(/[^\x20-\x7e]/g, "."),
      };
    });
  } catch (error) {
    return { error: error.message };
  }
}

async function selectDownload(
  downloadSelection,
  downloadLink,
  value,
  { timeout = 300000, stableFor = 0 } = {}
) {
  await downloadSelection.evaluate((select, nextValue) => {
    if (!select.selectize) {
      throw new Error("Download selection is missing its Selectize controller");
    }
    select.selectize.setValue(nextValue);
  }, value);
  await expect(downloadSelection).toHaveValue(value, { timeout });
  await expect(downloadLink).toContainText(`Download ${value}`, { timeout });
  await expect(downloadLink).toHaveAttribute(
    "aria-label", `Download ${value}`, { timeout }
  );
  if (stableFor > 0) {
    await downloadLink.evaluate((_, delay) => new Promise((resolve) => {
      setTimeout(resolve, delay);
    }), stableFor);
    await expect(downloadSelection).toHaveValue(value);
    await expect(downloadLink).toContainText(`Download ${value}`);
  }
}

async function waitForStableDownloadGeneration(
  downloadSelection,
  requiredOption,
  { timeout = 300000, stableFor = 2000 } = {}
) {
  await expect.poll(async () => downloadSelection.evaluate(
    (select, expected) => {
      const options = Object.keys(select.selectize ? select.selectize.options : {});
      if (!options.includes(expected)) return -1;
      const signature = options.join("\u001f");
      const now = performance.now();
      const previous = window.__openspecyDownloadGeneration;
      if (!previous || previous.node !== select ||
          previous.signature !== signature) {
        window.__openspecyDownloadGeneration = {
          node: select,
          signature,
          since: now,
        };
        return 0;
      }
      return now - previous.since;
    },
    requiredOption
  ), { timeout }).toBeGreaterThanOrEqual(stableFor);
}

async function setShinyCheckbox(input, checked) {
  await input.evaluate((element, nextValue) => {
    if (Boolean(element.checked) !== nextValue) element.click();
  }, checked);
  if (checked) {
    await expect(input).toBeChecked();
  } else {
    await expect(input).not.toBeChecked();
  }
}

async function dismissQueuedAlerts(root) {
  for (let attempt = 0; attempt < 8; attempt += 1) {
    const alert = root.locator(
      ".swal2-popup.swal2-show, .sweet-alert.showSweetAlert.visible"
    ).first();
    if (!await alert.isVisible().catch(() => false)) return;
    const confirm = alert.locator(
      "button.swal2-confirm, button.confirm, button:has-text('OK')"
    ).first();
    if (!await confirm.isVisible().catch(() => false)) return;
    await confirm.click({ force: true });
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
}

async function verifyNativeDownload({
  page,
  link,
  label,
  filenamePattern,
  contentTypePattern,
  contentPattern,
  expectedPrefix,
  probeEndpoint = true,
  testInfo,
  runtimeDiagnostics,
}) {
  await expect(link).toBeVisible();
  await expect.poll(async () => link.getAttribute("href"), {
    timeout: 30000,
  }).toMatch(/(?:^|\/)session\/[^/]+\/download\/download_data/);
  await expect(link).not.toHaveClass(/\bdisabled\b/, { timeout: 300000 });

  const linkState = await link.evaluate((element) => ({
    href: element.href,
    download: element.getAttribute("download"),
    target: element.getAttribute("target"),
  }));
  const clickResponsePromise = page.waitForResponse(
    (response) => response.url() === linkState.href,
    { timeout: 30000 }
  ).then(summarizeResponse).catch((error) => ({ error: error.message }));
  let download = null;
  let eventError = null;
  try {
    [download] = await Promise.all([
      page.waitForEvent("download", { timeout: 30000 }),
      link.click({ force: true }),
    ]);
  } catch (error) {
    eventError = error.message;
  }

  let failure = null;
  let suggestedFilename = null;
  let downloadPath = null;
  let content = null;
  let pathError = null;
  if (download) {
    failure = await download.failure();
    suggestedFilename = download.suggestedFilename();
    if (!failure) {
      try {
        downloadPath = await download.path();
        if (downloadPath) content = fs.readFileSync(downloadPath);
      } catch (error) {
        pathError = error.message;
      }
    }
  }

  const clickResponse = await Promise.race([
    clickResponsePromise,
    page.waitForTimeout(1000).then(() => ({
      error: "No matching click response was observed within one second",
    })),
  ]);

  // This duplicate GET is diagnostic only. The click-to-disk artifact below
  // remains authoritative, so a successful fetch can never excuse a canceled
  // or empty native browser download.
  const endpoint = probeEndpoint ? await probeDownloadEndpoint(link) : null;
  const endpointFilename = endpoint && !endpoint.error ?
    dispositionFilename(endpoint.disposition) : "";
  const preview = content ? bytePreview(content) : null;
  const problems = [];
  if (eventError) problems.push(`download event: ${eventError}`);
  if (!download) problems.push("no native browser download was captured");
  if (failure) problems.push(`native download failure: ${failure}`);
  if (pathError) problems.push(`download path: ${pathError}`);
  if (!suggestedFilename || !filenamePattern.test(suggestedFilename)) {
    problems.push(`unexpected filename: ${suggestedFilename || "<none>"}`);
  }
  if (!content || content.length === 0) {
    problems.push("downloaded file is missing or empty");
  }
  if (content && contentPattern && !contentPattern.test(content.toString("utf8"))) {
    problems.push("downloaded file content did not match the expected payload");
  }
  if (content && expectedPrefix &&
      !content.subarray(0, expectedPrefix.length).equals(expectedPrefix)) {
    problems.push(`downloaded file did not start with ${expectedPrefix.toString("hex")}`);
  }
  if (!probeEndpoint) {
    if (clickResponse.error) {
      problems.push(`click response: ${clickResponse.error}`);
    } else {
      if (clickResponse.status !== 200) {
        problems.push(`click response status: ${clickResponse.status}`);
      }
      if (!contentTypePattern.test(clickResponse.contentType)) {
        problems.push(
          `unexpected click MIME type: ${clickResponse.contentType || "<none>"}`
        );
      }
      const clickFilename = dispositionFilename(clickResponse.disposition);
      if (!clickFilename || !filenamePattern.test(clickFilename)) {
        problems.push(
          `unexpected click disposition: ${clickResponse.disposition || "<none>"}`
        );
      }
      // Chromium may expose an empty response.body() for a streamed
      // attachment even when the native download artifact is complete. The
      // saved click-to-disk bytes above remain authoritative here.
    }
  } else if (endpoint.error) {
    problems.push(`endpoint probe: ${endpoint.error}`);
  } else {
    if (!endpoint.ok || endpoint.status !== 200) {
      problems.push(`endpoint status: ${endpoint.status}`);
    }
    if (!contentTypePattern.test(endpoint.contentType)) {
      problems.push(`unexpected MIME type: ${endpoint.contentType || "<none>"}`);
    }
    if (!endpointFilename || !filenamePattern.test(endpointFilename)) {
      problems.push(`unexpected disposition: ${endpoint.disposition || "<none>"}`);
    }
    if (endpoint.length === 0) problems.push("endpoint response was empty");
  }

  const evidence = {
    label,
    request: linkState,
    clickResponse,
    eventError,
    failure,
    suggestedFilename,
    downloadPath,
    savedBytes: content ? content.length : 0,
    savedFirstBytesHex: preview ? preview.hex : "",
    savedFirstBytesText: preview ? preview.text : "",
    endpoint,
    endpointFilename,
    problems,
    runtimeDiagnostics: problems.length ? runtimeDiagnostics.slice(-100) : [],
  };
  await testInfo.attach(
    `shinylive-download-${label.toLowerCase().replace(/[^a-z0-9]+/g, "-")}`,
    {
      body: JSON.stringify(evidence, null, 2),
      contentType: "application/json",
    }
  );

  if (problems.length) {
    const diagnostics = JSON.stringify(evidence, null, 2);
    console.error(`Shinylive ${label} download diagnostics:\n${diagnostics}`);
    throw new Error(`${label} native download failed: ${problems.join("; ")}`);
  }

  return {
    content,
    filename: suggestedFilename,
    path: downloadPath,
    endpoint: endpoint || clickResponse,
  };
}

test("landing page embeds a working OpenSpecy Shinylive app", async ({ page }, testInfo) => {
  const url = process.env.SHINYLIVE_SMOKE_URL || "http://127.0.0.1:8080/";
  const expectedVersion = process.env.OPENSPECY_EXPECTED_VERSION;
  const consoleErrors = [];
  const runtimeDiagnostics = [];

  // WebAssembly startup, preprocessing, and identification share one browser
  // thread. Keep the overall budget above the sum of those real phases while
  // retaining shorter per-action timeouts for selector failures.
  test.setTimeout(1800000);
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

  const pkgdownUrl = new URL("pkgdown/", url).toString();
  const pkgdownResponse = await page.goto(pkgdownUrl, {
    waitUntil: "domcontentloaded",
  });
  expect(pkgdownResponse?.ok()).toBe(true);
  await expect(page.locator("body")).toContainText("OpenSpecy");
  await expect(page.locator("footer")).toContainText("pkgdown");
  await expect(page.locator("[data-openspecy-embed]")).toHaveCount(0);
  await page.screenshot({
    path: testInfo.outputPath("pkgdown-documentation.png"),
  });

  const landingResponse = await page.goto(url, {
    waitUntil: "domcontentloaded",
  });
  expect(landingResponse?.ok()).toBe(true);
  await expect(page).toHaveTitle(/OpenSpecy/i);
  await expect(page.getByRole("heading", { level: 1 })).toBeVisible();
  await expect(page.locator('meta[name="description"]')).toHaveAttribute(
    "content", /Raman|FTIR/i
  );
  await expect(page.locator('link[rel="canonical"]')).toHaveAttribute(
    "href", /^https:\/\//
  );
  const structuredData = page.locator('script[type="application/ld+json"]');
  await expect(structuredData).toHaveCount(1);
  const applicationSchema = JSON.parse(await structuredData.textContent());
  expect(applicationSchema.name).toBe("OpenSpecy");
  const applicationTypes = Array.isArray(applicationSchema["@type"])
    ? applicationSchema["@type"]
    : [applicationSchema["@type"]];
  expect(applicationTypes.some((type) =>
    ["SoftwareApplication", "WebApplication"].includes(type)
  )).toBe(true);
  await expect(page.locator('a[href^="pkgdown/"]').first()).toBeAttached();
  await page.screenshot({
    path: testInfo.outputPath("landing-page-desktop.png"),
  });
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
    path: testInfo.outputPath("landing-app-loading.png"),
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
  const runButton = appFrame.locator("#run_analysis").first();
  const firstMatch = appFrame.locator("#event table tbody tr").first();

  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("landing-embedded-app-desktop.png"),
  });

  await fullscreenButton.click();
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  await expect(fullscreenButton).toHaveText("Exit expanded view");
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

  const downloadSelection = appFrame.locator("#download_selection");
  const downloadLink = appFrame.locator("#download_data");
  await expect(downloadSelection).toHaveValue("Test Data");
  await expect(downloadLink).toContainText("Download Test Data");
  await verifyNativeDownload({
    page,
    link: downloadLink,
    label: "Test Data",
    filenamePattern: /^Test-Data-.*\.csv$/i,
    contentTypePattern: /^(?:text\/(?:csv|plain)|application\/octet-stream)/i,
    contentPattern: /wavenumber[\s,]+intensity/i,
    testInfo,
    runtimeDiagnostics,
  });
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);

  await selectDownload(downloadSelection, downloadLink, "Test Map");
  await verifyNativeDownload({
    page,
    link: downloadLink,
    label: "Test Map",
    filenamePattern: /^Test-Map-.*\.zip$/i,
    contentTypePattern: /^application\/(?:octet-stream|(?:x-)?zip(?:-compressed)?)/i,
    expectedPrefix: Buffer.from("PK", "ascii"),
    testInfo,
    runtimeDiagnostics,
  });
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);

  await selectDownload(downloadSelection, downloadLink, "User Metadata");
  await verifyNativeDownload({
    page,
    link: downloadLink,
    label: "User Metadata",
    filenamePattern: /^os_metadata_.*\.csv$/i,
    contentTypePattern: /^(?:text\/(?:csv|plain)|application\/octet-stream)/i,
    contentPattern: /recorded_at[\s,]+app_version[\s,]+session_id/i,
    testInfo,
    runtimeDiagnostics,
  });
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);

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
  await expect(runButton).toBeEnabled({ timeout: 60000 });
  await runButton.click();
  await expect(firstMatch).toContainText(/poly\(ethylene\)/i, {
    timeout: 600000,
  });
  await expect(appFrame.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 120000,
  });
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);

  {
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
    await expect(downloadSelection).toHaveValue("Top Matches");
    await expect(appFrame.locator("#top_n_input")).toBeAttached({
      timeout: 120000,
    });

    await selectDownload(downloadSelection, downloadLink, "Processed Spectra");
    await verifyNativeDownload({
      page,
      link: downloadLink,
      label: "Processed Spectra",
      filenamePattern: /^Processed-Spectra-.*\.csv$/i,
      contentTypePattern: /^(?:text\/(?:csv|plain)|application\/octet-stream)/i,
      contentPattern: /signal_to_noise/i,
      testInfo,
      runtimeDiagnostics,
    });

    await selectDownload(downloadSelection, downloadLink, "Top Matches");
    await expect(appFrame.locator("#top_n_input")).toBeAttached({
      timeout: 120000,
    });
    await verifyNativeDownload({
      page,
      link: downloadLink,
      label: "Top Matches",
      filenamePattern: /^Top-Matches-.*\.csv$/i,
      contentTypePattern: /^(?:text\/(?:csv|plain)|application\/octet-stream)/i,
      contentPattern: /poly\(ethylene\)/i,
      testInfo,
      runtimeDiagnostics,
    });
    await expect(embed).toHaveClass(/\bis-fullscreen\b/);
  }

  // A real multi-spectrum map is the memory-sensitive Top Matches case. Keep
  // identification enabled here: the one-spectrum fixture cannot expose a
  // full correlation-matrix expansion before Top N selection.
  const mapUploadPath = path.resolve("inst", "extdata", "CA_tiny_map.zip");
  expect(fs.existsSync(mapUploadPath)).toBe(true);
  await fileInput.setInputFiles(mapUploadPath);
  await expect.poll(async () => fileInput.evaluate((input) =>
    input.files?.[0]?.name || ""
  )).toBe("CA_tiny_map.zip");
  await expect(runButton).toBeEnabled({ timeout: 60000 });
  await runButton.click();
  // Wait for a map-owned output and an idle Shiny generation so that stale
  // Selectize state from the previous Raman upload cannot satisfy this check.
  await expect(appFrame.locator("#heatmap_frame")).toBeVisible({
    timeout: 300000,
  });
  await expect.poll(async () => appFrame.locator("#heatmapA").evaluate((plot) => {
    const trace = (plot.data || []).find((candidate) =>
      candidate.type === "heatmap" && Array.isArray(candidate.z)
    );
    return trace ? trace.z.flat(Infinity).filter(Number.isFinite).length : 0;
  }), { timeout: 300000 }).toBeGreaterThan(1);
  await expect(firstMatch).toBeVisible({ timeout: 600000 });
  await expect.poll(async () => firstMatch.textContent(), {
    timeout: 600000,
  }).toMatch(/\S/);
  await expect(appFrame.locator("#eventmetadata")).toContainText(
    "CA small UF.dat",
    { timeout: 900000 }
  );
  await dismissQueuedAlerts(appFrame);
  const mapSpectraCard = appFrame.locator("#spectra_box");
  const mapSidebarToggle = appFrame.locator("#mycardsidebar");
  await mapSidebarToggle.click();
  await expect(mapSpectraCard).toHaveClass(/direct-chat-contacts-open/);
  const mapSidebar = mapSpectraCard.locator(".direct-chat-contacts");
  await mapSidebar.getByRole("link", {
    name: "Uploaded Metadata", exact: true,
  }).click();
  const mapMetadataTable = mapSidebar.locator(
    "#sidebar_metadata .dataTables_scrollBody table"
  );
  await expect(mapMetadataTable).toBeVisible({
    timeout: 300000,
  });
  const nonFirstMapMetadataRow = mapMetadataTable.locator("tbody tr").nth(1);
  await expect(nonFirstMapMetadataRow).toContainText("0_1", {
    timeout: 300000,
  });
  await nonFirstMapMetadataRow.click();
  await expect(appFrame.locator("#eventmetadata table")).toContainText("0_1", {
    timeout: 300000,
  });
  await mapSidebarToggle.click();
  await expect(mapSpectraCard).not.toHaveClass(/direct-chat-contacts-open/);
  await expect(appFrame.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 300000,
  });
  await waitForStableDownloadGeneration(downloadSelection, "Top Matches");
  await selectDownload(
    downloadSelection,
    downloadLink,
    "Top Matches",
    { timeout: 300000, stableFor: 1500 }
  );
  // This fixture's 209-line assertion requires one retained match per map
  // spectrum. Configure that precondition explicitly: Top N is an analysis
  // control whose product default may change independently of this smoke.
  // The settings panel opens on another tab, so make the owning tab visible
  // before using Playwright's user-level form interaction.
  await appFrame.getByRole("link", {
    name: "Identification", exact: true,
  }).click();
  const mapTopNInput = appFrame.locator("#top_n_input");
  await expect(mapTopNInput).toBeVisible();
  await mapTopNInput.fill("1");
  await mapTopNInput.press("Tab");
  await expect(mapTopNInput).toHaveValue("1");
  const mapDiagnosticStart = runtimeDiagnostics.length;
  const mapTopMatches = await verifyNativeDownload({
    page,
    link: downloadLink,
    label: "Test Map Top Matches",
    filenamePattern: /^Top-Matches-.*\.csv$/i,
    contentTypePattern: /^(?:text\/(?:csv|plain)|application\/octet-stream)/i,
    contentPattern: /file_name.*col_id.*material_class.*match_val.*signal_to_noise/i,
    probeEndpoint: false,
    testInfo,
    runtimeDiagnostics,
  });
  const mapTopMatchLines = mapTopMatches.content.toString("utf8")
    .split(/\r?\n/).filter(Boolean);
  expect(mapTopMatchLines).toHaveLength(209);
  const mapDownloadLogs = runtimeDiagnostics.slice(mapDiagnosticStart).join("\n");
  expect(mapDownloadLogs).toMatch(/creating 'Top Matches' download/i);
  expect(mapDownloadLogs).toMatch(/completed 'Top Matches' download/i);
  expect(mapDownloadLogs).not.toMatch(/cannot allocate vector/i);

  // Prove that the map export leaves the WebR session healthy for another
  // map-dependent download instead of turning every later endpoint into HTTP 500.
  await selectDownload(downloadSelection, downloadLink, "Processed Spectra");
  await verifyNativeDownload({
    page,
    link: downloadLink,
    label: "Processed Spectra After Map",
    filenamePattern: /^Processed-Spectra-.*\.csv$/i,
    contentTypePattern: /^(?:text\/(?:csv|plain)|application\/octet-stream)/i,
    contentPattern: /wavenumber/i,
    testInfo,
    runtimeDiagnostics,
  });

  // Thresholded Particles is contextual to map uploads with collapsing on.
  // Enable the SNR threshold that owns the logical feature mask used for
  // collapsing, then click Run so the change actually takes effect.
  await setShinyCheckbox(appFrame.locator("#threshold_decision"), false);
  await setShinyCheckbox(appFrame.locator("#collapse_decision"), true);
  await expect(runButton).toBeEnabled({ timeout: 60000 });
  await runButton.click();
  await expect(appFrame.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 300000,
  });
  const mapSnrThreshold = await appFrame.locator("#heatmapA")
    .evaluate((plot) => {
      const trace = (plot.data || []).find((candidate) =>
        candidate.type === "heatmap" && Array.isArray(candidate.z)
      );
      const values = (trace?.z || []).flat(Infinity)
        .filter(Number.isFinite).sort((left, right) => left - right);
      return values.length ? values[Math.floor(values.length / 2)] : null;
    });
  expect(Number.isFinite(mapSnrThreshold)).toBe(true);
  await appFrame.locator("#MinSNR").evaluate((input, value) => {
    input.value = String(value);
    input.dispatchEvent(new Event("input", { bubbles: true }));
    input.dispatchEvent(new Event("change", { bubbles: true }));
    window.Shiny?.setInputValue("MinSNR", value, { priority: "event" });
  }, mapSnrThreshold);
  await setShinyCheckbox(appFrame.locator("#threshold_decision"), true);
  // Settings only take effect once Run is clicked; the download list must
  // not react to the threshold change above on its own.
  await expect(runButton).toBeEnabled({ timeout: 60000 });
  await runButton.click();
  await expect(appFrame.locator("html")).not.toHaveClass(/\bshiny-busy\b/, {
    timeout: 300000,
  });
  await expect(appFrame.locator("html")).not.toHaveClass(
    /\bopenspecy-busy-visible\b/, { timeout: 300000 }
  );
  await expect.poll(async () => downloadSelection.evaluate((select) =>
    Object.keys(select.selectize ? select.selectize.options : {})
  ), { timeout: 300000 }).toContain("Thresholded Particles");
  await waitForStableDownloadGeneration(
    downloadSelection,
    "Thresholded Particles"
  );
  await selectDownload(
    downloadSelection,
    downloadLink,
    "Thresholded Particles",
    { timeout: 300000, stableFor: 1500 }
  );
  const thresholdedParticles = await verifyNativeDownload({
    page,
    link: downloadLink,
    label: "Thresholded Particles",
    filenamePattern: /^Thresholded-Particles-.*\.zip$/i,
    contentTypePattern: /^application\/zip/i,
    expectedPrefix: Buffer.from("PK", "ascii"),
    testInfo,
    runtimeDiagnostics,
  });
  const thresholdedEntries = readZipEntryNames(thresholdedParticles.path);
  expect(thresholdedEntries).toContain("particle_summary.csv");
  expect(thresholdedEntries).toContain("particle_details.csv");
  await expect(embed).toHaveClass(/\bis-fullscreen\b/);

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
  await expect(fullscreenButton).toHaveText("Expand app");

  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() => window.scrollTo(0, 0));
  await page.screenshot({
    path: testInfo.outputPath("landing-page-mobile.png"),
  });
  await embed.scrollIntoViewIfNeeded();
  await page.screenshot({
    path: testInfo.outputPath("landing-embedded-app-mobile.png"),
  });
});
