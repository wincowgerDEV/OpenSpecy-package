const { test, expect } = require("@playwright/test");
const path = require("path");

const landingUrl = process.env.PAGES_SHELL_SMOKE_URL;
const screenshotDir = process.env.PAGES_SHELL_SCREENSHOT_DIR;
if (!landingUrl) {
  throw new Error("PAGES_SHELL_SMOKE_URL is required.");
}
if (!screenshotDir) {
  throw new Error("PAGES_SHELL_SCREENSHOT_DIR is required.");
}

test.setTimeout(120000);

test("static landing shell is crawlable, responsive, and base-path safe", async ({ page }) => {
  const severe = [];
  const failed = [];
  const landingOrigin = new URL(landingUrl).origin;
  page.on("console", (message) => {
    if (message.type() !== "error") return;
    const sourceUrl = message.location().url;
    if (!sourceUrl || new URL(sourceUrl).origin === landingOrigin) {
      severe.push(message.text());
    }
  });
  page.on("pageerror", (error) => severe.push(error.message));
  page.on("requestfailed", (request) => {
    if (new URL(request.url()).origin === landingOrigin) {
      failed.push(`${request.method()} ${request.url()}: ${request.failure()?.errorText || "failed"}`);
    }
  });

  const response = await page.goto(landingUrl, { waitUntil: "networkidle" });
  expect(response && response.ok()).toBeTruthy();
  await expect(page).toHaveTitle(/OpenSpecy.*Raman.*FTIR/i);
  await expect(page.locator("h1")).toHaveCount(1);
  await expect(page.locator("main")).toBeVisible();

  const description = await page.locator('meta[name="description"]').getAttribute("content");
  expect(description && description.length).toBeGreaterThan(80);
  expect(description && description.length).toBeLessThan(180);
  await expect(page.locator('link[rel="canonical"]')).toHaveAttribute(
    "href",
    /^https:\/\/wincowgerdev\.github\.io\/OpenSpecy-package\/$/
  );
  await expect(page.locator('meta[property="og:title"]')).toHaveAttribute("content", /OpenSpecy/);
  await expect(page.locator('meta[name="twitter:card"]')).toHaveAttribute("content", /summary/);

  const structured = await page.locator('script[type="application/ld+json"]').allTextContents();
  expect(structured.length).toBeGreaterThan(0);
  const records = structured.map((value) => JSON.parse(value));
  expect(records.some((record) => {
    const types = Array.isArray(record["@type"]) ? record["@type"] : [record["@type"]];
    return types.includes("WebApplication") || types.includes("SoftwareApplication");
  })).toBeTruthy();
  const heroVideo = page.locator(".hero-video-card iframe");
  await expect(heroVideo).toBeVisible();
  await expect(heroVideo).toHaveAttribute(
    "src",
    /youtube-nocookie\.com\/embed\/8zrlQeTCwkQ\?autoplay=1&mute=1&playsinline=1&rel=0/
  );
  await expect(heroVideo).toHaveAttribute("allow", /autoplay/);
  await expect(page.locator(".video-load")).toBeVisible();
  await expect(page.locator("[data-video-embed]")).toHaveAttribute(
    "data-video-src",
    /youtube-nocookie\.com/
  );
  await expect(page.locator("[data-video-embed] iframe")).toHaveCount(0);

  const iframe = page.locator("#openspecy-app-frame");
  await expect(iframe).toHaveAttribute("src", "app/");
  const iframeUrl = new URL(await iframe.getAttribute("src"), page.url());
  expect(iframeUrl.pathname).toMatch(/\/site\/app\/$/);
  const docsLink = page.locator('a[href="pkgdown/"]').first();
  await expect(docsLink).toBeVisible();
  const docsUrl = new URL(await docsLink.getAttribute("href"), page.url());
  expect(docsUrl.pathname).toMatch(/\/site\/pkgdown\/$/);

  await page.screenshot({
    path: path.join(screenshotDir, "landing-desktop.png"),
    fullPage: true
  });

  const appFrame = page.frames().find((frame) => /\/site\/app\/?$/.test(new URL(frame.url()).pathname));
  expect(appFrame).toBeTruthy();
  await appFrame.evaluate(() => {
    window.parent.postMessage({ type: "openspecy:ready" }, window.location.origin);
  });
  const expand = page.locator("#openspecy-fullscreen");
  await expect(expand).toBeEnabled();
  await expand.click();
  await expect(page.locator("[data-openspecy-embed]")).toHaveClass(/is-fullscreen/);
  await expect(page.locator("html")).toHaveClass(/openspecy-app-fullscreen-open/);
  await page.keyboard.press("Escape");
  await expect(page.locator("[data-openspecy-embed]")).toHaveClass(/is-fullscreen/);
  await page.screenshot({ path: path.join(screenshotDir, "landing-expanded.png") });
  await expand.click();
  await expect(page.locator("[data-openspecy-embed]")).not.toHaveClass(/is-fullscreen/);
  expect(failed).toEqual([]);
  expect(severe).toEqual([]);

  const docsResponse = await page.goto(docsUrl.href, { waitUntil: "networkidle" });
  expect(docsResponse && docsResponse.ok()).toBeTruthy();
  await expect(page.locator(".template-home")).toBeVisible();
  await expect(page.locator('script[src="pkgdown.js"]')).toBeAttached();
  await page.screenshot({
    path: path.join(screenshotDir, "landing-pkgdown.png"),
    fullPage: true
  });

  failed.length = 0;
  severe.length = 0;
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(landingUrl, { waitUntil: "networkidle" });
  await expect(page.locator("h1")).toBeVisible();
  const bodyWidth = await page.evaluate(() => document.body.scrollWidth);
  const viewportWidth = await page.evaluate(() => document.documentElement.clientWidth);
  expect(bodyWidth).toBeLessThanOrEqual(viewportWidth + 1);
  await page.screenshot({
    path: path.join(screenshotDir, "landing-mobile.png"),
    fullPage: true
  });

  expect(failed).toEqual([]);
  expect(severe).toEqual([]);
});
