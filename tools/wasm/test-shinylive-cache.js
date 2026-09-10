"use strict";

const assert = require("assert");
const path = require("path");

const workerPath = process.argv[2];
const packageSha = process.argv[3];
if (!workerPath || !/^[0-9a-f]{40}$/i.test(packageSha || "")) {
  throw new Error("Usage: node test-shinylive-cache.js WORKER PACKAGE_SHA");
}

const handlers = {};
let claimed = 0;
global.self = {
  location: { origin: "http://127.0.0.1:8080", pathname: "/app/shinylive-sw.js" },
  addEventListener(name, handler) {
    handlers[name] = handler;
  },
  skipWaiting: async () => undefined,
  clients: {
    claim: async () => {
      claimed += 1;
    },
  },
};

let stored = null;
let fetchCount = 0;
let matchCount = 0;
let putCount = 0;
let openFails = false;
let matchFails = false;
let putFails = false;
let fetchFails = false;
let responseStatus = 200;
let cacheKeys = [];
const deletedKeys = [];

function response(body, status = 200) {
  return {
    body,
    status,
    statusText: status === 200 ? "OK" : "Error",
    ok: status >= 200 && status < 300,
    headers: new Headers(),
    clone() {
      return response(body, status);
    },
    async text() {
      return body;
    },
  };
}

const cache = {
  async match() {
    matchCount += 1;
    if (matchFails) throw new Error("Cache read unavailable");
    return stored;
  },
  async put(request, value) {
    putCount += 1;
    if (putFails) throw new Error("Quota exceeded");
    stored = value;
  },
};

global.caches = {
  async open() {
    if (openFails) throw new Error("CacheStorage disabled");
    return cache;
  },
  async keys() {
    if (openFails) throw new Error("CacheStorage disabled");
    return cacheKeys;
  },
  async delete(key) {
    deletedKeys.push(key);
    return true;
  },
};
global.fetch = async () => {
  fetchCount += 1;
  if (fetchFails) throw new Error("Network unavailable");
  return response("network", responseStatus);
};

require(path.resolve(workerPath));
assert.strictEqual(typeof handlers.install, "function");
assert.strictEqual(typeof handlers.activate, "function");
assert.strictEqual(typeof handlers.fetch, "function");

async function dispatchFetch(relativePath, options = {}) {
  let responsePromise = null;
  const lifetime = [];
  const request = {
    url: new URL(relativePath, "http://127.0.0.1:8080/app/").href,
    method: "GET",
    cache: "default",
    referrer: "",
    headers: { has: () => false },
    ...options,
  };
  handlers.fetch({
    request,
    respondWith(value) {
      responsePromise = Promise.resolve(value);
    },
    waitUntil(value) {
      lifetime.push(Promise.resolve(value));
    },
  });
  const result = responsePromise ? await responsePromise : null;
  await Promise.all(lifetime);
  return result;
}

(async () => {
  for (const asset of [
    "app.json",
    "pinned-wasm-library.json",
    "shinylive/shinylive.js",
    "shinylive/webr/packages/OpenSpecyPinned/library.data.gz",
  ]) {
    stored = null;
    const beforeFetch = fetchCount;
    const beforePut = putCount;
    const cold = await dispatchFetch(asset);
    assert.strictEqual(await cold.text(), "network");
    assert.strictEqual(fetchCount, beforeFetch + 1);
    assert.strictEqual(putCount, beforePut + 1);

    fetchFails = true;
    const warm = await dispatchFetch(asset);
    fetchFails = false;
    assert.strictEqual(await warm.text(), "network");
    assert.strictEqual(fetchCount, beforeFetch + 1, `${asset} was not cache-first`);
  }

  const beforeOutside = fetchCount;
  assert.strictEqual(await dispatchFetch("other.js"), null);
  assert.strictEqual(await dispatchFetch("app.json", {
    url: "https://example.test/app/app.json",
  }), null);
  assert.strictEqual(await dispatchFetch("app.json", { cache: "no-store" }), null);
  assert.strictEqual(fetchCount, beforeOutside);

  stored = null;
  matchFails = true;
  const readFallback = await dispatchFetch("app.json");
  assert.strictEqual(await readFallback.text(), "network");
  matchFails = false;

  stored = null;
  openFails = true;
  const storageFallback = await dispatchFetch("app.json");
  assert.strictEqual(await storageFallback.text(), "network");
  let installLifetime;
  handlers.install({ waitUntil(value) { installLifetime = Promise.resolve(value); } });
  await installLifetime;
  openFails = false;

  stored = null;
  putFails = true;
  const quotaFallback = await dispatchFetch("app.json");
  assert.strictEqual(await quotaFallback.text(), "network");
  putFails = false;

  stored = null;
  responseStatus = 500;
  const beforeFailedPut = putCount;
  const failedResponse = await dispatchFetch("app.json");
  assert.strictEqual(failedResponse.status, 500);
  assert.strictEqual(putCount, beforeFailedPut, "unsuccessful responses were cached");
  responseStatus = 200;

  const prefix = "openspecy-shinylive-runtime-v1-";
  cacheKeys = [
    "foreign-cache",
    `${prefix}${"b".repeat(40)}`,
    `${prefix}${packageSha.toLowerCase()}`,
  ];
  let activateLifetime;
  handlers.activate({ waitUntil(value) { activateLifetime = Promise.resolve(value); } });
  await activateLifetime;
  assert.strictEqual(claimed, 1);
  assert.deepStrictEqual(deletedKeys, [`${prefix}${"b".repeat(40)}`]);

  assert.ok(matchCount > 0);
  console.log("Shinylive runtime cache fixture passed.");
})().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
