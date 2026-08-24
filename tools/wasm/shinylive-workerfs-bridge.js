/* OPENSPECY_WORKERFS_BRIDGE_V1
 * Injected by prepare-shinylive-app.R into the pinned Shinylive 0.5.0 bundle.
 * Browser File/Blob objects remain structured-cloned references until R reads
 * their WORKERFS paths; their bytes never enter Shinylive's HTTP upload body.
 */
const openspecyWorkerfsState = { mountpoint: null };

async function openspecyWorkerfsCleanup(fs) {
  if (!openspecyWorkerfsState.mountpoint) return;
  const mountpoint = openspecyWorkerfsState.mountpoint;
  openspecyWorkerfsState.mountpoint = null;
  try { await fs.unmount(mountpoint); } catch (_) {}
  try { await fs.rmdir(mountpoint); } catch (_) {}
}

function openspecyWorkerfsNames(files) {
  const names = files.map((file) => String(file.name || ""));
  const invalid = names.some((name) => !name || name === "." || name === ".." ||
    /[\\/\u0000-\u001f\u007f]/.test(name));
  if (invalid) throw new Error("Selected files must have safe base filenames.");
  const folded = names.map((name) => name.toLocaleLowerCase("en-US"));
  if (new Set(folded).size !== folded.length) {
    throw new Error("Selected filenames must be unique (ignoring case).");
  }
  return names;
}

window.addEventListener("message", async (event) => {
  const message = event.data || {};
  if (event.origin !== window.location.origin ||
      message.type !== "openspecy:workerfs" || !event.ports.length) return;
  const appFrame = document.querySelector("iframe.app-frame");
  if (!appFrame || event.source !== appFrame.contentWindow) return;

  const reply = event.ports[0];
  try {
    const handle = await webRProxyHandlePromise;
    const fs = handle.webRProxy.webR.FS;
    if (message.action === "capability") {
      reply.postMessage({ ok: true, version: 1 });
      return;
    }
    if (message.action === "unmount") {
      await openspecyWorkerfsCleanup(fs);
      reply.postMessage({ ok: true, unmounted: true });
      return;
    }
    if (message.action !== "mount" || !Array.isArray(message.files) ||
        !message.files.length ||
        message.files.some((file) => !(file instanceof File))) {
      throw new Error("A non-empty browser File selection is required.");
    }

    const files = message.files;
    const names = openspecyWorkerfsNames(files);
    const total = files.reduce((sum, file) => sum + Number(file.size || 0), 0);
    if (!Number.isFinite(total) || total > 10 * 1024 * 1024 * 1024) {
      throw new Error("The selected files exceed the 10 GiB total limit.");
    }
    await openspecyWorkerfsCleanup(fs);

    const token = crypto.randomUUID().replace(/-/g, "");
    const mountpoint = `/tmp/openspecy-upload-${token}`;
    await fs.mkdir(mountpoint);
    let offset = 0;
    const metadata = { files: [], remote_package_size: total };
    files.forEach((file, index) => {
      metadata.files.push({
        filename: `/${names[index]}`,
        start: offset,
        end: offset + file.size,
      });
      offset += file.size;
    });
    const blob = new Blob(files, { type: "application/octet-stream" });
    try {
      await fs.mount("WORKERFS", {
        packages: [{ blob, metadata }],
      }, mountpoint);
    } catch (error) {
      try { await fs.rmdir(mountpoint); } catch (_) {}
      throw error;
    }
    openspecyWorkerfsState.mountpoint = mountpoint;
    reply.postMessage({
      ok: true,
      mountId: token,
      files: files.map((file, index) => ({
        name: names[index],
        size: file.size,
        type: file.type || "application/octet-stream",
        datapath: `${mountpoint}/${names[index]}`,
      })),
    });
  } catch (error) {
    reply.postMessage({ ok: false, error: error.message || String(error) });
  } finally {
    reply.close();
  }
});
