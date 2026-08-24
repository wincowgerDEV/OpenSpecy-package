(function () {
  "use strict";

  document.documentElement.setAttribute(
    "data-openspecy-parent-bridge", "loaded"
  );

  var notified = false;
  var readyProbeTimer = null;
  var busyTimer = null;
  var idleTimer = null;
  var elapsedTimer = null;
  var busyDelay = 650;
  var busyActionIdleGuard = 5000;
  var idleGrace = 200;
  var busyStartedAt = null;
  var busyActionStartedAt = null;
  var busyActionSawShinyBusy = false;
  var analysisPhaseActive = false;
  var shinyIsBusy = false;
  var mountedFileLimit = 10 * 1024 * 1024 * 1024;
  var busyState = {
    message: "Preparing analysis...",
    detail: "Open Specy is preparing the next result.",
    progress: 4
  };

  function isWasmMode() {
    var marker = document.querySelector('meta[name="openspecy-wasm-mode"]');
    return marker && marker.getAttribute("content") === "true";
  }

  function dispositionFilename(disposition) {
    var encoded = disposition.match(/filename\*=UTF-8''([^;]+)/i);
    if (encoded) {
      try {
        return decodeURIComponent(encoded[1]);
      } catch (_error) {
        return encoded[1];
      }
    }
    var plain = disposition.match(/filename="?([^";]+)"?/i);
    return plain ? plain[1] : "";
  }

  function safeFilename(filename) {
    return String(filename || "openspecy-download")
      .replace(/[\\/:*?"<>|]/g, "-")
      .replace(/^\.+|\.+$/g, "") || "openspecy-download";
  }

  // Shared immediate (zero-round-trip) busy decoration, so Run, Recalculate
  // Preview, and downloads all give the same instant visual response on
  // click instead of waiting on a server round trip -- reused below by
  // downloadInCurrentFrame() (wasm downloads) and bindInstantFeedback()
  // (Run/Recalculate Preview in both modes, plain downloads in non-wasm
  // mode).
  function markBusy(el) {
    if (!el) return;
    el.setAttribute("aria-busy", "true");
    el.classList.add("disabled");
  }

  function beginBusyAction(el, state) {
    if (!el || el.disabled || el.getAttribute("aria-disabled") === "true") {
      return false;
    }
    markBusy(el);
    analysisPhaseActive = true;
    shinyIsBusy = true;
    busyActionStartedAt = Date.now();
    busyActionSawShinyBusy = false;
    window.clearTimeout(busyTimer);
    busyTimer = null;
    window.clearTimeout(idleTimer);
    idleTimer = null;
    if (busyStartedAt === null) busyStartedAt = Date.now();
    busyState = {
      message: state.message,
      detail: state.detail,
      progress: state.progress
    };
    document.documentElement.setAttribute(
      "data-openspecy-busy-action", state.action
    );
    renderBusyState();
    scheduleBusy(true);
    return true;
  }

  function clearBusy(el) {
    if (!el) return;
    el.removeAttribute("aria-busy");
    el.classList.remove("disabled");
  }

  function recordUploadStatus(message) {
    if (message) {
      document.documentElement.setAttribute(
        "data-openspecy-upload-status", message
      );
    } else {
      document.documentElement.removeAttribute("data-openspecy-upload-status");
    }
  }

  function showUploadError(message, error) {
    recordUploadStatus(message);
    window.console.error(message, error || "");
    hideBusy();
    if (window.Shiny && window.Shiny.notifications &&
        window.Shiny.notifications.show) {
      window.Shiny.notifications.show({
        html: message,
        type: "error",
        duration: null
      });
    }
  }

  function showDownloadError(error) {
    var message = "Open Specy could not save this download. " + error.message;
    window.console.error(message, error);
    if (window.Shiny && window.Shiny.notifications &&
        window.Shiny.notifications.show) {
      window.Shiny.notifications.show({
        html: message,
        type: "error",
        duration: null
      });
    }
  }

  async function downloadInCurrentFrame(button) {
    if (button.dataset.openspecyDownloadActive === "true") return;
    var href = button.href;
    if (!href || href === window.location.href || /#$/.test(href)) {
      showDownloadError(new Error("The download is not ready yet."));
      return;
    }

    button.dataset.openspecyDownloadActive = "true";
    beginBusyAction(button, {
      action: "download",
      message: "Preparing download",
      detail: "Open Specy is generating and transferring the selected file.",
      progress: 8
    });
    try {
      await new Promise(function (resolve) {
        window.setTimeout(resolve, 250);
      });
      var request = function () { return window.fetch(href, {
        cache: "no-store",
        credentials: "same-origin"
      }); };
      var response = await request();
      if (response.status >= 500) {
        await new Promise(function (resolve) {
          window.setTimeout(resolve, 350);
        });
        response = await request();
      }
      if (!response.ok) {
        throw new Error("The server returned HTTP " + response.status + ".");
      }
      var blob = await response.blob();
      if (!blob.size) throw new Error("The generated file was empty.");

      var contentType = (response.headers.get("content-type") || "")
        .toLowerCase();
      var prefix = (await blob.slice(0, 128).text()).trimStart().toLowerCase();
      if (contentType.indexOf("text/html") === 0 ||
          contentType.indexOf("application/xhtml+xml") === 0 ||
          prefix.indexOf("<!doctype html") === 0 ||
          prefix.indexOf("<html") === 0) {
        throw new Error("The server returned an HTML page instead of the file.");
      }

      var filename = dispositionFilename(
        response.headers.get("content-disposition") || ""
      );
      var objectUrl = window.URL.createObjectURL(blob);
      var localLink = document.createElement("a");
      localLink.href = objectUrl;
      localLink.download = safeFilename(filename);
      localLink.style.display = "none";
      document.body.appendChild(localLink);
      localLink.click();
      localLink.remove();
      window.setTimeout(function () {
        window.URL.revokeObjectURL(objectUrl);
      }, 1000);
    } catch (error) {
      showDownloadError(error);
    } finally {
      delete button.dataset.openspecyDownloadActive;
      hideBusy();
    }
  }

  function bindWasmDownloads() {
    if (!isWasmMode()) return;
    document.addEventListener("click", function (event) {
      var target = event.target;
      var button = target && target.closest ? target.closest("#download_data") : null;
      if (!button) return;
      event.preventDefault();
      event.stopImmediatePropagation();
      void downloadInCurrentFrame(button);
    }, true);
  }

  var downloadFeedbackTimer = null;

  // Instant (pre-server-round-trip) busy feedback for Run, Recalculate
  // Preview, and plain (non-wasm) downloads, consistent with the fetch+blob
  // download path above. Run/Recalculate Preview are ordinary Shiny
  // actionButtons -- this only decorates them, it must never preventDefault
  // or stopPropagation, or Shiny's own click binding would stop receiving
  // the click. Cleared for Run/Recalculate by hideBusy() below, once the
  // real analysis-phase/idle cycle finishes; downloads get a fixed fallback
  // timeout since a same-tab file download has no reliable JS completion
  // event when using the native Shiny download binding (kept as-is here,
  // consistent with the wasm path only decorating, never replacing it).
  function bindInstantFeedback() {
    document.addEventListener("click", function (event) {
      var target = event.target;
      var closest = target && target.closest ? target.closest.bind(target) : null;
      if (!closest) return;

      var runButton = closest("#run_analysis");
      if (runButton) beginBusyAction(runButton, {
        action: "run",
        message: "Starting analysis",
        detail: "Open Specy is preparing the selected files and settings.",
        progress: 1
      });

      var recalcButton = closest("#recalculate_snr");
      if (recalcButton) beginBusyAction(recalcButton, {
        action: "recalculate",
        message: "Recalculating signal/noise preview",
        detail: "Open Specy is scanning the uploaded spectra.",
        progress: 8
      });

      if (isWasmMode()) return; // #download_data is fully handled by bindWasmDownloads() there
      var downloadButton = closest("#download_data");
      if (downloadButton) {
        beginBusyAction(downloadButton, {
          action: "download",
          message: "Preparing download",
          detail: "Open Specy is generating the selected file.",
          progress: 8
        });
        window.clearTimeout(downloadFeedbackTimer);
        downloadFeedbackTimer = window.setTimeout(function () {
          hideBusy();
        }, 4000);
      }
    }, true);
  }

  // Tab headers stay visible while the analysis card is collapsed. Expand the
  // owning card in capture phase so the same click both opens the card and
  // activates the requested tab, before Bootstrap/AdminLTE handle the link.
  function bindAnalysisSettings() {
    document.addEventListener("click", function (event) {
      var target = event.target;
      var tab = target && target.closest ?
        target.closest("#analysis_settings .nav-link") : null;
      if (!tab) return;
      var settingsBox = document.getElementById("analysis_settings_box");
      if (!settingsBox ||
          !settingsBox.classList.contains("collapsed-card")) return;
      var collapseControl = settingsBox.querySelector(
        ':scope > .card-header [data-card-widget="collapse"]'
      );
      if (collapseControl) collapseControl.click();
    }, true);
  }

  function workerfsRequest(action, files) {
    return new Promise(function (resolve, reject) {
      if (window.parent === window) {
        reject(new Error("The browser mount bridge is not available."));
        return;
      }
      var channel = new MessageChannel();
      var timer = window.setTimeout(function () {
        channel.port1.close();
        reject(new Error("The browser mount bridge did not respond."));
      }, 30000);
      channel.port1.onmessage = function (event) {
        window.clearTimeout(timer);
        channel.port1.close();
        if (event.data && event.data.ok) resolve(event.data);
        else reject(new Error(
          event.data && event.data.error ? event.data.error :
            "The browser mount failed."
        ));
      };
      window.parent.postMessage({
        type: "openspecy:workerfs",
        action: action,
        files: files || []
      }, window.location.origin, [channel.port2]);
    });
  }

  function mountedPayload(response) {
    return {
      transport: "workerfs",
      mount_id: response.mountId,
      name: response.files.map(function (file) { return file.name; }),
      size: response.files.map(function (file) { return file.size; }),
      type: response.files.map(function (file) { return file.type; }),
      datapath: response.files.map(function (file) { return file.datapath; })
    };
  }

  function bindWorkerfsUpload() {
    if (!isWasmMode()) return;
    var container = document.getElementById("openspecy_workerfs_upload");
    var input = document.getElementById("openspecy_workerfs_files");
    if (!container || !input) return;

    workerfsRequest("capability").then(function () {
      input.disabled = false;
    }).catch(function (error) {
      showUploadError(
        "Browser file mounting is unavailable. Reload the app or use local Open Specy.",
        error
      );
    });

    input.addEventListener("change", async function () {
      var files = Array.prototype.slice.call(input.files || []);
      if (!files.length) return;
      var runButton = document.getElementById("run_analysis");
      document.documentElement.setAttribute(
        "data-openspecy-materialized", "pending"
      );
      document.documentElement.removeAttribute(
        "data-openspecy-materialized-files"
      );
      if (runButton) {
        runButton.disabled = true;
      }
      var total = files.reduce(function (sum, file) {
        return sum + (Number(file.size) || 0);
      }, 0);
      if (total > mountedFileLimit) {
        input.value = "";
        showUploadError(
          "The selected files exceed the 10 GiB total upload ceiling. " +
          "Choose fewer or smaller files and try again."
        );
        return;
      }
      var fileCount = files.length + " file" + (files.length === 1 ? "" : "s");
      recordUploadStatus("Mounting " + fileCount + " in the browser.");
      beginBusyAction(input, {
        action: "upload",
        message: "Mounting selected files",
        detail: "Making " + fileCount +
          " available to Shinylive without copying the upload body.",
        progress: 3
      });
      try {
        var response = await workerfsRequest("mount", files);
        if (!window.Shiny || !window.Shiny.setInputValue) {
          throw new Error("The Shiny session is not ready.");
        }
        window.Shiny.setInputValue(
          "mounted_files", mountedPayload(response), { priority: "event" }
        );
        busyState.message = "Reading and materializing spectra";
        busyState.detail = fileCount +
          " mounted; reading the complete dataset into OpenSpecy memory.";
        busyState.progress = Math.max(busyState.progress, 8);
        recordUploadStatus(busyState.message + ": " + busyState.detail);
        renderBusyState();
        scheduleBusy(true);
      } catch (error) {
        input.value = "";
        showUploadError(
          "Browser mounting failed: " + error.message +
          " Reload the app or use local Open Specy.",
          error
        );
      }
    });

    window.addEventListener("pagehide", function () {
      workerfsRequest("unmount").catch(function () {});
    });
  }

  function bindUploadLimit() {
    if (isWasmMode()) return;
    var uploadLimit = 10 * 1024 * 1024 * 1024;
    document.addEventListener("change", function (event) {
      var input = event.target;
      if (!input || input.id !== "file" || !input.files) return;
      var total = Array.prototype.reduce.call(input.files, function (sum, file) {
        return sum + (Number(file.size) || 0);
      }, 0);
      if (total <= uploadLimit) return;
      event.preventDefault();
      event.stopImmediatePropagation();
      input.value = "";
      showUploadError(
        "The selected files exceed the 10 GiB total upload ceiling. " +
        "Choose fewer or smaller files and try again."
      );
    }, true);
  }

  function formatSeconds(seconds) {
    seconds = Math.max(0, Math.round(seconds));
    if (seconds < 60) return seconds + (seconds === 1 ? " second" : " seconds");
    var minutes = Math.floor(seconds / 60);
    var remainder = seconds % 60;
    return minutes + (minutes === 1 ? " minute" : " minutes") +
      (remainder ? " " + remainder + " seconds" : "");
  }

  function renderBusyState() {
    var overlay = document.getElementById("openspecy_busy_overlay");
    if (!overlay) return;
    var elapsed = busyStartedAt === null ? 0 : (Date.now() - busyStartedAt) / 1000;
    document.getElementById("openspecy_busy_message").textContent = busyState.message;
    document.getElementById("openspecy_busy_detail").textContent = busyState.detail;
    document.getElementById("openspecy_busy_elapsed").textContent =
      "Elapsed: " + formatSeconds(elapsed);
    var progress = Math.max(0, Math.min(100, Number(busyState.progress) || 0));
    var track = document.getElementById("openspecy_busy_progress");
    var fill = document.getElementById("openspecy_busy_progress_fill");
    if (track) {
      track.setAttribute("aria-valuenow", String(Math.round(progress)));
      track.setAttribute("aria-valuetext", Math.round(progress) + "% complete");
    }
    if (fill) fill.style.width = progress + "%";
  }

  function showBusy() {
    if (!analysisPhaseActive || !shinyIsBusy) return;
    var overlay = document.getElementById("openspecy_busy_overlay");
    if (!overlay) return;
    document.documentElement.classList.add("openspecy-busy-visible");
    overlay.setAttribute("aria-hidden", "false");
    renderBusyState();
    window.clearInterval(elapsedTimer);
    elapsedTimer = window.setInterval(renderBusyState, 1000);
  }

  function scheduleBusy(clientInitiated) {
    if (!analysisPhaseActive || !shinyIsBusy) return;
    if (document.documentElement.classList.contains("openspecy-busy-visible")) {
      renderBusyState();
      return;
    }
    if (busyTimer !== null) return;
    busyTimer = window.setTimeout(function () {
      busyTimer = null;
      if (clientInitiated && analysisPhaseActive) shinyIsBusy = true;
      showBusy();
    }, busyDelay);
  }

  function hideBusy() {
    var overlay = document.getElementById("openspecy_busy_overlay");
    window.clearTimeout(busyTimer);
    window.clearTimeout(idleTimer);
    window.clearTimeout(downloadFeedbackTimer);
    window.clearInterval(elapsedTimer);
    busyTimer = null;
    idleTimer = null;
    downloadFeedbackTimer = null;
    elapsedTimer = null;
    busyStartedAt = null;
    busyActionStartedAt = null;
    busyActionSawShinyBusy = false;
    analysisPhaseActive = false;
    shinyIsBusy = false;
    busyState = {
      message: "Preparing analysis...",
      detail: "Open Specy is preparing the next result.",
      progress: 4
    };
    document.documentElement.classList.remove("openspecy-busy-visible");
    document.documentElement.removeAttribute("data-openspecy-busy-action");
    if (overlay) {
      overlay.setAttribute("aria-hidden", "true");
      renderBusyState();
    }
    clearBusy(document.getElementById("run_analysis"));
    clearBusy(document.getElementById("recalculate_snr"));
    clearBusy(document.getElementById("download_data"));
    clearBusy(document.getElementById("openspecy_workerfs_files"));
  }

  function notifyReady() {
    if (notified || window.top === window) return;
    notified = true;
    window.clearTimeout(readyProbeTimer);
    readyProbeTimer = null;
    document.documentElement.setAttribute(
      "data-openspecy-parent-bridge", "ready-sent"
    );
    window.top.postMessage({ type: "openspecy:ready" }, window.location.origin);
  }

  function notifyIfConnectedAndIdle() {
    var socket = window.Shiny && window.Shiny.shinyapp &&
      window.Shiny.shinyapp.$socket;
    if (socket && socket.readyState === 1 &&
        !document.documentElement.classList.contains("shiny-busy")) {
      notifyReady();
    }
  }

  function probeReadyState() {
    notifyIfConnectedAndIdle();
    if (!notified) {
      readyProbeTimer = window.setTimeout(probeReadyState, 250);
    }
  }

  function bindReadyEvent() {
    var jquery = window.jQuery || window.$;
    if (!jquery) {
      window.setTimeout(bindReadyEvent, 50);
      return;
    }

    var shinyDocument = jquery(document);
    document.documentElement.setAttribute(
      "data-openspecy-parent-bindings", "ready"
    );

    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler("openspecy-analysis-phase", function (state) {
        analysisPhaseActive = true;
        window.clearTimeout(idleTimer);
        idleTimer = null;
        busyState.message = state.message || "Processing analysis...";
        busyState.detail = state.detail || "Open Specy is working on the current result.";
        if (busyStartedAt === null) {
          busyStartedAt = Date.now();
          busyState.progress = 4;
        }
        var nextProgress = Number(state.progress);
        if (Number.isFinite(nextProgress)) {
          busyState.progress = Math.max(
            busyState.progress,
            Math.max(0, Math.min(99, nextProgress))
          );
        }
        if (document.documentElement.getAttribute(
          "data-openspecy-busy-action"
        ) === "upload") {
          recordUploadStatus(busyState.message + ": " + busyState.detail);
        }
        renderBusyState();
        scheduleBusy();
      });

      window.Shiny.addCustomMessageHandler("openspecy-upload-materialized", function (state) {
        var files = Array.isArray(state.files) ? state.files :
          (state.files ? [state.files] : []);
        document.documentElement.setAttribute(
          "data-openspecy-materialized",
          state.transport || "unknown"
        );
        document.documentElement.setAttribute(
          "data-openspecy-materialized-files",
          JSON.stringify(files)
        );
        recordUploadStatus(
          files.length + " file" + (files.length === 1 ? "" : "s") +
          " fully materialized in OpenSpecy memory."
        );
      });

      window.Shiny.addCustomMessageHandler("openspecy-upload-status", function (state) {
        var message = state && state.message ? String(state.message) : "";
        if (!message) return;
        recordUploadStatus(message);
        if (state.type === "error") showUploadError(message);
      });

      window.Shiny.addCustomMessageHandler("openspecy-download-label", function (state) {
        var button = document.getElementById(state.id || "download_data");
        var label = state.label || "Download selected";
        if (!button) return;

        var icon = button.querySelector("i, svg");
        var labelNode = document.createElement("span");
        labelNode.className = "openspecy-download-label";
        labelNode.textContent = label;
        button.textContent = "";
        if (icon) button.appendChild(icon);
        button.appendChild(labelNode);
        button.setAttribute("aria-label", label);
        button.setAttribute("title", state.title || label);
      });

      window.Shiny.addCustomMessageHandler("openspecy-mounted-reset", function (_state) {
        var input = document.getElementById("openspecy_workerfs_files");
        if (input) input.value = "";
        workerfsRequest("unmount").catch(function () {});
      });
    }

    shinyDocument.on("shiny:busy.openspecyBusy", function () {
      shinyIsBusy = true;
      if (analysisPhaseActive) busyActionSawShinyBusy = true;
      window.clearTimeout(idleTimer);
      idleTimer = null;
      scheduleBusy();
    });

    shinyDocument.on("shiny:idle.openspecyBusy", function () {
      if (!analysisPhaseActive) {
        shinyIsBusy = false;
        return;
      }
      // Download generation does not own the WebSocket busy/idle cycle. A
      // pending idle event from an earlier configuration flush must not cancel
      // its overlay; native downloads use the bounded fallback timer and wasm
      // downloads hide it when their fetch/blob transfer actually completes.
      if (document.documentElement.getAttribute(
        "data-openspecy-busy-action"
      ) === "download") return;
      if (!busyActionSawShinyBusy && busyActionStartedAt !== null &&
          Date.now() - busyActionStartedAt < busyActionIdleGuard) return;
      shinyIsBusy = false;
      window.clearTimeout(busyTimer);
      busyTimer = null;
      window.clearTimeout(idleTimer);
      if (document.documentElement.classList.contains("openspecy-busy-visible")) {
        busyState.progress = 100;
        renderBusyState();
      }
      idleTimer = window.setTimeout(hideBusy, idleGrace);
    });

    shinyDocument.on("shiny:disconnected.openspecyBusy", hideBusy);

    shinyDocument.one("shiny:connected.openspecyParent", function () {
      window.setTimeout(notifyIfConnectedAndIdle, 0);
    });
    shinyDocument.one("shiny:idle.openspecyParent", notifyReady);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      bindWasmDownloads();
      bindInstantFeedback();
      bindAnalysisSettings();
      bindUploadLimit();
      bindWorkerfsUpload();
      probeReadyState();
      bindReadyEvent();
    }, { once: true });
  } else {
    bindWasmDownloads();
    bindInstantFeedback();
    bindAnalysisSettings();
    bindUploadLimit();
    bindWorkerfsUpload();
    probeReadyState();
    bindReadyEvent();
  }
})();
