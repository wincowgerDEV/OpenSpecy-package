(function () {
  "use strict";

  var notified = false;
  var busyTimer = null;
  var idleTimer = null;
  var elapsedTimer = null;
  var busyDelay = 650;
  var idleGrace = 200;
  var busyStartedAt = null;
  var analysisPhaseActive = false;
  var shinyIsBusy = false;
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
    button.setAttribute("aria-busy", "true");
    button.classList.add("disabled");
    try {
      var response = await window.fetch(href, {
        cache: "no-store",
        credentials: "same-origin"
      });
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
      button.removeAttribute("aria-busy");
      button.classList.remove("disabled");
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

  function showUploadLimitPopup() {
    var localInstruction = isWasmMode()
      ? "Run the local OpenSpecy app, open Advanced, and use Local H5 / ENVI source."
      : "Open Advanced and use Local H5 / ENVI source to bypass the browser copy.";
    var message = "The browser upload limit is 2 GB total. " + localInstruction;
    if (window.Swal && typeof window.Swal.fire === "function") {
      window.Swal.fire({
        icon: "warning",
        title: "Upload is larger than 2 GB",
        text: message,
        confirmButtonText: "OK"
      });
    } else {
      window.alert(message);
    }
  }

  function bindUploadLimit() {
    var uploadLimit = 2 * 1024 * 1024 * 1024;
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
      showUploadLimitPopup();
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

  function scheduleBusy() {
    if (!analysisPhaseActive || !shinyIsBusy) return;
    if (document.documentElement.classList.contains("openspecy-busy-visible")) {
      renderBusyState();
      return;
    }
    if (busyTimer !== null) return;
    busyTimer = window.setTimeout(function () {
      busyTimer = null;
      showBusy();
    }, busyDelay);
  }

  function hideBusy() {
    var overlay = document.getElementById("openspecy_busy_overlay");
    window.clearTimeout(busyTimer);
    window.clearTimeout(idleTimer);
    window.clearInterval(elapsedTimer);
    busyTimer = null;
    idleTimer = null;
    elapsedTimer = null;
    busyStartedAt = null;
    analysisPhaseActive = false;
    busyState = {
      message: "Preparing analysis...",
      detail: "Open Specy is preparing the next result.",
      progress: 4
    };
    document.documentElement.classList.remove("openspecy-busy-visible");
    if (overlay) {
      overlay.setAttribute("aria-hidden", "true");
      renderBusyState();
    }
  }

  function notifyReady() {
    if (notified || window.top === window) return;
    notified = true;
    window.top.postMessage({ type: "openspecy:ready" }, window.location.origin);
  }

  function bindReadyEvent() {
    if (!window.jQuery) {
      window.setTimeout(bindReadyEvent, 50);
      return;
    }

    var shinyDocument = window.jQuery(document);

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
        renderBusyState();
        scheduleBusy();
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
    }

    shinyDocument.on(
      "click.openspecySettings",
      "#analysis_settings .nav-link",
      function () {
        var settingsBox = this.closest("#analysis_settings_box");
        if (!settingsBox || !settingsBox.classList.contains("collapsed-card")) return;
        var collapseControl = settingsBox.querySelector('[data-card-widget="collapse"]');
        if (collapseControl) collapseControl.click();
      }
    );

    shinyDocument.on("shiny:busy.openspecyBusy", function () {
      shinyIsBusy = true;
      window.clearTimeout(idleTimer);
      idleTimer = null;
      scheduleBusy();
    });

    shinyDocument.on("shiny:idle.openspecyBusy", function () {
      shinyIsBusy = false;
      if (!analysisPhaseActive) return;
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

    shinyDocument.one("shiny:idle.openspecyParent", notifyReady);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      bindWasmDownloads();
      bindUploadLimit();
      bindReadyEvent();
    }, { once: true });
  } else {
    bindWasmDownloads();
    bindUploadLimit();
    bindReadyEvent();
  }
})();
