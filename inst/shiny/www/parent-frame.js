(function () {
  "use strict";

  var notified = false;
  var busyTimer = null;
  var idleTimer = null;
  var elapsedTimer = null;
  var busyDelay = 650;
  var idleGrace = 200;
  var busyStartedAt = null;
  var phaseStartedAt = null;
  var busyState = {
    message: "Preparing analysis...",
    detail: "Open Specy is preparing the next result.",
    eta: null
  };

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
    var phaseElapsed = phaseStartedAt === null ? 0 : (Date.now() - phaseStartedAt) / 1000;
    document.getElementById("openspecy_busy_message").textContent = busyState.message;
    document.getElementById("openspecy_busy_detail").textContent = busyState.detail;
    document.getElementById("openspecy_busy_elapsed").textContent =
      "Elapsed: " + formatSeconds(elapsed);

    var eta = document.getElementById("openspecy_busy_eta");
    if (!Array.isArray(busyState.eta) || busyState.eta.length !== 2) {
      eta.textContent = "Timing depends on spectrum and library size.";
    } else if (phaseElapsed > busyState.eta[1]) {
      eta.textContent = "Taking longer than usual; Open Specy is still working.";
    } else {
      var low = Math.max(0, busyState.eta[0] - phaseElapsed);
      var high = Math.max(low, busyState.eta[1] - phaseElapsed);
      eta.textContent = "Estimated remaining: " + formatSeconds(low) +
        " to " + formatSeconds(high) + ".";
    }
  }

  function showBusy() {
    var overlay = document.getElementById("openspecy_busy_overlay");
    if (!overlay) return;
    document.documentElement.classList.add("openspecy-busy-visible");
    overlay.setAttribute("aria-hidden", "false");
    renderBusyState();
    window.clearInterval(elapsedTimer);
    elapsedTimer = window.setInterval(renderBusyState, 1000);
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
    phaseStartedAt = null;
    document.documentElement.classList.remove("openspecy-busy-visible");
    if (overlay) overlay.setAttribute("aria-hidden", "true");
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
        busyState.message = state.message || "Processing analysis...";
        busyState.detail = state.detail || "Open Specy is working on the current result.";
        busyState.eta = Array.isArray(state.eta) ? state.eta : null;
        if (busyStartedAt === null) busyStartedAt = Date.now();
        phaseStartedAt = Date.now();
        renderBusyState();
      });
    }

    shinyDocument.on("shiny:busy.openspecyBusy", function () {
      window.clearTimeout(idleTimer);
      idleTimer = null;
      if (document.documentElement.classList.contains(
        "openspecy-busy-visible"
      )) return;

      if (busyStartedAt === null) busyStartedAt = Date.now();
      window.clearTimeout(busyTimer);
      busyTimer = window.setTimeout(showBusy, busyDelay);
    });

    shinyDocument.on("shiny:idle.openspecyBusy", function () {
      window.clearTimeout(busyTimer);
      busyTimer = null;
      window.clearTimeout(idleTimer);
      idleTimer = window.setTimeout(hideBusy, idleGrace);
    });

    shinyDocument.on("shiny:disconnected.openspecyBusy", hideBusy);

    shinyDocument.one("shiny:idle.openspecyParent", notifyReady);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bindReadyEvent, { once: true });
  } else {
    bindReadyEvent();
  }
})();
