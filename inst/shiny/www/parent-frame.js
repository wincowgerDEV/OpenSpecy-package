(function () {
  "use strict";

  var notified = false;
  var busyTimer = null;
  var busyDelay = 650;

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

    shinyDocument.on("shiny:busy.openspecyBusy", function () {
      if (document.documentElement.classList.contains(
        "openspecy-busy-visible"
      )) return;

      window.clearTimeout(busyTimer);
      busyTimer = window.setTimeout(function () {
        document.documentElement.classList.add("openspecy-busy-visible");
      }, busyDelay);
    });

    shinyDocument.on("shiny:idle.openspecyBusy", function () {
      window.clearTimeout(busyTimer);
      busyTimer = null;
      document.documentElement.classList.remove("openspecy-busy-visible");
    });

    shinyDocument.one("shiny:idle.openspecyParent", notifyReady);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bindReadyEvent, { once: true });
  } else {
    bindReadyEvent();
  }
})();
