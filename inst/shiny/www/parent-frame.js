(function () {
  "use strict";

  var notified = false;

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

    window.jQuery(document).one("shiny:idle.openspecyParent", notifyReady);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bindReadyEvent, { once: true });
  } else {
    bindReadyEvent();
  }
})();
