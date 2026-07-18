(function () {
  "use strict";

  function initEmbed() {
    var shell = document.querySelector("[data-openspecy-embed]");
    if (!shell) return;

    var status = document.getElementById("openspecy-app-status");
    var fullscreenButton = document.getElementById("openspecy-fullscreen");
    var readyMessage = "openspecy:ready";

    function markReady() {
      if (shell.classList.contains("is-ready")) return;
      shell.classList.add("is-ready");
      status.textContent = "Ready";
      fullscreenButton.disabled = false;
    }

    function updateFullscreenState(active) {
      shell.classList.toggle("is-fullscreen", active);
      document.documentElement.classList.toggle(
        "openspecy-app-fullscreen-open",
        active
      );
      fullscreenButton.textContent = active ? "Exit full screen" : "Full screen";
      fullscreenButton.setAttribute("aria-pressed", String(active));
    }

    window.addEventListener("message", function (event) {
      if (event.origin !== window.location.origin) return;
      if (!event.data || event.data.type !== readyMessage) return;
      markReady();
    });

    fullscreenButton.addEventListener("click", function () {
      updateFullscreenState(!shell.classList.contains("is-fullscreen"));
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initEmbed, { once: true });
  } else {
    initEmbed();
  }
})();
