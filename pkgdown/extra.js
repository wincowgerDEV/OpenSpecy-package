(function () {
  "use strict";

  function initEmbed() {
    var shell = document.querySelector("[data-openspecy-embed]");
    if (!shell) return;

    var frame = document.getElementById("openspecy-app-frame");
    var status = document.getElementById("openspecy-app-status");
    var fullscreenButton = document.getElementById("openspecy-fullscreen");
    var readyMessage = "openspecy:ready";

    function markReady() {
      if (shell.classList.contains("is-ready")) return;
      shell.classList.add("is-ready");
      status.textContent = "Ready";
      fullscreenButton.disabled = false;
    }

    function fullscreenElement() {
      return document.fullscreenElement || document.webkitFullscreenElement;
    }

    function updateFullscreenState() {
      var active = fullscreenElement() === shell;
      shell.classList.toggle("is-fullscreen", active);
      fullscreenButton.textContent = active ? "Exit full screen" : "Full screen";
      fullscreenButton.setAttribute("aria-pressed", String(active));
    }

    window.addEventListener("message", function (event) {
      if (event.origin !== window.location.origin) return;
      if (!event.data || event.data.type !== readyMessage) return;
      markReady();
    });

    fullscreenButton.addEventListener("click", function () {
      if (fullscreenElement() === shell) {
        var exit = document.exitFullscreen || document.webkitExitFullscreen;
        if (exit) exit.call(document);
        return;
      }

      var request = shell.requestFullscreen || shell.webkitRequestFullscreen;
      if (request) {
        var result = request.call(shell);
        if (result && typeof result.catch === "function") {
          result.catch(function () {
            window.location.assign(frame.src);
          });
        }
        return;
      }

      window.location.assign(frame.src);
    });

    document.addEventListener("fullscreenchange", updateFullscreenState);
    document.addEventListener("webkitfullscreenchange", updateFullscreenState);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initEmbed, { once: true });
  } else {
    initEmbed();
  }
})();
