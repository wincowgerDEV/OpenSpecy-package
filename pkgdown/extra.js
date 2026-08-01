(function () {
  "use strict";

  function initHomepageTheme() {
    if (!document.querySelector(".openspecy-welcome")) return;
    document.body.classList.add("openspecy-welcome-page");
  }

  function initEmbed() {
    var shell = document.querySelector("[data-openspecy-embed]");
    if (!shell) return;

    var status = shell.querySelector("#openspecy-app-status");
    var fullscreenButton = shell.querySelector("#openspecy-fullscreen");
    var frame = shell.querySelector("#openspecy-app-frame");
    var readyMessage = "openspecy:ready";

    function isFrameOrDescendant(source, candidate) {
      if (!source || !candidate) return false;
      if (source === candidate) return true;
      try {
        for (var index = 0; index < candidate.frames.length; index += 1) {
          if (isFrameOrDescendant(source, candidate.frames[index])) return true;
        }
      } catch (_error) {
        return false;
      }
      return false;
    }

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
      fullscreenButton.textContent = active ? "Exit expanded view" : "Expand app";
      fullscreenButton.setAttribute("aria-pressed", String(active));
      fullscreenButton.setAttribute(
        "aria-label",
        active ? "Exit expanded OpenSpecy app view" : "Expand OpenSpecy app"
      );
    }

    window.addEventListener("message", function (event) {
      if (event.origin !== window.location.origin) return;
      if (frame && !isFrameOrDescendant(event.source, frame.contentWindow)) return;
      if (!event.data || event.data.type !== readyMessage) return;
      markReady();
    });

    fullscreenButton.addEventListener("click", function () {
      updateFullscreenState(!shell.classList.contains("is-fullscreen"));
    });

  }

  function initHomepage() {
    initHomepageTheme();
    initEmbed();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initHomepage, { once: true });
  } else {
    initHomepage();
  }
})();
