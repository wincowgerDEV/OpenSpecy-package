(function () {
  "use strict";

  function initNavigation() {
    var toggle = document.querySelector(".nav-toggle");
    var links = document.getElementById("site-nav-links");
    if (!toggle || !links) return;

    function setOpen(open) {
      links.dataset.open = String(open);
      toggle.setAttribute("aria-expanded", String(open));
    }

    toggle.addEventListener("click", function () {
      setOpen(links.dataset.open !== "true");
    });

    links.addEventListener("click", function (event) {
      if (event.target.closest("a")) setOpen(false);
    });

    document.addEventListener("click", function (event) {
      if (links.dataset.open !== "true") return;
      if (links.contains(event.target) || toggle.contains(event.target)) return;
      setOpen(false);
    });

    document.addEventListener("keydown", function (event) {
      if (event.key !== "Escape" || links.dataset.open !== "true") return;
      setOpen(false);
      toggle.focus();
    });
  }

  function initEmbed() {
    var shell = document.querySelector("[data-openspecy-embed]");
    if (!shell) return;

    var status = shell.querySelector("#openspecy-app-status");
    var expandButton = shell.querySelector("#openspecy-fullscreen");
    var frame = shell.querySelector("#openspecy-app-frame");
    var readyMessage = "openspecy:ready";

    if (!status || !expandButton || !frame) return;

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
      expandButton.disabled = false;
    }

    function setExpanded(active, returnFocus) {
      shell.classList.toggle("is-fullscreen", active);
      document.documentElement.classList.toggle(
        "openspecy-app-fullscreen-open",
        active
      );
      expandButton.textContent = active ? "Exit expanded view" : "Expand app";
      expandButton.setAttribute("aria-pressed", String(active));
      expandButton.setAttribute(
        "aria-label",
        active ? "Exit expanded OpenSpecy app view" : "Expand OpenSpecy app"
      );
      if (!active && returnFocus) expandButton.focus();
    }

    window.addEventListener("message", function (event) {
      if (event.origin !== window.location.origin) return;
      if (!isFrameOrDescendant(event.source, frame.contentWindow)) return;
      if (!event.data || event.data.type !== readyMessage) return;
      markReady();
    });

    expandButton.addEventListener("click", function () {
      setExpanded(!shell.classList.contains("is-fullscreen"), false);
    });

  }

  function initVideo() {
    var container = document.querySelector("[data-video-embed]");
    if (!container) return;
    var button = container.querySelector(".video-load");
    if (!button) return;

    button.addEventListener("click", function () {
      var source = container.dataset.videoSrc;
      if (!source) return;
      var frame = document.createElement("iframe");
      frame.src = source;
      frame.title = "OpenSpecy full application tutorial";
      frame.referrerPolicy = "strict-origin-when-cross-origin";
      frame.allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share";
      frame.allowFullscreen = true;
      container.replaceChildren(frame);
    }, { once: true });
  }

  function initLanding() {
    initNavigation();
    initEmbed();
    initVideo();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initLanding, { once: true });
  } else {
    initLanding();
  }
})();
