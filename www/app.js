(function () {
  function setActions(enabled) {
    ["download_report", "print_report", "go_report"].forEach(function (id) {
      var el = document.getElementById(id);
      if (!el) return;
      el.setAttribute("aria-disabled", enabled ? "false" : "true");
      el.classList.toggle("disabled", !enabled);
      if (el.tagName === "BUTTON") el.disabled = !enabled;
    });
  }
  document.addEventListener("click", function (event) {
    var el = event.target.closest("[aria-disabled='true']");
    if (el) { event.preventDefault(); event.stopImmediatePropagation(); }
  }, true);
  $(document).on("shiny:connected", function () { setActions(false); });
  Shiny.addCustomMessageHandler("assessment-actions", function (message) { setActions(message.enabled === true); });
})();
