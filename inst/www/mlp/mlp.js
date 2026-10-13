// MLP-Design (Option A: Stufenkarten)
// - setzt body.mlp, solange eine MLP-Seite angezeigt wird (CSS greift nur dann)
// - nummeriert die Antwortkarten auf 5-stufigen Item-Seiten
// - markiert die angetippte Karte kurz und blättert dann weiter
// - zeigt nach "Zurück" die frühere Antwort (Karte markiert, Textfeld gefüllt)
(function () {
  // Wird im Standalone über test_options() und in Batterien über die
  // htmlDependency der Seiten geladen -> nur einmal initialisieren.
  if (window.mlpDesignLoaded) return;
  window.mlpDesignLoaded = true;

  var SELECT_DELAY_MS = 300;

  // Frühere Antwort der Seite mit diesem Label, vom Server geschickt
  // (R/back.R, Nachricht "mlp_prev"): {label: ..., value: ...}
  var prev = null;

  // Schlüsselwort der Stufe fett setzen. psychTestR maskiert HTML in den
  // Antwort-Labels (das Dictionary liefert <strong>...</strong>), daher nur
  // genau diese Tags wieder zurückwandeln; "**...**" dient als Fallback.
  function boldMarkup(el) {
    var html = el.innerHTML;
    if (html.indexOf("strong") < 0 && html.indexOf("**") < 0) return;
    el.innerHTML = html
      .replace(/&lt;(\/?)strong&gt;/g, "<$1strong>")
      .replace(/\*\*(.+?)\*\*/g, "<strong>$1</strong>");
  }

  // Die Nachricht kann vor oder nach dem Seitenaufbau eintreffen, daher wird
  // über das Label geprüft, ob sie zur angezeigten Seite gehört.
  function applyPrev() {
    if (!prev) return;
    var page = document.querySelector('[data-mlp-label="' + prev.label + '"]');
    if (!page) return;
    var input = document.getElementById("text_input");
    if (input) {
      if (input.dataset.mlpPrev) return;
      input.dataset.mlpPrev = "1";
      input.value = prev.value;
      input.dispatchEvent(new Event("input", { bubbles: true }));
      input.dispatchEvent(new Event("change", { bubbles: true }));
    } else {
      var btn = document.getElementById(prev.value);
      if (btn && btn.closest("#response_ui")) btn.classList.add("mlp-prev");
    }
  }

  function decorate() {
    var page = document.querySelector(".mlp-page");
    document.body.classList.toggle("mlp", !!page || window.mlpStandalone === true);
    applyPrev();
    if (!page) return;

    // psychTestR legt die Antwort-Buttons neben (nicht in) den Prompt-Container
    var ui = document.getElementById("response_ui");
    if (!ui || ui.dataset.mlpDone) return;
    ui.dataset.mlpDone = "1";

    var numbered = page.classList.contains("mlp-numbered");
    var buttons = ui.querySelectorAll(".btn");
    buttons.forEach(function (btn, i) {
      var label = document.createElement("span");
      label.className = "mlp-label";
      while (btn.firstChild) label.appendChild(btn.firstChild);
      boldMarkup(label);
      if (numbered) {
        btn.classList.add("mlp-lv" + (i + 1));
        var num = document.createElement("span");
        num.className = "mlp-num";
        num.setAttribute("aria-hidden", "true");
        num.textContent = String(i + 1);
        btn.appendChild(num);
      }
      btn.appendChild(label);
    });
  }

  // Klick abfangen, bevor psychTestRs onclick="trigger_button(...)" feuert
  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest("body.mlp #response_ui .btn");
    if (!btn) return;
    e.preventDefault();
    e.stopPropagation();
    var ui = btn.closest("#response_ui");
    if (ui.classList.contains("mlp-locked")) return;
    ui.classList.add("mlp-locked");
    ui.querySelectorAll(".mlp-prev").forEach(function (b) { b.classList.remove("mlp-prev"); });
    btn.classList.add("mlp-selected");
    setTimeout(function () { trigger_button(btn.id); }, SELECT_DELAY_MS);
  }, true);

  // Shiny ist beim Laden dieses Skripts evtl. noch nicht bereit
  function registerHandler() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return setTimeout(registerHandler, 100);
    Shiny.addCustomMessageHandler("mlp_prev", function (msg) { prev = msg; applyPrev(); });
  }

  function start() {
    decorate();
    new MutationObserver(decorate).observe(document.body, { childList: true, subtree: true });
    registerHandler();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
