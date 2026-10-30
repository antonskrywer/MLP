# Instruktion beim ersten Ausfüllen einer Lehrkraft: zwei Seiten, als HTML statt
# als Bild, damit die Grafik übersetzt wird und sich der Breite anpasst.
# Aussehen: Klassen mlp-guide-* in inst/www/mlp/mlp.css.

mlp_guide_step <- function(mark, key, outer = FALSE) {
  shiny::div(
    class = "mlp-guide-step",
    shiny::span(class = paste("mlp-guide-mark", if (outer) "mlp-guide-mark-outer"),
                `aria-hidden` = "true", mark),
    shiny::div(
      shiny::div(class = "mlp-guide-title", psychTestR::i18n(paste0(key, "_TITLE"))),
      shiny::div(class = "mlp-guide-text", psychTestR::i18n(paste0(key, "_TEXT")))
    )
  )
}

# Zeile der Tabelle "Welche Perspektive wann": `asked` je Spalte TRUE/FALSE
mlp_guide_row <- function(key, asked) {
  shiny::tags$tr(
    shiny::tags$th(scope = "row", psychTestR::i18n(key)),
    lapply(asked, function(yes) {
      shiny::tags$td(
        class = if (yes) "mlp-guide-yes" else "mlp-guide-no",
        shiny::span(`aria-hidden` = "true", if (yes) "✓" else "–"),
        shiny::span(class = "sr-only",
                    psychTestR::i18n(if (yes) "TMLP_GUIDE_YES" else "TMLP_GUIDE_NO"))
      )
    })
  )
}

mlp_guide_flow <- function() {
  shiny::div(
    shiny::h4(psychTestR::i18n("TMLP_GUIDE_FLOW_TITLE")),
    shiny::p(psychTestR::i18n("TMLP_GUIDE_FLOW_INTRO")),
    shiny::div(
      class = "mlp-guide-card",
      mlp_guide_step("A", "TMLP_GUIDE_START", outer = TRUE),
      shiny::div(
        class = "mlp-guide-parts",
        shiny::div(class = "mlp-guide-partslabel", psychTestR::i18n("TMLP_GUIDE_PARTS")),
        mlp_guide_step("1", "TMLP_GUIDE_PART1"),
        mlp_guide_step("2", "TMLP_GUIDE_PART2"),
        mlp_guide_step("3", "TMLP_GUIDE_PART3")
      ),
      mlp_guide_step("B", "TMLP_GUIDE_END", outer = TRUE),
      shiny::div(class = "mlp-guide-text mlp-guide-hint", psychTestR::i18n("TMLP_GUIDE_PRACTICE"))
    ),
    shiny::p(psychTestR::i18n("TMLP_GUIDE_PERSPECTIVES")),
    shiny::div(
      class = "mlp-guide-card",
      shiny::tags$table(
        class = "mlp-guide-table",
        shiny::tags$tr(
          shiny::tags$td(),
          shiny::tags$th(scope = "col", psychTestR::i18n("TMLP_GUIDE_COL1")),
          shiny::tags$th(scope = "col", psychTestR::i18n("TMLP_GUIDE_COL2")),
          shiny::tags$th(scope = "col", psychTestR::i18n("TMLP_GUIDE_COL3"))
        ),
        mlp_guide_row("TMLP_GUIDE_ROW1", c(TRUE, TRUE, TRUE)),
        mlp_guide_row("TMLP_GUIDE_ROW2", c(FALSE, TRUE, TRUE)),
        mlp_guide_row("TMLP_GUIDE_ROW3", c(FALSE, FALSE, TRUE))
      )
    )
  )
}

mlp_guide_benefit <- function(key) {
  shiny::div(
    class = "mlp-guide-benefit",
    shiny::div(class = "mlp-guide-title", psychTestR::i18n(paste0(key, "_TITLE"))),
    shiny::div(class = "mlp-guide-text", psychTestR::i18n(paste0(key, "_TEXT")))
  )
}

mlp_guide_benefits <- function() {
  shiny::div(
    shiny::h4(psychTestR::i18n("TMLP_GUIDE_BENEFIT_TITLE")),
    shiny::div(
      class = "mlp-guide-card",
      mlp_guide_benefit("TMLP_GUIDE_BENEFIT1"),
      mlp_guide_benefit("TMLP_GUIDE_BENEFIT2"),
      mlp_guide_benefit("TMLP_GUIDE_BENEFIT3")
    ),
    shiny::p(class = "mlp-guide-note", psychTestR::i18n("TMLP_GUIDE_NOTE_TIME")),
    shiny::p(class = "mlp-guide-note", psychTestR::i18n("TMLP_GUIDE_NOTE_BACK"))
  )
}
