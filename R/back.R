# Zurück-Navigation. psychTestR kennt kein "Zurück", daher:
# - Vor jeder Seite legt ein Code-Block den Stand (Position, Ergebnisse,
#   Locals) auf einen Verlaufsstapel (Global "mlp_history").
# - Der Zurück-Button stellt den Stand der zuvor gezeigten Seite wieder her
#   und springt dorthin; Antworten ab dieser Seite werden neu gegeben.
# - Jede gegebene Antwort wird zusätzlich gemerkt (Global "mlp_memory") und
#   beim erneuten Anzeigen der Seite an mlp.js geschickt, das die Karte
#   markiert bzw. das Textfeld vorausfüllt.

MLP_BACK <- "mlp_back"

mlp_is_back <- function(answer) inherits(answer, "mlp_back")

mlp_history_block <- function(label = NULL) {
  psychTestR::code_block(function(state, session, ...) {
    history <- psychTestR::get_global("mlp_history", state)
    if (is.null(history)) history <- list()
    history[[length(history) + 1L]] <- list(
      index         = state$passive$elt_index,
      results       = state$passive$results,
      locals        = state$passive$locals,
      parent_locals = state$passive$parent_locals
    )
    psychTestR::set_global("mlp_history", history, state)

    if (!is.null(label)) {
      previous <- psychTestR::get_global("mlp_memory", state)[[label]]
      if (!is.null(previous)) {
        session$sendCustomMessage("mlp_prev", list(label = label, value = previous))
      }
    }
  })
}

# Wird im on_complete der aktuellen Seite aufgerufen; psychTestR erhöht den
# Index danach um 1, daher das "- 1".
mlp_go_back <- function(state) {
  history <- psychTestR::get_global("mlp_history", state)
  i <- max(length(history) - 1L, 1L)
  target <- history[[i]]
  # der Code-Block der Zielseite legt seinen Eintrag gleich wieder an
  psychTestR::set_global("mlp_history", history[seq_len(i - 1L)], state)
  state$passive$results       <- target$results
  state$passive$locals        <- target$locals
  state$passive$parent_locals <- target$parent_locals
  psychTestR::skip_n_pages(state, target$index - state$passive$elt_index - 1L)
}

# Macht aus einer Seite die Elemente [Verlaufs-Block, Seite mit Zurück-Button].
# `back = FALSE` lässt den Button weg (erste Seite), die Antwort wird trotzdem
# gemerkt.
mlp_page <- function(page, back = TRUE) {
  label       <- page@label
  get_answer  <- page@get_answer
  validate    <- page@validate
  on_complete <- page@on_complete
  save_answer <- page@save_answer

  if (back) {
    page@ui <- shiny::div(
      page@ui,
      shiny::div(class = "mlp-back",
                 psychTestR::trigger_button(MLP_BACK, psychTestR::i18n("BACK")))
    )
  }
  page@get_answer <- function(input, ...) {
    if (identical(input$last_btn_pressed, MLP_BACK)) structure(list(), class = "mlp_back")
    else if (is.null(get_answer)) NULL
    else get_answer(input = input, ...)
  }
  # Zurück geht immer, auch bei leerem Pflichtfeld
  page@validate <- function(answer, ...) {
    if (mlp_is_back(answer) || is.null(validate)) TRUE
    else validate(answer = answer, ...)
  }
  page@save_answer <- FALSE
  page@on_complete <- function(answer, state, ...) {
    if (mlp_is_back(answer)) return(mlp_go_back(state))
    if (!is.null(label) && !is.null(answer)) {
      memory <- psychTestR::get_global("mlp_memory", state)
      if (is.null(memory)) memory <- list()
      memory[[label]] <- answer
      psychTestR::set_global("mlp_memory", memory, state)
    }
    if (save_answer) psychTestR::save_result(place = state, label = label, value = answer)
    if (!is.null(on_complete)) on_complete(answer = answer, state = state, ...)
  }
  list(mlp_history_block(label), page)
}
