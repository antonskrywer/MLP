# Fußzeile "Probleme? ..." pro Sprache; ohne E-Mail-Adresse der
# psychTestR-Standard (keine Fußzeile).
mlp_problems_info <- function(researcher_email, dict, languages) {
  if (is.null(researcher_email)) return("default")
  info <- lapply(languages, function(lang) {
    shiny::tags$span(
      dict$translate("PROBLEMS_INFO_1", lang),
      shiny::tags$br(),
      dict$translate("PROBLEMS_INFO_2", lang),
      shiny::tags$a(href = paste0("mailto:", researcher_email), researcher_email),
      dict$translate("PROBLEMS_INFO_3", lang))
  })
  names(info) <- languages
  info
}

# Eigene ID-Seite statt psychTestR::get_p_id(): get_p_id() setzt eine
# gespeicherte Sitzung mit derselben ID fort. Im Längsschnitt wird derselbe
# Schüler-Code bei jeder Beurteilung wieder eingegeben, man landete so auf der
# Schlussseite der letzten Sitzung, statt eine neue Beurteilung zu beginnen.
mlp_id_page <- function(validate_id = "auto") {
  if (identical(validate_id, "auto")) {
    validate_id <- function(answer, ...) {
      if (mlp_valid_id(answer)) TRUE else psychTestR::i18n("INVALID_ID")
    }
  }
  # mlp_page(): Der Code wird gemerkt und nach "Zurück" von der ersten
  # MLP-Seite wieder vorausgefüllt.
  mlp_page(psychTestR::text_input_page(
    label       = "p_id",
    prompt      = shiny::div(`data-mlp-label` = "p_id", psychTestR::i18n("ENTER_ID")),
    placeholder = paste(psychTestR::i18n("E.G."), "471113"),
    button_text = psychTestR::i18n("CONTINUE"),
    save_answer = FALSE,
    validate    = validate_id,
    on_complete = function(answer, state, ...) {
      set_p_id <- utils::getFromNamespace("p_id<-", "psychTestR")
      set_p_id(state, trimws(answer))
    }
  ), back = FALSE)
}

# Schüler-Code: nur Ziffern, die letzten beiden sind die Quersumme der
# Ziffern davor mit führender Null (4711 -> Quersumme 13 -> "471113").
mlp_valid_id <- function(id) {
  id <- trimws(id)
  if (!grepl("^[0-9]{3,100}$", id)) return(FALSE)
  digits <- as.integer(strsplit(id, "")[[1]])
  n <- length(digits)
  sprintf("%02d", sum(digits[seq_len(n - 2)])) == substr(id, n - 1, n)
}

#' MLP Standalone
#'
#' Startet eine eigenständige Testsitzung für das Musical Learning Protocol.
#' Für die Längsschnitt-Erhebung wird die ID-Abfrage (Schüler-Code, gespeichert
#' als \code{p_id}) standardmäßig aktiviert und die Kurzskala verwendet.
#' Die Vollversion mit allen Items gibt es über \code{shortscale = FALSE}.
#' Im Kopf erscheint das UHH-Logo, alle Seiten (auch ID- und Schlussseite)
#' nutzen das MLP-Design aus \code{inst/www/mlp}.
#'
#' Derselbe Schüler-Code beginnt jedes Mal eine neue Sitzung; eine frühere
#' Sitzung wird nicht fortgesetzt. Von der ersten MLP-Seite führt der
#' Zurück-Link zur Code-Eingabe.
#'
#' @param languages (Character vector) Verfügbare Sprachen (\code{"de"},
#'   \code{"en"}), Standard ist die erste.
#' @param with_id (Boolean) Schüler-Code abfragen (\code{p_id}). Standard: TRUE.
#' @param shortscale (Boolean) Nur Items der Kurzskala anzeigen. Standard: TRUE.
#' @param dict (i18n_dict) Dictionary für Internationalisierung.
#' @param admin_password (Character scalar) Passwort für das Admin-Panel.
#' @param researcher_email (Character scalar) Wenn nicht \code{NULL}, erscheint
#'   diese Adresse unten auf der Seite als Kontakt bei Problemen.
#' @param validate_id (Closure oder "auto") Funktion zur Prüfung des
#'   Schüler-Codes; "auto" verlangt Ziffern, deren letzte beide die
#'   Quersumme der Ziffern davor sind (z.B. 4711 -> \code{471113}).
#' @param ... Weitere Argumente an \code{psychTestR::test_options()}.
#' @export
MLP_standalone <- function(languages = c("de", "en"),
                           with_id = TRUE,
                           shortscale = TRUE,
                           dict = MLP::mlp_dict,
                           admin_password = "conifer",
                           researcher_email = NULL,
                           validate_id = "auto",
                           ...) {
  stopifnot(is.character(languages), all(languages %in% c("de", "en")))
  elts <- psychTestR::join(
    if (with_id) psychTestR::new_timeline(mlp_id_page(validate_id), dict = dict),
    MLP(dict = dict, shortscale = shortscale, back_from_first = with_id),
    psychTestR::new_timeline(psychTestR::final_page(
      shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER")
      )
    ), dict = dict)
  )
  title <- vapply(languages, function(lang) dict$translate("TMLP_0000_PROMPT", lang),
                  character(1))
  www_mlp <- system.file("www", "mlp", package = "MLP")
  shiny::addResourcePath("www_mlp", www_mlp)
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      problems_info = mlp_problems_info(researcher_email, dict, languages),
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      # Keine Sitzungs-Fortsetzung über die URL, siehe mlp_id_page()
      enable_resume_session = FALSE,
      logo = "www_mlp/uhh-logo.svg",
      logo_width = "110px",
      logo_height = "auto",
      display = psychTestR::display_options(css = file.path(www_mlp, "mlp.css")),
      additional_scripts = file.path(www_mlp, c("mlp-standalone.js", "mlp.js")),
      ...
    )
  )
}
