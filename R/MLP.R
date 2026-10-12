# Item-IDs pro Block (hartkodiert; Abgleich mit data_raw/item_banks/MLP_item_bank.csv
# in tests/testthat/test-MLP_items.R).
mlp_item_lists <- function() {
  list(
    general    = c("0003", "0004"),
    technique  = c("0005", "0006", "0007", "0009", "0010", "0011"),
    expression = c("0000", "0001", "0002"),
    automatic  = c("0035", "0036", "0037", "0038"),
    instrument = list(
      Woodwind = c("0012", "0013", "0014", "0015", "0016", "0017"),
      Guitar   = c("0018", "0019", "0020", "0021"),
      Brass    = c("0022", "0023", "0024", "0025"),
      Strings  = c("0026", "0027", "0028", "0029", "0030"),
      Piano    = c("0031", "0032", "0033")
    )
  )
}

# Design-Assets (inst/www/mlp): Stufenkarten-CSS, Klick-/Nummerierungs-JS.
mlp_design_dependency <- function() {
  htmltools::htmlDependency(
    name       = "mlp-design",
    version    = "1.0.0",
    src        = system.file("www", "mlp", package = "MLP"),
    stylesheet = "mlp.css",
    script     = "mlp.js"
  )
}

# Hülle für jede MLP-Seite: Design-Assets, Fortschrittsbalken und Fragetext.
# `type` steuert das Aussehen: "item" (5 nummerierte Stufenkarten),
# "choice" (Karten ohne Nummer), "text" (Eingabefeld), "intro" (Infoseite).
mlp_prompt <- function(prompt, progress, type = c("item", "choice", "text", "intro")) {
  type <- match.arg(type)
  classes <- paste(c("mlp-page", paste0("mlp-", type),
                     if (type == "item") "mlp-numbered"), collapse = " ")
  shiny::div(
    class = classes,
    mlp_design_dependency(),
    shiny::div(class = "mlp-progress",
               shiny::div(style = sprintf("width: %d%%;", as.integer(round(progress))))),
    shiny::div(class = "mlp-prompt", prompt)
  )
}

#' MLP
#'
#' Modul-Funktion für das Musical Learning Protocol (MLP) zur Einbindung in
#' eine psychTestR-Timeline oder eine Batterie. Für eigenständige
#' Testsitzungen siehe \code{\link{MLP_standalone}()}.
#' Hand gebaut (nicht \code{main_test()}), da MLP mehrstufige Items
#' (individuums-/norm-/kriterienbezogen: L1/L2/L3), einen bedingten
#' Instrumenten-Block und eine bedingte Sozio-Demografie-Sektion enthält.
#'
#' Ablauf pro Sitzung: Lehrkraft-Code, Erstbeurteilung ja/nein, "letzte
#' Sitzung vor den Schulferien" ja/nein, VdM-Stufe der aktuellen Literatur,
#' Hauptstückwechsel ja/nein, dann die Item-Blöcke. Bei einer Erstbeurteilung
#' entfallen die Ferien- und die Hauptstückwechsel-Frage. Pro Item wird L3 immer,
#' L2 nur in der letzten Sitzung vor den Schulferien und L1 nur ab der
#' zweiten Beurteilung eines Kindes angezeigt. In der Kurzskala erscheint für
#' die Items des automatic-controlled-Blocks (0035 bis 0038) nur L3.
#' Am Ende steht ein optionales Freitextfeld für Anmerkungen (de{comment}).
#' Zwischenstände werden nach
#' jedem Block gespeichert. Die Schüler-ID kommt über
#' \code{MLP_standalone(with_id = TRUE)} (Feld \code{p_id}).
#'
#' Design: Antworten erscheinen als Karten (5-stufige Items nummeriert),
#' eine angetippte Karte wird kurz markiert, dann folgt die nächste Seite.
#' Oben zeigt ein Balken den ungefähren Fortschritt. CSS/JS liegen in
#' \code{inst/www/mlp} und greifen nur auf MLP-Seiten.
#'
#' @param label (Character scalar) Label für die Ergebnisse in der Ausgabedatei.
#' @param dict (i18n_dict) Dictionary für Internationalisierung.
#' @param shortscale (Logical) Wenn TRUE, werden nur Items der Kurzskala
#'   angezeigt (feste Liste \code{mlp_shortscale_items} im Funktionskörper,
#'   von Hand aus der Spalte "shortscale" in MLP_Itembank_3Ebenen.xlsx
#'   abgeleitet). Standard: FALSE (alle Items).
#' @param ... Weitere Argumente.
#' @export
MLP <- function(label = "MLP", dict = MLP::mlp_dict, shortscale = FALSE, ...) {

  # Kurzskalen-Filter: feste Liste der Item-IDs mit shortscale == "T" in
  # MLP_Itembank_3Ebenen.xlsx. Bei shortscale = TRUE werden nur diese Items
  # verwendet, bei FALSE (Standard) bleiben alle Items erhalten.
  # ACHTUNG: muss von Hand nachgezogen werden, falls sich die
  # shortscale-Spalte im Excel künftig ändert.
  mlp_shortscale_items <- c(
    "0004", "0011", "0002",               # general / technique / Expression
    "0012", "0013", "0014",               # woodwind
    "0018", "0019", "0021",               # guitar
    "0022", "0023", "0025",               # brass
    "0026", "0027", "0028",               # strings
    "0031", "0032", "0033",               # piano
    "0035", "0036", "0037", "0038"        # automatic-controlled
  )
  mlp_apply_shortscale <- function(item_ids) {
    if (!isTRUE(shortscale)) return(item_ids)
    item_ids[item_ids %in% mlp_shortscale_items]
  }

  # Kurzskala: im automatic-controlled-Block nur die kriterienbezogene Ebene
  # (L3), auch wenn L2/L1 in der Sitzung sonst gezeigt würden.
  automatic_levels <- if (isTRUE(shortscale)) "L3" else c("L3", "L2", "L1")

  item_lists       <- mlp_item_lists()
  general_items    <- mlp_apply_shortscale(item_lists$general)
  technique_items  <- mlp_apply_shortscale(item_lists$technique)
  expression_items <- mlp_apply_shortscale(item_lists$expression)
  automatic_items  <- mlp_apply_shortscale(item_lists$automatic)
  instrument_items <- lapply(item_lists$instrument, mlp_apply_shortscale)

  # Fortschritt: jede Seite bekommt ihre Position in der längstmöglichen
  # Abfolge (alle drei Ebenen, größter Instrumentenblock). Übersprungene
  # Seiten (L1/L2, Sozio-Demografie) lassen den Balken nur springen.
  n_instr_max <- max(lengths(instrument_items))
  pos_general    <- 10                                            # nach 6 Meta-, 2 Sozio-, 1 Intro-Seite
  pos_technique  <- pos_general + 3 * length(general_items) + 1
  pos_expression <- pos_technique + 3 * length(technique_items) + 1
  pos_group      <- pos_expression + 3 * length(expression_items)
  pos_instrument <- pos_group + 2                                 # nach Gruppenwahl + Intro
  pos_automatic  <- pos_instrument + 3 * n_instr_max
  pos_practice   <- pos_automatic + length(automatic_levels) * length(automatic_items)
  n_total        <- pos_practice + 2
  pct <- function(pos) 100 * pos / n_total

  # Speichert die Antwort umkodiert (z.B. "CHOICE3" -> 3L, "yes"/"no")
  mlp_save_recoded <- function(label, recode) {
    function(answer, state, ...) {
      psychTestR::save_result(place = state, label = label, value = recode(answer))
    }
  }

  # Ein "Item" -> bis zu 3 NAFC-Seiten (L1 individuumsbezogen, L2 normbezogen,
  # L3 kriterienbezogen), siehe MLP_structure.txt "item_page logic".
  # L3 wird immer gezeigt, L2 nur in der letzten Sitzung vor den
  # Schulferien (Local "pre_holiday"), L1 nur ab der zweiten Beurteilung
  # eines Kindes (Local "first_time" == FALSE).
  # `lvls` schränkt die Ebenen eines Items ein (Standard: alle drei).
  mlp_item_pages <- function(item_id, pos, lvls = c("L3", "L2", "L1")) {
    lapply(seq_along(lvls), function(j) {
      lvl <- lvls[j]
      prefix <- paste0("TMLP_", item_id, "_", lvl)
      page <- psychTestR::NAFC_page(
        label       = prefix,
        prompt      = mlp_prompt(psychTestR::i18n(paste0(prefix, "_PROMPT")), pct(pos + j - 1), "item"),
        choices     = paste0("CHOICE", 1:5),
        labels      = sapply(1:5, function(k) psychTestR::i18n(paste0(prefix, "_CHOICE", k))),
        save_answer = FALSE,
        arrange_vertically = TRUE,
        on_complete = mlp_save_recoded(prefix, level_number)
      )
      if (lvl == "L2") {
        psychTestR::conditional(
          test  = function(state, ...) isTRUE(psychTestR::get_local("pre_holiday", state)),
          logic = page
        )
      } else if (lvl == "L1") {
        psychTestR::conditional(
          test  = function(state, ...) !isTRUE(psychTestR::get_local("first_time", state)),
          logic = page
        )
      } else {
        page
      }
    })
  }
  # conditional() liefert eine Liste von Elementen, eine Seite ist selbst ein
  # Element -> auf eine flache Liste von Test-Elementen bringen.
  mlp_block_pages <- function(item_ids, pos, lvls = c("L3", "L2", "L1")) {
    flat <- list()
    for (k in seq_along(item_ids)) {
      for (el in mlp_item_pages(item_ids[k], pos + length(lvls) * (k - 1), lvls)) {
        if (is.null(attr(el, "class"))) flat <- c(flat, el)   # conditional(): einfache Liste
        else flat <- c(flat, list(el))                        # einzelne Seite
      }
    }
    flat
  }

  # Ja/Nein- bzw. Auswahlseite ohne Stufennummern
  # `recode`: Funktion CHOICEk -> gespeicherter Wert (statt "CHOICEk")
  mlp_choice_page <- function(label, key, n_choices, pos, on_complete = NULL, recode = NULL) {
    psychTestR::NAFC_page(
      label   = label,
      prompt  = mlp_prompt(psychTestR::i18n(paste0(key, "_PROMPT")), pct(pos), "choice"),
      choices = paste0("CHOICE", seq_len(n_choices)),
      labels  = lapply(seq_len(n_choices), function(k) psychTestR::i18n(paste0(key, "_CHOICE", k))),
      save_answer = is.null(recode),
      arrange_vertically = TRUE,
      on_complete = if (is.null(recode)) on_complete else {
        save <- mlp_save_recoded(label, recode)
        function(answer, state, ...) {
          save(answer, state, ...)
          if (!is.null(on_complete)) on_complete(answer, state, ...)
        }
      }
    )
  }
  yes_no <- function(answer) c(CHOICE1 = "yes", CHOICE2 = "no")[[answer]]
  level_number <- function(answer) as.integer(sub("CHOICE", "", answer))
  mlp_text_page <- function(label, key, pos, validate = NULL, one_line = TRUE) {
    psychTestR::text_input_page(
      label       = label,
      prompt      = mlp_prompt(psychTestR::i18n(key), pct(pos), "text"),
      one_line    = one_line,
      width       = "100%",
      button_text = psychTestR::i18n("CONTINUE"),
      validate    = validate
    )
  }
  mlp_intro_page <- function(key, pos) {
    psychTestR::one_button_page(
      body        = mlp_prompt(psychTestR::i18n(key), pct(pos), "intro"),
      button_text = psychTestR::i18n("CONTINUE")
    )
  }
  mlp_instrument_block <- function(group) {
    psychTestR::conditional(
      test  = function(state, ...) identical(psychTestR::get_local("instrument_group", state), group),
      logic = psychTestR::join(
        mlp_intro_page("TMLP_0042_INTRO_INSTRUMENT_PROMPT", pos_group + 1),
        mlp_block_pages(instrument_items[[group]], pos_instrument)
      )
    )
  }

  # Zwischenspeichern nach jedem Block: überschreibt die vorige (unvollständige)
  # Datei der Sitzung, die vollständige Speicherung am Ende ersetzt sie.
  mlp_save_partial <- function() psychTestR::elt_save_results_to_disk(complete = FALSE)

  num_validate <- function(answer, ...) {
    if (grepl("^[0-9]+$", trimws(answer))) TRUE
    else "Bitte eine Zahl eingeben. / Please enter a number."
  }

 psychTestR::new_timeline(
  psychTestR::join(
    psychTestR::begin_module(label = label),

    # 1) Welcome
    mlp_intro_page("TMLP_0001_PROMPT", 0),

    # 2) Lehrkraft-Code (Pflichtfeld). Der Schüler-Code kommt über p_id,
    #    siehe MLP_standalone(with_id = TRUE).
    mlp_text_page("teacher_code", "TMLP_TEACHER_CODE_PROMPT", 1,
                  validate = function(answer, ...) {
                    if (nchar(trimws(answer)) > 0) TRUE
                    else "Bitte einen Code eingeben. / Please enter a code."
                  }),

    # 3) Erstbeurteilung dieses Kindes ja/nein -> steuert L1 und den
    #    Sozio-Demografie-Block
    mlp_choice_page("firsttime", "TMLP_FIRSTTIME", 2, 2, recode = yes_no,
                    on_complete = function(answer, state, ...) {
                      first <- answer == "CHOICE1"
                      psychTestR::set_local("first_time", first, state)
                      # Erstbeurteilung: Ferien-Frage entfällt -> kein L2
                      psychTestR::set_local("pre_holiday", FALSE, state)
                    }),

    # 4) Letzte Sitzung vor den Schulferien ja/nein -> steuert L2
    #    (nur ab der zweiten Beurteilung)
    psychTestR::conditional(
      test  = function(state, ...) !isTRUE(psychTestR::get_local("first_time", state)),
      logic = mlp_choice_page("pre_holiday", "TMLP_PRE_HOLIDAY", 2, 3, recode = yes_no,
                              on_complete = function(answer, state, ...) {
                                psychTestR::set_local("pre_holiday", answer == "CHOICE1", state)
                              })
    ),

    # 5) VdM-Ausbildungsstufe der aktuell gespielten Literatur
    mlp_choice_page("vdm_level", "TMLP_VDM_LEVEL", 5, 4, recode = level_number),

    # 6) Hauptstückwechsel seit der letzten Beurteilung
    #    (nur ab der zweiten Beurteilung)
    psychTestR::conditional(
      test  = function(state, ...) !isTRUE(psychTestR::get_local("first_time", state)),
      logic = mlp_choice_page("new_piece", "TMLP_NEW_PIECE", 2, 5, recode = yes_no)
    ),

    # 7-8) Sozio-Demografie, nur bei Erstausfüllung
    psychTestR::conditional(
      test  = function(state, ...) isTRUE(psychTestR::get_local("first_time", state)),
      logic = psychTestR::join(
        mlp_text_page("pupil_age", "TMLP_AGE_PROMPT", 6, validate = num_validate),
        mlp_text_page("pupil_instrument", "TMLP_INSTRUMENT_PROMPT", 7)
      )
    ),
    mlp_save_partial(),

    # general
    mlp_intro_page("TMLP_0039_INTRO_OVERALL_PROMPT", pos_general - 1),
    mlp_block_pages(general_items, pos_general),
    mlp_save_partial(),

    # technique
    mlp_intro_page("TMLP_0040_INTRO_TECHNIQUE_PROMPT", pos_technique - 1),
    mlp_block_pages(technique_items, pos_technique),
    mlp_save_partial(),

    # expression
    mlp_intro_page("TMLP_0041_INTRO_EXPRESSION_PROMPT", pos_expression - 1),
    mlp_block_pages(expression_items, pos_expression),
    mlp_save_partial(),

    # Instrumentengruppe
    mlp_choice_page("instrument_group", "TMLP_INSTRUMENT_GROUP", 5, pos_group,
                    recode = function(answer) {
                      c(CHOICE1 = "woodwind", CHOICE2 = "brass", CHOICE3 = "piano",
                        CHOICE4 = "guitar", CHOICE5 = "strings")[[answer]]
                    },
                    on_complete = function(answer, state, ...) {
                      group <- c(CHOICE1 = "Woodwind", CHOICE2 = "Brass", CHOICE3 = "Piano",
                                 CHOICE4 = "Guitar", CHOICE5 = "Strings")[[answer]]
                      psychTestR::set_local("instrument_group", group, state)
                    }),

    # instrumentenspezifische Items (nur die gewählte Gruppe)
    mlp_instrument_block("Woodwind"),
    mlp_instrument_block("Brass"),
    mlp_instrument_block("Piano"),
    mlp_instrument_block("Guitar"),
    mlp_instrument_block("Strings"),
    mlp_save_partial(),

    # automatic-controlled
    mlp_block_pages(automatic_items, pos_automatic, automatic_levels),
    mlp_save_partial(),

    # Übungszeit, Minuten mit Validierung
    mlp_text_page("practice_pupil_min", "TMLP_PRACTICE_PUPIL_PROMPT", pos_practice, validate = num_validate),
    mlp_text_page("practice_teacher_min", "TMLP_PRACTICE_TEACHER_PROMPT", pos_practice + 1, validate = num_validate),

    # optionale Anmerkung (darf leer bleiben)
    mlp_text_page("comment", "TMLP_COMMENT_PROMPT", pos_practice + 2, one_line = FALSE),

    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module()
  ),
  dict = dict
 )
}
