# Wiedererkennung über frühere Sitzungen: psychTestR liefert nur die laufende
# Sitzung, daher werden die Ergebnisdateien in opt$results_dir gelesen. Auch
# unvollständige Dateien zählen: Eine Lehrkraft, die abgebrochen hat, kennt die
# Instruktion bereits, und die Instrumentengruppe stimmt auch dort.

# Gespeicherter Wert der Instrumentengruppe je Antwort der Auswahlseite
mlp_groups <- c(CHOICE1 = "woodwind", CHOICE2 = "brass", CHOICE3 = "piano",
                CHOICE4 = "guitar", CHOICE5 = "strings")

# Gespeicherter Wert -> Local "instrument_group" ("piano" -> "Piano").
# Alte Ergebnisdateien enthalten noch "CHOICEk".
mlp_group_local <- function(group) {
  if (group %in% names(mlp_groups)) group <- mlp_groups[[group]]
  if (!group %in% mlp_groups) return(NULL)
  paste0(toupper(substr(group, 1, 1)), substring(group, 2))
}

# Codes werden wie im Data Monitor ohne Rücksicht auf Groß-/Kleinschreibung
# verglichen.
mlp_norm_code <- function(code) toupper(trimws(as.character(code)))

mlp_scalar <- function(x) {
  if (length(x) == 0 || is.na(x[[1]])) NA_character_ else as.character(x[[1]])
}

# Eine Ergebnisdatei -> Lehrkraft, Kind, Gruppe, Startzeit. Fremde oder
# beschädigte Dateien: NULL.
mlp_read_session <- function(file) {
  res <- tryCatch(as.list(readRDS(file)), error = function(e) NULL)
  mlp <- res[["MLP"]]
  session <- res[["session"]]
  if (is.null(mlp) || is.null(session)) return(NULL)
  list(teacher = mlp_norm_code(mlp_scalar(mlp$teacher_code)),
       pupil   = mlp_norm_code(mlp_scalar(session$p_id)),
       group   = mlp_scalar(mlp$instrument_group),
       time    = as.numeric(session$time_started))
}

# Sucht frühere Sitzungen. `known_teacher`: Es gibt eine Sitzung mit diesem
# Lehrkraft-Code. `instrument_group`: Gruppe der letzten Sitzung derselben
# Kombination aus Lehrkraft- und Schüler-Code, sonst NULL.
mlp_lookup <- function(dir, teacher_code, p_id = NULL) {
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  sessions <- Filter(Negate(is.null), lapply(files, mlp_read_session))
  teacher <- mlp_norm_code(teacher_code)
  own <- Filter(function(s) identical(s$teacher, teacher), sessions)

  group <- NULL
  if (length(p_id) == 1 && !is.na(p_id)) {
    pupil <- mlp_norm_code(p_id)
    same <- Filter(function(s) identical(s$pupil, pupil) && !is.na(s$group), own)
    if (length(same) > 0) {
      times <- vapply(same, function(s) s$time, numeric(1))
      group <- mlp_group_local(same[[which.max(times)]]$group)
    }
  }
  list(known_teacher = length(own) > 0, instrument_group = group)
}

# Setzt die Locals "new_teacher" und "known_group". Schlägt das Lesen fehl,
# gilt die Lehrkraft als neu und die Gruppe wird wie gewohnt erfragt.
mlp_lookup_block <- function() {
  psychTestR::code_block(function(state, opt, ...) {
    p_id <- psychTestR::p_id(state)
    found <- tryCatch(
      mlp_lookup(opt$results_dir, psychTestR::get_local("teacher_code", state), p_id),
      error = function(e) list(known_teacher = FALSE, instrument_group = NULL)
    )
    psychTestR::set_local("new_teacher", !found$known_teacher, state)
    psychTestR::set_local("known_group", found$instrument_group, state)
  })
}

# Bei einer Folgebeurteilung mit bekannter Gruppe entfällt die Auswahlseite;
# die Gruppe wird trotzdem gespeichert, der Data Monitor liest sie pro Sitzung.
mlp_known_group_block <- function() {
  psychTestR::code_block(function(state, ...) {
    group <- psychTestR::get_local("known_group", state)
    if (is.null(group) || isTRUE(psychTestR::get_local("first_time", state))) return(NULL)
    psychTestR::set_local("instrument_group", group, state)
    psychTestR::save_result(place = state, label = "instrument_group", value = tolower(group))
  })
}
