context("MLP lookup")

# Ergebnisdatei wie von psychTestR gespeichert: Abschnitte "MLP" und "session"
write_session <- function(dir, name, teacher, pupil, group = NULL, time = "2026-10-01 10:00:00") {
  mlp <- list(teacher_code = teacher)
  if (!is.null(group)) mlp$instrument_group <- group
  session <- list(p_id = pupil, time_started = as.POSIXct(time, tz = "UTC"))
  saveRDS(list(MLP = mlp, session = session), file.path(dir, paste0(name, ".rds")))
}

results_dir <- function() {
  dir <- tempfile("mlp_results")
  dir.create(dir)
  dir
}

test_that("neue Lehrkraft: leerer oder fehlender Ordner", {
  expect_false(MLP:::mlp_lookup(results_dir(), "T1", "471113")$known_teacher)
  expect_false(MLP:::mlp_lookup(file.path(tempdir(), "gibt-es-nicht"), "T1", "471113")$known_teacher)
})

test_that("Lehrkraft wird unabhängig von Schreibweise erkannt", {
  dir <- results_dir()
  write_session(dir, "a", "t1", "471113", "piano")
  expect_true(MLP:::mlp_lookup(dir, " T1 ", "100203")$known_teacher)
  expect_false(MLP:::mlp_lookup(dir, "T2", "471113")$known_teacher)
})

test_that("Gruppe zählt nur für die Kombination beider Codes", {
  dir <- results_dir()
  write_session(dir, "a", "T1", "471113", "piano")
  expect_identical(MLP:::mlp_lookup(dir, "T1", "471113")$instrument_group, "Piano")
  expect_null(MLP:::mlp_lookup(dir, "T2", "471113")$instrument_group)
  expect_null(MLP:::mlp_lookup(dir, "T1", "100203")$instrument_group)
})

test_that("die letzte Sitzung mit Gruppe entscheidet", {
  dir <- results_dir()
  write_session(dir, "a", "T1", "471113", "piano", "2026-10-01 10:00:00")
  write_session(dir, "b", "T1", "471113", "strings", "2026-10-08 10:00:00")
  write_session(dir, "c", "T1", "471113", NULL, "2026-10-15 10:00:00")   # abgebrochen
  expect_identical(MLP:::mlp_lookup(dir, "T1", "471113")$instrument_group, "Strings")
})

test_that("alte CHOICEk-Werte und fremde Dateien stören nicht", {
  dir <- results_dir()
  write_session(dir, "a", "T1", "471113", "CHOICE3")
  saveRDS(list(other = 1), file.path(dir, "fremd.rds"))
  writeLines("kaputt", file.path(dir, "kaputt.rds"))
  expect_identical(MLP:::mlp_lookup(dir, "T1", "471113")$instrument_group, "Piano")
})

test_that("ohne Schüler-Code gibt es keine Gruppe", {
  dir <- results_dir()
  write_session(dir, "a", "T1", NA_character_, "piano")
  expect_true(MLP:::mlp_lookup(dir, "T1", NA_character_)$known_teacher)
  expect_null(MLP:::mlp_lookup(dir, "T1", NA_character_)$instrument_group)
})

test_that("Timeline lässt sich bauen, auch ohne Schüler-Code", {
  expect_true(psychTestR::is.timeline(MLP::MLP(with_id = TRUE, shortscale = TRUE)))
  expect_true(psychTestR::is.timeline(MLP::MLP(with_id = FALSE)))
})
