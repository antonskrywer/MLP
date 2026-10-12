context("MLP items")

# Die hartkodierten Item-Listen in R/MLP.R müssen zur Item-Bank passen
# (Fehler in der Vergangenheit: 0002 fehlte in expression_items).
test_that("mlp_item_lists() stimmt mit MLP_item_bank.csv überein", {
  path <- testthat::test_path("../../data_raw/item_banks/MLP_item_bank.csv")
  skip_if_not(file.exists(path), "MLP_item_bank.csv nicht vorhanden")

  bank <- read.csv2(path, stringsAsFactors = FALSE)
  bank <- bank[bank$language == "de" & bank$level == "L3", ]
  bank$id <- sub("^TMLP_([0-9]+)_L3$", "\\1", bank$main_id)

  lists <- MLP:::mlp_item_lists()
  by_sub <- function(sub) sort(bank$id[bank$subscales == sub])

  expect_equal(sort(lists$general),    by_sub("overall"))
  expect_equal(sort(lists$technique),  by_sub("technique"))
  expect_equal(sort(lists$expression), by_sub("expression"))
  expect_equal(sort(lists$automatic),  by_sub("automatic_controlled"))
  expect_equal(sort(lists$instrument$Woodwind), by_sub("woodwind"))
  expect_equal(sort(lists$instrument$Guitar),   by_sub("guitar"))
  expect_equal(sort(lists$instrument$Brass),    by_sub("brass"))
  expect_equal(sort(lists$instrument$Strings),  by_sub("strings"))
  expect_equal(sort(lists$instrument$Piano),    by_sub("piano"))
})

test_that("Alle neuen MLP-Dictionary-Keys existieren in DE und EN", {
  keys <- c("TMLP_TEACHER_CODE_PROMPT", "TMLP_FIRSTTIME_PROMPT",
            "TMLP_PRE_HOLIDAY_PROMPT", paste0("TMLP_PRE_HOLIDAY_CHOICE", 1:2),
            "TMLP_VDM_LEVEL_PROMPT", paste0("TMLP_VDM_LEVEL_CHOICE", 1:5),
            "TMLP_NEW_PIECE_PROMPT", paste0("TMLP_NEW_PIECE_CHOICE", 1:2),
            # allgemeine Texte für ID-Seite, Schlussseite und Fußzeile
            "CONTINUE", "ENTER_ID", "E.G.", "RESULTS_SAVED", "CLOSE_BROWSER",
            paste0("PROBLEMS_INFO_", 1:3))
  for (k in keys) {
    for (lang in c("de", "en")) {
      txt <- MLP::mlp_dict$translate(k, lang)
      expect_true(nchar(txt) > 0, info = paste(k, lang))
    }
  }
})
