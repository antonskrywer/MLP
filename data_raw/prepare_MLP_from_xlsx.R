# data_raw/prepare_MLP_from_xlsx.R
# Einmaliges Konvertierungsskript. Liest MLP_Itembank_3Ebenen.xlsx ein und erzeugt
#   data_raw/item_banks/MLP_item_bank.csv
#   data_raw/dicts/MLP_dict.csv
# Danach die Paketdaten neu erzeugen (Working Directory = Paket-Root):
#   source("data_raw/build_data.R")

library(readxl)
library(dplyr)
library(stringr)
library(readr)

xlsx_path <- "data_raw/MLP_Itembank_3Ebenen.xlsx"  # Pfad ggf. anpassen
raw_all <- read_excel(xlsx_path, sheet = "MLP_Itembank_3Ebenen")

# Items, die laut MLP_structure.txt tatsächlich in der finalen MLP vorkommen.
# MLP_0008 fehlt in der Itembank (structure.txt nennt "0005 bis 0011") -> übersprungen.
used_items <- c(
  "MLP_0003", "MLP_0004",                                                    # overall
  "MLP_0005", "MLP_0006", "MLP_0007", "MLP_0009", "MLP_0010", "MLP_0011",    # technique
  "MLP_0000", "MLP_0001", "MLP_0002",                                        # expression
  "MLP_0012", "MLP_0013", "MLP_0014", "MLP_0015", "MLP_0016", "MLP_0017",    # woodwind
  "MLP_0018", "MLP_0019", "MLP_0020", "MLP_0021",                            # guitar
  "MLP_0022", "MLP_0023", "MLP_0024", "MLP_0025",                            # brass
  "MLP_0026", "MLP_0027", "MLP_0028", "MLP_0029", "MLP_0030",                # strings
  "MLP_0031", "MLP_0032", "MLP_0033",                                        # piano
  "MLP_0035", "MLP_0036", "MLP_0037", "MLP_0038"                             # automatic_controlled
)

# Keys der vier neuen Intro-/Break-Seiten (kein L1/L2/L3, kein PROMPT/CHOICE-Suffix,
# daher separat behandelt statt über used_items/kind-Regex)
intro_keys <- c(
  "MLP_0039_INTRO_OVERALL",
  "MLP_0040_INTRO_TECHNIQUE",
  "MLP_0041_INTRO_EXPRESSION",
  "MLP_0042_INTRO_INSTRUMENT"
)

raw <- raw_all %>%
  mutate(
    item  = str_extract(key, "^MLP_\\d{4}"),
    level = str_extract(key, "L[123]"),
    kind  = str_extract(key, "(PROMPT|CHOICE\\d+)$")
  ) %>%
  filter(item %in% used_items)

item_order <- tibble(item = used_items, item_rank = seq_along(used_items))

# --- 1) Item-Bank-CSV ---------------------------------------------------
# main_id-Schema: TMLP_<item>_<level>  (z.B. TMLP_0003_L1)
# Zusätzliche Spalte "level" (optional, s. psyquest-Skill Abschnitt 2/6) für
# eigenes Postprocessing, falls L1/L2/L3 getrennt ausgewertet werden sollen.
item_bank <- raw %>%
  filter(kind == "PROMPT") %>%
  distinct(item, level, subscale, shortscale) %>%
  left_join(item_order, by = "item") %>%
  arrange(item_rank, match(level, c("L1", "L2", "L3"))) %>%
  mutate(main_id = paste0("TMLP_", str_remove(item, "MLP_"), "_", level)) %>%
  tidyr::crossing(language = c("de", "en")) %>%
  transmute(
    main_id, language,
    template   = "5-option multiple choice",
    score_func = "function(x) x+0",   # Annahme: CHOICE1 (schlechtester) -> CHOICE5 (bester) durchgehend aufsteigend
    subscales  = subscale,
    layout     = "vertical",
    level,
    shortscale = shortscale == "T"    # NEU: konvertiert "T"/"F" -> logisches TRUE/FALSE
  )
dir.create("data_raw/item_banks", recursive = TRUE, showWarnings = FALSE)
dir.create("data_raw/dicts", recursive = TRUE, showWarnings = FALSE)
write_delim(item_bank, "data_raw/item_banks/MLP_item_bank.csv", delim = ";", quote = "all")

# --- 2) Dictionary-CSV ---------------------------------------------------
item_dict <- raw %>%
  transmute(
    key = paste0("TMLP_", str_remove(item, "MLP_"), "_", level, "_", kind),
    de, en
  )

# Intro-/Break-Texte vor den vier Item-Blöcken (Overall/Technique/Expression/Instrument)
intro_dict <- raw_all %>%
  filter(key %in% intro_keys) %>%
  transmute(
    key = paste0("TMLP_", str_remove(key, "^MLP_"), "_PROMPT"),
    de, en
  )
# Custom-Texte für die nicht-Item-Seiten (Titel, Intro, Meta-/Sozio-Fragen)
custom_dict <- tibble::tribble(
  ~key,                             ~de,                                                                       ~en,
  "TMLP_0000_PROMPT",               "DELTA-M · Musical Learning Protocol",                                               "DELTA-M · Musical Learning Protocol",
  "TMLP_0001_PROMPT",               "Willkommen zum Musical Learning Protocol (MLP).",                         "Welcome to the Musical Learning Protocol (MLP).",
  "TMLP_FIRSTTIME_PROMPT",          "Beurteilen Sie diese/n Schüler/in zum ersten Mal mit dem MLP?",           "Are you assessing this pupil with the MLP for the first time?",
  "TMLP_FIRSTTIME_CHOICE1",         "Ja",                                                                       "Yes",
  "TMLP_FIRSTTIME_CHOICE2",         "Nein",                                                                     "No",
  "TMLP_INSTRUMENT_GROUP_PROMPT",   "Welche Instrumentengruppe lernt der/die Schüler/in?",                     "Which instrumental group does the pupil learn?",
  "TMLP_INSTRUMENT_GROUP_CHOICE1",  "Holzblasinstrument",                                                       "Woodwind",
  "TMLP_INSTRUMENT_GROUP_CHOICE2",  "Blechblasinstrument",                                                      "Brass",
  "TMLP_INSTRUMENT_GROUP_CHOICE3",  "Klavier",                                                                  "Piano",
  "TMLP_INSTRUMENT_GROUP_CHOICE4",  "Gitarre",                                                                  "Guitar",
  "TMLP_INSTRUMENT_GROUP_CHOICE5",  "Streichinstrument",                                                        "Strings",
  "TMLP_PRACTICE_PUPIL_PROMPT",     "Fragen Sie die Schülerin/den Schüler: Wie viele Minuten wurde in der letzten Woche im Durchschnitt pro Tag geübt?", "Ask the pupil: On average, how many minutes per day did he or she practice in the last week?",
  "TMLP_PRACTICE_TEACHER_PROMPT",   "Wie viele Minuten hat der/die Schüler/in Ihrer Einschätzung nach in der letzten Woche im Durchschnitt pro Tag geübt?", "In your estimation, how many minutes per day did the pupil practice on average in the last week?",
  "TMLP_AGE_PROMPT",                "Wie alt ist der/die Schüler/in?",                                         "How old is the pupil?",
  "TMLP_INSTRUMENT_PROMPT",         "Welches Instrument lernt der/die Schüler/in?",                            "What instrument is the pupil learning?",
  "TMLP_TEACHER_CODE_PROMPT",   "Bitte geben Sie Ihren Lehrkraft-Code ein.",   "Please enter your teacher code.",
  "TMLP_PRE_HOLIDAY_PROMPT",   "Ist dies die letzte Sitzung vor den Schulferien?",   "Is this the last session before the school holidays?",
  "TMLP_PRE_HOLIDAY_CHOICE1",   "Ja",   "Yes",
  "TMLP_PRE_HOLIDAY_CHOICE2",   "Nein",   "No",
  "TMLP_VDM_LEVEL_PROMPT",   "Auf welcher Stufe liegt die Literatur, die der/die Schüler/in derzeit spielt? Bitte an den Ausbildungsstufen der VdM-Lehrpläne orientieren.",   "Which level is the repertoire the pupil is currently playing? Please refer to the VdM curriculum levels.",
  "TMLP_VDM_LEVEL_CHOICE1",   "Unterstufe I – sehr leicht bis leicht",   "Level 1 (lower grade I) – very easy to easy",
  "TMLP_VDM_LEVEL_CHOICE2",   "Unterstufe II – leicht bis mittelschwierig",   "Level 2 (lower grade II) – easy to moderately difficult",
  "TMLP_VDM_LEVEL_CHOICE3",   "Mittelstufe I – mittelschwierig",   "Level 3 (intermediate grade I) – moderately difficult",
  "TMLP_VDM_LEVEL_CHOICE4",   "Mittelstufe II – schwierig",   "Level 4 (intermediate grade II) – difficult",
  "TMLP_VDM_LEVEL_CHOICE5",   "Oberstufe – sehr schwierig",   "Level 5 (upper grade) – very difficult",
  "TMLP_NEW_PIECE_PROMPT",   "Hat der/die Schüler/in seit der letzten Beurteilung ein neues Hauptstück begonnen?",   "Has the pupil started a new main piece since the last assessment?",
  "TMLP_NEW_PIECE_CHOICE1",   "Ja",   "Yes",
  "TMLP_NEW_PIECE_CHOICE2",   "Nein",   "No",
  "TMLP_COMMENT_PROMPT",   "Optional: Gibt es noch etwas, das Sie vermerken möchten? (Besondere Auffälligkeiten, Name des Stücks, ausgefallene Sitzungen etc.)",   "Optional: Is there anything else you would like to note? (Anything unusual, name of the piece, cancelled sessions etc.)",
  # Allgemeine Texte (ID-Seite, Schlussseite, Fußzeile); in psyquest kamen sie
  # aus general_dict.csv, hier in Sie-Form.
  "CONTINUE",          "Weiter",                                             "Continue",
  "BACK",              "← Zurück",                                           "← Back",
  "INVALID_ID",        "Dieser Schüler-Code ist nicht gültig. Bitte prüfen Sie die Eingabe.", "This pupil code is not valid. Please check your entry.",
  "ENTER_ID",          "Bitte geben Sie den Schüler-Code ein.",              "Please enter the pupil code.",
  "E.G.",              "z.B.",                                               "e.g.",
  "RESULTS_SAVED",     "Ihre Ergebnisse wurden gespeichert.",                "Your results have been saved.",
  "CLOSE_BROWSER",     "<br>Sie können das Browserfenster jetzt schließen.", "<br>You can close the browser window now.",
  "PROBLEMS_INFO_1",   "Probleme?",                                          "Problems?",
  "PROBLEMS_INFO_2",   "Dann schreiben Sie eine E-Mail an ",                 "Contact ",
  "PROBLEMS_INFO_3",   " mit dem Verweis auf diese Seite.",                  " with a link to this page."
)

dict <- bind_rows(item_dict, custom_dict, intro_dict) %>% filter(!is.na(de), !is.na(en))

# Schlüsselwörter der Antwortstufen als **fett** markieren (siehe MLP_bold_phrases.R);
# setzt auch die sprachlich geänderten Texte (z. B. 0025, Stufe 5) ein.
source("data_raw/MLP_bold_phrases.R", encoding = "UTF-8")
dict <- mlp_apply_bold(dict)
write_delim(dict, "data_raw/dicts/MLP_dict.csv", delim = ";", quote = "all")
