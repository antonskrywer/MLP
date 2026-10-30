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
  "TMLP_FIRSTTIME_PROMPT",         "Beurteilen Sie diese/n Schüler/in zum ersten Mal mit dem MLP?",           "Are you assessing this pupil with the MLP for the first time?",
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
  "TMLP_TEACHER_CODE_PROMPT",   "Willkommen zum Musical Learning Protocol (MLP). Bitte geben Sie Ihren Lehrkraft-Code ein.",   "Welcome to the Musical Learning Protocol (MLP). Please enter your teacher code.",
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
  # Instruktion beim ersten Ausfüllen einer Lehrkraft (R/guide.R)
  "TMLP_GUIDE_FLOW_TITLE",      "So läuft das Protokoll ab",   "How the protocol works",
  "TMLP_GUIDE_FLOW_INTRO",      "Füllen Sie das Protokoll bitte nach jeder Unterrichtseinheit aus, also einmal pro Woche für jede/n Schüler/in. Dabei gehen Sie immer dieselben Schritte durch.",   "Please complete the protocol after every lesson, that is, once a week for each pupil. You always go through the same steps.",
  "TMLP_GUIDE_START_TITLE",     "Zum Einstieg",   "To begin",
  "TMLP_GUIDE_START_TEXT",      "Kurze Fragen zur aktuellen Situation",   "Short questions about the current situation",
  "TMLP_GUIDE_PARTS",           "Ihre Beurteilung in drei Teilen",   "Your assessment in three parts",
  "TMLP_GUIDE_PART1_TITLE",     "Allgemeine Qualität",   "Overall quality",
  "TMLP_GUIDE_PART1_TEXT",      "Gesamteindruck, Technik und Ausdruck",   "Overall impression, technique and expression",
  "TMLP_GUIDE_PART2_TITLE",     "Instrumentenspezifische Qualität",   "Instrument-specific quality",
  "TMLP_GUIDE_PART2_TEXT",      "Passend zum Instrument",   "Matched to the instrument",
  "TMLP_GUIDE_PART3_TITLE",     "Spielsicherheit",   "Playing security",
  "TMLP_GUIDE_PART3_TEXT",      "Tempo, Auswendigspiel und Konzentration",   "Tempo, playing from memory and concentration",
  "TMLP_GUIDE_END_TITLE",       "Zum Abschluss",   "To finish",
  "TMLP_GUIDE_END_TEXT",        "Übezeit und optionale Anmerkung",   "Practice time and an optional note",
  "TMLP_GUIDE_PRACTICE",        "Die Übezeit erfragen Sie am besten direkt im Unterricht bei Ihrer Schülerin oder Ihrem Schüler.",   "It is best to ask your pupil about practice time during the lesson.",
  "TMLP_GUIDE_PERSPECTIVES",    "Welche Perspektiven gefragt sind, steuert das Protokoll selbst:",   "The protocol decides by itself which perspectives are asked for:",
  "TMLP_GUIDE_COL1",            "Erste Beurteilung",   "First assessment",
  "TMLP_GUIDE_COL2",            "Weitere",   "Later ones",
  "TMLP_GUIDE_COL3",            "Vor den Ferien",   "Before holidays",
  "TMLP_GUIDE_ROW1",            "Konkrete Beschreibungen",   "Concrete descriptions",
  "TMLP_GUIDE_ROW2",            "Vergleich zur letzten Unterrichtseinheit",   "Comparison with the last lesson",
  "TMLP_GUIDE_ROW3",            "Vergleich mit Schüler/innen auf vergleichbarem Niveau",   "Comparison with pupils at a similar level",
  "TMLP_GUIDE_YES",             "ja",   "yes",
  "TMLP_GUIDE_NO",              "nein",   "no",
  "TMLP_GUIDE_BENEFIT_TITLE",   "Was Ihnen das Protokoll bringt",   "What the protocol offers you",
  "TMLP_GUIDE_BENEFIT1_TITLE",  "Lernverlauf sichtbar machen",   "Make learning progress visible",
  "TMLP_GUIDE_BENEFIT1_TEXT",   "Sie dokumentieren Ihren Unterricht systematisch und sehen die Entwicklung Ihrer Schüler/innen mit Ihrem Lehrkraft-Code unter <a href=\"https://testing.musikpsychologie.de/jms_monitor\" target=\"_blank\" rel=\"noopener\">testing.musikpsychologie.de/jms_monitor</a>.",   "You document your teaching systematically and can follow your pupils' progress with your teacher code at <a href=\"https://testing.musikpsychologie.de/jms_monitor\" target=\"_blank\" rel=\"noopener\">testing.musikpsychologie.de/jms_monitor</a>.",
  "TMLP_GUIDE_BENEFIT2_TITLE",  "Genauer hinschauen",   "Take a closer look",
  "TMLP_GUIDE_BENEFIT2_TEXT",   "Die Fragen sind aus der Forschung abgeleitet und benennen, worauf es beim Instrumentalspiel ankommt.",   "The questions are derived from research and name what matters in instrumental playing.",
  "TMLP_GUIDE_BENEFIT3_TITLE",  "Musikalisches Lernen verstehen",   "Understand musical learning",
  "TMLP_GUIDE_BENEFIT3_TEXT",   "Ihre Angaben fließen in die DELTA-M-Studie ein.",   "Your answers contribute to the DELTA-M study.",
  "TMLP_GUIDE_NOTE_TIME",       "Beim ersten Mal brauchen Sie etwas mehr Zeit, um die Beschreibungen zu lesen. Mit der Zeit kennen Sie die Struktur und kommen schneller voran.",   "The first time, you will need a little longer to read the descriptions. Over time you will know the structure and get through it faster.",
  "TMLP_GUIDE_NOTE_BACK",       "Über „Zurück“ können Sie Eingaben jederzeit korrigieren. Bitte tragen Sie keine Namen ein, auch nicht im Anmerkungsfeld.",   "You can correct entries at any time with “Back”. Please do not enter any names, not even in the notes field.",
  "TMLP_GUIDE_GO",              "Protokoll starten",   "Start protocol",
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
