# data_raw/build_data.R
# Erzeugt data/mlp_dict.rda aus data_raw/dicts/MLP_dict.csv.
# Working Directory = Paket-Root. Die CSV selbst entsteht über
# data_raw/prepare_MLP_from_xlsx.R.

# Die CSV muss UTF-8-kodiert sein.
mlp_dict_raw <- suppressMessages(
  readr::read_csv2("data_raw/dicts/MLP_dict.csv", col_types = readr::cols())
)
stopifnot(nrow(readr::problems(mlp_dict_raw)) == 0,
          identical(names(mlp_dict_raw), c("key", "de", "en")),
          !anyDuplicated(mlp_dict_raw$key),
          !anyNA(mlp_dict_raw))

mlp_dict <- psychTestR::i18n_dict$new(as.data.frame(mlp_dict_raw))

dir.create("data", showWarnings = FALSE)
save(mlp_dict, file = "data/mlp_dict.rda", compress = "xz")
