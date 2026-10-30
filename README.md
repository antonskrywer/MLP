# MLP

Musical Learning Protocol (DELTA-M) als eigenständiges R-Paket für
[psychTestR](https://github.com/pmcharrison/psychTestR). Aus dem Paket
`psyquest` herausgelöst, damit es unabhängig davon installiert werden kann.

## Installation

```r
remotes::install_github("antonskrywer/MLP")
```

## Verwendung

`app.R` auf dem Server:

```r
library(MLP)
MLP_standalone(languages = "de", shortscale = TRUE)
```

Als Baustein in einer eigenen psychTestR-Timeline: `MLP::MLP()`.

Die App liest beim Start einer Sitzung die Ergebnisdateien in `output/results`.
Eine neue Lehrkraft sieht einmalig die Instruktion. Bei Folgebeurteilungen
kommt die Instrumentengruppe aus der letzten Sitzung derselben Kombination aus
Lehrkraft- und Schüler-Code. Dieser Ordner muss deshalb zwischen Updates
erhalten bleiben.

## Texte ändern

Die Texte stehen in `data_raw/MLP_Itembank_3Ebenen.xlsx` (Items) und in
`data_raw/prepare_MLP_from_xlsx.R` (übrige Seiten). Nach Änderungen im
Paket-Root:

```r
source("data_raw/prepare_MLP_from_xlsx.R", encoding = "UTF-8")  # -> data_raw/dicts/MLP_dict.csv
source("data_raw/build_data.R", encoding = "UTF-8")             # -> data/mlp_dict.rda
devtools::document()
devtools::test()
```
