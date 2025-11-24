#Instaler dersom det er behov
# install.packages("readxl")
# install.packages("dplyr")

library(readxl)
library(dplyr)

# Registrer hvor jeg henter filen fra
filsti <- "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/Økologisk komdata.xlsx"


# 2. Hjelper funksjonen med å lese, endrer navn på siste kolonne og sorterer slik at verdier for samme kommune kommer etter hverandre
prepare_period <- function(path, sheet_name) {
  df <- read_excel(path, sheet = sheet_name)
  
  # Rename the LAST column to "Tilstand_element"
  names(df)[ncol(df)] <- "Tilstand_element"
  
  # Sort so that:
  #  - primary order: Kvalitetselement (parameter)
  #  - then Kommune
  #  - then Navn (location name)
  #  - then År (year), so multiple measures for same place/municipality go underneath each other
  df <- df %>%
    arrange(`Kvalitetselement`, Kommune, Navn, `År`)
  
  return(df)
}


# 3. Create the three datasets

# 2023 (sheet: "Alle kommuner 2023")
data_2023 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2023"
)

# 2022–2020 (sheet: "Alle kommuner 2022-2020")
data_2022_2020 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2022-2020"
)

# 2019–2015 (sheet: "Alle kommuner 2019 - 2015")
data_2019_2015 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2019 - 2015"
)

classify_tilstand <- function(x) {
  case_when(
    x < 0.2              ~ "SVÆRT DÅRLIG",
    x < 0.4              ~ "DÅRLIG",
    x < 0.6              ~ "MODERAT",
    x < 0.8              ~ "GOD",
    !is.na(x)            ~ "SVÆRT GOD",
    TRUE                 ~ NA_character_
  )
}

# ---------------------
# 5. Helper: merge rows + clean columns
# ---------------------
make_merged_dataset <- function(df) {
  df %>%
    select(
      -any_of(c(
        "Navn",
        "Vannlokalitet Id",
        "Klassifiseringstype",
        "Status",
        "Antall påvirkningstyper"
      ))
    ) %>%
    group_by(Kommune, Kvalitetselement, År) %>%
    summarise(
      nEQR = mean(nEQR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Tilstand_element = classify_tilstand(nEQR)
    ) %>%
    relocate(Kommune)
}

# ---------------------
# 6. Create the three new merged datasets
# ---------------------
data_2023_merged       <- make_merged_dataset(data_2023)
data_2022_2020_merged  <- make_merged_dataset(data_2022_2020)
data_2019_2015_merged  <- make_merged_dataset(data_2019_2015)



View(data_2023_merged)
View(data_2022_2020_merged)
View(data_2019_2015_merged)


library(qs)

qs::qsave(
  data_2023_merged,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2023_merged.qs"
)

qs::qsave(
  data_2022_2020_merged,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2022_2020_merged.qs"
)

qs::qsave(
  data_2019_2015_merged,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2019_2015_merged.qs"
)

