#Instaler dersom det er behov
# install.packages("readxl")
# install.packages("dplyr")

library(readxl)
library(dplyr)

# Legger stien for å avlese excel filen, må endres på ved bruk
filsti <- "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/Økologisk komdata.xlsx"


# Hjelper funksjonen med å lese, endrer navn på siste kolonne og sorterer slik at verdier for samme kommune kommer etter hverandre
prepare_period <- function(path, sheet_name) {
  df <- read_excel(path, sheet = sheet_name)
  
  # Endrer navn på siste kolonne til "Tilstand_element"
  names(df)[ncol(df)] <- "Tilstand_element"
  
  # arrange brukes for å sortere sånn at:
  #  - viktigste variablen Kvalitetselement kommer først
  #  - deretter Kommune
  #  - deretter Navn, navn på lokasjon
  #  - deretter År
  df <- df %>%
    arrange(`Kvalitetselement`, Kommune, Navn, `År`)
  
  return(df)
}


# 3. Danner 3 nye dataset

# 2023 (sheet/ark: "Alle kommuner 2023")
data_2023 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2023"
)

# 2022–2020 (sheet/ark: "Alle kommuner 2022-2020")
data_2022_2020 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2022-2020"
)

# 2019–2015 (sheet/ark: "Alle kommuner 2019 - 2015")
data_2019_2015 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2019 - 2015"
)
#Lager en funskjon som definerer de ulike nEQR verdiene som ulike tilstander,
#nødvendig å spesifisere for å kunne kategorisere etter å slått sammen og tatt gjennomsnittet av nEQR av alle 
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


# Hjelper med å slå sammen rader og rense kolonnene for ikke relevante variabler

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
    group_by(Kommune, Kvalitetselement, År) %>% #Kriteriene for at man skal slå sammen variablen 
    summarise(
      nEQR = mean(nEQR, na.rm = TRUE), #Beregner gjennomsnittet av de ulike nEQR verdiene av observasjoner som er like,
      .groups = "drop"
    ) %>%
    mutate(
      Tilstand_element = classify_tilstand(nEQR)
    ) %>%
    relocate(Kommune)
}


# Lager 3 nye sammenslåtte variabler datasett

data_2023_merged       <- make_merged_dataset(data_2023)
data_2022_2020_merged  <- make_merged_dataset(data_2022_2020)
data_2019_2015_merged  <- make_merged_dataset(data_2019_2015)



View(data_2023_merged)
View(data_2022_2020_merged)
View(data_2019_2015_merged)


library(qs)
#Lagrer de tre ulike datasettene i riktig lokasjon
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

