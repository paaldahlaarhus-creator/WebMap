# Instaler dersom det er behov
# install.packages("readxl")
# install.packages("dplyr")

library(readxl)
library(dplyr)

# Legger stien for å avlese excel filen, må endres på ved bruk.
filsti <- "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/Økologisk komdata.xlsx"   # <--- CHANGE THIS


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


# Lager 3 nye datasett

# 2023
data_2023 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2023"
)

# 2022–2020 
data_2022_2020 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2022-2020"
)

# 2019–2015 
data_2019_2015 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2019 - 2015"
)

make_highest_dataset <- function(df) {
  df %>%
    # Fjerner kolonner som ikke er nødvendige
    select(
      -any_of(c(
        "Navn",
        "Vannlokalitet Id",
        "Klassifiseringstype",
        "Status",
        "Antall påvirkningstyper"
      ))
    ) %>%
    # Definisjonen for variabler som kvalifiserer seg til å være i samme gruppe
    group_by(Kommune, Kvalitetselement, År) %>%
    # beholder bar den høyeste nEQR
    slice_max(order_by = nEQR, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Flytter kommune variablen frem
    relocate(Kommune)
}

# Redefinerer de 3 nye datasettene for høyeste verdi
data_2023_highest       <- make_highest_dataset(data_2023)
data_2022_2020_highest  <- make_highest_dataset(data_2022_2020)
data_2019_2015_highest  <- make_highest_dataset(data_2019_2015)

#Kan fjerne # på de under for å se de nye datasettene
#View(data_2023_highest)
#view(data_2022_2020_highest)
#View(data_2019_2015_highest)

library(qs)

# Lagrer 2023 highest
qs::qsave(
  data_2023_highest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2023_highest.qs"
)

# Lagrer 2022–2020 highest
qs::qsave(
  data_2022_2020_highest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2022_2020_highest.qs"
)

# Lagrer 2019–2015 highest
qs::qsave(
  data_2019_2015_highest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2019_2015_highest.qs"
)


