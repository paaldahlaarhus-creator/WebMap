
# Instaler dersom det er behov:
# install.packages("readxl")
# install.packages("dplyr")

library(readxl)
library(dplyr)

# Legger stien for å avlese excel filen, må endres på ved bruk.
filsti <- "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/Økologisk komdata.xlsx"   # <--- CHANGE THIS


# Hjelper funksjonen med å lese, endrer navn på siste kolonne og sorterer slik at verdier for samme kommune kommer etter hverandre
prepare_period <- function(path, sheet_name) {
  df <- read_excel(path, sheet = sheet_name)
  
  # Endrer navn på siste kollonne til "Tilstand_element"
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


# 3. Lager 3 nye datasett

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

# 2019–2015 (sheet: "Alle kommuner 2019 - 2015")
data_2019_2015 <- prepare_period(
  path = filsti,
  sheet_name = "Alle kommuner 2019 - 2015"
)
make_lowest_dataset <- function(df) {
  df %>%
    # Remove unwanted columns
    select(
      -any_of(c(
        "Navn",
        "Vannlokalitet Id",
        "Klassifiseringstype",
        "Status",
        "Antall påvirkningstyper"
      ))
    ) %>%
    # Gruperer data etter kriterier
    group_by(Kommune, Kvalitetselement, År) %>%
    # Beholder bare verdier til den i gruppa med den laveste nEQR verdien
    slice_min(order_by = nEQR, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Flytter Kommune forerst
    relocate(Kommune)
}

# Redefinerer de 3 nye datasettene for laveste verdier
data_2023_lowest       <- make_lowest_dataset(data_2023)
data_2022_2020_lowest  <- make_lowest_dataset(data_2022_2020)
data_2019_2015_lowest  <- make_lowest_dataset(data_2019_2015)

#Kan fjerne # på de under for å se de nye datasettene
#View(data_2023_lowest)
#View(data_2022_2020_lowest)
#View(data_2019_2015_lowest)

library(qs)

# Lagrer 2023 lowest
qs::qsave(
  data_2023_lowest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2023_lowest.qs"
)

# Lagrer 2022–2020 lowest
qs::qsave(
  data_2022_2020_lowest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2022_2020_lowest.qs"
)

# Lagrer 2019–2015 lowest
qs::qsave(
  data_2019_2015_lowest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2019_2015_lowest.qs"
)
