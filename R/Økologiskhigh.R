# Install packages once if needed:
# install.packages("readxl")
# install.packages("dplyr")

library(readxl)
library(dplyr)

# 1. Set path to your file (you'll update this string in the next step)
filsti <- "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/Økologisk komdata.xlsx"   # <--- CHANGE THIS


# 2. Helper function to read, rename last column, and sort
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

make_highest_dataset <- function(df) {
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
    # Group by desired variables
    group_by(Kommune, Kvalitetselement, År) %>%
    # Keep only the row with the highest nEQR
    slice_max(order_by = nEQR, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Move Kommune to the front
    relocate(Kommune)
}

# Create the three highest-value datasets
data_2023_highest       <- make_highest_dataset(data_2023)
data_2022_2020_highest  <- make_highest_dataset(data_2022_2020)
data_2019_2015_highest  <- make_highest_dataset(data_2019_2015)

#Kan fjerne # på de under for å se de nye datasettene
#View(data_2023_highest)
#view(data_2022_2020_highest)
#View(data_2019_2015_highest)

library(qs)

# Lagre 2023 highest
qs::qsave(
  data_2023_highest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2023_highest.qs"
)

# Lagre 2022–2020 highest
qs::qsave(
  data_2022_2020_highest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2022_2020_highest.qs"
)

# Lagre 2019–2015 highest
qs::qsave(
  data_2019_2015_highest,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/eco_2019_2015_highest.qs"
)


