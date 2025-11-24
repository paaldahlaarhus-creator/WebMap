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
View(data_2023)
view(data_2022_2020)
