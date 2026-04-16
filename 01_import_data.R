#=======================================
# Import and clean raw survey data
#=======================================

library(dplyr)
library(haven)
library(here)
library(stringr)

# Import SPSS file
data_raw <- read_sav(here("data", "raw", "survey_data.sav"))
message("Imported: ", nrow(data_raw), " respondents, ", ncol(data_raw), " variables")

# Convert labelled vectors to factors and include numeric socio-demographic vars.
data_raw <- data_raw |> 
  mutate(
    across(where(is.labelled), haven::as_factor))

# Remove empty columns and rename to block
data_raw <- data_raw |>
  dplyr::select(-C7, -C7r4oe, -C8, -C8r8oe, -status) |>
  rename(
    block = hidFinalC1C6,
    C7_comment = C7_newr4oe,
    C8_comment = C8_newr10oe)

# Save cleaned raw data
saveRDS(data_raw, here("data", "processed", "data_raw.rds"))