#================================================================
# Build long-format choice dataset and merge experimental design
# Input: data/processed/data_raw.rds
# Output: data/processed/choices_full.rds
#================================================================

library(dplyr)
library(tidyr)
library(here)
library(stringr)
library(haven)

data_raw <- readRDS(here("data", "processed", "data_raw.rds"))

#---------------------------------------------
# 1. Extract chosen alternative from C1-C6 
#---------------------------------------------
choice_vars <- paste0("C", 1:6)

choices_long <- data_raw |>
  dplyr::select(
    uuid, gender, age_group, kommune2024, fylke2024,
    block, all_of(choice_vars)
  ) |>
  
  # Wide -> long
  pivot_longer(
    cols = all_of(choice_vars),
    names_to = "choice_card",
    values_to = "chosen_string"
  ) |>
  
  # Extract the SitX number to identify which alternative was selected
  mutate(
    chosen_alt = as.integer(str_extract(chosen_string, "(?<=Sit)\\d+"))
  )

# Stop if any chosen alternative could not be parsed from the SPSS filename string
n_failed <- sum(is.na(choices_long$chosen_alt))
if (n_failed > 0) stop(n_failed, " rows where chosen_alt could not be parsed
                       - check chosen_string values")
  
choices_long <- choices_long |>
  # Create 3 rows per choice card
  mutate(alt = list(1:3)) |>
  unnest(alt) |> 
  
  # Convert block from "Block1" -> 1 (SPSS labels use capital B)
  mutate(
    block = as.integer(str_remove(block, regex("block", ignore_case = TRUE)))
  ) |>
  # Create chosen indicator
  mutate(chosen = if_else(alt == chosen_alt, 1L, 0L))

#---------------------------------------------------------------
# 2. Create the experimental design table 
#---------------------------------------------------------------

#---------------------------------------------------------------
# Attribute design: which region(s) improves in each alternative
#---------------------------------------------------------------
   
attribute_design <- tibble::tribble(
  ~block, ~choice_card, ~alt, ~indre, ~midtre, ~ytre_vest, ~ytre_ost,
  
  # -------- Block 1 --------
  1, "C1", 1, 0, 1, 0, 1,
  1, "C1", 2, 1, 0, 1, 0,
  
  1, "C2", 1, 1, 0, 1, 0,
  1, "C2", 2, 0, 1, 0, 1,
  
  1, "C3", 1, 0, 0, 1, 1,
  1, "C3", 2, 1, 1, 0, 0,
  
  1, "C4", 1, 0, 1, 1, 1,
  1, "C4", 2, 1, 0, 0, 0,
  
  1, "C5", 1, 1, 1, 0, 1,
  1, "C5", 2, 0, 0, 0, 1,
  
  1, "C6", 1, 0, 1, 0, 0,
  1, "C6", 2, 1, 0, 1, 1,
  
  # -------- Block 2 --------
  2, "C1", 1, 1, 1, 0, 0,
  2, "C1", 2, 1, 0, 1, 1,
  
  2, "C2", 1, 1, 0, 0, 1,
  2, "C2", 2, 1, 1, 1, 0,
  
  2, "C3", 1, 1, 1, 0, 1,
  2, "C3", 2, 0, 1, 0, 0,
  
  2, "C4", 1, 1, 0, 0, 0,
  2, "C4", 2, 1, 1, 1, 1,
  
  2, "C5", 1, 1, 1, 0, 0,
  2, "C5", 2, 0, 1, 1, 1,
  
  2, "C6", 1, 1, 0, 1, 0,
  2, "C6", 2, 0, 1, 0, 1,
  
  # -------- Block 3 --------
  3, "C1", 1, 1, 0, 0, 1,
  3, "C1", 2, 0, 1, 1, 0,
  
  3, "C2", 1, 0, 0, 1, 1,
  3, "C2", 2, 1, 1, 0, 0,
  
  3, "C3", 1, 0, 1, 1, 0,
  3, "C3", 2, 1, 0, 0, 1,
  
  3, "C4", 1, 0, 1, 0, 1,
  3, "C4", 2, 1, 0, 1, 0,
  
  3, "C5", 1, 0, 1, 1, 1,
  3, "C5", 2, 1, 0, 0, 1,
  
  3, "C6", 1, 1, 1, 0, 0,
  3, "C6", 2, 0, 0, 0, 1
)

#-----------------------------------------------
# Cost design
#-----------------------------------------------
cost_design <- tribble(
  ~block, ~choice_card, ~alt, ~cost,
  # -----Block 1-----
  1, "C1", 1, 500, 1, "C1", 2, 4000,
  1, "C2", 1, 3500, 1, "C2", 2, 500,
  1, "C3", 1, 1000, 1, "C3", 2, 4000, 
  1, "C4", 1, 4000, 1, "C4", 2, 2500,
  1, "C5", 1, 4000, 1, "C5", 2, 500,
  1, "C6", 1, 1000, 1, "C6", 2, 1500,
  
  # ------Block 2--------
  2, "C1", 1, 500, 2, "C1", 2, 4000,
  2, "C2", 1, 500, 2, "C2", 2, 3500,
  2, "C3", 1, 2500, 2, "C3", 2, 500,
  2, "C4", 1, 1000, 2, "C4", 2, 2000,
  2, "C5", 1, 1500, 2, "C5", 2, 2000,
  2, "C6", 1, 2500, 2, "C6", 2, 3000,
  
  # -------Block 3--------
  3, "C1", 1, 3500, 3, "C1", 2, 500,
  3, "C2", 1, 500, 3, "C2", 2, 4000,
  3, "C3", 1, 500, 3, "C3", 2, 3500,
  3, "C4", 1, 3500, 3, "C4", 2, 500,
  3, "C5", 1, 2500, 3, "C5", 2, 500,
  3, "C6", 1, 3500, 3, "C6", 2, 2500
  
)

# Combine all block x card x alternative combinations with attributes and costs
design_table <- expand_grid(
  block = 1:3,
  choice_card = paste0("C", 1:6), 
  alt = 1:3
  ) |>
  left_join(attribute_design, by = c("block", "choice_card", "alt")) |>
  left_join(cost_design, by = c("block", "choice_card", "alt")) |>
  mutate(across(c(indre, midtre, ytre_vest, ytre_ost), ~if_else(alt == 3, 0L, .x))
  )

#---------------------------------------------------------------
# 3. Merge respondent choices with the experimental design 
#---------------------------------------------------------------
choices_full <- choices_long |>
  left_join(design_table, by = c("block", "choice_card", "alt")) |>
  mutate(
    cost = if_else(alt == 3, 0L, cost),
    cost_k = cost / 1000,
    chid = interaction(uuid, choice_card, drop = TRUE)
  )

# Check for unmatched rows
unmatched <- choices_full |>
  filter(alt != 3 & (is.na(indre) | is.na(cost_k)))
if (nrow(unmatched) > 0) {
  warning(nrow(unmatched), " rows with missing attributes or cost after join")
}

#---------------------------------------------------------------
# 4. Keep only the variables needed for modeling
#---------------------------------------------------------------

choices_full <- choices_full |>
  dplyr::select(uuid, chid, block, choice_card, alt, chosen,
                indre, midtre, ytre_vest, ytre_ost, cost, cost_k,
                gender, age_group, fylke2024, kommune2024)

# Save long-format choice dataset 
saveRDS(choices_full, here("data", "processed", "choices_full.rds"))