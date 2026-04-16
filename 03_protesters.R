#=====================================================================
# 03_protesters.R
# Identify and classify protest responses
#
# Two step classification
#  Step 1: always-SQ respondents classified via C8 checkboxes
#  Step 2: comment-based consistency check for always-SQ respondents 
#          (exports sq_comments_review.csv for manual review;
#          applies overrides from sq_comments_review_coded.csv)
#
# Input:  data/processed/data_raw.rds
#         data/processed/choices_full.rds
#         data/processed/sq_comments_review_coded.csv
#         
# Output: data/processed/response_classification.rds
#         data/processed/choices_model.rds
#         data/processed/choices_model_r2excl.rds
#         data/processed/sq_comments_review.csv
#======================================================================

library(dplyr)
library(stringr)
library(here)
library(readr)

# Helper: detect selected SPSS checkboxes
# SPSS exports unselected checkboxes as "NO TO <label>"
is_selected <- function(x) {
  !is.na(x) & str_trim(x) != "" & !str_detect(x, "^NO TO")
}

#---------------------------------------------
# 1. Load data
#---------------------------------------------
data_raw     <- readRDS(here("data", "processed", "data_raw.rds"))
choices_full <- readRDS(here("data", "processed", "choices_full.rds"))

# 2. Count SQ choices
sq_counts <- choices_full |> 
  group_by(uuid) |> 
  summarise(
    n_sq = sum(chosen[alt == 3]),
    n_total = n_distinct(choice_card),
    sq_all = (n_sq == n_total),
    .groups = "drop"
  )

# Detect C8 receipt (used for diagnostics only)
c8_receipt <- data_raw |> 
  mutate(
    received_c8 = is_selected(C8_newr1) | is_selected(C8_newr2) |
                  is_selected(C8_newr3) | is_selected(C8_newr4) |                                           
                  is_selected(C8_newr5) | is_selected(C8_newr6) |
                  is_selected(C8_newr7) | is_selected(C8_newr8) |                                           
                  is_selected(C8_newr9) | is_selected(C8_newr10)                                            
  ) |>
  dplyr::select(uuid, received_c8)

# 3. Build C8 reason flags - checkboxes only
  #   Protest:     C8_newr2, r5, r6, r7                                                                       
  #   True zero:   C8_newr1, r3, r4, r9
  #   Sensitivity: C8_newr8

c8_reasons <- data_raw |>                                                                                   
  mutate(       
    reason_protest     = is_selected(C8_newr2) | is_selected(C8_newr5) |
                         is_selected(C8_newr6) | is_selected(C8_newr7),                                     
    reason_truezero    = is_selected(C8_newr1) | is_selected(C8_newr3) |
                         is_selected(C8_newr4) | is_selected(C8_newr9),                                     
    reason_sensitivity = is_selected(C8_newr8)
  ) |>
  dplyr::select(uuid, reason_protest, reason_truezero, reason_sensitivity)

# 4. Classify respondents (step 1 - checkbox only)
# Gate: sq_all == TRUE (always-SQ)
# Priority order: Protest > True zero > Sensitivity > Unclassified
# Non-always respondents with n_sq > 3 are flagged for potential future
# review but not excluded. All others: Non-protest
response_classification <- sq_counts |> 
  left_join(c8_receipt, by = "uuid") |>
  left_join(c8_reasons, by = "uuid") |>
  mutate(
    response_type = case_when(
      sq_all & reason_protest      ~ "Protest",
      sq_all & reason_truezero     ~ "True zero",
      sq_all & reason_sensitivity  ~ "Sensitivity",
      sq_all                       ~ "Unclassified",
      TRUE                         ~ "Non-protest"
    ),
    is_protest = (response_type == "Protest")
  )

n_total  <- nrow(response_classification)
n_sq_all <- sum(response_classification$sq_all)
n_c8     <- sum(response_classification$received_c8, na.rm = TRUE)

message("\n--- Sample overview ---")
message("Total respondents: ", n_total)
message("Always-SQ: ", n_sq_all, " (", round(100 * n_sq_all / n_total, 1), "%)")
message("Received C8: ", n_c8, " (", round(100 * n_c8 / n_total, 1), "%)")

message("\n--- Step 1 classification (checkboxes only) ---")
print(
  response_classification |> 
    count(response_type) |> 
    mutate(share = round(n / sum(n), 3))
)

# 5. Extract always-SQ comments for manual review (step 2)
# Targets always-SQ respondents who left a comment in C7 or C8 
# Export for manual review - fill in comment_override and save 
# as sq_comments_review_coded.csv to apply overrides.

sq_comments_raw <- data_raw |> 
  mutate(
    has_c7_comment = !is.na(C7_comment),
    has_c8_comment = !is.na(C8_comment)
    ) |> 
  filter(has_c7_comment | has_c8_comment) |> 
  dplyr::select(uuid, C7_newr1, C7_newr2, C7_newr3, C7_comment,
         C8_newr1, C8_newr2, C8_newr3, C8_newr4, C8_newr5,
         C8_newr6, C8_newr7, C8_newr8, C8_newr9, C8_comment
         )

sq_comments_review <- response_classification |> 
  filter(sq_all) |> 
  dplyr::select(uuid, n_sq, response_type, reason_protest,
         reason_truezero, reason_sensitivity) |>
  inner_join(sq_comments_raw, by = "uuid") |> 
  mutate(comment_override = NA_character_)

message("\nAlways-SQ respondents with comments to review: ",
        nrow(sq_comments_review))

write_delim(
  sq_comments_review,
  here("data", "processed", "sq_comments_review.csv"),
  delim = ";"
)
message("Saved: sq_comments_review.csv")

#---------------------------------------------                            
# 6. Apply comment overrides (step 2) 
# Loads sq_comments_review_coded.csv and replace 
# response_type where comment_override is filled in.
# NA leaves the step 1 classification unchanged.
#---------------------------------------------
coded_path <- here("data", "processed", "sq_comments_review_coded.csv")

if (file.exists(coded_path)) {
  coded <- read_delim(coded_path, delim = ";", show_col_types = FALSE)

  response_classification <- response_classification |> 
    left_join(coded |> dplyr::select(uuid, comment_override), by = "uuid") |>
    mutate(
      response_type = case_when(
        !is.na(comment_override) ~ comment_override, 
        TRUE                     ~ response_type
        ),
      is_protest = (response_type == "Protest")
      )

  n_overridden <- sum(!is.na(coded$comment_override))
  message("\nStep 2 override applied: ", n_overridden, "respondent(s) reclassified")

  message("\n--- Final classification (after overrides) ---")
  print(
    response_classification |> 
      count(response_type) |> 
      mutate(share = round(n / sum(n), 3))
    )
  } else {
    warning("sq_comments_review_coded.csv not found - using step 1 classification only.")
}

# 7. Build modelling datasets
# Uses final response_classification after step 2 overrides
# choices_model: protest excluded (main sample)
# choices_model_r2excl: also excludes self-reported cost ignorers
protest_ids <- response_classification |>
  filter(is_protest) |>                                                   
  pull(uuid)

choices_model <- choices_full |>
  filter(!uuid %in% protest_ids)

# SQ frequency in the modelling sample (post protest removal)
sq_freq_model <- choices_model |> 
  filter(alt == 3) |> 
  summarise(
    n_choices = n(),
    n_sq_chosen = sum(chosen),
    sq_rate = mean(chosen)
  )
message("SQ choice rate (no-protest sample: ",
        round(sq_freq_model$sq_rate * 100, 1), "% of choice occasions")


cost_ignoring_ids <- data_raw |>                                          
  filter(is_selected(C7_newr2)) |>
  pull(uuid)                                                              

choices_model_r2excl <- choices_full |>
  filter(!uuid %in% c(protest_ids, cost_ignoring_ids))

message("\nRespondents removed (protest):                    ",
        length(protest_ids))                                                      
message("Additional removed (self-reported cost ignoring):   ",
        length(setdiff(cost_ignoring_ids, protest_ids)))
message("Respondents in choices_model:                       ",           
        n_distinct(choices_model$uuid))
message("Respondents in choices_model_r2excl:                ",           
        n_distinct(choices_model_r2excl$uuid))

#---------------------------------------------
# 8. Save outputs                                                     
#---------------------------------------------
saveRDS(response_classification, here("data", "processed", "response_classification.rds"))
message("Saved: response_classification.rds")

saveRDS(choices_model, here("data", "processed", "choices_model.rds"))
message("Saved: choices_model.rds")                                       

saveRDS(choices_model_r2excl, here("data", "processed", "choices_model_r2excl.rds"))
message("Saved: choices_model_r2excl.rds") 