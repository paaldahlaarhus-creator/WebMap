#============================
# explor.R
# Exploratory and diagnostic analysis
# 
# Run after 01-03
# Sections:
# 1. Attribute design
# 2. Cost design
# 3. Behavioral checks
# 4. Protest diagnostics
#============================

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(purrr)
library(here)
library(readr)

# Helper: detect selected SPSS checkboxes
is_selected <- function(x) {
  !is.na(x) & str_trim(x) != "" & !str_detect(x, "^NO TO")
}

# Load data
data_raw <- readRDS(here("data", "processed", "data_raw.rds"))
choices_full <- readRDS(here("data", "processed", "choices_full.rds"))
sq_classification <- readRDS(here("data", "processed", "response_classification.rds"))

# Policy alternatives only (alt 1 & 2)
design <- choices_full |> 
  filter(alt != 3) |> 
  distinct(block, choice_card, alt, indre, midtre, ytre_vest, ytre_ost, cost_k)

#----------------------------
# 1. Attribute design
#----------------------------

# How often does each region appear across the full design?
design |> 
  summarise(across(c(indre, midtre, ytre_vest, ytre_ost), sum)) |> 
  pivot_longer(everything(), names_to = "region", values_to = "n_appearances")

# Correlation between region attributes
# High negative correlation makes it harder to separate regional WTP estimates
design |> 
  dplyr::select(indre, midtre, ytre_vest, ytre_ost) |> 
  cor()

# Region appearances per block
design |> 
  group_by(block) |> 
  summarise(across(c(indre, midtre, ytre_vest, ytre_ost), sum))

# How many regions improve simultaneously in each alternative?
design |>                                                                               
  mutate(n_improving = indre + midtre + ytre_vest + ytre_ost) |>                        
  count(n_improving)

# How often is each region the sole improving region, by block?              
design |>
  mutate(                                                                               
    n_improving    = indre + midtre + ytre_vest + ytre_ost,
    indre_sole     = indre     == 1  & n_improving == 1,
    midtre_sole    = midtre    == 1  & n_improving == 1,                                 
    ytre_vest_sole = ytre_vest == 1  & n_improving == 1,
    ytre_ost_sole  = ytre_ost  == 1  & n_improving == 1                                  
  ) |>
  group_by(block) |> 
  summarise(across(ends_with("_sole"), sum))

#----------------------------
# 2. Cost design
#----------------------------

# Overall distribution of cost levels
design |> 
  count(cost_k) |>                                                                      
  arrange(cost_k)

# Cost range and mean per block
design |> 
  group_by(block) |>
  summarise(
    min_cost  = min(cost_k),
    max_cost  = max(cost_k),
    mean_cost = mean(cost_k),
    n_levels  = n_distinct(cost_k)
  )

# Frequency of each cost level per block?
design |> 
  count(block, cost_k) |>                                     
  pivot_wider(names_from  = block,
              values_from = n,
              names_prefix = "block_",
              values_fill  = 0)

#---------------------------------------------                
# 3. Behavioural checks                                       
#---------------------------------------------                
                                                           
# Cost sensitivity: choice rate should fall as cost rises  
choices_full |>
  filter(alt != 3) |>
  group_by(cost_k) |>
  summarise(choice_rate = mean(chosen))

# Choice shares by alternative — A and B should be roughly equal
choices_full |>
  group_by(alt) |>
  summarise(share = mean(chosen))

# SQ share by block
choices_full |>
  group_by(block) |>
  summarise(sq_share = mean(chosen[alt == 3]))

# SQ choice frequency distribution 
sq_classification |>
  count(n_sq, name = "n_respondents") |>
  arrange(n_sq) |>
  mutate(
    share = round(n_respondents / sum(n_respondents), 3),
    cumulative_share = round(cumsum(n_respondents) / sum(n_respondents), 3)
  )

                                                          
# 4. Protest diagnostics 
# These analysis informed the classification decisions in 03

# --- 4a. C8 receipt ---
message("Respondents who received C8: ", 
        sum(sq_classification$received_c8),
        " (", round(100 * mean(sq_classification$received_c8), 1), "% of sample)")

# --- 4b. C8 routing check: was C8 sent only to always-SQ respondents? ---            
message("\n--- C8 receipt x always-SQ (2x2) ---")                                     
print(with(sq_classification, table(sq_all, received_c8)))                            

# n_sq distribution among C8 recipients only                                          
message("\n--- n_sq distribution among C8 recipients ---")                            
print(                                                                                
  sq_classification |>                                                                
    filter(received_c8) |>                                                            
    count(n_sq) |>
    mutate(share = round(n / sum(n), 3))
)                    

# Individual C8 checkbox frequency among recipients
c8_recipients <- data_raw |> 
  left_join(sq_classification |> dplyr::select(uuid, received_c8), by = "uuid") |>
  filter(received_c8)
n_c8 <- nrow(c8_recipients)
message("C8 recipients: ", n_c8)

c8_checkbox_dist <- c8_recipients |>
  mutate(across(C8_newr1:C8_newr10, is_selected)) |>                        
  summarise(across(C8_newr1:C8_newr10, sum)) |>
  tidyr::pivot_longer(everything(), names_to = "checkbox", values_to = "n") |>              
  mutate(share = round(n / n_c8, 3)) |>                                     
  arrange(checkbox) 

message("\n--- C8 checkbox frequencies (among recipients) ---")
print(c8_checkbox_dist)                                                               

# How many boxes did each recipient check?                             
c8_combos <- c8_recipients |>
  mutate(                                                                             
    across(C8_newr1:C8_newr10, is_selected),
    n_checked = rowSums(across(C8_newr1:C8_newr10))                                   
  ) |>
  count(n_checked) |>                                                                 
  mutate(share = round(n / sum(n), 3))                                                

message("\n--- Number of C8 boxes checked per respondent ---")                        
print(c8_combos)

# Which specific combinations appear more than once?                                  
c8_patterns <- c8_recipients |>
  mutate(across(C8_newr1:C8_newr10, is_selected)) |>                                  
  group_by(across(C8_newr1:C8_newr10)) |>
  summarise(n = n(), .groups = "drop") |>                                             
  filter(n > 1) |>
  arrange(desc(n))                                                                    

message("\n--- Repeated C8 checkbox combinations (n > 1) ---")                        
print(c8_patterns)


# --- 4b. High-SQ respondents who did not receive C8 ---
message("\n--- High-SQ respondents who did NOT recieve C8 ---")
print(
  sq_classification |> 
    filter(n_sq > 3, !received_c8) |> 
    count(n_sq) |> 
    mutate(share = round(n / sum(n), 3))
)


# --- 4c. C7 self-reported choice behaviour flags ---
c7_summary <- data_raw |>
  mutate(
    r1 = is_selected(C7_newr1),
    r2 = is_selected(C7_newr2),
    r3 = is_selected(C7_newr3),
    r4 = is_selected(C7_newr4)
  ) |>
  summarise(across(c(r1, r2, r3, r4), list(n = sum, share = mean))) |>
  tidyr::pivot_longer(everything(), names_to  = c("flag", ".value"), names_sep = "_") |>
  mutate(
    label = c(
      "r1" = "Ignored regions I don't often visit",
      "r2" = "Did not take costs into account",
      "r3" = "Always chose lowest cost option",
      "r4" = "Wrote a comment"
    )[flag],
    share = round(share, 3)
  ) |>
  dplyr::select(flag, label, n, share)

message("\n--- C7 choice behaviour flags (full sample) ---")
print(c7_summary)

# --- 4d. C7 cost flags x C8 receipt ---
c7_c8 <- data_raw |>
  mutate(
    c7_r2 = is_selected(C7_newr2),
    c7_r3 = is_selected(C7_newr3)
  ) |>
  left_join(sq_classification |> dplyr::select(uuid, received_c8), by = "uuid")

message("\n--- C7_newr2 (ignored costs) x C8 receipt ---")
print(with(c7_c8, table(c7_r2, received_c8)))

message("\n--- C7_newr3 (always lowest cost) x C8 receipt ---")
print(with(c7_c8, table(c7_r3, received_c8)))

# --- 4e. Classification breakdown ---
message("\n--- SQ classification (full sample) ---")
print(sq_classification |> 
        count(response_type) |> 
        mutate(share = round(n / sum(n), 3))
)

# --- 4e (plot). Classification distribution ---
# Visual summary of response_type counts and shares
response_type_plot <- sq_classification |>
  count(response_type) |>
  mutate(
    share = n / sum(n),
    label = paste0(n, "\n(", round(share * 100, 1), "%)"),
    # Order bars from most to least common
    response_type = forcats::fct_reorder(response_type, n, .desc = TRUE)
  ) |>
  ggplot(aes(x = response_type, y = n)) +
  geom_col(fill = "#1565C0", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = label), vjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x     = NULL,
    y     = "Number of respondents",
    title = "Distribution of response types",
    caption = "Classification based on always-SQ behaviour and C8 checkbox responses"
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(response_type_plot)

# --- 4f. Protest rate by block ---
# Checks whether protesters are concentrated in any one block
message("\n--- Protest and always-SQ rates by block ---")
print(
  sq_classification |> 
    left_join(choices_full |> distinct(uuid, block), by = "uuid") |> 
    group_by(block) |> 
    summarise(
      n = n(),
      n_protest = sum(is_protest),
      share_protest = round(mean(is_protest), 3),
      n_sq_all = sum(sq_all),
      share_sq_all = round(mean(sq_all), 3),
      .groups = "drop"
    )
)

# --- 4g. Protest rate by gender ---
message("\n--- Protest rate by gender ---")
print(
  sq_classification |>
    left_join(data_raw |> dplyr::select(uuid, gender), by = "uuid") |>
    group_by(gender) |>
    summarise(
      n = n(),
      n_protest = sum(is_protest),
      share_protest = round(mean(is_protest), 3),
      .groups = "drop"
    )
)

# --- 4h. Protest rate by background characteristics ---
# For each socio-demographic variable, shows the share of protesters within
# each category. Reveals whether any subgroup is over-represented among
# protesters, which could indicate systematic non-response bias.
#
# Note: uses age_group (categorical) — update the key below if the raw
# variable has a different name.

protest_bg <- sq_classification |>
  dplyr::select(uuid, is_protest) |>
  left_join(
    data_raw |> dplyr::select(uuid, age_group, fylke2024, C9, C10,
                               educationLevel, household_income),
    by = "uuid"
  )

# Helper: dodged bar chart comparing the distribution of a background variable
# between protesters and the full sample. Both groups sum to 100%, so bars show
# composition shares with a common denominator per group — making it easy to
# spot over- or under-representation of any category among protesters.
# var   : column name as a string (used with .data[[var]])
# title : plot title string
protest_rate_chart <- function(data, var, title) {
  total_protesters  <- sum(data$is_protest)
  total_respondents <- nrow(data)

  # Compute shares within protesters and within full sample separately
  protest_shares <- data |>
    filter(is_protest) |>
    group_by(.data[[var]]) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(group = paste0("Protesters (n=", total_protesters, ")"),
           share = n / total_protesters)

  sample_shares <- data |>
    group_by(.data[[var]]) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(group = paste0("Full sample (n=", total_respondents, ")"),
           share = n / total_respondents)

  # Use protester ordering for both groups so bars align
  cat_order <- protest_shares |>
    arrange(desc(share)) |>
    pull(.data[[var]]) |>
    as.character()

  bind_rows(protest_shares, sample_shares) |>
    filter(!is.na(.data[[var]])) |>
    mutate(
      category  = factor(as.character(.data[[var]]), levels = cat_order),
      group     = factor(group, levels = c(
        paste0("Protesters (n=", total_protesters, ")"),
        paste0("Full sample (n=", total_respondents, ")")
      )),
      bar_label = paste0(n, "\n", round(share * 100, 1), "%")
    ) |>
    ggplot(aes(x = category, y = share, fill = group)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    geom_text(aes(label = bar_label),
              position = position_dodge(width = 0.7),
              vjust = -0.2, size = 2.8, lineheight = 0.9) +
    scale_fill_manual(values = c("#C62828", "#1565C0"), name = NULL) +
    scale_y_continuous(
      labels = \(x) paste0(round(x * 100), "%"),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.25))
    ) +
    labs(
      x       = NULL,
      y       = "Share within group",
      title   = title,
      caption = "Labels: n  |  % share within group. Both groups sum to 100%."
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x  = element_text(angle = 35, hjust = 1),
      legend.position = "bottom"
    )
}

bg_vars <- c(
  age_group        = "Protest rate by age group",
  fylke2024        = "Protest rate by county (fylke)",
  C9               = "Protest rate by C9",
  C10              = "Protest rate by C10",
  educationLevel   = "Protest rate by education level",
  household_income = "Protest rate by household income"
)

iwalk(bg_vars, \(title, var) print(protest_rate_chart(protest_bg, var, title)))

