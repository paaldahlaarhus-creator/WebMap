#=======================================
# 06_outputs.R
# WTP tables, model tables, figures
#
# Inputs:  data/models/mnl_*.rds
#          data/models/rpmxl.rds
#          data/processed/wtp_rpmxl.rds
#
# Outputs: output/tables/protest_comparison_table.docx
#          output/tables/model_table.docx
#          output/figures/wtp_mnl.png
#          output/figures/wtp_comparison.png
#=======================================

library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(ggplot2)
library(here)
library(modelsummary)


#---------------------------------------------
# 1. Load models and pre-computed MXL WTP
#---------------------------------------------

mnl_full          <- readRDS(here("data", "models", "mnl_full.rds"))
mnl_noprotest     <- readRDS(here("data", "models", "mnl_noprotest.rds"))
mnl_noprotest_asc <- readRDS(here("data", "models", "mnl_noprotest_asc.rds"))

# Pre-computed MNL WTP (saved by 04_mnl_models.R) — load directly to avoid
# re-running the K-R simulation and guarantee both scripts use identical results
wtp_results <- readRDS(here("data", "processed", "wtp_results.rds"))

# Pre-computed MXL WTP from Apollo unconditional distributions (05_mxl_model.R)
# Note: modelsummary does not support Apollo objects — MXL WTP handled separately
wtp_rpmxl   <- readRDS(here("data", "processed", "wtp_rpmxl.rds"))


#---------------------------------------------
# 2. WTP — filter MNL K-R results from 04
#
#    compute_wtp_kr() lives in 04_mnl_models.R.
#    Loading wtp_results.rds avoids re-running
#    the simulation and keeps both scripts in sync.
#    Division by a random cost (as in MXL) gives
#    undefined moments, so MXL WTP comes from
#    the unconditional distributions in wtp_rpmxl.
#---------------------------------------------

# Filter to preferred model; rename wtp -> wtp_1000kr for clarity
wtp_mnl_ci <- wtp_results |>
  dplyr::filter(model == "'Protesters' excluded (ASC)") |>
  dplyr::rename(wtp_1000kr = wtp)

message("--- MNL WTP (1 000 NOK, Krinsky-Robb 95% CI) ---")
print(wtp_mnl_ci |> dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3))))


#---------------------------------------------
# 3. Protest exclusion table
#    Full sample vs no-protest sample, base MNL (no ASC).
#    Shows how coefficients shift when protesters are removed.
#---------------------------------------------

coef_labels <- c(
  indre     = "Indre fjord",
  midtre    = "Midtre fjord",
  ytre_vest = "Ytre vest",
  ytre_ost  = "Ytre øst",
  cost_k    = "Cost (1 000 NOK)",
  ASC_SQ    = "ASC (status quo)"
)

modelsummary(
  list(
    "MNL — full sample"    = mnl_full,
    "MNL — 'protesters' excluded" = mnl_noprotest
  ),
  coef_rename = coef_labels,
  stars       = TRUE,
  gof_map     = c("nobs", "logLik", "AIC"),
  output      = here("output", "tables", "protest_comparison_table.docx")
)

message("Saved: output/tables/protest_comparison_table.docx")


#---------------------------------------------
# 4. MNL model table (modelsummary)
#
#    Apollo objects are not supported by
#    modelsummary, so only MNL models are
#    included here. MXL results appear in
#    the WTP comparison figure below.
#---------------------------------------------

modelsummary(
  list(
    "Full (no ASC)"        = mnl_full,
    "'Protesters' excluded (no ASC)" = mnl_noprotest,
    "'Protesters' excluded + ASC"    = mnl_noprotest_asc
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(                                                                 
    "indre"     = "Indre",                                                      
    "midtre"    = "Midtre",
    "ytre_vest" = "Ytre vest",                                                  
    "ytre_ost"  = "Ytre øst",
    "cost_k"    = "Cost (1 000 NOK)",
    "ASC_SQ"    = "ASC (status quo)"                                            
  ),
  gof_map = c("nobs", "logLik", "AIC", "BIC"),                                  
  title   = "MNL sensitivity: sample and specification"
)                                                                               

message("Saved: output/tables/model_table.docx")


#---------------------------------------------
# 5. WTP figure — MNL+ASC (Krinsky-Robb CIs)
#---------------------------------------------

attr_labels <- c(
  indre     = "Indre fjord",
  midtre    = "Midtre fjord",
  ytre_vest = "Ytre vest",
  ytre_ost  = "Ytre øst"
)

wtp_mnl_plot <- wtp_mnl_ci |>
  dplyr::mutate(
    region = factor(attribute,
                    levels = names(attr_labels),
                    labels = attr_labels),
    # Significant if the 95% CI does not straddle zero
    sig    = !(ci_lower < 0 & ci_upper > 0)
  ) |>
  ggplot(aes(x = region, y = wtp_1000kr, fill = sig)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "#90A4AE"),
                    labels = c("TRUE" = "Significant", "FALSE" = "Not significant"),
                    name   = NULL) +
  labs(
    x        = NULL,
    y        = "WTP (1 000 NOK per household)",
    title    = "Marginal WTP for ecological improvement by fjord region",
    subtitle = "MNL+ASC model, no-protest sample",
    caption  = "Error bars = 95% Krinsky-Robb confidence intervals"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  here("output", "figures", "wtp_mnl.png"),
  plot = wtp_mnl_plot, width = 7, height = 4.5, dpi = 300
)

message("Saved: output/figures/wtp_mnl.png")


#-------------------------------------------------------------
# 6. WTP comparison figure — MNL vs RP-MXL
#
#    MNL:    point estimate + K-R 95% CI
#    RP-MXL: median of unconditional distribution
#            (CIs suppressed - log-normal cost produces
#            fat-tailed WTP; median is the preffered summary)
#-------------------------------------------------------------

# Standardise column names before stacking
wtp_mnl_long <- wtp_mnl_ci |>
  dplyr::mutate(
    region = factor(attribute,
                    levels = names(attr_labels),
                    labels = attr_labels),
    model  = "MNL+ASC",
    wtp    = wtp_1000kr
  ) |>
  dplyr::select(model, region, wtp, ci_lower, ci_upper)

wtp_mxl_long <- wtp_rpmxl |>
  dplyr::rename(wtp = median) |>
  dplyr::mutate(
    region = factor(attribute,
                    levels = c("Indre fjord", "Midtre fjord",
                               "Ytre vest",   "Ytre øst")),
    model  = "RP-MXL", 
    # CIs suppressed: log-normal cost causes fat-tailed WTP distributions
    # where population-level percentile CIs are not comparable to MNL K-R CIs
    ci_lower = wtp,
    ci_upper = wtp
  ) |>
  dplyr::select(model, region, wtp, ci_lower, ci_upper)

wtp_combined <- bind_rows(wtp_mnl_long, wtp_mxl_long) |>
  dplyr::mutate(model = factor(model, levels = c("MNL+ASC", "RP-MXL")))

wtp_comparison_plot <- ggplot(
  wtp_combined,
  aes(x = region, y = wtp, fill = model,
      ymin = ci_lower, ymax = ci_upper)
) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.8) +
  geom_errorbar(position = position_dodge(width = 0.6), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = c("MNL+ASC" = "#2196F3", "RP-MXL" = "#E65100")) +
  labs(
    x       = NULL,
    y       = "WTP (1 000 NOK per household)",
    fill    = NULL,
    title   = "WTP for ecological improvement by fjord region",
    subtitle = "No-protest sample",
    caption = "MNL: 95% Krinsky-Robb CI. RP-MXL: median of unconditional population distribution (CIs not shown)."
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  here("output", "figures", "wtp_comparison.png"),
  plot = wtp_comparison_plot, width = 7, height = 5, dpi = 300
)

message("Saved: output/figures/wtp_comparison.png")
