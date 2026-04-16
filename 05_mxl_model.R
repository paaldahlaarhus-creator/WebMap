#=============================================
# 05_mxl_model.R
# Estimate Random Parameters Mixed Logit (RP-MXL)
#
# Specification:
#   Regional attributes: random, normal
#   Cost: random, log-normal (sign-flipped),
#         ensures cost coefficient is always negative
#   Cost rescaled to 10 000 NOK units for numerical stability
#   500 Halton draws, 7 cores
#
# Input:  data/processed/choices_full.rds
#         data/processed/choices_model.rds
#
# Output: data/models/rpmxl_full.rds
#         data/models/rpmxl_noprotest.rds
#         data/processed/wtp_mxl.rds
#=============================================

# Load tidyverse packages before apollo to avoid masking conflicts
library(dplyr)
library(tidyr)
library(here)
library(apollo)

#---------------------------------------------
# Helper: prepare Apollo database
#    Apollo requires wide format with one row
#    per respondent x choice task, with attributes
#    in separate columns per alternative.
#    Use dplyr:: throughout - apollo masks several
#    dplyr functions on load.
#    Cost rescaled from 1 000 NOK to 10 000 NOK units for
#    numerical stability against 0/1 attribute dummies.
#---------------------------------------------
prepare_apollo_data <- function(choices) {
  chosen_per_task <- choices |>
    dplyr::filter(chosen == 1) |>
    dplyr::select(uuid, choice_card, chosen_alt = alt)

  choices |>
    dplyr::select(uuid, choice_card, alt,
                  indre, midtre, ytre_vest, ytre_ost, cost_k) |>
    tidyr::pivot_wider(
      names_from  = alt,
      values_from = c(indre, midtre, ytre_vest, ytre_ost, cost_k),
      names_glue  = "{.value}_alt{alt}"
    ) |>
    dplyr::left_join(chosen_per_task, by = c("uuid", "choice_card")) |>
    dplyr::mutate(
      id_individual  = as.integer(factor(uuid)),
      choice_task    = as.integer(factor(choice_card, levels = paste0("C", 1:6))),
      choice         = chosen_alt,
      cost_10k_alt1  = cost_k_alt1 / 10,
      cost_10k_alt2  = cost_k_alt2 / 10,
      cost_10k_alt3  = 0,
      indre_alt3     = tidyr::replace_na(indre_alt3,     0),
      midtre_alt3    = tidyr::replace_na(midtre_alt3,    0),
      ytre_vest_alt3 = tidyr::replace_na(ytre_vest_alt3, 0),
      ytre_ost_alt3  = tidyr::replace_na(ytre_ost_alt3,  0)
    )
}

#---------------------------------------------
# 1. Load data
#---------------------------------------------
choices_full      <- readRDS(here("data", "processed", "choices_full.rds"))
choices_noprotest <- readRDS(here("data", "processed", "choices_model.rds"))

message("Full sample:       ", n_distinct(choices_full$uuid),      " respondents")
message("No-protest sample: ", n_distinct(choices_noprotest$uuid), " respondents")

#---------------------------------------------
# 2. Starting values (shared across both models)
#    mu_*  = mean of underlying normal distribution
#    sd_*  = std dev of underlying normal distribution
#
#    Cost is log-normal with sign flip:
#      r_cost = -exp(mu_cost + sd_cost * draw)
#    This ensures the cost coefficient is always negative.
#    mu_cost = -1 centers the distribution around
#    -exp(-1) = -0.37 per 10 000 NOK
#---------------------------------------------
apollo_beta <- c(
  mu_indre     =  0.10,
  sd_indre     =  0.10,
  mu_midtre    =  0.30,
  sd_midtre    =  0.10,
  mu_ytre_vest =  0.30,
  sd_ytre_vest =  0.10,
  mu_ytre_ost  =  0.40,
  sd_ytre_ost  =  0.10,
  mu_cost      = -1.00,
  sd_cost      =  0.50
)

# All parameters freely estimated
apollo_fixed <- character(0)

#---------------------------------------------
# 3. Random draws (shared across both models)
#    500 Halton draws balances accuracy and runtime.
#---------------------------------------------
apollo_draws <- list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_indre", "draws_midtre",
                     "draws_ytre_vest", "draws_ytre_ost",
                     "draws_cost"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

#---------------------------------------------
# 4. Random coefficients (shared across both models)
#    Attributes: normal     r_k    = mu_k + sd_k * draw
#    Cost: log-normal       r_cost = -exp(mu_cost + sd_cost * draw)
#---------------------------------------------
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list(
    r_indre     = mu_indre     + sd_indre     * draws_indre,
    r_midtre    = mu_midtre    + sd_midtre    * draws_midtre,
    r_ytre_vest = mu_ytre_vest + sd_ytre_vest * draws_ytre_vest,
    r_ytre_ost  = mu_ytre_ost  + sd_ytre_ost  * draws_ytre_ost,
    r_cost      = -exp(mu_cost + sd_cost      * draws_cost)
  )
  return(randcoeff)
}

#---------------------------------------------
# 5. Likelihood function (shared across both models)
#    Utility is a linear combination of random
#    coefficients and attributes.
#    Alt 3 (SQ) utility = 0 (no ASC).
#---------------------------------------------
apollo_probabilities <- function(apollo_beta, apollo_inputs,
                                 functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  V <- list(
    alt1 = r_indre     * indre_alt1     +
           r_midtre    * midtre_alt1    +
           r_ytre_vest * ytre_vest_alt1 +
           r_ytre_ost  * ytre_ost_alt1  +
           r_cost      * cost_10k_alt1,

    alt2 = r_indre     * indre_alt2     +
           r_midtre    * midtre_alt2    +
           r_ytre_vest * ytre_vest_alt2 +
           r_ytre_ost  * ytre_ost_alt2  +
           r_cost      * cost_10k_alt2,

    alt3 = 0
  )

  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    V            = V
  )

  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#---------------------------------------------
# 6. Estimate models
#    database and apollo_control are reassigned
#    between models; apollo_validateInputs()
#    reads them from the global environment.
#    All other Apollo objects are shared.
#---------------------------------------------

# --- Model 1: Full sample ---
set.seed(42)
apollo_initialise()
database <- prepare_apollo_data(choices_full)
apollo_control <- list(
  modelName  = "rpmxl_full",
  modelDescr = "RP-MXL, log-normal cost, full sample",
  indivID    = "id_individual",
  mixing     = TRUE,
  nCores     = 7
)
message("\nEstimating rpmxl_full — ", n_distinct(database$id_individual), " individuals")
apollo_inputs_full <- apollo_validateInputs()

rpmxl_full <- apollo_estimate(
  apollo_beta, apollo_fixed,
  apollo_probabilities,
  apollo_inputs_full,
  estimate_settings = list(estimationRoutine = "bgw",
                           writeIter = FALSE, silent = FALSE)
)

# --- Model 2: No-protest sample ---
set.seed(42)
apollo_initialise()
database <- prepare_apollo_data(choices_noprotest)
apollo_control <- list(
  modelName  = "rpmxl_noprotest",
  modelDescr = "RP-MXL, log-normal cost, no-protest sample",
  indivID    = "id_individual",
  mixing     = TRUE,
  nCores     = 7
)
message("\nEstimating rpmxl_noprotest — ", n_distinct(database$id_individual), " individuals")
apollo_inputs_noprotest <- apollo_validateInputs()

rpmxl_noprotest <- apollo_estimate(
  apollo_beta, apollo_fixed,
  apollo_probabilities,
  apollo_inputs_noprotest,
  estimate_settings = list(estimationRoutine = "bgw",
                           writeIter = FALSE, silent = FALSE)
)

#---------------------------------------------
# 7. Coefficient table
#---------------------------------------------
print(data.frame(
  full_est = round(rpmxl_full$estimate,      3),
  full_se  = round(rpmxl_full$robse,         3),
  np_est   = round(rpmxl_noprotest$estimate, 3),
  np_se    = round(rpmxl_noprotest$robse,    3)
))

#---------------------------------------------
# 8. Unconditional WTP from population distributions
#    Draws from the estimated parametric distributions for each
#    random coefficient and computes WTP = r_attr / |r_cost|.
#    Cost was rescaled to 10 000 NOK during estimation, so
#    multiply by 10 to convert back to 1 000 NOK units.
#    Median is preferred: the log-normal cost produces a
#    fat-tailed WTP distribution where the mean is sensitive
#    to extreme draws.
#---------------------------------------------

# Inputs:  Apollo model object, number of simulation draws
# Output:  tibble with attribute and median WTP in 1 000 NOK
compute_wtp_mxl <- function(model, n_sim = 100000) {
  est <- model$estimate

  set.seed(42)
  r_indre     <- est["mu_indre"]     + est["sd_indre"]     * rnorm(n_sim)
  r_midtre    <- est["mu_midtre"]    + est["sd_midtre"]    * rnorm(n_sim)
  r_ytre_vest <- est["mu_ytre_vest"] + est["sd_ytre_vest"] * rnorm(n_sim)
  r_ytre_ost  <- est["mu_ytre_ost"]  + est["sd_ytre_ost"]  * rnorm(n_sim)
  # Cost is log-normal with sign flip; always negative
  r_cost      <- -exp(est["mu_cost"] + est["sd_cost"] * rnorm(n_sim))

  tibble::tibble(
    attribute  = c("Indre", "Midtre", "Ytre vest", "Ytre øst"),
    median_wtp = c(
      median(r_indre     / abs(r_cost)),
      median(r_midtre    / abs(r_cost)),
      median(r_ytre_vest / abs(r_cost)),
      median(r_ytre_ost  / abs(r_cost))
    ) * 10  # convert 10 000 NOK -> 1 000 NOK
  )
}

wtp_full      <- compute_wtp_mxl(rpmxl_full)      |> dplyr::mutate(model = "Full sample")
wtp_noprotest <- compute_wtp_mxl(rpmxl_noprotest) |> dplyr::mutate(model = "'Protesters' excluded")
wtp_mxl       <- dplyr::bind_rows(wtp_full, wtp_noprotest) |>
  dplyr::select(model, attribute, median_wtp)

print(wtp_mxl |> dplyr::mutate(median_wtp = round(median_wtp, 3)))

#---------------------------------------------
# 9. Save models and WTP
#---------------------------------------------
dir.create(here("data", "models"), showWarnings = FALSE)

saveRDS(rpmxl_full,      here("data", "models", "rpmxl_full.rds"))
saveRDS(rpmxl_noprotest, here("data", "models", "rpmxl_noprotest.rds"))
saveRDS(wtp_mxl,         here("data", "processed", "wtp_mxl.rds"))

message("Saved: rpmxl_full, rpmxl_noprotest, wtp_mxl")
