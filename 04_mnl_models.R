#===================================================
# 04_mnl_models.R
# Estimate Multinomial Logit (MNL) models
#
# Input:  data/processed/choices_full.rds
#         data/processed/choices_model.rds
#
# Output: data/models/mnl_full.rds
#         data/models/mnl_noprotest.rds
#         data/processed/wtp_mnl.rds
#===================================================

library(dplyr)
library(purrr)
library(mlogit)
library(modelsummary)
library(here)
library(MASS)  # mvrnorm() for Krinsky-Robb; loaded last — masks dplyr::select

#---------------------------------------------
# 1. Load data
#---------------------------------------------
choices_full      <- readRDS(here("data", "processed", "choices_full.rds"))
choices_noprotest <- readRDS(here("data", "processed", "choices_model.rds"))

message("Full sample:       ", n_distinct(choices_full$uuid),      " respondents")
message("No-protest sample: ", n_distinct(choices_noprotest$uuid), " respondents")

#---------------------------------------------
# 2. Convert to mlogit format
#---------------------------------------------
to_mlogit <- function(data) {
  mlogit.data(
    data,
    choice   = "chosen",
    chid.var = "chid",
    alt.var  = "alt",
    id.var   = "uuid"
  )
}

mdata_full      <- to_mlogit(choices_full)
mdata_noprotest <- to_mlogit(choices_noprotest)

#---------------------------------------------
# 3. Model formula
#    | 0 suppresses mlogit's default per-alternative intercepts
#    so all attributes get one shared coefficient across alts.
#---------------------------------------------
formula_base <- chosen ~ indre + midtre + ytre_vest + ytre_ost + cost_k | 0

#---------------------------------------------
# 4. Estimate models
#---------------------------------------------
mnl_full      <- mlogit(formula_base, data = mdata_full)
mnl_noprotest <- mlogit(formula_base, data = mdata_noprotest)

#---------------------------------------------
# 5. Coefficient table
#    Stars: * p<0.1   ** p<0.05   *** p<0.01
#---------------------------------------------
print(modelsummary(
  list("Full sample" = mnl_full, "'Protesters' excluded" = mnl_noprotest),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "cost_k"    = "Cost (1 000 NOK)",
    "indre"     = "Indre",
    "midtre"    = "Midtre",
    "ytre_vest" = "Ytre vest",
    "ytre_ost"  = "Ytre øst"
  ),
  gof_map = c("nobs", "aic", "r2.mcfadden"),
  title   = "MNL coefficient estimates"
))

message("Log-likelihood — Full: ",      round(as.numeric(logLik(mnl_full)), 2),
        "  |  No-protest: ", round(as.numeric(logLik(mnl_noprotest)), 2))

#---------------------------------------------
# 6. Krinsky-Robb WTP simulation
#    Draws n_sim coefficient vectors from MVN(β̂, Σ̂)
#    and computes WTP = -β_attr / β_cost per draw.
#    95% CI from the 2.5th and 97.5th percentiles.
#    WTP is in 1 000 NOK units (same as cost_k).
#---------------------------------------------

# Inputs: mlogit model, number of simulation draws
# Output: tibble with attribute, mean WTP, and 95% CI
compute_wtp_kr <- function(model, n_sim = 10000) {
  beta  <- coef(model)
  sigma <- vcov(model)
  draws <- MASS::mvrnorm(n_sim, mu = beta, Sigma = sigma)

  attrs <- c("indre", "midtre", "ytre_vest", "ytre_ost")

  map_dfr(attrs, function(attr) {
    wtp_draws <- -draws[, attr] / draws[, "cost_k"]
    tibble(
      attribute = attr,
      wtp       = mean(wtp_draws),
      ci_lower  = quantile(wtp_draws, 0.025),
      ci_upper  = quantile(wtp_draws, 0.975)
    )
  })
}

set.seed(42)
wtp_mnl <- bind_rows(
  compute_wtp_kr(mnl_full)      |> mutate(model = "Full sample"),
  compute_wtp_kr(mnl_noprotest) |> mutate(model = "'Protesters' excluded")
) |> dplyr::select(model, attribute, wtp, ci_lower, ci_upper)

print(wtp_mnl |> mutate(across(where(is.numeric), \(x) round(x, 3))))

#---------------------------------------------
# 7. Save models and WTP
#---------------------------------------------
dir.create(here("data", "models"), showWarnings = FALSE)

saveRDS(mnl_full,      here("data", "models", "mnl_full.rds"))
saveRDS(mnl_noprotest, here("data", "models", "mnl_noprotest.rds"))
saveRDS(wtp_mnl,       here("data", "processed", "wtp_mnl.rds"))

message("Saved: mnl_full, mnl_noprotest, wtp_mnl")
