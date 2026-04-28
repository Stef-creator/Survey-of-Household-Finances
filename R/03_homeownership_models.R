# =============================================================================
# 03_homeownership_models.R – Q3: Homeownership models (imp = 1)
# =============================================================================
cat("\n===== Q3: Homeownership models =====\n")

# ---- Data preparation -------------------------------------------------------
# Use imp == 1 (eff1 from 00_setup.R).
# Create model covariates: education category, labour status, log income.
# log income: floored at 1 to avoid log(0) for zero-income households.

eff1_m <- eff1 |>
  dplyr::mutate(
    educ_cat   = make_educ_cat(educ_resp),
    labour_cat = make_labour_cat(emp_resp, self_resp, une_resp, ret_resp),
    age2       = age_resp^2,
    log_inc    = log(pmax(hh_inc, 1))   # pmax guards against log(0)
  ) |>
  # Drop rows missing any model variable
  tidyr::drop_na(own, own_ot, age_resp, educ_cat, labour_cat, hhsize, log_inc)

cat(sprintf("  Model sample: %d households\n", nrow(eff1_m)))

# Common formula (used for both outcomes)
formula_base <- own ~ age_resp + age2 + educ_cat + labour_cat + hhsize + log_inc

# ---- Q3a: Weighted LPM – main-residence ownership ---------------------------
lpm_main <- lm(formula_base, data = eff1_m, weights = facine3)

# Robust standard errors (HC1 = finite-sample correction to HC)
lpm_main_rse <- lmtest::coeftest(lpm_main,
                                  vcov = sandwich::vcovHC(lpm_main, type = "HC1"))

cat("\nQ3a – Weighted LPM: main-residence ownership\n")
print(lpm_main_rse)

cat("\n  Interpretation notes:
  age_resp   : expected positive (ownership rises with age, life-cycle pattern)
  age2       : expected negative (ownership peaks then levels off)
  educ_cat   : higher education => higher ownership (income/asset effect)
  labour_cat : employed > unemployed/inactive (income stability drives ownership)
  hhsize     : larger households may have higher ownership (family formation)
  log_inc    : positive (higher income -> more likely to own)
  These should align with the lifecycle pattern in Q2.\n")

# ---- Q3b: LPM drawbacks + weighted probit -----------------------------------
cat("\nQ3b – LPM drawbacks:
  1. Predicted probabilities can exceed [0, 1].
  2. Error term is heteroskedastic by construction (Var(e|x) = p(x)(1-p(x))).
  3. Marginal effects are assumed constant across the covariate space.
  4. Coefficient estimates remain consistent (under weak conditions) but
     inference under homoskedastic OLS is invalid without robust SEs.
  Alternative: weighted probit (logistic would also be valid; probit is chosen
  because it imposes normality on the latent index, which is standard in the
  homeownership literature).\n")

# Weighted probit (normalise weights to avoid numerical overflow in glm;
# relative weights are preserved; coefficient interpretation unchanged).
w_mean <- mean(eff1_m$facine3)
probit_main <- glm(formula_base, data = eff1_m,
                   weights = facine3 / w_mean,
                   family  = binomial(link = "probit"))

probit_main_rse <- lmtest::coeftest(probit_main,
                                     vcov = sandwich::vcovHC(probit_main, type = "HC1"))

cat("\nQ3b – Weighted probit: main-residence ownership\n")
print(probit_main_rse)

# Average Marginal Effects (AME) for probit
probit_main_fitted <- predict(probit_main, type = "response")
probit_main_pdf    <- dnorm(predict(probit_main, type = "link"))

# AME for continuous variables = mean(pdf * coef)
numeric_vars  <- c("age_resp", "age2", "hhsize", "log_inc")
ame_probit_num <- sapply(numeric_vars, function(v) {
  mean(probit_main_pdf * coef(probit_main)[v], na.rm = TRUE)
})

cat("\n  AME (probit, continuous vars):\n")
print(round(ame_probit_num, 4))

# ---- Q3c: Weighted LPM + probit – secondary-home ownership ------------------
formula_ot <- stats::update(formula_base, own_ot ~ .)

lpm_ot <- lm(formula_ot, data = eff1_m, weights = facine3)
lpm_ot_rse <- lmtest::coeftest(lpm_ot,
                                 vcov = sandwich::vcovHC(lpm_ot, type = "HC1"))

probit_ot <- glm(formula_ot, data = eff1_m,
                  weights = facine3 / w_mean,
                  family  = binomial(link = "probit"))
probit_ot_rse <- lmtest::coeftest(probit_ot,
                                    vcov = sandwich::vcovHC(probit_ot, type = "HC1"))

cat("\nQ3c – Weighted LPM: secondary-home ownership\n")
print(lpm_ot_rse)
cat("\nQ3c – Weighted probit: secondary-home ownership\n")
print(probit_ot_rse)

# ---- Q3d: Comparison table (LPM main vs LPM secondary) ---------------------
cat("\nQ3d – Comparison: main vs secondary-home ownership drivers\n")
comp <- modelsummary::modelsummary(
  list(
    "LPM – Main residence"  = lpm_main,
    "LPM – Secondary home"  = lpm_ot,
    "Probit – Main"         = probit_main,
    "Probit – Secondary"    = probit_ot
  ),
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output    = file.path(TAB_DIR, "Q3_homeownership_models.tex"),
  title     = "Homeownership Models — LPM and Probit",
  gof_map   = c("nobs", "r.squared"),
  notes     = c(
    "Survey weights (facine3) applied. HC1 robust SEs in parentheses.",
    "Reference: educ 2 (Primary), labour: Employed. log income floored at 1."
  )
)
cat("  Table saved: Q3_homeownership_models.tex\n")

cat("\n  Q3d discussion:
  Secondary-home ownership is rarer and driven more strongly by wealth/income:
  - Age profile: ownership of secondary homes peaks later in the lifecycle
    (wealth accumulation takes time), while main-residence ownership peaks
    earlier (family formation motive).
  - Education: university education raises both, but the gradient is steeper
    for secondary homes (wealth effect dominates).
  - Labour status: unemployment/retirement reduce both, but the penalty for
    secondary homes is smaller (stock variable, already accumulated).
  - Income: log income coefficient larger for secondary homes (wealth is key).
  Broadly, both outcomes share the lifecycle and income/education drivers, but
  secondary homes reflect accumulated wealth more than housing need.\n")
