# =============================================================================
# 06_mortgage_regressions.R – Q6: Mortgage holder + regressions + LaTeX table
# =============================================================================
cat("\n===== Q6: Mortgage holder regressions =====\n")

# ---- Q6a: Construct mortgage_holder indicator -------------------------------
# Definition:
#   "Owns real estate" = own == 1 OR own_ot == 1
#     (uses ownership indicators; realest > 0 is unreliable as it includes
#      valuables per the codebook, and could be > 0 even without property ownership)
#   "Strictly positive real-estate debt" = deudre > 0
#   mortgage_holder = 1 iff both conditions hold; 0 otherwise.
#   If realest == 0 but own == 1 (value not yet recorded), household is still
#   classified as owning real estate.

eff1_q6 <- eff1_c |>
  dplyr::mutate(
    owns_re          = as.integer(own == 1 | own_ot == 1),
    mortgage_holder  = as.integer(owns_re == 1 & deudre > 0)
  )

cat(sprintf(
  "\nQ6a – mortgage_holder: %d households (%.1f%% of all HH | %.1f%% of homeowners)\n",
  sum(eff1_q6$mortgage_holder),
  mean(eff1_q6$mortgage_holder) * 100,
  mean(eff1_q6$mortgage_holder[eff1_q6$own == 1]) * 100
))

# ---- Q6b: Three weighted LPM specs on homeowners sample --------------------
# Sample: own == 1 only
eff1_own <- eff1_q6 |>
  dplyr::filter(own == 1) |>
  dplyr::mutate(
    educ_cat   = make_educ_cat(educ_resp),
    age2       = age_resp^2,
    # Wealth quintiles (computed within homeowners sample)
    wealth_q   = cut(
      totnet,
      breaks = c(-Inf,
                 wt_quantile(totnet, facine3, probs = c(0.2, 0.4, 0.6, 0.8)),
                 Inf),
      labels = paste0("Q", 1:5),
      right  = TRUE
    )
  ) |>
  tidyr::drop_na(mortgage_holder, non_emp_rate, fin_share, age_resp, educ_cat,
                  hhsize, wealth_q)

cat(sprintf("  Homeowner sample: %d households\n", nrow(eff1_own)))

# Specification 1 – Baseline: controls + non_emp_rate + fin_share
m1 <- lm(
  mortgage_holder ~ non_emp_rate + fin_share + age_resp + age2 + educ_cat + hhsize,
  data    = eff1_own,
  weights = facine3
)

# Specification 2 – Baseline + wealth quintile
m2 <- lm(
  mortgage_holder ~ non_emp_rate + fin_share + age_resp + age2 + educ_cat + hhsize
                  + wealth_q,
  data    = eff1_own,
  weights = facine3
)

# Specification 3 – Spec 2 + interaction non_emp_rate × fin_share
m3 <- lm(
  mortgage_holder ~ non_emp_rate + fin_share + age_resp + age2 + educ_cat + hhsize
                  + wealth_q + non_emp_rate:fin_share,
  data    = eff1_own,
  weights = facine3
)

# Coefficient tables with HC1 robust SEs
cat("\nQ6b – Mortgage holder LPM: Specification 1 (Baseline)\n")
print(lmtest::coeftest(m1, vcov = sandwich::vcovHC(m1, type = "HC1")))

cat("\nQ6b – Mortgage holder LPM: Specification 2 (+ Wealth Quintile)\n")
print(lmtest::coeftest(m2, vcov = sandwich::vcovHC(m2, type = "HC1")))

cat("\nQ6b – Mortgage holder LPM: Specification 3 (+ Interaction)\n")
print(lmtest::coeftest(m3, vcov = sandwich::vcovHC(m3, type = "HC1")))

cat("\n  Q6b Interpretation notes:
  non_emp_rate : expected NEGATIVE — more non-employed members => lower
                 credit access / less ability to sustain mortgage payments.
                 May become attenuated once wealth quintile is controlled.
  fin_share    : expected NEGATIVE — high financial wealth share relative to
                 net wealth implies less real-estate wealth and thus likely
                 fewer / smaller mortgages.
  age_resp     : expected hump-shaped (positive then negative): young homeowners
                 are more likely to be actively paying a mortgage; older ones
                 have paid it off.
  educ_cat     : higher education => higher mortgage probability (income/access).
  hhsize       : larger households => more likely to have a mortgage (family need).
  wealth_q     : adding quintiles typically absorbs the non_emp_rate effect
                 (wealth correlates with both employment and mortgage payoff).
  Interaction  : if positive, the penalty of non-employment is smaller for
                 households with high financial wealth (buffer effect).\n")

# ---- Q6c: LaTeX regression table with modelsummary -------------------------
models_q6 <- list(
  "(1) Baseline"           = m1,
  "(2) + Wealth Quintile"  = m2,
  "(3) + Interaction"      = m3
)

# Custom row names for gof statistics
gof_q6 <- tibble::tribble(
  ~raw,          ~clean,   ~fmt,
  "nobs",        "N",      0,
  "r.squared",   "$R^2$",  3
)

modelsummary::modelsummary(
  models_q6,
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  output    = file.path(TAB_DIR, "Q6_mortgage_lpm.tex"),
  fmt       = 4,
  gof_map   = gof_q6,
  title     = "Mortgage Holder — Weighted LPM (Three Specifications)",
  notes     = c(
    "Sample: homeowners (\\texttt{own} = 1) from EFF 2017 (imp = 1).",
    "Survey weights (\\textit{facine3}) applied throughout.",
    "Robust standard errors (HC1) in parentheses.",
    "Reference: educ\\_cat = 2\\_Primary; wealth\\_q = Q1.",
    "$^{*}p<0.10$; $^{**}p<0.05$; $^{***}p<0.01$."
  )
)
cat("  LaTeX table saved: Q6_mortgage_lpm.tex\n")
