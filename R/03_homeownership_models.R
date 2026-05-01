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

# ---- Q3d: Comparison: main vs secondary-home ownership drivers --------------
cat("\nQ3d - Comparison: main vs secondary-home ownership drivers\n")

# Coefficient rename map (raw R name -> clean label) - shared across LPM/probit
coef_map_q3 <- c(
  "(Intercept)"             = "Intercept",
  "age_resp"                = "Age",
  "age2"                    = "Age$^2$",
  "educ_cat2_Primary"       = "\\hspace{6pt} Primary",
  "educ_cat3_Lower_sec"     = "\\hspace{6pt} Lower secondary (ESO)",
  "educ_cat4_Vocational"    = "\\hspace{6pt} Vocational",
  "educ_cat5_Upper_sec"     = "\\hspace{6pt} Upper secondary (Bachillerato)",
  "educ_cat6_Post_sec"      = "\\hspace{6pt} Post-secondary",
  "educ_cat7_University"    = "\\hspace{6pt} University",
  "educ_cat8_Other"         = "\\hspace{6pt} Other",
  "labour_catSelf_employed" = "\\hspace{6pt} Self-employed",
  "labour_catUnemployed"    = "\\hspace{6pt} Unemployed",
  "labour_catRetired"       = "\\hspace{6pt} Retired",
  "labour_catInactive"      = "\\hspace{6pt} Inactive",
  "hhsize"                  = "Household size",
  "log_inc"                 = "Log income"
)

options(modelsummary_factory_latex = "kableExtra")

# -- Helper: ordered group_map for the Q3 coef set ----------------------------
# Education block sits between Age^2 and the labour block; labour block sits
# between Education and hhsize/log_inc. Using group_map keeps the row order
# correct regardless of how many models are stacked.
group_map_q3 <- list(
  "\\textit{Education (ref.: Illiterate)}" = c(
    "educ_cat2_Primary", "educ_cat3_Lower_sec", "educ_cat4_Vocational",
    "educ_cat5_Upper_sec", "educ_cat6_Post_sec", "educ_cat7_University",
    "educ_cat8_Other"
  ),
  "\\textit{Labour status (ref.: Employed)}" = c(
    "labour_catSelf_employed", "labour_catUnemployed",
    "labour_catRetired", "labour_catInactive"
  )
)

# -- LPM-only table -----------------------------------------------------------
tbl_lpm <- modelsummary::modelsummary(
  list(
    "Main residence" = lpm_main,
    "Secondary home" = lpm_ot
  ),
  coef_map  = coef_map_q3,
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt       = fmt_smart(3),
  output    = "latex",
  booktabs  = TRUE,
  title     = "Weighted LPM estimates of homeownership (EFF 2017, imp~=~1)",
  label     = "lpm_only",
  gof_map   = list(
    list(raw = "nobs",      clean = "$N$",   fmt = "%d"),
    list(raw = "r.squared", clean = "$R^2$", fmt = "%.3f")
  ),
  escape = FALSE
) |>
  add_ref_groups(coef_map_q3, list(
    "Education (ref.: Illiterate)"   = c("educ_cat2_Primary", "educ_cat8_Other"),
    "Labour status (ref.: Employed)" = c("labour_catSelf_employed", "labour_catInactive")
  )) |>
  kableExtra::footnote(
    general = paste(
      "HC1 robust standard errors in parentheses.",
      "Survey weights (\\\\texttt{facine3}) applied.",
      "Log income $= \\\\ln(\\\\max(\\\\text{hh\\\\_inc},\\\\,1))$."
    ),
    escape         = FALSE,
    general_title  = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_lpm, file.path(TAB_DIR, "Q3c_lpm_only.tex"))
cat("  Table saved: Q3c_lpm_only.tex\n")

# -- Probit-only table --------------------------------------------------------
# Inject McFadden R² via glance_custom (not available for weighted GLM by default)
glance_custom.glm <- function(x, ...) {
  null_mod <- update(x, . ~ 1)
  mcf <- 1 - as.numeric(logLik(x)) / as.numeric(logLik(null_mod))
  data.frame(r2.mcfadden = mcf)
}

tbl_probit <- modelsummary::modelsummary(
  list(
    "Main residence" = probit_main,
    "Secondary home" = probit_ot
  ),
  coef_map  = coef_map_q3,
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt       = fmt_smart(3),
  output    = "latex",
  booktabs  = TRUE,
  title     = "Weighted probit estimates of homeownership (EFF 2017, imp~=~1)",
  label     = "probit_only",
  gof_map   = list(
    list(raw = "nobs",        clean = "$N$",               fmt = "%d"),
    list(raw = "r2.mcfadden", clean = "McFadden $R^2$",    fmt = "%.3f")
  ),
  escape = FALSE
) |>
  add_ref_groups(coef_map_q3, list(
    "Education (ref.: Illiterate)"   = c("educ_cat2_Primary", "educ_cat8_Other"),
    "Labour status (ref.: Employed)" = c("labour_catSelf_employed", "labour_catInactive")
  )) |>
  kableExtra::footnote(
    general = paste(
      "HC1 robust standard errors in parentheses.",
      "Survey weights (\\\\texttt{facine3}) applied; probit weights normalised by $\\\\text{mean}(\\\\text{facine3})$.",
      "Log income $= \\\\ln(\\\\max(\\\\text{hh\\\\_inc},\\\\,1))$."
    ),
    escape         = FALSE,
    general_title  = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_probit, file.path(TAB_DIR, "Q3d_probit_only.tex"))
cat("  Table saved: Q3d_probit_only.tex\n")

# ---- Average Marginal Effects table (probit models) -------------------------
# AME for continuous vars: mean[ phi(eta) * beta_k ]
# AME for binary/factor vars: mean[ Phi(eta | var=1) - Phi(eta | var=0) ]

compute_ame_probit <- function(model, data) {
  eta  <- predict(model, type = "link")
  pdf  <- dnorm(eta)
  b    <- coef(model)
  mm   <- model.matrix(model)

  purrr::map_dfr(colnames(mm)[-1], function(term) {
    if (!term %in% names(b)) return(NULL)
    col_vals <- mm[, term]
    if (length(unique(col_vals)) == 2) {
      # Binary dummy: exact AME via prediction difference
      d1 <- d0 <- data
      # identify the column in data that maps to this model matrix column
      # For factor levels the column values in mm are 0/1
      p1 <- pnorm(eta + b[term] * (1 - col_vals))   # counterfactual: all = 1
      p0 <- pnorm(eta - b[term] * col_vals)          # counterfactual: all = 0
      ame <- mean(p1 - p0)
    } else {
      ame <- mean(pdf * b[term])
    }
    dplyr::tibble(term = term, ame = ame)
  })
}

ame_main <- compute_ame_probit(probit_main, eff1_m) |>
  dplyr::rename(ame_main = ame)
ame_sec  <- compute_ame_probit(probit_ot,   eff1_m) |>
  dplyr::rename(ame_sec  = ame)

ame_tbl <- dplyr::left_join(ame_main, ame_sec, by = "term") |>
  dplyr::mutate(
    Variable = dplyr::recode(term,
      "age_resp"                = "Age",
      "age2"                    = "Age$^2$",
      "educ_cat2_Primary"       = "Primary",
      "educ_cat3_Lower_sec"     = "Lower secondary (ESO)",
      "educ_cat4_Vocational"    = "Vocational",
      "educ_cat5_Upper_sec"     = "Upper secondary (Bachillerato)",
      "educ_cat6_Post_sec"      = "Post-secondary",
      "educ_cat7_University"    = "University",
      "educ_cat8_Other"         = "Other",
      "labour_catSelf_employed" = "Self-employed",
      "labour_catUnemployed"    = "Unemployed",
      "labour_catRetired"       = "Retired",
      "labour_catInactive"      = "Inactive",
      "hhsize"                  = "Household size",
      "log_inc"                 = "Log income"
    ),
    `Main residence` = formatC(ame_main, format = "f", digits = 4),
    `Secondary home`  = formatC(ame_sec,  format = "f", digits = 4)
  ) |>
  dplyr::select(Variable, `Main residence`, `Secondary home`)

# ame_tbl rows: 1-2 = age/age2, 3-9 = 7 educ cats, 10-13 = 4 labour cats, 14-15 = hhsize/log_inc

tbl_ame_tex <- kableExtra::kbl(
  ame_tbl,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  align     = c("l", "r", "r"),
  caption   = "Average marginal effects from probit homeownership models (EFF 2017, imp~=~1)",
  label     = "homeownership_ame"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::pack_rows("Labour status (ref.: Employed)", 10, 13,
                        bold = FALSE, italic = TRUE, escape = FALSE,
                        indent = FALSE, latex_gap_space = "0.3em") |>
  kableExtra::pack_rows("Education (ref.: Illiterate)", 3, 9,
                        bold = FALSE, italic = TRUE, escape = FALSE,
                        indent = FALSE, latex_gap_space = "0.3em") |>
  kableExtra::footnote(
    general = paste0(
      "AME computed as $\\\\overline{\\\\phi(\\\\hat{\\\\eta}_i)\\\\,\\\\hat{\\\\beta}_k}$ ",
      "for continuous variables and as $\\\\overline{\\\\Phi(\\\\hat{\\\\eta}_i\\\\mid d=1) - ",
      "\\\\Phi(\\\\hat{\\\\eta}_i\\\\mid d=0)}$ for binary indicators. ",
      "HC1 robust standard errors not reported here; see Tables~\\\\ref{tab:lpm_only} and~\\\\ref{tab:probit_only}."
    ),
    escape        = FALSE,
    general_title = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_ame_tex, file.path(TAB_DIR, "Q3b_homeownership_ame.tex"))
cat("  Table saved: Q3b_homeownership_ame.tex\n")

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

# =============================================================================
# Q3e: Full vs grouped education specification comparison
# =============================================================================
cat("\n===== Q3e: Full vs grouped education comparison =====\n")

# Full model: all 14 raw educ_resp codes as separate dummies
eff1_full <- eff1 |>
  dplyr::mutate(
    educ_full  = factor(educ_resp),
    labour_cat = make_labour_cat(emp_resp, self_resp, une_resp, ret_resp),
    age2       = age_resp^2,
    log_inc    = log(pmax(hh_inc, 1))
  ) |>
  tidyr::drop_na(own, age_resp, educ_full, labour_cat, hhsize, log_inc)

lpm_full <- lm(own ~ age_resp + age2 + educ_full + labour_cat + hhsize + log_inc,
               data = eff1_full, weights = facine3)

cat("\nQ3e – Full model (14 raw educ codes), HC1 robust SEs:\n")
print(lmtest::coeftest(lpm_full, vcov. = sandwich::vcovHC(lpm_full, type = "HC1")))

cat("\nQ3e – Grouped model (8 categories), HC1 robust SEs:\n")
print(lmtest::coeftest(lpm_main, vcov. = sandwich::vcovHC(lpm_main, type = "HC1")))

cat(sprintf("\n  R2: full = %.4f | grouped = %.4f\n",
            summary(lpm_full)$r.squared,
            summary(lpm_main)$r.squared))

# Combined coef_map covering all coefficients from both models (used for the
# full-spec standalone table below).
coef_map_q3e <- c(
  "(Intercept)"             = "Intercept",
  "age_resp"                = "Age",
  "age2"                    = "Age$^2$",
  "educ_full2"              = "\\hspace{6pt} Primary",
  "educ_full3"              = "\\hspace{6pt} Voc.\\ (< ESO)",
  "educ_full4"              = "\\hspace{6pt} Lower sec.\\ (ESO)",
  "educ_full5"              = "\\hspace{6pt} Voc.\\ (ESO)",
  "educ_full6"              = "\\hspace{6pt} Bachillerato",
  "educ_full7"              = "\\hspace{6pt} Voc.\\ (Bachillerato)",
  "educ_full8"              = "\\hspace{6pt} Post-sec.\\ vocational",
  "educ_full9"              = "\\hspace{6pt} Post-sec.\\ (2+ yr)",
  "educ_full11"             = "\\hspace{6pt} Master",
  "educ_full12"             = "\\hspace{6pt} PhD",
  "educ_full97"             = "\\hspace{6pt} Other (raw)",
  "educ_full1001"           = "\\hspace{6pt} Diplomado",
  "educ_full1002"           = "\\hspace{6pt} Licenciado",
  "labour_catSelf_employed" = "\\hspace{6pt} Self-employed",
  "labour_catUnemployed"    = "\\hspace{6pt} Unemployed",
  "labour_catRetired"       = "\\hspace{6pt} Retired",
  "labour_catInactive"      = "\\hspace{6pt} Inactive",
  "hhsize"                  = "Household size",
  "log_inc"                 = "Log income"
)

# -- Full model standalone table ----------------------------------------------
coef_map_full_only <- c(
  "(Intercept)"             = "Intercept",
  "age_resp"                = "Age",
  "age2"                    = "Age$^2$",
  "educ_full2"              = "\\hspace{6pt} Primary",
  "educ_full3"              = "\\hspace{6pt} Voc.\\ (< ESO)",
  "educ_full4"              = "\\hspace{6pt} Lower sec.\\ (ESO)",
  "educ_full5"              = "\\hspace{6pt} Voc.\\ (ESO)",
  "educ_full6"              = "\\hspace{6pt} Bachillerato",
  "educ_full7"              = "\\hspace{6pt} Voc.\\ (Bachillerato)",
  "educ_full8"              = "\\hspace{6pt} Post-sec.\\ vocational",
  "educ_full9"              = "\\hspace{6pt} Post-sec.\\ (2+ yr)",
  "educ_full11"             = "\\hspace{6pt} Master",
  "educ_full12"             = "\\hspace{6pt} PhD",
  "educ_full97"             = "\\hspace{6pt} Other",
  "educ_full1001"           = "\\hspace{6pt} Diplomado",
  "educ_full1002"           = "\\hspace{6pt} Licenciado",
  "labour_catSelf_employed" = "\\hspace{6pt} Self-employed",
  "labour_catUnemployed"    = "\\hspace{6pt} Unemployed",
  "labour_catRetired"       = "\\hspace{6pt} Retired",
  "labour_catInactive"      = "\\hspace{6pt} Inactive",
  "hhsize"                  = "Household size",
  "log_inc"                 = "Log income"
)

tbl_full <- modelsummary::modelsummary(
  list("Main residence (full education spec.)" = lpm_full),
  coef_map  = coef_map_full_only,
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt       = fmt_smart(3),
  output    = "latex",
  booktabs  = TRUE,
  title     = "Weighted LPM: main-residence ownership, full education specification (EFF 2017, imp~=~1)",
  label     = "lpm_full_educ",
  gof_map   = list(
    list(raw = "nobs",      clean = "$N$",   fmt = "%d"),
    list(raw = "r.squared", clean = "$R^2$", fmt = "%.4f")
  ),
  escape = FALSE
) |>
  add_ref_groups(coef_map_full_only, list(
    "Education (ref.: Illiterate)"   = c("educ_full2", "educ_full1002"),
    "Labour status (ref.: Employed)" = c("labour_catSelf_employed", "labour_catInactive")
  )) |>
  kableExtra::footnote(
    general = paste(
      "HC1 robust SEs in parentheses. Survey weights (\\\\texttt{facine3}) applied.",
      "All 14 raw \\\\texttt{educ\\\\_resp} codes included as dummies (ref.\\\\ = Illiterate, code 1).",
      "Log income $= \\\\ln(\\\\max(\\\\text{hh\\\\_inc},\\\\,1))$."
    ),
    escape         = FALSE,
    general_title  = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_full, file.path(TAB_DIR, "Q3e_lpm_full_educ.tex"))
cat("  Table saved: Q3e_lpm_full_educ.tex\n")
