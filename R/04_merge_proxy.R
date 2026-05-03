# =============================================================================
# 04_merge_proxy.R – Q4: Merge + proxy for internationally exposed education
# =============================================================================
cat("\n===== Q4: Merge + proxy variable =====\n")

# ---- Q4a: Merge eff1 with secondlang_prob on educ_resp ----------------------
# sl_raw has educ_resp values: 1,2,3,4,6,8,9,11,12,97,1001,1002  (12 rows)
# eff1 has educ_resp values:   1,2,3,4,5,6,7,8,9,11,12,97,1001,1002
# Missing from sl_raw: educ_resp = 5 and educ_resp = 7
# => Left join produces NAs for households with educ_resp ∈ {5, 7}

eff1_merged <- eff1 |>
  dplyr::left_join(sl_raw, by = "educ_resp")

# Diagnose
n_miss <- sum(is.na(eff1_merged$p_knows_second_lang))
educ_miss_vals <- eff1_merged |>
  dplyr::filter(is.na(p_knows_second_lang)) |>
  dplyr::distinct(educ_resp) |>
  dplyr::pull(educ_resp) |>
  sort()

cat(sprintf(
  "\nQ4a – Merge diagnosis:\n  Not a perfect merge.\n  %d households have missing p_knows_second_lang (educ_resp = %s).\n",
  n_miss, paste(educ_miss_vals, collapse = ", ")
))
cat("  Reason: secondlang_prob does not contain rows for educ_resp = 5 and 7.\n")
cat("  These are intermediate vocational tracks:\n")
cat("    5 = Vocational training requiring ESO (between levels 4 and 6)\n")
cat("    7 = Vocational training requiring Bachillerato (between levels 6 and 8)\n")

# Imputation: linear midpoint interpolation between adjacent levels.
# - educ = 5 sits between educ 4 (0.269) and educ 6 (0.378) => (0.269+0.378)/2
# - educ = 7 sits between educ 6 (0.378) and educ 8 (0.500) => (0.378+0.500)/2
# Justification: vocational tracks with ESO/bachillerato requirements imply
# language exposure intermediate to the purely academic tracks above and below.

imp_vals <- sl_raw |>
  dplyr::filter(educ_resp %in% c(4, 6, 8)) |>
  dplyr::arrange(educ_resp) |>
  dplyr::pull(p_knows_second_lang)

p_educ5 <- (imp_vals[1] + imp_vals[2]) / 2   # avg of 4 (0.269) and 6 (0.378)
p_educ7 <- (imp_vals[2] + imp_vals[3]) / 2   # avg of 6 (0.378) and 8 (0.500)

cat(sprintf("  Imputed values:  educ=5 -> %.4f | educ=7 -> %.4f\n",
            p_educ5, p_educ7))

eff1_merged <- eff1_merged |>
  dplyr::mutate(
    p_knows_second_lang = dplyr::case_when(
      educ_resp == 5 ~ p_educ5,
      educ_resp == 7 ~ p_educ7,
      TRUE           ~ p_knows_second_lang
    )
  )

# Summary table of p_knows_second_lang by education level after imputation
sl_table <- eff1_merged |>
  dplyr::distinct(educ_resp, p_knows_second_lang) |>
  dplyr::arrange(educ_resp) |>
  dplyr::mutate(
    imputed = educ_resp %in% c(5, 7),
    educ_label = dplyr::case_when(
      educ_resp == 1    ~ "Illiterate",
      educ_resp == 2    ~ "Primary",
      educ_resp == 3    ~ "Vocational (< ESO)",
      educ_resp == 4    ~ "Lower secondary (ESO)",
      educ_resp == 5    ~ "Vocational (ESO) [imputed]",
      educ_resp == 6    ~ "Upper secondary (Bachillerato)",
      educ_resp == 7    ~ "Vocational (Bachillerato) [imputed]",
      educ_resp == 8    ~ "Specialised vocational / Higher",
      educ_resp == 9    ~ "Post-secondary (2+ yr, Bachillerato)",
      educ_resp == 11   ~ "Master's degree",
      educ_resp == 12   ~ "PhD",
      educ_resp == 97   ~ "Other",
      educ_resp == 1001 ~ "Short-cycle university (Diplomado)",
      educ_resp == 1002 ~ "Long-cycle university (Licenciado)",
      TRUE              ~ as.character(educ_resp)
    )
  )

cat("\nQ4a - p_knows_second_lang by education level (after imputation):\n")
print(dplyr::select(sl_table, educ_resp, educ_label, p_knows_second_lang, imputed),
      n = Inf)

# ---- Q4a: LaTeX export — observed + interpolated p, with Δ and Δ² ---------
# (Was previously authored by hand from the Python exploratory notebook;
#  this block reproduces the same table directly from sl_table.)

q4a_labels <- c(
  "1"    = "Illiterate",
  "2"    = "Primary",
  "3"    = "Vocational (< ESO)",
  "4"    = "Lower secondary (ESO)",
  "5"    = "Vocational (ESO entry) \\textit{[interpolated]}",
  "6"    = "Upper secondary (Bachillerato)",
  "7"    = "Vocational (Bachillerato entry) \\textit{[interpolated]}",
  "8"    = "Specialised vocational / Higher",
  "9"    = "Post-secondary (2+ yr, Bachillerato)",
  "11"   = "Master's degree",
  "12"   = "PhD",
  "97"   = "Other",
  "1001" = "Short-cycle university (Diplomado)",
  "1002" = "Long-cycle university (Licenciado)"
)

q4a_tbl <- sl_table |>
  dplyr::arrange(match(as.character(educ_resp), names(q4a_labels))) |>
  dplyr::transmute(
    educ_resp,
    label    = q4a_labels[as.character(educ_resp)],
    p_hat    = p_knows_second_lang,
    imputed,
    delta    = p_hat - dplyr::lag(p_hat),
    delta2   = delta - dplyr::lag(delta)
  )

fmt_signed <- function(x, digits = 4) {
  ifelse(is.na(x), "---",
         ifelse(x >= 0,
                paste0("+", formatC(x, format = "f", digits = digits)),
                formatC(x, format = "f", digits = digits)))
}

q4a_rows <- vapply(seq_len(nrow(q4a_tbl)), function(i) {
  r <- q4a_tbl[i, ]
  sprintf(
    "  %s & %s & %.4f & %s & %s & %s \\\\",
    format(r$educ_resp, width = 4),
    r$label,
    r$p_hat,
    if (isTRUE(r$imputed)) "\\checkmark" else "",
    fmt_signed(r$delta),
    fmt_signed(r$delta2)
  )
}, character(1))

q4a_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{P(knows second language) by education level --- observed and interpolated values}",
  "\\label{tab:secondlang_interp}",
  "\\begin{tabular}{clcccc}",
  "\\toprule",
  "\\textit{educ\\_resp} & Education level & $\\hat{p}$ & Interpolated & $\\Delta$ & $\\Delta^2$ \\\\",
  "\\midrule",
  q4a_rows,
  "\\bottomrule",
  "\\multicolumn{6}{l}{\\rule{0pt}{1em}\\textit{Notes:} Interpolated values use the midpoint between adjacent education levels.} \\\\",
  "\\multicolumn{6}{l}{\\rule{0pt}{1em}$\\Delta$ = first difference of $\\hat{p}$ vs preceding code; $\\Delta^2$ = change in $\\Delta$ (acceleration).} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(q4a_tex, file.path(TAB_DIR, "Q4a_secondlang_interp.tex"))
cat("  Table saved: Q4a_secondlang_interp.tex\n")

# ---- Q4b: Proxy for internationally exposed higher education ----------------
# high_educ = 1 if college-level education or above
# (educ_resp in {8, 9, 1001, 1002, 11, 12}: post-secondary / university)
# proxy = p_knows_second_lang * high_educ
#   -> 0 for everyone below college
#   -> p_knows_second_lang for college-educated individuals
#
# Validity discussion: the proxy captures two complementary dimensions:
#   (i)  having high education (college attendance, often bilingual programs)
#   (ii) residing in an environment with higher second-language prevalence
# Limitation: it measures probability at the education-group level (not individual)
# and conflates domestic bilingual programs with study abroad.

eff1_proxy <- eff1_merged |>
  dplyr::mutate(
    high_educ = as.integer(educ_resp %in% c(8, 9, 1001, 1002, 11, 12)),
    proxy     = p_knows_second_lang * high_educ,
    # Wealth quintiles based on totnet
    wealth_q  = cut(
      totnet,
      breaks = c(-Inf,
                 wt_quantile(totnet, facine3, probs = c(0.2, 0.4, 0.6, 0.8)),
                 Inf),
      labels = paste0("Q", 1:5),
      right  = TRUE
    )
  )

# Bar chart: average proxy by wealth quintile
proxy_by_q <- eff1_proxy |>
  dplyr::group_by(wealth_q) |>
  dplyr::summarise(avg_proxy = wt_mean(proxy, facine3), .groups = "drop")

p4b <- ggplot2::ggplot(proxy_by_q,
                       ggplot2::aes(x = wealth_q, y = avg_proxy,
                                    fill = wealth_q)) +
  ggplot2::geom_col(width = 0.6, colour = "white") +
  ggplot2::scale_fill_brewer(palette = "Blues", direction = 1) +
  ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  ggplot2::labs(
    x       = "Net wealth quintile",
    y       = "Average proxy value",
    title   = "Q4b – Average proxy by net wealth quintile",
    caption = "Proxy = P(2nd lang) × I(high educ) | Source: EFF 2017 (imp = 1)",
    fill    = "Quintile"
  ) +
  theme_eff() +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

save_plot(p4b, "Q4b_proxy_by_wealth_quintile.png", width = 7, height = 5)

# ---- Q4c: Weighted LPM for business ownership + AME by wealth quintile ------
eff1_proxy_m <- eff1_proxy |>
  dplyr::mutate(
    labour_cat = make_labour_cat(emp_resp, self_resp, une_resp, ret_resp),
    age2       = age_resp^2
  ) |>
  tidyr::drop_na(neg, proxy, age_resp, labour_cat, hhsize, wealth_q)

lpm_neg <- lm(
  neg ~ proxy * wealth_q + age_resp + age2 + labour_cat + hhsize,
  data    = eff1_proxy_m,
  weights = facine3
)

lpm_neg_rse <- lmtest::coeftest(lpm_neg,
                                  vcov = sandwich::vcovHC(lpm_neg, type = "HC1"))
cat("\nQ4c – Weighted LPM: business ownership (neg)\n")
print(lpm_neg_rse)

# Quintile-specific AMEs via proxy × wealth_q interactions.
# AME_Q1 = β_proxy  (reference quintile)
# AME_Qk = β_proxy + β_{proxy:wealth_qQk}  for k = 2..5
# SEs via delta method: Var(β_a + β_b) = Var(β_a) + Var(β_b) + 2 Cov(β_a, β_b)

vcov_hc1  <- sandwich::vcovHC(lpm_neg, type = "HC1")
coefs_lpm <- coef(lpm_neg)

quint_labels <- paste0("Q", 1:5)

ame_q <- vapply(quint_labels, function(q) {
  if (q == "Q1") coefs_lpm["proxy"]
  else coefs_lpm["proxy"] + coefs_lpm[paste0("proxy:wealth_q", q)]
}, numeric(1))

ame_se_q <- vapply(quint_labels, function(q) {
  if (q == "Q1") {
    sqrt(vcov_hc1["proxy", "proxy"])
  } else {
    nm <- paste0("proxy:wealth_q", q)
    sqrt(vcov_hc1["proxy","proxy"] + vcov_hc1[nm,nm] + 2*vcov_hc1["proxy",nm])
  }
}, numeric(1))

pred_by_q <- eff1_proxy_m |>
  dplyr::mutate(y_hat = fitted(lpm_neg)) |>
  dplyr::group_by(wealth_q) |>
  dplyr::summarise(
    n_hh        = dplyr::n(),
    avg_pred_pr = wt_mean(y_hat, facine3),
    .groups     = "drop"
  )

ame_by_q <- pred_by_q |>
  dplyr::mutate(
    ame_pp    = ame_q * 100,
    ame_se_pp = ame_se_q * 100,
    t_stat    = ame_q / ame_se_q,
    p_val     = 2 * pt(-abs(t_stat), df = nrow(eff1_proxy_m) - length(coef(lpm_neg)))
  )

cat("\nQ4c – AME of proxy on business ownership by wealth quintile (interaction LPM):\n")
print(ame_by_q |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), n = Inf)
cat("  AME_Q1 = beta_proxy; AME_Qk = beta_proxy + beta_{proxy:Qk} (delta-method SEs).\n")

# (LPM-only AME LaTeX export removed: Q4c_ame_quintile_probit.tex below combines
#  the LPM AME with the probit AME in a single, canonical table.)

# ---- Q4c: LaTeX table for Overleaf ------------------------------------------
coef_map_q4 <- c(
  "(Intercept)"               = "Intercept",
  "age_resp"                  = "Age",
  "age2"                      = "Age$^2$",
  "proxy"                     = "Education proxy",
  "wealth_qQ2"                = "\\hspace{6pt} Q2",
  "wealth_qQ3"                = "\\hspace{6pt} Q3",
  "wealth_qQ4"                = "\\hspace{6pt} Q4",
  "wealth_qQ5"                = "\\hspace{6pt} Q5",
  "proxy:wealth_qQ2"          = "\\hspace{6pt} Proxy $\\times$ Q2",
  "proxy:wealth_qQ3"          = "\\hspace{6pt} Proxy $\\times$ Q3",
  "proxy:wealth_qQ4"          = "\\hspace{6pt} Proxy $\\times$ Q4",
  "proxy:wealth_qQ5"          = "\\hspace{6pt} Proxy $\\times$ Q5",
  "labour_catSelf_employed"   = "\\hspace{6pt} Self-employed",
  "labour_catUnemployed"      = "\\hspace{6pt} Unemployed",
  "labour_catRetired"         = "\\hspace{6pt} Retired",
  "labour_catInactive"        = "\\hspace{6pt} Inactive",
  "hhsize"                    = "Household size"
)

options(modelsummary_factory_latex = "kableExtra")

tbl_lpm_neg <- modelsummary::modelsummary(
  list("Business ownership" = lpm_neg),
  coef_map  = coef_map_q4,
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt       = fmt_smart(4),
  output    = "latex",
  booktabs  = TRUE,
  title     = "Weighted LPM with interactions: business ownership (EFF 2017, imp~=~1)",
  label     = "lpm_business",
  gof_map   = list(
    list(raw = "nobs",      clean = "$N$",   fmt = "%d"),
    list(raw = "r.squared", clean = "$R^2$", fmt = "%.4f")
  ),
  escape = FALSE
) |>
  add_ref_groups(coef_map_q4, list(
    "Wealth quintile (ref.: Q1)"     = c("wealth_qQ2", "wealth_qQ5"),
    "Proxy $\\times$ wealth quintile" = c("proxy:wealth_qQ2", "proxy:wealth_qQ5"),
    "Labour status (ref.: Employed)" = c("labour_catSelf_employed", "labour_catInactive")
  )) |>
  kableExtra::footnote(
    general = paste(
      "HC1 robust standard errors in parentheses.",
      "Survey weights (\\\\texttt{facine3}) applied.",
      "Proxy $= \\\\hat{p}(\\\\text{2nd lang} \\\\mid \\\\text{educ}) \\\\times \\\\mathbb{1}(\\\\text{college+})$."
    ),
    escape         = FALSE,
    general_title  = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_lpm_neg, file.path(TAB_DIR, "Q4c_lpm_business.tex"))
cat("  Table saved: Q4c_lpm_business.tex\n")

# ---- Q4c: Baseline LPM (no proxy x wealth interactions) --------------------
# Same controls as the interaction model, with proxy entering linearly only.
lpm_neg_baseline <- lm(
  neg ~ proxy + wealth_q + age_resp + age2 + labour_cat + hhsize,
  data    = eff1_proxy_m,
  weights = facine3
)

cat("\nQ4c – Baseline weighted LPM (no proxy x wealth interactions):\n")
print(lmtest::coeftest(lpm_neg_baseline,
                        vcov = sandwich::vcovHC(lpm_neg_baseline, type = "HC1")))

coef_map_q4_baseline <- c(
  "(Intercept)"               = "Intercept",
  "age_resp"                  = "Age",
  "age2"                      = "Age$^2$",
  "proxy"                     = "Education proxy",
  "wealth_qQ2"                = "\\hspace{6pt} Q2",
  "wealth_qQ3"                = "\\hspace{6pt} Q3",
  "wealth_qQ4"                = "\\hspace{6pt} Q4",
  "wealth_qQ5"                = "\\hspace{6pt} Q5",
  "labour_catSelf_employed"   = "\\hspace{6pt} Self-employed",
  "labour_catUnemployed"      = "\\hspace{6pt} Unemployed",
  "labour_catRetired"         = "\\hspace{6pt} Retired",
  "labour_catInactive"        = "\\hspace{6pt} Inactive",
  "hhsize"                    = "Household size"
)

tbl_lpm_neg_baseline <- modelsummary::modelsummary(
  list("Business ownership" = lpm_neg_baseline),
  coef_map  = coef_map_q4_baseline,
  vcov      = "HC1",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt       = fmt_smart(4),
  output    = "latex",
  booktabs  = TRUE,
  title     = "Weighted LPM (baseline): business ownership (EFF 2017, imp~=~1)",
  label     = "lpm_business_baseline",
  gof_map   = list(
    list(raw = "nobs",      clean = "$N$",   fmt = "%d"),
    list(raw = "r.squared", clean = "$R^2$", fmt = "%.4f")
  ),
  escape = FALSE
) |>
  add_ref_groups(coef_map_q4_baseline, list(
    "Wealth quintile (ref.: Q1)"     = c("wealth_qQ2", "wealth_qQ5"),
    "Labour status (ref.: Employed)" = c("labour_catSelf_employed", "labour_catInactive")
  )) |>
  kableExtra::footnote(
    general = paste(
      "HC1 robust standard errors in parentheses.",
      "Survey weights (\\\\texttt{facine3}) applied.",
      "Proxy $= \\\\hat{p}(\\\\text{2nd lang} \\\\mid \\\\text{educ}) \\\\times \\\\mathbb{1}(\\\\text{college+})$.",
      "No proxy $\\\\times$ wealth quintile interactions."
    ),
    escape         = FALSE,
    general_title  = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_lpm_neg_baseline,
           file.path(TAB_DIR, "Q4c_lpm_business_baseline.tex"))
cat("  Table saved: Q4c_lpm_business_baseline.tex\n")

# =============================================================================
# Q4c (extension): Probit AME by wealth quintile
# -----------------------------------------------------------------------------
# In the LPM the marginal effect of the proxy is constant by construction.
# To address this directly we re-estimate the same specification as a probit
# and report quintile-specific AMEs:
#     AME_q = mean_{i in q}[ phi(eta_i) * beta_proxy ]
# This shows whether the proxy's effect varies across the wealth distribution.
# =============================================================================
cat("\nQ4c (extension) – Probit AME by wealth quintile\n")

w_mean_q4 <- mean(eff1_proxy_m$facine3)
probit_neg <- glm(
  neg ~ proxy + age_resp + age2 + labour_cat + hhsize,
  data    = eff1_proxy_m,
  weights = facine3 / w_mean_q4,
  family  = binomial(link = "probit")
)

eta_neg          <- predict(probit_neg, type = "link")
pdf_neg          <- dnorm(eta_neg)
beta_proxy_prob  <- coef(probit_neg)[["proxy"]]

ame_by_q_probit <- eff1_proxy_m |>
  dplyr::mutate(
    pdf_i  = pdf_neg,
    p_hat  = predict(probit_neg, type = "response")
  ) |>
  dplyr::group_by(wealth_q) |>
  dplyr::summarise(
    n_hh        = dplyr::n(),
    avg_pred_pr = wt_mean(p_hat, facine3),
    ame_proxy   = wt_mean(pdf_i * beta_proxy_prob, facine3),
    .groups     = "drop"
  )

cat("  Probit AME of proxy by wealth quintile:\n")
print(ame_by_q_probit, digits = 4)

# LaTeX table — combined LPM (constant) vs probit (quintile-specific) AME
ame_combined <- ame_by_q_probit |>
  dplyr::left_join(
    dplyr::select(ame_by_q, wealth_q, ame_pp),
    by = "wealth_q"
  ) |>
  dplyr::mutate(
    wealth_q      = as.character(wealth_q),
    ame_lpm_pp    = ame_pp,
    ame_probit_pp = ame_proxy * 100
  ) |>
  dplyr::select(wealth_q, n_hh, avg_pred_pr, ame_lpm_pp, ame_probit_pp)

tbl_ame_combined <- kableExtra::kbl(
  ame_combined,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  digits    = c(0, 0, 3, 2, 2),
  align     = "crrrr",
  col.names = c("Wealth quintile", "$N$",
                "Mean $\\hat{P}(\\text{neg}=1)$",
                "AME LPM (pp)", "AME Probit (pp)"),
  caption   = paste0(
    "Average marginal effect of the education proxy on business ownership ",
    "by net wealth quintile --- LPM vs probit (EFF 2017, imp~=~1)"
  ),
  label     = "ame_quintile_probit"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general = paste0(
      "LPM AME varies by quintile via proxy $\\\\times$ wealth quintile interactions (delta-method SEs). ",
      "Probit AME computed as $\\\\overline{\\\\phi(\\\\hat{\\\\eta}_i)\\\\,\\\\hat{\\\\beta}_{\\\\text{proxy}}}$ ",
      "within each quintile. Survey weights (\\\\texttt{facine3}) applied; ",
      "HC1 robust SEs used for both models."
    ),
    escape        = FALSE,
    general_title = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_ame_combined, file.path(TAB_DIR, "Q4c_ame_quintile_probit.tex"))
cat("  Table saved: Q4c_ame_quintile_probit.tex\n")

# =============================================================================
# Q4c (extension): Collinearity diagnostics — self-employed vs business owner
# -----------------------------------------------------------------------------
# Motivation: the LPM in Table 11 reports R^2 = 0.49, but inspection of the
# coefficients suggests that 'self_employed' (labour_cat) is doing nearly all
# the work (beta = 0.70). We document the strong overlap between the
# self-employment dummy and the business-ownership outcome (`neg`) using:
#   1. weighted cross-tabulation
#   2. weighted Pearson correlation
#   3. variance inflation factors (VIF)
#   4. partial R^2 — refit dropping self-employment and report the R^2 drop
# =============================================================================
cat("\nQ4c (extension) – Collinearity diagnostics: self-employed vs neg\n")

dx <- eff1_proxy_m |>
  dplyr::mutate(self_emp = as.integer(labour_cat == "Self_employed"))

# 1. Weighted cross-tabulation
xtab <- dx |>
  dplyr::group_by(self_emp, neg) |>
  dplyr::summarise(w = sum(facine3), .groups = "drop") |>
  dplyr::mutate(share = w / sum(w))

cat("  Weighted joint distribution of (self_emp, neg):\n")
print(xtab, digits = 4)

# Conditional shares
p_neg_given_se   <- with(dx, wt_mean(neg[self_emp == 1], facine3[self_emp == 1]))
p_neg_given_emp  <- with(dx, wt_mean(neg[self_emp == 0], facine3[self_emp == 0]))
p_se_given_neg   <- with(dx, wt_mean(self_emp[neg == 1], facine3[neg == 1]))
cat(sprintf(
  "  P(neg = 1 | self_emp = 1) = %.3f\n  P(neg = 1 | self_emp = 0) = %.3f\n  P(self_emp = 1 | neg = 1) = %.3f\n",
  p_neg_given_se, p_neg_given_emp, p_se_given_neg
))

# 2. Weighted Pearson correlation
r_se_neg <- wt_cor(dx$self_emp, dx$neg, dx$facine3)
cat(sprintf("  Weighted Pearson correlation r(self_emp, neg) = %.4f\n", r_se_neg))

# 3. Variance inflation factors (multicollinearity among regressors)
#    VIF > 10 conventionally indicates serious multicollinearity.
vif_vals <- tryCatch(
  car::vif(lpm_neg),
  error = function(e) NULL
)
if (!is.null(vif_vals)) {
  cat("  VIFs for lpm_neg regressors:\n")
  print(round(vif_vals, 3))
} else {
  cat("  (Skipping VIF — package 'car' not installed.)\n")
}

# 4. Refit LPM dropping self-employment status to isolate its explanatory power.
#    Self-employment is removed from the labour_cat factor: collapse it into
#    "Employed" so the labour categorisation still spans the sample.
#    The comparison is apples-to-apples against the baseline LPM
#    (lpm_neg_baseline: proxy + wealth_q + age + age2 + labour_cat + hhsize).
dx_no_se <- dx |>
  dplyr::mutate(
    labour_cat_nose = forcats::fct_recode(labour_cat, "Employed" = "Self_employed")
  )

lpm_neg_nose <- lm(
  neg ~ proxy + wealth_q + age_resp + age2 + labour_cat_nose + hhsize,
  data    = dx_no_se,
  weights = facine3
)

r2_full <- summary(lpm_neg_baseline)$r.squared
r2_nose <- summary(lpm_neg_nose)$r.squared
cat(sprintf(
  "  R^2 with self-employment    : %.4f\n  R^2 without self-employment : %.4f\n  Drop in R^2 attributable to self_emp: %.4f (%.1f%% of full R^2)\n",
  r2_full, r2_nose, r2_full - r2_nose, 100 * (r2_full - r2_nose) / r2_full
))

# t-test on the weighted Pearson correlation
n_collin   <- nrow(dx)
t_se_neg   <- r_se_neg * sqrt((n_collin - 2) / (1 - r_se_neg^2))
p_se_neg   <- 2 * pt(-abs(t_se_neg), df = n_collin - 2)

cat(sprintf("  t-test on r(self_emp, neg): t(%d) = %.3f, p = %.4f\n",
            n_collin - 2, t_se_neg, p_se_neg))

# Save diagnostics as a small LaTeX table
collin_tbl <- dplyr::tibble(
  Statistic = c(
    "Weighted Pearson $r(\\text{self\\_emp},\\,\\text{neg})$",
    "\\quad $t$-statistic ($H_0\\colon r = 0$)",
    "\\quad $p$-value",
    "$R^2$ with self-employment",
    "$R^2$ without self-employment",
    "Drop in $R^2$ (share of full $R^2$)"
  ),
  Value = c(
    sprintf("%.4f", r_se_neg),
    sprintf("%.3f", t_se_neg),
    sprintf("%.4f", p_se_neg),
    sprintf("%.4f", r2_full),
    sprintf("%.4f", r2_nose),
    sprintf("%.4f (%.1f\\%%)",
            r2_full - r2_nose, 100 * (r2_full - r2_nose) / r2_full)
  )
)

tbl_collin <- kableExtra::kbl(
  collin_tbl,
  format   = "latex",
  booktabs = TRUE,
  escape   = FALSE,
  align    = "lr",
  caption  = paste0(
    "Collinearity diagnostics: self-employment status and business ownership ",
    "(EFF 2017, imp~=~1)"
  ),
  label    = "collin_self_emp"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general = paste0(
      "$t$-test uses the standard Pearson formula $t = r\\\\sqrt{(n-2)/(1-r^2)}$ ",
      "with $df = n - 2 = ", n_collin - 2, "$. ",
      "The drop in $R^2$ compares the full LPM (with self-employment in labour status) ",
      "against a restricted model where self-employed households are reclassified as employed. ",
      "Survey weights (\\\\texttt{facine3}) applied throughout."
    ),
    escape        = FALSE,
    general_title = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_collin, file.path(TAB_DIR, "Q4c_collinearity_self_emp.tex"))
cat("  Table saved: Q4c_collinearity_self_emp.tex\n")
