# =============================================================================
# 01_implicates.R – Q1: Multiple imputations (implicates) and weights
# =============================================================================
cat("\n===== Q1: Implicates and weights =====\n")

# ---- Q1a: Weight transformation when stacking implicates --------------------
# Each household appears M_IMP = 5 times (once per implicate).
# If the raw weight facine3 represents the number of households in the
# population that each surveyed household represents, stacking 5 implicates
# means each household contributes 5 rows.  To preserve the population total
# (sum of weights = true population size), divide each weight by M_IMP = 5.

eff_stacked <- eff_raw |>
  dplyr::mutate(w_adj = facine3 / M_IMP)

wm_stacked <- wt_mean(eff_stacked$totnet, eff_stacked$w_adj)

cat(sprintf(
  "\nQ1a – Weighted average total net wealth (w / %d): %s EUR\n",
  M_IMP, formatC(wm_stacked, format = "f", big.mark = ",", digits = 2)
))
cat(sprintf("      Sum of adjusted weights: %s  (= approx. population size)\n",
            formatC(sum(eff_stacked$w_adj), format = "f", big.mark = ",", digits = 0)))

# ---- Q1b: Error from pooling without weight adjustment ----------------------
# Error 1 – Weighted TOTALS (e.g. aggregate household wealth) are inflated x5.
# Error 2 – The standard error of the weighted mean is mis-estimated because
#            the survey package sees 5·N 'observations'; treating the same
#            household's 5 imputed values as independent observations also
#            introduces between-imputation variance as if it were additional
#            between-household variance.
# Note: the weighted MEAN formula normalises by sum(w), so the point estimate
#       is numerically unchanged; the problem lies in totals and inference.

wm_wrong  <- wt_mean(eff_stacked$totnet, eff_stacked$facine3)  # unchanged
tot_correct <- sum(eff_stacked$w_adj  * eff_stacked$totnet)
tot_wrong   <- sum(eff_stacked$facine3 * eff_stacked$totnet)

# SE comparison using svydesign
des_correct <- survey::svydesign(ids = ~1, weights = ~w_adj,   data = eff_stacked)
des_wrong   <- survey::svydesign(ids = ~1, weights = ~facine3, data = eff_stacked)
des_imp1    <- survey::svydesign(ids = ~1, weights = ~facine3,
                                 data = dplyr::filter(eff_raw, imp == 1))

se_correct <- as.numeric(survey::SE(survey::svymean(~totnet, des_correct)))
se_wrong   <- as.numeric(survey::SE(survey::svymean(~totnet, des_wrong)))
se_imp1    <- as.numeric(survey::SE(survey::svymean(~totnet, des_imp1)))

cat("\nQ1b – Errors from NOT dividing weights before pooling:\n")
cat(sprintf("  Weighted MEAN  (correct w/5):  %s EUR\n",
            formatC(wm_stacked, format = "f", big.mark = ",", digits = 2)))
cat(sprintf("  Weighted MEAN  (raw weights):  %s EUR  <-- same (normalisation cancels)\n",
            formatC(wm_wrong,   format = "f", big.mark = ",", digits = 2)))
cat(sprintf("  Weighted TOTAL (correct w/5):  %s EUR\n",
            formatC(tot_correct, format = "e", digits = 4)))
cat(sprintf("  Weighted TOTAL (raw weights):  %s EUR  <-- inflated by factor %d\n",
            formatC(tot_wrong,   format = "e", digits = 4), M_IMP))
cat(sprintf("  SE (single implicate, imp=1):   %s EUR  <-- baseline reference\n",
            formatC(se_imp1,    format = "f", big.mark = ",", digits = 2)))
cat(sprintf("  SE (stacked, w/5):              %s EUR  <-- underestimated (5N treated as indep.)\n",
            formatC(se_correct, format = "f", big.mark = ",", digits = 2)))
cat(sprintf("  SE (stacked, raw weights):      %s EUR  <-- same underestimation (weight scaling cancels)\n",
            formatC(se_wrong,   format = "f", big.mark = ",", digits = 2)))
cat("  => Proper combined SE requires Rubin's rules (see Q1d).\n")

# ---- Q1c: Weighted average per implicate ------------------------------------
imp_estimates <- purrr::map_dfr(1:M_IMP, function(m) {
  d   <- dplyr::filter(eff_raw, imp == m)
  des <- survey::svydesign(ids = ~1, weights = ~facine3, data = d)
  est <- survey::svymean(~totnet, des)
  dplyr::tibble(
    implicate = m,
    Q_m       = as.numeric(coef(est)),
    SE_m      = as.numeric(survey::SE(est)),
    U_m       = as.numeric(stats::vcov(est))   # within-implicate sampling variance
  )
})

cat("\nQ1c – Weighted average total net wealth by implicate:\n")
print(imp_estimates, digits = 6)

# ---- Q1d: Rubin's rules (Rubin 1987) ----------------------------------------
# Q̄  = combined point estimate (mean of implicate estimates)
# W   = average within-imputation variance
# B   = between-imputation variance (using M-1 denominator)
# T   = total variance = W + (1 + 1/M) * B
# df  = Barnard & Rubin (1999) degrees of freedom

Q_bar  <- mean(imp_estimates$Q_m)
W_bar  <- mean(imp_estimates$U_m)
B      <- var(imp_estimates$Q_m)          # uses 1/(M-1) denominator
T_var  <- W_bar + (1 + 1 / M_IMP) * B
SE_T   <- sqrt(T_var)

# Degrees of freedom (Barnard & Rubin 1999)
r_hat  <- (1 + 1 / M_IMP) * B / W_bar   # relative increase in variance
df_old <- (M_IMP - 1) * (1 + 1 / r_hat)^2

cat("\nQ1d – Rubin's combined estimate:\n")
cat(sprintf("  Implicate estimates Q_m: %s\n",
            paste(formatC(imp_estimates$Q_m, format = "f", big.mark = ",",
                          digits = 2), collapse = " | ")))
cat(sprintf("  Combined estimate (Q̄):  %s EUR\n",
            formatC(Q_bar,  format = "f", big.mark = ",", digits = 2)))
cat(sprintf("  Within-imp. variance (W̄): %.4e\n", W_bar))
cat(sprintf("  Between-imp. variance (B): %.4e\n", B))
cat(sprintf("  Total variance (T):        %.4e\n", T_var))
cat(sprintf("  Standard error (Rubin):   %s EUR\n",
            formatC(SE_T, format = "f", big.mark = ",", digits = 2)))
cat(sprintf("  95%% CI: [%s , %s] EUR\n",
            formatC(Q_bar - qt(0.975, df_old) * SE_T, format = "f",
                    big.mark = ",", digits = 2),
            formatC(Q_bar + qt(0.975, df_old) * SE_T, format = "f",
                    big.mark = ",", digits = 2)))

# ---- LaTeX tables -----------------------------------------------------------

fmt_eur <- function(x) formatC(x, format = "f", big.mark = ",", digits = 2)
fmt_sci <- function(x) formatC(x, format = "e", digits = 4)

# -- Table A: per-implicate estimates (Q1c) -----------------------------------
tbl_imp <- imp_estimates |>
  dplyr::mutate(
    Implicate        = implicate,
    `$\\hat{Q}_m$ (EUR)`  = fmt_eur(Q_m),
    `$\\text{SE}_m$ (EUR)` = fmt_eur(SE_m),
    `$U_m$ (variance)`    = fmt_sci(U_m)
  ) |>
  dplyr::select(Implicate, `$\\hat{Q}_m$ (EUR)`,
                `$\\text{SE}_m$ (EUR)`, `$U_m$ (variance)`)

tbl_imp_tex <- kableExtra::kbl(
  tbl_imp,
  format   = "latex",
  booktabs = TRUE,
  escape   = FALSE,
  caption  = "Weighted mean total net wealth by implicate (EFF 2017)",
  label    = "tab:implicates"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general = "Survey-weighted estimates using \\\\texttt{facine3}. Each implicate contains 6,413 households.",
    escape  = FALSE,
    general_title = "Note:"
  )

writeLines(tbl_imp_tex,
           file.path(TAB_DIR, "Q1a_implicate_estimates.tex"))
cat("\nQ1 – Table saved: output/tables/Q1a_implicate_estimates.tex\n")

# -- Table B: Rubin's rules summary (Q1d) -------------------------------------
ci_lo <- Q_bar - qt(0.975, df_old) * SE_T
ci_hi <- Q_bar + qt(0.975, df_old) * SE_T

rubin_rows <- data.frame(
  Quantity = c(
    "Combined point estimate $\\bar{Q}$",
    "Avg.\ within-imputation variance $\\bar{W}$",
    "Between-imputation variance $B$",
    "Total variance $T = \\bar{W} + (1 + 1/M)B$",
    "Standard error $\\sqrt{T}$",
    "95\\% confidence interval"
  ),
  Value = c(
    paste0(fmt_eur(Q_bar),  " EUR"),
    fmt_sci(W_bar),
    fmt_sci(B),
    fmt_sci(T_var),
    paste0(fmt_eur(SE_T),   " EUR"),
    paste0("[", fmt_eur(ci_lo), ",\\; ", fmt_eur(ci_hi), "] EUR")
  ),
  stringsAsFactors = FALSE
)

tbl_rubin_tex <- kableExtra::kbl(
  rubin_rows,
  format   = "latex",
  booktabs = TRUE,
  escape   = FALSE,
  col.names = c("Quantity", "Value"),
  caption  = "Rubin's rules: combined estimate for mean total net wealth (EFF 2017)",
  label    = "tab:rubin"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::column_spec(1, width = "8cm") |>
  kableExtra::column_spec(2, width = "5cm") |>
  kableExtra::footnote(
    general = "$M = 5$ implicates. Degrees of freedom follow Barnard \\\\& Rubin (1999).",
    escape  = FALSE,
    general_title = "Note:"
  )

writeLines(tbl_rubin_tex,
           file.path(TAB_DIR, "Q1b_rubin_rules.tex"))
cat("Q1 – Table saved: output/tables/Q1b_rubin_rules.tex\n")

# -- Table C: SE comparison (Q1b vs Q1d) --------------------------------------
se_rows <- data.frame(
  Approach = c(
    "Single implicate ($m = 1$ only)",
    "Stacked, adjusted weights ($w_i / M$)",
    "Stacked, raw weights ($w_i$)",
    "Rubin's rules (correct, $M = 5$)"
  ),
  SE = c(
    fmt_eur(se_imp1),
    fmt_eur(se_correct),
    fmt_eur(se_wrong),
    fmt_eur(SE_T)
  ),
  Notes = c(
    "Ignores imputation uncertainty",
    "Treats 5N obs.\ as independent; underestimates SE",
    "Same as above (scaling cancels in mean)",
    "Accounts for both sampling and imputation uncertainty"
  ),
  stringsAsFactors = FALSE
)

tbl_se_tex <- kableExtra::kbl(
  se_rows,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  col.names = c("Approach", "SE (EUR)", "Comment"),
  caption   = "Standard error of weighted mean total net wealth under alternative approaches (EFF 2017)",
  label     = "tab:se_comparison"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::column_spec(1, width = "5.5cm") |>
  kableExtra::column_spec(2, width = "2.2cm") |>
  kableExtra::column_spec(3, width = "6.5cm") |>
  kableExtra::row_spec(4, bold = TRUE) |>
  kableExtra::footnote(
    general = paste0(
      "Combined Rubin estimate: $\\\\bar{Q} = ",
      fmt_eur(Q_bar),
      "$ EUR. 95\\\\% CI: [",
      fmt_eur(Q_bar - qt(0.975, df_old) * SE_T), ",\\\\; ",
      fmt_eur(Q_bar + qt(0.975, df_old) * SE_T), "] EUR."
    ),
    escape        = FALSE,
    general_title = "Note:"
  )

writeLines(tbl_se_tex,
           file.path(TAB_DIR, "Q1c_se_comparison.tex"))
cat("Q1 – Table saved: output/tables/Q1c_se_comparison.tex\n")
