# =============================================================================
# 05_constructed_vars.R – Q5: Constructed variables and group comparisons
# =============================================================================
cat("\n===== Q5: Constructed variables =====\n")

# ---- Q5a: Construct three variables -----------------------------------------
# All constructed on imp == 1 (eff1).

eff1_c <- eff1 |>
  dplyr::mutate(
    # i) Financial wealth share of net wealth
    #    = finet / totnet
    #    Excluded if totnet <= 0 (undefined or ill-defined ratio).
    fin_share = dplyr::if_else(totnet > 0, finet / totnet, NA_real_),

    # ii) Total Debt to Net Wealth ratio
    #     = deud / totnet
    #     Excluded if totnet <= 0 (negative net wealth makes ratio uninterpretable;
    #     zero net wealth causes division by zero).
    debt_ratio = dplyr::if_else(totnet > 0, deud / totnet, NA_real_),

    # iii) Household non-employment rate
    #      = share of members (up to min(hhsize, 9)) who are NOT employed or
    #        self-employed (i.e., unemployed + retired + inactive).
    #      Members beyond position 9 are not captured; households with hhsize > 9
    #      are extremely rare in EFF and are treated as having full coverage.
    non_emp_rate = compute_non_emp_rate(dplyr::pick(dplyr::everything()))
  )

# Weighted medians (full sample, excluding NAs)
med_fin_share   <- wt_quantile(eff1_c$fin_share,    eff1_c$facine3, 0.5, na.rm = TRUE)
med_debt_ratio  <- wt_quantile(eff1_c$debt_ratio,   eff1_c$facine3, 0.5, na.rm = TRUE)
med_non_emp     <- wt_quantile(eff1_c$non_emp_rate, eff1_c$facine3, 0.5, na.rm = TRUE)
n_excl_fin  <- sum(is.na(eff1_c$fin_share))
n_excl_debt <- sum(is.na(eff1_c$debt_ratio))

cat(sprintf("\nQ5a – Constructed variables (weighted medians):\n"))
cat(sprintf("  fin_share    (finet/totnet):  %.4f  | excluded n=%d (totnet <= 0)\n",
            med_fin_share,  n_excl_fin))
cat(sprintf("  debt_ratio   (deud/totnet):   %.4f  | excluded n=%d (totnet <= 0)\n",
            med_debt_ratio, n_excl_debt))
cat(sprintf("  non_emp_rate (share non-emp): %.4f\n", med_non_emp))

# ---- Q5a: Percentile distribution table (LaTeX) ----------------------------
valid_d <- eff1_c[!is.na(eff1_c$debt_ratio), ]
valid_f <- eff1_c[!is.na(eff1_c$fin_share), ]
valid_n <- eff1_c[!is.na(eff1_c$non_emp_rate), ]

dist_tbl <- dplyr::tibble(
  Statistic = c("Mean", "P25", "Median (P50)", "P75", "P90",
                "Share with zero debt (\\%)", "$N$ (valid obs.)"),
  `Debt-to-net-wealth ratio` = c(
    sprintf("%.3f", wt_mean(valid_d$debt_ratio, valid_d$facine3)),
    sprintf("%.3f", wt_quantile(valid_d$debt_ratio, valid_d$facine3, 0.25)),
    sprintf("%.3f", wt_quantile(valid_d$debt_ratio, valid_d$facine3, 0.50)),
    sprintf("%.3f", wt_quantile(valid_d$debt_ratio, valid_d$facine3, 0.75)),
    sprintf("%.3f", wt_quantile(valid_d$debt_ratio, valid_d$facine3, 0.90)),
    sprintf("%.1f", wt_mean(as.integer(valid_d$debt_ratio == 0), valid_d$facine3) * 100),
    sprintf("%d",   nrow(valid_d))
  ),
  `Financial wealth share` = c(
    sprintf("%.3f", wt_mean(valid_f$fin_share, valid_f$facine3)),
    sprintf("%.3f", wt_quantile(valid_f$fin_share, valid_f$facine3, 0.25)),
    sprintf("%.3f", wt_quantile(valid_f$fin_share, valid_f$facine3, 0.50)),
    sprintf("%.3f", wt_quantile(valid_f$fin_share, valid_f$facine3, 0.75)),
    sprintf("%.3f", wt_quantile(valid_f$fin_share, valid_f$facine3, 0.90)),
    "---",
    sprintf("%d",   nrow(valid_f))
  ),
  `Non-employment rate` = c(
    sprintf("%.3f", wt_mean(valid_n$non_emp_rate, valid_n$facine3)),
    sprintf("%.3f", wt_quantile(valid_n$non_emp_rate, valid_n$facine3, 0.25)),
    sprintf("%.3f", wt_quantile(valid_n$non_emp_rate, valid_n$facine3, 0.50)),
    sprintf("%.3f", wt_quantile(valid_n$non_emp_rate, valid_n$facine3, 0.75)),
    sprintf("%.3f", wt_quantile(valid_n$non_emp_rate, valid_n$facine3, 0.90)),
    "---",
    sprintf("%d",   nrow(valid_n))
  )
)

options(modelsummary_factory_latex = "kableExtra")

tbl_dist <- kableExtra::kable(
  dist_tbl,
  col.names = c("Statistic",
                "Debt ratio",
                "Fin.\\ wealth share",
                "Non-emp.\\ rate"),
  caption   = "Survey-weighted distribution of constructed variables (EFF 2017, imp~=~1)",
  label     = "dist_constructed",
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  align     = "lccc"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general = paste0(
      "Debt ratio $= \\\\mathit{deud}/\\\\mathit{totnet}$; ",
      "financial wealth share $= \\\\mathit{finet}/\\\\mathit{totnet}$; ",
      "both exclude households with $\\\\mathit{totnet} \\\\leq 0$ ($n = ", n_excl_debt, "$). ",
      "Non-employment rate $=$ share of household members not employed or self-employed. ",
      "All statistics survey-weighted using \\\\texttt{facine3}."
    ),
    escape        = FALSE,
    general_title = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_dist, file.path(TAB_DIR, "Q5a_dist_constructed.tex"))
cat("  Table saved: Q5a_dist_constructed.tex\n")

# ---- Q5b: Group comparison --------------------------------------------------
# Groups (mutually exclusive, exhaustive) following the assignment definition
# of "exactly one property" / "more than one property" / "renters":
#   "One property"        : owns exactly one type of real estate, either
#                            main residence only OR additional only
#                            (own == 1 & own_ot == 0) | (own == 0 & own_ot == 1)
#   "Multiple properties" : owns both main residence AND additional RE
#                            (own == 1 & own_ot == 1)
#   "Renter"              : owns no real estate
#                            (own == 0 & own_ot == 0)
# Note: own_ot is binary in the EFF, so we cannot distinguish 2 vs many
# additional properties; the classification therefore relies on owning at
# least one of each kind to count as "multiple".

eff1_c <- eff1_c |>
  dplyr::mutate(
    prop_group = dplyr::case_when(
      own == 1 & own_ot == 1 ~ "Multiple properties",
      own == 1 | own_ot == 1 ~ "One property",
      TRUE                   ~ "Renter"
    ) |> factor(levels = c("One property", "Multiple properties", "Renter"))
  )

# Weighted summary statistics per group
summarise_var <- function(x, w, varname) {
  keep <- !is.na(x)
  x <- x[keep]; w <- w[keep]
  dplyr::tibble(
    variable = varname,
    mean_w   = wt_mean(x, w),
    median_w = wt_quantile(x, w, 0.50),
    p75_w    = wt_quantile(x, w, 0.75)
  )
}

group_stats <- eff1_c |>
  dplyr::group_by(prop_group) |>
  dplyr::reframe(
    dplyr::bind_rows(
      summarise_var(debt_ratio,  facine3, "Debt-to-net-wealth ratio"),
      summarise_var(fin_share,   facine3, "Financial wealth share")
    )
  )

cat("\nQ5b – Weighted statistics by property-ownership group:\n")
print(group_stats |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))),
      n = Inf)

# Save as LaTeX table
tbl_group <- kableExtra::kable(
  group_stats |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))),
  col.names = c("Group", "Variable", "W. Mean", "W. Median", "W. P75"),
  caption   = "Weighted statistics by property-ownership group (EFF 2017, imp~=~1)",
  label     = "group_comparison",
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general = paste0(
      "Groups are mutually exclusive and exhaustive: \"One property\" $=$ owns exactly one of \\\\{main residence, other real estate\\\\}; ",
      "\"Multiple properties\" $=$ owns both (\\\\texttt{own} $=1$ and \\\\texttt{own\\\\_ot} $=1$); ",
      "\"Renter\" $=$ owns no real estate. ",
      "Debt and financial wealth ratios exclude households with $\\\\mathit{totnet} \\\\leq 0$. ",
      "All statistics survey-weighted using \\\\texttt{facine3}."
    ),
    escape        = FALSE,
    general_title = "Notes: ",
    threeparttable = FALSE
  )

writeLines(tbl_group, file.path(TAB_DIR, "Q5b_group_comparison.tex"))
cat("  Table saved: Q5b_group_comparison.tex\n")

# ---- Q5c: Weighted Pearson correlation: non_emp_rate vs debt_ratio ----------
# Sample restriction: both variables defined (totnet > 0, non_emp_rate not NA)
eff1_corr <- eff1_c |>
  tidyr::drop_na(non_emp_rate, debt_ratio)

r_weighted <- wt_cor(eff1_corr$non_emp_rate, eff1_corr$debt_ratio,
                     eff1_corr$facine3)

# Approximate significance via standard Pearson t-test on the unweighted sample
# (provides indicative p-value; formal weighted inference would require bootstrap)
n_corr <- nrow(eff1_corr)
t_stat <- r_weighted * sqrt((n_corr - 2) / (1 - r_weighted^2))
p_val  <- 2 * pt(-abs(t_stat), df = n_corr - 2)

cat(sprintf(
  "\nQ5c – Weighted Pearson correlation (non_emp_rate vs debt_ratio):\n  r = %.4f | t(%d) = %.3f | p ≈ %.4f\n",
  r_weighted, n_corr - 2, t_stat, p_val
))

# ---- Q5d: Fraction combining high debt AND high non-employment --------------
# "High debt": debt_ratio > 75th weighted percentile (computed on valid obs)
# "High non-employment": non_emp_rate > 0.50
# Report both the weighted (design-consistent population share) and the
# unweighted (raw sample share) for transparency.

p75_debt <- wt_quantile(eff1_corr$debt_ratio, eff1_corr$facine3, 0.75)

cat(sprintf("\nQ5d – 75th weighted percentile of debt_ratio: %.4f\n", p75_debt))

joint_ind <- as.integer(
  eff1_corr$debt_ratio > p75_debt & eff1_corr$non_emp_rate > 0.5
)

frac_both_w  <- wt_mean(joint_ind, eff1_corr$facine3)
frac_both_uw <- mean(joint_ind)

cat(sprintf(
  "  Weighted   share (debt_ratio > P75 AND non_emp_rate > 50%%): %.4f (%.1f%%)\n",
  frac_both_w, frac_both_w * 100
))
cat(sprintf(
  "  Unweighted share (debt_ratio > P75 AND non_emp_rate > 50%%): %.4f (%.1f%%)\n",
  frac_both_uw, frac_both_uw * 100
))
