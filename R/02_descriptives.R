# =============================================================================
# 02_descriptives.R – Q2: Descriptive statistics (imp = 1 only)
# =============================================================================
cat("\n===== Q2: Descriptive statistics (imp = 1) =====\n")

# Age groups: 5-year bins covering 25–80.
# Using right-closed intervals with break points shifted by 1 so that each
# interval is (lower-1, upper], i.e. (24,29] = {25..29}, ..., (74,80] = {75..80}.
age_breaks <- c(24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 80)
age_labels  <- c("25-29", "30-34", "35-39", "40-44", "45-49",
                 "50-54", "55-59", "60-64", "65-69", "70-74", "75-80")

# Working dataset: imp == 1, respondent age 25–80
eff1_age <- eff1 |>
  dplyr::filter(age_resp >= 25, age_resp <= 80) |>
  dplyr::mutate(
    age_group = cut(age_resp, breaks = age_breaks, labels = age_labels,
                    right = TRUE, include.lowest = FALSE)
  )

# ---- Q2a: Weighted homeownership rate by age group --------------------------
# 'own' = 1 if household owns main residence.
homeown_by_age <- eff1_age |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(
    n_hh          = dplyr::n(),
    sum_w         = sum(facine3),
    own_rate      = wt_mean(own, facine3),
    .groups = "drop"
  )

cat("\nQ2a – Weighted homeownership rate by 5-year age group:\n")
print(homeown_by_age, digits = 4)

# ---- Q2b: Plot homeownership by age group, split by gender ------------------
# gender_resp: 1 = woman, 0 = man
homeown_gender <- eff1_age |>
  dplyr::group_by(age_group, gender_resp) |>
  dplyr::summarise(own_rate = wt_mean(own, facine3), .groups = "drop") |>
  dplyr::mutate(
    Gender = dplyr::case_when(
      gender_resp == 0 ~ "Men",
      gender_resp == 1 ~ "Women",
      TRUE             ~ "Other"
    )
  )

homeown_overall <- eff1_age |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(own_rate = wt_mean(own, facine3), .groups = "drop") |>
  dplyr::mutate(Gender = "Overall")

plot2b_data <- dplyr::bind_rows(homeown_overall, homeown_gender)

p2b <- ggplot2::ggplot(plot2b_data,
                       ggplot2::aes(x = age_group, y = own_rate,
                                    colour = Gender, group = Gender,
                                    linetype = Gender)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::scale_colour_manual(
    values = c("Overall" = "#333333", "Men" = "#2c7fb8", "Women" = "#d73027")
  ) +
  ggplot2::scale_linetype_manual(
    values = c("Overall" = "solid", "Men" = "dashed", "Women" = "dotted")
  ) +
  ggplot2::labs(
    x       = "Age group",
    y       = "Weighted homeownership rate",
    title   = "Q2b – Homeownership rate by age group and gender",
    caption = "Source: EFF 2017 (imp = 1) | Weighted by facine3"
  ) +
  theme_eff()

save_plot(p2b, "Q2b_homeownership_by_age_gender.png")

# ---- Q2c: Median total wealth by age group with bootstrapped 95% CI ---------
set.seed(123)
N_BOOT <- 500

age_groups_ordered <- levels(eff1_age$age_group)

# Bootstrap statistic: weighted median of totnet by age group (returns vector)
boot_wmed_by_age <- function(data, idx) {
  d <- data[idx, ]
  sapply(age_groups_ordered, function(ag) {
    sub <- d[d$age_group == ag, ]
    if (nrow(sub) == 0) return(NA_real_)
    wt_quantile(sub$totnet, sub$facine3, probs = 0.5)
  })
}

cat("\nQ2c – Running bootstrap (n =", N_BOOT, ")... this may take ~1 min.\n")
boot_res <- boot::boot(eff1_age, boot_wmed_by_age, R = N_BOOT)

# Observed medians and 95% percentile bootstrap CIs
boot_ci_df <- purrr::map_dfr(seq_along(age_groups_ordered), function(j) {
  t_j <- boot_res$t[, j]
  t_j <- t_j[!is.na(t_j)]
  dplyr::tibble(
    age_group = age_groups_ordered[j],
    median_w  = boot_res$t0[j],
    lower_ci  = quantile(t_j, 0.025),
    upper_ci  = quantile(t_j, 0.975)
  )
})

p2c <- ggplot2::ggplot(boot_ci_df,
                       ggplot2::aes(x = age_group, y = median_w / 1e3)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower_ci / 1e3, ymax = upper_ci / 1e3, group = 1),
    alpha = 0.25, fill = "#2c7fb8"
  ) +
  ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#2c7fb8", linewidth = 1) +
  ggplot2::geom_point(colour = "#2c7fb8", size = 2.5) +
  ggplot2::scale_y_continuous(labels = scales::comma_format(suffix = "k")) +
  ggplot2::labs(
    x       = "Age group",
    y       = "Weighted median total net wealth (EUR thousands)",
    title   = "Q2c – Total wealth by age group: median with bootstrapped 95% CI",
    caption = paste0("Bootstrap n = ", N_BOOT, " | Source: EFF 2017 (imp = 1)")
  ) +
  theme_eff()

save_plot(p2c, "Q2c_wealth_by_age_bootstrap.png")

cat("\nQ2c – Median wealth with 95% bootstrap CI:\n")
print(boot_ci_df |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x / 1e3, 1))),
      digits = 2)

# ---- Q2d: Reusable function: plot_ownership_rate ----------------------------
# Function arguments:
#   data        – data frame with age_group, a binary outcome, and weights
#   outcome_var – name of the binary outcome column (string)
#   weight_var  – name of the weight column (default "facine3")
#   age_var     – name of the age-group column (default "age_group")
#   xlab, ylab  – axis labels (character)
#   title       – optional plot title
#
# Returns a ggplot object.

plot_ownership_rate <- function(data,
                                outcome_var,
                                weight_var  = "facine3",
                                age_var     = "age_group",
                                xlab        = "Age group",
                                ylab        = "Ownership rate",
                                title       = NULL,
                                colour      = "#2c7fb8") {
  rate_df <- data |>
    dplyr::group_by(.data[[age_var]]) |>
    dplyr::summarise(
      rate = wt_mean(.data[[outcome_var]], .data[[weight_var]]),
      .groups = "drop"
    )

  ggplot2::ggplot(rate_df,
                  ggplot2::aes(x = .data[[age_var]], y = rate,
                               group = 1)) +
    ggplot2::geom_line(colour = colour, linewidth = 1) +
    ggplot2::geom_point(colour = colour, size = 2.5) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, NA)
    ) +
    ggplot2::labs(
      x       = xlab,
      y       = ylab,
      title   = title,
      caption = "Source: EFF 2017 (imp = 1) | Weighted by facine3"
    ) +
    theme_eff()
}

# Demonstrate the function: secondary-home ownership by age group
p2d <- plot_ownership_rate(
  data        = eff1_age,
  outcome_var = "own_ot",
  xlab        = "Age group",
  ylab        = "Secondary-home ownership rate",
  title       = "Q2d – Secondary-home ownership rate by age group",
  colour      = "#d73027"
)
save_plot(p2d, "Q2d_secondary_home_ownership_by_age.png")

# ---- LaTeX tables -----------------------------------------------------------

fmt_pct <- function(x) paste0(formatC(x * 100, format = "f", digits = 1), "\\%")
fmt_eur <- function(x) formatC(x, format = "f", big.mark = ",", digits = 0)

# -- Table A: Homeownership rate by age group and gender (Q2a / Q2b) ----------
own_gender_wide <- homeown_gender |>
  dplyr::select(age_group, Gender, own_rate) |>
  tidyr::pivot_wider(names_from = Gender, values_from = own_rate) |>
  dplyr::left_join(
    homeown_overall |> dplyr::select(age_group, own_rate) |>
      dplyr::rename(Overall = own_rate),
    by = "age_group"
  ) |>
  dplyr::left_join(
    homeown_by_age |> dplyr::select(age_group, n_hh),
    by = "age_group"
  ) |>
  dplyr::mutate(
    `Age group`  = as.character(age_group),
    `$N$ (HH)`   = formatC(n_hh, format = "d", big.mark = ","),
    `Men`         = fmt_pct(Men),
    `Women`       = fmt_pct(Women),
    `Overall`     = fmt_pct(Overall)
  ) |>
  dplyr::select(`Age group`, `$N$ (HH)`, Men, Women, Overall)

tbl_own_tex <- kableExtra::kbl(
  own_gender_wide,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  align     = c("l", "r", "r", "r", "r"),
  caption   = "Weighted homeownership rate by age group and gender (EFF 2017, imp = 1)",
  label     = "tab:homeownership_age_gender"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::add_header_above(
    c(" " = 2, "Homeownership rate" = 3),
    escape = FALSE
  ) |>
  kableExtra::footnote(
    general       = "Survey-weighted rates using \\\\texttt{facine3}. Respondents aged 25--80.",
    escape        = FALSE,
    general_title = "Note:"
  )

writeLines(tbl_own_tex,
           file.path(TAB_DIR, "Q2a_homeownership_by_age_gender.tex"))
cat("\nQ2 – Table saved: output/tables/Q2a_homeownership_by_age_gender.tex\n")

# -- Table B: Median total wealth with bootstrapped CI (Q2c) ------------------
tbl_wealth <- boot_ci_df |>
  dplyr::mutate(
    `Age group`    = as.character(age_group),
    `Median (EUR)` = fmt_eur(median_w),
    `95\\% CI`     = paste0(
      "[", fmt_eur(lower_ci), ",\\; ", fmt_eur(upper_ci), "]"
    )
  ) |>
  dplyr::select(`Age group`, `Median (EUR)`, `95\\% CI`)

tbl_wealth_tex <- kableExtra::kbl(
  tbl_wealth,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  align     = c("l", "r", "r"),
  caption   = "Weighted median total net wealth by age group with bootstrapped 95\\% confidence intervals (EFF 2017, imp = 1)",
  label     = "tab:wealth_by_age"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general       = paste0(
      "Percentile bootstrap, $B = ", N_BOOT, "$ replications, \\\\texttt{set.seed(123)}. ",
      "Weighted median of total net wealth (\\\\texttt{totnet}) by age group."
    ),
    escape        = FALSE,
    general_title = "Note:"
  )

writeLines(tbl_wealth_tex,
           file.path(TAB_DIR, "Q2b_wealth_by_age_bootstrap.tex"))
cat("Q2 – Table saved: output/tables/Q2b_wealth_by_age_bootstrap.tex\n")
