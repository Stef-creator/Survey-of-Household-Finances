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

cat("\nQ4a – p_knows_second_lang by education level (after imputation):\n")
print(dplyr::select(sl_table, educ_resp, educ_label, p_knows_second_lang, imputed),
      n = Inf)

# Save as a kable table
kableExtra::kable(
  dplyr::select(sl_table, educ_label, p_knows_second_lang, imputed),
  col.names = c("Education level", "P(knows 2nd language)", "Imputed"),
  digits    = 3,
  caption   = "Q4a – P(knows second language) by education level",
  format    = "latex",
  booktabs  = TRUE
) |>
  kableExtra::save_kable(file = file.path(TAB_DIR, "Q4a_secondlang_table.tex"))
cat("  Table saved: Q4a_secondlang_table.tex\n")

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
    title   = "Q4b – Average internationally-exposed education proxy by wealth quintile",
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
  neg ~ proxy + age_resp + age2 + labour_cat + hhsize,
  data    = eff1_proxy_m,
  weights = facine3
)

lpm_neg_rse <- lmtest::coeftest(lpm_neg,
                                  vcov = sandwich::vcovHC(lpm_neg, type = "HC1"))
cat("\nQ4c – Weighted LPM: business ownership (neg)\n")
print(lpm_neg_rse)

# For LPM the marginal effect of the proxy = its coefficient (constant).
# "AME by wealth quintile" here means: within each quintile, the coefficient
# is the same (LPM is linear), but we report the mean predicted probability
# of business ownership per quintile to show where ownership is concentrated.

me_proxy <- coef(lpm_neg)[["proxy"]]

ame_by_q <- eff1_proxy_m |>
  dplyr::mutate(y_hat = fitted(lpm_neg)) |>
  dplyr::group_by(wealth_q) |>
  dplyr::summarise(
    n_hh        = dplyr::n(),
    avg_pred_pr = wt_mean(y_hat, facine3),
    me_proxy    = me_proxy,          # constant for LPM
    .groups     = "drop"
  )

cat("\nQ4c – AME of proxy on business ownership (constant in LPM =", round(me_proxy, 4), ")\n")
cat("  Mean predicted P(business ownership) by wealth quintile:\n")
print(ame_by_q, digits = 4)
cat("\n  Interpretation: a one-unit increase in the proxy (i.e., moving from
  zero to p_knows_second_lang for a college-educated household) raises the
  probability of business ownership by", round(me_proxy * 100, 2), "pp.
  This is constant across quintiles (LPM); the rising mean predicted probability
  across quintiles reflects the positive wealth-business ownership gradient.\n")
