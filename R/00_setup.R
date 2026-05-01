# =============================================================================
# 00_setup.R – Packages, paths, data import, and shared helper functions
# =============================================================================

# ---- 1. Package installation & loading --------------------------------------
required_packages <- c(
  "haven",         # read Stata .dta files
  "dplyr",         # data manipulation
  "tidyr",         # reshaping
  "ggplot2",       # visualisation
  "purrr",         # functional programming utilities
  "forcats",       # factor manipulation
  "scales",        # axis formatting in ggplot2
  "survey",        # survey-weighted estimation (svydesign, svymean, svyglm)
  "boot",          # non-parametric bootstrap
  "sandwich",      # robust variance estimators (HC1)
  "lmtest",        # coefficient tests with robust SEs (coeftest)
  "modelsummary",  # publication-quality regression tables
  "kableExtra",    # LaTeX / HTML table formatting
  "broom",         # tidy() and glance() for model objects
  "car"            # variance inflation factors (vif)
)

to_install <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]
if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---- modelsummary / kableExtra global options -------------------------------
# Drop \num{} wrapping so tables render without requiring \usepackage{siunitx}.
options(modelsummary_format_numeric_latex = "plain")

# fmt_smart(): fixed-decimal formatter that falls back to scientific notation
# for non-zero values that would otherwise round to 0 (e.g. age^2 coefficients
# of order 1e-4 displayed with 3 decimals).
fmt_smart <- function(digits = 3, sci_digits = 2) {
  function(x) {
    out <- formatC(x, format = "f", digits = digits)
    bad <- !is.na(x) & x != 0 & abs(x) < 5 * 10^(-digits)
    if (any(bad)) {
      out[bad] <- formatC(x[bad], format = "e", digits = sci_digits)
    }
    out
  }
}

# ---- 2. Output directories --------------------------------------------------
DATA_DIR <- "toshare"
FIG_DIR  <- file.path("output", "figures")
TAB_DIR  <- file.path("output", "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 3. Load raw data -------------------------------------------------------
eff_raw <- haven::read_dta(file.path(DATA_DIR, "eff.dta")) |> haven::zap_labels()
sl_raw  <- haven::read_dta(file.path(DATA_DIR, "secondlang_prob.dta")) |> haven::zap_labels()

message("EFF loaded:  ", nrow(eff_raw), " rows × ", ncol(eff_raw), " cols")
message("SL  loaded:  ", nrow(sl_raw),  " rows × ", ncol(sl_raw),  " cols")

# Number of implicates
M_IMP <- length(unique(eff_raw$imp))   # = 5

# Implicate 1 only (used from Q2 onward)
eff1 <- eff_raw |> dplyr::filter(imp == 1)

# ---- 4. Shared helper functions ---------------------------------------------

# Weighted quantile (exact definition: smallest x such that cum. weight >= prob)
wt_quantile <- function(x, w, probs = 0.5, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w) & w > 0
    x <- x[keep]; w <- w[keep]
  }
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  ord   <- order(x)
  x     <- x[ord]
  w     <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cum_w >= p)[1]])
}

# Weighted mean
wt_mean <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]; w <- w[keep]
  }
  sum(w * x) / sum(w)
}

# Weighted Pearson correlation
wt_cor <- function(x, y, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(y) & !is.na(w)
    x <- x[keep]; y <- y[keep]; w <- w[keep]
  }
  w  <- w / sum(w)
  mx <- sum(w * x); my <- sum(w * y)
  cov_xy <- sum(w * (x - mx) * (y - my))
  sd_x   <- sqrt(sum(w * (x - mx)^2))
  sd_y   <- sqrt(sum(w * (y - my)^2))
  cov_xy / (sd_x * sd_y)
}

# Consistent ggplot2 theme
theme_eff <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(size = 8, colour = "grey50"),
      legend.position = "bottom"
    )
}

# Save plot to output/figures/
save_plot <- function(p, filename, width = 9, height = 6) {
  ggsave(file.path(FIG_DIR, filename), plot = p,
         width = width, height = height, dpi = 300)
  message("  Figure saved: ", filename)
}

# ---- 5. Education category helper (used in Q3, Q4, Q6) ---------------------
# Groups educ_resp into 8 meaningful ordered categories.
# Reference level in regressions: "Illiterate" (1_Illiterate) — alphabetically first.
make_educ_cat <- function(educ_resp) {
  lvl <- dplyr::case_when(
    educ_resp == 1                        ~ "1_Illiterate",
    educ_resp == 2                        ~ "2_Primary",
    educ_resp %in% c(3, 4)               ~ "3_Lower_sec",
    educ_resp %in% c(5, 7)               ~ "4_Vocational",
    educ_resp == 6                        ~ "5_Upper_sec",
    educ_resp %in% c(8, 9)               ~ "6_Post_sec",
    educ_resp %in% c(1001, 1002, 11, 12) ~ "7_University",
    TRUE                                  ~ "8_Other"
  )
  factor(lvl)  # "1_Illiterate" is the reference (alphabetically first)
}

# Labour market status (priority order handles rare overlaps)
make_labour_cat <- function(emp, self, une, ret) {
  lvl <- dplyr::case_when(
    emp  == 1 ~ "Employed",
    self == 1 ~ "Self_employed",
    une  == 1 ~ "Unemployed",
    ret  == 1 ~ "Retired",
    TRUE      ~ "Inactive"
  )
  factor(lvl, levels = c("Employed", "Self_employed", "Unemployed",
                          "Retired", "Inactive"))
}

# Non-employment rate: share of household members not employed or self-employed.
# Only counts members up to hhsize (max 9 captured in data).
compute_non_emp_rate <- function(df) {
  n_rows <- nrow(df)
  act_mat <- matrix(NA_real_, nrow = n_rows, ncol = 9)
  act_mat[, 1] <- pmin(df$emp_resp + df$self_resp, 1)
  for (i in 2:9) {
    act_i <- pmin(df[[paste0("emp_", i)]] + df[[paste0("self_", i)]], 1)
    act_i[df$hhsize < i] <- NA   # member does not exist
    act_mat[, i] <- act_i
  }
  non_emp_count <- rowSums(1 - act_mat, na.rm = TRUE)
  non_emp_count / df$hhsize
}

message("Setup complete.")

# ---- 6. Helper to insert italic group headers above coefficient blocks ------
# `groups` is a named list "Header" = c("first_var", "last_var") referring to
# entries in `coef_map` (raw R names). Two display rows per coefficient
# (estimate + SE) are assumed, which matches modelsummary's default output.
# Headers are added bottom-up so earlier insertions don't shift later indices.
add_ref_groups <- function(tbl, coef_map, groups) {
  positions <- lapply(groups, function(vars) {
    pos_first <- which(names(coef_map) == vars[1])
    pos_last  <- which(names(coef_map) == vars[2])
    c(start = (pos_first - 1) * 2 + 1, end = pos_last * 2)
  })
  ord <- order(vapply(positions, `[`, numeric(1), 1), decreasing = TRUE)
  for (i in ord) {
    p <- positions[[i]]
    tbl <- kableExtra::pack_rows(
      tbl, names(groups)[i], p["start"], p["end"],
      bold = FALSE, italic = TRUE, escape = FALSE,
      indent = FALSE,
      latex_gap_space = "0.3em"
    )
  }
  tbl
}
