# Spanish Survey of Household Finances (EFF 2017) — Analysis Pipeline

An end-to-end econometric analysis of the **Encuesta Financiera de las Familias (EFF) 2017**, the Bank of Spain's survey of household finances. The pipeline covers multiple imputation, descriptive statistics, homeownership modelling, proxy variable analysis, variable construction, and mortgage regressions.

## Contents

```
.
├── main.R                        # Master script — runs all analyses in order
├── main.tex                      # Full write-up with results, tables, and figures
├── explore_data.ipynb            # Exploratory data analysis (Python/Jupyter)
├── R/
│   ├── 00_setup.R                # Packages, paths, data loading, shared helpers
│   ├── 01_implicates.R           # Multiple imputation & Rubin's rules
│   ├── 02_descriptives.R         # Descriptive statistics & visualisations
│   ├── 03_homeownership_models.R # LPM & Probit homeownership models
│   ├── 04_merge_proxy.R          # Dataset merge & proxy variable analysis
│   ├── 05_constructed_vars.R     # Variable construction & group comparisons
│   └── 06_mortgage_regressions.R # Mortgage regression with robust SEs
├── output/
│   ├── figures/                  # PNG plots (ggplot2)
│   └/tables/                    # LaTeX-formatted regression tables
└── toshare/
    ├── eff.dta                   # EFF 2017 microdata (Stata format)
    ├── secondlang_prob.dta       # Language probability scores dataset
    ├── codebook.pdf              # Variable codebook
    └── eff_user_guide_2017.pdf   # Official EFF user guide
```

## Topics Covered

| Section | Description |
|---|---|
| **Q1 — Multiple Imputation** | Correct weight transformation when stacking implicates; Rubin's rules for pooled SEs; comparison of naive vs. corrected standard errors |
| **Q2 — Descriptive Statistics** | Homeownership rates by age and gender; bootstrapped wealth estimates by age group; secondary home ownership patterns |
| **Q3 — Homeownership Models** | Linear Probability Model vs. Probit; average marginal effects (AMEs); education gradient in homeownership |
| **Q4 — Proxy Variable Analysis** | Merging EFF with language probability data; collinearity diagnostics (VIF); AMEs by wealth quintile |
| **Q5 — Variable Construction** | Building composite variables from raw survey responses; distributional summaries and group comparisons |
| **Q6 — Mortgage Regressions** | Determinants of mortgage holding; heteroskedasticity-robust (HC1) standard errors; publication-ready LaTeX tables |

## Requirements

### R packages
```r
haven, dplyr, tidyr, ggplot2, purrr, forcats, scales,
survey, boot, sandwich, lmtest, modelsummary, kableExtra, broom, car
```

Install missing packages automatically on first run via `main.R`.

### Python (optional, for notebook)
```
pandas, numpy, matplotlib, seaborn, pyreadstat
```

## Usage

Open `eff_task.Rproj` in RStudio and run:

```r
source("main.R")
```

This executes all six analysis scripts in order and writes tables to `output/tables/` and figures to `output/figures/`. The LaTeX write-up (`main.tex`) inputs these outputs directly and can be compiled with any standard LaTeX distribution.

## Data

The EFF 2017 is a stratified survey of Spanish households conducted by the Bank of Spain. The dataset uses **five implicates** (multiply imputed copies) to handle item non-response. Survey weights (`facine3`) must be divided by the number of implicates (M = 5) before stacking for correct population-level estimates.

Full documentation: [Bank of Spain — EFF](https://www.bde.es/bde/en/areas/estadis/estadisticas-por/encuestas-hogar/relacionados/Encuesta_Financ/)
