# CFA Analysis Script - HolzingerSwineford1939
# Demonstrates all features of birtir

library(birtir)
library(lavaan)
library(ggplot2)

md_text("# Confirmatory Factor Analysis: Holzinger & Swineford (1939)")
md_text("This report evaluates a three-factor CFA model (Visual, Textual, Speed) using the `HolzingerSwineford1939` dataset.")

# 1. LaTeX Model Equations
cfa_model <- "
  Visual =~ x1 + x2 + x3
  Text   =~ x4 + x5 + x6
  Speed  =~ x7 + x8 + x9
"

md_text("## 1. Model Specification and Equations")
md_text("The latent factor structure is defined as follows:")
eqs <- lavaan_to_latex(cfa_model)
for (eq in eqs) {
  md_text("- {eq}")
}

md_text("Measurement model in matrix notation: {lavaan_to_latex(cfa_model, notation = 'matrix')}")
md_text("Example auxiliary regression equation: {formula_to_latex(x1 ~ ageyr + (1 | school))}")

# 2. Data Summaries with describe_data()
dat <- HolzingerSwineford1939

md_text("## 2. Descriptive Statistics")
desc_uni <- describe_data(dat, x1 ~ 1)
md_text("### Indicator x1 Summary")
md_text(as_report_text(desc_uni))

md_text("### Grouped Summary by School")
desc_grp <- describe_data(dat, x1 ~ school)
md_table(as_report_table(desc_grp), caption = "Visual Indicator x1 by School", digits = 2, note = "Data from Pasteur and Grant-White schools.")

md_text("### Bivariate Relationship (x1 and x2)")
desc_rel <- describe_data(dat, x2 ~ x1)
md_text(as_report_text(desc_rel))

md_text("### Random-Intercept Structure")
desc_ran <- describe_data(dat, x1 ~ (1 | school))
md_text(as_report_text(desc_ran))

# 3. Inline Character Visualizations
md_text("## 3. Inline Text Visualizations")
md_text("Histogram for x1:")
inline_hist(dat$x1[!is.na(dat$x1)], n_bins = 6)

md_text("Boxplot for x1:")
inline_boxplot(dat$x1[!is.na(dat$x1)], width = 30)

md_text("Scatter plot for x1 vs x2:")
inline_scatter(dat$x1[1:30], dat$x2[1:30], width = 20, height = 5)

md_text("School counts bar chart:")
inline_bar(table(dat$school), width = 15)

md_text("Forest summary preview:")
inline_forest(
  estimate = c(Visual = 0.65, Textual = 0.78, Speed = 0.52),
  ci_low = c(0.52, 0.68, 0.39),
  ci_high = c(0.78, 0.88, 0.65),
  n = c(301, 301, 301),
  width = 15
)

# 4. CFA Model Estimation and Parameter Tables
md_text("## 4. CFA Model Fit and Parameter Estimates")

fit <- lavaan::cfa(cfa_model, data = dat)
fit_summary <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

fit_df <- data.frame(
  Measure = names(fit_summary),
  Value = as.numeric(fit_summary)
)

md_table(fit_df, caption = "CFA Model Fit Statistics", digits = 3, note = "CFI >= 0.95 and RMSEA <= 0.06 indicate good model fit.")

pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
pe_loadings <- pe[pe$op == "=~", c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all")]

md_table(
  pe_loadings,
  caption = "Factor Loadings",
  digits = 3,
  note = "Standardized loadings (std.all) shown for visual, textual, and speed factors."
)

# 5. Diagnostic Figures
md_text("## 5. Diagnostic Figures")

p <- ggplot(pe_loadings, aes(x = rhs, y = std.all, fill = lhs)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Standardized Factor Loadings",
    x = "Indicator",
    y = "Standardized Loading",
    fill = "Factor"
  )

md_plot(p, caption = "Comparison of Standardized Loadings Across Indicators", width = 6, height = 4, note = "Loadings computed using maximum likelihood estimation.")

# 6. Formatting Helpers
md_text("## 6. Summary and Conclusion")
chi_val <- fmt_num(fit_summary["chisq"], digits = 2)
p_val <- fmt_p(fit_summary["pvalue"])
cfi_val <- fmt_num(fit_summary["cfi"], digits = 3, drop_leading_zero = TRUE)

md_text("The 3-factor model demonstrated strong fit to the data, chi-square = {chi_val}, p {p_val}, CFI = {cfi_val}.")
