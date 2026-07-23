test_that("CFA HolzingerSwineford1939 workflow renders to MD and converts to DOCX", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  pandoc_bin <- birtir:::birtir_find_pandoc()
  if (!nzchar(pandoc_bin)) {
    skip("Pandoc is not available on this system.")
  }

  temp_dir <- withr::local_tempdir()
  script_path <- file.path(temp_dir, "cfa_script.R")

  script_lines <- c(
    "library(birtir)",
    "library(lavaan)",
    "library(ggplot2)",
    "",
    "md_text('# Holzinger & Swineford CFA')",
    "cfa_model <- 'Visual =~ x1 + x2 + x3\\nText =~ x4 + x5 + x6\\nSpeed =~ x7 + x8 + x9'",
    "eqs <- lavaan_to_latex(cfa_model)",
    "for (eq in eqs) { md_text('- {eq}') }",
    "md_text('Matrix notation: {lavaan_to_latex(cfa_model, notation = \"matrix\")}')",
    "md_text('Formula notation: {formula_to_latex(x1 ~ ageyr + (1 | school))}')",
    "",
    "dat <- HolzingerSwineford1939",
    "desc_uni <- describe_data(dat, x1 ~ 1)",
    "md_text(as_report_text(desc_uni))",
    "",
    "desc_grp <- describe_data(dat, x1 ~ school)",
    "md_table(as_report_table(desc_grp), caption = 'Grouped Table', note = 'Group note')",
    "",
    "desc_rel <- describe_data(dat, x2 ~ x1)",
    "md_text(as_report_text(desc_rel))",
    "",
    "desc_ran <- describe_data(dat, x1 ~ (1 | school))",
    "md_text(as_report_text(desc_ran))",
    "",
    "inline_hist(dat$x1, n_bins = 5)",
    "inline_boxplot(dat$x1, width = 20)",
    "inline_scatter(dat$x1[1:20], dat$x2[1:20], width = 15, height = 4)",
    "inline_bar(table(dat$school), width = 10)",
    "inline_forest(estimate = c(F1 = 0.5), ci_low = c(0.3), ci_high = c(0.7), n = c(100))",
    "",
    "fit <- lavaan::cfa(cfa_model, data = dat)",
    "fit_stats <- lavaan::fitMeasures(fit, c('chisq', 'df', 'pvalue', 'cfi'))",
    "fit_df <- data.frame(Metric = names(fit_stats), Value = as.numeric(fit_stats))",
    "md_table(fit_df, caption = 'Fit Measures')",
    "",
    "p <- ggplot(fit_df, aes(x = Metric, y = Value)) + geom_col()",
    "md_plot(p, caption = 'Fit Bar Chart')",
    "",
    "chi_val <- fmt_num(fit_stats['chisq'], digits = 2)",
    "p_val <- fmt_p(fit_stats['pvalue'])",
    "md_text('Final result: chi-square = {chi_val}, p {p_val}')"
  )

  writeLines(script_lines, script_path)

  output_dir <- file.path(temp_dir, "outputs")
  md_path <- render_analysis_md(script_path, output_dir = output_dir)

  expect_true(file.exists(md_path))
  md_lines <- readLines(md_path, warn = FALSE)
  expect_true(any(grepl("Holzinger & Swineford CFA", md_lines)))
  expect_true(any(grepl("Grouped Table", md_lines)))
  expect_true(any(grepl("Fit Measures", md_lines)))
  expect_true(any(grepl("Final result: chi-square =", md_lines)))

  docx_path <- convert_md(md_path, to = "docx")

  expect_true(file.exists(docx_path))
  expect_true(file.info(docx_path)$size > 0)
  expect_identical(tools::file_ext(docx_path), "docx")
})
