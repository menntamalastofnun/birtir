devtools::load_all(".")

model <- lm(mpg ~ wt, data = mtcars)

coef_tbl <- as.data.frame(summary(model)$coefficients) |>
  tibble::rownames_to_column("term")

birtir::md_table(coef_tbl, caption = "Coefficient table", digits = 3)

p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

birtir::md_plot(p, caption = "MPG vs weight")
