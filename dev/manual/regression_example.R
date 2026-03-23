#| h1: Regression example
#| text: Simple linear model using mtcars.

model <- lm(mpg ~ wt, data = mtcars)
summary(model)

coef_tbl <- as.data.frame(summary(model)$coefficients) |>
  tibble::rownames_to_column("term")

birtir::md_table(coef_tbl, caption = "Coefficient table", digits = 3)

#| h2: Plot

p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

birtir::md_plot(p, caption = "MPG vs weight")
