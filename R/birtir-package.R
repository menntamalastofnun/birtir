#' birtir: Render Analysis Scripts and Statistical Descriptions
#'
#' `birtir` runs plain `.R` scripts and turns them into structured Markdown
#' reports, saving plots and tables alongside the generated `.md` file. It also
#' provides report-ready statistical description helpers.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
## usethis namespace: end
NULL

utils::globalVariables(c(
  "x",
  "y"
))
