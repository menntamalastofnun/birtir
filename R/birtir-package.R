#' birtir: Render Analysis Scripts to Plain Markdown Reports
#'
#' `birtir` runs plain `.R` scripts and turns them into structured Markdown
#' reports, saving plots and tables alongside the generated `.md` file.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

utils::globalVariables(c(
  "difficulty",
  "einkunn",
  "fill_label",
  "group",
  "in_group",
  "item_id",
  "item_statement",
  "item_statement_wrapped",
  "label",
  "maelitala",
  "medalmaelitala",
  "point_index",
  "profhluti",
  "rett",
  "sf",
  "sfmt",
  "student_response",
  "tyngd",
  "tyngd_num",
  "umsogn",
  "x",
  "x_jitter",
  "y",
  "ymax",
  "ymin"
))
