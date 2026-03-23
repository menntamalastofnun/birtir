#' Sanitize text for safe file/folder names
#'
#' @param x Character string.
#'
#' @return A cleaned string.
#' @keywords internal
sanitize_name <- function(x) {
  x |>
    tolower() |>
    gsub("[^a-z0-9]+", "-", x = _) |>
    gsub("(^-+|-+$)", "", x = _)
}

#' Append lines to markdown state
#'
#' @param state Internal state environment.
#' @param x Character vector.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
add_md_line <- function(state, x) {
  state$md <- c(state$md, x)
  invisible(NULL)
}

#' Append a fenced text block to markdown state
#'
#' @param state Internal state environment.
#' @param x Character vector.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
add_md_text_block <- function(state, x) {
  if (length(x) == 0) {
    return(invisible(NULL))
  }

  add_md_line(state, "```text")
  add_md_line(state, x)
  add_md_line(state, "```")
  add_md_line(state, "")

  invisible(NULL)
}

#' Signal that a function belongs to the legacy workflow
#'
#' @param name Function name without parentheses.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
warn_legacy <- function(name) {
  lifecycle::deprecate_warn(
    when = "0.0.3",
    what = paste0(name, "()"),
    details = paste(
      "This function belongs to the legacy plotting workflow.",
      "It is kept temporarily while `birtir` moves to `render_analysis_md()`."
    ),
    always = FALSE
  )

  invisible(NULL)
}
