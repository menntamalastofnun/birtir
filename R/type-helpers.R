# Variable type helpers ---------------------------------------------------

read_variable_type <- function(x) {
  if (is.factor(x) && is.ordered(x)) {
    return("ordinal")
  }

  if (is.factor(x)) {
    return("nominal")
  }

  if (is.character(x)) {
    return("nominal")
  }

  if (is.logical(x)) {
    return("logical")
  }

  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return("datetime")
  }

  if (is.numeric(x) || is.integer(x)) {
    return("continuous")
  }

  "unknown"
}
