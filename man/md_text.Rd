\name{md_text}
\alias{md_text}
\title{Format inline Markdown text with glue interpolation}
\usage{
md_text(
  text,
  ...,
  digits = 2,
  style = c("apa", "plain"),
  trailing_zeros = TRUE,
  .envir = parent.frame()
)
}
\arguments{
\item{text}{A glue-compatible character string.}

\item{...}{Additional values passed to \code{\link[glue:glue]{glue()}}.}

\item{digits}{Number of decimal places for numeric values. Defaults to \code{2}.}

\item{style}{Number formatting style. Use \code{"apa"} to suppress leading
zeros for values between \code{-1} and \code{1}, or \code{"plain"} to keep
standard decimals.}

\item{trailing_zeros}{Logical; if \code{TRUE}, keep a fixed number of decimals.
Defaults to \code{TRUE}.}

\item{.envir}{Environment used to evaluate glue expressions. Defaults to the
caller environment.}
}
\value{
Invisibly returns the formatted Markdown text.
}
\description{
\code{md_text()} formats inline text with \code{\link[glue:glue]{glue()}} and
applies lightweight number formatting to values injected with \verb|{...}|
expressions.
}
\details{
When called inside \code{\link{render_analysis_md}()}, the formatted text is
appended to the active report as Markdown. Outside rendering, the text is
printed as a console preview and returned invisibly.
}
