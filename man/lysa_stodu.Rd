% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teikna_mynd.R
\name{lysa_stodu}
\alias{lysa_stodu}
\title{Mynd af stadsetningu nemanda a kvarda asamt lysingu}
\usage{
lysa_stodu(data, kvardi, fag)
}
\arguments{
\item{data}{einkunnir nemanda}

\item{kvardi}{tegund kvarda sem er notadur}

\item{fag}{"les" eða "stf" - ræður fjölda bakgrunsslita}
}
\value{
skilar mynd af stadsettningu nemanda
}
\description{
Mynd af stadsetningu nemanda a kvarda asamt lysingu
}
\examples{
lysa_stodu(fa_heildartolu(5), fa_kvarda())

}
