% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MedtRedX.R
\name{MedtRedX}
\alias{MedtRedX}
\title{Expoures dimension reduction}
\usage{
MedtRedX(
  eSet = eSet,
  VarsY = "Y1",
  VarsC = eSet$Expo$Voca \%>\% dplyr::filter(str_detect(SerialNo, "^C")) \%>\%
    dplyr::select(SerialNo) \%>\% as.matrix() \%>\% as.vector(),
  VarsM = NULL,
  Method = "gcdnet",
  Iter = 500
)
}
\arguments{
\item{eSet}{eSet, A R6 class object}

\item{VarsY}{A character indicating the name of outcome}

\item{VarsC}{A character vector indicating the name of covariates}

\item{VarsM}{A character vector indicating the name of mediator}

\item{Method}{Dimension reduction method, available options include "gcdnet"(default) and "mean"}

\item{Iter}{Iterations of mediation models,default is 500}
}
\value{
eSet, A R6 class object
}
\description{
Expoures dimension reduction
}
\examples{
eSet <- MedtRedX(eSet = eSet,
                 VarsY = "Y1",
                 VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
                 VarsM = c("M1","M2","M3"),
                 Method = "mean",
                 Iter = 500)
}
