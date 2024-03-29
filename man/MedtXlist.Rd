% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TransSubgroup.R
\name{MedtXlist}
\alias{MedtXlist}
\title{Divide exposures into separate groups according to the dictionary dataset}
\usage{
MedtXlist(eSet, voca = eSet$Expo$Voca)
}
\arguments{
\item{eSet}{eSet, A R6 class object}

\item{voca}{Vocabulary dataset containing variable information}
}
\value{
eSet, A R6 class object
}
\description{
Divide exposures into separate groups according to the dictionary dataset
}
\examples{
eSet <- MedtXlist(eSet = eSet)
}
