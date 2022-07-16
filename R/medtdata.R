#' @title Demo data for ExpoMediation
#' @description The data includes ID information (SampleID and SubjectID), Group, Outcome (Y1), Covariates, Exposures and Mediators.
#' @docType data
#' @usage data(medtdata)
#' @format A tibble with 161 rows and 101 variables:
#' \describe{
#' \item{SampleID}{a numeric variable indicating sample id information}
#' \item{SubjectID}{a numeric variable indicating subject id information, each subject may have several sample id}
#' \item{C1-C6}{covariates}
#' \item{X1-X38}{exposures}
#' \item{M1-M53}{mediators}
#' }
#' @examples
#' data(medtdata)
#' head(medtdata)
