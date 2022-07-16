#' @title Demo voca data for ExpoMediation
#' @description The data includes the variable information including their full name, group name, subgroup name, data type etc.
#' @docType data
#' @usage data(medtvoca)
#' @format A tibble with 98 rows and 12 variables:
#' \describe{
#' \item{SerialNo}{a character variable indicating the variable names}
#' \item{FullName}{a character variable indicating the full name for each SerialNo}
#' \item{GroupName}{outcome, demography, chemical, immunome}
#' \item{SubgroupName}{the subgroup names inside their own groups}
#' \item{DataType}{numeric, factor}
#' }
#' @examples
#' data(medtvoca)
#' head(medtvoca)
