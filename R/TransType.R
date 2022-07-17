#' Transform Data type
#'
#' @param eSet eSet, A R6 class object
#' @param Vars Variables to be transformed
#' @param TypeTo Transform methods, available methods include "integer", "numeric", "character", "factor", "logical" and "date"
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table
#' @examples eSet <- TransType(eSet = eSet,
#'                   Vars = c("C1", "C5", "C6"),
#'                   TypeTo = "factor")
TransType <- function(eSet,
                      Vars,
                      TypeTo #integer, numeric, character, factor, logical, date
){
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/Transform")
  if(!file.exists(path)) {dir.create(path)}

  # Define data -----------------------------------------------------------------------------
  eSet$Expo$Data  -> df.all

  # func_data.type -----------------------------------------------------------------------------
  switch(TypeTo,
         "integer" = {
           df.all %>%
             dplyr::mutate(dplyr::across(all_of(Vars), as.integer)) -> eSet$Expo$Data
         },
         "numeric" = {
           df.all %>%
             dplyr::mutate(dplyr::across(all_of(Vars), as.numeric)) -> eSet$Expo$Data
         },
         "character" = {
           df.all %>%
             dplyr::mutate(dplyr::across(all_of(Vars), as.character)) -> eSet$Expo$Data
         },
         "factor" = {
           df.all %>%
             dplyr::mutate(dplyr::across(all_of(Vars), as.factor)) -> eSet$Expo$Data
         },
         "logical" = {
           df.all %>%
             dplyr::mutate(dplyr::across(all_of(Vars), as.logical)) -> eSet$Expo$Data
         },
         "date" = {
           df.all %>%
             dplyr::mutate(dplyr::across(all_of(Vars), as_date)) -> eSet$Expo$Data
         }
  )

  #save data --------------------------------------------------------------------------------
  ddpcr::quiet(
    eSet$Expo$Data %>%
      vroom::vroom_write(stringr::str_c(path, "/Data_TransDataTypeTo_", TypeTo, "_", NowTime,".csv"),
                         delim = ",")
  )

  ddpcr::quiet(
    eSet$Expo$Voca %>%
      vroom::vroom_write(stringr::str_c(path, "/Voca_TransDataTypeTo_", TypeTo, "_", NowTime,".csv"),
                         delim = ",")
  )

  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransType(eSet = eSet, ",
                                                     "Vars = Target variables, ",
                                                     "TypeTo = ",TypeTo, ")"))

  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #print message and save log
  message("Complete transform -> ", TypeTo, " . ",NowTime, "\n")
  eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> ", TypeTo, ". ", NowTime))

  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  eSet %>%
    save(file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()

  return(eSet)
}
