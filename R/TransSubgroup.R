
#' Divide exposures into separate groups according to the dictionary dataset
#'
#' @param eSet eSet, A R6 class object
#' @param voca Vocabulary dataset containing variable information
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table
#' @examples eSet <- MedtXlist(eSet = eSet)
MedtXlist = function(eSet,
                     voca = eSet$Expo$Voca){
  tictoc::tic()
  #group index
  eSet$Expo$Voca %>%
    dplyr::filter(stringr::str_detect(SerialNo, "^X")) %>%
    dplyr::distinct(SubgroupName) %>%
    as.matrix() %>%
    as.vector() -> xgroup
  #loop function
  F1 = function(xgroup){
    voca %>%
      dplyr::filter(SubgroupName == xgroup) %>% #class needs to be assigned
      dplyr::select(SerialNo) %>%
      as.matrix() %>%
      as.vector() -> idx
    tmp <- eSet$Expo$Data[, idx]
    return(tmp)
  }
  #execute
  eSet$Expo$ExpoList <- list()
  eSet$Expo$ExpoList <- purrr::map(xgroup, F1)
  names(eSet$Expo$ExpoList) <- xgroup
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand("eSet <- MedtXlist(eSet = eSet)")
  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #message and log
  message(stringr::str_c("Complete dividing exposures into subgroups!","\n","The users can check the results in eSet$Expo$ExpoList for details.", "\n", lubridate::now()), "\n")
  eSet$ExcecutionLog <- eSet$AddLog(stringr::str_c("Complete dividing exposures into subgroups! ", lubridate::now()))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
  #save eSet
  save(eSet,
       file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()
  return(eSet)
}

#' Divide mediators into separate groups according to the dictionary dataset
#'
#' @param eSet eSet, A R6 class object
#' @param voca Vocabulary dataset containing variable information
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table
#' @examples eSet <- MedtMlist(eSet = eSet)
MedtMlist = function(eSet,
                     voca = eSet$Expo$Voca){
  tictoc::tic()
  #group index
  voca %>%
    dplyr::filter(stringr::str_detect(SerialNo, "^M")) %>%
    dplyr::distinct(SubgroupName) %>%
    as.matrix() %>%
    as.vector() -> mgroup
  #loop function
  F1 = function(mgroup){
    voca %>%
      dplyr::filter(SubgroupName == mgroup) %>% #class needs to be assigned
      dplyr::select(SerialNo) %>%
      as.matrix() %>%
      as.vector() -> idx
    tmp <- eSet$Expo$Data[, idx]
    return(tmp)
  }
  #execute
  eSet$Expo$MediList <- list()
  eSet$Expo$MediList <- purrr::map(mgroup, F1)
  names(eSet$Expo$MediList) <- mgroup
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand("eSet <- MedtMlist(eSet = eSet)")
  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))
  #message and log
  message(stringr::str_c("Complete dividing mediators into subgroups!","\n","The users can check the results in eSet$Expo$MediList for details.", "\n", lubridate::now()), "\n")
  eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete dividing mediators into subgroups! ", lubridate::now()))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
  #save eSet
  save(eSet,
       file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()
  return(eSet)
}
