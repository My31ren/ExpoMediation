#' Title
#'
#' @param eSet A R6 class object
#'
#' @return A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table caret
#' @examples eSet <- DelNearZeroVar(eSet = eSet)
DelNearZeroVar <- function(eSet){

  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut,"/DeleteVars")
  if(!file.exists(path)) {dir.create(path)}

  #get the Vars names
  if(nrow(eSet$Expo$Data) > 0){
    ddpcr::quiet(names(eSet$Expo$Data) %>%
        stringr::str_extract_all("^[XCMG].*",
                                 simplify = T) %>%
        tibble::as_tibble() %>%
        dplyr::filter(V1 != "") %>%
        .$V1 -> VarsXCMG)

    names(eSet$Expo$Data) %>%
      stringr::str_extract_all("^[SYCXMG].*",
                               simplify = T) %>%
      tibble::as_tibble() %>%
      dplyr::filter(V1 != "") %>%
      .$V1 -> Vars.all

    #get the names of the deleted features
    eSet$Expo$Data %>%
      dplyr::select(all_of(VarsXCMG)) %>%
      caret::nearZeroVar(saveMetrics = T,
                         names = T) %>%
      dplyr::filter(nzv == "TRUE") %>%
      rownames() -> VarsDeleted

    #get the kept and deleted data
    eSet$Expo$Data %>%
      dplyr::select(all_of(VarsDeleted)) -> eSet$ExpoDel$Data
    eSet$Expo$Voca%>%
      dplyr::filter(SerialNo %in% VarsDeleted) -> eSet$ExpoDel$Voca

    eSet$Expo$Data %>%
      dplyr::select(-all_of(VarsDeleted)) -> eSet$Expo$Data
    eSet$Expo$Voca%>%
      dplyr::filter(!SerialNo %in% VarsDeleted) -> eSet$Expo$Voca

    ddpcr::quiet(
      eSet$Expo$Data %>%
        vroom::vroom_write(stringr::str_c(path, "/ExpoData_", NowTime,".csv"),
                           delim = ",")
    )

    ddpcr::quiet(
      eSet$Expo$Voca %>%
        vroom::vroom_write(stringr::str_c(path, "/ExpoVoca_", NowTime,".csv"),
                           delim = ",")
    )

    ddpcr::quiet(
      eSet$ExpoDel$Data %>%
        vroom::vroom_write(stringr::str_c(path, "/ExpoDataDeleted_", NowTime,".csv"),
                           delim = ",")
    )

    ddpcr::quiet(
      eSet$ExpoDel$Voca %>%
        vroom::vroom_write(stringr::str_c(path, "/ExpoVocaDeleted_", NowTime,".csv"),
                           delim = ",")
    )

    message("Delete ", ncol(eSet$ExpoDel$Data)," from ",
            ncol(eSet$Expo$Data)+ncol(eSet$ExpoDel$Data), " features (remainning: ",
            ncol(eSet$Expo$Data), ") with zero or near zero variance. ",
            NowTime, "\n")

    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Delete ", ncol(eSet$ExpoDel$Data)," from ",
                                                      ncol(eSet$Expo$Data)+ncol(eSet$ExpoDel$Data), " features (remainning: ",
                                                      ncol(eSet$Expo$Data), ") with zero or near zero variance. ",
                                                      NowTime))
  }else{
    message("Error: The exposome data is empty! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: The exposome data is empty! ", NowTime))
  }

  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- DelNearZeroVar(eSet = eSet)"))

  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #save updated variables
  eSet$Expo$Voca %>%
    dplyr::select(SerialNo:FullName) %>%
    dplyr::filter(SerialNo != "Group") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/variables.csv"))

  #save log
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  eSet %>%
    save(file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()

  return(eSet)
}

