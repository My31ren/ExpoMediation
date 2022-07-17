#' Delete variables that contain missing values
#'
#' @param eSet A R6 class object
#'
#' @return A R6 class object
#' @export
#' @import tictoc lubridate tidyverse naniar ddpcr vroom data.table
#' @examples eSet <- DelMiss(eSet = eSet)
DelMiss <- function(eSet){

  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/DeleteVars")
  if(!file.exists(path)) {dir.create(path)}

  #get the missing information
  eSet$Expo$Data %>%
    naniar::miss_var_summary() %>%  #to be improved for the vars with chr type
    dplyr::filter(n_miss > 0) %>%
    purrr::set_names("vars",
                     "number_miss",
                     "percent_miss") -> temp1

  temp1 -> eSet$StatMiss
  #save the missing information
  if(nrow(temp1) > 0){
    ddpcr::quiet(
      temp1 %>%
        vroom::vroom_write(stringr::str_c(path, "/StatMissing.csv"),
                           delim = ",")
    )
  }

  #update the kept and deletedexposome datasets
  if(length(eSet$ExpoDel$Data) > 0){
    eSet$ExpoDel$Data %>%
      cbind(eSet$Expo$Data  %>%
              dplyr::select(temp1$vars)) %>%
      tibble::as_tibble() -> eSet$ExpoDel$Data


    eSet$ExpoDel$Voca %>%
      rbind(eSet$Expo$Voca  %>%
              dplyr::filter(SerialNo %in% temp1$vars)) ->  eSet$ExpoDel$Voca
  }else{
    eSet$Expo$Data  %>%
      dplyr::select(temp1$vars)-> eSet$ExpoDel$Data


    eSet$Expo$Voca  %>%
      dplyr::filter(SerialNo %in% temp1$vars) ->  eSet$ExpoDel$Voca
  }


  eSet$Expo$Data  %>%
    dplyr::select(-temp1$vars) -> eSet$Expo$Data

  eSet$Expo$Voca  %>%
    dplyr::filter(!SerialNo %in% temp1$vars) ->  eSet$Expo$Voca

  ddpcr::quiet(
    eSet$Expo$Data %>%
      vroom::vroom_write(stringr::str_c(path, "/ExpoData_",NowTime,".csv"),
                         delim = ",")
  )

  ddpcr::quiet(
    eSet$Expo$Voca  %>%
      vroom::vroom_write(stringr::str_c(path, "/ExpoVoca_",NowTime,".csv"),
                         delim = ",")
  )

  ddpcr::quiet(
    eSet$ExpoDel$Data %>%
      vroom::vroom_write(stringr::str_c(path, "/ExpoDataDeleted_",NowTime,".csv"),
                         delim = ",")
  )

  ddpcr::quiet(
    eSet$ExpoDel$Voca  %>%
      vroom::vroom_write(stringr::str_c(path, "/ExpoVocaDeleted_",NowTime,".csv"),
                         delim = ",")
  )

  #save updated variables
  eSet$Expo$Voca %>%
    dplyr::select(SerialNo:FullName) %>%
    dplyr::filter(SerialNo != "Group") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/variables.csv"))

  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- DelMiss(eSet = eSet)"))

  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #print message and save log
  message("Delete ", nrow(temp1)," from ", ncol(eSet$Expo$Data)+nrow(temp1),
          " features (remainning:", ncol(eSet$Expo$Data),") with missing values > 1.", " ",
          NowTime, "\n")

  eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Delete ", nrow(temp1)," in ",
                                                    ncol(eSet$Expo$Data),
                                                    " features with missing values > 1. ",
                                                    NowTime))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  eSet %>%
    save(file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()

  return(eSet)
}
