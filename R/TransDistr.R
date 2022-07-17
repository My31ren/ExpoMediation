#' Transform distribution
#'
#' @param eSet eSet, A R6 class object
#' @param Vars Variables to be transformed
#' @param Method Transform distribution methods, including "ln", "log2" and "log10"
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table
#' @examples eSet <- TransDistr(eSet = eSet,
#'                    Vars = c("X1", "X2", "X3"),
#'                    Method = "log10")
TransDistr <- function(eSet,
                       Vars,
                       Method)
  {
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/Transform")
  if(!file.exists(path)) {dir.create(path)}


  # Define data -----------------------------------------------------------------------------
  eSet$Expo$Data  -> df.all

  # func_trans.distribution ---------------------------------------------------------------------------
  df.all %>%
    dplyr::select(all_of(Vars)) %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(where(~ min(.x, na.rm = TRUE) > 0)) %>%
    colnames() -> Vars

  if(length(Vars) > 0){
    switch(Method,
           "ln" = {
             df.all %>%  #dplyr::across the function with the vector of target variables
               dplyr::select(all_of(Vars)) %>%
               dplyr::mutate(dplyr::across(where(is.numeric),
                                           log)) -> df.temp
             df.temp %>%
               cbind(df.all %>%
                       dplyr::select(-all_of(Vars))) %>%
               tibble::as_tibble() %>%
               dplyr::select(names(df.all)) -> eSet$Expo$Data
           },

           "log2" = {
             eSet$Expo$Data %>%  #dplyr::across the function with the vector of target variables
               dplyr::select(all_of(Vars)) %>%
               dplyr::mutate(dplyr::across(where(is.numeric),
                                           log2)) -> df.temp
             df.temp %>%
               cbind(df.all %>%
                       dplyr::select(-all_of(Vars))) %>%
               tibble::as_tibble() %>%
               dplyr::select(names(df.all)) -> eSet$Expo$Data
           },

           "log10" = {
             df.all %>%  #dplyr::across the function with the vector of target variables
               dplyr::select(all_of(Vars)) %>%
               dplyr::mutate(dplyr::across(where(is.numeric),
                                           log10)) -> df.temp
             df.temp %>%
               cbind(df.all %>%
                       dplyr::select(-all_of(Vars))) %>%
               tibble::as_tibble() %>%
               dplyr::select(names(df.all)) -> eSet$Expo$Data
           }
    )
  }else{
    message("Error: These group of variables can not be classified!", NowTime, "\n")
  }

  #execute task --------------------------------------------------------------------------------
  ddpcr::quiet(
    eSet$Expo$Data %>%
      vroom::vroom_write(stringr::str_c(path, "/Data_TransDistrBy_", Method,".csv"),
                         delim = ",")
  )

  ddpcr::quiet(
    eSet$Expo$Voca %>%
      vroom::vroom_write(stringr::str_c(path, "/Voca_TransDistrBy_", Method,".csv"),
                         delim = ",")
  )
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransDistr(eSet = eSet, Vars = Target variables, ",
                                                     "Method = ",Method,")"))

  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #print message and save log
  message("Complete transform -> Distribution! ", NowTime, "\n")
  eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> distribution! ", NowTime))

  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  eSet %>%
    save(file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()

  return(eSet)
}
