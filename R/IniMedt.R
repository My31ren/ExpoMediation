
#' Initialize for ExpoMediation
#'
#' @param PID A unique identification for user
#' @param FileDirIn The physical path that contains the input files
#' @param FileDirOut The output path that stores the result files.
#' @param Module Mediation
#'
#' @return eSet, a R6 class object
#' @export
#' @import data.table vroom lubridate tidyverse R6 tictoc ddpcr
#' @examples
#' eSet <- IniMedt(PID = "R1",
#' FileDirIn = "default",
#' FileDirOut = "default",
#' Module = "Mediation")

IniMedt <- function(PID = "auto",
                    FileDirIn = "default",
                    FileDirOut = "default",
                    Module = NULL)
{
  #record time
  tictoc::tic()

  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime
  # create R6 class eSet ----------------------------------------------------
  eSet <- R6::R6Class(
    "eSet",
    public = list(
      PID = NULL,
      EpiDesign = NULL,
      DataStr = NULL,
      FileDirIn = NULL,
      FileDirOut = NULL,
      ExcecutionLog = NULL,
      Expo = list(Data = NULL,
                  Voca = NULL),
      ExpoDel = list(Data = NULL,
                     Voca = NULL),
      AddLog = function(x){
        self$ExcecutionLog = c(self$ExcecutionLog,
                               stringr::str_c(x))},

      AddCommand = function(x){
        self$RCommandLog = c(self$RCommandLog,
                             stringr::str_c(x))},

      initialize = function(FileDirIn,
                            FileDirOut) {
        self$FileDirOut = FileDirIn
        self$FileDirOut = FileDirOut
      }
    ),

    lock_class = FALSE,
    lock_objects = FALSE
  )

  eSet <- eSet$new(FileDirIn = FileDirIn,
                   FileDirOut = FileDirOut)

  if(PID != "auto"){
    eSet$PID <- PID
  }else{
    eSet$PID <- stringr::str_c(lubridate::now() %>%
                                 stringr::str_remove_all(" ") %>%
                                 stringr::str_remove_all("-") %>%
                                 stringr::str_remove_all(":") %>%
                                 stringr::str_sub(9,-1),
                               collapse = "") %>%
      stringr::str_c(sample(c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X","Y","Z"),
                            6, replace = TRUE) %>%
                       stringr::str_c(collapse = ""))
  }

  # initialize the folder ---------------------------------------------------
  # FileDirIn
  if(FileDirIn == "default"){
    stringr::str_c(getwd(),
                   "/input_",
                   eSet$PID) -> eSet$FileDirIn

    if(!file.exists(eSet$FileDirIn)){
      dir.create(eSet$FileDirIn)
    }
  }else if(!file.exists(FileDirIn)){
    stringr::str_c(FileDirIn,
                   "/input_",
                   eSet$PID) -> eSet$FileDirIn
    dir.create(eSet$FileDirIn)
  }
  # FileDirOut
  if(FileDirOut == "default"){
    stringr::str_c(getwd(),
                   "/output_",
                   eSet$PID) -> eSet$FileDirOut

    if(!file.exists(eSet$FileDirOut)){
      dir.create(eSet$FileDirOut)
    }
  }else if(!file.exists(FileDirOut)){
    stringr::str_c(FileDirOut,
                   "/output_",
                   eSet$PID) -> eSet$FileDirOut

    dir.create(eSet$FileDirOut)
  }
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- InitExpoData(PID = Any ID your like, FileDirOut = Any output file directory your like)"))
  eSet$RCommandLog %>%
    as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  message("Complete initializing the '", Module, "' module. \n",lubridate::now(),"\n")

  eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete initializing the", Module,"module. \n", lubridate::now(),"\n"))
  ddpcr::quiet(eSet$ExcecutionLog %>%
                 as_tibble() %>%
                 purrr::set_names("running log") %>%
                 data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
  )
  #save eSet
  save(eSet,
       file = str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()
  return(eSet)
}
