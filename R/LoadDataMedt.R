#' Load data for mediation analyses
#'
#' @param eSet A R6 class containing all information that users have provided
#' @param UseExample True/False, whether use example data
#' @param FileDirExpo The name of data file to be uploaded
#' @param FileDirVoca The name of vocabulary file to be uploaded
#'
#' @return eSet, a R6 class object
#' @export
#' @import data.table vroom lubridate tidyverse R6 tictoc ddpcr
#' @examples eSet <- LoadData(eSet = eSet,
#' UseExample = "example1",
#' FileDirExpo = NULL,
#' FileDirVoca = NULL)

LoadDataMedt <- function(eSet,
                     UseExample = "default",
                     FileDirExpo = NULL,
                     FileDirVoca = NULL)
  {
  tictoc::tic()

  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/Load")
  if(!file.exists(path)) {dir.create(path)}

  #find the detailed FileDirExpo
  switch(UseExample,
         "default" = {
           stringr::str_c("input_", eSet$PID, "/", FileDirExpo) -> FileDirExpo
           stringr::str_c("input_", eSet$PID, "/", FileDirVoca) -> FileDirVoca

         ddpcr::quiet(
           if(stringr::str_sub(FileDirExpo,-4,-1) == "xlsx"){
             readxl::read_xlsx(FileDirExpo) -> eSet$Expo$Data
           }else{
             vroom::vroom(FileDirExpo,
                          show_col_types = F) -> eSet$Expo$Data
           })

         ddpcr::quiet(
           if(stringr::str_sub(FileDirVoca,-4,-1) == "xlsx"){
             readxl::read_xlsx(FileDirVoca) -> eSet$Expo$Voca
           }else{
             vroom::vroom(FileDirVoca,
                          show_col_types = F) -> eSet$Expo$Voca
           })
         },
         "example1" = {
           medtdata -> eSet$Expo$Data
           medtvoca -> eSet$Expo$Voca
         }
  )#end switch
  eSet$Expo$Data %>%
    names() -> VarsName

  # check the name of Exposome file-------------------------------------------------------------------

  if(!VarsName[1] %in% "SampleID"){
    message("Error: Please rename the sample column to 'SampleID'")
    flag1 = F
  }else{
    flag1 = T
  }

  if(!VarsName[2] %in% "SubjectID"){
    message("Error: Please rename the subject column to 'SubjectID'")
    flag2 = F
  }else{
    flag2 = T
  }
  if(!VarsName[3] %in% "Group"){
    message("Error: Please rename the group column tp 'Group' ")
    flag3 = F
  }else{
    flag3 = T
  }
  if(!VarsName[4] %>% stringr::str_detect("^Y")){
    message("Error: Please rename the outcome column with 'Y' as beginning")
    flag4 = F
  }else{
    flag4 = T
  }
  if(any(!VarsName[5:length(VarsName)] %>% stringr::str_detect("(^X)|(^M)|(^C)"))){
    message("Error: Please rename the exposome variable columns with 'X', the mediator variable columns with 'M', and covariate variable columns with 'C' as beginning")
    flag5 = F
  }else{
    flag5 = T
  }

  if(all(c(flag1,flag2, flag3,flag4,flag5))){
    message("Complete loading Exposome file! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete loading Exposome file!", NowTime))

    eSet$Expo$Voca %>%
      dplyr::mutate(SerialNo_Raw = SerialNo) %>%
      dplyr::select(SerialNo, SerialNo_Raw, everything()) -> eSet$Expo$Voca

    #save data
    ddpcr::quiet(
      eSet$Expo$Data %>%
        vroom::vroom_write(stringr::str_c(path, "/ExpoData_",NowTime, ".csv"),
                           delim = ",")
    )
    ddpcr::quiet(
      eSet$Expo$Voca %>%
        vroom::vroom_write(stringr::str_c(path, "/ExpoVoca_",NowTime, ".csv"),
                           delim = ",")
    )
  }else{
    message("Error: Fail to load Exposome file! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Error: Fail to load Exposome file!", NowTime))
    eSet$Expo$Voca  <- NULL
    eSet$Expo$Data <- NULL
  }
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- LoadData(eSet = eSet, FileDirExpo = Exposome data file director, FileDirVoca = Exposome vocabulary file directory)"))
  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #save data and log
  eSet$Expo$Voca %>%
    dplyr::select(SerialNo:FullName) %>%
    dplyr::filter(SerialNo != "Group") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/variables.csv"))
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
