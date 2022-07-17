#' Transform (factor) variables into dummy variables
#'
#' @param eSet eSet, A R6 class object
#' @param Vars Variables (factor) to be transformed
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table modelr
#' @examples eSet <- TransDummy(eSet = eSet,
#'                    Vars = c("C1", "C5", "C6"))
TransDummy <- function(eSet,
                       Vars = "default"
){

  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/Transform")
  if(!file.exists(path)) {dir.create(path)}

  # Define data ---------------------------------------------
  eSet$Expo$Data  -> df.all

  # FuncDummySingle -----------------------------------------
  FuncDummySingle <- function(Var){
    df.all %>%
      dplyr::select(all_of(Var)) %>%
      modelr::model_matrix(as.formula(stringr::str_c("~", Var))) %>%
      dplyr::select(-1) %>%
      purrr::set_names(stringr::str_c(Var, ".", 2:length(unique(df.all[[Var]])))) -> temp

    return(temp)
  }

  #reshape data
  if(all(Vars == "default")){
    df.all %>%
      dplyr::select(!contains("Y")) %>%
      dplyr::select(where(is.factor)) %>%
      names() -> Vars
  }else{
    df.all %>%
      dplyr::mutate(dplyr::across(all_of(Vars), as.factor)) ->  eSet$Expo$Data
  }

  if(length(Vars) > 0){
    purrr::map_dfc(Vars, FuncDummySingle) %>%
      cbind(df.all %>%
          dplyr::select(-all_of(Vars))) %>%
          tibble::as_tibble() -> temp1

    temp1 %>%
      names() %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_detect(value, "^X")) %>%
      dplyr::mutate(value = as.numeric(stringr::str_remove(value, "X"))) %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(value = stringr::str_c("X", value)) %>%
      .$value -> VarsX

    temp1 %>%
      names() %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_detect(value, "^C")) %>%
      dplyr::mutate(value = as.numeric(stringr::str_remove(value, "C"))) %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(value = stringr::str_c("C", value)) %>%
      .$value -> VarsC

    temp1 %>%
      names() %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_detect(value, "^M")) %>%
      dplyr::mutate(value = as.numeric(stringr::str_remove(value, "M"))) %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(value = stringr::str_c("M", value)) %>%
      .$value -> VarsM

    temp1 %>%
      names() %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_detect(value, "^Y")) %>%
      dplyr::mutate(value = as.numeric(stringr::str_remove(value, "Y"))) %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(value = stringr::str_c("Y", value)) %>%
      .$value -> VarsY

    temp1 %>%
      dplyr::select("SampleID",
                    "SubjectID",
                    "Group",
                    all_of(VarsY),
                    all_of(VarsC),
                    all_of(VarsM),
                    all_of(VarsX)) -> eSet$Expo$Data

    #save expo data
    ddpcr::quiet(
      eSet$Expo$Data %>%
        vroom::vroom_write(stringr::str_c(path, "/Data_Trans_Dummy.csv"),
                           delim = ",")
    )

    #reshape vocabulary
    tibble::tibble(SerialNo = names(eSet$Expo$Data)) %>%
      dplyr::slice(-c(1:2)) %>%
      dplyr::mutate(SerialNo_Raw = stringr::str_remove(SerialNo, "\\..*")) %>%
      dplyr::left_join(eSet$Expo$Voca,
                       by = c("SerialNo_Raw" = "SerialNo")) %>%
      dplyr::select(SerialNo, everything()) -> eSet$Expo$Voca

    #save expo vocabulary
    ddpcr::quiet(
      eSet$Expo$Voca %>%
        vroom::vroom_write(stringr::str_c(path, "/Voca_Trans_Dummy.csv"),
                           delim = ",")
    )

    message("Complete transform -> dummy! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete transform -> dummy! ", NowTime))
  }else{
    message("No factor Variables need transforming -> dummy! ", NowTime, "\n")
    eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("No factor Variables need transforming -> dummy! ", NowTime))
  }

  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- CrosPred(eSet = eSet, Vars = Target X variables)"))

  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))

  #save updataed variables
  eSet$Expo$Voca %>%
    dplyr::select(SerialNo,
                  SerialNo_Raw,
                  FullName) %>%
    dplyr::filter(SerialNo != "Group") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/variables.csv"))

  #save data and log
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  eSet %>%
    save(file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()

  return(eSet)
}
