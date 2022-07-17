

#' Transform for data imputation
#'
#' @param eSet A R6 class object containning the dataset
#' @param Group True/False, indicating whether execute imputation by group variable
#' @param Vars Variables to be imputed
#' @param Method Imputation methods, available methods include "cart", "lod"
#'
#' @return eSet, a R6 class object
#' @export
#' @import tictoc lubridate tidyverse mice tidyr ddpcr vroom data.table
#' @examples eSet <- TransImput(eSet = eSet,
#'                    Group = F,
#'                    Vars = c("X3","M22","M29"),
#'                    Method = "lod")

TransImput <- function(eSet,
                       Group = T, #T F
                       Vars,
                       Method = NULL
){
  tictoc::tic()

  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/Transform")
  if(!file.exists(path)) {dir.create(path)}

  # Define data -----------------------------------------------------------------------------
  eSet$Expo$Data  -> df.all

  eSet$Expo$Data %>%
    dplyr::filter(Group == "train") -> df.train

  eSet$Expo$Data %>%
    dplyr::filter(Group == "test") -> df.test


  # func_imputation ----------------------------------------------------------------
  switch(Method,
         "cart" = {
           if(!Group){
             df.all %>%
               dplyr::select(all_of(Vars)) %>%
               mice::mice(m = 5,
                          seed = 1,
                          method = "cart",
                          print = FALSE) %>%
               tidyr::complete() %>%
               tibble::as_tibble() -> df.temp

             df.all %>%
               dplyr::select(-all_of(Vars)) %>%
               cbind(df.temp) %>%
               tibble::as_tibble() %>%
               dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
           }else{
             df.train %>%
               dplyr::select(all_of(Vars)) %>%
               mice::mice(m = 5,
                          seed = 1,
                          method = "cart",
                          print = FALSE) %>%
               tidyr::complete() %>%
               tibble::as_tibble() -> df.temp1

             df.test %>%
               dplyr::select(all_of(Vars)) %>%
               mice::mice(m = 5,
                          seed = 1,
                          method = "cart",
                          print = FALSE) %>%
               tidyr::complete() %>%
               tibble::as_tibble() -> df.temp2

             df.all %>%
               dplyr::select(-all_of(Vars)) %>%
               cbind(rbind(df.temp1, df.temp2)) %>% #combine the train and test dataset
               tibble::as_tibble() %>%
               dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
           }

         },

         "lod" = {
           eSet$Expo$Voca %>%
             dplyr::select(SerialNo, Lod) %>%
             dplyr::filter(SerialNo %in% Vars) %>%
             dplyr::filter(!Lod %in% c(NA)) -> df.Lod

           if(nrow(df.Lod) > 0){
             if(!Group){
               df.all %>%
                 dplyr::select(SampleID, df.Lod$SerialNo) %>%
                 tidyr::pivot_longer(-SampleID,
                                     names_to = "SerialNo",
                                     values_to = "values") %>%
                 dplyr::left_join(df.Lod,
                                  by = "SerialNo") %>%
                 dplyr::distinct(SampleID,
                          SerialNo,
                          .keep_all = T) %>%
                 dplyr::mutate("ValueLod" = dplyr::case_when(values >= Lod &
                                                        !is.na(values) ~ values,
                                                      values < Lod |
                                                        is.na(values) ~ Lod/sqrt(2),
                                                      T ~ Lod/sqrt(2))
                 ) %>%
                 dplyr::select(SampleID,
                               SerialNo,
                               ValueLod) %>%
                 tidyr::pivot_wider(SampleID,
                                    names_from = SerialNo,
                                    values_from = ValueLod)  -> df.temp

               df.all %>%
                 dplyr::select(-df.Lod$SerialNo) %>%
                 dplyr::left_join(df.temp,
                           by = "SampleID") %>%
                 dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
             }else{
               df.train %>%
                 dplyr::select(SampleID, df.Lod$SerialNo) %>%
                 tidyr::pivot_longer(-SampleID,
                                     names_to = "SerialNo",
                                     values_to = "values") %>%
                 dplyr::left_join(df.Lod,
                                  by = "SerialNo") %>%
                 dplyr::distinct(SampleID,
                          SerialNo,
                          .keep_all = T) %>%
                 dplyr::mutate("ValueLod" = dplyr::case_when(values >= Lod &
                                                        !is.na(values) ~ values,
                                                      values < Lod |
                                                        is.na(values) ~ Lod/sqrt(2),
                                                      T ~ Lod/sqrt(2))
                 ) %>%
                 dplyr::select(SampleID,
                               SerialNo,
                               ValueLod) %>%
                 tidyr::pivot_wider(SampleID,
                                    names_from = SerialNo,
                                    values_from = ValueLod)  -> df.temp1

               df.test %>%
                 dplyr::select(SampleID, df.Lod$SerialNo) %>%
                 tidyr::pivot_longer(-SampleID,
                                     names_to = "SerialNo",
                                     values_to = "values") %>%
                 dplyr::left_join(df.Lod,
                                  by = "SerialNo") %>%
                 dplyr::distinct(SampleID,
                          SerialNo,
                          .keep_all = T) %>%
                 dplyr::mutate("ValueLod" = dplyr::case_when(values >= Lod &
                                                        !is.na(values) ~ values,
                                                      values < Lod |
                                                        is.na(values) ~ Lod/sqrt(2),
                                                      T ~ Lod/sqrt(2))
                 ) %>%
                 dplyr::select(SampleID,
                               SerialNo,
                               ValueLod) %>%
                 tidyr::pivot_wider(SampleID,
                                    names_from = SerialNo,
                                    values_from = ValueLod)  -> df.temp2


               df.all %>%
                 dplyr::select(-df.Lod$SerialNo) %>%
                 dplyr::left_join(rbind(df.temp1,df.temp2), #combine the train and test dataset
                           by = "SampleID") %>%
                 dplyr::select(names(eSet$Expo$Data)) -> eSet$Expo$Data
             }
           }else{
             message("Please add the LOD data! ", NowTime, "\n")
           }
         }
  )


  # execute--------------------------------------------------------------------------------
  ddpcr::quiet(
    eSet$Expo$Data %>%
      vroom::vroom_write(stringr::str_c(path, "/Data_TransImputBy_", Method,"_", NowTime, ".csv"),
                         delim = ",")
  )
  ddpcr::quiet(
    eSet$Expo$Voca %>%
      vroom::vroom_write(stringr::str_c(path, "/Voca_TransImputBy_", Method,"_", NowTime, ".csv"),
                         delim = ",")
  )
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- TransImput(eSet = eSet, Vars = Target X variables, ",
                                                     "Group = ",Group, ", ",
                                                     "Vars = Target variables, ",
                                                     "Method = ",Method,
                                                     ")"))
  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))


  #print message and save log
  message("Complete imputation by ", Method, " method! ", NowTime, "\n")
  eSet$ExcecutionLog  <- eSet$AddLog(stringr::str_c("Complete imputation by ", Method, " method! ", NowTime))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

  #save eSet
  eSet %>%
    save(file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()
  return(eSet)
}
