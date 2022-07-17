#input:
#M: either a list(groups of mediators) or a tibble(all mediators).
#Y: a vector indicating outcome, ONLY continuous type is available
#X: a dataframe or matrix indicating exposures
#C: a dataframe or matrix indicating covariates
#' Calculate the importance of each mediator
#'
#' @param eSet eSet, A R6 class object
#' @param VarsY A character indicating the name of outcome
#' @param VarsX A character vector indicating the name of exposures
#' @param VarsC A character vector indicating the name of covariates
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse naniar ddpcr vroom data.table mediation bama HIMA
#' @examples eSet <- MedtImptM(eSet = eSet,
#'                   VarsY = "Y1",
#'                   VarsX = c("X1", "X2", "X3"),
#'                   VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"))
MedtImptM = function(eSet = eSet,
                     VarsY = NULL,
                     VarsX = eSet$Expo$Voca %>%
                       dplyr::filter(GroupName == "chemical") %>%
                       dplyr::select(SerialNo) %>%
                       as.matrix() %>%
                       as.vector(),
                     VarsC = eSet$Expo$Voca %>%
                       dplyr::filter(stringr::str_detect(SerialNo, "^C")) %>%
                       dplyr::select(SerialNo) %>%
                       as.matrix() %>%
                       as.vector())
{
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime
  #path
  path = stringr::str_c(eSet$FileDirOut, "/MImportance")
  if(!file.exists(path)){dir.create(path)}

  Y = eSet$Expo$Data[, VarsY]
  X = eSet$Expo$Data[, VarsX]
  C = eSet$Expo$Data[, VarsC]

  MImportance = function(M){

    #loop function
    F3 = function(m, m_grpname){
      #bama Loop function
      #X: dataframe containing exposures
      #name: colnames of X, indicating the name list of exposure
      F1 = function(X, name){
        x <- as.vector(as.matrix(X))#define individual exposure
        y <- as.vector(as.matrix(Y))#define outcome
        c <- as.matrix(C)#define covariates
        set.seed(111)
        #the recommended burnin=30000, and ndraws=35000
        fit.bama <- bama::bama(y, x, m, c, c, method = "BSLMM", burnin=3000, ndraws=3500)
        tmp <- summary(fit.bama)[4]
        tmp %<>%
          dplyr::mutate(exposure = name,
                        mediator = rownames(.),
                        mediator_group = m_grpname) %>%
          dplyr::select(exposure, mediator, mediator_group, pip)
        return(tmp)
      }#end F1
      # hima Loop function
      #X: dataframe containing exposures
      #name: colnames of X, indicating the name list of exposure
      F2 = function(X,name){
        #inputs:
        #X: exposures (vector)
        #name: name of exposures (character type)
        x <- as.matrix(X)
        y <- as.matrix(Y) #outcome
        c <- as.matrix(C) #covariates

        hima.fit <- HIMA::hima(x, y, m, COV.XM = c, COV.MY = c, family = "gaussian")
        hima.fit %<>%
          dplyr::mutate(exposure = name,
                        mediator = rownames(.),
                        mediator_group = m_grpname) %>%
          dplyr::select(exposure, mediator, mediator_group, everything())
        return(hima.fit)
      }#end F2

      #execute loop functions
      bama.result <- purrr::map2_dfr(X, colnames(X),F1)
      hima.result <- purrr::map2_dfr(X, colnames(X), F2)
      #bind results
      results <- dplyr::full_join(bama.result, hima.result, by = c("exposure", "mediator", "mediator_group"))

      return(results)
    }#end F3

    #if
    if (tibble::is_tibble(M)==TRUE) {
      m_grpname = "All mediators"
      m <- as.matrix(M)
      ddpcr::quiet(eSet$MedtImptM$all <- F3(m, m_grpname))

      eSet$MedtImptM$all %>%
        vroom::vroom_write(stringr::str_c(path, "/MedtImptM-all-mediators-",NowTime, ".csv"), ",")

      #message and log
      message(stringr::str_c("Complete calculating MImportance for all mediators!", NowTime), "\n")
      eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete calculating MImportance for all mediators!", NowTime))
      eSet$ExcecutionLog %>%
        tibble::as_tibble() %>%
        purrr::set_names("running log") %>%
        data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
    }
    else if(tibble::is_tibble(M)==FALSE) {
      m_grpname = rlist::list.names(M)
      names(M) <- NULL
      m <- purrr::map(M, as.matrix)
      ddpcr::quiet(eSet$MedtImptM$list <- purrr::map2_dfr(m, m_grpname, F3))

      eSet$MedtImptM$list %>%
        vroom::vroom_write(stringr::str_c(path, "/MedtImptM-each-mediator-group-",NowTime, ".csv"), ",")
      #message and log
      message(stringr::str_c("Complete calculating MImportance for each mediator group!", NowTime), "\n")
      eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete calculating MImportance for each mediator group!", NowTime))
      eSet$ExcecutionLog %>%
        tibble::as_tibble() %>%
        purrr::set_names("running log") %>%
        data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
    }
    return(eSet)
  }#end MImportance

  #for all mediators
  eSet <- MImportance(M = eSet$Expo$Data[eSet$Expo$Voca %>%
                                           dplyr::filter(GroupName == "immunome") %>%
                                           dplyr::select(SerialNo) %>%
                                           as.matrix() %>%
                                           as.vector()])
  #for each mediator group
  eSet <- MImportance(M = eSet$Expo$MediList)

  #save R command and log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- MedtImptM(eSet, VarsY = MedtImptMVarsY,  VarsX = MedtImptMVarsX, VarsC = MedtImptMVarsC)"))
  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))
  #save eSet
  save(eSet,
       file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))

  tictoc::toc()
  return(eSet)
}
