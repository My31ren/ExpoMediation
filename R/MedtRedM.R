#' Mediator dimension reduction: extract the first principal direction of mediator
#'
#' @param eSet eSet, A R6 class object
#' @param VarsY A character indicating the name of outcome
#' @param VarsX A character vector indicating the name of exposures
#' @param VarsC A character vector indicating the name of covariates
#' @param Method Dimension reduction method, available options are "mean"(default) and "PDM1"
#' @param Iter Iterations of mediation models,default is 500
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse naniar ddpcr vroom data.table mediation PDM
#' @examples eSet <- MedtRedM(eSet = eSet,
#'                  VarsY = "Y1",
#'                  VarsX = c("X1", "X2", "X3"),
#'                  VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
#'                  Method = "mean",
#'                  Iter = 500)
MedtRedM = function(eSet = eSet,
                    VarsY = "Y1",
                    VarsX = NULL,
                    VarsC = eSet$Expo$Voca %>%
                      dplyr::filter(str_detect(SerialNo, "^C")) %>%
                      dplyr::select(SerialNo) %>%
                      as.matrix() %>%
                      as.vector(),
                    Method = "mean",
                    Iter = 500)
  {
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime
  #path
  path = stringr::str_c(eSet$FileDirOut, "/Mreduction")
  if(!file.exists(path)){dir.create(path)}

  F3 = function(M){
    F2 <- function(m, m_grpname){
      #get data
      df2 <- eSet$Expo$Data
      #loop function
      F1 <- function(X){
        x <- as.matrix(df2[,X])
        y <- as.matrix(df2[,VarsY])
        c <- as.matrix(df2[,VarsC])
        set.seed(20220422)

        switch(Method,
               "mean" = {
                 m %>%
                   tibble::as_tibble() %>%
                   dplyr::mutate(pdm1 = rowMeans(.[1:ncol(.)])) %>%
                   dplyr::select(pdm1) %>%
                   dplyr::bind_cols(df2) -> df2
               },
               "pdm1" = {
                 #extract PDM1 via PDM_1 fuction
                 tmp1 <- PDM::PDM_1(x = x, y = y, m = m, #the inputs
                               imax = 100, #max number of iterations
                               tol = 10^-(5), #inputs tolerence?
                               theta = rep(1,5), #starting value for the pathway coefficients
                               w1 = rep(1,ncol(m)), #starting value for the 1st PDM
                               interval = 10^6, #the intervals from where the smoothing parameter is searched
                               step = 10^4 #the step width for smoothing parameter search
                 )
                 #construct the 1st PDM for further mediation analyses
                 df2$pdm1 <- m%*%as.matrix(tmp1$w1, ncol = 1)
               }
        )#switch end

        #construct equations
        #y.eqt
        y.eqt <- paste(colnames(y), "~", "pdm1", "+", X, "+", paste(colnames(c), collapse = "+"))
        #m.eqt
        m.eqt <- paste("pdm1", "~", X, "+", paste(colnames(c), collapse = "+"))

        #fit y
        fit.y <- lm(eval(parse(text = y.eqt)), data = df2)
        fit.m <- lm(eval(parse(text = m.eqt)), data = df2)
        fitmedi <- mediation::mediate(fit.m, fit.y,
                                      treat = X, #x：exposure
                                      mediator = "pdm1",#m：mediator
                                      sims = Iter)#iter: numbers of iterations
        tmp2 <- tibble::as_tibble(cbind(Expo = X, #Exposure
                                Mediator = m_grpname, #mediator group
                                fitmedi$d0,fitmedi$d0.ci[1],fitmedi$d0.ci[2],fitmedi$d0.p,#ACME.Control
                                fitmedi$d1, fitmedi$d1.ci[1], fitmedi$d1.ci[2], fitmedi$d1.p,#ACME.Treat
                                fitmedi$z0, fitmedi$z0.ci[1], fitmedi$z0.ci[2], fitmedi$z0.p,#ADE.Control
                                fitmedi$z1, fitmedi$z1.ci[1], fitmedi$z1.ci[2], fitmedi$z1.p,#ADE.Treat
                                fitmedi$n0, fitmedi$n0.ci[1], fitmedi$n0.ci[2], fitmedi$n0.p,#PropMedi.Control
                                fitmedi$n1, fitmedi$n1.ci[1], fitmedi$n1.ci[2], fitmedi$n1.p,#PropMedi.Treat
                                fitmedi$tau.coef, fitmedi$tau.ci[1], fitmedi$tau.ci[2], fitmedi$tau.p,#TE(Total effect)
                                fitmedi$d.avg, fitmedi$d.avg.ci[1], fitmedi$d.avg.ci[2], fitmedi$d.avg.p,#ACME average
                                fitmedi$z.avg, fitmedi$z.avg.ci[1], fitmedi$z.avg.ci[2], fitmedi$z.avg.p,#ADE average
                                fitmedi$n.avg, fitmedi$n.avg.ci[1], fitmedi$n.avg.ci[2], fitmedi$n.avg.p #PropMedi average
        ))
        colnames(tmp2) <- c("Expo",
                            "Mediator",
                            "ACME.C", "ACME.C.LCL", "ACME.C.UCL", "ACME.C.Pvalue",
                            "ACME.T", "ACME.T.LCL", "ACME.T.UCL", "ACME.T.Pvalue",
                            "ADE.C", "ADE.C.LCL", "ADE.C.UCL", "ADE.C.Pvalue",
                            "ADE.T", "ADE.T.LCL", "ADE.T.UCL", "ADE.T.Pvalue",
                            "PMedi.C", "PMedi.C.LCL", "PMedi.C.UCL", "PMedi.C.Pvalue",
                            "PMedi.T", "PMedi.T.LCL", "PMedi.T.UCL", "PMedi.T.Pvalue",
                            "TE", "TE.LCL", "TE.UCL", "TE.Pvalue",
                            "ACME.avg", "ACME.avg.LCL", "ACME.avg.UCL", "ACME.avg.Pvalue",
                            "ADE.avg", "ADE.avg.LCL", "ADE.avg.UCL", "ADE.avg.Pvalue",
                            "PMedi.avg", "PMedi.avg.LCL", "PMedi.avg.UCL", "PMedi.avgC.Pvalue"
        )
        return(tmp2)
      }#end F1 (for each individual exposure)

      ddpcr::quiet(tmp3 <- purrr::map_dfr(VarsX, F1))
      return(tmp3)
    }#end F2 (for each mediator group)
    #if
    if (tibble::is_tibble(M)==TRUE) {
      m_grpname = "All mediators"
      m <- as.matrix(M)
      eSet$MedtRedM$all <- F2(m, m_grpname)
      eSet$MedtRedM$all %>%
        vroom::vroom_write(stringr::str_c(path, "/MedtRedM-all-mediators-",NowTime, ".csv"), ",")
      #message and log
      message(stringr::str_c("Complete Mreduction for all mediators!", "\n", "The users may search eSet$MedtRedM$all or see Mreduction folder for detailed results.", "\n", NowTime), "\n")
      eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete Mreduction for all exposures!", NowTime))
      eSet$ExcecutionLog %>%
        tibble::as_tibble() %>%
        purrr::set_names("running log") %>%
        data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
    }
    else if (tibble::is_tibble(M)==FALSE) {
      m_grpname = rlist::list.names(M)
      names(M) <- NULL
      m <- purrr::map(M, as.matrix)
      eSet$MedtRedM$list <- purrr::map2_dfr(m, m_grpname, F2)
      eSet$MedtRedM$list %>%
        vroom::vroom_write(stringr::str_c(path, "/MedtRedM-each-mediator-group-",NowTime, ".csv"), ",")

      #message and log
      message(stringr::str_c("Complete Mreduction for each mediator group!", "\n", "The users may search eSet$MedtRedM$list or see Mreduction folder for detailed result.", "\n", NowTime), "\n")
      eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete Mreduction for each mediator group!", NowTime))
      eSet$ExcecutionLog %>%
        tibble::as_tibble() %>%
        purrr::set_names("running log") %>%
        data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
    }
    return(eSet)
  }#end F3

  #for all mediators
  eSet <- F3(M = eSet$Expo$Data[eSet$Expo$Voca %>%
                                  dplyr::filter(GroupName == "immunome") %>%
                                  dplyr::select(SerialNo) %>%
                                  as.matrix() %>%
                                  as.vector()])
  #for each exposure group
  eSet <- F3(eSet$Expo$MediList)

  #save R command and log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- MedtRedM(eSet, VarsY = MedtRedMVarsY, VarsX = MedtRedMVarsX, VarsC = MedtRedMVarsC, Method = MedtRedMMethod, Iter = Iter)"))
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
