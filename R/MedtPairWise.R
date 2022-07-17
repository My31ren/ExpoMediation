#' Implementing pair-wise mediation analyses for each exposure-mediator combination
#' @param eSet eSet, A R6 class object
#' @param VarsY A character indicating the name of outcome
#' @param VarsX A character vector indicating the name of exposures
#' @param VarsM A character vector indicating the name of mediator
#' @param VarsC A character vector indicating the name of covariates
#' @param Iter The number of iterations of mediation modeling
#'
#' @return eSet, A R6 class object
#' @import tictoc lubridate tidyverse naniar ddpcr vroom data.table mediation
#' @export
#' @examples eSet <- MedtPairWise(eSet = eSet,
#'                      VarsY = "Y1",
#'                      VarsX = c("X1","X2","X3", "X10", "X11", "X12", "X19", "X20", "X21"),
#'                      VarsM = c("M1","M2","M3","M4","M5","M8","M8"),
#'                      VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
#'                      Iter = 500)
MedtPairWise = function(eSet,
                        VarsY,
                        VarsX,
                        VarsM,
                        VarsC,
                        Iter)
  {
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime

  path = stringr::str_c(eSet$FileDirOut, "/Pairwise")
  if(!file.exists(path)){dir.create(path)}
  df2 <- eSet$Expo$Data[,c(VarsY,VarsX,VarsM,VarsC)]#combine as a dataframe for mediation modeling

  ####--------Mission 1:建造pairwise mediation公式-------####
  #1. extract names of exposures mediators confounders and outcome
  c.y <- VarsY
  c.x <- VarsX
  c.m <- VarsM
  c.c <- VarsC
  #2. combine covariates with +
  c.c <- paste(c.c, collapse = "+")

  #3. create all possible combinations of exposures and mediators with function crossing
  eqt <- tidyr::crossing(c.x,c.m)#pairwise exposures and mediators
  #4. create y equations and m qeuations
  eqt %>%
    dplyr::mutate(m.eqt = paste(c.m, "~", c.x, "+", c.c),
                  y.eqt = paste(c.y, "~", c.m, "+", c.x, "+", c.c))-> eqt#characters of equations
  #---------建造pairwise mediation公式end--------------#

  #循环体函数
  Pairmedi = function(c.x, c.m, m.eqt, y.eqt){
    #1. pairwise mediation equations via parse and eval functions
    #fit y
    fity <- lm(eval(parse(text = y.eqt)), data = df2)#y公式：yeqt
    #fit m
    fitm <- lm(eval(parse(text = m.eqt)), data = df2)#m公式：meqt
    #fit mediation
    fitmedi <- mediation::mediate(fitm, fity,
                                  treat = as.character(c.x), #x：exposure
                                  mediator = as.character(c.m),#m：mediator
                                  sims = Iter)#iter: numbers of iterations
    tmp <- tibble::as_tibble(dplyr::bind_cols(Pairs = paste(as.character(c.x),"*",as.character(c.m)), #x：eqt[1,"c.x"], m：eqt[1,"c.m"]
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
    colnames(tmp) <- c("Pairs",
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
    return(tmp)
  }
  #批量执行循环体函数
  ddpcr::quiet(eSet$MedtPairWise$Stats <- purrr::pmap_dfr(eqt, Pairmedi))
  eSet$MedtPairWise$Stats %>%
    vroom::vroom_write(stringr::str_c(path, "/MedtPairWise.stats.",NowTime, ".csv"),",")
  message(stringr::str_c("Complete pairwise mediation modelling!","\n","The users can check the results in eSet$MedtPairWise$Stats or output physical files in Pairwise folder. ","\n", NowTime, "\n"))
  #message and log
  eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete pairwise mediation modelling!", NowTime))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- MedtPairWise(eSet = eSet, VarsY = MedtPairWiseVarsY, VarsX = MedtPairWiseVarsX, VarsM = MedtPairWiseVarsM, VarsC = MedtPairWiseVarsC, Iter = Iter)"))
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
