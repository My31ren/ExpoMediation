#' Expoures dimension reduction
#'
#' @param eSet eSet, A R6 class object
#' @param VarsY A character indicating the name of outcome
#' @param VarsC A character vector indicating the name of covariates
#' @param VarsM A character vector indicating the name of mediator
#' @param Method Dimension reduction method, available options include "gcdnet"(default) and "mean"
#' @param Iter Iterations of mediation models,default is 500
#'
#' @return eSet, A R6 class object
#' @import tictoc lubridate tidyverse naniar ddpcr vroom data.table mediation gcdnet
#' @export
#'
#' @examples eSet <- MedtRedX(eSet = eSet,
#'                  VarsY = "Y1",
#'                  VarsC = c("C1.2", "C1.3", "C2", "C3", "C4", "C5.2", "C6.2", "C6.3", "C6.4"),
#'                  VarsM = c("M1","M2","M3"),
#'                  Method = "mean",
#'                  Iter = 500)
MedtRedX = function(eSet = eSet,
                    VarsY = "Y1",
                    VarsC = eSet$Expo$Voca %>%
                        dplyr::filter(str_detect(SerialNo, "^C")) %>%
                        dplyr::select(SerialNo) %>%
                        as.matrix() %>%
                        as.vector(),
                    VarsM = NULL,
                    Method = "gcdnet",
                    Iter = 500)
  {
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime
  path = stringr::str_c(eSet$FileDirOut, "/Xreduction")
  if(!file.exists(path)){dir.create(path)}

  XReduction = function(
    X){
    #get data
    df2 <- eSet$Expo$Data
    #loop function
    F1 = function(X){
      tictoc::tic()
      set.seed(20220414)
      switch(Method,
             "mean" = {
               X %>%
                 tibble::as_tibble() %>%
                 dplyr::mutate(rsum = rowMeans(.[1:ncol(.)])) %>%
                 dplyr::select(rsum) %>%
                 as.matrix() %>%
                 as.vector() -> ERS
             },
             "gcdnet" = {
               # 2. Exposure reduction: using gcdnet
               #将数据集转换为矩阵,将x与c合并为independent data
               dt.x <- as.matrix(X)#exposure data
               dt.c <- as.matrix(df2[, VarsC])#covariate data
               dt.y <- as.matrix(df2[, VarsY])# outcome data
               dt.ind <- cbind(dt.x, dt.c)
               #create fold-id list for use
               fold.id <- sample(
                 rep(seq(5), length = nrow(dt.ind)),
                 replace = FALSE)# 5 folded cross validation

               #Part1: Optimize Lambda2 with min CV
               #1 create candidate L2 sequence
               L2.list <- seq(0, 1, by = 0.01)
               #2 create penalty factors that tells the gcdnet function allows the shrinkage of only exposures, but not covariates
               pf <- c(rep(1, ncol(dt.x)), rep(0, ncol(dt.c)))
               #3 collect the min mean cv error (min cvm)for each Lambda2 and select the optimized L2 for min cv error
               L2cv = function(L2){
                 cv.fit <- gcdnet::cv.gcdnet(dt.ind,
                                     dt.y,
                                     foldid = fold.id,
                                     lambda2 = L2,
                                     method = "ls",#alternatives: "ls" for cotinuous outcome, "logit" for categorical ones. other terms: "hhsvm", "sqsvm" for svm(categorical outcome) and "er" for expectile regression(continuous outcome).
                                     pred.loss = "loss",#alternatives: "loss" for margin based loss, "ls" for least square loss, "er" for expectile regression loss and "misclass" only for svm miclassification loss.
                                     pf = pf,
                                     pf2 = pf,
                                     standardize = TRUE
                 )
                 cvm <- min(cv.fit$cvm)
                 return(cvm)
               }
               tmp <- purrr::map(L2.list, L2cv)#get cv list
               #4 get the optimized lambda2 value
               L2.opt = L2.list[which.min(tmp)]
               print(paste("The optimal Lambda2 with minimum CV: ", L2.opt))

               #Part2: Optimize Lambda1 with optimal Lambda2 value
               cv.fit <- gcdnet::cv.gcdnet(dt.ind,
                                   dt.y,
                                   foldid = fold.id,
                                   lambda2 = L2.opt,
                                   method = "ls",
                                   pred.loss = "loss",
                                   pf = pf,
                                   pf2 = pf,
                                   standardize = TRUE
               )
               L1.opt = cv.fit$lambda.min
               print(paste("The optimal Lambda1 with minimum CV: ", L1.opt))

               #Part3: fit gcdnet model to get coefficients
               fit.net <- gcdnet::gcdnet(dt.ind,
                                 dt.y,
                                 lambda = L1.opt,
                                 lambda2 = L2.opt,
                                 method = "ls",
                                 pf = pf,
                                 pf2 = pf,
                                 standardize = TRUE
               )
               beita <- coef(fit.net)

               #Part4: get Adaptive ENET weights (aw)
               #calculate gamma
               gamma <- ceiling(2*log(ncol(dt.ind))/log(nrow(dt.ind))/(1-log(ncol(dt.ind))/log(nrow(dt.ind))))+1
               #calculate sample sd for each predictor
               ind.sd <- apply(dt.ind,2,sd)*((nrow(dt.ind)-1)/nrow(dt.ind))^0.5
               #calculate adaptive weight (aw)
               aw <- (abs(beita[-1,]*ind.sd)+1/nrow(dt.ind))^(-gamma)
               aw[(ncol(dt.x)+1):ncol(dt.ind)] <- 0#let the weights of covariates as 0

               #Part5: optimize Lambda2 and Lambda1 for final aENET
               L2cv_2 = function(L2){
                 cv.adfit <- gcdnet::cv.gcdnet(dt.ind,
                                       dt.y,
                                       foldid = fold.id,
                                       lambda2 = L2,
                                       method = "ls",#alternatives: "ls" for cotinuous outcome, "logit" for categorical ones. other terms: "hhsvm", "sqsvm" for svm(categorical outcome) and "er" for expectile regression(continuous outcome).
                                       pred.loss = "loss",#alternatives: "loss" for margin based loss, "ls" for least square loss, "er" for expectile regression loss and "misclass" only for svm miclassification loss.
                                       pf = aw,
                                       standardize = TRUE
                 )
                 cvm <- min(cv.adfit$cvm)
                 return(cvm)
               }
               tmp <- purrr::map(L2.list, L2cv_2)#get cv list
               L2.ad.opt = L2.list[which.min(tmp)]#Lambda2 for aENET
               print(paste("The optimal adaptive Lambda2 with minimum CV: ", L2.ad.opt))
               cv.adfit <- gcdnet::cv.gcdnet(dt.ind,
                                     dt.y,
                                     foldid = fold.id,
                                     lambda2 = L2.ad.opt,
                                     method = "ls",
                                     pred.loss = "loss",
                                     pf = aw,
                                     standardize = TRUE
               )
               L1.ad.opt = cv.adfit$lambda.min#Lambda1 for aENET
               print(paste("The optimal adaptive Lambda1 with minimum CV: ", L1.ad.opt))

               #Part6: fit Adaptive ENET
               fit.aENET <- gcdnet::gcdnet(dt.ind,
                                   dt.y,
                                   lambda = L1.ad.opt,
                                   lambda2 = L2.ad.opt,
                                   pf = aw,
                                   method = "ls",
                                   standardize = TRUE
               )

               #calculate parameters
               beita <- coef(fit.aENET)#beita for all exposures and covariats plus intercept
               index.ne0 <- which(beita[-1]!=0)#index for non-zero predictors
               sigma2 <- t(dt.y-cbind(1,dt.ind)%*%beita)%*%(dt.y-cbind(1,dt.ind)%*%beita)/(nrow(dt.ind)-length(index.ne0)-1)#estimated sigma square

               ind.mean <- apply(dt.ind,2,mean)#mean value of indemendent variables
               dt.ind.sdz <- t((t(dt.ind)-ind.mean)/ind.sd)#standardized independent varaiables
               dt.ind.sdz.ne0 <- dt.ind.sdz[,index.ne0]#standardized non-zero variables
               sigma.ne0 <- t(dt.ind.sdz.ne0)%*%dt.ind.sdz.ne0#simgma for non-zero variables
               var.sdz.ne0 <- as.numeric(sigma2)*(1+L2.ad.opt/2)^2*solve(sigma.ne0+diag(rep(nrow(dt.ind)*L2.ad.opt,length(index.ne0)))+ (nrow(dt.ind)*L2.ad.opt/2)^2*solve(sigma.ne0))#variance matrix for non-zero of standardized predictors
               var.original <- diag(1/ind.sd[index.ne0])%*%var.sdz.ne0%*%t(diag(1/ind.sd[index.ne0]))#variance for original predictors
               var.intercept <- t(as.matrix(ind.mean[index.ne0]))%*%var.original%*%as.matrix(ind.mean[index.ne0])#variance for intercept

               #constructe result dataframe
               name.index <- c("(Intercept)", colnames(dt.ind)[index.ne0])#names of variable of non-zero coef
               se <- data.frame(se = sqrt(c(var.intercept, diag(var.original))), row.names = name.index)#se
               pvalue <- data.frame(pvalue = 2-2*apply(abs(beita[c(1,index.ne0+1)])/se,1,pnorm),row.names=name.index)

               #create blank result dataframe
               result <- cbind(numeric(ncol(dt.ind)+1),matrix(NA,ncol(dt.ind)+1,2))
               colnames(result) <- c("beta","SE","p-value")
               rownames(result) <- c("(Intercept)",colnames(dt.ind))
               result[name.index,1] <- beita[name.index,]
               result[name.index,2] <- se[name.index,]
               result[name.index,3] <- pvalue[name.index,]
               result = as.data.frame(result)
               result$loci = result$beta - 1.96*result$SE
               result$hici = result$beta + 1.96*result$SE

               # 4-Constructe ERS variable
               result %>%
                 dplyr::select(beta) %>%
                 dplyr::slice(2:(ncol(X)+1)) %>%
                 as.matrix(ncol = 1) -> wt
               ERS <- as.matrix(X)%*%wt
               ERS <- as.vector(ERS)
             }
      )#switch end
      tictoc::toc()
      return(ERS)
    }#end loop
    if (tibble::is_tibble(X)==TRUE) {
      eSet$MedtRedX$ERSall <- F1(X)
      eSet$MedtRedX$ERSall <- tibble::as_tibble(eSet$MedtRedX$ERSall)
      colnames(eSet$MedtRedX$ERSall) <- "ERS_all"
      #export ERS variable
      message(stringr::str_c("Complete Xreduction for all exposures!", "\n", "The users may search eSet$MedtRedX$ERSall for detailed results of ERS variables.", "\n", NowTime), "\n")
      eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete Xreduction for all exposures!", NowTime))
      eSet$ExcecutionLog %>%
        tibble::as_tibble() %>%
        purrr::set_names("running log") %>%
        data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
    }
    else if (tibble::is_tibble(X)==FALSE) {
      eSet$MedtRedX$ERSlist <- purrr::map_dfc(X, F1)
      eSet$MedtRedX$ERSlist %>%
        dplyr::rename_with(~stringr::str_replace_all(.x, " ", "_"), tidyselect::matches(" ")) -> eSet$MedtRedX$ERSlist
      message(stringr::str_c("Complete Xreduction for each exposure group!", "\n", "The users may search eSet$MedtRedX$ERSlist for detailed results of ERS variables.", "\n", NowTime), "\n")
      eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete Xreduction for each exposure group!", NowTime))
      eSet$ExcecutionLog %>%
        tibble::as_tibble() %>%
        purrr::set_names("running log") %>%
        data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
    }
    return(eSet)
  }
  #for all exposures
  eSet <- XReduction(X = eSet$Expo$Data[eSet$Expo$Voca %>%
                                          dplyr::filter(GroupName == "chemical") %>%
                                          dplyr::select(SerialNo) %>%
                                          as.matrix() %>%
                                          as.vector()])
  #for each exposure group
  eSet <- XReduction(X = eSet$Expo$ExpoList)
  #cbind data
  eSet$Expo$Data %>%
    dplyr::bind_cols(eSet$MedtRedX$ERSall, eSet$MedtRedX$ERSlist) -> tmp

  #Pairwise function------------
  Pairwise = function(data,
                      Y,
                      X,
                      M,
                      C,
                      iter){
    tictoc::tic()
    df2 <- data[,c(Y,X,M,C)]#combine as a dataframe for mediation modeling

    ####--------Mission 1:建造pairwise mediation公式-------####
    #1. extract names of exposures mediators confounders and outcome
    c.y <- Y
    c.x <- X
    c.m <- M
    c.c <- C
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
                                    sims = iter)#iter: numbers of iterations
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
    ddpcr::quiet(eSet$MedtRedX$Stats <- purrr::pmap_dfr(eqt, Pairmedi))
    eSet$MedtRedX$Stats %>%
      vroom::vroom_write(stringr::str_c(path, "/MedtRedX-stats-",NowTime, ".csv"),",")
    message(stringr::str_c("Complete Xreduction mediation modelling!","\n","The users can check the results in eSet$MedtRedX$Stats or output physical files in Xreduction folder. ","\n", NowTime), "\n")
    eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete Xreduction mediation modelling!", NowTime))
    eSet$ExcecutionLog %>%
      tibble::as_tibble() %>%
      purrr::set_names("running log") %>%
      data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))

    return(eSet)

  }
  #Pair mediation analyses for ERS variables
  eSet <- Pairwise(
    data = tmp,
    Y = VarsY,
    X = c(colnames(eSet$MedtRedX$ERSall),colnames(eSet$MedtRedX$ERSlist)),#Take a short Xlist as example
    M = VarsM,
    C = VarsC,
    iter = Iter)

  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- MedtRedX(eSet, VarsY = MedtRedXVarsY, VarsC = MedtRedXVarsC, VarsM = MedtRedXVarsM, Method = MedtRedXMethod, Iter = Iter)"))
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

