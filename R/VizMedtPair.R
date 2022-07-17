#' Visualize pairwise mediation modelling result
#'
#' @param eSet eSet, A R6 class object
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table flextable ggplot2 grid grDevices
#' @examples eSet <- VizMedtPair(eSet = eSet)
VizMedtPair <- function(eSet = eSet)
{
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime
  #path
  path = stringr::str_c(eSet$FileDirOut, "/VizMedtPair")
  if(!file.exists(path)) {dir.create(path)}

  #prepare dataset
  eSet$MedtPairWise$Stats %>%
    dplyr::mutate(Exposure = stringr::str_extract(Pairs, "(^X)[0-9]*(?=\\ \\*)"),
                  Mediator = stringr::str_extract(Pairs, "(?<=\\ \\*\\ )M[0-9]*")) %>%
    dplyr::select(Exposure, Mediator, ACME.T.Pvalue)-> tmp

  eSet$Expo$Voca %>%
    dplyr::select(SerialNo, SubgroupName, FullName) %>%
    dplyr::filter(stringr::str_detect(SerialNo, "^X")) %>%
    dplyr::rename(Exposure =  SerialNo,
           NameX = FullName,
           SubgrpX = SubgroupName) %>%
    dplyr::right_join(tmp, by = "Exposure") -> tmp

  eSet$Expo$Voca %>%
    dplyr::select(SerialNo, SubgroupName, FullName) %>%
    dplyr::rename(Mediator = SerialNo,
           NameM = FullName,
           SubgrpM = SubgroupName) %>%
    dplyr::right_join(tmp, by = "Mediator") -> tmp


  #plot for pairwise modelling results
  pdf(stringr::str_c(path, "/MedtPairWise-Indirect-Effect-Plot-", NowTime,".pdf"),
      width = 10,
      height = 6)

  tmp %>%
    ggplot2::ggplot(ggplot2::aes(x = -log10(ACME.T.Pvalue), y = NameX)) +
    ggplot2::geom_point(ggplot2::aes(color = SubgrpM),
               size = 3) +
    ggplot2::xlab(expression("-Log"["10"]~flextable::italic("P")~"value"))+
    ggplot2::geom_vline(xintercept = -log10(0.05),
               linetype = "dashed",
               color = "red",
               size = 1,
               alpha = 0.5) +
    ggplot2::theme_bw()+
    ggplot2::facet_grid(SubgrpX ~ .,
               scales = "free",
               space = "free",
               labeller = ggplot2::label_wrap_gen(13)) +
    ggrepel::geom_label_repel(data = (tmp %>%
                                        dplyr::filter(ACME.T.Pvalue < 0.05)),
                              ggplot2::aes(label = NameM,
                                           color = SubgrpM),
                              size = 4,
                              show.legend = F)+
    ggplot2::scale_color_discrete(name = "Mediator Groups") +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"),#plot background
          strip.text.y = ggplot2::element_text(size = 14, angle = 0),#facet label
          panel.spacing = grid::unit(0, "lines"),#gaps between facets
          axis.title.y = ggplot2::element_blank(),#delete y-axis title
          axis.text.x = ggplot2::element_text(size = 12),#x axis
          axis.text.y = ggplot2::element_text(size = 11),#y axis
          legend.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 13))-> p

  #custom facet background color
  #extract gtable of p
  g = ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  #find strip-r index
  idx <- stringr::str_which(g$layout$name, "strip-r")
  #prepare colors vector
  fills <- c("#FAEBD7", "#B0E0E6", "#D8BFD8", "#CDC5BF", "#EE6A50",
             "#EED8AE", "#EEDC82", "#FFA07A", "#7AC5CD", "#CDC1C5")
  k = 1
  for (i in idx) {
    j <- stringr::str_which(g$grobs[[i]]$grobs[[1]]$childrenOrder,"rect")
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  grid::grid.draw(g)

  # eSet$MedtPairWise$Plot <- recordPlot()
  # #save plot
  # ggplot2::ggsave(stringr::str_c(path, "/MedtPairWise-Indirect-Effect-Plot-", NowTime,".jpg"),
  #        plot = replayPlot(eSet$MedtPairWise$Plot),
  #        width = 35,
  #        height = 20,
  #        units = "mm")
  #save method2

  grDevices::dev.off()
  grid::grid.draw(g)

  eSet$MedtPairWise$Plot <- grDevices::recordPlot()
  #message and log
  message(stringr::str_c("Complete ploting VizMedtPair! ", NowTime, "\n"))
  eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete pairwise mediation plot!", NowTime))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- VizMedtPair(eSet = eSet))"))
  eSet$RCommandLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("R commands") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/rcommands log.txt"))
  tictoc::toc()
  #save eSet
  save(eSet,
       file = stringr::str_c(eSet$FileDirOut,"/eSet.Rdata"))
  return(eSet)
}
#eSet <- VizMedtPair(eSet = eSet)
