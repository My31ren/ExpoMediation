#' Visualize MedtRedXM mediation modelling result
#'
#' @param eSet eSet, A R6 class object
#'
#' @return eSet, A R6 class object
#' @export
#' @import tictoc lubridate tidyverse ddpcr vroom data.table flextable ggplot2 grid grDevices
#' @examples eSet <- VizMedtRedXM(eSet = eSet)
VizMedtRedXM <- function(eSet = eSet)
{
  tictoc::tic()
  lubridate::now() %>%
    stringr::str_replace_all(":",".") %>%
    stringr::str_replace_all("-",".") -> NowTime
  #path
  path = stringr::str_c(eSet$FileDirOut, "/VizMedtRedXM")
  if(!file.exists(path)) {dir.create(path)}
  #prepare dataset for plotting
  #prepare effect value
  eSet$MedtRedXM$list %>%
    dplyr::bind_rows(eSet$MedtRedXM$all) %>%
    tidyr::pivot_longer(cols= c(ACME.T, ADE.T, TE),
                 names_to = "Effect",
                 values_to = "Effect_Value") %>%
    dplyr::select(Expo, Mediator, Effect, Effect_Value)%>%
    dplyr::mutate(Effect = stringr::str_replace(Effect,".T",""))-> tmp1
  #prepare LCL value
  eSet$MedtRedXM$list %>%
    dplyr::bind_rows(eSet$MedtRedXM$all) %>%
    tidyr::pivot_longer(cols = c(ACME.T.LCL, ADE.T.LCL, TE.LCL),
                 names_to = "Effect",
                 values_to = "LowerCL") %>%
    dplyr::select(Expo, Mediator, Effect, LowerCL) %>%
    dplyr::mutate(Effect = stringr::str_replace_all(Effect,".T|.LCL",""))->tmp2
  #prepare UCL value
  eSet$MedtRedXM$list %>%
    dplyr::bind_rows(eSet$MedtRedXM$all) %>%
    tidyr::pivot_longer(cols = c(ACME.T.UCL, ADE.T.UCL, TE.UCL),
                 names_to = "Effect",
                 values_to = "UpperCL")%>%
    dplyr::select(Expo, Mediator, Effect, UpperCL)%>%
    dplyr::mutate(Effect = stringr::str_replace_all(Effect,".T|.UCL",""))->tmp3
  #cbind data
  tmp1 %>%
    dplyr::bind_cols(
              tmp2 %>%
                dplyr::select(LowerCL),
              tmp3 %>%
                dplyr::select(UpperCL)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("Effect_Value","LowerCL","UpperCL")), as.numeric)) %>%
    dplyr::mutate(Expo = stringr::str_replace_all(Expo, "_", " "))-> tmp


  #plot
  tmp %>%  ggplot2::ggplot(ggplot2::aes(x = Effect_Value,y = Effect))+
    ggplot2::geom_point()+
    ggplot2::xlab("Effect Value") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = LowerCL,xmax = UpperCL),
               size = 0.1,
               width = 0) +
    ggplot2::geom_vline(xintercept = 0,
                        linetype = "dashed",
                        color = "red",
                        size = 1,
                        alpha = 0.5) +
    ggplot2::facet_grid(Mediator ~ Expo,
                        # space = "free",
                        scales = "free",
                        labeller = label_wrap_gen(13)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"),#plot background
                   strip.text.y = ggplot2::element_text(size = 12, angle = 0),#facet label
                   strip.text.x = ggplot2::element_text(size = 12),
                   panel.spacing = grid::unit(0.2, "lines"),#gaps between facets
                   axis.title.y = ggplot2::element_blank(),#delete y-axis title
                   axis.text.x = ggplot2::element_text(size = 9),#x axis
                   axis.text.y = ggplot2::element_text(size = 11),#y axis
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 13)) -> plot
  #save
  #save plot to local folder
  ggplot2::ggsave(plot = plot,
         filename = stringr::str_c("/MedtRedXM-Forest-Plot-", NowTime,".pdf"),
         path = path,
         width = 12,
         height = 7,
         units = "in")
  #save plot to eSet
  eSet$MedtRedXM$Plot <- plot

  #message and log
  message(stringr::str_c("Complete ploting VizMedtRedXM! ", NowTime, "\n"))
  eSet$ExecutionLog <- eSet$AddLog(stringr::str_c("Complete forest plot!", NowTime))
  eSet$ExcecutionLog %>%
    tibble::as_tibble() %>%
    purrr::set_names("running log") %>%
    data.table::fwrite(stringr::str_c(eSet$FileDirOut,"/running log.txt"))
  #save R command log
  eSet$RCommandLog <- eSet$AddCommand(stringr::str_c("eSet <- VizMedtRedXM(eSet = eSet))"))
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
#eSet <- VizMedtRedXM(eSet = eSet)
