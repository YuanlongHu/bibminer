#' merge_bib
#'
#'
#' @title merge_bib
#' @param ... data
#' @importFrom dplyr distinct
#' @return A data frame.
#' @author Yuanlong Hu
#' @export

merge_bib <- function(...){
  data <- rbind(...)
  data <- distinct(data,T1, JF, YR, .keep_all = TRUE)
  return(data)
}
