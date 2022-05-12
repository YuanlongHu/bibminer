#' get words
#'
#'
#' @title get_words
#' @param data file
#' @param user A path to user dictionary
#' @param add_T1 add T1
#' @param add_K1 add K1
#' @importFrom jiebaR segment
#' @importFrom jiebaR worker
#' @importFrom dplyr %>%
#' @importFrom jiebaR filter_segment
#' @return A list.
#' @author Yuanlong Hu
#' @export

get_words <- function(data, user, add_T1 = TRUE, add_K1 = TRUE){

  AB <- data$AB

  if(add_T1) AB <- paste0(AB, data$T1)
  if(add_K1) AB <- paste0(AB, data$K1)

  work <- worker(user=user)
  data2 <- lapply(as.list(AB), function(x){
    segment(x, work)
  })
  names(data2) <- data$ID
  return(data2)
}

