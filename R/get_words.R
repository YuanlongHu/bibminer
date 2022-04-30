#' get words
#'
#'
#' @title get_words
#' @param data file
#' @param user A path to user dictionary
#' @param remove remove
#' @param add_T1 add T1
#' @param add_K1 add K1
#' @importFrom jiebaR segment
#' @importFrom jiebaR worker
#' @importFrom dplyr %>%
#' @importFrom jiebaR filter_segment
#' @return A list.
#' @author Yuanlong Hu
#' @export

get_words <- function(data, user, remove, add_T1 = TRUE, add_K1 = TRUE){

  data <- data$AB

  if(add_T1) data <- paste0(data, data$T1)
  if(add_K1) data <- paste0(data, data$K1)

  work <- worker(user=user)
  data2 <- lapply(as.list(data), function(x){
    segment(x, work) %>%
      filter_segment(filter_words = remove)
  })
}

