#' freq_words
#'
#'
#' @title freq_words
#' @param list list
#' @importFrom dplyr %>%
#' @return A data frame.
#' @author Yuanlong Hu
#' @export

freq_words <- function(list){

  freq <- table(unlist(list)) %>% as.data.frame()
  names(freq) <- c("Words", "Freq")
  return(freq)
}
