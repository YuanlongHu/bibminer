#' recode_words
#'
#'
#' @title recode_words
#' @param list list
#' @param re_data re_data
#' @importFrom dplyr recode
#' @importFrom rlang !!!
#' @return A list.
#' @author Yuanlong Hu
#' @export

recode_words <- function(list, re_data){
  re_words <- re_data[,2]
  names(re_words) <- re_data[,1]

  lapply(list, function(x){
    x <- recode(x, !!!re_words)
    x <- x[!is.na(x)]
    x <- unique(x)
    return(x)
  })
}
