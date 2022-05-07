#' recode_words
#'
#'
#' @title recode_words
#' @param list list
#' @param rep rep
#' @param remove remove words
#' @importFrom dplyr recode
#' @importFrom rlang !!!
#' @return A list.
#' @author Yuanlong Hu
#' @export

recode_words <- function(list, rep, remove){
  re_words <- rep[,2]
  names(re_words) <- rep[,1]

  lapply(list, function(x){
    x <- x[!x %in% remove]
    x <- recode(x, !!!re_words)
    x <- unique(x)
    return(x)
  })
}
