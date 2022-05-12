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

recode_words <- function(list, rep=NULL, remove=NULL){
  if(!is.null(rep)) {
    re_words <- rep[,2]
    names(re_words) <- rep[,1]
    data <- lapply(list, function(x){
                     x <- recode(x, !!!re_words)
                     x <- unique(x)
                     return(x)
                  })
  }
  
  if(!is.null(remove)) {
    data <- lapply(list, function(x){
        x <- x[!x %in% remove]
        return(x)
     })
  }
   return(data)
}
