#' get keywords
#'
#'
#' @title get_keywords
#' @param data file
#' @importFrom jiebaR segment
#' @importFrom jiebaR worker
#' @importFrom dplyr %>%
#' @importFrom jiebaR filter_segment
#' @return A list.
#' @author Yuanlong Hu
#' @export


get_keywords <- function(data){

  source <- unique(data$source)
  source <- as.list(source)
  names(source) <- source
  keywords <- lapply(source, function(x){
    data <- data[data$source == x,]
    if (x == "WanFang") {
      keywords <- str_split(data$K1, " ",simplify = F)
    }else{
      keywords <- str_split(data$K1, ";",simplify = F)
    }
      names(keywords) <- data$ID

    })
  keywords <- Reduce(c, keywords)
  return(keywords)
}
