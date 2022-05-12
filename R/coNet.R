#' coNet
#'
#'
#' @title coNet
#' @param list list
#' @importFrom igraph graph.data.frame
#' @importFrom igraph simplify
#' @importFrom widyr pairwise_count
#' @importFrom dplyr %>%
#' @return A igraph
#' @author Yuanlong Hu
#' @export

coNet <- function(list){

  ck <- lapply(list, function(x) {
          length(x) != 0
      }) %>% unlist()
  list <- list[ck]
  keywords <- NULL
  for (i in 1:length(list)) {
    list1 <- data.frame(term = names(list)[i],
                        words = list[[i]])
    keywords <- rbind(keywords, list1)
  }

  freq <- table(unlist(keywords$words)) %>% as.data.frame()
  names(freq) <- c("name", "freq")
  freq <- freq[order(freq$freq, decreasing = TRUE),]

  keywords <- pairwise_count(keywords, words, term, sort = TRUE)
  names(keywords) <- c("from", "to", "freq")
  net <- graph.data.frame(keywords, directed = F, vertices = freq)
  net <- simplify(net, edge.attr.comb="mean")
  return(net)
}
