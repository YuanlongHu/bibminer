#' coNet
#'
#'
#' @title coNet
#' @param list list
#' @importFrom igraph graph.data.frame
#' @importFrom igraph simplify
#' @importFrom widyr pairwise_count
#' @return A list.
#' @author Yuanlong Hu
#' @export

coNet <- function(list){
  keywords <- NULL
  for (i in 1:length(list)) {
    list1 <- data.frame(term = rep(names(list)[i]),
                        words = list[[i]])
    keywords <- rbind(keywords, list1)
  }

  freq <- table(unlist(keywords)) %>% as.data.frame()
  names(freq) <- c("name", "size")

  keywords <- pairwise_count(keywords, words, term, sort = TRUE)
  names(keywords) <- c("from", "to", "width")
  net <- graph.data.frame(keywords, directed = F, vertices = freq)
  net <- simplify(net, edge.attr.comb="mean")
  return(net)
}
