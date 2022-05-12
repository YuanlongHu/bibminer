#' plot_coNet
#'
#'
#' @title plot_coNet
#' @param graph graph
#' @param freq_node freq_node
#' @param freq_edge freq_edge
#' @param w1 w1
#' @param w2 w2
#' @param remove_single remove_single
#' @importFrom igraph delete.edges
#' @importFrom igraph delete.vertices
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom igraph degree
#' @importFrom visNetwork toVisNetworkData
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visOptions
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visNetworkEditor
#' @importFrom dplyr %>%
#' @return A igraph
#' @author Yuanlong Hu
#' @export

plot_coNet <- function(graph, freq_node = 2, freq_edge = 2, w1=10, w2=10, remove_single=TRUE){

  graph <- delete.vertices(graph, which(V(graph)$freq<freq_node))
  graph <- delete.edges(graph, which(E(graph)$freq<freq_node))

  if(remove_single) graph <- delete.vertices(graph, which(degree(graph)==0))

  data_visNetwork <- toVisNetworkData(graph)
  if(nrow(data_visNetwork$edges)>0) data_visNetwork$edges$width <- log(data_visNetwork$edges$freq)*w2
  if(nrow(data_visNetwork$nodes)>0) data_visNetwork$nodes$size <- log(data_visNetwork$nodes$freq)*w1
  visNetwork(nodes = data_visNetwork$nodes,
             edges = data_visNetwork$edges,
             height = "700px", width = "100%") %>%
    visOptions(selectedBy = "type",
               manipulation = TRUE,
               highlightNearest = TRUE)%>%
    visEdges(smooth = FALSE) %>%
    visNetworkEditor()

}
