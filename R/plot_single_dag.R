#' Visualize Bayesian Networks
#'
#' This function visualizes a Bayesian network or its subgraphs, highlighting specified nodes and their Markov blankets.
#' If "all" is provided for the nodes parameter, the entire network structure is visualized.
#'
#' @param bn A Bayesian network (bn object) to visualize. This should be created using the \code{bnlearn} package.
#' @param nodes A character vector specifying the names of the nodes to visualize along with their Markov blankets.
#'        If \code{"all"}, the entire network structure is visualized. Default is \code{"all"}.
#' @param name A title for the graph. Default is "DAG".
#'
#' @return A plot of the Bayesian network or its subgraph, highlighting the specified nodes and their Markov blankets.
#'
#' @details
#' The function uses \code{bnlearn::graphviz.plot()} to generate the plot. If specific nodes are provided, their
#' Markov blankets are highlighted in the subgraph. If no arcs exist in the subgraph, only nodes are customized.
#' Otherwise, both nodes and edges are styled for better visualization.
#'
#' Node and edge customizations include:
#' \itemize{
#'   \item Highlighted nodes are filled with a distinct color (\code{"#A9DFBF"}).
#'   \item Other nodes are filled with a default color (\code{"#D6EAF8"}).
#'   \item Edges are styled uniformly with a default color and line width.
#' }
#'
#' @examples
#' library(bnlearn)
#'
#' # Create a random Bayesian network
#' bn <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Visualize the full network
#' vis_dag(bn, nodes = "all", name = "Full DAG")
#'
#' # Visualize specific nodes and their Markov blankets
#' vis_dag(bn, nodes = c("A", "B"), name = "Subgraph with A and B")
#'
#' @seealso \code{\link[bnlearn]{graphviz.plot}}, \code{\link[bnlearn]{mb}}, \code{\link[bnlearn]{subgraph}}
#'
#' @importFrom bnlearn graphviz.plot mb subgraph
#' @importFrom graph nodeRenderInfo edgeRenderInfo
#' @import Rgraphviz
#' @export
vis_dag <- function(bn, nodes = "all", name = "DAG") {

  # Basic Input Validation
  if (!inherits(bn, "bn")) {
    stop("bn must be a valid Bayesian network object (class 'bn').")
  }

  # Load required libraries
  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("The 'bnlearn' package is required but not installed.")
  }

  # Determine subgraphs based on the 'nodes' parameter
  if (identical(nodes, "all")) {
    subgraph_bn <- bn
  } else {
    mb_bn <- unique(unlist(lapply(nodes, function(node) bnlearn::mb(bn, node))))
    # Create subgraphs containing the specified nodes and their Markov blankets
    subgraph_bn <- bnlearn::subgraph(bn, nodes = unique(c(nodes, mb_bn)))
  }



  if (length(arcs(subgraph_bn)) == 0) {

    # If the network has edges, customize both nodes and edges
    bn.viz <- bnlearn::graphviz.plot(subgraph_bn, main = name,
                                      shape = "ellipse", fontsize = 12, highlight = list(
                                        nodes = nodes(subgraph_bn), col = "#4D4D4D",
                                        fill = "#A9DFBF"))

  } else {
    # If the network does not have edges, only customize nodes
    bn.viz <- bnlearn::graphviz.plot(subgraph_bn, main = name,
                                      shape = "ellipse", fontsize = 12, render = FALSE)
    graph::nodeRenderInfo(bn.viz)$fill = "#D6EAF8"
    graph::nodeRenderInfo(bn.viz)$col = "#4D4D4D"
    graph::nodeRenderInfo(bn.viz)$fill[nodes] = "#A9DFBF"
    graph::edgeRenderInfo(bn.viz)$col = "#4D4D4D"
    graph::edgeRenderInfo(bn.viz)$lwd = 3
    graph::nodeRenderInfo(bn.viz)$lwd = 1
    renderGraph(bn.viz)
  }
}

