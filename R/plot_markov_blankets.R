#' Visualize and Compare Bayesian Networks, Highlighting Matching Arcs
#'
#' This function visualizes two Bayesian networks side by side, either as full networks or restricted to
#' specified nodes and their Markov blankets. Matching arcs between the two networks are highlighted
#' for easy comparison.
#'
#' @param bn1 A Bayesian network (bn object) representing the first network.
#' @param bn2 A Bayesian network (bn object) representing the second network.
#' @param nodes A character vector specifying the names of the nodes to visualize along with their Markov blankets.
#'        If \code{"all"}, the entire network structures are visualized. Default is \code{"all"}.
#'
#' @return A side-by-side visualization of the two Bayesian networks, with matching arcs highlighted.
#'
#' @details
#' The function creates subgraphs of the Bayesian networks based on the provided nodes. If \code{"all"} is specified,
#' the entire network structures are visualized. Matching arcs between the two networks are styled differently
#' for emphasis.
#'
#' Visualization details:
#' \itemize{
#'   \item Matching arcs are highlighted with a specific color.
#'   \item Nodes are styled uniformly across both networks.
#'   \item Non-matching arcs are styled with a default color for each network.
#' }
#'
#' The function uses \code{bnlearn::graphviz.plot()} for visualization, along with the \code{Rgraphviz} package for customization.
#'
#' @examples
#' library(bnlearn)
#'
#' # Create two random Bayesian networks
#' bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
#' bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Visualize the full networks side by side
#' vis_dag_comparison(bn1, bn2, nodes = "all")
#'
#' # Visualize specific nodes and their Markov blankets
#' vis_dag_comparison(bn1, bn2, nodes = c("A", "B"))
#'
#' @seealso \code{\link[bnlearn]{graphviz.plot}}, \code{\link[bnlearn]{mb}}, \code{\link[bnlearn]{subgraph}}
#'
#' @importFrom bnlearn mb subgraph arcs graphviz.plot
#' @importFrom graph nodeRenderInfo edgeRenderInfo
#' @import Rgraphviz
#' @export

vis_dag_comparison <- function(bn1, bn2, option = 1, nodes = "all", name1 = "DAG 1", name2 = "DAG 2") {

  # Basic Input Validation
  if (!inherits(bn1, "bn")) {
    stop("bn1 must be a valid Bayesian network object (class 'bn').")
  }
  if (!inherits(bn2, "bn")) {
    stop("bn2 must be a valid Bayesian network object (class 'bn').")
  }

  # Load required libraries
  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("The 'bnlearn' package is required but not installed.")
  }

  # Set up plotting window for side by side plots
  op <- par(mfrow = c(1, 2))  # Save original parameters to restore later

  # Ensure original plotting parameters are restored on function exit
  on.exit(par(op))

  # Determine subgraphs based on the 'nodes' parameter
  if (identical(nodes, "all")) {
    subgraph_bn1 <- bn1
    subgraph_bn2 <- bn2
  } else {
    if (option == 1){
      # Extract Markov blankets for the specified nodes
      mb_bn1 <- unique(unlist(lapply(nodes, function(node) bnlearn::mb(bn1, node))))
      mb_bn2 <- unique(unlist(lapply(nodes, function(node) bnlearn::mb(bn2, node))))

      # Create subgraphs containing the specified nodes and their Markov blankets
      subgraph_bn1 <- bnlearn::subgraph(bn1, nodes = unique(c(nodes, mb_bn1)))
      subgraph_bn2 <- bnlearn::subgraph(bn2, nodes = unique(c(nodes, mb_bn2)))
    } else if(option == 2){
      # Extract the Markov blanket of the nodes in bn1 and compare with the same nodes in bn2
      mb_bn1 <- unique(unlist(lapply(nodes, function(node) bnlearn::mb(bn1, node))))

      # Subgraphs containing the nodes and their Markov blankets in bn1 and the same set of nodes in bn2
      subgraph_bn1 <- bnlearn::subgraph(bn1, nodes = unique(c(nodes, mb_bn1)))
      subgraph_bn2 <- bnlearn::subgraph(bn2, nodes = unique(c(nodes, mb_bn1)))
    } else{
      stop("\n
Please choose either option 1 or option 2\n
Option 1: Comparison of Markov blankets of two DAGs\n
Option 2: Comparison of Markov blanket of the first DAG with the same set of nodes in the other DAG")
    }
  }

  # Extract arcs from both subgraphs
  arcs_bn1 <- bnlearn::arcs(subgraph_bn1)
  arcs_bn2 <- bnlearn::arcs(subgraph_bn2)

  # Convert arcs to a comparable format (concatenated strings)
  arcs_bn1_str <- apply(arcs_bn1, 1, paste, collapse = "|")
  arcs_bn2_str <- apply(arcs_bn2, 1, paste, collapse = "|")

  # Identify matching arcs using base R's intersect
  matching_arcs_str <- intersect(arcs_bn1_str, arcs_bn2_str)

  # Convert matching arcs back to a two-column matrix
  if (length(matching_arcs_str) > 0) {
    matching_arcs <- do.call(rbind, strsplit(matching_arcs_str, "\\|"))
    colnames(matching_arcs) <- c("from", "to")
    matching_arcs <- as.matrix(matching_arcs)  # Ensure it's a matrix
  } else {
    matching_arcs <- NULL  # No matching arcs
  }


  if (length(arcs(subgraph_bn1)) == 0) {

    # If the network has edges, customize both nodes and edges
    bn1.viz <- bnlearn::graphviz.plot(subgraph_bn1, main = name1,
                                      shape = "ellipse", fontsize = 12, highlight = list(
                                        nodes = nodes(subgraph_bn1), col = "#4D4D4D",
                                        fill = "#A9DFBF"))

} else {
    # If the network does not have edges, only customize nodes
    bn1.viz <- bnlearn::graphviz.plot(subgraph_bn1, main = name1,
                                      shape = "ellipse", fontsize = 12, render = FALSE)
    graph::nodeRenderInfo(bn1.viz)$fill = "#D6EAF8"
    graph::nodeRenderInfo(bn1.viz)$col = "#4D4D4D"
    graph::nodeRenderInfo(bn1.viz)$fill[nodes] = "#A9DFBF"
    graph::edgeRenderInfo(bn1.viz)$col = "#4D4D4D"
    graph::edgeRenderInfo(bn1.viz)$lwd = 3

    if (!is.null(matching_arcs)) {
      # Ensure that arcs are unique and sorted
      unique_arcs <- as.data.frame(t(apply(matching_arcs, 1, sort)), stringsAsFactors = FALSE)
      colnames(unique_arcs) <- c("from", "to")
      unique_arcs <- unique_arcs[!duplicated(unique_arcs), ]

      # Extract the unique nodes from the arcs
      unique_nodes <- unique(c(unique_arcs$from, unique_arcs$to))

      # Create the subgraph with the unique nodes
      subgraph_result <- subgraph(subgraph_bn1, unique_nodes)

      # Extract the arcs from the subgraph
      g <- arcs(subgraph_result)

      # Format the arcs for rendering
      g <- as.character(interaction(g[, "from"], g[, "to"], sep = "~"))

      # Set the edge color in the visualization
      graph::edgeRenderInfo(bn1.viz)$col[g] <- "#27AE60"
    }
    graph::nodeRenderInfo(bn1.viz)$lwd = 1
    renderGraph(bn1.viz)
  }




  if (length(arcs(subgraph_bn2)) == 0) {
    # If the network has edges, customize both nodes and edges
    bn2.viz <- bnlearn::graphviz.plot(subgraph_bn2, main = name2,
                                      shape = "ellipse", fontsize = 12, highlight = list(
                                        nodes = nodes(subgraph_bn2), col = "#4D4D4D",
                                        fill = "#A9DFBF"))
  } else {
    # If the network does not have edges, only customize nodes
    bn2.viz <- bnlearn::graphviz.plot(subgraph_bn2, main = name2,
                                      shape = "ellipse", fontsize = 12, render = FALSE)
    graph::nodeRenderInfo(bn2.viz)$fill = "#D6EAF8"
    graph::nodeRenderInfo(bn2.viz)$col = "#4D4D4D"
    graph::nodeRenderInfo(bn2.viz)$fill[nodes] = "#A9DFBF"
    graph::edgeRenderInfo(bn2.viz)$col = "#4D4D4D"
    graph::edgeRenderInfo(bn2.viz)$lwd = 3

    if (!is.null(matching_arcs)) {
      # Ensure that arcs are unique and sorted
      unique_arcs <- as.data.frame(t(apply(matching_arcs, 1, sort)), stringsAsFactors = FALSE)
      colnames(unique_arcs) <- c("from", "to")
      unique_arcs <- unique_arcs[!duplicated(unique_arcs), ]

      # Extract the unique nodes from the arcs
      unique_nodes <- unique(c(unique_arcs$from, unique_arcs$to))

      # Create the subgraph with the unique nodes
      subgraph_result <- subgraph(subgraph_bn2, unique_nodes)

      # Extract the arcs from the subgraph
      g <- arcs(subgraph_result)

      # Format the arcs for rendering
      g <- as.character(interaction(g[, "from"], g[, "to"], sep = "~"))

      # Set the edge color in the visualization
      graph::edgeRenderInfo(bn2.viz)$col[g] <- "#27AE60"
    }
    graph::nodeRenderInfo(bn2.viz)$lwd = 1
    renderGraph(bn2.viz)
  }

  # Inform the user about matching nodes and arcs
  matching_nodes <- intersect(bnlearn::nodes(subgraph_bn1), bnlearn::nodes(subgraph_bn2))

  if (length(matching_nodes) > 0) {
    cat("Matching nodes: ", paste(matching_nodes, collapse = ", "), "\n")
  } else {
    cat("No matching nodes.\n")
  }

  if (!is.null(matching_arcs)) {
    cat("Number of matching arcs highlighted in green: ", nrow(matching_arcs), "\n")
  } else {
    cat("No matching arcs to highlight.\n")
  }
}
