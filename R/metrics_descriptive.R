#' Calculate Descriptive Metrics for a Bayesian Network
#'
#' This function calculates a set of descriptive metrics for a Bayesian network represented as a `bn` object.
#' These metrics include the number of edges, colliders, root nodes, leaf nodes, isolated nodes, directed arcs,
#' undirected arcs, compelled arcs, reversible arcs, and v-structures. Additionally, it calculates node-specific metrics
#' such as in-degree, out-degree, and the number of incident arcs for a specified node, if provided.
#'
#' @param bn An object of class `bn` representing a Bayesian network. This should be created using the `bnlearn` package.
#' @param node (Optional) A specific node (character string) within the network for which to calculate
#'        the in-degree, out-degree, and the number of incident arcs. If not provided, these node-specific metrics
#'        will be set to "n/a".
#'
#' @return A named list containing the following metrics:
#' \item{n_edges}{The total number of edges (arcs) in the Bayesian network.}
#' \item{n_colliders}{The total number of colliders (v-structures) in the network.}
#' \item{n_root_nodes}{The number of root nodes (nodes with no parents) in the network.}
#' \item{n_leaf_nodes}{The number of leaf nodes (nodes with no children) in the network.}
#' \item{n_isolated_nodes}{The number of isolated nodes (nodes with no parents or children) in the network.}
#' \item{n_nodes}{The total number of nodes in the network.}
#' \item{n_directed_arcs}{The number of directed arcs in the network.}
#' \item{n_undirected_arcs}{The number of undirected arcs in the network.}
#' \item{n_compelled_arcs}{The number of compelled arcs (arcs that must exist in the DAG).}
#' \item{n_reversible_arcs}{The number of reversible arcs (arcs with an undetermined direction).}
#' \item{in_degree}{The in-degree of the specified node (number of incoming arcs). Returned as "n/a" if `node` is not provided.}
#' \item{out_degree}{The out-degree of the specified node (number of outgoing arcs). Returned as "n/a" if `node` is not provided.}
#' \item{n_incident_arcs}{The total number of incident arcs (connected to the specified node). Returned as "n/a" if `node` is not provided.}
#'
#' @examples
#' # Example Bayesian network using the bnlearn package
#' library(bnlearn)
#' bn <- random.graph(nodes = c("A", "B", "C", "D"))
#' metrics <- calculate_descriptive_metrics(bn)
#' print(metrics)
#'
#' # Example with node-specific metrics
#' metrics_with_node <- calculate_descriptive_metrics(bn, node = "A")
#' print(metrics_with_node)
#'
#' @seealso \code{\link[bnlearn]{bn}}, \code{\link[bnlearn]{narcs}}, \code{\link[bnlearn]{vstructs}},
#' \code{\link[bnlearn]{root.nodes}}, \code{\link[bnlearn]{leaf.nodes}},
#' \code{\link[bnlearn]{isolated.nodes}}, \code{\link[bnlearn]{cpdag}}
#'
#' @importFrom bnlearn narcs vstructs root.nodes leaf.nodes isolated.nodes nnodes cpdag
#' @importFrom bnlearn directed.arcs undirected.arcs compelled.arcs reversible.arcs in.degree out.degree incident.arcs
#' @export
calculate_descriptive_metrics <- function(bn, node = NULL) {
  metrics <- list(
    n_edges = narcs(bn),
    n_colliders = length(vstructs(bn)) / 3,
    n_root_nodes = length(root.nodes(bn)),
    n_leaf_nodes = length(leaf.nodes(bn)),
    n_isolated_nodes = length(isolated.nodes(bn)),
    n_nodes = nnodes(bn),
    n_directed_arcs = length(directed.arcs(bn)) / 2,
    n_undirected_arcs = length(undirected.arcs(bn)) / 4,
    n_compelled_arcs = length(compelled.arcs(bn)) / 2,
    n_reversible_arcs = length(reversible.arcs(bn)) / 4
  )

  # Add metrics specific to a given node, if provided, otherwise set to "n/a"
  if (!is.null(node)) {
    metrics$in_degree <- in.degree(bn, node)
    metrics$out_degree <- out.degree(bn, node)
    metrics$n_incident_arcs <- length(unique(t(apply(incident.arcs(cpdag(bn), node), 1, sort)))) / 2
  } else {
    metrics$in_degree <- "n/a"
    metrics$out_degree <- "n/a"
    metrics$n_incident_arcs <- "n/a"
  }

  return(metrics)
}
