# In R/metrics_comparison.R

#' Calculate SHD (Structural Hamming Distance) between Two DAGs
#'
#' This function calculates the Structural Hamming Distance (SHD) between two directed acyclic graphs (DAGs),
#' represented as Bayesian networks (`bn` objects), for a given set of nodes. The SHD measures the number of
#' edge differences (additions, deletions, or reversals) needed to transform one graph into the other.
#'
#' @param bn1 An object of class `bn`, representing the first Bayesian network.
#' @param bn2 An object of class `bn`, representing the second Bayesian network.
#' @param nodes A character vector specifying the names of the nodes to include in the comparison.
#'
#' @return A numeric value representing the Structural Hamming Distance (SHD) between the two DAGs for the specified nodes.
#'
#' @details The function uses the \code{subgraph} function from the \pkg{bnlearn} package to extract the subgraphs
#' of interest from both networks. These subgraphs are converted to `graphNEL` objects for compatibility with the
#' \code{shd} function from the \pkg{pcalg} package.
#'
#' @examples
#' library(bnlearn)
#' library(pcalg)
#'
#' # Create two random Bayesian networks
#' bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
#' bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Calculate SHD for all nodes
#' shd_all_nodes <- calculate_shd(bn1, bn2, nodes = c("A", "B", "C", "D"))
#' print(shd_all_nodes)
#'
#' # Calculate SHD for a subset of nodes
#' shd_subset <- calculate_shd(bn1, bn2, nodes = c("A", "B"))
#' print(shd_subset)
#'
#' @seealso \code{\link[bnlearn]{subgraph}}, \code{\link[pcalg]{shd}}
#' @importFrom pcalg shd
#' @importFrom bnlearn subgraph
#' @export
#'

calculate_shd <- function(bn1, bn2, nodes) {
  sub_bn1 <- as.graphNEL(subgraph(bn1, nodes))
  sub_bn2 <- as.graphNEL(subgraph(bn2, nodes))

  shd_value <- pcalg::shd(sub_bn1, sub_bn2)
  return(shd_value)
}

#' Calculate HD (Hamming Distance) between Two DAGs
#'
#' This function calculates the Hamming Distance (HD) between two directed acyclic graphs (DAGs),
#' represented as Bayesian networks (`bn` objects), for a given set of nodes. The HD measures the
#' number of differing edges (presence/absence) between two graphs without considering edge orientation.
#'
#' @param bn1 An object of class `bn`, representing the first Bayesian network.
#' @param bn2 An object of class `bn`, representing the second Bayesian network.
#' @param nodes A character vector specifying the names of the nodes to include in the comparison.
#'
#' @return A numeric value representing the Hamming Distance (HD) between the two DAGs for the specified nodes.
#'
#' @details The function uses the \code{subgraph} function from the \pkg{bnlearn} package to extract the subgraphs
#' of interest from both networks. It then applies the \code{hamming} function from the \pkg{bnlearn} package
#' to calculate the edge differences.
#'
#' @examples
#' library(bnlearn)
#'
#' # Create two random Bayesian networks
#' bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
#' bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Calculate HD for all nodes
#' hd_all_nodes <- calculate_hd(bn1, bn2, nodes = c("A", "B", "C", "D"))
#' print(hd_all_nodes)
#'
#' # Calculate HD for a subset of nodes
#' hd_subset <- calculate_hd(bn1, bn2, nodes = c("A", "B"))
#' print(hd_subset)
#'
#' @seealso \code{\link[bnlearn]{hamming}}, \code{\link[bnlearn]{subgraph}}
#' @importFrom bnlearn hamming subgraph
#' @export
calculate_hd <- function(bn1, bn2, nodes) {
  sub_bn1 <- subgraph(bn1, nodes)
  sub_bn2 <- subgraph(bn2, nodes)

  hd_value <- bnlearn::hamming(sub_bn1, sub_bn2)
  return(hd_value)
}

#' Calculate True Positives (TP), False Positives (FP), and False Negatives (FN) Between Two DAGs
#'
#' This function calculates the number of true positives (TP), false positives (FP), and false negatives (FN)
#' between two directed acyclic graphs (DAGs), represented as Bayesian networks (`bn` objects), for a given set of nodes.
#'
#' @param bn1 An object of class `bn`, representing the first Bayesian network.
#' @param bn2 An object of class `bn`, representing the second Bayesian network.
#' @param nodes A character vector specifying the names of the nodes to include in the comparison.
#'
#' @return A list containing the following metrics:
#' \item{tp}{The number of true positives, representing the edges that exist in both DAGs.}
#' \item{fp}{The number of false positives, representing the edges that exist in the second DAG but not in the first.}
#' \item{fn}{The number of false negatives, representing the edges that exist in the first DAG but not in the second.}
#'
#' @details The function uses the \code{subgraph} function from the \pkg{bnlearn} package to extract the subgraphs
#' for the specified nodes. The \code{compare} function from the \pkg{bnlearn} package is then used to calculate
#' the metrics based on edge presence and absence.
#'
#' @examples
#' library(bnlearn)
#'
#' # Create two random Bayesian networks
#' bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
#' bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Calculate TP, FP, FN for all nodes
#' comparison_metrics <- calculate_tp_fp_fn(bn1, bn2, nodes = c("A", "B", "C", "D"))
#' print(comparison_metrics)
#'
#' # Calculate TP, FP, FN for a subset of nodes
#' subset_metrics <- calculate_tp_fp_fn(bn1, bn2, nodes = c("A", "B"))
#' print(subset_metrics)
#'
#' @seealso \code{\link[bnlearn]{compare}}, \code{\link[bnlearn]{subgraph}}
#' @importFrom bnlearn subgraph compare
#' @export
calculate_tp_fp_fn <- function(bn1, bn2, nodes) {
  sub_bn1 <- subgraph(bn1, nodes)
  sub_bn2 <- subgraph(bn2, nodes)
  comparison_metrics <- bnlearn::compare(sub_bn1, sub_bn2)
  return (list(tp = comparison_metrics$tp, fp = comparison_metrics$fp, fn = comparison_metrics$fn))
}
