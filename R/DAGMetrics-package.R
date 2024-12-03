#' DAGMetrics: A Package for Evaluating and Comparing Bayesian Networks
#'
#' The `DAGMetrics` package provides tools to evaluate, compare, and visualize Bayesian networks (DAGs).
#' It includes functionality for calculating descriptive metrics of DAGs, comparing two DAGs using various
#' comparison metrics (e.g., SHD, HD, TP, FP, FN, precision, recall, F1), and visualizing DAG structures or their
#' subgraphs with highlights for specific nodes and arcs.
#'
#' @section Key Features:
#' \itemize{
#'   \item \strong{Descriptive Metrics:} Calculate metrics such as the number of edges, colliders, root nodes,
#'         leaf nodes, isolated nodes, directed/undirected arcs, and degree-related metrics for nodes.
#'   \item \strong{Comparison Metrics:} Compare two DAGs and compute metrics such as Structural Hamming Distance (SHD),
#'         Hamming Distance (HD), true positives (TP), false positives (FP), false negatives (FN), precision,
#'         recall, and F1 score.
#'   \item \strong{Visualization:} Visualize entire DAGs or specific subgraphs, highlighting Markov blankets or
#'         matching arcs between two DAGs.
#' }
#'
#' @section Functions in the Package:
#' \itemize{
#'   \item \code{\link[DAGMetrics]{calculate_descriptive_metrics}}: Compute descriptive metrics for a Bayesian network.
#'   \item \code{\link[DAGMetrics]{calculate_shd}}: Calculate Structural Hamming Distance (SHD) between two DAGs.
#'   \item \code{\link[DAGMetrics]{calculate_hd}}: Calculate Hamming Distance (HD) between two DAGs.
#'   \item \code{\link[DAGMetrics]{calculate_tp_fp_fn}}: Compute true positives, false positives, and false negatives between two DAGs.
#'   \item \code{\link[DAGMetrics]{compare_dags}}: Compare two DAGs and compute descriptive and comparison metrics.
#'   \item \code{\link[DAGMetrics]{vis_dag}}: Visualize a Bayesian network or its subgraph, highlighting specific nodes.
#'   \item \code{\link[DAGMetrics]{vis_dag_comparison}}: Visualize and compare two Bayesian networks side by side, highlighting matching arcs.
#' }
#'
#' @section Example Usage:
#' Below is an example demonstrating key functionalities of the package:
#' @examples
#' library(DAGMetrics)
#' library(bnlearn)
#'
#' # Create two random Bayesian networks
#' bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
#' bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Calculate descriptive metrics
#' descriptive_metrics <- calculate_descriptive_metrics(bn1)
#' print(descriptive_metrics)
#'
#' # Compare two Bayesian networks
#' comparison_results <- compare_dags(bn1, bn2)
#' print(comparison_results)
#'
#' # Visualize a Bayesian network
#' vis_dag(bn1, nodes = "all", name = "Full DAG")
#'
#' # Visualize and compare two Bayesian networks
#' vis_dag_comparison(bn1, bn2, nodes = "all")
#'
#' @keywords Bayesian networks, DAGs, structure learning, causal discovery
"_PACKAGE"
