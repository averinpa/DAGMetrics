#' Compare DAGs and Compute Descriptive and Comparison Metrics
#'
#' This function compares two Bayesian networks (DAGs represented as `bn` objects) and computes both
#' descriptive metrics for each network and comparison metrics between the networks. Metrics include
#' Structural Hamming Distance (SHD), Hamming Distance (HD), true positives (TP), false positives (FP),
#' false negatives (FN), precision, recall, and F1 score. The user can choose specific metrics to include.
#'
#' @param bn1 A Bayesian network (bn object) representing the first DAG.
#' @param bn2 A Bayesian network (bn object) representing the second DAG.
#' @param descriptive_metrics A vector of descriptive metrics to calculate (e.g., "n_edges", "n_colliders").
#'        Default is "all", which computes all available descriptive metrics.
#' @param comparison_metrics A vector of comparison metrics to calculate (e.g., "shd", "tp", "fp", "fn").
#'        Default is "all", which computes all available comparison metrics.
#'
#' @return A data frame with the selected descriptive and comparison metrics for each node's Markov blanket
#'         and for the entire network (labeled as "All").
#'
#' @details The function works in two main steps:
#' \itemize{
#'   \item Descriptive Metrics: Computes specified metrics for the entire network and for each node's Markov blanket.
#'         Metrics include number of edges, colliders, root/leaf/isolated nodes, directed/undirected arcs, and degree-related metrics.
#'   \item Comparison Metrics: Compares the structure of two Bayesian networks, calculating SHD, HD, TP, FP, FN, precision, recall, and F1 score.
#'         These are calculated globally and for each node's Markov blanket.
#' }
#'
#' The user can filter the results to include only specific metrics by setting \code{descriptive_metrics} and \code{comparison_metrics}.
#'
#' @examples
#' library(bnlearn)
#'
#' # Generate two random Bayesian networks
#' bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
#' bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
#'
#' # Compare the DAGs using all metrics
#' result <- compare_dags(bn1, bn2)
#' print(result)
#'
#' # Compare the DAGs with specific metrics
#' result_filtered <- compare_dags(
#'   bn1, bn2,
#'   descriptive_metrics = c("n_edges", "n_colliders"),
#'   comparison_metrics = c("shd", "tp", "fp")
#' )
#' print(result_filtered)
#'
#' @seealso \code{\link[bnlearn]{nodes}}, \code{\link[bnlearn]{mb}}, \code{\link[bnlearn]{subgraph}},
#'          \code{\link[pcalg]{shd}}, \code{\link[bnlearn]{compare}}
#' @importFrom bnlearn nodes mb subgraph
#' @importFrom dplyr mutate
#' @importFrom pcalg shd
#' @export
compare_dags <- function(bn1, bn2, descriptive_metrics = "all", comparison_metrics = "all") {

  # Set default for descriptive metrics if 'all' is provided
  if (identical(descriptive_metrics, "all")) {
    descriptive_metrics <- c(
      "n_edges",
      "n_colliders",
      "n_root_nodes",
      "n_leaf_nodes",
      "n_isolated_nodes",
      "n_nodes",
      "n_directed_arcs",
      "n_undirected_arcs",
      "n_compelled_arcs",
      "n_reversible_arcs",
      "n_vstructs",
      "in_degree",
      "out_degree",
      "n_incident_arcs"
    )
  }

  # Set default for comparison metrics if 'all' is provided
  if (identical(comparison_metrics, "all")) {
    comparison_metrics <- c("shd", "hd", "tp", "fp", "fn", "precision", "recall", "f1")
  }

  # Initialize result dataframe to store all metrics
  result <- data.frame(Variable = character(),
                       base_n_edges = integer(),
                       base_n_colliders = integer(),
                       base_n_root_nodes = integer(),
                       base_n_leaf_nodes = integer(),
                       base_n_isolated_nodes = integer(),
                       base_n_nodes = integer(),
                       base_n_directed_arcs = integer(),
                       base_n_undirected_arcs = integer(),
                       base_n_compelled_arcs = integer(),
                       base_n_reversible_arcs = integer(),
                       base_n_vstructs = integer(),
                       base_in_degree = character(),
                       base_out_degree = character(),
                       base_n_incident_arcs = character(),
                       n_edges = integer(),
                       n_colliders = integer(),
                       n_root_nodes = integer(),
                       n_leaf_nodes = integer(),
                       n_isolated_nodes = integer(),
                       n_nodes = integer(),
                       n_directed_arcs = integer(),
                       n_undirected_arcs = integer(),
                       n_compelled_arcs = integer(),
                       n_reversible_arcs = integer(),
                       n_vstructs = integer(),
                       in_degree = character(),
                       out_degree = character(),
                       n_incident_arcs = character(),
                       shd = character(),
                       hd = character(),
                       tp = character(),
                       fp = character(),
                       fn = character(),
                       stringsAsFactors = FALSE)



  # Compare Markov blankets of individual nodes
  node_names <- nodes(bn1)

  for (node in node_names) {
    # Extract the Markov blanket for the node in bn1
    bn1_mb <- mb(bn1, node)
    bn2_mb <- mb(bn2, node)

    # If the Markov blanket is empty in bn1, include only the node itself
    if (length(bn1_mb) == 0) {
      bn1_mb <- node
    } else {
      bn1_mb <- c(node, bn1_mb)
    }

    # If the Markov blanket is empty in bn2, include only the node itself
    if (length(bn2_mb) == 0) {
      bn2_mb <- node
    } else {
      bn2_mb <- c(node, bn2_mb)
    }

    # Comparison metrics for markov blankets
    local_shd <- calculate_shd(bn1, bn2, bn1_mb)
    local_hd <- calculate_hd(bn1, bn2, bn1_mb)
    local_tp_fp_fn <- calculate_tp_fp_fn(bn1, bn2, bn1_mb)
    local_tp <- local_tp_fp_fn$tp
    local_fp <- local_tp_fp_fn$fp
    local_fn <- local_tp_fp_fn$fn

    bn1_s <- subgraph(bn1, bn1_mb)
    bn2_s <- subgraph(bn2, bn2_mb)

    # Compute all descriptive metrics for this node's Markov blanket in bn1 and bn2
    node_metrics_bn1 <- calculate_descriptive_metrics(bn1_s, node)
    node_metrics_bn2 <- calculate_descriptive_metrics(bn2_s, node)

    # Append the results for this node to the result data frame
    result <- rbind(result, data.frame(Variable = node,
                                       base_n_edges = node_metrics_bn1$n_edges,
                                       base_n_colliders = node_metrics_bn1$n_colliders,
                                       base_n_root_nodes = node_metrics_bn1$n_root_nodes,
                                       base_n_leaf_nodes = node_metrics_bn1$n_leaf_nodes,
                                       base_n_isolated_nodes = node_metrics_bn1$n_isolated_nodes,
                                       base_n_nodes = node_metrics_bn1$n_nodes,
                                       base_n_directed_arcs = node_metrics_bn1$n_directed_arcs,
                                       base_n_undirected_arcs = node_metrics_bn1$n_undirected_arcs,
                                       base_n_compelled_arcs = node_metrics_bn1$n_compelled_arcs,
                                       base_n_reversible_arcs = node_metrics_bn1$n_reversible_arcs,
                                       n_edges = node_metrics_bn2$n_edges,
                                       n_colliders = node_metrics_bn2$n_colliders,
                                       n_root_nodes = node_metrics_bn2$n_root_nodes,
                                       n_leaf_nodes = node_metrics_bn2$n_leaf_nodes,
                                       n_isolated_nodes = node_metrics_bn2$n_isolated_nodes,
                                       n_nodes = node_metrics_bn2$n_nodes,
                                       n_directed_arcs = node_metrics_bn2$n_directed_arcs,
                                       n_undirected_arcs = node_metrics_bn2$n_undirected_arcs,
                                       n_compelled_arcs = node_metrics_bn2$n_compelled_arcs,
                                       n_reversible_arcs = node_metrics_bn2$n_reversible_arcs,
                                       base_in_degree = node_metrics_bn1$in_degree,
                                       base_out_degree = node_metrics_bn1$out_degree,
                                       base_n_incident_arcs = node_metrics_bn1$n_incident_arcs,
                                       in_degree = node_metrics_bn2$in_degree,
                                       out_degree = node_metrics_bn2$out_degree,
                                       n_incident_arcs = node_metrics_bn2$n_incident_arcs,
                                       shd = local_shd,
                                       hd = local_hd,
                                       tp = local_tp,
                                       fp = local_fp,
                                       fn = local_fn))
  }

  # Calculate the comparison metrics (SHD, TP, FP, FN) for the entire network
  global_shd <- calculate_shd(bn1, bn2, nodes(bn1))
  global_hd <- calculate_hd(bn1, bn2, nodes(bn1))
  global_tp_fp_fn <- calculate_tp_fp_fn(bn1, bn2, nodes(bn1))
  global_tp <- global_tp_fp_fn$tp
  global_fp <- global_tp_fp_fn$fp
  global_fn <- global_tp_fp_fn$fn

  global_desc_bn1 <- calculate_descriptive_metrics(bn1)
  global_desc_bn2 <- calculate_descriptive_metrics(bn2)



  # Append global metrics to the result dataframe
  all_row <- data.frame(Variable = "All",
                        base_n_edges = global_desc_bn1$n_edges,
                        base_n_colliders = global_desc_bn1$n_colliders,
                        base_n_root_nodes = global_desc_bn1$n_root_nodes,
                        base_n_leaf_nodes = global_desc_bn1$n_leaf_nodes,
                        base_n_isolated_nodes = global_desc_bn1$n_isolated_nodes,
                        base_n_nodes = global_desc_bn1$n_nodes,
                        base_n_directed_arcs = global_desc_bn1$n_directed_arcs,
                        base_n_undirected_arcs = global_desc_bn1$n_undirected_arcs,
                        base_n_compelled_arcs = global_desc_bn1$n_compelled_arcs,
                        base_n_reversible_arcs = global_desc_bn1$n_reversible_arcs,
                        n_edges = global_desc_bn2$n_edges,
                        n_colliders = global_desc_bn2$n_colliders,
                        n_root_nodes = global_desc_bn2$n_root_nodes,
                        n_leaf_nodes = global_desc_bn2$n_leaf_nodes,
                        n_isolated_nodes = global_desc_bn2$n_isolated_nodes,
                        n_nodes = global_desc_bn2$n_nodes,
                        n_directed_arcs = global_desc_bn2$n_directed_arcs,
                        n_undirected_arcs = global_desc_bn2$n_undirected_arcs,
                        n_compelled_arcs = global_desc_bn2$n_compelled_arcs,
                        n_reversible_arcs = global_desc_bn2$n_reversible_arcs,
                        base_in_degree = "n/a",
                        base_out_degree = "n/a",
                        base_n_incident_arcs = "n/a",
                        in_degree = "n/a",
                        out_degree = "n/a",
                        n_incident_arcs = "n/a",
                        shd = global_shd,
                        hd = global_hd,
                        tp = global_tp,
                        fp = global_fp,
                        fn = global_fn)

  result <- rbind(all_row, result)

  # ---- Calculation of precision, recall and F1 score ----
  result <- result |>
    dplyr::mutate(
      precision = tp / (tp + fp),
      recall = tp / (tp + fn),
      f1 = 2 * (precision * recall) / (precision + recall)
    )



  # ---- FILTERING STEP ----

  # Combine descriptive and comparison metrics for filtering
  selected_metrics <- c(descriptive_metrics, comparison_metrics)

  # Build column names with the "base_" prefix where necessary
  selected_columns <- c("Variable")

  if ("n_edges" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_edges", "n_edges")
  }

  if ("n_colliders" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_colliders", "n_colliders")
  }

  if ("n_root_nodes" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_root_nodes", "n_root_nodes")
  }

  if ("n_leaf_nodes" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_leaf_nodes", "n_leaf_nodes")
  }

  if ("n_isolated_nodes" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_isolated_nodes", "n_isolated_nodes")
  }

  if ("n_nodes" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_nodes", "n_nodes")
  }

  if ("n_directed_arcs" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_directed_arcs", "n_directed_arcs")
  }

  if ("n_undirected_arcs" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_undirected_arcs", "n_undirected_arcs")
  }

  if ("n_compelled_arcs" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_compelled_arcs", "n_compelled_arcs")
  }

  if ("n_reversible_arcs" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_reversible_arcs", "n_reversible_arcs")
  }

  if ("in_degree" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_in_degree", "in_degree")
  }

  if ("out_degree" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_out_degree", "out_degree")
  }

  if ("n_incident_arcs" %in% descriptive_metrics) {
    selected_columns <- c(selected_columns, "base_n_incident_arcs", "n_incident_arcs")
  }

  # Comparison metrics
  if ("shd" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "shd")
  }
  if ("hd" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "hd")
  }

  if ("tp" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "tp")
  }

  if ("fp" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "fp")
  }

  if ("fn" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "fn")
  }

  if ("precision" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "precision")
  }

  if ("recall" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "recall")
  }

  if ("f1" %in% comparison_metrics) {
    selected_columns <- c(selected_columns, "f1")
  }

  # Filter the result data frame to include only the selected columns
  result <- result[, selected_columns]

  return(result)
}
