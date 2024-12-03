# DAGMetrics: A Package for Evaluating and Comparing Bayesian Networks


`DAGMetrics` is an R package designed to evaluate, compare, and visualize Bayesian networks (DAGs). It provides tools to compute descriptive metrics, compare structures using similarity measures, and visualize individual networks or side-by-side comparisons.

---

## Installation

Install the package directly from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install DAGMetrics from GitHub
devtools::install_github("averinpa/DAGMetrics")
```
## Features

### Descriptive Metrics
- Calculate structural properties of DAGs, including:
  - **Number of Edges**: Total directed arcs in the network.
  - **Number of Colliders**: Number of v-structures in the DAG.
  - **Root Nodes**: Nodes without parents.
  - **Leaf Nodes**: Nodes without children.
  - **Isolated Nodes**: Nodes with no parents or children.
  - **Degree-Based Metrics**:
    - In-degree: Number of incoming arcs to a node.
    - Out-degree: Number of outgoing arcs from a node.
    - Incident Arcs: Total arcs connected to a node.

### Comparison Metrics
- Compare two DAGs using:
  - **Structural Hamming Distance (SHD)**:
    - Measures edge changes (additions, deletions, reversals) needed to transform one DAG into another.
  - **Hamming Distance (HD)**:
    - Measures edge presence/absence differences between two DAGs.
  - **True Positives (TP)**:
    - Edges correctly present in both DAGs.
  - **False Positives (FP)**:
    - Edges present in the second DAG but not in the first.
  - **False Negatives (FN)**:
    - Edges present in the first DAG but not in the second.
  - **Precision, Recall, and F1 Score**:
    - Precision: Proportion of correctly predicted edges out of all predicted edges.
    - Recall: Proportion of correctly predicted edges out of all true edges.
    - F1 Score: Harmonic mean of precision and recall.

### Visualization
- **Single DAG Visualization**:
  - Visualize the entire network structure.
  - Highlight specific nodes or Markov blankets for targeted analysis.
- **Side-by-Side DAG Comparison**:
  - Compare two DAGs, highlighting:
    - Matching arcs.
    - Differences in structure.
  - Visualize subgraphs or the full network.

### Additional Features
- Easy integration with other R packages like `bnlearn` and `pcalg`.
- Scalable for small to moderately large Bayesian networks.
- Designed for causal discovery and structure learning tasks.




## Usage

Here’s how to use the `DAGMetrics` package to evaluate, compare, and visualize Bayesian networks:

### 1. Load the Library
```r
library(DAGMetrics)
library(bnlearn)
```
### 2. Create Example Bayesian Networks
Generate two random Bayesian networks using the bnlearn package:
```r
# Generate two random Bayesian networks
bn1 <- random.graph(nodes = c("A", "B", "C", "D"))
bn2 <- random.graph(nodes = c("A", "B", "C", "D"))
```
### 3. Calculate Descriptive Metrics
Analyze the structural properties of a Bayesian network:

```r
# Calculate descriptive metrics for bn1
descriptive_metrics <- calculate_descriptive_metrics(bn1)
print(descriptive_metrics)
```
### 4. Compare Two Bayesian Networks
Compute various comparison metrics between two Bayesian networks:
```r
# Compare bn1 and bn2
comparison_results <- compare_dags(bn1, bn2)
print(comparison_results)
```
### 5. Visualize a Bayesian Network
Visualize the full structure of a Bayesian network or a subgraph:
```r
# Visualize the entire DAG structure of bn1
vis_dag(bn1, nodes = "all", name = "Full DAG")
```
### 6. Compare and Visualize Two Networks
Visualize two Bayesian networks side-by-side and highlight their matching arcs:
```r
# Compare and visualize bn1 and bn2
vis_dag_comparison(bn1, bn2, nodes = "all")
```
## Contributing

We welcome contributions! To contribute:
1. Fork the repository.
2. Make your changes.
3. Submit a pull request.

For bug reports or feature requests, open an issue in the repository.

---

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

## Acknowledgments

`DAGMetrics` is built using:
- [`bnlearn`](https://cran.r-project.org/package=bnlearn) for Bayesian network structure learning.
- [`pcalg`](https://cran.r-project.org/package=pcalg) for causal structure discovery.
- [`Rgraphviz`](https://bioconductor.org/packages/release/bioc/html/Rgraphviz.html) for DAG visualization.

---

## Author

**Pavel Averin**  
[Email](mailto:averinj@gmail.com) | [GitHub](https://github.com/averinpa)

---

## Feedback

If you find this package useful or have suggestions for improvement, feel free to open an issue or contact me directly.
