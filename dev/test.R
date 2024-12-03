
rm(list=ls())
devtools::load_all()
library(bnlearn)


?calculate_descriptive_metrics()

# Demonstration with synthetic DAGS ---------------------------------------
bn1 = model2network(paste("[C][B|A][A|C]", sep = ""))
bn2 = model2network(paste("[A][B|A][C|A]", sep = ""))
# use pcalg for SHD

compare_dags(cpdag(bn1), cpdag(bn2), descriptive_metrics = "all")

vis_dag_comparison(cpdag(bn1), cpdag(bn2), option = 1, nodes="all")
vis_dag(bn1)

?vis_dag()
package_help("DAGMetrics")
help(package="DAGMetrics")
library(DAGMetrics)
