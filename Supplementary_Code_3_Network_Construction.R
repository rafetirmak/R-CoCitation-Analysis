# Supplementary_Code_3_Network_Construction.R
# Purpose: Build a citation network from an edge list and derive a similarity network
#          (bibliographic coupling or co-citation) using igraph.
# Input:
#   - PMC_PUBMED_citation_raw_edgelist.csv produced by Supplementary_Code_1
#     Expected columns (CSV2): Target;Source  (Target = cited item, Source = citing item)
# Output:
#   - similarity_edgelist_unweighted.csv
#   - similarity_edgelist_weighted.csv

# -----------------------------
# Configuration (edit as needed)
# -----------------------------
INPUT_EDGE_FILE  <- "PMC_PUBMED_citation_raw_edgelist.csv"
OUT_UNWEIGHTED   <- "similarity_edgelist_unweighted.csv"
OUT_WEIGHTED     <- "similarity_edgelist_weighted.csv"

# Choose similarity type: "bibcoupling" or "cocitation"
SIMILARITY_TYPE  <- "bibcoupling"

# -----------------------------
# Setup
# -----------------------------
getwd()

# memory.limit() is Windows-specific; guard it to avoid errors on other OSes
if (Sys.info()[["sysname"]] == "Windows") {
  # Attempt to raise memory limit (best-effort)
  try(memory.limit(3 * 1024), silent = TRUE)  # 3 GB (in MB)
}

library(igraph)

# -----------------------------
# Read edge list
# -----------------------------
# read.csv2 assumes ';' separator by default (typical in write.csv2 outputs)
a <- read.csv2(INPUT_EDGE_FILE, header = TRUE, stringsAsFactors = FALSE)

# Basic sanity check
stopifnot(all(c("Target", "Source") %in% colnames(a)))

# Build a directed citation graph:
# Source --> Target means: Source article cites Target article
c_graph <- graph_from_data_frame(a[, c("Source", "Target")], directed = TRUE)

cat("Citation graph nodes:", vcount(c_graph), "\n")
cat("Citation graph edges:", ecount(c_graph), "\n")

# -----------------------------
# Derive similarity network
# -----------------------------
# NOTE:
# - bibliographic coupling connects documents that share references (outgoing links)
# - co-citation connects documents that are cited together (incoming links)

if (SIMILARITY_TYPE == "bibcoupling") {
  sim_mat <- bibcoupling(c_graph)
} else if (SIMILARITY_TYPE == "cocitation") {
  sim_mat <- cocitation(c_graph)
} else {
  stop("SIMILARITY_TYPE must be either 'bibcoupling' or 'cocitation'")
}

# Convert adjacency matrix to an undirected graph
sim_graph <- graph_from_adjacency_matrix(sim_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

cat("Similarity graph nodes:", vcount(sim_graph), "\n")
cat("Similarity graph edges:", ecount(sim_graph), "\n")

# -----------------------------
# Clean and weight edges
# -----------------------------
# Remove isolated nodes (degree 0)
sim_graph <- delete_vertices(sim_graph, which(degree(sim_graph) == 0))

cat("After removing isolates - nodes:", vcount(sim_graph), "\n")
cat("After removing isolates - edges:", ecount(sim_graph), "\n")

# Ensure a simple graph:
# - remove multiple edges by summing weights
# - remove loops
sim_graph <- simplify(
  sim_graph,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = list(weight = "sum")
)

# -----------------------------
# Export edge lists
# -----------------------------
# Unweighted edge list (just pairs)
unweighted <- as_edgelist(sim_graph, names = TRUE)
unweighted <- as.data.frame(unweighted, stringsAsFactors = FALSE)
colnames(unweighted) <- c("source", "target")

# Weighted edge list
weighted <- unweighted
weighted$weight <- E(sim_graph)$weight

write.csv2(unweighted, file = OUT_UNWEIGHTED, row.names = FALSE)
write.csv2(weighted,   file = OUT_WEIGHTED,   row.names = FALSE)

cat("Saved:", OUT_UNWEIGHTED, "\n")
cat("Saved:", OUT_WEIGHTED, "\n")
