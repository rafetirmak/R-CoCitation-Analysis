# Supplementary_Code_1_Citation_Retrieval.R
# Purpose: Retrieve citation links (cited-by) for a list of PubMed IDs (PMIDs)
# Data source: NCBI E-utilities via reutils
# Output:
#   1) PMC_PUBMED_citation_raw_edgelist.csv  (Target PMID, Source PMID)
#   2) hata_raw_edgelist.csv                 (PMIDs that returned repeated errors)

# -----------------------------
# Configuration (edit as needed)
# -----------------------------
DELAY_SEC            <- 0.04   # small pause to reduce throttling (e.g., 0.05 ~= 20 req/sec in ideal conditions)
SLEEP_EVERY_N        <- 9      # apply DELAY_SEC every N requests
CHECKPOINT_EVERY_N   <- 500    # write intermediate results every N PMIDs
RETRY_SLEEP_SEC      <- 1      # wait time before retrying after an API error
MAX_RETRIES_PER_PMID <- 50     # safety cap to avoid infinite loops

EDGE_FILE  <- "PMC_PUBMED_citation_raw_edgelist.csv"
ERROR_FILE <- "hata_raw_edgelist.csv"

# -----------------------------
# Setup
# -----------------------------
getwd()

library(reutils)

# IMPORTANT:
# Do not hardcode your API key in supplementary files.
# If you have an NCBI API key, set it locally before running:
# options(reutils.api.key = "YOUR_NCBI_API_KEY")
# Alternatively, set it in your .Renviron or environment variables.

# -----------------------------
# Input PMIDs
# -----------------------------
# Choose ONE of the following input methods:

# 1) From clipboard (default)
t <- readClipboard()

# 2) From a text file (one PMID per line)
# t <- scan("pubmed_result.txt", what = character(), quiet = TRUE)

# 3) Manual vector
# t <- c("8623078","11074683","11805642")

t <- as.character(t)
t <- t[nzchar(t)]
t <- unique(t)

# -----------------------------
# Initialize containers
# -----------------------------
edges <- matrix(c("Target", "Source"), nrow = 1, ncol = 2)
errors <- c("PMID_with_error")

# -----------------------------
# Main loop
# -----------------------------
for (i in seq_along(t)) {

  pmid <- t[i]

  # Retry loop for transient API errors
  tries <- 0
  repeat {
    tries <- tries + 1

    e <- elink(pmid, dbFrom = "pubmed", dbTo = "pubmed", linkname = "pubmed_pubmed_citedin")

    # If no error field exists, break
    if (is.null(e$errors$error)) break

    # Log error and retry
    message("NCBI error for PMID ", pmid, ": ", e$errors$error)
    errors <- c(errors, pmid)
    write.csv2(errors, file = ERROR_FILE)

    Sys.sleep(RETRY_SLEEP_SEC)

    # Safety cap
    if (tries >= MAX_RETRIES_PER_PMID) {
      message("Max retries reached for PMID ", pmid, ". Skipping.")
      break
    }
  }

  # Parse linkset
  f <- linkset(e)

  citedin <- f[["pubmed_pubmed_citedin"]]
  cited_count <- if (is.null(citedin)) 0 else length(citedin)

  # Add edges if any citing PMIDs exist
  if (cited_count > 0) {
    g <- cbind(rep(uid(e), cited_count), citedin)
    edges <- rbind(edges, g)
  }

  cat(i, " ", pmid, " ", cited_count, "\n")
  flush.console()

  # Rate limiting
  if ((i %% SLEEP_EVERY_N) == 0) {
    Sys.sleep(DELAY_SEC)
  }

  # Checkpoint saving
  if ((i %% CHECKPOINT_EVERY_N) == 0) {
    write.csv2(edges, file = EDGE_FILE)
    write.csv2(errors, file = ERROR_FILE)
  }
}

# -----------------------------
# Save final outputs
# -----------------------------
write.csv2(edges, file = EDGE_FILE)
write.csv2(errors, file = ERROR_FILE)

dim(edges)
