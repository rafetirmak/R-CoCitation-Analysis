# Supplementary_Code_1_Citation_Retrieval_rentrez.R
# Purpose: Retrieve citation links (cited-by) for a list of PubMed IDs (PMIDs)
# Data source: NCBI E-utilities via rentrez
# Output:
#   1) PMC_PUBMED_citation_raw_edgelist.csv  (Target PMID, Source PMID)
#   2) hata_raw_edgelist.csv                 (PMIDs that returned repeated errors)

# -----------------------------
# Configuration (edit as needed)
# -----------------------------
DELAY_SEC            <- 0.04
SLEEP_EVERY_N        <- 9
CHECKPOINT_EVERY_N   <- 500
RETRY_SLEEP_SEC      <- 1
MAX_RETRIES_PER_PMID <- 50

EDGE_FILE  <- "PMC_PUBMED_citation_raw_edgelist.csv"
ERROR_FILE <- "hata_raw_edgelist.csv"

# -----------------------------
# Setup
# -----------------------------
getwd()

library(rentrez)

# OPTIONAL (recommended):
# Set your email (NCBI best practice)
# entrez_email <- "your_email@example.com"
# set_entrez_email(entrez_email)

# OPTIONAL:
# If you have an NCBI API key, set it locally (do NOT hardcode in supplementary files):
# Sys.setenv(ENTREZ_KEY = "YOUR_NCBI_API_KEY")
# rentrez will pick it up automatically.

# -----------------------------
# Input PMIDs
# -----------------------------
# 1) From clipboard (Windows)
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
edges  <- matrix(c("Target", "Source"), nrow = 1, ncol = 2)
errors <- c("PMID_with_error")

# -----------------------------
# Helper: robust link retrieval
# -----------------------------
get_citedin_pmids <- function(pmid) {
  # pubmed_pubmed_citedin: returns PMIDs of articles that cite the target
  link_obj <- entrez_link(
    dbfrom   = "pubmed",
    id       = pmid,
    db       = "pubmed",
    cmd      = "neighbor",
    linkname = "pubmed_pubmed_citedin"
  )

  # rentrez returns a list; links live in link_obj$links$pubmed_pubmed_citedin
  citedin <- NULL
  if (!is.null(link_obj$links) && !is.null(link_obj$links$pubmed_pubmed_citedin)) {
    citedin <- link_obj$links$pubmed_pubmed_citedin
  }

  if (is.null(citedin)) return(character(0))
  as.character(citedin)
}

# -----------------------------
# Main loop
# -----------------------------
for (i in seq_along(t)) {

  pmid <- t[i]

  tries <- 0
  citedin <- character(0)

  repeat {
    tries <- tries + 1

    ok <- TRUE
    citedin <- tryCatch(
      get_citedin_pmids(pmid),
      error = function(e) {
        ok <<- FALSE
        message("NCBI error for PMID ", pmid, ": ", conditionMessage(e))
        character(0)
      }
    )

    if (ok) break

    # log error and retry
    errors <- c(errors, pmid)
    write.csv2(errors, file = ERROR_FILE)

    Sys.sleep(RETRY_SLEEP_SEC)

    if (tries >= MAX_RETRIES_PER_PMID) {
      message("Max retries reached for PMID ", pmid, ". Skipping.")
      break
    }
  }

  cited_count <- length(citedin)

  # Add edges: Target = pmid, Source = citing pmid
  if (cited_count > 0) {
    g <- cbind(rep(pmid, cited_count), citedin)
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
    write.csv2(edges,  file = EDGE_FILE)
    write.csv2(errors, file = ERROR_FILE)
  }
}

# -----------------------------
# Save final outputs
# -----------------------------
write.csv2(edges,  file = EDGE_FILE)
write.csv2(errors, file = ERROR_FILE)

dim(edges)
