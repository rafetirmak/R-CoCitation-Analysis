# Supplementary_Code_2_Metadata_Enrichment.R
# Purpose: Retrieve PubMed node metadata (journal, dates, first author, title) for a list of PMIDs
# Data source: NCBI E-utilities via reutils (esummary)
# Input: a text file containing PMIDs (one per line)
# Output: pubmed_summary_raw.csv

# -----------------------------
# Configuration (edit as needed)
# -----------------------------
INPUT_FILE        <- "002_node_summary.txt"
OUTPUT_FILE       <- "pubmed_summary_raw.csv"

SLEEP_EVERY_N     <- 9       # apply sleep every N requests
DELAY_SEC         <- 0.10    # pause duration to reduce throttling

# -----------------------------
# Setup
# -----------------------------
getwd()
library(reutils)

# IMPORTANT:
# Do not hardcode your API key in supplementary files.
# If you have an NCBI API key, set it locally before running:
# options(reutils.api.key = "YOUR_NCBI_API_KEY")

# -----------------------------
# Input PMIDs
# -----------------------------
t <- scan(INPUT_FILE, what = character(), quiet = TRUE)
t <- as.character(t)
t <- t[nzchar(t)]
t <- unique(t)

# -----------------------------
# Helper: safely extract first author name
# -----------------------------
get_first_author <- function(authors_obj) {
  # authors_obj may be NULL, a vector, or a nested structure depending on parsing
  if (is.null(authors_obj)) return(NA_character_)

  # Common case: a character vector of author names
  if (is.character(authors_obj) && length(authors_obj) >= 1) return(authors_obj[1])

  # Sometimes parsed content may yield a list-like object; try to coerce
  tryCatch({
    a <- unlist(authors_obj, use.names = FALSE)
    if (length(a) >= 1) return(as.character(a[1]))
    NA_character_
  }, error = function(e) NA_character_)
}

# -----------------------------
# Initialize output table
# -----------------------------
h <- matrix(nrow = 0, ncol = 7)
colnames(h) <- c("ID", "J. Abbr.", "Journal", "Pub Date", "e-Pub Date", "1st Author", "Title")

# -----------------------------
# Main loop
# -----------------------------
for (i in seq_along(t)) {

  pmid <- t[i]

  e <- esummary(pmid, db = "pubmed")
  f <- content(e, "parsed")

  # Defensive extraction (fields may be missing)
  id        <- if (!is.null(f$Id)) as.character(f$Id) else NA_character_
  j_abbr    <- if (!is.null(f$Source)) as.character(f$Source) else NA_character_
  journal   <- if (!is.null(f$FullJournalName)) as.character(f$FullJournalName) else NA_character_
  pub_date  <- if (!is.null(f$PubDate)) as.character(f$PubDate) else NA_character_
  epub_date <- if (!is.null(f$EPubDate)) as.character(f$EPubDate) else NA_character_
  first_auth <- get_first_author(f$Authors$Author$Name)
  title     <- if (!is.null(f$Title)) as.character(f$Title) else NA_character_

  g <- c(id, j_abbr, journal, pub_date, epub_date, first_auth, title)
  h <- rbind(h, g)

  cat(i, " ", pmid, "\n")
  flush.console()

  # Rate limiting
  if ((i %% SLEEP_EVERY_N) == 0) {
    Sys.sleep(DELAY_SEC)
  }
}

# -----------------------------
# Save output
# -----------------------------
write.csv2(h, file = OUTPUT_FILE, row.names = FALSE)
