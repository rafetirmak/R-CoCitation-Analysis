# Supplementary_Code_2_Metadata_Enrichment_rentrez.R
# Purpose: Retrieve PubMed node metadata (journal, dates, first author, title) for a list of PMIDs
# Data source: NCBI E-utilities via rentrez (entrez_summary)
# Input: a text file containing PMIDs (one per line)
# Output: pubmed_summary_raw.csv

# -----------------------------
# Configuration (edit as needed)
# -----------------------------
INPUT_FILE        <- "002_node_summary.txt"
OUTPUT_FILE       <- "pubmed_summary_raw.csv"

SLEEP_EVERY_N     <- 9
DELAY_SEC         <- 0.10

# -----------------------------
# Setup
# -----------------------------
getwd()
library(rentrez)

# OPTIONAL (recommended):
# set_entrez_email("your_email@example.com")

# OPTIONAL:
# Sys.setenv(ENTREZ_KEY = "YOUR_NCBI_API_KEY")

# -----------------------------
# Input PMIDs
# -----------------------------
t <- scan(INPUT_FILE, what = character(), quiet = TRUE)
t <- as.character(t)
t <- t[nzchar(t)]
t <- unique(t)

# -----------------------------
# Helper: safely extract first author
# -----------------------------
get_first_author <- function(authors_obj) {
  if (is.null(authors_obj)) return(NA_character_)
  # rentrez summary often returns a character vector or list of strings
  if (is.character(authors_obj) && length(authors_obj) >= 1) return(authors_obj[1])

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

  s <- tryCatch(
    entrez_summary(db = "pubmed", id = pmid),
    error = function(e) NULL
  )

  if (is.null(s)) {
    g <- c(pmid, NA, NA, NA, NA, NA, NA)
    h <- rbind(h, g)
  } else {
    # rentrez returns an "esummary" object; coerce to list for field access
    f <- tryCatch(as.list(s), error = function(e) list())

    id        <- if (!is.null(f$uid)) as.character(f$uid) else as.character(pmid)
    j_abbr    <- if (!is.null(f$source)) as.character(f$source) else NA_character_
    journal   <- if (!is.null(f$fulljournalname)) as.character(f$fulljournalname) else NA_character_
    pub_date  <- if (!is.null(f$pubdate)) as.character(f$pubdate) else NA_character_
    epub_date <- if (!is.null(f$epubdate)) as.character(f$epubdate) else NA_character_

    # Authors: often in f$authors as list of {name=...}; handle both patterns
    first_auth <- NA_character_
    if (!is.null(f$authors)) {
      if (is.list(f$authors) && length(f$authors) >= 1) {
        # common rentrez pattern: list of lists with $name
        if (!is.null(f$authors[[1]]$name)) first_auth <- as.character(f$authors[[1]]$name)
        else first_auth <- get_first_author(f$authors)
      } else {
        first_auth <- get_first_author(f$authors)
      }
    }

    title     <- if (!is.null(f$title)) as.character(f$title) else NA_character_

    g <- c(id, j_abbr, journal, pub_date, epub_date, first_auth, title)
    h <- rbind(h, g)
  }

  cat(i, " ", pmid, "\n")
  flush.console()

  if ((i %% SLEEP_EVERY_N) == 0) {
    Sys.sleep(DELAY_SEC)
  }
}

write.csv2(h, file = OUTPUT_FILE, row.names = FALSE)
