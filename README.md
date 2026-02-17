# R-CoCitation-Analysis 
**Automated Bibliometric Networks from PubMed via NCBI E-utilities**

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXXX)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

## Description
This repository provides a **robust R-based workflow** for researchers and scientometricians to automate the retrieval of citation data and the construction of scholarly networks using PubMed IDs (PMIDs).

Unlike manual citation harvesting, these scripts utilize the **NCBI E-utilities API** (via `reutils` or `rentrez`) to fetch "cited-by" information and metadata. The workflow is specifically designed to handle API rate limits and potential connection errors through built-in checkpoints and retry loops, ensuring data integrity for large-scale analyses.

---

## Key Features
* **Citation Retrieval:** Automates the collection of "cited-by" links to build raw citation edgelists.
* **Metadata Enrichment:** Fetches detailed bibliographic information (Journal, Publication Date, First Author, Title) for any list of PMIDs.
* **Network Construction:** Converts raw citation data into **Bibliographic Coupling** or **Co-citation** networks using `igraph`.
* **Robustness:** Includes rate-limiting (sleep cycles) and progress checkpoints to prevent data loss during long-running tasks.
* **Format Compatibility:** Outputs standardized CSV files ready for further analysis or visualization in tools like **Gephi** or **VOSviewer**.

---

## Repository Structure

| File | Purpose | Key Library |
| :--- | :--- | :--- |
| `Supplementary_Code_1_Citation_Retrieval.R` | Retrieves citation links (cited-by) for target PMIDs. | `reutils` |
| `Supplementary_Code_1_Citation_Retrieval_rentrez.R` | Alternative retrieval script using the `rentrez` package. | `rentrez` |
| `Supplementary_Code_2_Metadata_Enrichment.R` | Collects metadata (authors, journals, dates) for PMIDs. | `reutils` |
| `Supplementary_Code_3_Network_Construction.R` | Generates weighted/unweighted similarity networks. | `igraph` |

---

## How to Use

### 1. Prerequisites
Install the required R packages before running the scripts:
```R
install.packages(c("reutils", "rentrez", "igraph"))
