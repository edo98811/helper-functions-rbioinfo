
# helper-functions-rbioinfo

**An R package designed to help with RNA-seq and bioinformatics analyses by simplifying data handling and report generation.**

---

## Introduction

`helper-functions-rbioinfo` is an R package created to facilitate the creation of reproducible reports from RNA-seq or bioinformatics analyses. It helps manage data preprocessing, differential expression analysis results, and enrichment outputs, allowing you to focus on interpretation and report generation without worrying about data management or inconsistencies.

---

## Installation

You can install the package directly from GitHub using the `remotes` package:

```r
# Install remotes if you don't have it
install.packages("remotes")

# Install helper-functions-rbioinfo from GitHub
remotes::install_github("edo98811/helper-functions-rbioinfo")
```

---

## Usage Overview

The package workflow is centered around a **SummarizedExperiment (SE)** object and includes the following main steps:

### Key Parameters

The following parameters are used throughout the workflow and passed as a list to several main functions:

* `run_computations` (logical): Whether to run the computations or just load precomputed results.
* `analysis_name` (string): Name for the current analysis.
* `source_se` (string): Path to an RDS file containing the starting SummarizedExperiment object.
* `subset_object` (logical): Whether to subset the data object.
* `species` (string): Species identifier (`"hs"` for human, `"mm"` for mouse).
* `metadata_file` (string): Path to metadata in XLSX or CSV format.
* `workflow` (string): Choose from `"se_vdx"`, `"se_dds"`, `"se"`, or `"dds"`.
* `save_results` (logical): Whether to save results after computation.

These parameters are passed as the `params` argument to the following key functions in the package:

* `prepare_workspace`
* `load_results`
* `create_annotations`
* `save_results`
