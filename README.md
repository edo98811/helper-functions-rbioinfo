
# helper-functions-rbioinfo

**An R package designed to help with RNA-seq and bioinformatics analyses by simplifying data handling and report generation.**

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

### Key Parameters

The following parameters are used throughout the workflow and passed as a list to functions:

* `run_computations` (logical): Whether to run the computations or just load precomputed results.
* `analysis_name` (string): Name for the current analysis.
* `source_object` (string): Path to an RDS file containing the starting object.
* `species` (string): Species identifier (`"hs"` for human, `"mm"` for mouse).
* `metadata_file` (string): Path to metadata in XLSX or CSV format.
* `save_results` (logical): Whether to save results after computation.