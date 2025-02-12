# List of required packages
required_packages <- c(
  "SummarizedExperiment",  # Bioconductor
  "ggplot2",               # CRAN
  "limma",                 # Bioconductor
  "pcaExplorer",           # Bioconductor
  "DESeq2",                # Bioconductor
  "GeneTonic",             # Bioconductor
  "DT",                    # CRAN
  "org.Mm.eg.db",          # Bioconductor
  "biomaRt",               # Bioconductor
  "clusterProfiler",       # Bioconductor
  "topGO",                 # Bioconductor
  "plotly",                 # CRAN
  "tximeta",
  "SummarizedExperiment"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  # Check if BiocManager is installed
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  
  for (pkg in packages) {

    # Check if installed
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing ", pkg, "...")
      
      # Try CRAN first
      tryCatch(
        {
          # Check if the package is available on CRAN for the current R version
          cran_status <- tryCatch(
            install.packages(pkg), 
            warning = function(w) {
              message("Warning: ", pkg, " is not available on CRAN for your R version. Trying Bioconductor")
              # Try to install the package from Bioconductor instead
              tryCatch(
                {
                  BiocManager::install(pkg)
                  message(pkg, " installed from Bioconductor.")
                },
                error = function(e) {
                  # If both CRAN and Bioconductor fail, suggest GitHub or other alternatives
                  message("Failed to install ", pkg, ". Please check package availability or try GitHub.")
                }
              )       
            }, 
            error = function(e) {
              message("Package", pkg, "not installed")
            }
          )
        }
      )
    } else {
      message(pkg, " is already installed.")
    }
  }
}
