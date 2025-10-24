#' Differential Expression Analysis Table Formatter for Transcriptomics Reports
#' Formats and annotates a differential expression analysis (DEA) table for metabolomics reports.
#'
#' This function processes a DEA results table, applies an alpha threshold to filter significant features,
#' adds a specified identifier column (e.g., KEGG), and optionally converts gene/protein identifiers to interactive links.
#' Numeric columns are rounded for improved readability in reports.
#'
#' @param dea_table A data.frame containing the results of differential expression analysis.
#' @param rownames Character. The name of the column to use for row identifiers (e.g., "KEGG"). Default is "KEGG".
#' @param interactive Logical. If TRUE, gene/protein identifiers are converted to interactive links. Default is TRUE.
#' @param alpha Numeric. The significance threshold for adjusted p-values (padj). Must be between 0 and 1. Default is 1.
#' @param pvalue_column Character. The name of the column containing adjusted p-values. Default is "padj".
#' @param species Character. Species code for ENSEMBL links (e.g., "Mm" for mouse). Default is "Mm".
#'
#' @return A data.frame with formatted and annotated DEA results, or NULL if no features pass the alpha threshold.
#'
#' @details
#' The function removes rows with missing values, filters features based on the specified alpha threshold,
#' and adds a column with the specified row identifier. If \code{interactive = TRUE}, supported identifier columns
#' (ENSEMBL, SYMBOL, UNIPROT) are converted to interactive links using helper functions. Numeric columns are rounded to 4 decimal places.
#'
#' @export
dea_for_report_metabolomics <- function(dea_table, rownames = "KEGG", interactive = TRUE, alpha = 1, pvalue_column = "padj", species = "Mm") {
  if (alpha <= 1 && alpha >= 0) {
    message("alpha threshold is valid.")
  } else {
    stop("alpha threshold must be between 0 and 1.")
  }

  # if (format == "deseq2") {
  #   required_cols <- c("id", "baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj")
  #   if (!all(required_cols %in% colnames(dea_table))) {
  #     stop("The dea table must be the output of dea() from DeeDeeExperiment in the original format (par format = 'original')")
  #   }
  # }

  if (inherits(dea_table, "data.frame")) {
    if (nrow(dea_table) == 0) {
      message("No features found.")
      return(NULL)
    }
  }
  message("Extracting tables...")
  dea_table <- dea_table[rowSums(is.na(dea_table)) == 0, ]
  if (alpha < 1) dea_table <- dea_table[dea_table[[pvalue_column]] < alpha, ]
  # dea_table$ENSEMBL <- gsub("\\.[0-9]*$", "", rownames(dea_table))
  dea_table[[rownames]] <- rownames(dea_table)
  # dea_table$SYMBOL <- anns$SYMBOL[match(dea_table$ENSEMBL, anns$ENSEMBL)]

  if (nrow(dea_table) == 0) {
    warning("No differentially expressed genes found after applying the alpha threshold.")
    return(NULL)
  }

  if (interactive) {
    if ("ENSEMBL" %in% colnames(dea_table)) {
      dea_table$ENSEMBL <- createLinkENS(dea_table$ENSEMBL, species = species)
    }
    if ("SYMBOL" %in% colnames(dea_table)) {
      dea_table$SYMBOL <- createLinkGeneSymbol(dea_table$SYMBOL)
    }
    if ("UNIPROT" %in% colnames(dea_table)) {
      dea_table$UNIPROT <- createLinkUNIPROT(dea_table$UNIPROT)
    }
    if ("KEGG" %in% colnames(dea_table)) {
      # dea_table$KEGG <- createLinkKEGG(dea_table$KEGG)
    }

    num_cols <- sapply(dea_table, is.numeric)
    dea_table[, num_cols] <- lapply(dea_table[, num_cols], round, 4)
  }

  return(dea_table)
}

