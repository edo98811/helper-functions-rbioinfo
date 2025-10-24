#' Differential Expression Analysis Table Formatter for Transcriptomics Reports
#'
#' Formats and annotates a differential expression analysis (DEA) table for transcriptomics reports.
#' Adds gene symbols and ENSEMBL IDs, applies an alpha threshold, and optionally creates interactive links for gene identifiers.
#'
#' @param dea_table A data.frame containing the results of differential expression analysis.
#' @param anns A data.frame with annotation information, including ENSEMBL and SYMBOL columns.
#' @param interactive Logical. If TRUE, gene identifiers are converted to interactive links. Default is TRUE.
#' @param alpha Numeric. The significance threshold for adjusted p-values (padj). Must be between 0 and 1. Default is 1.
#' @param pvalue_column Character. The name of the column containing adjusted p-values. Default is "padj".
#' @param species Character. Species code for ENSEMBL links (e.g., "Mm" for mouse). Default is "Mm".
#'
#' @return A data.frame with formatted and annotated DEA results, or NULL if no features pass the alpha threshold.
#'
#' @export
dea_for_report_transcriptomics <- function(dea_table, anns, rownames = "ENSEMBL", interactive = TRUE, alpha = 1, pvalue_column = "padj", species = "Mm") {
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
  dea_table$ENSEMBL <- gsub("\\.[0-9]*$", "", rownames(dea_table))
  dea_table$SYMBOL <- anns$SYMBOL[match(dea_table$ENSEMBL, anns$ENSEMBL)]

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

    num_cols <- sapply(dea_table, is.numeric)
    dea_table[, num_cols] <- lapply(dea_table[, num_cols], round, 4)
  }

  return(dea_table)
}



#' Filter and Format Differential Expression Analysis Table for Reporting
#'
#' This function filters a differential expression analysis (DEA) table based on an alpha threshold,
#' optionally rounds numeric columns for interactive reporting, and returns the filtered table.
#'
#' @param fea_table A data.frame containing differential expression results. Must include a column named \code{padj}.
#' @param anns Annotations associated with the features (not used in current implementation).
#' @param interactive Logical. If \code{TRUE}, numeric columns are rounded to 4 decimal places for reporting.
#' @param alpha Numeric threshold between 0 and 1 for adjusted p-value (\code{padj}) filtering.
#' @param value_column Name of the column containing values of interest (not used in current implementation).
#'
#' @return A filtered and optionally formatted \code{data.frame} of differentially expressed features, or \code{NULL} if no features pass the threshold.
#'
#' @export
fea_for_report_transcriptomics <- function(fea_table, interactive = TRUE, alpha = 0.05, pvalue_column = "p.value_elim") {
  if (alpha <= 1 && alpha >= 0) {
    message("alpha threshold is valid.")
  } else {
    stop("alpha threshold must be between 0 and 1.")
  }
  if (is.null(fea_table)) {
    return(NULL)
  }
  if (inherits(fea_table, "data.frame")) {
    if (nrow(fea_table) == 0) {
      message("No features found.")
      return(NULL)
    }
  }

  # if (!is.null(anns) && !inherits(anns, "data.frame")) {
  #   stop("anns must be a data.frame or NULL.")
  # }

  message("Extracting tables...")
  fea_table <- fea_table[rowSums(is.na(fea_table)) == 0, ]

  if (nrow(fea_table) == 0) {
    warning("No differentially expressed genes found after applying the alpha threshold.")
    return(NULL)
  }

  if (alpha < 1) fea_table <- fea_table[fea_table[[pvalue_column]] < alpha, ]

  if (interactive) {
    num_cols <- sapply(fea_table, is.numeric)
    fea_table[, num_cols] <- lapply(fea_table[, num_cols], round, 4)
  }

  return(fea_table)
}
