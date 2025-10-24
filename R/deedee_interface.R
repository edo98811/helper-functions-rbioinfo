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
dea_for_report <- function(dea_table, anns, rownames_col = "ENSEMBL", interactive = TRUE, alpha = 0.05, pvalue_column = "padj", species = "Mm") {
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
  } else {
    stop("Input dea_table is not a data.frame.")
  }
  message("Extracting tables...")
  dea_table <- dea_table[rowSums(is.na(dea_table)) == 0, ]
  if (alpha < 1) dea_table <- dea_table[dea_table[[pvalue_column]] < alpha, ]
  dea_table[[rownames_col]] <- rownames(dea_table)

  # Stop if no table rows remain after filtering
  if (nrow(dea_table) == 0) {
    warning("No differentially expressed genes found after applying the alpha threshold.")
    return(NULL)
  }

  # Add annotations
  for (col_name in colnames(anns)) {
    if (col_name != rownames_col) { # skip rownames column
      dea_table[[col_name]] <- anns[[col_name]][match(dea_table[[rownames_col]], anns[[rownames_col]])]
    }
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
      dea_table$KEGG <- createLinkKEGG(dea_table$KEGG)
    }

    num_cols <- sapply(dea_table, is.numeric)
    dea_table[, num_cols] <- lapply(dea_table[, num_cols], round, 4)
  }

  return(dea_table)
}

#' Display an interactive table with a custom caption
#'
#' This function shows a data frame as an interactive table using the DT package.
#' If the table is empty, a message is printed instead. When knitting a report,
#' the table is formatted for inclusion in the output document.
#'
#' @param table A data frame to display.
#' @param title A character string for the table caption.
#' @param knitting Logical; if \code{TRUE}, formats the table for knitting (e.g., RMarkdown).
#'
#' @return Displays the interactive table or prints a message if the table is empty.
#'
#' @export
show_interactive_table <- function(table, title, knitting = FALSE) {
  # Check if the table is empty
  if (is.null(table)) {
    cat("The input table is NULL. Maybe there are no identified de?")
    return(
      htmltools::div(
        style = "color: red; font-weight: bold; margin: 10px 0;",
        "The input table is NULL. Maybe there are no identified de?"
      )
    )
  } else if (!inherits(table, "data.frame")) {
    stop("The input must be a data frame.")
  } else if (nrow(table) == 0) {
    return(
      htmltools::div(
        style = "color: red; font-weight: bold; margin: 10px 0;",
        paste0("The table is empty. No results to display for ", title, ".")
      )
    )
  } else {
    # Create the datatable with scrollX
    dt <- DT::datatable(
      table,
      escape = FALSE,
      rownames = FALSE,
      options = list(scrollX = TRUE), # enables horizontal scroll in DT
      caption = htmltools::tags$caption(
        style = "caption-side: top; color:black; font-size: 2em; text-align: left;",
        title
      )
    )

    # Wrap the datatable in a scrollable div
    scrollable_div <- htmltools::div(
      style = "overflow-x: auto; width: 100%;",
      dt
    )

    # Output based on context
    if (knitting) {
      tryCatch(
        {
          cat(knitr::knit_print(scrollable_div))
        },
        error = function(e) {
          message("Error in knitting the table. Are you creating a report or running the code in RStudio? If you're in RStudio, set knitting = FALSE. Error: ", e$message)
        }
      )
    } else {
      print(dt)
    }
  }
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
fea_for_report <- function(fea_table, interactive = TRUE, alpha = 0.05, pvalue_column = "p.value_elim") {
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
  if (alpha < 1) fea_table <- fea_table[fea_table[[pvalue_column]] < alpha, ]
  if (nrow(fea_table) == 0) {
    warning("No rows remaining after applying the alpha threshold.")
    return(NULL)
  }

  if (interactive) {
    num_cols <- sapply(fea_table, is.numeric)
    fea_table[, num_cols] <- lapply(fea_table[, num_cols], round, 4)
  }

  return(fea_table)
}


# functions to make results nicer
createLinkGO <- function(val) {
  sprintf('<a href="http://amigo.geneontology.org/amigo/term/%s" target="_blank" class="btn btn-primary">%s</a>', val, val)
}

createLinkENS <- function(val, species) {
  paste0('<a href="http://www.ensembl.org/', species, "/Gene/Summary?g=", val, '" target="_blank" class="btn btn-primary">', val, "</a>")
}

createLinkGeneSymbol <- function(val) {
  # possibilities:
  # ncbi
  # genecards
  paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=', val, '[sym]" target="_blank" class="btn btn-primary">', val, "</a>")
}

createLinkUNIPROT <- function(val) {
  sprintf('<a href="https://www.uniprot.org/uniprotkb/%s/entry" target="_blank" class="btn btn-primary"">%s</a>', val, val)
}

createLinkKEGG <- function(val) {
  val
}

#' get_fea_list but does not fail when empty
#'
#' This is a wrapper to the get_fea_list function
#'
#' @param dde A DeeDeeExperiment object
#'
#' @return A list of the available tables in the fea slot
#'
#' @export
get_fea_list_report <- function(dde, ...) {
  tryCatch(
    {
      return(getFEAList(dde, ...))
    },
    error = {
      warning("No results found in FEA slot.")
      return(NULL)
    }
  )
}

#' get_dea_list but does not fail when empty
#'
#' This is a wrapper to the get_dea_list function
#'
#' @param dde A DeeDeeExperiment object
#'
#' @return A list of the available tables in the fea slot
#'
#' @export
get_dea_list_report <- function(dde, ...) {
  tryCatch(
    {
      return(getDEAList(dde, ...))
    },
    error = {
      warning("No results found in DEA slot.")
      return(NULL)
    }
  )
}
