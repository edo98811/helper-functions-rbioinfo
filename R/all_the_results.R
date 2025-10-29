#' Process and extract all results for a given contrast from DESeq2 or limma analysis
#'
#' This function extracts and processes DESeq2 results for a given contrast,
#' performs LFC shrinkage, and generates various result tables and plots.
#'
#' @param object A DESeqDataSet object or MArrayLM.
#' @param resuSet A list to which the results will be added.
#' @param contrast A character vector specifying the contrast.
#' @param FDR A numeric value specifying the false discovery rate threshold.
#' @param anns A data frame containing gene annotations (generated with create_annotations)
#' @param species A character string specifying the species for creating links.
#'
#' @return A list containing the processed results, including DESeq2 results,
#'         LFC shrinkage results, result tables, and interactive tables.
#' 
#' @details
#' # sections present in myResuSet
#'- res_DESeq / fitted_model -> DESeq2 results object / limma model
#'- tbl_res_all -> Data frame of all DESeq2 results
#'- tbl_res_DE -> Data frame of significant DESeq2 results
#'- etbl_res_DE -> Interactive table of significant DESeq2 results
#'- topGO_tbl -> topGO results table (not implemented in this script)
#'
#' @examples
#' \dontrun{
#' resuSet <- list()
#' dds_obj <- DESeqDataSet(...)
#' contrast <- c("condition", "treated", "control")
#' FDR <- 0.05
#' anns <- data.frame(ENSEMBL = ..., SYMBOL = ...)
#' anns <- data.frame(ENSEMBL = ..., description = ..., chromosome_name = ...)
#' species <- "Homo sapiens"
#' alltheresults(resuSet, dds_obj, contrast, FDR, anns, anns, species)
#' }
#' @export
alltheresults <- function(object, resuSet, contrast, FDR, anns, species) {
  if (inherits(object, "DESeqDataSet")) {
    message("Processing DESeq2 object...")
    # Call DESeq2-specific analysis function
    return(alltheresults_dds(object, resuSet, contrast, FDR, anns, species))
  } else if (inherits(object, "MArrayLM")) {
    message("Processing limma object...")
    # Call limma-specific analysis function
    return(alltheresults_limma(object, resuSet, contrast, FDR, anns, species))
  } else {
    stop("Unsupported object type: must be DESeqDataSet or MArrayLM")
  }
}

alltheresults_dds <- function(dds_obj, resuSet, contrast, FDR, anns, species) {
  # id_contrast <- paste0(contrast[2],"_vs_",contrast[3])
  id_contrast <- contrast
  resuSet[[id_contrast]] <- list()

  # mycoef <- resultsNames(dds_obj)[2]
  mycoef <- contrast

  message("Extracting results...")
  resuSet[[id_contrast]][["res_DESeq"]] <- results(dds_obj, name = mycoef, alpha = FDR)
  message("Performing LFC shrinkage...")
  resuSet[[id_contrast]][["res_DESeq"]] <- lfcShrink(dds_obj, coef = mycoef, res = resuSet[[id_contrast]][["res_DESeq"]], type = "apeglm")
  resuSet[[id_contrast]][["res_DESeq"]]$SYMBOL <- anns$SYMBOL[match(rownames(resuSet[[id_contrast]][["res_DESeq"]]), anns$ENSEMBL)]

  message("Summary MAplot...")
  summary(resuSet[[id_contrast]][["res_DESeq"]])
  # resuSet[[id_contrast]][["maplot_res"]] <-
  #   ideal::plot_ma(resuSet[[id_contrast]][["res_DESeq"]], ylim = c(-2,2), title = id_contrast, FDR = FDR) # commented because it makes my computer crash....
  resuSet[[id_contrast]][["maplot_res"]] <- NULL
  # plotMA(resuSet[[id_contrast]][["res_DESeq"]], ylim = c(-2, 2), main = id_contrast, alpha = FDR)

  message("Extracting tables...")
  resuSet[[id_contrast]][["tbl_res_all"]] <- deseqresult2df(resuSet[[id_contrast]][["res_DESeq"]])
  resuSet[[id_contrast]][["tbl_res_all"]]$ENSEMBL <- gsub("\\.[0-9]*$", "", resuSet[[id_contrast]][["tbl_res_all"]]$id)
  resuSet[[id_contrast]][["tbl_res_all"]]$SYMBOL <- anns$SYMBOL[match(resuSet[[id_contrast]][["tbl_res_all"]]$ENSEMBL, anns$ENSEMBL)]
  # resuSet[[id_contrast]][["tbl_res_all"]]$description <- anns$description[match(resuSet[[id_contrast]][["tbl_res_all"]]$ENSEMBL, anns$ENSEMBL)]

  message("Extracting DEtables...")
  resuSet[[id_contrast]][["tbl_res_DE"]] <- deseqresult2df(resuSet[[id_contrast]][["res_DESeq"]], FDR = FDR)
  resuSet[[id_contrast]][["tbl_res_DE"]]$ENSEMBL <- gsub("\\.[0-9]*$", "", resuSet[[id_contrast]][["tbl_res_DE"]]$id)
  resuSet[[id_contrast]][["tbl_res_DE"]]$SYMBOL <- anns$SYMBOL[match(resuSet[[id_contrast]][["tbl_res_DE"]]$ENSEMBL, anns$ENSEMBL)]
  # resuSet[[id_contrast]][["tbl_res_DE"]]$description <- anns$description[match(resuSet[[id_contrast]][["tbl_res_DE"]]$ENSEMBL, anns$ENSEMBL)]
  # resuSet[[id_contrast]][["tbl_res_DE"]]$chromosome_name <- anns$chromosome_name[match(resuSet[[id_contrast]][["tbl_res_DE"]]$ENSEMBL, anns$ENSEMBL)]

  if (nrow(resuSet[[id_contrast]][["tbl_res_DE"]]) > 0) {
    message("Generating interactive DEtable...")
    resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_DE"]]
    resuSet[[id_contrast]][["etbl_res_DE"]]$ENSEMBL <- createLinkENS(resuSet[[id_contrast]][["etbl_res_DE"]]$ENSEMBL, species = species)
    resuSet[[id_contrast]][["etbl_res_DE"]]$SYMBOL <- createLinkGeneSymbol(resuSet[[id_contrast]][["etbl_res_DE"]]$SYMBOL)

    # Round numerical columns to 3 decimal points
    num_cols <- sapply(resuSet[[id_contrast]][["etbl_res_DE"]], is.numeric)
    resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols] <- lapply(resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols], round, 4)
  }

  mybuttons <- c("copy", "csv", "excel", "pdf", "print")
  datatable(resuSet[[id_contrast]][["etbl_res_DE"]], caption = paste0(id_contrast, ", DE genes"), escape = F, extensions = "Buttons", options = list(dom = "Bfrtip", buttons = mybuttons))

  return(resuSet)
}

alltheresults_limma <- function(fitted_limma_model, resuSet, contrast, FDR, anns, species = "Mus_musculus") {
  # id_contrast <- paste0(contrast[2],"_vs_",contrast[3])
  id_contrast <- contrast
  resuSet[[id_contrast]] <- list()

  mycoef <- contrast

  message("Extracting results...")
  resuSet[[id_contrast]][["fitted_model"]] <- fitted_limma_model

  message("Extracting tables...")
  resuSet[[id_contrast]][["tbl_res_all"]] <- add_gene_info(topTable(fitted_limma_model, coef = contrast, adjust = "fdr", number = Inf, confint = FALSE), anns)

  message("Extracting DEtables...")
  resuSet[[id_contrast]][["tbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_all"]][resuSet[[id_contrast]][["tbl_res_all"]]$adj.P.Val < FDR, ]

  message("Extracting DEtables ENSEMBL...")
  resuSet[[id_contrast]][["tbl_res_DE_genes"]] <- results_to_ENSEMBL(resuSet[[id_contrast]][["tbl_res_DE"]], anns)

  if (nrow(resuSet[[id_contrast]][["tbl_res_DE"]]) > 0) {
    message("Generating interactive DEtable...")
    resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_DE"]]
    resuSet[[id_contrast]][["etbl_res_DE"]]$ENSEMBL <- createLinkENS(resuSet[[id_contrast]][["etbl_res_DE"]]$ENSEMBL, species = species)
    resuSet[[id_contrast]][["etbl_res_DE"]]$SYMBOL <- createLinkGeneSymbol(resuSet[[id_contrast]][["etbl_res_DE"]]$SYMBOL)
    resuSet[[id_contrast]][["etbl_res_DE"]]$UNIPROT <- createLinkUNIPROT(resuSet[[id_contrast]][["etbl_res_DE"]]$UNIPROT)

    # Move SYMBOL and UNIPROT as first columns
    message("Reordering columns...")
    resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["etbl_res_DE"]][, c("SYMBOL", "UNIPROT", setdiff(names(resuSet[[id_contrast]][["etbl_res_DE"]]), c("SYMBOL", "UNIPROT")))]

    # Round numerical columns to 3 decimal points
    num_cols <- sapply(resuSet[[id_contrast]][["etbl_res_DE"]], is.numeric)
    resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols] <- lapply(resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols], round, 4)
  }

  mybuttons <- c("copy", "csv", "excel", "pdf", "print")
  DT::datatable(resuSet[[id_contrast]][["etbl_res_DE"]], caption = paste0(id_contrast, ", DE genes"), escape = F, extensions = "Buttons", options = list(dom = "Bfrtip", buttons = mybuttons))

  return(resuSet)
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
