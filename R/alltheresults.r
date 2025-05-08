# sections present in myResuSet
#  - res_DESeq -> DESeq2 results object
#  - maplot_res -> MA plot of DESeq2 results
#  - tbl_res_all -> Data frame of all DESeq2 results
#  - tbl_res_DE -> Data frame of significant DESeq2 results
#  - etbl_res_DE -> Interactive table of significant DESeq2 results
#  - clupro_tbl -> ClusterProfiler results table (not implemented in this script)
#  - topGO_tbl -> topGO results table (not implemented in this script)
#' Extract and process DESeq2 results
#'
#' This function extracts and processes DESeq2 results for a given contrast,
#' performs LFC shrinkage, and generates various result tables and plots.
#'
#' @param resuSet A list to store the results.
#' @param dds_obj A DESeqDataSet object.
#' @param contrast A character vector specifying the contrast.
#' @param FDR A numeric value specifying the false discovery rate threshold.
#' @param anns A data frame containing gene annotations with columns `gene_id` and `gene_name`.
#' @param anns A data frame containing additional annotations with columns `ensembl_gene_id`, `description`, and `chromosome_name`.
#' @param species A character string specifying the species for creating links.
#'
#' @return A list containing the processed results, including DESeq2 results,
#'         LFC shrinkage results, result tables, and interactive tables.
#'
#' @examples
#' \dontrun{
#' resuSet <- list()
#' dds_obj <- DESeqDataSet(...)
#' contrast <- c("condition", "treated", "control")
#' FDR <- 0.05
#' anns <- data.frame(gene_id = ..., gene_name = ...)
#' anns <- data.frame(ensembl_gene_id = ..., description = ..., chromosome_name = ...)
#' species <- "Homo sapiens"
#' alltheresults(resuSet, dds_obj, contrast, FDR, anns, anns, species)
#' }
#' @export
alltheresults <- function(resuSet, dds_obj, contrast, FDR, anns, species) {
  # id_contrast <- paste0(contrast[2],"_vs_",contrast[3])
  id_contrast <- contrast
  resuSet[[id_contrast]] <- list()

  # mycoef <- resultsNames(dds_obj)[2]
  mycoef <- contrast

  message("Extracting results...")
  resuSet[[id_contrast]][["res_DESeq"]] <- results(dds_obj, name = mycoef, alpha = FDR)
  message("Performing LFC shrinkage...")
  resuSet[[id_contrast]][["res_DESeq"]] <- lfcShrink(dds_obj, coef = mycoef, res = resuSet[[id_contrast]][["res_DESeq"]], type = "apeglm")
  resuSet[[id_contrast]][["res_DESeq"]]$gene_name <- anns$gene_name[match(rownames(resuSet[[id_contrast]][["res_DESeq"]]), anns$gene_id)]

  message("Summary MAplot...")
  summary(resuSet[[id_contrast]][["res_DESeq"]])
  # resuSet[[id_contrast]][["maplot_res"]] <-
  #   ideal::plot_ma(resuSet[[id_contrast]][["res_DESeq"]], ylim = c(-2,2), title = id_contrast, FDR = FDR) # commented because it makes my computer crash....
  resuSet[[id_contrast]][["maplot_res"]] <- NULL
  # plotMA(resuSet[[id_contrast]][["res_DESeq"]], ylim = c(-2, 2), main = id_contrast, alpha = FDR)

  message("Extracting tables...")
  resuSet[[id_contrast]][["tbl_res_all"]] <- deseqresult2df(resuSet[[id_contrast]][["res_DESeq"]])
  resuSet[[id_contrast]][["tbl_res_all"]]$ensembl_gene_id <- gsub("\\.[0-9]*$", "", resuSet[[id_contrast]][["tbl_res_all"]]$id)
  resuSet[[id_contrast]][["tbl_res_all"]]$gene_name <- anns$gene_name[match(resuSet[[id_contrast]][["tbl_res_all"]]$ensembl_gene_id, anns$gene_id)]
  resuSet[[id_contrast]][["tbl_res_all"]]$description <- anns$description[match(resuSet[[id_contrast]][["tbl_res_all"]]$ensembl_gene_id, anns$ensembl_gene_id)]

  message("Extracting DEtables...")
  resuSet[[id_contrast]][["tbl_res_DE"]] <- deseqresult2df(resuSet[[id_contrast]][["res_DESeq"]], FDR = FDR)
  resuSet[[id_contrast]][["tbl_res_DE"]]$ensembl_gene_id <- gsub("\\.[0-9]*$", "", resuSet[[id_contrast]][["tbl_res_DE"]]$id)
  resuSet[[id_contrast]][["tbl_res_DE"]]$gene_name <- anns$gene_name[match(resuSet[[id_contrast]][["tbl_res_DE"]]$ensembl_gene_id, anns$gene_id)]
  resuSet[[id_contrast]][["tbl_res_DE"]]$description <- anns$description[match(resuSet[[id_contrast]][["tbl_res_DE"]]$ensembl_gene_id, anns$ensembl_gene_id)]
  # resuSet[[id_contrast]][["tbl_res_DE"]]$chromosome_name <- anns$chromosome_name[match(resuSet[[id_contrast]][["tbl_res_DE"]]$ensembl_gene_id, anns$ensembl_gene_id)]

  if (nrow(resuSet[[id_contrast]][["tbl_res_DE"]]) > 0) {
    message("Generating interactive DEtable...")
    resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_DE"]]
    resuSet[[id_contrast]][["etbl_res_DE"]]$ensembl_gene_id <- createLinkENS(resuSet[[id_contrast]][["etbl_res_DE"]]$ensembl_gene_id, species = species)
    resuSet[[id_contrast]][["etbl_res_DE"]]$gene_name <- createLinkGeneSymbol(resuSet[[id_contrast]][["etbl_res_DE"]]$gene_name)

    # Round numerical columns to 3 decimal points
    num_cols <- sapply(resuSet[[id_contrast]][["etbl_res_DE"]], is.numeric)
    resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols] <- lapply(resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols], round, 3)
  }

  mybuttons <- c("copy", "csv", "excel", "pdf", "print")
  datatable(resuSet[[id_contrast]][["etbl_res_DE"]], caption = paste0(id_contrast, ", DE genes"), escape = F, extensions = "Buttons", options = list(dom = "Bfrtip", buttons = mybuttons))

  return(resuSet)
}

# functions to make results nicer
createLinkGO <- function(val) {
  sprintf('<a href="http://amigo.geneontology.org/amigo/term/%s" target="_blank" class="btn btn-primary">%s</a>', val, val)
}

createLinkENS <- function(val, species = "Mus_musculus") {
  paste0('<a href="http://www.ensembl.org/', species, "/Gene/Summary?g=", val, '" target="_blank" class="btn btn-primary">', val, "</a>")
}

createLinkGeneSymbol <- function(val) {
  # possibilities:
  # ncbi
  # genecards
  paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=', val, '[sym]" target="_blank" class="btn btn-primary">', val, "</a>")
}
