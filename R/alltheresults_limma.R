#' Extract and Annotate Differential Expression Results from Limma Analysis
#'
#' This function extracts and annotates differential expression results from a fitted Limma model, 
#' and generates interactive tables with gene information and links.
#'
#' @param resuSet A list to store the results.
#' @param fitted_limma_model The fitted Limma model object.
#' @param contrast A character vector specifying the contrast of interest.
#' @param FDR A numeric value specifying the false discovery rate threshold.
#' @param anns A data frame containing annotation information with columns for gene symbols, Ensembl gene IDs, and UniProt IDs.
#' @param species A character string specifying the species (default is "mus_musculus").
#'
#' @return A list containing the extracted and annotated results, including interactive tables.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Extracts the fitted model and results table.
#'   \item Annotates the results table with gene information.
#'   \item Filters the results table based on the specified FDR threshold.
#'   \item Generates interactive tables with links to external databases for gene symbols, Ensembl gene IDs, and UniProt IDs.
#'   \item Reorders columns and rounds numerical values for better readability.
#' }
#'
#' @examples
#' \dontrun{
#' resuSet <- list()
#' fitted_limma_model <- lmFit(...)
#' contrast <- c("condition1", "condition2")
#' FDR <- 0.05
#' anns <- data.frame(...)
#' results <- alltheresults_limma(resuSet, fitted_limma_model, contrast, FDR, anns)
#' }
#'
#' @export
alltheresults_limma <- function(resuSet, fitted_limma_model, contrast, FDR, anns, species = "Mus_musculus") {

    # id_contrast <- paste0(contrast[2],"_vs_",contrast[3])
    id_contrast <- contrast
    resuSet[[id_contrast]] <- list()

    mycoef <- contrast

    message("Extracting results...")
    resuSet[[id_contrast]][["fitted_model"]] <- fitted_limma_model

    message("Extracting tables...")
    resuSet[[id_contrast]][["tbl_res_all"]] <- add_gene_info(topTable(fit2, coef=contrast, adjust="fdr", number=Inf, confint = FALSE), anns)

    message("Extracting DEtables...")
    resuSet[[id_contrast]][["tbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_all"]][resuSet[[id_contrast]][["tbl_res_all"]]$adj.P.Val < FDR, ]
    
    message("Extracting DEtables ensembl_gene_id...")
    resuSet[[id_contrast]][["tbl_res_DE_genes"]] <- results_to_gene_id(resuSet[[id_contrast]][["tbl_res_DE"]], anns)
    
    if(nrow(resuSet[[id_contrast]][["tbl_res_DE"]]) > 0) {
      message("Generating interactive DEtable...")
      resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["tbl_res_DE"]]
      resuSet[[id_contrast]][["etbl_res_DE"]]$ensembl_gene_id <- createLinkENS(resuSet[[id_contrast]][["etbl_res_DE"]]$ensembl_gene_id, species = species)
      resuSet[[id_contrast]][["etbl_res_DE"]]$gene_symbol <- createLinkGeneSymbol(resuSet[[id_contrast]][["etbl_res_DE"]]$gene_symbol)
      resuSet[[id_contrast]][["etbl_res_DE"]]$uniprot_id <- createLinkUNIPROT(resuSet[[id_contrast]][["etbl_res_DE"]]$uniprot_id)
      
      # Move gene_symbol and uniprot_id as first columns
      message("Reordering columns...")
      resuSet[[id_contrast]][["etbl_res_DE"]] <- resuSet[[id_contrast]][["etbl_res_DE"]][, c("gene_symbol", "uniprot_id", setdiff(names(resuSet[[id_contrast]][["etbl_res_DE"]]), c("gene_symbol", "uniprot_id")))]
      
      # Round numerical columns to 3 decimal points
      num_cols <- sapply(resuSet[[id_contrast]][["etbl_res_DE"]], is.numeric)
      resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols] <- lapply(resuSet[[id_contrast]][["etbl_res_DE"]][, num_cols], round, 3)
    }

    mybuttons <- c('copy', 'csv', 'excel', 'pdf', 'print')
    DT::datatable(resuSet[[id_contrast]][["etbl_res_DE"]],caption = paste0(id_contrast,", DE genes"), escape=F, extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = mybuttons))

    return(resuSet)
}

# functions to make results nicer
createLinkGO <- function(val) {
  sprintf('<a href="http://amigo.geneontology.org/amigo/term/%s" target="_blank" class="btn btn-primary">%s</a>',val,val)
}

createLinkENS  <- function(val, species="Mus_musculus") {
  paste0('<a href="http://www.ensembl.org/',species,'/Gene/Summary?g=',val,'" target="_blank" class="btn btn-primary">',val,'</a>')
}
createLinkUNIPROT  <- function(val) {
  sprintf('<a href="https://www.uniprot.org/uniprotkb/%s/entry" target="_blank" class="btn btn-primary"">%s</a>',val,val)
}

createLinkGeneSymbol <- function(val) {
  # possibilities:
  # ncbi
  # genecards
  paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',val,'[sym]" target="_blank" class="btn btn-primary">',val,'</a>')
}

# add_gene_info <- function(results_table, anns) {

#     if (!"uniprot_id" %in% colnames(results_table)) {
#         results_table$uniprot_id <- rownames(results_table)
#     }  
    
#     results_table$gene_symbol <- anns[match(results_table$uniprot_id, anns$unprot_id), "gene_symbol"]
#     results_table$ensembl_gene_id <- anns[match(results_table$uniprot_id, anns$unprot_id), "ensembl_gene_id"]
#     results_table$uniprot_id <- anns[match(results_table$uniprot_id, anns$unprot_id), "unprot_id"]

#     return(results_table)
# }

