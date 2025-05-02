#' Convert Results Table to Gene ID
#'
#' This function converts the row names of a results table to Ensembl gene IDs.
#' If the `ensembl_gene_id` column is not present in the results table, it will be added using the `anns` annotation dataframe.
#'
#' @param results_table A data frame containing the results table. It should have a column named `uniprot_id`.
#' @param anns A data frame containing annotation information, which will be used to add gene information if necessary.
#'
#' @return A data frame with Ensembl gene IDs as row names and the `ensembl_gene_id` column removed.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Checks if the `ensembl_gene_id` column is present in the results table. If not, it adds the gene information using `anns`.
#'   \item Reorders the columns to place `ensembl_gene_id` first.
#'   \item Issues warnings if there are NA or duplicated gene names.
#'   \item Removes rows with NA or duplicated `ensembl_gene_id`.
#'   \item Sets the row names of the results table to `ensembl_gene_id` and removes the `ensembl_gene_id` column.
#' }
#'
#' @examples
#' \dontrun{
#' results_table <- data.frame(uniprot_id = c("P12345", "Q67890"), some_value = c(1.2, 3.4))
#' anns <- data.frame(uniprot_id = c("P12345", "Q67890"), ensembl_gene_id = c("ENSG000001", "ENSG000002"))
#' results_to_gene_id(results_table, anns)
#' }
#'
#' @export
results_to_gene_id <- function(results_table, anns) {
    if (!"ensembl_gene_id" %in% colnames(results_table)) {
        results_table <- add_gene_info(results_table, anns)
    }

    results_table <- results_table[, c("ensembl_gene_id", setdiff(names(results_table), "ensembl_gene_id"))]
    na_genes <- results_table$uniprot_id[is.na(results_table$ensembl_gene_id)]

    # warning if NA or duplicated gene names
    if (length(na_genes) > 0) {
        warning("There are NA gene names (will be deleted): ", paste(na_genes, collapse = ", "))
    }

    duplicated_genes <- results_table$uniprot_id[duplicated(results_table$ensembl_gene_id)]
    if (length(duplicated_genes) > 0) {
        warning("There are duplicated gene name (will be deleted): ", paste(duplicated_genes, collapse = ", "))
    }

    results_table <- results_table[!duplicated(results_table$ensembl_gene_id) & !is.na(results_table$ensembl_gene_id), ]
    rownames(results_table) <- results_table$ensembl_gene_id
    results_table <- results_table[, -which(names(results_table) == "ensembl_gene_id")]

    return(results_table)
}


#' Add Gene Information to Results Table
#'
#' This function adds gene symbol and Ensembl gene ID information to a results table based on UniProt IDs.
#'
#' @param results_table A data frame containing the results. It must have row names or a column named "uniprot_id".
#' @param anns A data frame containing annotation information. It must have columns "unprot_id", "gene_symbol", and "ensembl_gene_id".
#'
#' @return A data frame with added columns "gene_symbol" and "ensembl_gene_id".
#'
#' @examples
#' results_table <- data.frame(value = c(1.2, 3.4, 5.6), row.names = c("P12345", "Q67890", "A11111"))
#' anns <- data.frame(unprot_id = c("P12345", "Q67890", "A11111"), gene_symbol = c("GeneA", "GeneB", "GeneC"), ensembl_gene_id = c("ENSG000001", "ENSG000002", "ENSG000003"))
#' add_gene_info(results_table, anns)
#'
#' @export
add_gene_info <- function(results_table, anns) {

    if (!"uniprot_id" %in% colnames(results_table)) {
        results_table$uniprot_id <- rownames(results_table)
    }  
    
    results_table$gene_symbol <- anns[match(results_table$uniprot_id, anns$unprot_id), "gene_symbol"]
    results_table$ensembl_gene_id <- anns[match(results_table$uniprot_id, anns$unprot_id), "ensembl_gene_id"]

    return(results_table)
}


#' Convert Results Table Row Names to Protein IDs
#'
#' This function takes a results table and an annotation data frame, and replaces the row names of the results table with protein IDs from the annotation data frame.
#'
#' @param results_table A data frame containing the results table with row names that need to be converted to protein IDs.
#' @param anns A data frame containing the annotation information, where one of the columns is `gene_symbol` and the row names are protein IDs.
#'
#' @return A data frame with the same content as `results_table`, but with row names replaced by protein IDs from `anns`. The column `prot_ID` is removed from the final output.
#'
#' @examples
#' \dontrun{
#' results_table <- data.frame(matrix(ncol = 3, nrow = 3))
#' rownames(results_table) <- c("gene1", "gene2", "gene3")
#' anns <- data.frame(gene_symbol = c("gene1", "gene2", "gene3"), row.names = c("prot1", "prot2", "prot3"))
#' results_to_prot_id(results_table, anns)
#' }
#' @export
results_to_prot_id <- function(results_table, anns) {


    results_table$prot_ID <- rownames(anns[match(rownames(results_table), anns$gene_symbol), ]) # rownames are protID (magari da cambiare per interoperability)
    results_table <- results_table[, c("prot_ID", setdiff(names(results_table), "prot_ID"))]
    results_table <- results_table[!duplicated(results_table$prot_ID) & !is.na(results_table$prot_ID), ]
    rownames(results_table) <- results_table$prot_ID
    results_table <- results_table[, -which(names(results_table) == "prot_ID")]

    return(results_table)
}
