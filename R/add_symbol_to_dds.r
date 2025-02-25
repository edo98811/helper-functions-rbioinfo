#' Add Gene Symbols to DESeq2 Dataset
#'
#' This function adds gene symbols to the row data of a DESeq2 dataset object.
#'
#' @param dds A DESeq2 dataset object.
#' @param annotation_dataset A data frame containing gene annotations with at least two columns: 
#'   \code{gene_id} and \code{gene_name}. The \code{gene_id} column should match the row names of the DESeq2 dataset.
#'
#' @return A DESeq2 dataset object with an additional column \code{SYMBOL} in the row data, containing the gene symbols.
#'
#' @examples
#' \dontrun{
#' dds <- DESeqDataSetFromMatrix(countData = count_matrix, colData = col_data, design = ~ condition)
#' annotation_dataset <- data.frame(gene_id = c("gene1", "gene2"), gene_name = c("Gene A", "Gene B"))
#' dds <- add_symbols(dds, annotation_dataset)
#' }
#' @export
add_symbols <- function(dds, annotation_dataset) {
    rowData(dds)$SYMBOL <- annotation_dataset$gene_name[
        match(rownames(
            rowData(dds)), 
            annotation_dataset$gene_id)
        ]
    return(dds)
}