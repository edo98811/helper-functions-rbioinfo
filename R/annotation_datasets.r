#' Annotate Datasets
#'
#' This function annotates datasets by retrieving gene annotations from the Ensembl database.
#'
#' @param dds A DESeq2 dataset object.
#'
#' @return A list containing:
#' \item{anns}{A data frame with Ensembl gene IDs, external gene names, and descriptions.}
#' \item{anno_df}{A data frame with annotations retrieved using the `pcaExplorer` package.}
#'
#' @importFrom pcaExplorer get_annotation_orgdb
#' @importFrom biomaRt useMart getBM
#' @importFrom SummarizedExperiment rowData
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' library(DESeq2)
#' dds <- makeExampleDESeqDataSet()
#' annotations <- annotation_datasets(dds)
#' }
#'
#' @export
annotation_datasets <- function(dds){
  head(rownames(rowData(dds)))

  row_names <- rownames(as.data.frame(rowData(dds)))
  rownames(dds) <- gsub("\\.[0-9]*$", "", row_names)

  head(rownames(rowData(dds)))

  anno_df <- pcaExplorer::get_annotation_orgdb(dds, "org.Mm.eg.db", "ENSEMBL")
  # anno df and anns hanno la stessa funzione

  library("biomaRt")
  mart <- useMart(biomart="ENSEMBL_MART_ENSEMBL", dataset="mmusculus_gene_ensembl", host = "https://www.ensembl.org")
  # "www" → Main server (https://www.ensembl.org)
  # "useast" → US East (https://useast.ensembl.org)
  # "uswest" → US West (https://uswest.ensembl.org)
  # "asia" → Asia (https://asia.ensembl.org)

  # https://www.rdocumentation.org/packages/biomaRt/versions/2.28.0/topics/getBM
  anns <- getBM(attributes = c("ensembl_gene_id", "external_gene_name", "description"), 
                filters = "ensembl_gene_id",
                values = rownames(dds), 
                mart = mart)

  anns <- anns[match(rownames(dds), anns$ensembl_gene_id), ]

  return(list(
    anns = anns, 
    anno_df = anno_df)
  )
}
