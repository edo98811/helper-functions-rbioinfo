#' Create Annotations
#'
#' This function generates annotation datasets from a DESeqDataSet object and saves them as RDS files.
#'
#' @param dds A DESeqDataSet object. The function will stop if the provided object is not of class 'DESeqDataSet'.
#'
#' @return This function does not return a value. It saves the generated annotations and annotation dataframe as RDS files in the "analyses_data" directory.
#' Additionally, it assigns the annotations and annotation dataframe to the parent environment.
#'
#' @examples
#' \dontrun{
#' dds <- DESeqDataSet(...) # Create or load a DESeqDataSet object
#' create_annotations(dds)
#' }
#'
#' @export
create_annotations <- function(dds) {

  if (!inherits(dds, "DESeqDataSet")) stop("dds object is not of class 'DDSdataset'. Please provide a valid dds object.")

  anns_path <- file.path("analyses_data", "anns.RDS")
  anno_df_path <- file.path("analyses_data", "anno_df.RDS")

  annotations <- annotation_datasets(dds)
  anns <- annotations$anns
  anno_df <- annotations$anno_df
  saveRDS(anns, anns_path)
  saveRDS(anno_df, anno_df_path)
  remove(annotations)

  assign("anns", anns, envir = parent.frame())
  assign("anno_df", anno_df, envir = parent.frame())
}



annotation_datasets <- function(dds, organism = "Human"){

  row_names <- rownames(as.data.frame(rowData(dds)))
  rownames(dds) <- gsub("\\.[0-9]*$", "", row_names)

  if (organism == "Human") {

    anno_df <- pcaExplorer::get_annotation_orgdb(dds, "org.Hs.eg.db", "ENSEMBL")
    # anno df and anns hanno la stessa funzione

    mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl", host = "https://www.ensembl.org")
    # "www" → Main server (https://www.ensembl.org)
    # "useast" → US East (https://useast.ensembl.org)
    # "uswest" → US West (https://uswest.ensembl.org)
    # "asia" → Asia (https://asia.ensembl.org)
  } else if (organism == "Mouse") {

    anno_df <- pcaExplorer::get_annotation_orgdb(dds, "org.Mm.eg.db", "ENSEMBL")
    # anno df and anns hanno la stessa funzione

    mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", dataset="mmusculus_gene_ensembl", host = "https://www.ensembl.org")
    # "www" → Main server (https://www.ensembl.org)
    # "useast" → US East (https://useast.ensembl.org)
    # "uswest" → US West (https://uswest.ensembl.org)
    # "asia" → Asia (https://asia.ensembl.org)

  } else { stop("Invalid organism") }
  # https://www.rdocumentation.org/packages/biomaRt/versions/2.28.0/topics/getBM
  anns <- biomaRt::getBM(attributes = c("ensembl_gene_id", "external_gene_name", "uniprotswissprot", "description"), 
                filters = "ensembl_gene_id",
                values = rownames(dds), 
                mart = mart)
  colnames(anns) <- c("unprot_id", "gene_symbol", "ensembl_gene_id", "description")

  anns <- anns[match(rownames(dds), anns$ensembl_gene_id), ]

  return(list(
    anns = anns, 
    # anno_df = anno_df)
  )
}
