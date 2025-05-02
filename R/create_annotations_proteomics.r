#' Create Annotations
#'
#' This function generates annotation datasets from a SummarizedExperiment object and saves them as RDS files.
#'
#' @param se A SummarizedExperiment object. The function will stop if the provided object is not of class 'SummarizedExperiment'.
#'
#' @return This function does not return a value. It saves the generated annotations and annotation dataframe as RDS files in the "analyses_data" directory.
#' Additionally, it assigns the annotations and annotation dataframe to the parent environment.
#'
#' @examples
#' \dontrun{
#' se <- SummarizedExperiment(...) # Create or load a SummarizedExperiment object
#' create_annotations(se)
#' }
#'
#' @export
create_annotations_proteomics <- function(df) {

  if (!is.data.frame(df)) stop("The input object is not a dataframe. Please provide a valid dataframe.")

  anno_df_path <- file.path("analyses_data", "anno_df.RDS")

  annotations <- annotation_datasets_proteomics(df)
  anno_df <- annotations$anno_df
  saveRDS(anno_df, anno_df_path)
  remove(annotations)

  assign("anno_df", anno_df, envir = parent.frame())
}



annotation_datasets_proteomics <- function(features_rowdata){

  # <- as.data.frame(rowData(se))

  # Check if the input dataframe contains a 'proteinID' column
  if (!"proteinID" %in% colnames(features_rowdata)) {
    stop("The input dataframe must contain a 'proteinID' column.")
  }

  # Connect to Ensembl database
  ensembl <- biomaRt::useMart("ensembl", dataset = "mmusculus_gene_ensembl", host = "https://www.ensembl.org")
  # "www" → Main server (https://www.ensembl.org)
  # "useast" → US East (https://useast.ensembl.org)
  # "uswest" → US West (https://uswest.ensembl.org)
  # "asia" → Asia (https://asia.ensembl.org)


  # Get gene names from protein accession (UniProt ID)
  anns <- biomaRt::getBM(attributes = c("uniprotswissprot", "mgi_symbol", "ensembl_gene_id", "description"), 
                    filters = "uniprotswissprot",
                    values = features_rowdata$proteinID,
                    mart = ensembl)

  anns <- anns[match(features_rowdata$proteinID, anns$uniprotswissprot), ]

  anns <- anns[, c("uniprotswissprot", "external_gene_name", "ensembl_gene_id", "description")]
  colnames(anns) <- c("unprot_id", "gene_symbol", "ensembl_gene_id", "description")

  return(list(
    anno_df = anns)
  )
}
