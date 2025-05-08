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
create_annotations_limma <- function(df) {

  if (!is.data.frame(df)) stop("The input object is not a dataframe. Please provide a valid dataframe.")

  anno_df_path <- file.path("analyses_data", "anns.RDS")
  if (!dir.exists("analyses_data")) dir.create("analyses_data")

  annotations <- annotation_datasets_limma(df)
  anns <- annotations$anns

  saveRDS(anns, anno_df_path)
  remove(annotations)

  assign("anns", anns, envir = parent.frame())
}



annotation_datasets_limma <- function(features_rowdata, organism = "Human"){

  # <- as.data.frame(rowData(se))

  # Check if the input dataframe contains a 'proteinID' column
  if (!"uniprot_id" %in% colnames(features_rowdata)) {
    stop("The input dataframe must contain a 'unprot_id' column.")
  }

  # "www" → Main server (https://www.ensembl.org)
  # "useast" → US East (https://useast.ensembl.org)
  # "uswest" → US West (https://uswest.ensembl.org)
  # "asia" → Asia (https://asia.ensembl.org)

  if (organism == "Human") {
    mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl", host = "https://www.ensembl.org")
  } else if (organism == "Mouse") {
    mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", dataset="mmusculus_gene_ensembl", host = "https://www.ensembl.org")
  } else { stop("Invalid organism") }


  # Get gene names from protein accession (UniProt ID)
  anns <- biomaRt::getBM(attributes = c("uniprotswissprot", "external_gene_name", "ensembl_gene_id", "description"), 
                    filters = "uniprotswissprot",
                    values = features_rowdata$uniprot_id,
                    mart = mart)

  anns <- anns[match(features_rowdata$uniprot_id, anns$uniprotswissprot), ]

  anns <- anns[, c("uniprotswissprot", "external_gene_name", "ensembl_gene_id", "description")]
  colnames(anns) <- c("unprot_id", "gene_symbol", "ensembl_gene_id", "description")

  return(list(
    anns = anns)
  )
}
