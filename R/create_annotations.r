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
